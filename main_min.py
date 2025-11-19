import sys
import time
from typing import Optional

import numpy as np
from PyQt6.QtCore import (
    Qt,
    QThread,
    pyqtSignal,
    QObject,
    QTimer,
    QSize,
)
from PyQt6.QtGui import (
    QImage,
    QPixmap,
    QFontDatabase,
    QIcon,
)
from PyQt6.QtWidgets import (
    QApplication,
    QMainWindow,
    QWidget,
    QLabel,
    QPushButton,
    QFileDialog,
    QVBoxLayout,
    QHBoxLayout,
    QComboBox,
    QStatusBar,
)

from fortran_dns_min import FortranDnsSimulator


class SimulationWorker(QObject):
    """
    Runs the DNS simulation in its own thread, emitting frames as they are ready.
    """

    frame_ready = pyqtSignal(np.ndarray, float, int)  # pixels, time, iteration
    finished = pyqtSignal()

    def __init__(self, simulator: FortranDnsSimulator, parent=None) -> None:
        super().__init__(parent)
        self.sim = simulator
        self._running = False

    def start(self) -> None:
        self._running = True

    def stop(self) -> None:
        self._running = False

    def run(self) -> None:
        """
        Main simulation loop. Intended to be connected to QThread.started.
        """
        thread = QThread.currentThread()
        last_ts = time.time()

        # Loop until the thread is asked to stop
        while not thread.isInterruptionRequested():
            if not self._running:
                if thread.isInterruptionRequested():
                    break
                time.sleep(0.01)
                continue

            self.sim.step()
            pixels = self.sim.get_frame_pixels()
            t = self.sim.get_time()
            it = self.sim.get_iteration()

            self.frame_ready.emit(pixels.copy(), t, it)

            now = time.time()
            dt = now - last_ts
            if dt < 0.01:
                time.sleep(0.01 - dt)
            last_ts = now

        self.finished.emit()


class MainWindow(QMainWindow):
    def __init__(self) -> None:
        super().__init__()

        # Create simulator
        self.sim = FortranDnsSimulator()

        # GUI elements
        self.image_label = QLabel()
        self.image_label.setAlignment(Qt.AlignmentFlag.AlignCenter)

        # Small icon buttons instead of large text buttons
        self.start_button = QPushButton()
        self.start_button.setIcon(QIcon.fromTheme("media-playback-start"))
        self.start_button.setToolTip("Start simulation")
        self.start_button.setFixedSize(36, 36)
        self.start_button.setIconSize(QSize(24, 24))

        self.stop_button = QPushButton()
        self.stop_button.setIcon(QIcon.fromTheme("media-playback-stop"))
        self.stop_button.setToolTip("Stop simulation")
        self.stop_button.setFixedSize(36, 36)
        self.stop_button.setIconSize(QSize(24, 24))

        self.step_button = QPushButton("Step")
        self.reset_button = QPushButton("Reset")
        self.save_button = QPushButton("Save Frame")
        self._status_update_counter = 0

        self.variable_combo = QComboBox()
        self.variable_combo.addItems(
            ["U", "V", "K", "Ω", "φ"]
        )

        # Layout
        button_row = QHBoxLayout()
        button_row.addWidget(self.start_button)
        button_row.addWidget(self.stop_button)
        button_row.addWidget(self.step_button)
        button_row.addWidget(self.reset_button)
        button_row.addWidget(self.save_button)
        button_row.addStretch()
        button_row.addWidget(self.variable_combo)

        central = QWidget()
        layout = QVBoxLayout(central)
        layout.addWidget(self.image_label, stretch=1)
        layout.addLayout(button_row)
        self.setCentralWidget(central)

        # Status bar
        self.status = QStatusBar()
        self.setStatusBar(self.status)

        # --- NEW: force monospaced font for non-jumping text ---
        mono = QFontDatabase.systemFont(QFontDatabase.SystemFont.FixedFont)
        self.status.setFont(mono)

        # Thread + worker
        self.thread = QThread(self)
        self.worker = SimulationWorker(self.sim)
        self.worker.moveToThread(self.thread)

        self.thread.started.connect(self.worker.run)
        self.worker.frame_ready.connect(self.on_frame_ready)

        # Button connections
        self.start_button.clicked.connect(self.on_start_clicked)
        self.stop_button.clicked.connect(self.on_stop_clicked)
        self.step_button.clicked.connect(self.on_step_clicked)
        self.reset_button.clicked.connect(self.on_reset_clicked)
        self.save_button.clicked.connect(self.on_save_clicked)
        self.variable_combo.currentIndexChanged.connect(self.on_variable_changed)

        # Window properties
        self.setWindowTitle("2D Turbulent DNS (PyQt6)")
        self.resize(self.sim.px + 40, self.sim.py + 120)

        # Initial frame
        self._last_pixels: Optional[np.ndarray] = None
        self._last_frame_time: Optional[float] = None
        self._sim_start_time: Optional[float] = None
        self._update_image(self.sim.get_frame_pixels())

        # Start in OMEGA mode (index 3)
        self.variable_combo.setCurrentIndex(3)
        self.sim.set_variable(FortranDnsSimulator.VAR_OMEGA)

        # Start worker thread
        self.thread.start()
        self._sim_start_time = time.time()
        self.worker.start()  # auto-start simulation

    # ---- GUI slots --------------------------------------------------

    def on_start_clicked(self) -> None:
        self._last_frame_time = None
        self._sim_start_time = time.time()
        self.worker.start()

    def on_stop_clicked(self) -> None:
        self.worker.stop()

    def on_step_clicked(self) -> None:
        self.sim.step()
        self._update_image(self.sim.get_frame_pixels())
        t = self.sim.get_time()
        it = self.sim.get_iteration()
        self._update_status(t, it, fps=None)

    def on_reset_clicked(self) -> None:
        self.worker.stop()
        self.sim.reset_field()
        self._last_frame_time = None
        self._update_image(self.sim.get_frame_pixels())
        self._update_status(self.sim.get_time(), self.sim.get_iteration(), fps=None)
        self._sim_start_time = time.time()
        self.worker.start()  # auto-restart after reset

    def on_save_clicked(self) -> None:
        path, _ = QFileDialog.getSaveFileName(
            self, "Save frame", "frame.png",
            "PNG images (*.png);;All files (*)"
        )
        if not path:
            return
        pixmap = self.image_label.pixmap()
        if pixmap is not None:
            pixmap.save(path, "PNG")

    def on_variable_changed(self, index: int) -> None:
        mapping = {
            0: FortranDnsSimulator.VAR_U,
            1: FortranDnsSimulator.VAR_V,
            2: FortranDnsSimulator.VAR_ENERGY,
            3: FortranDnsSimulator.VAR_OMEGA,
            4: FortranDnsSimulator.VAR_STREAM,
        }
        self.sim.set_variable(mapping.get(index))
        self._update_image(self.sim.get_frame_pixels())

    def closeEvent(self, event) -> None:
        self.worker.stop()
        self.thread.requestInterruption()
        self.thread.quit()
        self.thread.wait()
        super().closeEvent(event)

    # ---- worker callbacks -------------------------------------------

    def on_frame_ready(self, pixels: np.ndarray, t: float, it: int) -> None:
        fps: Optional[float] = None

        # FPS based on real elapsed wall-clock time since simulation start
        if self._sim_start_time is not None:
            elapsed = time.time() - self._sim_start_time
            if elapsed > 0.0:
                fps = it / elapsed

        self._update_image(pixels)

        # Update status every 10th frame
        self._status_update_counter += 1
        if self._status_update_counter >= 10:
            self._status_update_counter = 0
            self._update_status(t, it, fps)

    # ---- helpers ----------------------------------------------------

    def _update_image(self, pixels: np.ndarray) -> None:
        """Render packed 32-bit grayscale from Fortran FIELD2PIX into the QLabel."""
        if pixels.ndim != 2:
            raise ValueError(f"Expected 2D array, got {pixels.shape}")

        # FIELD2PIX packs the gray level L (0–255) into all three bytes:
        # PIXEL = L * (1 + 256 + 65536) = 0x00LLLLLL
        # We only need the low byte for an 8-bit QImage.
        if pixels.dtype != np.uint8:
            pixels = (pixels & 0xFF).astype(np.uint8, copy=False)

        h, w = pixels.shape
        pixels_c = np.ascontiguousarray(pixels)

        qimg = QImage(
            pixels_c.data,
            w,
            h,
            w,  # bytes per line
            QImage.Format.Format_Grayscale8,
        )

        self._last_pixels = pixels_c
        pixmap = QPixmap.fromImage(qimg)
        self.image_label.setPixmap(pixmap)


    def _update_status(self, t: float, it: int, fps: Optional[float]) -> None:
        # Monospaced + fixed width → no text jumping
        fps_str = f"{fps:4.0f}" if fps is not None else "     n/a"
        it_str = f"{it:4d}"
        t_str = f"{t:4.2f}"

        text = f"FPS: {fps_str} | Iter: {it_str} | T: {t_str}"
        self.status.showMessage(text)

    def _position_window(self) -> None:
        screen = self.screen() or QApplication.primaryScreen()
        if not screen:
            return
        geo = screen.availableGeometry()
        frame = self.frameGeometry()
        frame.moveCenter(geo.center())
        self.move(frame.topLeft())


def main() -> None:
    app = QApplication(sys.argv)
    window = MainWindow()
    window.show()
    QTimer.singleShot(0, window._position_window)
    sys.exit(app.exec())


if __name__ == "__main__":
    main()