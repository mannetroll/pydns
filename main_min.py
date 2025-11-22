import sys
import time
import os
from typing import Optional
import numpy as np

os.environ["OMP_NUM_THREADS"] = "4"
os.environ["OMP_DISPLAY_ENV"] = "TRUE"

from PyQt6.QtCore import (
    Qt,
    QThread,
    pyqtSignal,
    QObject,
    QSize,
)
from PyQt6.QtGui import (
    QImage,
    QPixmap,
    QFontDatabase,
    QIcon,
    QPainter,
    QFont,
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
from color_maps import COLOR_MAPS, DEFAULT_CMAP_NAME


def resource_path(name: str) -> str:
    """
    Resolve the resource path both in dev (source) and in PyInstaller .app/.exe.

    banner.png should sit in the project root (same folder as main_min.py)
    when running from source and be collected by PyInstaller when frozen.
    """
    if hasattr(sys, "_MEIPASS"):
        # PyInstaller temporary folder
        return os.path.join(sys._MEIPASS, name)
    # Normal Python run: relative to this file
    return os.path.join(os.path.dirname(__file__), name)


class SimulationWorker(QObject):
    """
    Runs the DNS simulation in its own thread, emitting frames as they are ready.
    """

    frame_ready = pyqtSignal(float, int, float)  # t, it, fps
    finished = pyqtSignal()

    def __init__(self, simulator: FortranDnsSimulator, parent=None) -> None:
        super().__init__(parent)
        self.sim = simulator
        self._running = False
        self._start_time = time.time()

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
            t = self.sim.get_time()
            it = self.sim.get_iteration()

            # ---- AUTO-RESTART -------------------------------------------------
            if it >= 3000:  # or 2000
                self.sim.reset_field()
                self._start_time = time.time()
                last_ts = time.time()
            # -------------------------------------------------------------------

            elapsed = time.time() - self._start_time
            fps = it / elapsed if elapsed > 0 else 0.0
            self.frame_ready.emit(t, it, fps)

            now = time.time()
            dt = now - last_ts
            threashold = 0.004
            if dt < threashold:
                time.sleep(threashold - dt)
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
        self.variable_combo.addItems(["U", "V", "K", "Ω", "φ"])

        # Colormap selector
        self.cmap_combo = QComboBox()
        self.cmap_combo.addItems(list(COLOR_MAPS.keys()))
        self.current_cmap_name = DEFAULT_CMAP_NAME
        idx = self.cmap_combo.findText(DEFAULT_CMAP_NAME)
        if idx >= 0:
            self.cmap_combo.setCurrentIndex(idx)

        # Layout
        button_row = QHBoxLayout()
        button_row.addWidget(self.start_button)
        button_row.addWidget(self.stop_button)
        button_row.addWidget(self.step_button)
        button_row.addWidget(self.reset_button)
        button_row.addWidget(self.save_button)
        button_row.addStretch()
        button_row.addWidget(self.variable_combo)
        button_row.addWidget(self.cmap_combo)

        central = QWidget()
        layout = QVBoxLayout(central)
        layout.addWidget(self.image_label, stretch=1)
        layout.addLayout(button_row)
        self.setCentralWidget(central)

        # Status bar
        self.status = QStatusBar()
        self.setStatusBar(self.status)

        # --- monospaced font for non-jumping text ---
        mono = QFontDatabase.systemFont(QFontDatabase.SystemFont.FixedFont)
        self.status.setFont(mono)

        # --- threads-used indicator ---
        self.threads_label = QLabel(self)
        self._update_threads_label()
        self.status.addPermanentWidget(self.threads_label)

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
        self.cmap_combo.currentTextChanged.connect(self.on_cmap_changed)

        # Window properties
        self.setWindowTitle("2D Turbulent DNS (PyQt6)")
        self.resize(self.sim.px + 40, self.sim.py + 120)

        self._last_pixels = None
        self._last_frame_time = None

        # initial draw
        self._update_image(self.sim.get_frame_pixels())

        # Start in OMEGA mode (index 3)
        self.variable_combo.setCurrentIndex(3)
        self.sim.set_variable(FortranDnsSimulator.VAR_OMEGA)

        # Color Turbo mode (index 5)
        self.cmap_combo.setCurrentIndex(5)

        # Start a worker thread
        self.thread.start()
        self.worker.start()  # auto-start simulation

    # ---- GUI slots --------------------------------------------------

    def on_start_clicked(self) -> None:
        self._last_frame_time = None
        self._update_threads_label()  # refresh in case env changed
        self.worker.start()

    def on_stop_clicked(self) -> None:
        self.worker.stop()

    def on_step_clicked(self) -> None:
        self.sim.step()
        pixels = self.sim.get_frame_pixels()
        self._update_image(pixels)
        t = self.sim.get_time()
        it = self.sim.get_iteration()
        self._update_status(t, it, fps=None)

    def on_reset_clicked(self) -> None:
        self.worker.stop()
        self.sim.reset_field()
        self._update_image(self.sim.get_frame_pixels())
        self._update_status(self.sim.get_time(), self.sim.get_iteration(), None)
        self.worker.start()  # auto-restart after reset

    def on_save_clicked(self) -> None:
        path, _ = QFileDialog.getSaveFileName(
            self, "Save frame", "frame.png",
            "PNG images (*.png);;All files (*)"
        )
        if path:
            pm = self.image_label.pixmap()
            if pm:
                pm.save(path, "PNG")

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

    def on_cmap_changed(self, name: str) -> None:
        if name in COLOR_MAPS:
            self.current_cmap_name = name
            # redraw with new colormap
            self._update_image(self.sim.get_frame_pixels())

    def closeEvent(self, event) -> None:
        self.worker.stop()
        self.thread.requestInterruption()
        self.thread.quit()
        self.thread.wait()
        super().closeEvent(event)

    # ---- worker callbacks -------------------------------------------

    def on_frame_ready(self, t: float, it: int, fps: float):
        self._status_update_counter += 1
        if self._status_update_counter >= 10:
            pixels = self.sim.get_frame_pixels()
            self._update_image(pixels)
            self._status_update_counter = 0
            self._update_status(t, it, fps)

    def _update_image(self, pixels: np.ndarray) -> None:
        # pixels = H×W uint8, map through current colormap LUT
        lut = COLOR_MAPS.get(getattr(self, "current_cmap_name", DEFAULT_CMAP_NAME))
        if lut is None:
            # fallback: grayscale
            h, w = pixels.shape
            qimg = QImage(pixels.data, w, h, w, QImage.Format.Format_Grayscale8)
        else:
            rgb = lut[pixels]  # H×W×3
            h, w, _ = rgb.shape
            self._last_pixels = rgb  # keep reference alive
            qimg = QImage(
                rgb.data,
                w,
                h,
                3 * w,
                QImage.Format.Format_RGB888,
            )

        self.image_label.setPixmap(QPixmap.fromImage(qimg))

    def _update_status(self, t: float, it: int, fps: Optional[float]):
        fps_str = f"{fps:4.0f}" if fps is not None else " n/a"
        txt = f"FPS: {fps_str} | Iter: {it:4d} | T: {t:4.2f}"
        self.status.showMessage(txt)

    def _update_threads_label(self) -> None:
        """Update the 'threads used' indicator in the status bar."""
        env_threads = os.environ.get("OMP_NUM_THREADS")

        if  env_threads:
            text = f"Threads: {env_threads} (OMP_NUM_THREADS)"
        else:
            cpu = os.cpu_count() or 1
            text = f"Threads: {cpu} (OpenMP default)"

        self.threads_label.setText("")
        #self.threads_label.setText(text)

    def keyPressEvent(self, event):
        key = event.key()

        # ----- ROTATE VARIABLE (P) -----
        if key == Qt.Key.Key_P:
            idx = self.variable_combo.currentIndex()
            count = self.variable_combo.count()
            new_idx = (idx + 1) % count
            self.variable_combo.setCurrentIndex(new_idx)
            return

        # ----- ROTATE COLORMAP (C) -----
        if key == Qt.Key.Key_C:
            idx = self.cmap_combo.currentIndex()
            count = self.cmap_combo.count()
            new_idx = (idx + 1) % count
            self.cmap_combo.setCurrentIndex(new_idx)
            return

        super().keyPressEvent(event)


def _create_banner_pixmap() -> QPixmap:
    """
    Try to load banner.png, otherwise create a simple text banner so
    the splash is always visible.
    """
    banner_path = resource_path("banner.png")

    pix = QPixmap()
    if os.path.exists(banner_path):
        pix = QPixmap(banner_path)

    if not pix.isNull():
        return pix

    # Fallback: white banner with text
    pix = QPixmap(900, 500)
    pix.fill(Qt.GlobalColor.white)

    painter = QPainter(pix)
    painter.setRenderHint(QPainter.RenderHint.Antialiasing)

    title_font = QFont()
    title_font.setPointSize(40)
    title_font.setBold(True)

    subtitle_font = QFont()
    subtitle_font.setPointSize(22)

    company_font = QFont()
    company_font.setPointSize(18)

    painter.setFont(title_font)
    painter.drawText(pix.rect(), Qt.AlignmentFlag.AlignHCenter | Qt.AlignmentFlag.AlignVCenter,
                     "pydns")

    painter.setFont(subtitle_font)
    painter.drawText(pix.rect().adjusted(0, 60, 0, 0),
                     Qt.AlignmentFlag.AlignHCenter | Qt.AlignmentFlag.AlignTop,
                     "loading...")

    painter.setFont(company_font)
    painter.drawText(pix.rect().adjusted(0, 0, 0, -40),
                     Qt.AlignmentFlag.AlignHCenter | Qt.AlignmentFlag.AlignBottom,
                     "Mannetroll Solutions AB")

    painter.end()
    return pix


def main() -> None:
    app = QApplication(sys.argv)

    # --- Splash/banner while DNS + GUI init happens ---
    #pix = _create_banner_pixmap()
    #splash = QSplashScreen(pix)
    #splash.show()
    #app.processEvents()  # make sure splash is drawn immediately

    window = MainWindow()
    window.adjustSize()          # force layout to compute size
    # center based on the calculated size
    screen = app.primaryScreen().availableGeometry()
    g = window.geometry()
    g.moveCenter(screen.center())
    window.setGeometry(g)
    window.show()    # already centered when it appears

    # Remove splash AFTER the window is visible and stable
    #QTimer.singleShot(500, lambda: splash.finish(window))

    sys.exit(app.exec())


if __name__ == "__main__":
    main()