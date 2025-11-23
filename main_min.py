import time
import os
t0 = time.time()
print("Top in main_min.py:", t0)

# ----------------------------------------------------------------------
# OpenMP environment
os.environ["OMP_NUM_THREADS"] = "4"
os.environ["OMP_DISPLAY_ENV"] = "TRUE"
#os.environ.setdefault("OMP_NUM_THREADS", "4")
#os.environ.setdefault("OMP_DISPLAY_ENV", "TRUE")

# Preload Fortran/OpenMP once at import time
from fortran_dns_min import FortranDnsSimulator

try:
    _preload_sim = FortranDnsSimulator()
    del _preload_sim
except Exception as exc:
    print("WARNING: preload FortranDnsSimulator failed:", repr(exc))

import sys
from typing import Optional

import numpy as np
from PyQt6.QtCore import Qt, QSize, QTimer
from PyQt6.QtGui import QImage, QPixmap, QFontDatabase, QIcon
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

from color_maps import COLOR_MAPS, DEFAULT_CMAP_NAME

class MainWindow(QMainWindow):
    def __init__(self, sim: FortranDnsSimulator) -> None:
        super().__init__()

        self.sim = sim
        self.current_cmap_name = DEFAULT_CMAP_NAME

        # --- central image label ---
        self.image_label = QLabel()
        self.image_label.setAlignment(Qt.AlignmentFlag.AlignCenter)

        # --- small icon buttons ---
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

        # Variable selector
        self.variable_combo = QComboBox()
        self.variable_combo.addItems(["U", "V", "K", "Ω", "φ"])

        # Colormap selector
        self.cmap_combo = QComboBox()
        self.cmap_combo.addItems(list(COLOR_MAPS.keys()))
        idx = self.cmap_combo.findText(DEFAULT_CMAP_NAME)
        if idx >= 0:
            self.cmap_combo.setCurrentIndex(idx)

        # --- layout ---
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

        # --- status bar ---
        self.status = QStatusBar()
        self.setStatusBar(self.status)

        mono = QFontDatabase.systemFont(QFontDatabase.SystemFont.FixedFont)
        self.status.setFont(mono)

        self.threads_label = QLabel(self)
        #self._update_threads_label()
        self.status.addPermanentWidget(self.threads_label)

        # Timer-based simulation (no QThread)
        self.timer = QTimer(self)
        self.timer.setInterval(0)   # as fast as Qt allows
        self.timer.timeout.connect(self._on_timer)

        # signal connections
        self.start_button.clicked.connect(self.on_start_clicked)
        self.stop_button.clicked.connect(self.on_stop_clicked)
        self.step_button.clicked.connect(self.on_step_clicked)
        self.reset_button.clicked.connect(self.on_reset_clicked)
        self.save_button.clicked.connect(self.on_save_clicked)
        self.variable_combo.currentIndexChanged.connect(self.on_variable_changed)
        self.cmap_combo.currentTextChanged.connect(self.on_cmap_changed)

        # window setup
        self.setWindowTitle("2D Turbulent DNS (PyQt6)")
        self.resize(self.sim.px + 40, self.sim.py + 120)

        self._last_pixels_rgb: Optional[np.ndarray] = None

        # --- FPS from simulation start ---
        self._sim_start_time = time.time()
        self._sim_start_iter = self.sim.get_iteration()

        # initial draw (omega mode)
        self.variable_combo.setCurrentIndex(3)
        self.sim.set_variable(self.sim.VAR_OMEGA)

        # (optional) a nice colormap index
        self.cmap_combo.setCurrentIndex(1)

        self._update_image(self.sim.get_frame_pixels())
        self._update_status(self.sim.get_time(), self.sim.get_iteration(), None)

        self.timer.start()  # auto-start simulation immediately

    # ------------------------------------------------------------------
    def on_start_clicked(self) -> None:
        #self._update_threads_label()
        if not self.timer.isActive():
            self.timer.start()

    def on_stop_clicked(self) -> None:
        if self.timer.isActive():
            self.timer.stop()

    def on_step_clicked(self) -> None:
        self.sim.step()
        pixels = self.sim.get_frame_pixels()
        self._update_image(pixels)
        t = self.sim.get_time()
        it = self.sim.get_iteration()
        # one manual step doesn't really change the global FPS much,
        # so we leave fps=None here
        self._update_status(t, it, fps=None)

    def on_reset_clicked(self) -> None:
        self.on_stop_clicked()
        self.sim.reset_field()

        # reset FPS baseline to "new simulation start"
        self._sim_start_time = time.time()
        self._sim_start_iter = self.sim.get_iteration()

        self._update_image(self.sim.get_frame_pixels())
        self._update_status(self.sim.get_time(), self.sim.get_iteration(), None)

    def on_save_clicked(self) -> None:
        path, _ = QFileDialog.getSaveFileName(
            self,
            "Save frame",
            "frame.png",
            "PNG images (*.png);;All files (*)",
        )
        if path:
            pm = self.image_label.pixmap()
            if pm:
                pm.save(path, "PNG")

    def on_variable_changed(self, index: int) -> None:
        mapping = {
            0: self.sim.VAR_U,
            1: self.sim.VAR_V,
            2: self.sim.VAR_ENERGY,
            3: self.sim.VAR_OMEGA,
            4: self.sim.VAR_STREAM,
        }
        self.sim.set_variable(mapping.get(index, self.sim.VAR_U))
        self._update_image(self.sim.get_frame_pixels())

    def on_cmap_changed(self, name: str) -> None:
        if name in COLOR_MAPS:
            self.current_cmap_name = name
            self._update_image(self.sim.get_frame_pixels())

    # ------------------------------------------------------------------
    def _on_timer(self) -> None:
        # one DNS step per timer tick
        self.sim.step()

        # Count frames since last GUI update
        self._status_update_counter += 1

        # Update GUI only every 20 frames
        UPDATE_INTERVAL = 20
        if self._status_update_counter >= UPDATE_INTERVAL:
            pixels = self.sim.get_frame_pixels()
            self._update_image(pixels)

            # ---- FPS from simulation start ----
            now = time.time()
            elapsed = now - self._sim_start_time
            steps = self.sim.get_iteration() - self._sim_start_iter
            fps = None
            if elapsed > 0 and steps > 0:
                fps = steps / elapsed

            self._update_status(
                self.sim.get_time(),
                self.sim.get_iteration(),
                fps,
            )

            self._status_update_counter = 0

        # Optional auto-reset like before
        if self.sim.get_iteration() >= 3000:
            self.sim.reset_field()
            self._sim_start_time = time.time()
            self._sim_start_iter = self.sim.get_iteration()

    # ------------------------------------------------------------------
    def _update_image(self, pixels: np.ndarray) -> None:
        """
        Map H×W uint8 pixels through colormap and show in label.
        """
        pixels = np.asarray(pixels, dtype=np.uint8)
        if pixels.ndim != 2:
            return

        lut = COLOR_MAPS.get(self.current_cmap_name)
        if lut is None:
            # fallback: grayscale
            h, w = pixels.shape
            qimg = QImage(
                pixels.data,
                w,
                h,
                w,
                QImage.Format.Format_Grayscale8,
            )
        else:
            lut_arr = np.asarray(lut, dtype=np.uint8)
            rgb = lut_arr[pixels]  # H×W×3
            h, w, _ = rgb.shape
            self._last_pixels_rgb = rgb  # keep alive
            qimg = QImage(
                rgb.data,
                w,
                h,
                3 * w,
                QImage.Format.Format_RGB888,
            )

        self.image_label.setPixmap(QPixmap.fromImage(qimg))

    def _update_status(self, t: float, it: int, fps: Optional[float]) -> None:
        fps_str = f"{fps:4.0f}" if fps is not None else " n/a"
        txt = f"FPS: {fps_str} | Iter: {it:5d} | T: {t:6.3f}"
        self.status.showMessage(txt)

#    def _update_threads_label(self) -> None:
#        env_threads = os.environ.get("OMP_NUM_THREADS")
#        if env_threads:
#            text = f"Threads: {env_threads} (OMP_NUM_THREADS)"
#        else:
#            cpu = os.cpu_count() or 1
#            text = f"Threads: {cpu} (OpenMP default)"
#        self.threads_label.setText(text)

    # ------------------------------------------------------------------
    def keyPressEvent(self, event) -> None:
        key = event.key()

        # rotate variable (P)
        if key == Qt.Key.Key_P:
            idx = self.variable_combo.currentIndex()
            count = self.variable_combo.count()
            self.variable_combo.setCurrentIndex((idx + 1) % count)
            return

        # rotate colormap (C)
        if key == Qt.Key.Key_C:
            idx = self.cmap_combo.currentIndex()
            count = self.cmap_combo.count()
            self.cmap_combo.setCurrentIndex((idx + 1) % count)
            return

        super().keyPressEvent(event)


# ----------------------------------------------------------------------
def main() -> None:
    print(f"Enter main(): {time.time()}  (elapsed={time.time() - t0:.3f}s)")
    app = QApplication(sys.argv)

    sim_init_start = time.time()
    sim = FortranDnsSimulator()
    print(
        "After Fortran init:",
        time.time() - t0,
        "(sim init took:",
        time.time() - sim_init_start,
        ")",
    )

    window = MainWindow(sim)
    window.adjustSize()

    screen = app.primaryScreen().availableGeometry()
    g = window.geometry()
    g.moveCenter(screen.center())
    window.setGeometry(g)
    window.show()

    sys.exit(app.exec())


if __name__ == "__main__":
    main()