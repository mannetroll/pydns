# fortran_dns_min.py
from __future__ import annotations

from pathlib import Path
from typing import Union

import numpy as np
from fortran import dns_fortran
from PIL import Image


class FortranDnsSimulator:
    """
    High-level Python driver using DNS_INIT / DNS_STEP / DNS_SNAPSHOT.

    Fortran side is assumed to expose:
      - dns_init(n, re, k0)
      - dns_step(t, dt, cn)
      - dns_frame(px, py, comp)
      - dns_kinetic(px, py)
      - dns_om2phys(px, py)
      - dns_streamfunc(px, py)
      - dns_set_ur_real(field, nx, ny)
    """

    # Variable selector constants (used by the Qt GUI)
    VAR_U = 0
    VAR_V = 1
    VAR_ENERGY = 2
    VAR_OMEGA = 3       # currently mapped to U (TODO)
    VAR_STREAM = 4      # currently mapped to U (TODO)

    def __init__(self, n: int = 192, re: float = 10000.0, k0: float = 10.0):
        self.n = int(n)
        self.re = float(re)
        self.k0 = float(k0)

        # UR dimensions from Fortran workspace: UR(2+3N/2, 3N/2, 3)
        self.nx = 2 + 3 * self.n // 2   # "height"
        self.ny = 3 * self.n // 2       # "width"

        # expose for GUI sizing
        self.py = self.nx   # height
        self.px = self.ny   # width

        # time integration scalars
        self.t = np.float32(0.0)
        self.dt = np.float32(0.0)
        self.cn = np.float32(1.0)
        self.iteration = 0

        # which field to visualize
        self.current_var = self.VAR_U

        # initialize Fortran side
        dns_fortran.dns_init(self.n, self.re, self.k0)

    # ------------------------------------------------------------------
    def step(self) -> None:
        """Advance one DNS step on the Fortran side."""
        t = np.array(self.t, dtype=np.float32)
        dt = np.array(self.dt, dtype=np.float32)
        cn = np.array(self.cn, dtype=np.float32)

        dns_fortran.dns_step(t, dt, cn)

        self.t = float(t)
        self.dt = float(dt)
        self.cn = float(cn)
        self.iteration += 1

    # ------------------------------------------------------------------
    def reset_field(self) -> None:
        """Reinitialize the DNS state on the Fortran side."""
        self.t = np.float32(0.0)
        self.dt = np.float32(0.0)
        self.cn = np.float32(1.0)
        self.iteration = 0
        dns_fortran.dns_init(self.n, self.re, self.k0)

    # ------------------------------------------------------------------
    def diagnostics(self) -> dict:
        return {"t": float(self.t), "dt": float(self.dt), "cn": float(self.cn)}

    # ------------------------------------------------------------------
    def _snapshot(self, comp: int) -> np.ndarray:
        """
        Raw snapshot from Fortran, now using dns_frame with 3× scale-up.
        comp is currently ignored on the Fortran side.
        """
        plane = dns_fortran.dns_frame(3 * self.n, 3 * self.n, comp)
        plane = np.array(plane, copy=False)
        return plane

    # ------------------------------------------------------------------
    def make_pixels(self, comp: int = 1) -> np.ndarray:
        """
        Convenience: comp-based visualization.
        comp = 1,2,3 (UR components).
        """
        return self._snapshot(comp)

    # ------------------------------------------------------------------
    def make_pixels_component(self, var: int | None = None) -> np.ndarray:
        """
        High-level selector used by the GUI:
          VAR_U      -> UR(:,:,1)
          VAR_V      -> UR(:,:,2)
          VAR_ENERGY -> sqrt(u^2+v^2+w^2)
          VAR_OMEGA  -> (currently same as U, TODO proper vorticity)
          VAR_STREAM -> (currently same as U, TODO streamfunction)
        """
        if var is None:
            var = self.current_var

        if var == self.VAR_U:
            plane = self._snapshot(1)
        elif var == self.VAR_V:
            plane = self._snapshot(2)
        elif var == self.VAR_ENERGY:
            plane = dns_fortran.dns_kinetic(3 * self.n, 3 * self.n)
            plane = np.array(plane, copy=False)
        elif var == self.VAR_OMEGA:
            plane = dns_fortran.dns_om2phys(3 * self.n, 3 * self.n)
            plane = np.array(plane, copy=False)
        elif var == self.VAR_STREAM:
            plane = dns_fortran.dns_streamfunc(3 * self.n, 3 * self.n)
            plane = np.array(plane, copy=False)
        else:
            plane = self._snapshot(1)

        return plane

    # ------------------------------------------------------------------
    def get_frame_pixels(self) -> np.ndarray:
        """Used by the Qt app worker thread.

        FIELD2PIX / dns_frame currently return 32-bit packed gray pixels
        (0x00LLLLLL). Here we reduce that once to an 8-bit contiguous
        array so the GUI can push it straight into a QImage.
        """
        plane = self.make_pixels_component(self.current_var)
        pixels32 = np.asarray(plane, dtype=np.uint32)
        pixels8 = (pixels32 & 0xFF).astype(np.uint8)
        return np.ascontiguousarray(pixels8)

    def set_variable(self, var: int) -> None:
        """Select which variable the GUI should visualize."""
        self.current_var = int(var)

    def get_time(self) -> float:
        return float(self.t)

    def get_iteration(self) -> int:
        return self.iteration

    # ---------- PNG EXPORT -------------------------------------------
    def save_png(self, path: Union[str, Path], comp: int = 1) -> None:
        """
        Export current field component (UR(:,:,comp)) as grayscale PNG.

        comp = 1,2,3 (UR components).
        """
        pixels = self.make_pixels(comp)
        img = Image.fromarray(pixels, mode="L")  # L = 8-bit grayscale
        img.save(str(path))


# ----------------------------------------------------------------------
if __name__ == "__main__":
    sim = FortranDnsSimulator()

    print("Starting DNS simulation")
    for i in range(10):
        sim.step()
        d = sim.diagnostics()
        print(f"Step {i+1:2d}: T={d['t']:.6f}, DT={d['dt']:.6f}, CN={d['cn']:.6f}")

    pix = sim.make_pixels_component()
    print("Pixels:", pix.shape, pix[0, 0], pix[10, 10])

    arr = sim._snapshot(1)
    np.savetxt(
        "snapshot.csv",
        arr,
        fmt="%d",  # integer formatting
        delimiter=","
    )