# dns_sim_min.py
from typing import Union

import numpy as np
import dns_fortran
from pathlib import Path
from PIL import Image

class FortranDnsSimulator:
    def __init__(self, n: int = 128, re: float = 1000.0, k0: float = 10.0):
        self.n = int(n)
        self.re = float(re)
        self.k0 = float(k0)

        # UR dimensions from Fortran workspace
        self.nx = 2 + 3 * self.n // 2
        self.ny = 3 * self.n // 2

        self.t = np.float32(0.0)
        self.dt = np.float32(0.0)
        self.cn = np.float32(1.0)

        dns_fortran.dns_init(self.n, self.re, self.k0)

    def step(self):
        t = np.array(self.t, dtype=np.float32)
        dt = np.array(self.dt, dtype=np.float32)
        cn = np.array(self.cn, dtype=np.float32)

        dns_fortran.dns_step(t, dt, cn)

        self.t = float(t)
        self.dt = float(dt)
        self.cn = float(cn)

    def diagnostics(self):
        return {"t": self.t, "dt": self.dt, "cn": self.cn}

    def make_pixels(self, comp: int = 1) -> np.ndarray:
        """
        Fetch UR(:,:,comp) from Fortran and convert to grayscale.
        comp = 1,2,3
        """

        # *** THIS IS THE CORRECT CALL ***
        plane = dns_fortran.dns_snapshot(self.nx, self.ny, comp)
        #plane = dns_fortran.dns_frame(self.nx, self.ny)
        plane = np.array(plane, copy=False)

        # normalize to 0–255 uint8
        vmin, vmax = plane.min(), plane.max()
        if vmax > vmin:
            norm = (plane - vmin) / (vmax - vmin)
        else:
            norm = np.zeros_like(plane)

        pixels = (norm * 255.0).astype(np.uint8, order="C")
        return pixels

    def get_frame_pixels(self):
        return self.make_pixels(1)

    def get_time(self):
        return self.t

    def get_iteration(self):
        if self.dt > 0:
            return int(round(self.t / self.dt))
        return 0

   # ---------- NEW: PNG EXPORT --------------------------------------
    def save_png(self, path: Union[str, Path], comp: int = 1) -> None:
        """
        Export current field component as grayscale PNG.
        comp = 1,2,3 (which UR-component dns_snapshot returns).
        """
        pixels = self.make_pixels(comp)
        img = Image.fromarray(pixels, mode="L")  # L = 8-bit grayscale
        img.save(str(path))

if __name__ == "__main__":
    sim = FortranDnsSimulator()

    print("Starting DNS simulation")
    for i in range(10):
        sim.step()
        d = sim.diagnostics()
        print(f"Step {i+1}: T={d['t']:.6f}, DT={d['dt']:.6f}, CN={d['cn']:.6f}")

    pix = sim.make_pixels()
    print("Pixels:", pix.shape, pix[0, 0], pix[10, 10])
    sim.save_png("dns_frame_100.png", comp=1)