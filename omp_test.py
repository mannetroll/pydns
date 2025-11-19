import os
os.environ["OMP_DISPLAY_ENV"] = "TRUE"
os.environ["OMP_NUM_THREADS"] = "4"   # or 8 etc, before first import

from fortran import dns_fortran
import numpy as np

# 1) Initialize DNS
n = 256
dns_fortran.dns_init(n=n, re=1000.0, k0=10.0)

t = np.array(0, dtype=np.float32)
dt = np.array(0, dtype=np.float32)
cn = np.array(1, dtype=np.float32)

# 2) Advance a few timesteps
for i in range(10):
    dns_fortran.dns_step(t, dt, cn)
    print(f"step={i+1:3d}  T={t:.6f}  DT={dt:.6e}  CN={cn:.6f}")

