# dns_fortran.pyi
#
# Type stub for the compiled extension module dns_fortran.
# Python uses dns_fortran.cpython-313-darwin.so at runtime; tools like
# PyCharm and mypy use this file for completion and type checking.

from typing import Any
import numpy as np
import numpy.typing as npt

NDInt32 = npt.NDArray[np.int32]
NDReal  = npt.NDArray[np.floating]


def dns_init(n: int, re: float, k0: float) -> None: ...
# Fortran:
#   subroutine dns_init(n, re, k0)
#       integer intent(in) :: n
#       real    intent(in) :: re
#       real    intent(in) :: k0
#   end subroutine dns_init


def dns_step(t: NDReal, dt: NDReal, cn: NDReal) -> None: ...
# Fortran:
#   subroutine dns_step(t, dt, cn)
#       real intent(inout) :: t
#       real intent(inout) :: dt
#       real intent(inout) :: cn
#   end subroutine dns_step
#
# Python usage (your wrapper):
#   t  = np.array(self.t,  dtype=np.float32)
#   dt = np.array(self.dt, dtype=np.float32)
#   cn = np.array(self.cn, dtype=np.float32)
#   dns_fortran.dns_step(t, dt, cn)
#   self.t  = float(t)
#   self.dt = float(dt)
#   self.cn = float(cn)


def dns_frame(px: int, py: int, comp: int) -> NDInt32: ...

def dns_kinetic(px: int, py: int) -> NDInt32: ...

def run_dns(n: int, re: float, k0: float) -> None: ...
# Fortran:
#   subroutine run_dns(n, re, k0)
#       integer intent(in) :: n
#       real    intent(in) :: re
#       real    intent(in) :: k0
#   end subroutine run_dns


def dns_snapshot(nx: int, ny: int, comp: int) -> NDReal: ...
# Fortran:
#   subroutine dns_snapshot(plane, nx, ny, comp)
#       integer, intent(in)  :: nx, ny, comp
#       real,    intent(out) :: plane(nx, ny)
#   end subroutine dns_snapshot
#
# Python usage:
#   plane = dns_fortran.dns_snapshot(nx, ny, comp)