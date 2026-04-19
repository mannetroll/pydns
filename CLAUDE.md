# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

2D/3D turbulent DNS (Direct Numerical Simulation) solver. Legacy Fortran core wrapped with `numpy.f2py` and driven by a PyQt6 GUI. Target platform is macOS (Apple Silicon); Linux build scripts also exist. Requires Python 3.13.

## Common commands

Environment / deps (uses `uv`):
```bash
uv venv --python 3.13.9 && source .venv/bin/activate
uv sync
```

Run the viewer:
```bash
uv run python main_min.py
```

Build the f2py extension in `fortran/` (pick one; both produce `fortran/dns_fortran.cpython-313-darwin.so`):
```bash
cd fortran && bash build_dns_macos.sh        # serial, simpler
cd fortran && bash build_dns_macos_omp.sh    # OpenMP, production path
```

Build wheel / app / publish:
```bash
uv build                                     # wheel (invokes setup.py F2PyBuildExt)
uv run pyinstaller pydns.spec                # produces dist/pydns.app
uv run python -m build && uv run twine upload dist/*
```

Standalone Fortran executables (no Python, benchmarking / dev of the solver itself):
```bash
cd fortran && bash run_dns3d.sh              # F77 fixed-form, OpenMP
cd fortran && bash run_dns3d90.sh            # F90 free-form, takes N as arg
cd fortran && bash fps_sweep.sh              # sweeps N=2^5..2^12, writes fps_sweep.csv
```

Smoke-test the Fortran binding directly:
```bash
uv run python omp_test.py
uv run python fortran_dns_min.py             # steps 10x, writes snapshot.csv
```

## Architecture

Three layers, each with its own build surface:

**Fortran core** (`fortran/*.f`, `*.f90`). The numerical solver. Split deliberately:
- `vfft.f` — FFT library. **Must be compiled WITHOUT OpenMP** (not thread-safe). Built into a static `libvfft.a` and linked in.
- `pao.f`, `visasub3d.f`, `dns_driver_min3d.f` — DNS kernels, compiled **with** `-fopenmp`.
- `dns3d.f` / `dns3d90.f90` — standalone-executable variants (not used by the Python path). Keep in sync with the `dns_driver_min3d.f` physics when changing the solver.
- Workspace array convention: `UR(2 + 3N/2, 3N/2, 3)`. The `2+` accounts for FFT padding; `3N/2` is the dealiased grid size. This dimensioning is baked into both Fortran and the Python wrapper (`fortran_dns_min.py:43-46`).
- Compile flags are legacy-F77 intentional: `-std=legacy -ffixed-line-length-none -fno-automatic -finit-local-zero -fallow-argument-mismatch`. `-fno-automatic` + `-finit-local-zero` make locals behave like old SAVE semantics — do not drop these when touching build scripts.

**f2py interface** (`fortran/dns_fortran_min.pyf`). Declares the Python-visible API: `dns_init`, `dns_step`, `dns_frame`, `dns_kinetic`, `dns_om2phys`, `dns_streamfunc`, `dns_snapshot`, `run_dns`. When you add a Fortran subroutine that should be callable from Python, add a matching block here — f2py will not pick it up otherwise.

**Python layer**:
- `fortran_dns_min.py` — `FortranDnsSimulator` class. Owns the single global Fortran state (there is no per-instance state on the Fortran side — a second simulator would clobber the first). Visualization selectors (`VAR_U/V/ENERGY/OMEGA/STREAM`) map to different Fortran frame routines. `dns_frame` returns 32-bit packed gray pixels; the wrapper masks to 8-bit for Qt.
- `main_min.py` — PyQt6 viewer. Single-threaded, `QTimer` drives `sim.step()` per tick (no QThread). Auto-resets at iteration 3000. Preloads `FortranDnsSimulator` at import time to amortize OpenMP startup. GUI updates every 20 steps, not every step, to keep FPS high.
- `color_maps.py` — 256×3 uint8 LUTs applied in Python (not Fortran); `DEFAULT_CMAP_NAME = "Doom Fire"` but the key in `COLOR_MAPS` is `"Doom"` (mismatch — the default-index lookup in `main_min.py` falls back to index 1).

**Two parallel build paths** — keep both working when you change Fortran sources:
1. The shell scripts in `fortran/` (developer loop).
2. `setup.py`'s `F2PyBuildExt` (wheel / PyPI). It replicates the same two-phase build: `vfft.f` → `libvfft.a` serial, then f2py with OpenMP for the rest. It also locates `libgomp` via `gfortran -print-file-name` and injects an rpath so the wheel works outside a Homebrew shell.

## Gotchas

- `fortran/__init__.py` is empty (1 line) by design — the package exists only to host the compiled `.so`.
- If you see threading-related FFT corruption, check that `vfft.f` was built without `-fopenmp`. The two build scripts enforce this; a naive "add `-fopenmp` everywhere" change will break it.
- `OMP_NUM_THREADS` is hard-set to `4` in `main_min.py` before the Fortran import. Changing it after import has no effect.
- `setup.py` expects `dns_driver_min3d.f` + `visasub3d.f` (3D variants). The simpler `build_dns_macos.sh` uses the 2D `dns_driver_min.f` + `visasub.f`. The `.pyf` interface is shared, so both produce a compatible module.
