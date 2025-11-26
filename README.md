# PyDNS — 2D DNS Viewer (PyQt6 + Fortran)

A lightweight 2D Direct Numerical Simulation (DNS) visualizer using:

- **PyQt6** for the GUI  
- **NumPy + f2py** (Fortran) for the DNS solver  
- Runs on macOS  
- Optional `.app` bundle via PyInstaller  

---

## 🚀 Quick Install (macOS + gcc + uv)

### 1. Install system tools

```bash
brew install uv
brew install gcc
```

- `uv` — Fast Python package manager + virtualenv + runner  
- `gcc` — provides `gfortran` for the Fortran module  

### 2. Install Python and activate it

```bash
git clone git@github.com:mannetroll/pydns.git
cd pydns
uv python install 3.13.9
uv python pin 3.13.9
uv venv --python 3.13.9
source .venv/bin/activate
uv sync
```

---

## 🔧 Build the Fortran extension without OpenMP

If your solver file is `dns_driver_min.f`:

```bash
cd fortran
bash build_dns_macos.sh
```

This produces:

```
dns_fortran.cpython-313-darwin.so
```

---

## ▶ Run the DNS Viewer

![DNS Viewer Window](https://github.com/mannetroll/pydns/raw/main/window.png)

```bash
uv run python main_min.py
```

---

Build Wheel:

```bash
uv build
```

Produces:

```
dist/pydns-0.1.1-cp313-cp313-macosx_15_0_arm64.whl
```

## 🖥 Build a macOS `.app`

The project includes `pydns.spec`.

Build the bundle:

```bash
uv run pyinstaller pydns.spec
```

Produces:

```
dist/pydns.app
```

Double-click to launch.

## twine & pypi.org

```
uv add --dev build twine
uv run python -m build
uv run twine upload dist/*
```

## one-liner

```
curl -LsSf https://astral.sh/uv/install.sh | sh
uv run --with mannetroll-pydns==0.1.2 python -m main_min
```

---
