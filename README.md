# PyDNS — 2D DNS Viewer (PyQt6 + Fortran)

A lightweight 2D Direct Numerical Simulation (DNS) visualizer using:

- **PyQt6** for the GUI  
- **NumPy + f2py** (Fortran) for the DNS solver  
- Runs on macOS  
- Optional `.app` bundle via PyInstaller  

---

## 🚀 Quick Install (macOS + pyenv + uv)

### 1. Install system tools

```bash
brew install pyenv
brew install uv
brew install gcc
```

- `pyenv` — Python version manager  
- `uv` — fast package manager + virtualenv + runner  
- `gcc` — provides `gfortran` for the Fortran module  

### 2. Install Python and activate it

```bash
pyenv install 3.13.5
pyenv local 3.13.5
```

Ensure pyenv is active:

```bash
echo 'eval "$(pyenv init -)"' >> ~/.zprofile
eval "$(pyenv init -)"
```

### 3. Create environment + install Python packages

```bash
uv venv
uv pip install pyqt6 numpy scipy pyinstaller
```

---

## 🔧 Build the Fortran extension

If your solver file is `dns_fortran.f90`:

```bash
uv run python -m numpy.f2py -c -m dns_fortran dns_fortran.f90
```

This produces:

```
dns_fortran.cpython-313-darwin.so
```

Place it next to `main_min.py`.

---

## ▶ Run the DNS Viewer

```bash
uv run python main_min.py
```

---

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

---

## 📁 Minimal Project Layout

```
pydns/
├── main_min.py
├── fortran_dns_min.py
├── dns_fortran.f90
├── dns_fortran.cpython-313-darwin.so
└── pydns.spec
```
