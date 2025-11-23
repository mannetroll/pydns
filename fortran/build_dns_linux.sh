#!/bin/bash

set -euo pipefail

# Fortran compiler + flags
export FC=${FC:-gfortran}
export FFLAGS="-Ofast -mcpu=native -funroll-loops -std=legacy \
  -ffixed-line-length-none -fno-automatic -finit-local-zero \
  -fallow-argument-mismatch"

# Force NumPy/distutils to use the normal gcc instead of aarch64-linux-gnu-gcc
export CC=${CC:-gcc}
export CXX=${CXX:-g++}

# Optional: sanity check
if ! command -v "$CC" >/dev/null 2>&1; then
  echo "ERROR: C compiler '$CC' not found in PATH"
  exit 1
fi

# Clean previous build artifacts (Linux suffix)
rm -f dns_fortran*.so

python -m numpy.f2py -c \
  --compiler=unix \
  --fcompiler=gnu95 \
  dns_fortran_min.pyf \
  pao.f vfft.f visasub.f dns_driver_min.f

python -c "import dns_fortran; dns_fortran.run_dns(1000.0, 10.0)"
echo "Done!"

