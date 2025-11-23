#!/bin/bash

set -euo pipefail

export FC=gfortran

# Common flags for old F77 / fixed-form
COMMON="-std=legacy -ffixed-line-length-none -Wno-tabs \
  -fallow-argument-mismatch -w"

# DNS / driver code: legacy semantics (SAVE-like locals)
DNS_FLAGS="$COMMON -fno-automatic -finit-local-zero \
  -Ofast -mcpu=native -funroll-loops -flto -fopenmp"

# FFT library: needs automatic locals for thread safety
VFFT_FLAGS="$COMMON -frecursive \
  -Ofast -mcpu=native -funroll-loops -flto -fopenmp"

# Find where libgomp lives and link to it explicitly (Linux: .so)
LIBGOMP_PATH=$(gfortran -print-file-name=libgomp.so)
LIBGOMP_DIR=$(dirname "$LIBGOMP_PATH")

# Link against libgomp, and add rpath so Linux can find it at runtime
export LDFLAGS="-L${LIBGOMP_DIR} -lgomp -Wl,-rpath,${LIBGOMP_DIR}"

# Tell NumPy to append (not overwrite) flags
export NPY_DISTUTILS_APPEND_FLAGS=1

# Runtime OpenMP threads
export OMP_NUM_THREADS=4
export OMP_DISPLAY_ENV=TRUE

rm -f dns_fortran*.so *.o

echo "Compiling Fortran objects with OpenMP..."

# vfft: compiled separately, re-entrant
"$FC" $VFFT_FLAGS -c vfft.f

# DNS core + driver: with -fno-automatic
"$FC" $DNS_FLAGS -c pao.f visasub3d.f dns_driver_min3d.f

echo "Building f2py module dns_fortran (with OpenMP)..."

python -m numpy.f2py -c \
  dns_fortran_min.pyf \
  pao.o visasub3d.o dns_driver_min3d.o vfft.o \
  -lgomp

python -c "import dns_fortran; dns_fortran.run_dns(1000.0, 10.0)"
echo "Done!"