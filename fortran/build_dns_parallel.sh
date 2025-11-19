#!/bin/bash

export FC=gfortran

# Common flags for old F77 / fixed-form
export FFLAGS_COMMON="-std=legacy -ffixed-line-length-none -Wno-tabs \
  -fno-automatic -finit-local-zero -fallow-argument-mismatch -w"

# Release flags + auto loop parallelization
# -fopt-info-optimized
export FFLAGS="$FFLAGS_COMMON -Ofast -mcpu=native -funroll-loops -flto \
  -ftree-parallelize-loops=8"

# Find where libgomp lives and link to it explicitly
LIBGOMP_PATH=$(gfortran -print-file-name=libgomp.dylib)
LIBGOMP_DIR=$(dirname "$LIBGOMP_PATH")

# Link against libgomp, and add rpath so macOS can find it at runtime
export LDFLAGS="-L${LIBGOMP_DIR} -lgomp -Wl,-rpath,${LIBGOMP_DIR}"

# Tell NumPy to append (not overwrite) flags
export NPY_DISTUTILS_APPEND_FLAGS=1

# Use up to 8 threads at runtime for auto-parallelized loops
export OMP_NUM_THREADS=8

rm -f dns_fortran.cpython-313-darwin.so

python -m numpy.f2py -c dns_fortran_min.pyf pao.f vfft.f visasub.f dns_driver_min.f

