#!/bin/bash

export FC=gfortran

# Common flags for old F77 / fixed-form
export FFLAGS_COMMON="-std=legacy -ffixed-line-length-none -Wno-tabs \
  -fno-automatic -finit-local-zero -fallow-argument-mismatch -w"

# Release flags + auto loop parallelization + OpenMP runtime
export FFLAGS="$FFLAGS_COMMON -Ofast -mcpu=native -funroll-loops -flto \
  -ftree-parallelize-loops=8 -fopenmp"

# Make sure the linker also gets OpenMP/libgomp
export LDFLAGS="-fopenmp"

# Help NumPy distutils append (not override) flags
export NPY_DISTUTILS_APPEND_FLAGS=1

# -fopt-info-optimized
# Use all 8 threads at runtime
export OMP_NUM_THREADS=8

rm -f dns_fortran.cpython-313-darwin.so

python -m numpy.f2py -c dns_fortran_min.pyf pao.f vfft.f visasub.f dns_driver_min.f
