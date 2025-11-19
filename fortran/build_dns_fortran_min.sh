#!/bin/bash

export FC=gfortran

# Common flags for old F77 / fixed-form
# NOTE: add -w to suppress *all* warnings so Meson can't turn them into errors.
export FFLAGS_COMMON="-std=legacy -ffixed-line-length-none -Wno-tabs \
  -fno-automatic -finit-local-zero -fallow-argument-mismatch -w"

# Release flags, export FFLAGS="$FFLAGS_COMMON -O3"
export FFLAGS="$FFLAGS_COMMON -Ofast -mcpu=native -funroll-loops -flto"

rm -f dns_fortran.cpython-313-darwin.so

python -m numpy.f2py -c dns_fortran_min.pyf pao.f vfft.f visasub.f dns_driver_min.f
