#!/bin/bash

export FC=gfortran
export FFLAGS="-Ofast -mcpu=native -funroll-loops -std=legacy \
  -ffixed-line-length-none -fno-automatic -finit-local-zero \
  -fallow-argument-mismatch"

# If you still want to completely silence warnings, you can optionally add:
#  -w -Wno-tabs
# to FFLAGS, e.g.:
# export FFLAGS="$FFLAGS -w -Wno-tabs"

rm -f dns_fortran.cpython-313-darwin.so

python -m numpy.f2py -c dns_fortran_min.pyf pao.f vfft.f visasub.f dns_driver_min.f