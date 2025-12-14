#!/bin/bash
set -e

rm -f dns3d90.exe
rm -f *.o

export OMP_DISPLAY_ENV=TRUE
export OMP_NUM_THREADS=4
# export OMP_SCHEDULE=STATIC

FFLAGS_COMMON="-ffixed-line-length-none -Wno-tabs -finit-local-zero -fallow-argument-mismatch"
FFLAGS="$FFLAGS_COMMON -Ofast -mcpu=native -fopenmp -frecursive"

# vfft: compiled separately, re-entrant
# vfft: compiled separately, re-entrant
gfortran -std=legacy -ffixed-line-length-none -Wno-tabs -frecursive -Ofast -mcpu=native -c vfft.f

# legacy code + driver
gfortran $FFLAGS -c pao.f visasub3d.f
gfortran $FFLAGS -c dns3d90.f90

# link
gfortran -Ofast -mcpu=native -fopenmp -o dns3d90.exe pao.o dns3d90.o visasub3d.o vfft.o

./dns3d90.exe 8192