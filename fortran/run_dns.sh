#!/bin/bash

rm dns3d.exe
rm *.o

export OMP_DISPLAY_ENV=TRUE
export OMP_NUM_THREADS=4
# export OMP_SCHEDULE=STATIC

FFLAGS_COMMON="-std=legacy -ffixed-line-length-none -Wno-tabs -fno-automatic -finit-local-zero -fallow-argument-mismatch"
FFLAGS="$FFLAGS_COMMON -Ofast -mcpu=native"

# vfft: compiled separately, re-entrant
gfortran -std=legacy -ffixed-line-length-none -Wno-tabs -frecursive -Ofast -mcpu=native -c vfft.f

# rest of code with your usual flags + OpenMP
gfortran $FFLAGS -c pao.f dns3d.f visasub3d.f

# link
gfortran -Ofast -mcpu=native -o dns3d_nomp.exe pao.o dns3d.o visasub3d.o vfft.o
./dns3d_nomp.exe

