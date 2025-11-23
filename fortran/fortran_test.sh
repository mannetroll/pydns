#!/bin/bash

rm fortran_test.exe
rm *.o

FFLAGS_COMMON="-std=legacy -ffixed-line-length-none -Wno-tabs -fno-automatic -finit-local-zero -fallow-argument-mismatch"
FFLAGS="$FFLAGS_COMMON -O0 -g"

gfortran $FFLAGS -c pao.f dns.f vfft.f visasub.f
gfortran $FFLAGS -o fortran_test.exe pao.o dns.o vfft.o visasub.o
./fortran_test.exe

