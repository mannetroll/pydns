#!/bin/bash

set -euo pipefail

# Python interpreter (your 3.13 venv)
# In a uv venv, "python" already points to 3.13, so that's safest.
PYTHON=${PYTHON:-python}

# Compilers (Fedora / GCC toolchain)
export FC=${FC:-gfortran}
export CC=${CC:-gcc}
export CXX=${CXX:-g++}

# Source dir (where this script, vfft.f etc. live)
SRC_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# -----------------------------
#  Fortran flags
# -----------------------------
# Common flags for old F77 / fixed-form
COMMON="-std=legacy -ffixed-line-length-none -Wno-tabs \
  -fallow-argument-mismatch -w -fPIC"

# DNS / driver code: legacy semantics + OpenMP
DNS_FFLAGS="$COMMON -frecursive -fno-automatic -finit-local-zero \
  -Ofast -mcpu=native -funroll-loops -fopenmp"

# FFT library: NO OpenMP (thread-unsafe), still recursive + optimized
VFFT_FFLAGS="$COMMON -frecursive \
  -Ofast -mcpu=native -funroll-loops"

# Tell f2py/meson to use DNS_FFLAGS for all Fortran it compiles itself
export FFLAGS="$DNS_FFLAGS"

# -----------------------------
#  OpenMP runtime (libgomp)
# -----------------------------
LIBGOMP_PATH="$($FC -print-file-name=libgomp.so)"
if [[ "$LIBGOMP_PATH" == "libgomp.so" ]]; then
  echo "ERROR: libgomp.so not found via: $FC -print-file-name=libgomp.so"
  exit 1
fi
LIBGOMP_DIR="$(dirname "$LIBGOMP_PATH")"

# Link against libgomp and add rpath so the loader can find it
export LDFLAGS="-L${LIBGOMP_DIR} -lgomp -Wl,-rpath,${LIBGOMP_DIR}"

# Runtime OpenMP knobs
export OMP_NUM_THREADS=${OMP_NUM_THREADS:-4}
export OMP_DISPLAY_ENV=${OMP_DISPLAY_ENV:-TRUE}

echo "Using Python:           $PYTHON"
echo "Using Fortran compiler: $FC ($(command -v "$FC"))"
echo "Using C compiler:       $CC ($(command -v "$CC"))"
echo "Using C++ compiler:     $CXX ($(command -v "$CXX"))"
echo "SRC_DIR:                $SRC_DIR"
echo "FFLAGS (DNS):           $FFLAGS"
echo "VFFT_FFLAGS:            $VFFT_FFLAGS"
echo "LDFLAGS:                $LDFLAGS"
echo "libgomp path:           $LIBGOMP_PATH"
echo "OMP_NUM_THREADS:        $OMP_NUM_THREADS"

# -----------------------------
#  Clean old build artefacts
# -----------------------------
rm -f "$SRC_DIR"/dns_fortran*.so \
      "$SRC_DIR"/vfft.o \
      "$SRC_DIR"/libvfft.a

# -----------------------------
#  Compile vfft.f WITHOUT OpenMP and archive into libvfft.a
# -----------------------------
echo "Compiling vfft.f (NO OpenMP)..."
"$FC" $VFFT_FFLAGS -c "$SRC_DIR"/vfft.f -o "$SRC_DIR"/vfft.o

echo "Creating static library libvfft.a..."
ar rcs "$SRC_DIR"/libvfft.a "$SRC_DIR"/vfft.o

# -----------------------------
#  Build dns_fortran via f2py/meson
#  - f2py compiles pao/visasub3d/dns_driver_min3d with OpenMP (FFLAGS)
#  - vfft code is pulled in from libvfft.a via -L<SRC_DIR> -lvfft
# -----------------------------
echo "Building f2py module dns_fortran (DNS with OpenMP, vfft serial)..."

"$PYTHON" -m numpy.f2py -c \
  "$SRC_DIR"/dns_fortran_min.pyf \
  "$SRC_DIR"/pao.f "$SRC_DIR"/visasub3d.f "$SRC_DIR"/dns_driver_min3d.f \
  -L"$SRC_DIR" -lvfft

# -----------------------------
#  Quick sanity check
# -----------------------------
echo "Running quick sanity check..."
"$PYTHON" -c "import dns_fortran; dns_fortran.run_dns(1000.0, 10.0)"
echo "Done!"