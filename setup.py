from __future__ import annotations

import os
import sys
import shutil
import subprocess
from pathlib import Path

from setuptools import setup, Extension
from setuptools.command.build_ext import build_ext


class F2PyBuildExt(build_ext):
    """
    Custom build_ext that builds fortran.dns_fortran using:

      - vfft.f compiled *without* OpenMP into libvfft.a
      - DNS core (pao.f, visasub3d.f, dns_driver_min3d.f) built by
        `numpy.f2py` with OpenMP via FFLAGS
      - libgomp located from the Fortran compiler
      - meson backend (NumPy 2.x on Python 3.12+) under the hood

    Result: a ready-to-import extension `fortran.dns_fortran` that matches
    the behavior of the external build script.
    """

    def build_extension(self, ext):
        # Only special-case this one extension
        if ext.name != "fortran.dns_fortran":
            return super().build_extension(ext)

        root = Path(__file__).resolve().parent
        src_dir = root / "fortran"
        src_dir.mkdir(exist_ok=True)

        py_exe = sys.executable
        env = os.environ.copy()

        # -----------------------------
        #  Platform-specific defaults
        # -----------------------------
        if sys.platform == "darwin":
            default_fc = "/opt/homebrew/bin/gfortran"
            default_cc = "clang"
            default_cxx = "clang++"
            libgomp_name = "libgomp.dylib"
        else:
            # Linux / Fedora etc.
            default_fc = "gfortran"
            default_cc = "gcc"
            default_cxx = "g++"
            libgomp_name = "libgomp.so"

        # Compilers (env overrides defaults)
        fc = env.get("FC", default_fc)
        env["FC"] = fc
        env.setdefault("CC", default_cc)
        env.setdefault("CXX", default_cxx)

        # -----------------------------
        #  Fortran flags (same idea as scripts)
        # -----------------------------
        common = (
            "-std=legacy -ffixed-line-length-none -Wno-tabs "
            "-fallow-argument-mismatch -w -fPIC"
        )

        # DNS core: OpenMP + legacy semantics
        dns_fflags = (
            f"{common} "
            "-frecursive -fno-automatic -finit-local-zero "
            "-Ofast -mcpu=native -funroll-loops -fopenmp"
        )

        # VFFT: NO OpenMP, but recursive + optimized
        vfft_fflags = (
            f"{common} "
            "-frecursive "
            "-Ofast -mcpu=native -funroll-loops"
        )

        env["FFLAGS"] = dns_fflags

        # -----------------------------
        #  OpenMP runtime (libgomp)
        # -----------------------------
        ldflags = env.get("LDFLAGS", "")
        try:
            libgomp_path = subprocess.check_output(
                [fc, "-print-file-name", libgomp_name],
                text=True,
            ).strip()
        except Exception:
            libgomp_path = ""

        if libgomp_path and libgomp_path != libgomp_name:
            libgomp_dir = str(Path(libgomp_path).parent)
            extra_ld = f"-L{libgomp_dir} -lgomp -Wl,-rpath,{libgomp_dir}"
            ldflags = (ldflags + " " + extra_ld).strip()
        else:
            # If we can't locate libgomp, just continue without tweaking LDFLAGS.
            # This still lets you run without OpenMP if needed.
            pass

        env["LDFLAGS"] = ldflags

        env.setdefault("OMP_NUM_THREADS", "4")
        env.setdefault("OMP_DISPLAY_ENV", "TRUE")

        print("== F2PY build env ==")
        print("  sys.platform =", sys.platform)
        print("  FC           =", env["FC"])
        print("  CC           =", env["CC"])
        print("  CXX          =", env["CXX"])
        print("  FFLAGS       =", env.get("FFLAGS", ""))
        print("  LDFLAGS      =", env.get("LDFLAGS", ""))
        print("  src_dir      =", src_dir)

        # -----------------------------
        #  Compile vfft.f WITHOUT OpenMP into libvfft.a
        # -----------------------------
        vfft_f = src_dir / "vfft.f"
        vfft_o = src_dir / "vfft_build.o"
        libvfft_a = src_dir / "libvfft.a"

        if not vfft_f.exists():
            raise FileNotFoundError(f"Missing {vfft_f}")

        # Compile vfft.f
        subprocess.check_call(
            [fc, *vfft_fflags.split(), "-c", str(vfft_f), "-o", str(vfft_o)],
            env=env,
        )

        # Archive into libvfft.a (overwrite if present from sdist)
        if libvfft_a.exists():
            libvfft_a.unlink()
        subprocess.check_call(
            ["ar", "rcs", str(libvfft_a), str(vfft_o)],
            env=env,
        )

        # -----------------------------
        #  Run f2py to build dns_fortran
        #  (DNS core with OpenMP; vfft from libvfft.a in src_dir)
        # -----------------------------
        pyf = src_dir / "dns_fortran_min.pyf"
        pao_f = src_dir / "pao.f"
        visa_f = src_dir / "visasub3d.f"
        driver_f = src_dir / "dns_driver_min3d.f"

        for f in (pyf, pao_f, visa_f, driver_f):
            if not f.exists():
                raise FileNotFoundError(f"Missing {f}")

        cmd = [
            py_exe,
            "-m",
            "numpy.f2py",
            "-c",
            str(pyf),
            str(pao_f),
            str(visa_f),
            str(driver_f),
            f"-L{src_dir}",
            "-lvfft",
            "-lgomp",
        ]

        print("== Running f2py command ==")
        print(" ", " ".join(cmd))
        subprocess.check_call(cmd, cwd=src_dir, env=env)

        # -----------------------------
        #  Move built .so to setuptools' expected location
        # -----------------------------
        candidates = list(src_dir.glob("dns_fortran*.so"))
        if not candidates:
            raise RuntimeError("f2py did not produce dns_fortran*.so in fortran/")

        built_so = max(candidates, key=lambda p: p.stat().st_mtime)
        target_path = Path(self.get_ext_fullpath(ext.name))
        target_path.parent.mkdir(parents=True, exist_ok=True)

        print(f"Copying {built_so} -> {target_path}")
        shutil.copy2(built_so, target_path)


# Dummy extension; all build logic is in F2PyBuildExt.build_extension
ext_modules = [
    Extension(
        name="fortran.dns_fortran",
        sources=[],  # f2py drives this, not setuptools
    )
]

setup(
    name="pydns",
    ext_modules=ext_modules,
    cmdclass={"build_ext": F2PyBuildExt},
    include_package_data=True,
)