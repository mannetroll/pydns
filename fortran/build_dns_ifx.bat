@echo off
REM Initialize Intel oneAPI environment (if not already done)
call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" >nul 2>&1

REM Tell f2py to use Intel ifx
set FC=ifx

REM Roughly similar optimization / legacy settings to your gfortran flags
REM (tuned for old fixed-form Fortran)
set FFLAGS=/O3 /QxHost /Qunroll /Qsave /extend-source:132

REM Clean previous Windows extension (Python 3.13, win_amd64)
del /Q dns_fortran*.pyd 2>nul

REM Build the extension with f2py
python -m numpy.f2py -c dns_fortran_min.pyf ^
    pao.f vfft.f visasub.f dns_driver_min.f

echo.
echo Build finished.