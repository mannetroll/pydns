# pydns.spec — PyInstaller spec for the PyQt6 DNS viewer on macOS

# Run with:
#   pyinstaller pydns.spec
#
# This assumes the following in the same directory:
#   - main_min.py          (entry point, imports fortran_dns_min)
#   - fortran_dns_min.py   (Python wrapper around the Fortran DNS simulator)
#   - dns_fortran.*.so     (f2py-built extension module)

# If you have extra Fortran .dylib files that are loaded via ctypes or dlopen,
# see the "OPTIONAL: extra binaries" section further down.

from PyInstaller.utils.hooks import collect_submodules

block_cipher = None

# If dns_fortran is a standalone extension module, PyInstaller will usually
# detect it automatically via imports. collect_submodules is just a bit of
# extra safety if you split things into packages later.
hidden_imports = collect_submodules("dns_fortran") + collect_submodules("fortran_dns_min")

# OPTIONAL: extra binaries (e.g., separate .dylib compiled from Fortran)
# If your f2py build already linked everything into dns_fortran*.so, you can
# leave `extra_binaries` as an empty list.
extra_binaries = [
    # Example (uncomment and adjust if you have a separate Fortran .dylib):
    # ("path/to/libdns_fortran.dylib", ".", "BINARY"),
]

a = Analysis(
    ["main_min.py"],
    pathex=[],
    binaries=extra_binaries,
    datas=[],
    hiddenimports=hidden_imports,
    hookspath=[],
    hooksconfig={},
    runtime_hooks=[],
    excludes=[],
    win_no_prefer_redirects=False,
    win_private_assemblies=False,
    cipher=block_cipher,
    noarchive=False,
)

pyz = PYZ(
    a.pure,
    a.zipped_data,
    cipher=block_cipher,
)

exe = EXE(
    pyz,
    a.scripts,
    a.binaries,
    a.zipfiles,
    a.datas,
    [],
    name="pydns",
    debug=False,
    bootloader_ignore_signals=False,
    strip=False,
    upx=True,
    upx_exclude=[],
    runtime_tmpdir=None,
    console=False,  # GUI app, no console window
    disable_windowed_traceback=False,
    target_arch=None,
    codesign_identity=None,   # you can set your Apple dev ID here later
    entitlements_file=None,
)

app = BUNDLE(
    exe,
    name="pydns.app",
    icon=None,  # you can point to a .icns file here later
    bundle_identifier="se.qtagg.pydns",  # adjust to your preferred ID
)