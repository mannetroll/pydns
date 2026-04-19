#!/usr/bin/env bash
set -euo pipefail

#
#  nohup stdbuf -oL bash fps_sweep.sh > fps_sweep.txt 2>&1 &
#
rm -f fps_N*.log
rm -f fps_sweep.csv

BIN=./dns3d.exe
OUT_CSV="fps_sweep.csv"

export OMP_DISPLAY_ENV=TRUE
export OMP_NUM_THREADS=4

echo "N,FPS" > "${OUT_CSV}"

for K in $(seq 5 12); do
    N=$((2 ** K))

    LOG="fps_N${N}.log"
    echo "Running N=${N} ..."

    # Capture full output (including OMP display)
    stdbuf -oL "${BIN}" "${N}" 2>&1 | tee "${LOG}"

    # USER FPS is the SYSTEM_CLOCK one, printed as: "FPS:  <value>"
    USER_FPS=$(grep -E '^[[:space:]]*FPS:' "${LOG}" | tail -n 1 | awk '{print $NF}')

    echo "${N},${USER_FPS}" >> "${OUT_CSV}"
done

echo "Done. Results written to ${OUT_CSV}"