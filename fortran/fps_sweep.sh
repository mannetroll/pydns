#!/usr/bin/env bash
set -euo pipefail

#
#  nohup bash fps_sweep.sh > fps_sweep.txt  2>&1 &
#
rm -f *.log
rm -f fps_sweep.csv
BIN=./dns3d.exe
OUT_CSV="fps_sweep.csv"

# Header
echo "N,FPS" > "${OUT_CSV}"

for K in $(seq 5 12); do
    N=$((2 ** K))

    LOG="fps_N${N}.log"
    echo "Running N=${N} ..."

    # Run and capture full output
    stdbuf -oL "${BIN}" "${N}" | tee "${LOG}"

    # Extract FPS from "Frames per second (FPS)" line (last field)
    FPS=$(grep "FPS:" "${LOG}" | tail -n 1 | awk '{print $NF}')

    # Append to CSV
    echo "${N},${FPS}" >> "${OUT_CSV}"
done

echo "Done. Results written to ${OUT_CSV}"