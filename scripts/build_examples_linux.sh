#!/bin/bash
set -e

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. && pwd)"
cd "$ROOT_DIR"

OUT_DIR="examples/bin"
mkdir -p "$OUT_DIR"
mkdir -p "$ROOT_DIR/tmp"

RUN_ID="${FAFAFA_BUILD_EXAMPLES_RUN_ID:-$(date +%Y%m%d_%H%M%S)_$$}"
BUILD_LOG="$ROOT_DIR/tmp/build_examples_linux_${RUN_ID}.log"

FPC="${FPC:-$(command -v fpc || true)}"
if [ -z "$FPC" ]; then
  FPC_CANDIDATES=(
    "$HOME/freePascal/fpc/bin/x86_64-linux/fpc"
    "/usr/local/bin/fpc"
    "/usr/bin/fpc"
  )
  for candidate in "${FPC_CANDIDATES[@]}"; do
    if [ -x "$candidate" ]; then
      FPC="$candidate"
      break
    fi
  done
fi

if [ -z "$FPC" ] || [ ! -x "$FPC" ]; then
  echo "[FAIL] Free Pascal compiler not found"
  exit 1
fi

FPC_VERSION="$("$FPC" -iV 2>/dev/null || true)"
FPC_UNITS="${FPC_UNITS:-}"
if [ -z "$FPC_UNITS" ]; then
  FPC_UNITS_CANDIDATES=()
  if [ -n "$FPC_VERSION" ]; then
    FPC_UNITS_CANDIDATES+=("/usr/lib/fpc/${FPC_VERSION}/units/x86_64-linux")
    FPC_UNITS_CANDIDATES+=("/usr/lib/x86_64-linux-gnu/fpc/${FPC_VERSION}/units/x86_64-linux")
  fi
  FPC_UNITS_CANDIDATES+=("$HOME/freePascal/fpc/units/x86_64-linux")
  for candidate in "${FPC_UNITS_CANDIDATES[@]}"; do
    if [ -d "$candidate" ]; then
      FPC_UNITS="$candidate"
      break
    fi
  done
fi

UNIT_PATHS=("-Fusrc")
if [ -n "$FPC_UNITS" ]; then
  for subdir in "rtl-objpas" "fcl-base"; do
    unit_dir="$FPC_UNITS/$subdir"
    if [ -d "$unit_dir" ]; then
      UNIT_PATHS+=("-Fu$unit_dir")
    fi
  done
fi

echo "Building examples (*.lpr/*.pas) → $OUT_DIR"
echo "Using FPC: $FPC"
if [ -n "$FPC_UNITS" ]; then
  echo "FPC units root: $FPC_UNITS"
else
  echo "FPC units root: <auto not found, using compiler defaults>"
fi

shopt -s nullglob

count=0
for src in examples/*.lpr examples/*.pas; do
  base=$(basename "$src")
  name="${base%.*}"
  echo "[+] Compiling $src"
  if "$FPC" "${UNIT_PATHS[@]}" -FE"$OUT_DIR" "$src" >"$BUILD_LOG" 2>&1; then
    echo "    OK -> $OUT_DIR/$name"
    count=$((count+1))
  else
    echo "    SKIP (build failed)"
    head -5 "$BUILD_LOG" || true
  fi
done

echo "Done. Built $count example(s)."

