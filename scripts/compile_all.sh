#!/bin/bash
#
# compile_all.sh - Compile all fafafa.ssl Pascal sources
#
# Usage: ./scripts/compile_all.sh
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
SRC_DIR="$PROJECT_DIR/src"
TESTS_DIR="$PROJECT_DIR/tests"
OUTPUT_DIR="$TESTS_DIR/bin"

# Create output directories
mkdir -p "$OUTPUT_DIR"
mkdir -p "$TESTS_DIR/security/bin"
mkdir -p "$TESTS_DIR/integration/bin"

# Find FPC
FPC=$(which fpc 2>/dev/null || echo "/home/dtamade/freePascal/fpc/bin/x86_64-linux/fpc")

if [ ! -x "$FPC" ]; then
    echo "ERROR: Free Pascal compiler not found"
    exit 1
fi

echo "Using FPC: $FPC"
echo "FPC Version: $($FPC -iV)"
echo ""

# Compiler flags
FPC_FLAGS="-Fusrc -Fu$OUTPUT_DIR -FE$OUTPUT_DIR -O2"

# Track results
COMPILED=0
FAILED=0

# Compile source files
echo "=== Compiling source files ==="
for file in "$SRC_DIR"/*.pas; do
    if [ -f "$file" ]; then
        basename=$(basename "$file")
        echo -n "  Compiling $basename... "
        if $FPC $FPC_FLAGS "$file" > /dev/null 2>&1; then
            echo "OK"
            COMPILED=$((COMPILED + 1))
        else
            echo "FAILED"
            FAILED=$((FAILED + 1))
        fi
    fi
done

# Compile test files
echo ""
echo "=== Compiling test files ==="
for file in "$TESTS_DIR"/*.pas "$TESTS_DIR"/security/*.pas; do
    if [ -f "$file" ]; then
        basename=$(basename "$file")
        dirname=$(dirname "$file")
        outdir="$dirname/bin"
        mkdir -p "$outdir"
        echo -n "  Compiling $basename... "
        if $FPC -Fusrc -Fu"$OUTPUT_DIR" -FE"$outdir" -O2 "$file" > /dev/null 2>&1; then
            echo "OK"
            COMPILED=$((COMPILED + 1))
        else
            echo "FAILED"
            FAILED=$((FAILED + 1))
        fi
    fi
done

# Summary
echo ""
echo "=== Compilation Summary ==="
echo "Compiled: $COMPILED"
echo "Failed:   $FAILED"

# In CI mode, don't fail on compilation errors (some files need platform-specific libs)
if [ -n "$CI" ]; then
    echo ""
    echo "CI mode: Compilation errors are non-fatal"
    exit 0
fi

if [ $FAILED -gt 0 ]; then
    exit 1
fi

exit 0
