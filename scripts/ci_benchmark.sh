#!/bin/bash
#
# CI Performance Regression Detection Script
#
# Usage:
#   ./ci_benchmark.sh [baseline_file]
#
# Exit codes:
#   0 - No regression detected
#   1 - Regression detected (>15% slowdown)
#   2 - Build/execution error
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
BENCHMARK_DIR="$PROJECT_ROOT/tests/benchmarks"
BASELINE_FILE="${1:-$BENCHMARK_DIR/baseline_reference.json}"
CURRENT_RESULTS="$BENCHMARK_DIR/current_results.json"
REGRESSION_THRESHOLD=15  # Percentage

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "================================================"
echo "  fafafa.ssl CI Performance Regression Check"
echo "================================================"
echo ""

# Step 1: Build benchmark if needed
if [ ! -f "$BENCHMARK_DIR/bin/benchmark_ssl" ]; then
    echo "Building benchmark suite..."
    mkdir -p "$BENCHMARK_DIR/bin"
    cd "$PROJECT_ROOT"
    /home/dtamade/freePascal/fpc/bin/x86_64-linux/fpc \
        -Fi/home/dtamade/freePascal/fpc/units/x86_64-linux \
        -Fi/home/dtamade/freePascal/fpc/units/x86_64-linux/rtl \
        -Fu/home/dtamade/freePascal/fpc/units/x86_64-linux \
        -Fu/home/dtamade/freePascal/fpc/units/x86_64-linux/rtl \
        -Fu/home/dtamade/freePascal/fpc/units/x86_64-linux/rtl-objpas \
        -Fu"$BENCHMARK_DIR" \
        -FE"$BENCHMARK_DIR/bin" \
        -O2 \
        "$BENCHMARK_DIR/benchmark_ssl.pas" || exit 2
    echo "Build complete."
    echo ""
fi

# Step 2: Run benchmark
echo "Running benchmarks (500 iterations)..."
cd "$PROJECT_ROOT"
"$BENCHMARK_DIR/bin/benchmark_ssl" 500 > /dev/null 2>&1

# Find the generated baseline file
GENERATED=$(ls -t baseline_*.json 2>/dev/null | head -1)
if [ -z "$GENERATED" ]; then
    echo -e "${RED}ERROR: No baseline generated${NC}"
    exit 2
fi
mv "$GENERATED" "$CURRENT_RESULTS"
echo "Results saved to: $CURRENT_RESULTS"
echo ""

# Step 3: Compare with baseline if exists
if [ ! -f "$BASELINE_FILE" ]; then
    echo -e "${YELLOW}WARNING: No baseline file found at $BASELINE_FILE${NC}"
    echo "Creating initial baseline..."
    cp "$CURRENT_RESULTS" "$BASELINE_FILE"
    echo -e "${GREEN}Initial baseline created. No regression check performed.${NC}"
    exit 0
fi

# Step 4: Parse and compare results
echo "Comparing with baseline..."
echo ""

# Simple JSON comparison using Python (available on most CI systems)
python3 << EOF
import json
import sys

with open('$BASELINE_FILE', 'r') as f:
    baseline = json.load(f)

with open('$CURRENT_RESULTS', 'r') as f:
    current = json.load(f)

baseline_tests = {t['name']: t for t in baseline.get('tests', [])}
current_tests = {t['name']: t for t in current.get('tests', [])}

threshold = $REGRESSION_THRESHOLD / 100.0
regressions = []

print(f"{'Test':<25} {'Baseline':<12} {'Current':<12} {'Delta':<10} {'Status':<10}")
print("-" * 75)

for name, curr in current_tests.items():
    if name not in baseline_tests:
        print(f"{name:<25} {'N/A':<12} {curr['mean_ms']:<12.3f} {'NEW':<10} {'OK':<10}")
        continue

    base = baseline_tests[name]
    base_mean = base['mean_ms']
    curr_mean = curr['mean_ms']

    if base_mean > 0:
        delta = (curr_mean - base_mean) / base_mean
    else:
        delta = 0

    delta_pct = delta * 100

    if delta > threshold:
        status = "REGRESS"
        regressions.append((name, base_mean, curr_mean, delta_pct))
    elif delta < -threshold:
        status = "FASTER"
    else:
        status = "OK"

    print(f"{name:<25} {base_mean:<12.3f} {curr_mean:<12.3f} {delta_pct:>+8.1f}% {status:<10}")

print()

if regressions:
    print("\033[0;31m=== REGRESSIONS DETECTED ===\033[0m")
    for name, base, curr, delta in regressions:
        print(f"  {name}: {base:.3f}ms -> {curr:.3f}ms ({delta:+.1f}%)")
    sys.exit(1)
else:
    print("\033[0;32m=== NO REGRESSIONS ===\033[0m")
    sys.exit(0)
EOF

exit_code=$?

# Step 5: Output result
echo ""
if [ $exit_code -eq 0 ]; then
    echo -e "${GREEN}Performance check passed!${NC}"
else
    echo -e "${RED}Performance regression detected!${NC}"
fi

exit $exit_code
