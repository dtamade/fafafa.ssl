#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_ci_gate_mixed_output_priority_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
FAKE_BIN_DIR="$WORK_DIR/fakebin"
REPORTS_REL="$WORK_REL/reports"
REPORTS_DIR="$ROOT_DIR/$REPORTS_REL"
CUSTOM_REL="$WORK_REL/custom/nested"
EXAMPLES_REL="$CUSTOM_REL/examples_override.json"
SUMMARY_REL="$CUSTOM_REL/summary_override.md"
BENCH_REL="$CUSTOM_REL/bench_override.json"
EXAMPLES_FILE="$ROOT_DIR/$EXAMPLES_REL"
SUMMARY_FILE="$ROOT_DIR/$SUMMARY_REL"
BENCH_FILE="$ROOT_DIR/$BENCH_REL"
RUN_ID="wave_b_mixed_output_$$"

cleanup() {
  rm -rf "$WORK_DIR"
}
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local file="$1"
  local pattern="$2"
  if ! rg -F --quiet -- "$pattern" "$file"; then
    echo "[FAIL] missing expected pattern: $pattern"
    sed -n '1,320p' "$file" || cat "$file" || true
    exit 1
  fi
}

echo "[TEST] wave b ci gate mixed output priority contract"

mkdir -p "$FAKE_BIN_DIR"
cat > "$FAKE_BIN_DIR/fpc" <<'EOF_FAKE_FPC'
#!/bin/bash
set -euo pipefail
if [[ "${1:-}" == "-iV" ]]; then
  echo "3.2.2"
  exit 0
fi
exit 0
EOF_FAKE_FPC
chmod +x "$FAKE_BIN_DIR/fpc"

cat > "$FAKE_BIN_DIR/bash" <<'EOF_FAKE_BASH'
#!/bin/bash
set -euo pipefail
script="$1"
shift || true
case "$script" in
  scripts/check_tls13_signer_pure_pascal.sh)
    echo "TLS13_SIGNER_PURITY PASS"
    ;;
  scripts/run_freepascal_tls13_servercertverify_bench.sh)
    json_out="${FAFAFA_TLS13_SIGN_BENCH_JSON_OUT:?}"
    mkdir -p "$(dirname "$json_out")"
    cat > "$json_out" <<EOF_JSON
{
  "bench_scheme": "rsa_pkcs1_sha256",
  "bench_iterations": 2,
  "bench_warmup": 1,
  "crt_avg_ms": 10.5,
  "d_avg_ms": 3.1,
  "speedup_d_over_crt": 3.39
}
EOF_JSON
    echo "CRT_avg_ms=10.5"
    echo "D_avg_ms=3.1"
    echo "Speedup_D_over_CRT=3.39"
    ;;
  *)
    exec /bin/bash "$script" "$@"
    ;;
esac
EOF_FAKE_BASH
chmod +x "$FAKE_BIN_DIR/bash"

PATH="$FAKE_BIN_DIR:$PATH" \
FAFAFA_WAVE_B_CI_GATE_RUN_ID="$RUN_ID" \
/bin/bash scripts/run_wave_b_ci_gate.sh \
  --skip-compile \
  --skip-modules \
  --reports-dir "$REPORTS_REL" \
  --examples-report "$EXAMPLES_REL" \
  --summary-out "$SUMMARY_REL" \
  --with-tls13-sign-purity-check \
  --with-tls13-sign-bench \
  --tls13-sign-bench-json-out "$BENCH_REL" >/dev/null

[[ -f "$EXAMPLES_FILE" ]] || fail "explicit examples report should be generated"
[[ -f "$SUMMARY_FILE" ]] || fail "explicit summary should be generated"
[[ -f "$BENCH_FILE" ]] || fail "explicit bench json should be generated"
[[ -f "$REPORTS_DIR/wave_b_examples_${RUN_ID}.log" ]] || fail "examples log should still be written under reports dir"
[[ -f "$REPORTS_DIR/wave_b_tls13_sign_bench_${RUN_ID}.log" ]] || fail "bench log should still be written under reports dir"
[[ -f "$REPORTS_DIR/examples_compile_ci_gate_${RUN_ID}.json" ]] || fail "run-scoped copy should still be written under reports dir"
[[ -f "$REPORTS_DIR/examples-compile-history/examples_compile_ci_gate_${RUN_ID}.json" ]] || fail "archive copy should still be written under reports dir"

assert_contains "$SUMMARY_FILE" "- Report: \`$EXAMPLES_REL\`"
assert_contains "$SUMMARY_FILE" "- Current Alias: \`$EXAMPLES_REL\`"
assert_contains "$SUMMARY_FILE" "- Run-Scoped Copy: \`$REPORTS_REL/examples_compile_ci_gate_${RUN_ID}.json\`"
assert_contains "$SUMMARY_FILE" "- Archive Copy: \`$REPORTS_REL/examples-compile-history/examples_compile_ci_gate_${RUN_ID}.json\`"
assert_contains "$SUMMARY_FILE" "- JSON: \`$BENCH_REL\`"

[[ ! -f "$REPORTS_DIR/wave_b_ci_gate_summary_${RUN_ID}.md" ]] || fail "default summary path under reports dir should not be used when summary-out overrides it"
[[ ! -f "$REPORTS_DIR/wave_b_tls13_signer_${RUN_ID}.json" ]] || fail "default bench json path under reports dir should not be used when explicit bench path overrides it"

assert_contains "$EXAMPLES_FILE" "\"run_id\": \"$RUN_ID\""

echo "[PASS] wave b ci gate mixed output priority contract passed"
