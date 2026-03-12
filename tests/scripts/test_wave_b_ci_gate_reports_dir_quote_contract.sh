#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_ci_gate_quote_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
FAKE_BIN_DIR="$WORK_DIR/fakebin"
REPORTS_REL="$WORK_REL/reports' dir"
REPORTS_DIR="$ROOT_DIR/$REPORTS_REL"
RUN_ID="wave_b_quote_$$"
SUMMARY_FILE="$REPORTS_DIR/wave_b_ci_gate_summary_${RUN_ID}.md"
BENCH_JSON_FILE="$REPORTS_DIR/wave_b_tls13_signer_${RUN_ID}.json"
LEGACY_DIR="$ROOT_DIR/test-reports"

cleanup() {
  rm -rf "$WORK_DIR"
  rm -f \
    "$LEGACY_DIR/wave_b_ci_gate_summary_${RUN_ID}.md" \
    "$LEGACY_DIR/wave_b_tls13_signer_${RUN_ID}.json"
}
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] wave b ci gate quoted-path contract"

mkdir -p "$FAKE_BIN_DIR"
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

if ! PATH="$FAKE_BIN_DIR:$PATH" \
  FAFAFA_WAVE_B_CI_GATE_RUN_ID="$RUN_ID" \
  FAFAFA_WAVE_B_REPORTS_DIR="$REPORTS_REL" \
  /bin/bash scripts/run_wave_b_ci_gate.sh \
    --only-tls13-sign-bench \
    --with-tls13-sign-purity-check; then
  find "$WORK_DIR" -maxdepth 3 -type f | sort || true
  if [[ -f "$REPORTS_DIR/wave_b_tls13_sign_purity_${RUN_ID}.log" ]]; then
    sed -n '1,160p' "$REPORTS_DIR/wave_b_tls13_sign_purity_${RUN_ID}.log" || true
  fi
  if [[ -f "$REPORTS_DIR/wave_b_tls13_sign_bench_${RUN_ID}.log" ]]; then
    sed -n '1,160p' "$REPORTS_DIR/wave_b_tls13_sign_bench_${RUN_ID}.log" || true
  fi
  fail "run_wave_b_ci_gate should accept reports-dir paths containing single quotes"
fi

[[ -f "$SUMMARY_FILE" ]] || fail "summary should be generated under quoted reports dir"
[[ -f "$BENCH_JSON_FILE" ]] || fail "bench json should be generated under quoted reports dir"

if ! rg -F --quiet -- '- Overall Status: **PASS**' "$SUMMARY_FILE"; then
  sed -n '1,260p' "$SUMMARY_FILE" || true
  fail "summary should record PASS under quoted reports dir"
fi

if ! rg -F --quiet -- 'tls13_signer_purity | `0` | **PASS**' "$SUMMARY_FILE"; then
  sed -n '1,260p' "$SUMMARY_FILE" || true
  fail "purity step should pass under quoted reports dir"
fi

if ! rg -F --quiet -- 'tls13_servercertverify_bench | `0` | **PASS**' "$SUMMARY_FILE"; then
  sed -n '1,260p' "$SUMMARY_FILE" || true
  fail "bench step should pass under quoted reports dir"
fi

[[ ! -f "$LEGACY_DIR/wave_b_ci_gate_summary_${RUN_ID}.md" ]] || fail "summary should not be written under test-reports"
[[ ! -f "$LEGACY_DIR/wave_b_tls13_signer_${RUN_ID}.json" ]] || fail "bench json should not be written under test-reports"

echo "[PASS] wave b ci gate quoted-path contract passed"
