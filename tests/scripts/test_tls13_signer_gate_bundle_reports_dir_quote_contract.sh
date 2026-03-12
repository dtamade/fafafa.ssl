#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_tls13_signer_gate_bundle_quote_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
FAKE_BIN_DIR="$WORK_DIR/fakebin"
REPORTS_REL="$WORK_REL/reports' dir"
REPORTS_DIR="$ROOT_DIR/$REPORTS_REL"
RUN_ID="tls13_signer_quote_$$"
LEGACY_DIR="$ROOT_DIR/test-reports"

cleanup() {
  rm -rf "$WORK_DIR"
  rm -f \
    "$LEGACY_DIR/tls13_signer_gate_bundle_${RUN_ID}.md" \
    "$LEGACY_DIR/tls13_signer_gate_snapshot_${RUN_ID}.md" \
    "$LEGACY_DIR/tls13_signer_gate_status_${RUN_ID}.json"
}
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] tls13 signer gate bundle quoted-path contract"

mkdir -p "$FAKE_BIN_DIR"
cat > "$FAKE_BIN_DIR/bash" <<'EOF_FAKE_BASH'
#!/bin/bash
set -euo pipefail
script="$1"
shift || true
case "$script" in
  scripts/run_tls13_signer_gate_ci.sh)
    out_dir="${FAFAFA_TLS13_SIGNER_GATE_OUTPUT_DIR:?}"
    run_id="${FAFAFA_TLS13_SIGNER_GATE_RUN_ID:?}"
    mkdir -p "$out_dir"
    cat > "$out_dir/wave_b_ci_gate_summary_tls13_signer_${run_id}.md" <<EOF_SUMMARY
# Wave B CI Gate Summary

- Run ID: ${run_id}
- Overall Status: **PASS**

## Gate Steps

| step | description | status | notes |
|------|-------------|--------|-------|
| tls13_signer_purity | purity | **PASS** | clean |
| tls13_servercertverify_bench | bench | **PASS** | ok |
EOF_SUMMARY
    cat > "$out_dir/wave_b_tls13_signer_${run_id}.json" <<EOF_BENCH
{
  "bench_scheme": "rsa_pkcs1_sha256",
  "bench_iterations": 2,
  "bench_warmup": 1,
  "crt_avg_ms": 10.5,
  "d_avg_ms": 3.1,
  "speedup_d_over_crt": 3.39
}
EOF_BENCH
    cat > "$out_dir/tls13_signer_bench_history_${run_id}.md" <<EOF_HISTORY
# TLS13 Signer Bench History

- generated_at: 2026-03-09 00:00:00 +0800
- count: 1
EOF_HISTORY
    ;;
  *)
    exec /bin/bash "$script" "$@"
    ;;
esac
EOF_FAKE_BASH
chmod +x "$FAKE_BIN_DIR/bash"

if ! PATH="$FAKE_BIN_DIR:$PATH" FAFAFA_TLS13_SIGNER_GATE_ARCHIVE=0 /bin/bash scripts/run_tls13_signer_gate_bundle.sh \
  --run-id "$RUN_ID" \
  --reports-dir "$REPORTS_REL" \
  --strict; then
  find "$WORK_DIR" -maxdepth 3 -type f | sort || true
  if [[ -f "$REPORTS_DIR/tls13_signer_gate_ci_${RUN_ID}.log" ]]; then
    sed -n '1,160p' "$REPORTS_DIR/tls13_signer_gate_ci_${RUN_ID}.log" || true
  fi
  if [[ -f "$REPORTS_DIR/tls13_signer_gate_snapshot_${RUN_ID}.log" ]]; then
    sed -n '1,160p' "$REPORTS_DIR/tls13_signer_gate_snapshot_${RUN_ID}.log" || true
  fi
  if [[ -f "$REPORTS_DIR/tls13_signer_gate_status_${RUN_ID}.log" ]]; then
    sed -n '1,160p' "$REPORTS_DIR/tls13_signer_gate_status_${RUN_ID}.log" || true
  fi
  fail "bundle should accept reports-dir paths containing single quotes"
fi

for suffix in \
  "wave_b_ci_gate_summary_tls13_signer_${RUN_ID}.md" \
  "wave_b_tls13_signer_${RUN_ID}.json" \
  "tls13_signer_bench_history_${RUN_ID}.md" \
  "tls13_signer_gate_snapshot_${RUN_ID}.md" \
  "tls13_signer_gate_status_${RUN_ID}.json" \
  "tls13_signer_gate_bundle_${RUN_ID}.md"; do
  [[ -f "$REPORTS_DIR/$suffix" ]] || fail "missing expected TLS13 bundle artifact under quoted reports dir: $suffix"
done

if ! rg -F --quiet -- '- overall: **PASS**' "$REPORTS_DIR/tls13_signer_gate_bundle_${RUN_ID}.md"; then
  sed -n '1,220p' "$REPORTS_DIR/tls13_signer_gate_bundle_${RUN_ID}.md" || true
  fail "bundle report should record PASS under quoted reports dir"
fi

if ! rg -F --quiet -- '- overall_state: **HEALTHY**' "$REPORTS_DIR/tls13_signer_gate_bundle_${RUN_ID}.md"; then
  sed -n '1,220p' "$REPORTS_DIR/tls13_signer_gate_bundle_${RUN_ID}.md" || true
  fail "bundle report should record HEALTHY overall_state under quoted reports dir"
fi

if ! rg -F --quiet -- '"overall_state": "HEALTHY"' "$REPORTS_DIR/tls13_signer_gate_status_${RUN_ID}.json"; then
  cat "$REPORTS_DIR/tls13_signer_gate_status_${RUN_ID}.json" || true
  fail "status json should remain HEALTHY under quoted reports dir"
fi

[[ ! -f "$LEGACY_DIR/tls13_signer_gate_bundle_${RUN_ID}.md" ]] || fail "bundle report should not be written under test-reports"
[[ ! -f "$LEGACY_DIR/tls13_signer_gate_snapshot_${RUN_ID}.md" ]] || fail "snapshot should not be written under test-reports"
[[ ! -f "$LEGACY_DIR/tls13_signer_gate_status_${RUN_ID}.json" ]] || fail "status json should not be written under test-reports"

echo "[PASS] tls13 signer gate bundle quoted-path contract passed"
