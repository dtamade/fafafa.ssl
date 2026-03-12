#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_tls13_signer_gate_cli_reports_dir_passthrough_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
REPORTS_REL="$WORK_REL/reports dir"
REPORTS_DIR="$ROOT_DIR/$REPORTS_REL"
RUN_ID="tls13_cli_reports_dir_$$"
LEGACY_DIR="$ROOT_DIR/tmp/tls13_signer_gate_reports"

cleanup() {
  rm -rf "$WORK_DIR"
  rm -f "$LEGACY_DIR/tls13_signer_gate_snapshot_${RUN_ID}.md" "$LEGACY_DIR/tls13_signer_gate_status_${RUN_ID}.json"
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
    sed -n '1,260p' "$file" || true
    exit 1
  fi
}

echo "[TEST] tls13 signer gate cli reports-dir passthrough contract"

mkdir -p "$REPORTS_DIR"

cat > "$REPORTS_DIR/wave_b_ci_gate_summary_tls13_signer_${RUN_ID}.md" <<EOF_SUMMARY
# Wave B CI Gate Summary

- Run ID: $RUN_ID
- Overall Status: **PASS**

| stage | item | status | notes |
|------|------|--------|-------|
| gate | tls13_signer_purity | **PASS** | ok |
| gate | tls13_servercertverify_bench | **PASS** | ok |
EOF_SUMMARY

cat > "$REPORTS_DIR/wave_b_tls13_signer_${RUN_ID}.json" <<EOF_JSON
{
  "bench_scheme": "rsa_pss_rsae_sha256",
  "bench_iterations": 200,
  "bench_warmup": 20,
  "crt_avg_ms": 1.10,
  "d_avg_ms": 0.70,
  "speedup_d_over_crt": 1.57,
  "overall_state": "HEALTHY"
}
EOF_JSON

cat > "$REPORTS_DIR/tls13_signer_bench_history_${RUN_ID}.md" <<EOF_HISTORY
# TLS13 signer bench history

- run_id: $RUN_ID
EOF_HISTORY

cd "$ROOT_DIR"
bash scripts/generate_tls13_signer_gate_snapshot.sh --run-id "$RUN_ID" --reports-dir "$REPORTS_REL"
bash scripts/export_tls13_signer_gate_status_json.sh --run-id "$RUN_ID" --reports-dir "$REPORTS_REL"

SNAPSHOT_FILE="$REPORTS_DIR/tls13_signer_gate_snapshot_${RUN_ID}.md"
STATUS_FILE="$REPORTS_DIR/tls13_signer_gate_status_${RUN_ID}.json"
[[ -f "$SNAPSHOT_FILE" ]] || fail "snapshot should be generated under cli reports dir"
[[ -f "$STATUS_FILE" ]] || fail "status json should be generated under cli reports dir"
[[ ! -f "$LEGACY_DIR/tls13_signer_gate_snapshot_${RUN_ID}.md" ]] || fail "snapshot should not be written under default reports dir"
[[ ! -f "$LEGACY_DIR/tls13_signer_gate_status_${RUN_ID}.json" ]] || fail "status should not be written under default reports dir"

assert_contains "$SNAPSHOT_FILE" "- summary: $REPORTS_REL/wave_b_ci_gate_summary_tls13_signer_${RUN_ID}.md"
assert_contains "$SNAPSHOT_FILE" "- bench_json: $REPORTS_REL/wave_b_tls13_signer_${RUN_ID}.json"
assert_contains "$SNAPSHOT_FILE" "- history: $REPORTS_REL/tls13_signer_bench_history_${RUN_ID}.md"
assert_contains "$STATUS_FILE" '"summary": "'"$REPORTS_REL"'/wave_b_ci_gate_summary_tls13_signer_'"$RUN_ID"'.md"'
assert_contains "$STATUS_FILE" '"snapshot": "'"$REPORTS_REL"'/tls13_signer_gate_snapshot_'"$RUN_ID"'.md"'
assert_contains "$STATUS_FILE" '"bench_json": "'"$REPORTS_REL"'/wave_b_tls13_signer_'"$RUN_ID"'.json"'

echo "[PASS] tls13 signer gate cli reports-dir passthrough contract passed"
