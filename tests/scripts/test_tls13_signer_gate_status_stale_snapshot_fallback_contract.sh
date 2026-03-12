#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/export_tls13_signer_gate_status_json.sh"
WORK_REL="tmp/test_tls13_signer_status_stale_snapshot_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
REPORTS_REL="$WORK_REL/tls13_reports"
REPORTS_DIR="$ROOT_DIR/$REPORTS_REL"
TARGET_RUN="tls13_status_target_$$"
OTHER_RUN="tls13_status_other_$$"

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
    cat "$file" || true
    exit 1
  fi
}

assert_not_contains() {
  local file="$1"
  local pattern="$2"
  if rg -F --quiet -- "$pattern" "$file"; then
    echo "[FAIL] unexpected pattern present: $pattern"
    cat "$file" || true
    exit 1
  fi
}

echo "[TEST] tls13 signer gate status stale snapshot fallback contract"

mkdir -p "$REPORTS_DIR"

cat > "$REPORTS_DIR/wave_b_ci_gate_summary_tls13_signer_${TARGET_RUN}.md" <<EOF_SUMMARY
# Wave B CI Gate Summary

- Run ID: ${TARGET_RUN}
- Overall Status: **PASS**

## Gate Steps

| step | description | status | notes |
|------|-------------|--------|-------|
| tls13_signer_purity | purity | **PASS** | target |
| tls13_servercertverify_bench | bench | **PASS** | target |
EOF_SUMMARY

cat > "$REPORTS_DIR/wave_b_tls13_signer_${TARGET_RUN}.json" <<'EOF_BENCH'
{
  "bench_scheme": "rsa_pkcs1_sha256",
  "bench_iterations": 2,
  "bench_warmup": 1,
  "crt_avg_ms": 10.5,
  "d_avg_ms": 3.1,
  "speedup_d_over_crt": 3.39
}
EOF_BENCH

sleep 1

cat > "$REPORTS_DIR/tls13_signer_gate_snapshot_${OTHER_RUN}.md" <<EOF_SNAPSHOT
# TLS13 Signer Gate Snapshot

- run_id: ${OTHER_RUN}
- snapshot_state: **ATTENTION**
EOF_SNAPSHOT

FAFAFA_TLS13_SIGNER_GATE_REPORTS_DIR="$REPORTS_REL" \
  bash "$SCRIPT" --run-id "$TARGET_RUN" >/dev/null

STATUS_FILE="$REPORTS_DIR/tls13_signer_gate_status_${TARGET_RUN}.json"
[[ -f "$STATUS_FILE" ]] || fail "status json should be generated"

assert_contains "$STATUS_FILE" '"summary_overall": "PASS"'
assert_contains "$STATUS_FILE" '"bench_status": "PASS"'
assert_contains "$STATUS_FILE" '"snapshot_state": "MISSING"'
assert_contains "$STATUS_FILE" '"overall_state": "ATTENTION"'
assert_contains "$STATUS_FILE" "\"summary\": \"$REPORTS_REL/wave_b_ci_gate_summary_tls13_signer_${TARGET_RUN}.md\""
assert_contains "$STATUS_FILE" "\"bench_json\": \"$REPORTS_REL/wave_b_tls13_signer_${TARGET_RUN}.json\""
assert_not_contains "$STATUS_FILE" "$REPORTS_REL/tls13_signer_gate_snapshot_${OTHER_RUN}.md"

echo "[PASS] tls13 signer gate status stale snapshot fallback contract passed"
