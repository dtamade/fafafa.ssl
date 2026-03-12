#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_tls13_signer_gate_run_id_scoped_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
REPORTS_REL="$WORK_REL/tls13_reports"
REPORTS_DIR="$ROOT_DIR/$REPORTS_REL"
TARGET_RUN="tls13_target_$$"
OTHER_RUN="tls13_other_$$"

cleanup() {
  rm -rf "$WORK_DIR"
}
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] tls13 signer gate run-id scoped default selection contract"

mkdir -p "$REPORTS_DIR"

cat > "$REPORTS_DIR/wave_b_ci_gate_summary_tls13_signer_${TARGET_RUN}.md" <<EOF_TARGET_SUMMARY
# Wave B CI Gate Summary

- Run ID: ${TARGET_RUN}
- Overall Status: **PASS**

## Gate Steps

| step | description | status | notes |
|------|-------------|--------|-------|
| tls13_signer_purity | purity | **PASS** | target |
| tls13_servercertverify_bench | bench | **PASS** | target |
EOF_TARGET_SUMMARY

cat > "$REPORTS_DIR/wave_b_tls13_signer_${TARGET_RUN}.json" <<'EOF_TARGET_BENCH'
{
  "bench_scheme": "rsa_pkcs1_sha256",
  "bench_iterations": 2,
  "bench_warmup": 1,
  "crt_avg_ms": 10.5,
  "d_avg_ms": 3.1,
  "speedup_d_over_crt": 3.39
}
EOF_TARGET_BENCH

cat > "$REPORTS_DIR/tls13_signer_bench_history_${TARGET_RUN}.md" <<EOF_TARGET_HISTORY
# TLS13 Signer Bench History

- run_id: ${TARGET_RUN}
- count: 1
EOF_TARGET_HISTORY

sleep 1

cat > "$REPORTS_DIR/wave_b_ci_gate_summary_tls13_signer_${OTHER_RUN}.md" <<EOF_OTHER_SUMMARY
# Wave B CI Gate Summary

- Run ID: ${OTHER_RUN}
- Overall Status: **FAIL**

## Gate Steps

| step | description | status | notes |
|------|-------------|--------|-------|
| tls13_signer_purity | purity | **FAIL** | distractor |
| tls13_servercertverify_bench | bench | **FAIL** | distractor |
EOF_OTHER_SUMMARY

cat > "$REPORTS_DIR/wave_b_tls13_signer_${OTHER_RUN}.json" <<'EOF_OTHER_BENCH'
{
  "bench_scheme": "rsa_pkcs1_sha256",
  "bench_iterations": 9,
  "bench_warmup": 4,
  "crt_avg_ms": 99.9,
  "d_avg_ms": 99.9,
  "speedup_d_over_crt": 1.00
}
EOF_OTHER_BENCH

cat > "$REPORTS_DIR/tls13_signer_bench_history_${OTHER_RUN}.md" <<EOF_OTHER_HISTORY
# TLS13 Signer Bench History

- run_id: ${OTHER_RUN}
- count: 9
EOF_OTHER_HISTORY

FAFAFA_TLS13_SIGNER_GATE_REPORTS_DIR="$REPORTS_REL" \
  bash scripts/generate_tls13_signer_gate_snapshot.sh --run-id "$TARGET_RUN"

SNAPSHOT_FILE="$REPORTS_DIR/tls13_signer_gate_snapshot_${TARGET_RUN}.md"
[[ -f "$SNAPSHOT_FILE" ]] || fail "target snapshot should be generated"

if ! rg -F --quiet -- "- summary: $REPORTS_REL/wave_b_ci_gate_summary_tls13_signer_${TARGET_RUN}.md" "$SNAPSHOT_FILE"; then
  sed -n '1,220p' "$SNAPSHOT_FILE" || true
  fail "snapshot should default to run-id scoped summary"
fi

if ! rg -F --quiet -- "- bench_json: $REPORTS_REL/wave_b_tls13_signer_${TARGET_RUN}.json" "$SNAPSHOT_FILE"; then
  sed -n '1,220p' "$SNAPSHOT_FILE" || true
  fail "snapshot should default to run-id scoped bench json"
fi

if ! rg -F --quiet -- "- history: $REPORTS_REL/tls13_signer_bench_history_${TARGET_RUN}.md" "$SNAPSHOT_FILE"; then
  sed -n '1,220p' "$SNAPSHOT_FILE" || true
  fail "snapshot should default to run-id scoped history"
fi

if ! rg -F --quiet -- '- snapshot_state: **GREEN**' "$SNAPSHOT_FILE"; then
  sed -n '1,220p' "$SNAPSHOT_FILE" || true
  fail "target snapshot should stay GREEN despite newer distractor artifacts"
fi

sleep 1
cat > "$REPORTS_DIR/tls13_signer_gate_snapshot_${OTHER_RUN}.md" <<EOF_OTHER_SNAPSHOT
# TLS13 Signer Gate Snapshot

- run_id: ${OTHER_RUN}
- snapshot_state: **ATTENTION**
EOF_OTHER_SNAPSHOT

FAFAFA_TLS13_SIGNER_GATE_REPORTS_DIR="$REPORTS_REL" \
  bash scripts/export_tls13_signer_gate_status_json.sh --run-id "$TARGET_RUN"

STATUS_FILE="$REPORTS_DIR/tls13_signer_gate_status_${TARGET_RUN}.json"
[[ -f "$STATUS_FILE" ]] || fail "target status json should be generated"

if ! rg -F --quiet -- '"overall_state": "HEALTHY"' "$STATUS_FILE"; then
  cat "$STATUS_FILE" || true
  fail "status export should stay HEALTHY for target run"
fi

if ! rg -F --quiet -- "\"summary\": \"$REPORTS_REL/wave_b_ci_gate_summary_tls13_signer_${TARGET_RUN}.md\"" "$STATUS_FILE"; then
  cat "$STATUS_FILE" || true
  fail "status export should default to run-id scoped summary"
fi

if ! rg -F --quiet -- "\"snapshot\": \"$REPORTS_REL/tls13_signer_gate_snapshot_${TARGET_RUN}.md\"" "$STATUS_FILE"; then
  cat "$STATUS_FILE" || true
  fail "status export should default to run-id scoped snapshot"
fi

if ! rg -F --quiet -- "\"bench_json\": \"$REPORTS_REL/wave_b_tls13_signer_${TARGET_RUN}.json\"" "$STATUS_FILE"; then
  cat "$STATUS_FILE" || true
  fail "status export should default to run-id scoped bench json"
fi

echo "[PASS] tls13 signer gate run-id scoped default selection contract passed"
