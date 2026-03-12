#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_tls13_default_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
WAVE_B_REL="$WORK_REL/wave_b_reports"
WAVE_B_DIR="$ROOT_DIR/$WAVE_B_REL"
TLS13_REL="$WORK_REL/tls13_reports"
TLS13_DIR="$ROOT_DIR/$TLS13_REL"
LEGACY_DIR="$ROOT_DIR/test-reports"

RUN_WAVE_B="wave_b_tls13_default_$$"
RUN_TLS13="tls13_signer_default_$$"

cleanup() {
  rm -rf "$WORK_DIR"
  rm -f     "$LEGACY_DIR/wave_b_cross_platform_summary_${RUN_WAVE_B}.md"     "$LEGACY_DIR/wave_b_b2_closure_readiness_${RUN_WAVE_B}.md"     "$LEGACY_DIR/wave_b_b2_evidence_consistency_${RUN_WAVE_B}.md"     "$LEGACY_DIR/wave_b_b2_handoff_bundle_${RUN_WAVE_B}.md"     "$LEGACY_DIR/tls13_signer_gate_snapshot_${RUN_TLS13}.md"     "$LEGACY_DIR/tls13_signer_gate_status_${RUN_TLS13}.json"
}
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] wave b tls13 default reports runtime contract"

mkdir -p "$WAVE_B_DIR" "$TLS13_DIR"

cat > "$WAVE_B_DIR/wave_b_ci_gate_summary_${RUN_WAVE_B}.md" <<EOF_LINUX
# Wave B CI Gate Summary

- Run ID: ${RUN_WAVE_B}
- Overall Status: **PASS**

## Gate Steps

| step | description | status | notes |
|------|-------------|--------|-------|
| compile_all_modules | compile | **PASS** | 157/157 |
| run_all_module_tests | modules | **PASS** | ok |
| verify_examples_compile | examples | **PASS** | 71/75 |
EOF_LINUX

cat > "$WAVE_B_DIR/wave_b_macos_gate_summary_${RUN_WAVE_B}.md" <<EOF_MAC
# Wave B macOS Gate Summary

- run_id: ${RUN_WAVE_B}
- overall: **PASS**
EOF_MAC

cat > "$WAVE_B_DIR/wave_b_windows_gate_summary_${RUN_WAVE_B}.md" <<EOF_WIN
# Wave B Windows Gate Summary

- run_id: ${RUN_WAVE_B}
- overall: **PASS**
EOF_WIN

cat > "$WAVE_B_DIR/examples_compile_ci_gate.json" <<'EOF_JSON'
{
  "summary": {
    "total": 75,
    "passed": 71,
    "failed": 0,
    "skipped": 4,
    "pass_rate": "94.7%"
  }
}
EOF_JSON

cd "$ROOT_DIR"
FAFAFA_WAVE_B_REPORTS_DIR="$WAVE_B_REL" bash scripts/prepare_wave_b_b2_handoff_bundle.sh   --run-id "$RUN_WAVE_B"   --linux-summary "$WAVE_B_REL/wave_b_ci_gate_summary_${RUN_WAVE_B}.md"   --linux-examples "$WAVE_B_REL/examples_compile_ci_gate.json"   --macos-summary "$WAVE_B_REL/wave_b_macos_gate_summary_${RUN_WAVE_B}.md"   --windows-summary "$WAVE_B_REL/wave_b_windows_gate_summary_${RUN_WAVE_B}.md"

[[ -f "$WAVE_B_DIR/wave_b_cross_platform_summary_${RUN_WAVE_B}.md" ]] || fail "default cross-platform summary should be written under tmp/wave_b_reports"
[[ -f "$WAVE_B_DIR/wave_b_b2_closure_readiness_${RUN_WAVE_B}.md" ]] || fail "default closure report should be written under tmp/wave_b_reports"
[[ -f "$WAVE_B_DIR/wave_b_b2_evidence_consistency_${RUN_WAVE_B}.md" ]] || fail "default consistency report should be written under tmp/wave_b_reports"
[[ -f "$WAVE_B_DIR/wave_b_b2_handoff_bundle_${RUN_WAVE_B}.md" ]] || fail "default handoff bundle should be written under tmp/wave_b_reports"

[[ ! -f "$LEGACY_DIR/wave_b_cross_platform_summary_${RUN_WAVE_B}.md" ]] || fail "default cross-platform summary should no longer be written under test-reports"
[[ ! -f "$LEGACY_DIR/wave_b_b2_closure_readiness_${RUN_WAVE_B}.md" ]] || fail "default closure report should no longer be written under test-reports"
[[ ! -f "$LEGACY_DIR/wave_b_b2_evidence_consistency_${RUN_WAVE_B}.md" ]] || fail "default consistency report should no longer be written under test-reports"
[[ ! -f "$LEGACY_DIR/wave_b_b2_handoff_bundle_${RUN_WAVE_B}.md" ]] || fail "default handoff bundle should no longer be written under test-reports"

if ! rg -F --quiet -- "$WAVE_B_REL/wave_b_cross_platform_summary_${RUN_WAVE_B}.md" "$WAVE_B_DIR/wave_b_b2_handoff_bundle_${RUN_WAVE_B}.md"; then
  sed -n '1,240p' "$WAVE_B_DIR/wave_b_b2_handoff_bundle_${RUN_WAVE_B}.md" || true
  fail "handoff bundle should point to the default cross summary under tmp/wave_b_reports"
fi

cat > "$TLS13_DIR/wave_b_ci_gate_summary_tls13_signer_${RUN_TLS13}.md" <<EOF_TLS13_SUMMARY
# Wave B CI Gate Summary

- Run ID: ${RUN_TLS13}
- Overall Status: **PASS**

## Gate Steps

| step | description | status | notes |
|------|-------------|--------|-------|
| tls13_signer_purity | purity | **PASS** | clean |
| tls13_servercertverify_bench | bench | **PASS** | speedup ok |
EOF_TLS13_SUMMARY

cat > "$TLS13_DIR/wave_b_tls13_signer_${RUN_TLS13}.json" <<'EOF_TLS13_JSON'
{
  "bench_scheme": "rsa_pkcs1_sha256",
  "bench_iterations": 2,
  "bench_warmup": 1,
  "crt_avg_ms": 10.5,
  "d_avg_ms": 3.1,
  "speedup_d_over_crt": 3.39
}
EOF_TLS13_JSON

cat > "$TLS13_DIR/tls13_signer_bench_history_${RUN_TLS13}.md" <<'EOF_TLS13_HISTORY'
# TLS13 Signer Bench History

- generated_at: 2026-03-08 00:00:00 +0000
- count: 1
EOF_TLS13_HISTORY

FAFAFA_TLS13_SIGNER_GATE_REPORTS_DIR="$TLS13_REL" bash scripts/generate_tls13_signer_gate_snapshot.sh --run-id "$RUN_TLS13"

FAFAFA_TLS13_SIGNER_GATE_REPORTS_DIR="$TLS13_REL" bash scripts/export_tls13_signer_gate_status_json.sh --run-id "$RUN_TLS13"

[[ -f "$TLS13_DIR/tls13_signer_gate_snapshot_${RUN_TLS13}.md" ]] || fail "default TLS13 snapshot should be written under tmp/tls13_signer_gate_reports"
[[ -f "$TLS13_DIR/tls13_signer_gate_status_${RUN_TLS13}.json" ]] || fail "default TLS13 status json should be written under tmp/tls13_signer_gate_reports"

[[ ! -f "$LEGACY_DIR/tls13_signer_gate_snapshot_${RUN_TLS13}.md" ]] || fail "default TLS13 snapshot should no longer be written under test-reports"
[[ ! -f "$LEGACY_DIR/tls13_signer_gate_status_${RUN_TLS13}.json" ]] || fail "default TLS13 status json should no longer be written under test-reports"

if ! rg -F --quiet -- '- snapshot_state: **GREEN**' "$TLS13_DIR/tls13_signer_gate_snapshot_${RUN_TLS13}.md"; then
  sed -n '1,220p' "$TLS13_DIR/tls13_signer_gate_snapshot_${RUN_TLS13}.md" || true
  fail "TLS13 snapshot should be GREEN with healthy fake inputs"
fi

if ! rg -F --quiet -- '"overall_state": "HEALTHY"' "$TLS13_DIR/tls13_signer_gate_status_${RUN_TLS13}.json"; then
  cat "$TLS13_DIR/tls13_signer_gate_status_${RUN_TLS13}.json" || true
  fail "TLS13 status json should be HEALTHY with healthy fake inputs"
fi

echo "[PASS] wave b tls13 default reports runtime contract passed"
