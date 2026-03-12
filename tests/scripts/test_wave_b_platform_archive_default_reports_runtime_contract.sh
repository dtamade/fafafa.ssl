#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_platform_archive_default_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
WAVE_B_REL="$WORK_REL/wave_b_reports"
WAVE_B_DIR="$ROOT_DIR/$WAVE_B_REL"
TLS13_REL="$WORK_REL/tls13_reports"
TLS13_DIR="$ROOT_DIR/$TLS13_REL"
ARCHIVE_OUT_REL="$WORK_REL/archive_out"
LEGACY_DIR="$ROOT_DIR/test-reports"

RUN_MACOS="wave_b_macos_default_$$"
RUN_ARCHIVE="wave_b_archive_default_$$"
RUN_TLS13="tls13_archive_default_$$"

cleanup() {
  rm -rf "$WORK_DIR"
  rm -f     "$LEGACY_DIR/wave_b_macos_gate_summary_${RUN_MACOS}.md"     "$LEGACY_DIR/wave_b_ci_gate_summary_${RUN_ARCHIVE}.md"     "$LEGACY_DIR/wave_b_windows_gate_summary_${RUN_ARCHIVE}.md"     "$LEGACY_DIR/winssl_blocker_batch_${RUN_ARCHIVE}.md"     "$LEGACY_DIR/tls13_signer_gate_bundle_${RUN_TLS13}.md"     "$LEGACY_DIR/tls13_signer_gate_snapshot_${RUN_TLS13}.md"     "$LEGACY_DIR/tls13_signer_gate_status_${RUN_TLS13}.json"     "$LEGACY_DIR/wave_b_tls13_sign_bench_${RUN_TLS13}.log"     "$LEGACY_DIR/wave_b_tls13_signer_${RUN_TLS13}.json"     "$LEGACY_DIR/tls13_signer_bench_history_${RUN_TLS13}.md"
}
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] wave b platform archive default reports runtime contract"

mkdir -p "$WAVE_B_DIR" "$TLS13_DIR"

cd "$ROOT_DIR"
FAFAFA_WAVE_B_REPORTS_DIR="$WAVE_B_REL" bash scripts/run_wave_b_macos_gate.sh --dry-run --run-id "$RUN_MACOS" --modules PKCS7

[[ -f "$WAVE_B_DIR/wave_b_macos_gate_summary_${RUN_MACOS}.md" ]] || fail "default macOS summary should be written under tmp/wave_b_reports"
[[ -f "$WAVE_B_DIR/wave_b_macos_probe_${RUN_MACOS}.log" ]] || fail "default macOS probe log should be written under tmp/wave_b_reports"
[[ -f "$WAVE_B_DIR/wave_b_macos_path_check_${RUN_MACOS}.log" ]] || fail "default macOS path-check log should be written under tmp/wave_b_reports"
[[ -f "$WAVE_B_DIR/wave_b_macos_compile_${RUN_MACOS}.log" ]] || fail "default macOS compile log should be written under tmp/wave_b_reports"
[[ -f "$WAVE_B_DIR/wave_b_macos_modules_${RUN_MACOS}.log" ]] || fail "default macOS modules log should be written under tmp/wave_b_reports"
[[ -f "$WAVE_B_DIR/wave_b_macos_examples_${RUN_MACOS}.log" ]] || fail "default macOS examples log should be written under tmp/wave_b_reports"
[[ ! -f "$LEGACY_DIR/wave_b_macos_gate_summary_${RUN_MACOS}.md" ]] || fail "default macOS summary should no longer be written under test-reports"

if ! rg -F --quiet -- '- overall: **DRY_RUN**' "$WAVE_B_DIR/wave_b_macos_gate_summary_${RUN_MACOS}.md"; then
  sed -n '1,220p' "$WAVE_B_DIR/wave_b_macos_gate_summary_${RUN_MACOS}.md" || true
  fail "macOS dry-run summary should report DRY_RUN overall"
fi

cat > "$WAVE_B_DIR/wave_b_ci_gate_summary_${RUN_ARCHIVE}.md" <<EOF_WAVE_B
# Wave B CI Gate Summary

- run_id: ${RUN_ARCHIVE}
- Overall Status: **PASS**
EOF_WAVE_B

cat > "$WAVE_B_DIR/wave_b_windows_gate_summary_${RUN_ARCHIVE}.md" <<EOF_WIN
# Wave B Windows Gate Summary

- run_id: ${RUN_ARCHIVE}
- overall: **PASS**
EOF_WIN

cat > "$WAVE_B_DIR/winssl_blocker_batch_${RUN_ARCHIVE}.md" <<EOF_BLOCKER
# WinSSL Blocker Batch

- run_id: ${RUN_ARCHIVE}
- overall: **PASS**
EOF_BLOCKER

cat > "$TLS13_DIR/tls13_signer_gate_bundle_${RUN_TLS13}.md" <<EOF_BUNDLE
# TLS13 Signer Gate Bundle

- run_id: ${RUN_TLS13}
EOF_BUNDLE

cat > "$TLS13_DIR/tls13_signer_gate_snapshot_${RUN_TLS13}.md" <<EOF_SNAPSHOT
# TLS13 Signer Gate Snapshot

- run_id: ${RUN_TLS13}
EOF_SNAPSHOT

cat > "$TLS13_DIR/tls13_signer_gate_status_${RUN_TLS13}.json" <<EOF_STATUS
{"run_id":"${RUN_TLS13}","overall_state":"HEALTHY"}
EOF_STATUS

cat > "$TLS13_DIR/wave_b_tls13_sign_bench_${RUN_TLS13}.log" <<EOF_BENCH
BENCH_SCHEME=rsa_pkcs1_sha256
EOF_BENCH

cat > "$TLS13_DIR/wave_b_tls13_signer_${RUN_TLS13}.json" <<EOF_BENCH_JSON
{"bench_scheme":"rsa_pkcs1_sha256"}
EOF_BENCH_JSON

cat > "$TLS13_DIR/tls13_signer_bench_history_${RUN_TLS13}.md" <<EOF_HISTORY
# TLS13 Signer Bench History

- run_id: ${RUN_TLS13}
EOF_HISTORY

ARCHIVE_OUT="$({
  FAFAFA_WAVE_B_REPORTS_DIR="$WAVE_B_REL"   FAFAFA_TLS13_SIGNER_GATE_REPORTS_DIR="$TLS13_REL"   bash scripts/archive_ci_artifacts_draft.sh --profile pr --run-id "$RUN_ARCHIVE" --output-root "$ARCHIVE_OUT_REL" --dry-run
} 2>&1)"

for expected in   "$WAVE_B_REL/wave_b_ci_gate_summary_${RUN_ARCHIVE}.md"   "$WAVE_B_REL/wave_b_macos_gate_summary_${RUN_MACOS}.md"   "$WAVE_B_REL/wave_b_windows_gate_summary_${RUN_ARCHIVE}.md"   "$WAVE_B_REL/winssl_blocker_batch_${RUN_ARCHIVE}.md"   "$TLS13_REL/tls13_signer_gate_bundle_${RUN_TLS13}.md"   "$TLS13_REL/tls13_signer_gate_snapshot_${RUN_TLS13}.md"   "$TLS13_REL/tls13_signer_gate_status_${RUN_TLS13}.json"   "$TLS13_REL/wave_b_tls13_sign_bench_${RUN_TLS13}.log"   "$TLS13_REL/wave_b_tls13_signer_${RUN_TLS13}.json"   "$TLS13_REL/tls13_signer_bench_history_${RUN_TLS13}.md"
  do
  if [[ "$ARCHIVE_OUT" != *"$expected"* ]]; then
    echo "$ARCHIVE_OUT"
    fail "archive dry-run should include active artifact $expected"
  fi
done

echo "[PASS] wave b platform archive default reports runtime contract passed"
