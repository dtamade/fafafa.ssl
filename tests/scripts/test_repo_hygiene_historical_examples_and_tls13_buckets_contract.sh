#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
EXAMPLES_ARCHIVE_DIR="$ROOT_DIR/docs/archive/reports/examples-compile-history"
MANIFEST="$ROOT_DIR/docs/archive/reports/2026-03-test-reports-migration-manifest.md"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] repo hygiene historical examples_compile and tls13_signer buckets contract"

if git -C "$ROOT_DIR" ls-files "test-reports/examples_compile*" | grep -q .; then
  echo "[INFO] remaining tracked examples_compile bucket sample:"
  git -C "$ROOT_DIR" ls-files "test-reports/examples_compile*" | sed -n "1,20p"
  fail "historical examples_compile* bucket should not remain tracked under test-reports"
fi

if git -C "$ROOT_DIR" ls-files "test-reports/tls13_signer_*" | grep -q .; then
  echo "[INFO] remaining tracked tls13_signer bucket sample:"
  git -C "$ROOT_DIR" ls-files "test-reports/tls13_signer_*" | sed -n "1,20p"
  fail "historical tls13_signer_* bucket should not remain tracked under test-reports"
fi

[[ -d "$EXAMPLES_ARCHIVE_DIR" ]] || fail "missing archive dir: docs/archive/reports/examples-compile-history"
[[ -f "$MANIFEST" ]] || fail "missing migration manifest"

expected_examples_refs=(
  "examples_compile_after_b79_partial.json"
  "examples_compile_ci_gate.json"
  "examples_compile_gate_b83.json"
  "examples_compile_gate_b84.json"
  "examples_compile_gate_b85.json"
  "examples_compile_gate_b86.json"
  "examples_compile_gate_b87.json"
  "examples_compile_gate_b88.json"
  "examples_compile_latest.json"
)

expected_tls13_removed=(
  "tls13_signer_bench_history_local_bundle_smoke.md"
  "tls13_signer_bench_history_local_bundle_smoke2.md"
  "tls13_signer_gate_bundle_local_bundle_smoke.md"
  "tls13_signer_gate_bundle_local_bundle_smoke2.md"
  "tls13_signer_gate_snapshot_local_bundle_smoke.md"
  "tls13_signer_gate_snapshot_local_bundle_smoke2.md"
  "tls13_signer_gate_status_local_bundle_smoke.json"
  "tls13_signer_gate_status_local_bundle_smoke2.json"
)

for name in "${expected_examples_refs[@]}"; do
  [[ -f "$EXAMPLES_ARCHIVE_DIR/$name" ]] || fail "missing archived referenced examples artifact: $name"
done

for name in "${expected_examples_refs[@]}"; do
  if rg -n -F -- "test-reports/$name" "$ROOT_DIR" --glob '!test-reports/**' --glob '!docs/archive/reports/**' >/tmp/fafafa_historical_examples_refs.txt 2>/dev/null; then
    echo "[INFO] stale examples refs outside archive for $name:"
    sed -n "1,160p" /tmp/fafafa_historical_examples_refs.txt
    fail "stale test-reports/$name references should be migrated to archive paths"
  fi
done

for name in "${expected_tls13_removed[@]}"; do
  if rg -n -F -- "test-reports/$name" "$ROOT_DIR" --glob '!test-reports/**' --glob '!docs/archive/reports/**' >/tmp/fafafa_historical_tls13_refs.txt 2>/dev/null; then
    echo "[INFO] stale tls13_signer refs outside archive for $name:"
    sed -n "1,160p" /tmp/fafafa_historical_tls13_refs.txt
    fail "stale test-reports/$name references should not remain outside historical artifacts"
  fi
done

echo "[PASS] repo hygiene historical examples_compile and tls13_signer buckets contract passed"
