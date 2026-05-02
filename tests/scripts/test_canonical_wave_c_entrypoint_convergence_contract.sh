#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_ROOT"

require_fixed() {
  local file="$1"
  local expected="$2"
  local message="$3"
  if ! grep -Fq -- "$expected" "$file"; then
    echo "[FAIL] $message"
    echo "  file: $file"
    echo "  expected: $expected"
    exit 1
  fi
}

reject_regex() {
  local file="$1"
  local pattern="$2"
  local message="$3"
  if rg -n --quiet "$pattern" "$file"; then
    echo "[FAIL] $message"
    rg -n "$pattern" "$file" || true
    exit 1
  fi
}

require_fixed "README.md" '- 默认导航：先看 `docs/test_reports/WAVE_C_CLOSEOUT_STATUS_2026-03-18.md`，再看 `docs/test_reports/WAVE_C_LOCAL_FIRST_AND_PRE_CI_CHAIN_STATUS_2026-03-16.md`。' \
  "README is missing the canonical default Wave C navigation line"
require_fixed "README.md" '- 历史手册仅作参考：`docs/test_reports/WAVE_C_B121_ONE_PAGE_RUNBOOK_2026-02-08.md`、`docs/test_reports/WAVE_C_B127_LOCAL_GUARD_TROUBLESHOOTING_2026-02-09.md`。' \
  "README is missing the historical-only label for B121/B127"

require_fixed "docs/README.md" "## 当前工程入口（Wave C canonical chain）" \
  "docs/README.md is missing the canonical entrypoint section"
require_fixed "docs/README.md" "python3 scripts/compile_all_modules.py" \
  "docs/README.md is missing the canonical compile entry command"
require_fixed "docs/README.md" "bash scripts/run_minimal_ci_gate.sh --fast-local" \
  "docs/README.md is missing the canonical minimal gate command"
require_fixed "docs/README.md" '历史参考：`test_reports/WAVE_C_B121_ONE_PAGE_RUNBOOK_2026-02-08.md`、`test_reports/WAVE_C_B127_LOCAL_GUARD_TROUBLESHOOTING_2026-02-09.md`' \
  "docs/README.md is missing the historical-only Wave C references"

require_fixed "docs/DOCUMENTATION_INDEX.md" "## 🧭 当前工程入口（Wave C canonical chain）" \
  "docs/DOCUMENTATION_INDEX.md is missing the canonical Wave C entrypoint section"
require_fixed "docs/DOCUMENTATION_INDEX.md" "### 历史 Wave C 页面（仅归档参考）" \
  "docs/DOCUMENTATION_INDEX.md is missing the historical-only Wave C section label"

reject_regex "docs/guides/GETTING_STARTED.md" "bash build_linux\\.sh" \
  "GETTING_STARTED.md still treats build_linux.sh as active build/test guidance"
require_fixed "docs/guides/GETTING_STARTED.md" "bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local" \
  "GETTING_STARTED.md is missing the current phase2 dry-run guidance"

reject_regex "docs/guides/QUICKSTART.md" "bash build_linux\\.sh" \
  "QUICKSTART.md still treats build_linux.sh as active build/test guidance"
require_fixed "docs/guides/QUICKSTART.md" "bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local" \
  "QUICKSTART.md is missing the current phase2 dry-run guidance"

echo "[PASS] canonical Wave C entrypoint docs converge on current chain and active commands"
