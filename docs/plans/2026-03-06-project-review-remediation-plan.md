# Project Review Remediation Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Restore trust in the repository’s current health story by fixing the broken local smoke path, removing active doc/API drift, and aligning main CI with one real smoke signal.

**Architecture:** Start from the concrete failing contract in the local `--fast-local` path and fix it with the smallest compatibility-preserving change. Then lock active docs and comments to verified commands and current symbol names with shell contract tests. Finally, promote one real local smoke step into the main CI workflow and add a single current-health doc that separates live guidance from historical material.

**Tech Stack:** FreePascal (ObjFPC), Bash contract tests, GitHub Actions YAML, Markdown docs.

---

### Task 1: Restore the broken `--fast-local` smoke path

**Files:**
- Create: `src/fafafa.ssl.openssl.api.x509.chain.pas`
- Verify: `tests/openssl/test_openssl_chain_issuer_selection.pas`
- Verify: `tests/scripts/test_focused_compile_zero_noise_contract.sh`

**Step 1: Run the existing focused compile contract**

Run: `bash tests/scripts/test_focused_compile_zero_noise_contract.sh`

Expected: FAIL with `Can't find unit fafafa.ssl.openssl.api.x509.chain`.

**Step 2: Add the minimal compatibility shim**

```pascal
unit fafafa.ssl.openssl.api.x509.chain;

{$mode ObjFPC}{$H+}

interface

uses
  fafafa.ssl.openssl.x509.chain;

implementation

end.
```

**Step 3: Re-run the focused compile contract**

Run: `bash tests/scripts/test_focused_compile_zero_noise_contract.sh`

Expected: PASS with `focused compile remains zero-noise and runtime marker is present`.

**Step 4: Re-run the local smoke preset**

Run: `bash scripts/run_minimal_ci_gate.sh --fast-local`

Expected: PASS.

### Task 2: Lock active docs and comments to verified entry points

**Files:**
- Create: `tests/scripts/test_active_docs_current_entrypoints_contract.sh`
- Modify: `README.md`
- Modify: `docs/README.md`
- Modify: `src/fafafa.ssl.factory.pas`

**Step 1: Write a failing docs contract**

```bash
#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail() {
  echo "[FAIL] $1"
  exit 1
}

if rg -n 'ci_pipeline\.sh' README.md >/dev/null; then
  rg -n 'ci_pipeline\.sh' README.md
  fail 'README should not reference missing ci_pipeline.sh'
fi

if rg -n '\bsslClient\b' docs/README.md src/fafafa.ssl.factory.pas >/dev/null; then
  rg -n '\bsslClient\b' docs/README.md src/fafafa.ssl.factory.pas
  fail 'Active docs/comments should use sslCtxClient'
fi

echo '[PASS] active docs use current entry points and symbols'
```

**Step 2: Run the docs contract to verify it fails**

Run: `bash tests/scripts/test_active_docs_current_entrypoints_contract.sh`

Expected: FAIL with hits in `README.md`, `docs/README.md`, and `src/fafafa.ssl.factory.pas`.

**Step 3: Update the active docs and comments**

- Replace `./ci_pipeline.sh` references in `README.md` with the commands that are actually present today.
- Replace `sslClient` with `sslCtxClient` in `docs/README.md` and active examples/comments in `src/fafafa.ssl.factory.pas`.
- Leave archive docs unchanged unless they claim to be current guidance.

**Step 4: Re-run the docs contract**

Run: `bash tests/scripts/test_active_docs_current_entrypoints_contract.sh`

Expected: PASS.

### Task 3: Make main CI exercise one real local smoke step

**Files:**
- Create: `tests/scripts/test_main_ci_runs_fast_local_contract.sh`
- Modify: `.github/workflows/ci.yml`

**Step 1: Write a failing workflow contract**

```bash
#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORKFLOW="$ROOT_DIR/.github/workflows/ci.yml"

fail() {
  echo "[FAIL] $1"
  exit 1
}

rg -F --quiet -- 'python3 scripts/compile_all_modules.py' "$WORKFLOW" ||
  fail 'Main CI must compile core modules'

rg -F --quiet -- 'bash scripts/run_minimal_ci_gate.sh --fast-local' "$WORKFLOW" ||
  fail 'Main CI must run a real fast-local smoke step'

echo '[PASS] main CI runs compile gate and fast-local smoke'
```

**Step 2: Run the workflow contract to verify it fails**

Run: `bash tests/scripts/test_main_ci_runs_fast_local_contract.sh`

Expected: FAIL because `ci.yml` currently runs `--pre-commit-minimal`, not `--fast-local`.

**Step 3: Update the main CI workflow**

- Keep `python3 scripts/compile_all_modules.py`.
- Replace or supplement the current minimal-gate step with `bash scripts/run_minimal_ci_gate.sh --fast-local` after Task 1 is green.
- Preserve the workflow’s Linux/OpenSSL scope.

**Step 4: Re-run the workflow contract and a YAML sanity check**

Run: `bash tests/scripts/test_main_ci_runs_fast_local_contract.sh`

Expected: PASS.

Run: `python3 -c "import yaml,sys; yaml.safe_load(open('.github/workflows/ci.yml'))"`

Expected: PASS.

### Task 4: Publish one current-health entry point for contributors

**Files:**
- Create: `tests/scripts/test_current_health_doc_contract.sh`
- Create: `docs/testing/CURRENT_HEALTH.md`
- Modify: `docs/README.md`
- Modify: `docs/testing/TESTING_README.md`

**Step 1: Write a failing docs contract for the current-health page**

```bash
#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail() {
  echo "[FAIL] $1"
  exit 1
}

[[ -f docs/testing/CURRENT_HEALTH.md ]] || fail 'Missing current-health doc'
rg -F --quiet -- 'python3 scripts/compile_all_modules.py' docs/testing/CURRENT_HEALTH.md ||
  fail 'Current-health doc must include compile gate'
rg -F --quiet -- 'bash scripts/run_minimal_ci_gate.sh --fast-local' docs/testing/CURRENT_HEALTH.md ||
  fail 'Current-health doc must include fast-local gate'
rg -F --quiet -- 'CURRENT_HEALTH.md' docs/README.md ||
  fail 'Docs homepage must link to the current-health doc'

echo '[PASS] current-health doc is present and linked'
```

**Step 2: Run the contract to verify it fails**

Run: `bash tests/scripts/test_current_health_doc_contract.sh`

Expected: FAIL because the page and link do not exist yet.

**Step 3: Add the current-health doc**

Document:
- the canonical local commands,
- what `compile_all_modules.py` proves,
- what `--fast-local` proves,
- what the main CI workflow runs,
- which doc areas are historical snapshots.

**Step 4: Re-run the current-health contract**

Run: `bash tests/scripts/test_current_health_doc_contract.sh`

Expected: PASS.

**Step 5: Run a whitespace safety check**

Run: `git diff --check -- docs/testing/CURRENT_HEALTH.md docs/README.md docs/testing/TESTING_README.md`

Expected: PASS.
