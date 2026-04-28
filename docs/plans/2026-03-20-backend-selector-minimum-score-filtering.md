# Backend Selector Minimum Score Filtering Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Ensure `SelectBestBackends(...)` does not return backends whose required features pass but whose minimum score thresholds fail, so `MatchDetails.MeetsMinimumRequirements` stays semantically correct.

**Architecture:** Keep this batch narrowly focused on selector consistency.
- Scope in:
  - `CalculateTotalMatchScore(...)`
  - `SelectBestBackends(...)`
  - focused selector regression for minimum-security-score filtering
- Scope out:
  - builder import/export/merge of auto-selection requirements
  - broader selector ranking heuristics
  - backend capability score recalibration

Current harness with `fafafa.ssl.openssl.backed` and `fafafa.ssl.freepascal.lib` gives:
- OpenSSL: security score `90`
- FreePascal Native: security score `60`

That makes `MinSecurityScore := 95` a stable legal regression:
- correct behavior: `SelectBestBackends(...)` should return no candidates
- current drift: both backends leak into the results list with `MatchScore = 0` while `MeetsMinimumRequirements = True`

**Tech Stack:** Free Pascal, `tests/test_backend_selector_minimum_score_filtering.pas`, `src/fafafa.ssl.backend.selector.pas`

---

### Task 1: RED - Add focused selector regression

**Files:**
- Add: `tests/test_backend_selector_minimum_score_filtering.pas`

**Step 1: Add focused test**

- Build a requirement from `CreateDefaultRequirements(optBalanced)`.
- Set `MinSecurityScore := 95`.
- Assert:
  - `SelectBestBackends(...)` returns an empty array
  - no zero-score backends leak into results

**Step 2: Run focused RED**

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_backend_selector_minimum_score_filtering tests/test_backend_selector_minimum_score_filtering.pas && ./tmp/test_backend_selector_minimum_score_filtering`

Expected:
- FAIL
- results still contain zero-score backends even though minimum security threshold is unmet

### Task 2: GREEN - Fix minimum-score qualification semantics

**Files:**
- Modify: `src/fafafa.ssl.backend.selector.pas`

**Step 1: Fix root semantics**

- When `MinSecurityScore`, `MinPerformanceScore`, or `MinCompatibilityLevel` fails, mark `MeetsMinimumRequirements := False` before returning.
- Keep current scoring behavior otherwise.

**Step 2: Re-run focused regression**

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_backend_selector_minimum_score_filtering tests/test_backend_selector_minimum_score_filtering.pas && ./tmp/test_backend_selector_minimum_score_filtering`

Expected:
- PASS

### Task 3: Adjacent verification

**Files:**
- Test: `tests/test_backend_selector_basic.pas`
- Test: `tests/config/test_context_builder_try.pas`

**Step 1: Re-run adjacent tests**

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_backend_selector_basic tests/test_backend_selector_basic.pas && ./tmp/test_backend_selector_basic`

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/test_context_builder_try`

Expected:
- PASS

### Task 4: Full verification

**Step 1: Re-run config audit**

Run:
`mkdir -p tmp/config_audit_logs_selector_min_score && audit_rc=0; for f in tests/config/*.pas; do name=$(basename "$f" .pas); log="tmp/config_audit_logs_selector_min_score/${name}.log"; if fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/${name} "$f" >"$log" 2>&1 && ./tmp/${name} >>"$log" 2>&1; then printf 'PASS %s\n' "$name"; else printf 'FAIL %s\n' "$name"; audit_rc=1; fi; done; exit $audit_rc`

**Step 2: Re-run full module compile gate**

Run:
`python3 -u scripts/compile_all_modules.py`

Expected:
- all `tests/config/*.pas` pass
- compile gate passes

### Task 5: Documentation and next queue

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record evidence**

- capture the mismatch between zero score and `MeetsMinimumRequirements = True`
- capture the focused RED/green evidence

**Step 2: Queue the next batch**

- after selector consistency is closed, continue with:
  - auto-selection requirement import/export/merge contract
