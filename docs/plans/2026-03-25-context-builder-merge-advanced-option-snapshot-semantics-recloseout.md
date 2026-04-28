# Context Builder Merge Advanced Option Snapshot Semantics Re-closeout

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Re-close `TSSLContextBuilder.Merge(...)` so source snapshots with empty `server_name` / `alpn_protocols`, explicit `options=[]`, and OCSP booleans merge with the same semantics already recorded in working memory.

**Architecture:** Keep this batch narrow and builder-only. Add one focused config regression program that exercises `Merge(...)` through JSON-exported source snapshots, observe the current RED on disk, then apply the smallest safe fix in `src/fafafa.ssl.context.builder.pas` so merge copies fields whenever they are present, even if empty, and treats explicit empty option arrays as authoritative source state. Do not redesign client/server build paths, connector precedence, import/export semantics, or unrelated builder fields.

**Tech Stack:** Free Pascal, `fpjson`, focused config regression program

---

### Task 1: RED - Reproduce merge snapshot drift

**Files:**
- Add: `tests/config/test_context_builder_merge_advanced_option_snapshot_semantics.pas`
- Reference: `src/fafafa.ssl.context.builder.pas`

**Step 1: Write the failing test**

- Add a focused regression program that:
  - creates target builders with non-empty `server_name` / `alpn_protocols`, enabled SNI / ALPN options, and OCSP state
  - creates source builders whose exported snapshots intentionally carry:
    - `server_name=''`
    - `alpn_protocols=''`
    - `options=[]`
    - `ocsp_stapling_enabled` / `ocsp_stapling_required` booleans
  - merges source into target and asserts:
    - empty `server_name` clears the target field
    - empty `alpn_protocols` clears the target field
    - explicit `options=[]` clears the target option set
    - OCSP booleans match the source snapshot after merge

**Step 2: Run test to verify it fails**

Run:
- `mkdir -p tmp/context_builder_merge_advanced_option_snapshot_semantics_red && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_merge_advanced_option_snapshot_semantics_red -FEtmp/context_builder_merge_advanced_option_snapshot_semantics_red -otmp/context_builder_merge_advanced_option_snapshot_semantics_red/test_context_builder_merge_advanced_option_snapshot_semantics tests/config/test_context_builder_merge_advanced_option_snapshot_semantics.pas && ./tmp/context_builder_merge_advanced_option_snapshot_semantics_red/test_context_builder_merge_advanced_option_snapshot_semantics`

Expected:
- FAIL because current `Merge(...)` still ignores empty `server_name` / `alpn_protocols`
- FAIL because current `Merge(...)` still ignores explicit `options=[]`
- FAIL because current `Merge(...)` still does not copy OCSP booleans from the source snapshot

### Task 2: GREEN - Minimal merge snapshot fix

**Files:**
- Modify: `src/fafafa.ssl.context.builder.pas`

**Step 1: Write minimal implementation**

- In `TSSLContextBuilderImpl.Merge(...)`:
  - copy `server_name` whenever the field is present in the source JSON object, even if empty
  - copy `alpn_protocols` whenever the field is present in the source JSON object, even if empty
  - copy `ocsp_stapling_enabled` / `ocsp_stapling_required` booleans when present
  - treat `options=[]` as an explicit source option set by clearing `FOptions` before replaying the source array, even when the array is empty
  - keep all other merge semantics unchanged

**Step 2: Run focused test to verify it passes**

Run:
- `mkdir -p tmp/context_builder_merge_advanced_option_snapshot_semantics && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_merge_advanced_option_snapshot_semantics -FEtmp/context_builder_merge_advanced_option_snapshot_semantics -otmp/context_builder_merge_advanced_option_snapshot_semantics/test_context_builder_merge_advanced_option_snapshot_semantics tests/config/test_context_builder_merge_advanced_option_snapshot_semantics.pas && ./tmp/context_builder_merge_advanced_option_snapshot_semantics/test_context_builder_merge_advanced_option_snapshot_semantics`

Expected:
- PASS with the focused merge semantics assertions all green

### Task 3: Adjacent verification and writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Run adjacent regressions**

Run:
- `mkdir -p tmp/config_snapshot_clone_merge_adjacent && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_snapshot_clone_merge_adjacent -FEtmp/config_snapshot_clone_merge_adjacent -otmp/context_builder_merge_advanced_option_snapshot_semantics/test_config_snapshot_clone tests/config/test_config_snapshot_clone.pas && ./tmp/context_builder_merge_advanced_option_snapshot_semantics/test_config_snapshot_clone`
- `mkdir -p tmp/config_import_export_merge_adjacent && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_import_export_merge_adjacent -FEtmp/config_import_export_merge_adjacent -otmp/context_builder_merge_advanced_option_snapshot_semantics/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/context_builder_merge_advanced_option_snapshot_semantics/test_config_import_export`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-25-context-builder-merge-advanced-option-snapshot-semantics-recloseout.md tests/config/test_context_builder_merge_advanced_option_snapshot_semantics.pas src/fafafa.ssl.context.builder.pas task_plan.md findings.md progress.md`

**Step 2: Write back evidence**

- Record in working memory that this was a real reopen on current disk:
  - historical closeout existed in `progress.md`
  - focused tests were missing from the tree
  - current `Merge(...)` implementation had regressed to pre-fix semantics
- Mark the family closed again only after the fresh RED and GREEN evidence above
