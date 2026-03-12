# Historical Docs Snapshot Notices Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 为非归档但明显历史化的测试/验证页面增加统一的历史快照提示，减少它们被误读为当前状态页的风险。

**Architecture:** 先用一个 shell 契约测试锁定目标页面必须包含统一的 `Historical snapshot` 提示和当前入口指向 `docs/testing/TESTING_README.md`。然后只修改高风险页面的顶部说明，不改变原始正文内容和历史统计。

**Tech Stack:** shell contract tests, markdown docs.

---

### Task 1: Lock snapshot notice policy with a failing contract

**Files:**
- Create: `tests/scripts/test_historical_snapshot_notice_contract.sh`

**Step 1: Write failing contract**
- Assert the selected historical docs contain:
  - `Historical snapshot`
  - `docs/testing/TESTING_README.md`

**Step 2: Run test to verify RED**
- Run: `bash tests/scripts/test_historical_snapshot_notice_contract.sh`
- Expected: FAIL before the notices are added.

### Task 2: Add notices to selected historical docs

**Files:**
- Modify: `docs/testing/README_TESTING.md`
- Modify: `docs/testing/TESTING.md`
- Modify: `docs/testing/TEST_COVERAGE_ASSESSMENT.md`
- Modify: `docs/testing/TEST_PLAN.md`
- Modify: `docs/testing/TEST_RESULTS.md`
- Modify: `docs/validation/validation_report_20251003_013646.md`

**Step 1: Add a consistent banner**
- Add a concise blockquote near the top explaining the page is a point-in-time snapshot.
- Point people to `docs/testing/TESTING_README.md` for current verification commands.

### Task 3: Verify

**Files:**
- Verify only

**Step 1: Re-run the shell contract**
- `bash tests/scripts/test_historical_snapshot_notice_contract.sh`
- Expected: PASS.
