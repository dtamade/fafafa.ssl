# Wave B Cross-Platform Checklist Closure Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 修复 `scripts/generate_wave_b_cross_platform_summary.sh` 在 macOS/Windows 证据已提供时仍输出 `TODO` 占位的问题，确保跨平台 checklist 可被机器和人工直接判读。

**Architecture:** 先新增脚本合同测试制造 RED（当提供 macOS+Windows summary 时，checklist 不应出现 TODO）；再最小改动 summary 解析逻辑，把 `compile/modules/examples/overall` 状态映射到跨平台表格；最后跑回归确认不影响 dry-run 与现有输出。

**Tech Stack:** Bash, markdown parsing with shell helpers, deterministic fixture-based script test.

---

### Task 1: Add failing contract test (RED)

**Files:**
- Create: `tests/scripts/test_wave_b_cross_platform_summary.sh`
- Modify: `scripts/generate_wave_b_cross_platform_summary.sh`

**Step 1: Write failing test**

```bash
#!/usr/bin/env bash
set -euo pipefail
# ...create fixture summaries and assert checklist has no TODO when both platform summaries are provided
```

**Step 2: Run test to verify it fails**

Run: `bash tests/scripts/test_wave_b_cross_platform_summary.sh`
Expected: FAIL，提示 checklist 仍包含 TODO 占位。

**Step 3: Capture RED output**

Run: `bash tests/scripts/test_wave_b_cross_platform_summary.sh || true`
Expected: 输出包含 `TODO placeholders remain`。

---

### Task 2: Minimal implementation (GREEN)

**Files:**
- Modify: `scripts/generate_wave_b_cross_platform_summary.sh`

**Step 1: Implement minimal parsing + mapping**

```bash
# add helper to parse step status from platform summary tables
# map compile/modules/examples by platform instead of hardcoded TODO
```

**Step 2: Run test to verify GREEN**

Run: `bash tests/scripts/test_wave_b_cross_platform_summary.sh`
Expected: PASS，输出 `checklist TODO closure contract passed`。

---

### Task 3: Focused regression

**Files:**
- Verify only

**Step 1: Script dry-run compatibility**

Run:
- `bash scripts/generate_wave_b_cross_platform_summary.sh --run-id tdd_regression --linux-summary docs/archive/reports/wave-b-history/wave_b_ci_gate_summary_20260208_025426.md --macos-summary docs/archive/reports/wave-b-history/wave_b_macos_gate_summary_20260208_041500.md --dry-run`

Expected: PASS，dry-run 正常输出且无报错。

**Step 2: Existing contract regression**

Run:
- `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn`

Expected: PASS（0 fail）。

---

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record RED/GREEN evidence**

Run: `date '+%Y-%m-%d %H:%M:%S %z'`
Expected: 记录执行时间戳。

**Step 2: Record command outputs summary**

Run: `git diff -- scripts/generate_wave_b_cross_platform_summary.sh tests/scripts/test_wave_b_cross_platform_summary.sh`
Expected: 仅包含本轮脚本与测试变更。
