# 2026-02-24 Wave B Cross Summary 兼容 Windows Blocker 布局

## Goal

- 让 `scripts/generate_wave_b_cross_platform_summary.sh` 同时兼容两类 Windows gate 摘要：
  - 旧布局：`step|status|notes`
  - 新布局：`step|exit|status|evidence`（含 `winssl_blocker_batch/winssl/openssl/modules`）
- 避免在 Linux 侧汇总时出现 Windows 列 `TODO` 漏报。

## Architecture / Scope

- Script:
  - `scripts/generate_wave_b_cross_platform_summary.sh`
  - 改造 step 状态解析函数，支持三列/四列表格。
  - Windows checklist 映射兼容新布局：
    - `compile_all_modules` <- `modules`（fallback）
    - `p2_modules_gate` <- `winssl + openssl` 组合判定（优先）
    - `examples_compile_gate` <- `winssl_blocker_batch`（fallback）
- Contract:
  - `tests/scripts/test_wave_b_cross_platform_summary_windows_gate_blocker_layout.sh`

## Files

- `docs/plans/2026-02-24-wave-b-cross-summary-windows-blocker-layout-compat.md`
- `scripts/generate_wave_b_cross_platform_summary.sh`
- `tests/scripts/test_wave_b_cross_platform_summary_windows_gate_blocker_layout.sh`

## Step-by-step Commands

1. RED:
   - `bash tests/scripts/test_wave_b_cross_platform_summary_windows_gate_blocker_layout.sh`
2. GREEN:
   - 修改 `scripts/generate_wave_b_cross_platform_summary.sh` 的 step 解析与 Windows 映射。
3. Regression:
   - `bash tests/scripts/test_wave_b_cross_platform_summary_windows_gate_blocker_layout.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary_android_extension.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`
   - `bash scripts/scan_active_docs_noise_draft.sh --strict --output tmp/active_docs_noise_scan_20260224_round6.md`
   - `bash scripts/check_docs_index_dedup_draft.sh --scope all --strict --output tmp/docs_index_dedup_all_20260224_round6.md`
   - `bash -n scripts/generate_wave_b_cross_platform_summary.sh tests/scripts/test_wave_b_cross_platform_summary_windows_gate_blocker_layout.sh tests/scripts/test_wave_b_cross_platform_summary.sh tests/scripts/test_wave_b_cross_platform_summary_android_extension.sh tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`

## Expected Outputs

- 使用新版 Windows gate 摘要时，cross checklist 的 Windows 列不再出现 `TODO` 误报。
- 旧版 Windows 摘要合同与 Android/Linux 既有合同保持通过。
