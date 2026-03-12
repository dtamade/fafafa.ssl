# 2026-02-22 Wave B Cross-Platform Summary Android Extension

## Goal

- Extend Wave B cross-platform summary from 3 platforms (Linux/macOS/Windows) to 4 platforms (Linux/macOS/Windows/Android) without requiring real Android runner.

## Architecture / Scope

- Use fixture-based contract tests only (no platform runtime dependency).
- Add optional `--android-summary` input for markdown evidence parsing.
- Keep existing Linux/macOS/Windows behavior unchanged.
- Update summary report tables and dry-run output to include Android.

## Files

- `scripts/generate_wave_b_cross_platform_summary.sh`
- `tests/scripts/test_wave_b_cross_platform_summary.sh`
- `tests/scripts/test_wave_b_cross_platform_summary_android_extension.sh`

## Step-by-step Commands

1. RED:
   - Add Android extension contract test that passes `--android-summary`.
   - Run:
     - `bash tests/scripts/test_wave_b_cross_platform_summary_android_extension.sh`
   - Expected: fail (`Unknown option: --android-summary`).
2. GREEN:
   - Implement `--android-summary` parsing and state/check mapping.
   - Extend output:
     - `Platform Evidence Status` table includes `android`.
     - `Cross-Platform Checklist` table includes `android` column.
     - `Next Actions` includes Android runner evidence backfill.
3. Regression:
   - `bash tests/scripts/test_wave_b_cross_platform_summary_android_extension.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`
   - `bash -n scripts/generate_wave_b_cross_platform_summary.sh`

## Expected Outputs

- New Android extension contract passes.
- Existing Wave B cross-platform tests remain green.
