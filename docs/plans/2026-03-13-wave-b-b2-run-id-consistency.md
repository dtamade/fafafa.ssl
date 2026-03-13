# Wave B / B2: Linux run_id 一致性修复（证据链可审查）

## Goal
- 修复 B2 manual workflow 的证据一致性校验中出现的 **linux_summary run_id mismatch**，确保：
  - summary 文件名中的 `<run_id>` 与 summary 内容中的 Run ID 一致；
  - `check_wave_b_b2_evidence_consistency.sh` 不再因 run_id 漂移误报 `INCONSISTENT`。

## Background / Evidence
- B2 summary job 以 setup job 的 `run_id` 命名 Linux summary 文件：`wave_b_ci_gate_summary_<run_id>.md`。
- 但 `scripts/run_wave_b_ci_gate.sh` 内部自生成 `RUN_ID=$(date ...)` 并写入 summary 内容，导致：
  - 文件名 run_id 与内容 run_id 不一致；
  - 证据一致性脚本判定 `run_id mismatch`。

## Fix
- 为 `scripts/run_wave_b_ci_gate.sh` 增加 `--run-id`（覆盖内部 RUN_ID）。
- 在 `.github/workflows/wave-b-b2-manual.yml` 的 Linux gate 里传入同一个 `RUN_ID`。
- 同步 `scripts/run_tls13_signer_gate_ci.sh` 也传入 `--run-id`，避免同类漂移。

## Files
- Modify: `scripts/run_wave_b_ci_gate.sh`
- Modify: `.github/workflows/wave-b-b2-manual.yml`
- Modify: `scripts/run_tls13_signer_gate_ci.sh`
- Add: `tests/scripts/test_wave_b_ci_gate_run_id_passthrough_contract.sh`

## Verification
- `bash -n scripts/run_wave_b_ci_gate.sh`
- `bash tests/scripts/test_wave_b_ci_gate_run_id_passthrough_contract.sh`

## Expected Outputs / Acceptance
- `test_wave_b_ci_gate_run_id_passthrough_contract` => PASS
- B2 summary job 的 `wave_b_b2_evidence_consistency_<run_id>.md`：
  - `runid_mismatch_or_parse_issue=0`
  - `consistency_status` 不再因 Linux summary run_id 漂移而为 `INCONSISTENT`
