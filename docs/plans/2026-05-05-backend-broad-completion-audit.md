# Backend Broad Completion Audit Plan

**Goal:** 不再凭累计工作量或局部绿灯去推断“各个后端的接口和实现都完整”，而是把 broad objective 映射到可核对的 deliverable checklist，并用 fresh evidence 重新验证：`OpenSSL` / `WolfSSL` / `MbedTLS` / `FreePascal` / `WinSSL` 的公开接口合同、capability truth、实现级 gate、以及 `WinSSL` 的真实 runtime proof 是否全部闭合。

**Architecture:** 这批优先做 completion audit，而不是预设一定还有生产代码改动。按四层证据收口：
- `tests/contract/test_backend_contract.pas`：确认公开接口 surface 是否对当前可用 backend 闭合。
- `tests/test_capability_cache.pas`：确认 capability / `KnownIssues` truth 是否与当前 runtime evidence 一致。
- `python3 scripts/compile_all_modules.py` 与 `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local ...`：确认当前 Linux 主机上能被仓库自己验证的实现线没有回归。
- `command -v pwsh`、`wine --version`、`bash tests/scripts/test_winssl_windows_validation_bundle_contract.sh`、`bash tests/scripts/test_wave_b_windows_gate_pwsh_and_verbose_contract.sh`：确认 `WinSSL` 是真正只剩独立环境 blocker，而不是 validation bundle 自己还在漂移。

**Files:**

- Add: `docs/plans/2026-05-05-backend-broad-completion-audit.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: Build the completion checklist from the broad objective

Checklist dimensions:

1. `OpenSSL` / `WolfSSL` / `MbedTLS` / `FreePascal` / `WinSSL` 的公开接口合同是否都有明确证据
2. capability truth 是否与当前实现和 `KnownIssues` wording 对齐
3. 当前 Linux 主机可验证的实现线是否都经过 focused gate / compile gate
4. `WinSSL` 未完成如果仍存在，是否已经被证明是外部 Windows runtime blocker，而不是仓库内 drift

## Task 2: Pull fresh evidence

Run:

```bash
command -v pwsh
wine --version
mkdir -p tmp/backend_contract_units
fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas
./tmp/backend_contract_units/test_backend_contract
mkdir -p tmp/capability_cache_units
fpc -B -Fu./src -Fu./tests -FUtmp/capability_cache_units -FEtmp/capability_cache_units -otmp/capability_cache_units/test_capability_cache tests/test_capability_cache.pas
./tmp/capability_cache_units/test_capability_cache
python3 scripts/compile_all_modules.py
bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id broad_completion_audit_20260505
bash tests/scripts/test_winssl_windows_validation_bundle_contract.sh
bash tests/scripts/test_wave_b_windows_gate_pwsh_and_verbose_contract.sh
```

Expected interpretation:

- 若 `backend_contract` / `capability_cache` / compile gate / FreePascal completeness gate 全绿，则说明 Linux 主机可验证范围内的 backend surface 与主实现线没有 fresh regression。
- 若 `pwsh` 缺失且 `wine` 仍不能承担 Windows runtime，则 `WinSSL` broad closure 依然不能在当前主机上完成。
- 若 `WinSSL` bundle contracts 通过，则剩余 blocker 更纯粹地收敛到“缺真实 Windows runtime 环境”。

## Task 3: Decide whether the broad objective is actually achieved

Decision rules:

- 只有当每个 backend 都有与 broad objective 对应的“接口 + 实现”证据时，才允许宣称完成。
- `WinSSL` 若没有真实 Windows runtime proof，只能判定 broad objective 未完成，不能用 Linux 上的 source contract / cross-compile / bundle docs 代理替代。
- 如果当前主机已经没有新的 repo-side drift，而只剩独立环境 blocker，则本批输出必须明确停点，不盲开新的 backend 功能线。

### Definition Of Done

- broad objective 被拆成明确 checklist，并逐条映射到具体证据
- fresh evidence 重新确认 Linux 主机可验证范围内的 backend surface / capability / implementation gate
- `WinSSL` remaining blocker 的性质被重新证明
- 如果 broad objective 仍未完成，明确指出唯一剩余 requirement 与阻塞原因

## Execution Result

- fresh environment probe:
  - `command -v pwsh` => 空 / exit `1`
  - `wine --version` => exit `159`
- public interface evidence:
  - `tests/contract/test_backend_contract.pas` fresh result 为 `135 total / 111 passed / 0 failed / 24 skipped`
  - `OpenSSL` / `WolfSSL` / `MbedTLS` / `FreePascal` 的 `Contract 1-21` 全绿
  - `WinSSL` 继续因为平台 unavailable 跳过，`Contract 15` 还显式保留 `Windows-focused batch` 边界
- capability truth evidence:
  - `tests/test_capability_cache.pas` fresh result 全绿
  - `FreePascal` / `WolfSSL` / `MbedTLS` 的 `KnownIssues` wording 与当前 runtime truth 保持对齐
- Linux 主机实现线 evidence:
  - `python3 scripts/compile_all_modules.py` => `185/185`
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id broad_completion_audit_20260505` => summary `17 passed / 0 failed`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => compile `185/185`、模块 `17/17`、Phase2 baseline dry-run 可用、最终 `[PASS]`
- WinSSL repo-side evidence:
  - `bash tests/scripts/test_winssl_windows_validation_bundle_contract.sh` => PASS
  - `bash tests/scripts/test_wave_b_windows_gate_pwsh_and_verbose_contract.sh` => PASS
  - `bash tests/scripts/test_winssl_connection_context_access_contract.sh` => PASS
  - `bash tests/scripts/test_winssl_session_truth_source_contract.sh` => PASS
  - `bash tests/scripts/test_winssl_context_external_store_contract.sh` => PASS
- final audit decision:
  - 当前 Linux 主机上已经没有新的 repo-side drift
  - broad objective 仍未完成
  - 唯一剩余 requirement 是 `WinSSL` 的真实 Windows runtime proof
  - 在当前主机缺少 `pwsh` 且 `wine` 不能承担有效验证的前提下，这一项不能继续 productively 收口
