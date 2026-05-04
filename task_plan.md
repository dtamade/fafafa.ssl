# Task Plan - Early-Data Host-Gated Truth Alignment

## Goal
把当前仓库里 `early-data` 的 contract 和文档收口到真实 host/runtime truth：`OpenSSL` 保持 stable，`FreePascal` 保持 experimental，`WinSSL` / `MbedTLS` 保持 none，`WolfSSL` 则明确改成“按 build/runtime helper 门控；helper 缺失时 capability 发布为 `none`，context/connection 不暴露 early-data 接口”。

## Current Batch
1. 先补 focused RED：
   - `tests/test_openssl_wolfssl_early_data_connection_contract.pas` 显式链接 `OpenSSL` / `WolfSSL` backend，并按 runtime helper truth 验证 `WolfSSL`
   - 新增 `tests/scripts/test_early_data_docs_truth_contract.sh`
   - 用 docs contract 锁住 `README.md` / `docs/BACKEND_CAPABILITY_MATRIX.md` / `docs/guides/EARLY_DATA_GUIDE.md` 的当前真相
2. 最小 GREEN：
   - 如果 Pascal contract 只暴露文档/contract drift，则不改 `src/`
   - 把 README / capability matrix / early-data guide 收口到当前真相
3. 跑 focused contract / docs contract / diff hygiene，回写台账并提交。

## Status
- [completed] RED: host-gated early-data contract and docs truth contract
- [completed] GREEN: docs truth alignment
- [completed] Verification: focused Pascal contract, docs contract, and diff hygiene
- [in_progress] Review and commit

## Current Evidence
- `bash tests/scripts/test_early_data_docs_truth_contract.sh` 已通过，README / guide / capability matrix 现在对齐到同一套 early-data 真相。
- `git diff --check -- docs/plans/2026-05-05-early-data-host-gated-truth-alignment.md tests/scripts/test_early_data_docs_truth_contract.sh tests/test_openssl_wolfssl_early_data_connection_contract.pas README.md docs/BACKEND_CAPABILITY_MATRIX.md docs/guides/EARLY_DATA_GUIDE.md task_plan.md findings.md progress.md` 已通过。
- `fpc -B -Fu./src -Fu./tests -FUtmp/openssl_wolfssl_early_data_units -FEtmp/openssl_wolfssl_early_data_units -otest_openssl_wolfssl_early_data_connection_contract tests/test_openssl_wolfssl_early_data_connection_contract.pas` 重新通过；期间修掉了测试文件里一个悬空 `else` 的语法错误。
- `./tmp/openssl_wolfssl_early_data_units/test_openssl_wolfssl_early_data_connection_contract` 已通过，当前主机上的 `WolfSSL` 继续证明为 helper 缺失 => capability `none` + early-data interfaces absent。

## Verification Plan
- focused Pascal contract:
  - `mkdir -p tmp/openssl_wolfssl_early_data_units`
  - `fpc -B -Fu./src -Fu./tests -FUtmp/openssl_wolfssl_early_data_units -FEtmp/openssl_wolfssl_early_data_units -otest_openssl_wolfssl_early_data_connection_contract tests/test_openssl_wolfssl_early_data_connection_contract.pas`
  - `./tmp/openssl_wolfssl_early_data_units/test_openssl_wolfssl_early_data_connection_contract`
- docs contract:
  - `bash -n tests/scripts/test_early_data_docs_truth_contract.sh`
  - `bash tests/scripts/test_early_data_docs_truth_contract.sh`
- hygiene:
  - `git diff --check -- docs/plans/2026-05-05-early-data-host-gated-truth-alignment.md tests/scripts/test_early_data_docs_truth_contract.sh tests/test_openssl_wolfssl_early_data_connection_contract.pas README.md docs/BACKEND_CAPABILITY_MATRIX.md docs/guides/EARLY_DATA_GUIDE.md task_plan.md findings.md progress.md`

## Risks
- 这批不能把 `WolfSSL` 当前 host 的 helper 缺失，误写成 backend family 永久不支持 early-data。
- 文档必须同时保住两层 truth：backend family 的实验性能力，以及当前 host/build 可能退化成 `none` 的 runtime 现实。
- `FreePascal` 只能保持 experimental / local-persistent / fail-closed wording，不能被历史文档重新抬回 production ready。

## Follow-up Queue
1. 若这批后仍有 fresh RED，下一步只允许重开 early-data contract/docs 相邻 drift，不扩大到新的 TLS family。
2. `WinSSL` broad blocker 仍然是 Windows 主机 runtime proof，不在这批范围内。
3. 若 current host 的 `WolfSSL` helper set 未来升级，再考虑补独立 runtime proof，而不是先把 capability 调宽。
