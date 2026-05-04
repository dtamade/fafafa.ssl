# Task Plan - WolfSSL Server OCSP Runtime Proof

## Goal
把 `WolfSSL` 服务端 OCSP stapling 从“contract + compile gate 已接线”推进到当前真实 runtime truth：现有 focused contract 在本机真跑，scripted `TStream` baseline 握手已验证，而 `configured + requested => stapled DER` 只在 `wolfSSL >= 5.9.1` 主机上执行 emission proof；更低版本主机明确 skip 并记录上游边界。

## Current Batch
1. 修正 `tests/test_wolfssl_ocsp_stapling_contract.pas` 的 backend bring-up 路径，让它在当前主机上真正运行，而不是继续误报 `[SKIP]`.
2. 新增 `WolfSSL` server-side OCSP stapling focused runtime contract，baseline 层覆盖 `requested / not-requested / no-material / builder no-file`，emission 层覆盖 direct set 与 builder file-load。
3. 如果 runtime proof 在支持版本主机上暴露 repo-side 真实缺口，只在 `src/fafafa.ssl.wolfssl.*` 做最小修复；如果问题落在旧版 host WolfSSL 上，则按版本真值门控收口。
4. 跑 focused runtime test、`python3 scripts/compile_all_modules.py`、`bash scripts/run_minimal_ci_gate.sh --fast-local`，然后按批次提交。

## Status
- [completed] Runtime RED / backend bring-up
- [completed] GREEN implementation or proof-only closeout
- [completed] Verification
- [completed] Review and commit

## Outcome
- 目标是把 `WolfSSL` 从“源码契约 + focused contract 已绿，但 runtime proof 还缺”推进到“baseline handshake 有本机证据，configured emission truth 按 host version 诚实门控”。
- 当前最小可信收口不是强行把所有 emission 场景跑绿，而是避免把 `wolfSSL 5.7.2` 的上游限制误报成 repo 代码缺口。
- 当前这批 focused tests、compile gate 和 minimal CI gate 都已通过，可以按 host-gated closeout 提交。

## Risks
- `WolfSSL` 可能要求更明确的 stream I/O callback、status-type 或 builder verify 基线，runtime test 有可能打出新的 backend-specific seam。
- 这批只收口 manual stapled-response issuance，不重开 online fetch、refresh，或 responder 调度。

## Follow-up Queue
1. 在 `wolfSSL >= 5.9.1` 的主机上复跑当前跳过的 direct / builder emission 场景，补齐真正的 stapled DER runtime proof。
2. 如果升级 host 后 emission 仍失败，再回头只审 `src/fafafa.ssl.wolfssl.*` 的 repo-side runtime seam，而不是重开更大范围。
