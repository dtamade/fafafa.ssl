# Task Plan - OpenSSL Server OCSP Stapling Runtime Proof

## Goal
给 `OpenSSL` 服务端 OCSP stapling issuance 补上真实 client/server 握手证据，确认当前 callback wiring 不只是结构闭合，而是真的能把 caller-provided stapled OCSP response 发到客户端 surface。

## Current Batch
1. 新增一个 `OpenSSL server <-> OpenSSL client` focused runtime contract，用本地 socket 跑真实 TLS 握手。
2. 先锁 3 个最小场景：`requested + configured => client 收到 DER`，`not requested => absent`，`requested + no material => absent`。
3. 再补 builder `WithServerOCSPStapledResponseFile(...)` 的 runtime path。
4. 如果 runtime proof 暴露真实缺口，只在 `src/fafafa.ssl.openssl.*` 做最小修复，然后跑 focused runtime test、`python3 scripts/compile_all_modules.py`、`bash scripts/run_minimal_ci_gate.sh --fast-local`。

## Status
- [complete] Runtime test design / RED
- [complete] GREEN implementation or proof-only closeout
- [complete] Verification
- [complete] Review and commit

## Outcome
- 本批最终用 scripted `TStream` TLS 1.3 对端完成 runtime proof，因为本地执行环境无法可靠创建 listen socket。
- `OpenSSL` 真正的 runtime 缺口已经锁定并修复：服务端 stapling callback 注册需要走 `SSL_CTX_callback_ctrl(...)`，而不是继续把 callback 当普通 `SSL_CTX_ctrl(...)` 参数传入。
- `BuildServer + WithServerOCSPStapledResponseFile(...)` 的 runtime path 已经补到 focused test；中途暴露的 builder `Accept` 失败是测试基线差异，原因是 builder 默认 `verify-peer`，测试里已显式改成 `WithVerifyNone` 以对齐 direct server smoke。
- focused runtime、`python3 scripts/compile_all_modules.py`、`bash scripts/run_minimal_ci_gate.sh --fast-local` 全部通过。

## Risks
- OpenSSL TLS 1.3 runtime 可能带有兼容性 `ChangeCipherSpec` 或记录分片行为；因此本批优先用真实 OpenSSL client/server 握手，而不是继续扩 scripted parser。
- 这批只补 runtime proof，不新增能力；如果测试直接转绿，本批生产代码可能零改动。
- builder/runtime proof 若依赖本机 OpenSSL 特性，仍需在 focused test 里把 skip/fail 边界写清楚。

## Follow-up Queue
1. 若这条 runtime proof 成立，再回头判断 `docs/BACKEND_CAPABILITY_MATRIX.md` 和 `OCSP guide` 是否需要把 OpenSSL 证据说明从“callback wiring”提升到“runtime verified”。
2. WolfSSL 仍缺独立 runtime 握手证据，但它受当前主机 `libwolfssl.so` 可用性限制，需要另开条件允许的批次处理。
