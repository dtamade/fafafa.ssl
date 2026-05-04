# Task Plan - WolfSSL Context Stale Connection Removal

## Goal
删除 `src/fafafa.ssl.wolfssl.context.pas` 里仍保留的旧 `TWolfSSLConnection` 私有残留实现，避免它继续和 `src/fafafa.ssl.wolfssl.connection.pas` 的现代连接类分叉。

## Current Batch
1. 先加一个 focused source contract，锁住 `wolfssl.context` 不再保留旧 `TWolfSSLConnection` 私有类实现，同时工厂路径继续指向 `wolfssl.connection.TWolfSSLConnection`。
2. 只有契约打出真实 RED 后，才对 `src/fafafa.ssl.wolfssl.context.pas` 做最小改动：删除旧类声明与整段旧实现。
3. 跑 `bash -n`、focused script contract、`python3 scripts/compile_all_modules.py`、`bash scripts/run_minimal_ci_gate.sh --fast-local`，然后收口提交。

## Status
- [completed] RED/audit contract
- [completed] GREEN implementation
- [completed] Verification
- [completed] Review and commit

## Outcome
- `src/fafafa.ssl.wolfssl.context.pas` 里那套旧 `TWolfSSLConnection` 确认只是 `implementation` 私有残留，不是公开 API。
- 当前唯一真实连接 truth source 已收敛回 `src/fafafa.ssl.wolfssl.connection.pas`：`TWolfSSLContext.CreateConnection(...)` 的 socket/stream 路径都继续走现代连接类。
- 本批已删除：
  - 旧 `TWolfSSLConnection` 私有类声明
  - 旧构造/析构与整段私有连接实现
  - 只服务于旧实现的 context 内部流回调和多余 `uses`
- 验证已完成：
  - `bash -n tests/scripts/test_wolfssl_context_stale_connection_contract.sh`
  - `bash tests/scripts/test_wolfssl_context_stale_connection_contract.sh` => 全部 PASS
  - `python3 scripts/compile_all_modules.py` => `185/185`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => `[PASS] minimal CI gate finished`

## Risks
- 这批修的是 `implementation` 残留，不重开 WolfSSL 握手/OCSP/early-data 行为。
- 删除旧私有实现时，不能误伤当前已收口好的现代工厂路径。

## Follow-up Queue
1. 如果这批确认并收掉旧 `TWolfSSLConnection` 残留，再继续盘点其它仍停留在旧实现路径上的重复私有类或死代码。
2. 如果在删除旧实现时暴露额外 contract 漂移，再按最小边界补 focused contract，而不是顺手重构整个 WolfSSL backend。
