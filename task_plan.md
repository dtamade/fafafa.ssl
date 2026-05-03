# Task Plan - WolfSSL OCSP Stapling Alignment

## Goal
收口 `WolfSSL` 在 OCSP stapling 上的 public/runtime 漂移：补齐 client request、server stapled-response issuance 和 capability truth，让 builder / context / connection / docs 不再各说各话。

## Current Batch
1. 新增 focused contract，锁住 `WolfSSL` 的 OCSP stapling capability、`ISSLServerOCSPStaplingContext`、builder file-load 和 connection surface 基本语义。
2. 修正 `wolfSSL_UseOCSPStapling` 等 native binding 签名，补 server callback / client request 接线。
3. 收紧 `docs/BACKEND_CAPABILITY_MATRIX.md` 与 `docs/guides/OCSP_USAGE_GUIDE.md`，把 WolfSSL 表述从“未支持/稳定支持”收敛到实验性真值。
4. 跑 focused contract、`python3 scripts/compile_all_modules.py`、`bash scripts/run_minimal_ci_gate.sh --fast-local`。

## Status
- [completed] 接口/能力/测试映射
- [completed] RED focused contract
- [completed] GREEN implementation
- [completed] Docs truth alignment
- [completed] Verification
- [pending] Review and commit

## Risks
- `WolfSSL` 在当前 Linux 主机可能不可用，因此这批 runtime 证据大概率仍以 focused contract + compile/minimal gate 为主，本机不一定拿得到真实握手证据。
- `src/fafafa.ssl.wolfssl.context.pas` 内仍保留旧版 `TWolfSSLConnection` 实现；虽然 `CreateConnection(...)` 已改走现代 `fafafa.ssl.wolfssl.connection.TWolfSSLConnection`，但旧类还在源码里，后续若继续演进 WolfSSL 连接栈，最好再安排一次删除/合并。
- `wolfSSL_UseOCSPStapling` 当前仓库签名与本地头文件不一致，说明这条线不能只看 docs 或现有 unit comments，必须以本地 header 为准。

## Follow-up Queue
1. 如果后续要把 `WolfSSL` 从“实验性”抬到“稳定”，需要补独立的 runtime handshake / stapled-response consume 证据。
2. 继续检查 `ssoRequireOCSPStapling` 在 WolfSSL 上是否也需要更强的 fail-closed 语义，而不只是 request / consume。
