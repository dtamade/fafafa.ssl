# Task Plan - OpenSSL Server OCSP Runtime Doc Truth

## Goal
把 `OpenSSL` 服务端 OCSP stapling 的文档 truth 同步到刚完成的 runtime proof：对外文档不再停留在 “native callback wiring 已接通”，而是明确说明 focused runtime 证据、builder file-load path 和当前边界。

## Current Batch
1. 更新 `docs/BACKEND_CAPABILITY_MATRIX.md` 的 OpenSSL server OCSP stapling 条目，补 runtime-proof truth 和边界。
2. 更新 `docs/guides/OCSP_USAGE_GUIDE.md` 的 server-side manual stapling 段落，补 `WithVerifyNone` 示例与 runtime-proof 说明。
3. 新建一个 docs plan 文件并继续维护 `findings.md` / `progress.md`。
4. 复跑 `python3 scripts/compile_all_modules.py`、`bash scripts/run_minimal_ci_gate.sh --fast-local`，按仓库节奏提交这一批 docs truth。

## Status
- [complete] Doc targets confirmed
- [complete] Doc truth updates
- [complete] Verification
- [complete] Review and commit

## Outcome
- `BACKEND_CAPABILITY_MATRIX` 不再只写 “可加载 / 可回调”，而是会明确 OpenSSL server stapling 已有 focused runtime proof。
- `OCSP_USAGE_GUIDE` 会把 `WithVerifyNone` 写进最小 server 示例，避免把 builder 默认 verify 基线误读成“无客户端证书也照常握手”。
- 这一批不改实现，只同步文档 truth，并且 `python3 scripts/compile_all_modules.py` / `bash scripts/run_minimal_ci_gate.sh --fast-local` 已复跑通过。

## Risks
- 如果文档继续省略 `WithVerifyNone`，调用方很容易把 builder 默认 verify 行为当成 stapling runtime 缺陷。
- 这一批是 docs truth，对代码行为没有新改动，因此验证主要是确保门禁不被文档编辑扰动。

## Follow-up Queue
1. WolfSSL 仍缺独立 runtime 握手证据，但它受当前主机 `libwolfssl.so` 可用性限制，需要另开条件允许的批次处理。
2. 如果后续还有更多 server builder 文档示例，最好统一检查是否都需要显式写出 verify 基线。
