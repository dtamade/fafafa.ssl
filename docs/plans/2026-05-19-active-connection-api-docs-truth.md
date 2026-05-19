# 2026-05-19 Active Connection API Docs Truth

## Goal

继续沿着 interface/backend completeness 主线推进，收口高入口文档里仍在教授旧连接接口形状和过度同构叙事的残留：

- `docs/reference/API_DOCUMENTATION.md`
  - 仍把 `ISSLConnection.Connect` 写成 `Connect(host, port)`
  - 仍把 `CreateConnection` 写成接收端口号
  - 仍使用旧的 `WithSystemRootCerts` / `Disconnect` / `Write(string)` / `Read(TBytes)` / `Connection.GetLastError`
- `docs/guides/WINSSL_BEST_PRACTICES.md`
  - 测试最佳实践片段仍在使用 `LConn.Connect('host', port)` 旧调用形状
- `docs/guides/WINSSL_USER_GUIDE.md`
  - 仍把 WinSSL 说成“与 OpenSSL 后端完全相同的接口”，掩盖 backend-specific capability truth

## Scope

- 只处理高入口 active docs truth：
  - `docs/reference/API_DOCUMENTATION.md`
  - `docs/guides/WINSSL_BEST_PRACTICES.md`
  - `docs/guides/WINSSL_USER_GUIDE.md`
- 用 focused shell contract 锁住：
  - `ISSLConnection` 当前连接 API 形状
  - quick-start / troubleshooting 示例不再教授不存在的 surface
  - WinSSL 不再被描述成“完全同构” backend
- 不修改 runtime 实现
- 不扩到更大范围的全量 guide/reference 改写

## Files

- `docs/reference/API_DOCUMENTATION.md`
- `docs/guides/WINSSL_BEST_PRACTICES.md`
- `docs/guides/WINSSL_USER_GUIDE.md`
- `tests/scripts/test_active_connection_api_docs_truth_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `ISSLConnection.Connect` 当前 shipped source 真相是：
  - `function Connect: Boolean;`
  - host/port transport 先由 caller 建立，再 `CreateConnection(Socket/Stream)`
- client SNI 当前推荐路径是：
  - `ISSLClientConnection.SetServerName(...)`
  - 不是 `Connect(host, port)` 隐式传入，也不是 context-level `SetServerName(...)`
- 文本 I/O 当前 shipped truth 是：
  - 原始字节路径：`Read(var ABuffer; ACount)` / `Write(const ABuffer; ACount)`
  - convenience 文本路径：`ReadString(out AStr)` / `WriteString(const AStr)`
- `ISSLConnection` 当前没有 `Disconnect`、`GetLastError`、`GetPeerCertificateVerified` 这些文档里旧写法对应的 public surface
- WinSSL 与其它 backend 共享统一核心 public interface，但具体 published capability 仍以后端的 `ISSLLibrary.GetCapabilities` 和 capability matrix 为准

## Steps

1. 补 focused shell contract，让旧 `Connect(host, port)` / 旧连接 API 文档先 RED。
2. 把 `API_DOCUMENTATION` / `WINSSL_BEST_PRACTICES` / `WINSSL_USER_GUIDE` 改回当前连接 surface 与 capability 真相。
3. 同步台账，避免后续重复把这些旧片段当成 current source truth。
4. 跑轻量验证并提交。

## Commands

```bash
bash -n tests/scripts/test_active_connection_api_docs_truth_contract.sh
bash tests/scripts/test_active_connection_api_docs_truth_contract.sh
git diff --check
```

## Expected Result

- 高入口文档不再教授 `Connect(host, port)`、`Disconnect`、`Connection.GetLastError` 这类不存在或已变形的旧 surface
- `API_DOCUMENTATION` 的 quick-start / `ISSLConnection` section / troubleshooting 回到当前源码接口真相
- `WINSSL` 指南不再把 backend-specific capability 差异压平成“完全相同的接口”
