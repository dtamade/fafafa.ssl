# Connection Builder Hostname Precedence (explicit clear overrides context)

## Goal
- 收口 `TSSLConnectionBuilder.WithHostname(...)` 的 precedence 语义：
  - **connection override（含显式空字符串清空） > context default > empty**
- 避免出现“context 有默认 ServerName，但连接级无法清空”的回退漂移。

## Architecture
- `src/fafafa.ssl.connection.builder.pas`
  - 新增 `FHostnameSet: Boolean`，区分：
    - 未调用 `WithHostname`（不触碰 per-connection ServerName，保留 context fallback）
    - 调用 `WithHostname('')`（显式清空 per-connection ServerName）
  - `TryBuildClient` 中改为：当 `FHostnameSet=True` 时，总是调用
    `ISSLClientConnection.SetServerName(FHostname)`（允许空字符串）。
- 测试策略：
  - 新增 `tests/test_connection_builder_hostname_precedence.pas`：
    - 使用 mock `ISSLContext` + mock `ISSLClientConnection`（不依赖真实网络/后端库）。
    - mock context 在 `CreateConnection` 时将 context 默认 ServerName “注入”到新连接，模拟真实后端的继承行为。
    - 断言 builder 的 3 个路径（不设置 / 覆盖 / 清空）都符合 precedence。

## Files
- Modify: `src/fafafa.ssl.connection.builder.pas`
- Add: `tests/test_connection_builder_hostname_precedence.pas`

## Step-by-step
1) RED：新增 precedence 合同测试
   - `fpc -Fu./src tests/test_connection_builder_hostname_precedence.pas -otmp/test_connection_builder_hostname_precedence && ./tmp/test_connection_builder_hostname_precedence`
   - 期望：FAIL（`WithHostname('')` 不能清空 context fallback）。

2) GREEN：最小修复 builder
   - 增加 `FHostnameSet` 并在 `TryBuildClient` 按 flag 调用 `SetServerName`。

3) Regression（聚焦）
   - `fpc -Fu./src tests/test_connection_builder_hostname_precedence.pas -otmp/test_connection_builder_hostname_precedence && ./tmp/test_connection_builder_hostname_precedence`
   - `python3 -u scripts/compile_all_modules.py`
   - 期望：全部 PASS。

## Done Criteria
- precedence 合同通过（含显式清空路径）。
- `compile_all_modules` 通过，未引入新编译错误。
- 不引入网络依赖；不改变默认不设置 hostname 时的行为（仍保留 context fallback）。

