# Native-Handle Owner Surface Truth Freeze

## Goal

收掉当前 `native-handle / owner-surface` 这条接口设计漂移里最容易误导新读者的一批活跃面：

- `API_REFERENCE` 不再把 `GetNativeHandle` 画回 `ISSLContext` / `ISSLConnection` 核心接口
- `INTERFACE_DESIGN_V2` 不再把 `GetNativeHandle` 画进 `ISSLConnection` core，也不再把 `GetSelectedALPNProtocol` 画进 `ISSLClientConnection`
- `tests/connection/test_ssl_connection_local.pas` 不再按旧 core 假设读 `GetNativeHandle` / `GetConnectionInfo`

## Scope

- `docs/reference/API_REFERENCE.md`
- `docs/reference/INTERFACE_DESIGN_V2.md`
- `tests/connection/test_ssl_connection_local.pas`
- `tests/scripts/test_native_handle_owner_surface_truth_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不改生产源码接口定义
- 不改 native-handle helper 行为
- 不重开 broader `ISSLConnection` slimming 设计

## Why This Batch

这批不是“文档润色”，而是活跃 truth source 已经开始互相打架：

- `src/fafafa.ssl.base.pas` 明确把 `GetNativeHandle` 放到了 `ISSLNativeHandleAccess`
- `API_REFERENCE` 却还把它列进 `ISSLContext`
- `INTERFACE_DESIGN_V2` 还把它画进 `ISSLConnection` core，并把 `GetSelectedALPNProtocol` 画进 `ISSLClientConnection`
- `tests/connection/test_ssl_connection_local.pas` 真实编译也已经因为旧 core 假设而报错

所以这批最小正确动作，是先把 canonical docs 与 generic smoke test 一起拉回当前 source truth。

## Planned Changes

1. 新增 focused shell contract，锁住 native-handle owner surface truth。
2. 修 `API_REFERENCE`：
   - `ISSLContext` code listing 去掉 `GetNativeHandle`
   - 增加 `ISSLNativeHandleAccess` 可选接口说明
3. 修 `INTERFACE_DESIGN_V2`：
   - `ISSLConnection` core block 去掉 `GetNativeHandle`
   - `ISSLClientConnection` block 去掉 `GetSelectedALPNProtocol`
   - migration table 把 `GetNativeHandle` owner 改成 `ISSLNativeHandleAccess`
4. 修 `tests/connection/test_ssl_connection_local.pas`：
   - 改走 `ISSLNativeHandleAccess`
   - `GetConnectionInfo` 改走 `ISSLConnectionInfo`

## Verification

```bash
bash -n tests/scripts/test_native_handle_owner_surface_truth_contract.sh
bash tests/scripts/test_native_handle_owner_surface_truth_contract.sh
mkdir -p tmp/test_ssl_connection_local_units && fpc -B -Fu./src -Fu./tests -FUtmp/test_ssl_connection_local_units -FEtmp/test_ssl_connection_local_units -otmp/test_ssl_connection_local_units/test_ssl_connection_local tests/connection/test_ssl_connection_local.pas
./tmp/test_ssl_connection_local_units/test_ssl_connection_local
git diff --check
```

## Expected Outcome

- active reference docs no longer contradict the current optional native-handle surface
- generic local connection smoke no longer assumes removed core getters
- `ISSLNativeHandleAccess` / `ISSLConnectionInfo` owner truth is frozen into a focused contract
