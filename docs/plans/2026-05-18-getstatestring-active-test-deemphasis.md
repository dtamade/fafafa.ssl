# `GetStateString` Active Test De-emphasis

## Goal

把活跃 generic/integration 测试里仍把 `ISSLConnection.GetStateString` 当普通路径使用的调用点切到 `ISSLConnectionInfo.GetStateString`，让 `GetStateString` 主线先从“普通测试仍直连 core”收缩到“仅 backend-specific / contract mirror 残留”。

## Scope

本批只处理 active tests、focused contract 与台账：

- `tests/connection/test_connection_basic.pas`
- `tests/integration/test_real_https_connection.pas`
- `tests/scripts/test_isslconnectioninfo_getstatestring_active_test_contract.sh`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不修改 public signature
- 不改 backend runtime tests
- 不改 `src/` 生产实现

## Why This Batch

当前 `GetStateString` 的活跃文档已经基本转到 `ISSLConnectionInfo`，但普通测试仍有两类残余：

- `tests/connection/test_connection_basic.pas` 还直接调用 `LConnection.GetStateString`
- `tests/integration/test_real_https_connection.pas` 还把 `Conn.GetStateString` 用作通用握手失败输出

这两类文件比 backend-specific runtime files 更像“普通推荐路径”，因此更适合先收。

## Planned Changes

1. 把 `test_connection_basic.pas` 的长状态读取改成先 `Supports(..., ISSLConnectionInfo, ...)` 再走 `GetStateString`。
2. 在 `test_real_https_connection.pas` 中引入一个 `ISSLConnectionInfo`-first helper，用于握手失败输出。
3. 新增 focused contract，防止这两类活跃测试把 direct core `GetStateString` 教回普通路径。

## Verification

```bash
bash -n tests/scripts/test_isslconnectioninfo_getstatestring_active_test_contract.sh
bash tests/scripts/test_isslconnectioninfo_getstatestring_active_test_contract.sh
mkdir -p tmp/test_connection_basic && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_basic -FEtmp/test_connection_basic -otmp/test_connection_basic/test_connection_basic tests/connection/test_connection_basic.pas && ./tmp/test_connection_basic/test_connection_basic
mkdir -p tmp/test_real_https_connection && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_real_https_connection -FEtmp/test_real_https_connection -otmp/test_real_https_connection/test_real_https_connection tests/integration/test_real_https_connection.pas && ./tmp/test_real_https_connection/test_real_https_connection
git diff --check
```

## Expected Outcome

- ordinary generic/integration tests stop teaching direct core `GetStateString`
- `GetStateString` remaining live surface shrinks toward backend-specific runtime / contract-only residuals
- next batch can decide whether to classify those residuals or move to another mirror
