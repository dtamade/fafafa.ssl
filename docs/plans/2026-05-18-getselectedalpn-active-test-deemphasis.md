# `GetSelectedALPNProtocol` Active Test De-emphasis

## Goal

把普通 integration/contract 测试里仍把 `ISSLConnection.GetSelectedALPNProtocol` 当常规读取路径的调用点切到 `ISSLConnectionInfo.GetSelectedALPNProtocol`，让 `GetSelectedALPNProtocol` 的主线先从“普通测试仍直连 core”收缩到“仅 backend-specific runtime / contract mirror 残留”。

## Scope

本批只处理 active tests、focused contract 与台账：

- `tests/integration/test_real_https_connection.pas`
- `tests/integration/test_cross_backend_consistency_contract.pas`
- `tests/scripts/test_isslconnectioninfo_getselectedalpn_active_test_contract.sh`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不修改 public signature
- 不改 backend runtime ALPN tests
- 不改 `src/` 生产实现

## Why This Batch

当前活跃文档已经把 ALPN 推荐路径切到了 `ISSLConnectionInfo`，但普通 integration/contract 测试里仍有两类 direct core 残余：

- `tests/integration/test_real_https_connection.pas` 仍直接读取 `Conn.GetSelectedALPNProtocol`
- `tests/integration/test_cross_backend_consistency_contract.pas` 仍把 `Conn.GetSelectedALPNProtocol` 当归一化探测输出

这两类文件更像“普通推荐路径”，比 backend-specific runtime files 更适合优先收掉。

## Planned Changes

1. 在 `test_real_https_connection.pas` 中新增 `ISSLConnectionInfo`-first ALPN helper，并替换 ALPN 成功路径里的 direct core 读取。
2. 在 `test_cross_backend_consistency_contract.pas` 中新增 `ISSLConnectionInfo`-first helper，并把归一化探测输出切到该 helper。
3. 新增 focused contract，防止普通 integration/contract 测试重新把 direct core `GetSelectedALPNProtocol` 教回去。

## Verification

```bash
bash -n tests/scripts/test_isslconnectioninfo_getselectedalpn_active_test_contract.sh
bash tests/scripts/test_isslconnectioninfo_getselectedalpn_active_test_contract.sh
mkdir -p tmp/test_real_https_connection && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_real_https_connection -FEtmp/test_real_https_connection -otmp/test_real_https_connection/test_real_https_connection tests/integration/test_real_https_connection.pas && ./tmp/test_real_https_connection/test_real_https_connection
mkdir -p tmp/test_cross_backend_consistency_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_consistency_contract -FEtmp/test_cross_backend_consistency_contract -otmp/test_cross_backend_consistency_contract/test_cross_backend_consistency_contract tests/integration/test_cross_backend_consistency_contract.pas && ./tmp/test_cross_backend_consistency_contract/test_cross_backend_consistency_contract
git diff --check
```

## Expected Outcome

- ordinary integration/contract tests stop teaching direct core `GetSelectedALPNProtocol`
- `GetSelectedALPNProtocol` remaining live surface shrinks toward backend-specific runtime / contract-only residuals
- next batch can decide whether to freeze those residuals or discuss stronger client-owner wording
