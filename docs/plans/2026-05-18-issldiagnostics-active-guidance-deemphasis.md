# `ISSLDiagnostics` Active Guidance De-emphasis

## Goal

把普通 API 示例与通用 boundary 测试里仍把 diagnostics 能力当作 `ISSLConnection` 核心入口的调用点切到 `ISSLDiagnostics` owner path，让 diagnostics 这组能力面先从“普通文档/测试仍直连 core”收缩到“仅 backend-specific runtime / contract 残留”。

## Scope

本批只处理 active guidance、focused contract 与台账：

- `docs/reference/API_REFERENCE.md`
- `tests/test_sslctxboth_roleless_handshake_clarification.pas`
- `tests/scripts/test_issldiagnostics_active_guidance_contract.sh`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不修改生产实现
- 不改 `tests/winssl/test_winssl_monitoring.pas`
- 不改 `tests/winssl/test_winssl_connection_edge_cases.pas`
- 不重跑重型 Pascal/repo gate

## Why This Batch

当前 `ISSLDiagnostics` 的 cross-backend completion audit 已经完成，`tests/contract/test_backend_contract.pas` 也已把 owner path 锁住。

但普通 API 文档与一份通用 boundary 测试里仍有 direct core 残余：

- `docs/reference/API_REFERENCE.md` 的监控/诊断示例还在直接教 `LConn.IsHealthy` / `LConn.GetHealthStatus` / `LConn.GetPerformanceMetrics` / `LConn.GetDiagnosticInfo`
- `tests/test_sslctxboth_roleless_handshake_clarification.pas` 还在把 `LConn.GetHealthStatus` 当普通读取路径

这些文件更像“公开推荐路径”，比 backend-specific runtime tests 更适合优先收掉。

## Planned Changes

1. 把 `API_REFERENCE` 的普通 diagnostics 示例切到 `Supports(LConn, ISSLDiagnostics, LDiag)`。
2. 把 `test_sslctxboth_roleless_handshake_clarification.pas` 的 health 读取切到 diagnostics owner path。
3. 新增 focused contract，防止普通文档/通用测试重新把 diagnostics 教回 direct core。

## Verification

```bash
bash -n tests/scripts/test_issldiagnostics_active_guidance_contract.sh
bash tests/scripts/test_issldiagnostics_active_guidance_contract.sh
mkdir -p tmp/test_sslctxboth_roleless_handshake_clarification && fpc -B -Fu./src -Fu./tests -FUtmp/test_sslctxboth_roleless_handshake_clarification -FEtmp/test_sslctxboth_roleless_handshake_clarification -otmp/test_sslctxboth_roleless_handshake_clarification/test_sslctxboth_roleless_handshake_clarification tests/test_sslctxboth_roleless_handshake_clarification.pas && ./tmp/test_sslctxboth_roleless_handshake_clarification/test_sslctxboth_roleless_handshake_clarification
git diff --check
```

## Expected Outcome

- ordinary API guidance stops teaching direct core diagnostics getters
- the generic dual-context boundary test stops treating `GetHealthStatus` as the default path
- the remaining direct core diagnostics residuals stay intentionally confined to backend-specific runtime proof

## Result

- `docs/reference/API_REFERENCE.md` 的 ordinary diagnostics examples 现在统一改成：
  - `Supports(LConn, ISSLDiagnostics, LDiag)`
  - `LDiag.IsHealthy`
  - `LDiag.GetHealthStatus`
  - `LDiag.GetPerformanceMetrics`
  - `LDiagExt.GetDiagnosticInfo`
- `tests/test_sslctxboth_roleless_handshake_clarification.pas`
  现在先验证：
  - `Supports(LConn, ISSLDiagnostics, LDiag)`
  再读取：
  - `LDiag.GetHealthStatus`
- 新增 focused contract：
  - `tests/scripts/test_issldiagnostics_active_guidance_contract.sh`

## Route Impact

- `ISSLDiagnostics` 的 cross-backend completion truth 已经存在；这批之后 ordinary docs/tests 也不再继续把 diagnostics core getters 当推荐主路径
- 默认下一步不该再重复做 diagnostics active-guidance 清扫
- 若继续沿同类 optional-owner surface 推进，应切到下一组 ordinary guidance 仍偏 core 的接口，或回到更大的 interface-design completeness 选择
