# `ISSLSessionResumption` Active Guidance De-emphasis

## Goal

把普通 API 示例、integration guide、以及通用 E2E 场景里仍把 session-resumption 能力当作 `ISSLConnection` 核心入口的调用点切到 `ISSLSessionResumption` owner path，让会话恢复这组能力面从“普通文档/测试仍直连 core”收缩到“仅 compatibility-core mirrors / backend-specific runtime 残留”。

## Scope

本批只处理 active guidance、focused contract 与台账：

- `docs/reference/API_REFERENCE.md`
- `docs/reference/API_DOCUMENTATION.md`
- `docs/INTEGRATION_GUIDE.md`
- `tests/integration/test_e2e_scenarios.pas`
- `tests/scripts/test_isslsessionresumption_active_guidance_contract.sh`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不修改生产实现
- 不碰 backend-specific session runtime / benchmark / WinSSL 专项测试
- 不重跑重型 Pascal/repo gate

## Why This Batch

当前 `ISSLSessionResumption` 的 cross-backend completion audit 已经完成，`tests/contract/test_backend_contract.pas` 也已把 owner path 锁住。

但普通文档与通用 E2E 示例里仍有 direct core 残余：

- `docs/reference/API_REFERENCE.md` 的 session-resumption 示例还在直接教 `LConn.GetSession / SetSession / IsSessionReused`
- `docs/reference/API_DOCUMENTATION.md` 的会话缓存 / 性能排查示例仍在直接教 `Connection.SetSession`
- `docs/INTEGRATION_GUIDE.md` 的 resumed session + early-data 示例还在直接读 `InitialStream.Connection.GetSession`
- `tests/integration/test_e2e_scenarios.pas` 仍在把 `Conn1.GetSession / Conn2.SetSession / Conn2.IsSessionReused` 当通用路径

这些文件更像“公开推荐路径”，比 backend-specific runtime tests 更适合优先收掉。

## Planned Changes

1. 把 `API_REFERENCE` 的 session-resumption 示例切到 `Supports(..., ISSLSessionResumption, ...)`。
2. 把 `API_DOCUMENTATION` 的会话缓存 / 性能排查示例切到 session-resumption owner path。
3. 把 `INTEGRATION_GUIDE` 的 resumed-session 例子切到 `ISSLSessionResumption.GetSession`。
4. 把 `test_e2e_scenarios.pas` 的 session 提取 / 注入 / reuse 检查切到 `ISSLSessionResumption`。
5. 新增 focused contract，防止普通文档/通用测试重新把 session-resumption 教回 direct core。

## Verification

```bash
bash -n tests/scripts/test_isslsessionresumption_active_guidance_contract.sh
bash tests/scripts/test_isslsessionresumption_active_guidance_contract.sh
mkdir -p tmp/test_e2e_scenarios && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_e2e_scenarios -FEtmp/test_e2e_scenarios -otmp/test_e2e_scenarios/test_e2e_scenarios tests/integration/test_e2e_scenarios.pas && ./tmp/test_e2e_scenarios/test_e2e_scenarios
git diff --check
```

## Expected Outcome

- ordinary docs stop teaching direct core session-resumption getters/setters as the preferred path
- the generic E2E session-resumption scenario stops treating `GetSession / SetSession / IsSessionReused` as the default call path
- remaining direct core session mirrors stay intentionally confined to compatibility-core truth and backend-specific runtime proof

## Result

- `docs/reference/API_REFERENCE.md` 的普通 session-resumption 示例现在统一改成：
  - `Supports(LConn, ISSLSessionResumption, ...)`
  - `ISSLSessionResumption.GetSession`
  - `ISSLSessionResumption.SetSession`
  - `ISSLSessionResumption.IsSessionReused`
- `docs/reference/API_DOCUMENTATION.md`
  的会话缓存 / 性能排错示例现在都先 capability-gate `ISSLSessionResumption`
- `docs/INTEGRATION_GUIDE.md`
  的 resumed session + early-data 例子现在先从 `InitialStream.Connection` 取 `ISSLSessionResumption`
- `tests/integration/test_e2e_scenarios.pas`
  的 session 提取 / 注入 / reuse 检查现在都通过 owner interface 完成
- 新增 focused contract：
  - `tests/scripts/test_isslsessionresumption_active_guidance_contract.sh`

## Route Impact

- `ISSLSessionResumption` 的 cross-backend completion truth 原本就已存在；这批之后 ordinary docs/tests 也不再继续把 session mirrors 当推荐主路径
- 默认下一步不该再重复做 session-resumption active-guidance 清扫
- 若继续沿同一类 optional-owner surface 推进，当前最值得切的是 `ISSLOCSPStapling` ordinary guidance：
  - `docs/reference/API_DOCUMENTATION.md` 仍保留多处 `Connection.GetOCSP*` direct-core 示例
