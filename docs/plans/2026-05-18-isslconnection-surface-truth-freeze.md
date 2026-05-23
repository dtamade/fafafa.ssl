# ISSLConnection Surface Truth Freeze

## Goal

在不改动生产实现的前提下，把 `ISSLConnection` / `ISSLSession` 在活跃 API 文档中的源码真相冻结下来，避免后续会话继续被旧接口示例和过时签名误导。

## Scope

本批只处理文档与 focused contract：

- `docs/reference/API_REFERENCE.md`
- `tests/scripts/test_isslconnection_surface_truth_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`

不做：

- 不直接修改 `ISSLConnection` public signature
- 不修改 backend connection 实现
- 不重跑重型 compile-all / cross-platform gates

## Source Of Truth

- active source:
  - `src/fafafa.ssl.base.pas:1142-1544`
  - `src/fafafa.ssl.base.pas:1663-1684`
- current active doc drift:
  - `docs/reference/API_REFERENCE.md:413-930`
- design/migration reference:
  - `docs/reference/INTERFACE_DESIGN_V2.md:205-228`
  - `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`

## Why This Batch Comes Next

- `backend optional-surface` focused revalidation 已经补齐，当前不再需要怀疑这些 contract 是否真的跑过。
- `TSSLConfig` 那条线最近已经做过连续多批 truth freeze / parity / migration-map closeout。
- 当前真正会误导调用方和后续开发路线的，是活跃 API 文档里还在承诺旧版 `ISSLConnection` / `ISSLSession` surface。

## Planned Changes

1. 把 `ISSLConnection` 签名块改成当前源码真相：
   - 加回 `DoHandshake` / `IsHandshakeComplete` / `Renegotiate`
   - 加回 `WantRead` / `WantWrite` / `GetError`
   - 加回 `GetSelectedALPNProtocol`
   - 加回 `SetTimeout` / `GetTimeout`
   - 加回 `SetBlocking` / `GetBlocking`
   - 加回 `GetContext`
   - 加回 `GetStateString`
   - 加回 `GetSession` / `SetSession` / `IsSessionReused`
   - 加回 `GetVerifyResult` / `GetVerifyResultString`
   - 加回 `GetOCSP*`
2. 删除活跃文档里的旧接口承诺：
   - `GetCipherBits`
   - `VerifyPeerCertificate`
   - `GetSessionID`
   - `IsSessionResumed`
   - `GetSessionData`
   - `SetSessionData`
3. 明确可选接口 owner：
   - `ISSLNativeHandleAccess`
   - `ISSLConnectionInfo`
   - `ISSLDiagnostics`
   - `ISSLSessionResumption`
   - `ISSLCertificateVerification`
   - `ISSLOCSPStapling`
4. 更新 `WinSSL Session 管理` 示例，改用当前 `ISSLSession` surface：
   - `GetID`
   - `Serialize`
   - `IsSessionReused`

## Verification

```bash
bash -n tests/scripts/test_isslconnection_surface_truth_contract.sh
bash tests/scripts/test_isslconnection_surface_truth_contract.sh
git diff --check
```

## Expected Outcome

- 活跃 API 文档不再承诺源码里没有的 `ISSLConnection` / `ISSLSession` 方法
- `ISSLConnection` 当前的 compatibility-core reality 被清晰记录
- 下一批可以在稳定 truth freeze 之上，选择第一条真实 slimming slice，而不是继续修文档幻觉

## Execution Result

- PASS.
- Revalidated `tests/scripts/test_isslconnection_surface_truth_contract.sh` with `bash -n` and `bash`.
- Current API/session documentation still matches source truth; no runtime or doc edits were needed in this closeout pass.
