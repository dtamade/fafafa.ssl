# Interface Goal Route Refresh

## Goal

基于 2026-05-21 当前仓库真相，刷新 `fafafa.ssl` 这条
“接口设计 + 各 backend 实现 completeness” 主 goal 的即时路线，
避免后续会话继续把已经关闭的问题当成下一步。

## Why This Batch

最近 handoff / 审查摘要里，仍有两条已经关闭的路线被重新当成候选：

- `direct-library connection-scope clarification`
- `backend connection-surface execution receipt gap`

但当前仓库真相已经更靠前：

- `ValidateDirectLibraryConnectionScope(...)`
  已经存在于 shared helper，
  五个 backend library `CreateContext(AType)` 也都已接入
- focused contract / runtime proof 当前继续绿色
- 三份 2026-05-04 连接层旧 plan
  已经带有
  `Focused Revalidation Result (2026-05-18)`

这说明当前真正的问题，
已经不再是“这些地方还没修 / 还没验证”，
而是“路线选择如果不刷新，会反复回到旧 closeout”。

## Verified Truth

### 1. direct-library connection-scope 已关闭且仍然有效

- `src/fafafa.ssl.context.config.pas`
  当前已有
  `ValidateDirectLibraryConnectionScope(...)`
- `src/fafafa.ssl.openssl.backed.pas`
- `src/fafafa.ssl.freepascal.lib.pas`
- `src/fafafa.ssl.winssl.lib.pas`
- `src/fafafa.ssl.mbedtls.lib.pas`
- `src/fafafa.ssl.wolfssl.lib.pas`
  当前都已在
  `CreateContext(AType)`
  里接入同一条 validator
- focused proof：
  - `bash tests/scripts/test_direct_library_connection_scope_clarification_contract.sh`
    - PASS
  - `tests/test_freepascal_library_default_config_connection_scope_clarification.pas`
    - PASS
  - `tests/test_factory_connection_scope_clarification.pas`
    - PASS

### 2. 连接层旧 plan 的 execution receipt gap 已关闭

以下三份旧 plan 当前都已经带有 focused revalidation receipt：

- `docs/plans/2026-05-04-backend-client-connection-sni-interface-alignment.md`
- `docs/plans/2026-05-04-backend-connection-native-handle-interface-alignment.md`
- `docs/plans/2026-05-04-backend-ocsp-connection-interface-alignment.md`

因此当前不需要再为“补 receipt”单独拉起一轮 contract batch。

## Route Decision

从这一步开始，
默认不要再回到：

- 文档 metadata 扫尾
- direct-library `HandshakeTimeout` / `BufferSize` 漂移
- 连接层三份旧 plan 的 receipt 补写

当前更真实的下一阶段，
应切回仍未完成的 broader interface debt：

1. `ISSLConnection` core-too-fat
2. `TSSLConfig` mixed-scope public record 的剩余长期拆分设计
3. facade 仍同时导出多条历史入口

### Immediate Next Preference

就当前仓库状态看，
下一条最值得继续推进的主线应优先是：

- `ISSLConnection` broader slimming / recommendation truth 的下一刀

原因：

- `TSSLConfig` 这条线的高价值 slice
  已经完成了大量 truth freeze / helper adoption / parity closeout
- 当前再回去做它，
  更容易落成重复梳理，
  而不是新的接口价值
- `ISSLConnection` 仍然是接口层最显眼、
  也最接近真正 `v2` 设计债的残余核心

## Files

- `docs/plans/2026-05-21-interface-goal-route-refresh.md`
- `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification

```bash
bash -n tests/scripts/test_direct_library_connection_scope_clarification_contract.sh
bash tests/scripts/test_direct_library_connection_scope_clarification_contract.sh
mkdir -p tmp/test_freepascal_library_default_config_connection_scope_clarification && \
  fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_freepascal_library_default_config_connection_scope_clarification \
  -FEtmp/test_freepascal_library_default_config_connection_scope_clarification \
  -otmp/test_freepascal_library_default_config_connection_scope_clarification/test_freepascal_library_default_config_connection_scope_clarification \
  tests/test_freepascal_library_default_config_connection_scope_clarification.pas && \
  ./tmp/test_freepascal_library_default_config_connection_scope_clarification/test_freepascal_library_default_config_connection_scope_clarification
mkdir -p tmp/test_factory_connection_scope_clarification && \
  fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_factory_connection_scope_clarification \
  -FEtmp/test_factory_connection_scope_clarification \
  -otmp/test_factory_connection_scope_clarification/test_factory_connection_scope_clarification \
  tests/test_factory_connection_scope_clarification.pas && \
  ./tmp/test_factory_connection_scope_clarification/test_factory_connection_scope_clarification
```

## Expected Outcome

- 后续会话不再把已经关闭的 direct-library / receipt gap 当成即时下一步
- 当前主 goal 的路线回到真正未完成的 interface debt
- 用户再问“接下来怎么办”时，可以直接落到新的高价值切片，而不是重复治理
