# fafafa.ssl Interface And Backend Verification

**日期:** 2026-05-18  
**状态:** PARTIAL_CLOSEOUT  
**范围:** public interface、factory/builder/config、capability truth、各 backend 实现对齐  
**方式:** 静态审查 + focused contracts/tests + 最小真相修复

## 目标

验证 `fafafa.ssl` 的接口设计与各 backend 实现是否还保持同一套真相源，并留下稳定记录，避免后续再从 release closeout 或旧 roadmap 入口重新启动。

## 本轮结论

当前问题不是“项目没实现”，而是“public surface 与兼容语义还在背着历史包袱前进”。

这轮已经确认三类真问题：

1. **文档承诺漂移**
   - 活跃文档曾把 `ISSLServerConnection` 画进 public interface graph。
   - live source 没有这个接口。
   - 这类漂移已经在本批修复，并用静态合同守住。

2. **context-level SNI 旧语义仍然是当前实现真相**
   - `ISSLContext.SetServerName` 虽然 deprecated，但 factory、builder、各 backend connection 构造器仍在使用。
   - 多份 focused 测试还把这种 fallback 继承当成兼容行为固定了下来。
   - 这不是一个局部脏点，而是一条已经被实现和合同共同保护的兼容路径。

3. **capability model 仍存在系统性双真相**
   - backend `GetCapabilities` 仍同时发布 legacy boolean 与 support-level。
   - selector 信 support-level。
   - serializer / diff 同时 round-trip 两套字段。
   - backend contract 中仍有部分 optional interface alignment 主要看旧布尔字段。
   - 这意味着 capability compatibility layer 还没有完成“兼容字段从属化”。

## 已确认但不属于本批 bugfix 的设计债

### 1. `TSSLConfig` 跨层字段仍然偏重

- `BufferSize` / `HandshakeTimeout` 看起来像配置字段，但在 factory 路径里会被显式拒绝。
- 这说明它们不是 silent no-op，而是 connection-scoped config 被继续挂在公共 record 上。
- 这是设计负担，不是本批发现的新隐藏 bug。

### 2. SNI 迁移必须视作兼容性迁移

- 不能把“删掉 context-level ServerName fallback”当成一次小修。
- 它会同时影响：
  - `TSSLFactory`
  - `TSSLContextBuilderImpl`
  - OpenSSL / WinSSL / FreePascal / MbedTLS / WolfSSL connection 构造器
  - 多份 focused tests / contracts

## 本批修复

- 更新 [docs/ARCHITECTURE.md](../ARCHITECTURE.md)
  - 不再把 `ISSLServerConnection` 画进当前 public interface graph。
- 更新 [docs/reference/INTERFACE_DESIGN_V2.md](../reference/INTERFACE_DESIGN_V2.md)
  - 不再把 `ISSLServerConnection` 当作活跃接口层次的一部分。
- 新增 `tests/scripts/test_interface_docs_no_nonexistent_isserverconnection_contract.sh`
  - 静态守护“活跃文档不能再承诺源码里不存在的 public interface”。

## 增量收口：runtime capability bool/support 真相对齐

- `src/fafafa.ssl.base.pas` 新增 `NormalizeLegacyCapabilityBooleans(...)`
  - 规则非常明确：runtime truth 以 `SNISupport` / `ALPNSupport` / `OCSPStaplingSupport` / `CertTransparencySupport` / `SessionTicketsSupport` 为准。
  - legacy boolean 现在只作为 compatibility projection，由 shared helper 统一回填。

- 以下 backend 的 `GetCapabilities` 已接入统一归一化：
  - OpenSSL
  - FreePascal
  - WinSSL
  - MbedTLS
  - WolfSSL

- focused contracts 已同步切换到 support-level truth：
  - `tests/contract/test_capabilities_contract.pas`
    - major backend 的 capability presence 改为检查 `SNISupport` / `ALPNSupport <> None`
    - 新增 SNI / ALPN / OCSP / CT / SessionTickets 的 bool/support-level 一致性断言
  - `tests/contract/test_backend_contract.pas`
    - SNI / CT / OCSP optional interface alignment 改为信 `*Support <> None`

- 这一步的意义不是删除 legacy 字段，而是先把 runtime 侧的双真相压成单一来源：
  - backend 不再各自手填一套可能漂移的旧布尔值
  - contract 也不再把 legacy boolean 当作主真相字段

- focused 验证：
  - `bash tests/scripts/test_capability_legacy_bool_normalization_contract.sh`
    - PASS
  - `tests/contract/test_capabilities_contract.pas`
    - PASS (`63 passed, 0 failed, 1 skipped`)
  - `tests/contract/test_backend_contract.pas`
    - PASS (`111 passed, 0 failed, 24 skipped`)
  - `git diff --check`
    - PASS

## 增量收口：deserializer precedence 与 diff support-level truth

- 这轮继续往下审，确认 `serializer / deserializer / diff` 线不是“概念上双真相”，而是已经有两处具体行为缺口：
  1. `JSONToCapabilities(...)` / `XMLToCapabilities(...)`
     - 对同时带 legacy boolean 与 `*Support` 的 payload，没有真相优先级
     - 冲突输入会把旧布尔值错误保留下来
  2. `CompareCapabilities(...)`
     - 原先几乎只比较 legacy boolean
     - 会漏掉 `SNISupport`、`EarlyDataSupport` 这类 v1.2 support-level 真实变化

- 本批修复后的规则：
  - 反序列化时：
    - 若 payload 包含某个 `*Support` 字段，则该字段为真相源，并回填对应 legacy boolean
    - 若 payload 只有旧 boolean，则继续保持旧输入兼容，不强行猜测 support-level
  - diff 时：
    - paired capability 优先比较 `*Support`
    - support-level-only capability 也纳入 diff
    - legacy boolean 只作为没有 support-level truth 时的 fallback

- 新增 focused regressions：
  - `tests/test_capability_deserialization_truth_precedence.pas`
    - 先红后绿，钉住“support-level 覆盖冲突 legacy boolean”
  - `tests/test_capability_diff_support_level_truth.pas`
    - 先红后绿，钉住“diff 不能忽略 support-level 变化”

- 兼容确认：
  - 现有 `tests/test_capability_deserialization_roundtrip.pas`
    - PASS
    - 说明这次修的是 precedence / truth，对既有 JSON/XML round-trip 没有打穿

## 验证证据

- `bash tests/scripts/test_interface_docs_no_nonexistent_isserverconnection_contract.sh`
  - PASS
- `tests/test_factory_connection_scope_clarification.pas`
  - PASS
- `tests/test_factory_server_name_scope_clarification.pas`
  - PASS
- `tests/test_sslctxboth_client_capability_clarification.pas`
  - PASS
- `git diff --check`
  - PASS

## 当前最重要的路线判断

### 不要再迷失到这些线

- 不要回到 `v1.5.0` release closeout
- 不要回到旧的 SHA384 parity 重复验证
- 不要把 context-level SNI 问题误判成“一两个 setter 就能删掉”的局部清理

### 下一批最值得做的事

1. 收口 serializer / deserializer / diff 层的 capability 双真相
   - runtime `GetCapabilities`、deserializer precedence、diff truth 已经完成主真相化
   - 剩下的问题主要是：serializer 输出面是否还要对“手工构造但不一致”的 capability record 做受控归一化

2. 设计一份 **context-level SNI compatibility migration plan**
   - 先定义 compatibility shim 和 deprecation boundary
   - 再分批清理 factory / builder / connection constructor / tests

3. 再决定是否拆 `TSSLConfig`
   - 当前它更多是设计债，不是第一优先级实现 bug

## 总结

这轮已经把“接口设计是不是出了问题”和“这些问题有没有扩散到实现层”两件事都钉实了：

- **有问题，而且不是猜测。**
- **其中一部分已经是实现真相。**
- **最小高价值修复已经落地。**
- **更大的迁移方向也已经明确，不需要下次再重新判断。**
