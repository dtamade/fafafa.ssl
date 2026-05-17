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

1. 给 capability model 建一个**单一真相规则**
   - 明确 legacy boolean 只是兼容派生字段
   - focused contract 覆盖 bool/support-level 一致性

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
