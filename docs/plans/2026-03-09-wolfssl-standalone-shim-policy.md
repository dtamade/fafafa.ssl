# WolfSSL Standalone Shim Policy Plan

**Goal**
- 把 `src/fafafa.ssl.wolfssl.connection.pas` 的 shim 身份从“当前实现现状”提升为显式策略。
- 防止后续修复再次把 standalone 单元扩回第二套完整运行时实现。

**Architecture**
- `src/fafafa.ssl.wolfssl.connection.pas` 继续保留历史公开类名，服务旧 imports。
- runtime 真相源固定在 `src/fafafa.ssl.wolfssl.context.pas` 的 `TWolfSSLContext.CreateConnection(...)`。
- standalone 单元只负责桥接 `ISSLConnection` / `ISSLClientConnection` / `ISSLNativeHandleAccess` 到 runtime path。
- 后续运行期语义修复优先落在 runtime path；standalone 单元只做兼容层维护。

**Files**
- Add: `docs/plans/2026-03-09-wolfssl-standalone-shim-policy.md`
- Add: `tests/scripts/test_wolfssl_standalone_shim_policy_contract.sh`
- Modify: `src/fafafa.ssl.wolfssl.connection.pas`
- Modify: `docs/reference/ARCHITECTURE.md`
- Update: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 shim policy 合同 RED。
2. 跑 RED，确认策略仍未显式写入源码头与架构文档。
3. 补源码头与架构策略说明。
4. 跑合同验证，并回写 working memory。

**Expected Outputs**
- WolfSSL standalone 单元的长期定位清晰可见。
- 后续维护者知道 runtime 真相源在哪里。
- compile-all 会继续编译 standalone 单元，但它不再被当成独立运行时实现演进。
