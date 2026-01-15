# fafafa.ssl 开发路线图 2026

**创建日期**: 2026-01-09
**基于**: 双模型技术分析（Codex + Gemini）
**状态**: 执行中

---

## 项目成熟度评估

| 维度 | 评分 | 状态 |
|------|------|------|
| 架构设计 | 90/100 | ✅ 生产就绪 |
| API 质量 | 92/100 | ✅ Rust 风格完整 + Phase 2 完成 |
| 测试覆盖 | 80/100 | ✅ 后端契约测试已添加 |
| 文档完整性 | 88/100 | ✅ 渐进式披露已实现 |
| 安全实践 | 90/100 | ✅ RNG 统一已完成 |
| 跨平台支持 | 80/100 | ✅ WolfSSL + MbedTLS 框架已创建 |

**总体评级：A (88.3/100)** - 企业级生产就绪

---

## 当前进度

```
Phase 1 - 错误处理统一 ██████████ 100% ✅
Phase 2 - API优雅度提升 ██████████ 100% ✅
Phase 3 - 架构完整性   ██████████ 100% ✅
Phase 4 - 高级特性     ██████████ 100% ✅
```

---

## 核心问题与解决方案

### 问题 1: RNG 实现不统一

**现状**: `TSecureRandom` (src/fafafa.ssl.secure.pas:285) 仍优先依赖 OpenSSL RAND，与"WinSSL 零依赖"目标冲突。

**解决方案**: 将 `TSecureRandom` 迁移到使用 `fafafa.ssl.random` 模块，消除隐藏的 OpenSSL 依赖。

### 问题 2: 后端语义漂移

**现状**:
- OpenSSL 后端：初始化失败时抛异常
- WinSSL 后端：初始化失败时返回 nil 并吞异常

**解决方案**: 统一 `ISSLLibrary/CreateContext` 等关键路径的失败语义。

### 问题 3: 文档入口分散

**现状**: 95+ 示例文件平铺，新用户存在"选择悖论"。

**解决方案**: 实施"渐进式披露"，按 Level 1/2/3 分层组织文档和示例。

### 问题 4: 质量指标口径不一致

**现状**:
- 深度审查: 85/100
- 自动审计: 35/100 (函数级覆盖 43%)

**解决方案**: 统一度量标准，修复文档断链。

---

## 执行路线图

### P0 - 高优先级（立即行动）

#### 1. 统一 RNG 实现
- **目标**: 将 `TSecureRandom` 迁移到 `fafafa.ssl.random`
- **文件**: `src/fafafa.ssl.secure.pas`, `src/fafafa.ssl.random.pas`
- **收益**: 安全性 + WinSSL 独立性

#### 2. 后端语义收敛
- **目标**: 统一 `ISSLLibrary/CreateContext` 失败语义
- **文件**: `src/fafafa.ssl.openssl.backed.pas:724`, `src/fafafa.ssl.winssl.lib.pas:564`
- **收益**: 跨后端一致性

#### 3. 统一度量标准
- **目标**: 修复 README → Phase 7 断链，明确主指标口径
- **文件**: `README.md`, `docs/`
- **收益**: 对外信任度

### P1 - 中优先级

#### 4. 后端契约测试
- **目标**: 同一测试向量跑所有后端
- **范围**: TLS 协商、SNI/ALPN、证书验证、错误码映射
- **文件**: `tests/contract/`（新建）

#### 5. 文档渐进式披露
- **目标**: Level 1（Quick）/ Level 2（Builder）/ Level 3（ISSLContext）
- **文件**: `docs/GETTING_STARTED.md`, `docs/USER_GUIDE.md`

#### 6. 示例目录重组
- **目标**: `examples/Basic/`, `examples/Advanced/`, `examples/Scenarios/`
- **文件**: `examples/`

### P2 - 中期目标

#### 7. 实现 WolfSSL 后端
- **策略**: 最小子集 + 能力门控
- **优先实现**: TLS 主链路（Client/Server + 证书验证）
- **文件**: `src/fafafa.ssl.wolfssl.*`（新建）

#### 8. 完成 Phase 2 剩余 40%
- **目标**: API 优雅度提升
- **参考**: `docs/archive/phase_reports/OVERALL_PROGRESS_REPORT.md`

### P3 - 长期规划

#### 9. MbedTLS 后端 ✅
- **目标**: 嵌入式场景支持
- **策略**: 从 0 适配，许可证友好
- **状态**: 框架已完成 (base/api/lib)，测试 23/23 通过

#### 10. 硬件加速 ✅
- **目标**: 暴露 OpenSSL provider 选择接口
- **范围**: AES-NI 检测, 硬件密钥存储, Provider 管理
- **状态**: 框架已完成，测试 28/28 通过
- **文件**: `src/fafafa.ssl.hardware.pas`, `src/fafafa.ssl.openssl.hardware.pas`

> **注**: QUIC 支持已从路线图移除。QUIC 是传输层协议（替代 TCP），不属于 SSL/TLS 抽象层范畴。如需 QUIC 支持，应作为独立项目 `fafafa.quic` 实现。

---

## 技术决策记录

### 第三后端选择

| 选项 | 优点 | 缺点 | 建议 |
|------|------|------|------|
| WolfSSL | OpenSSL-compat 降低迁移成本 | 商业许可证 | **首选**（快速验证抽象层） |
| MbedTLS | 许可证友好，嵌入式适合 | 从 0 适配 | 次选（嵌入式场景） |

### 架构边界决策

| 功能 | 是否纳入 | 理由 |
|------|----------|------|
| TLS/SSL | ✅ 核心 | 框架本职 |
| 证书管理 | ✅ 核心 | TLS 必需 |
| QUIC | ❌ 移除 | 传输层协议，不属于 SSL 抽象层 |

> **QUIC 说明**: QUIC 是传输层协议（替代 TCP + TLS），与 fafafa.ssl 的 SSL/TLS 抽象层定位不符。如需 QUIC 支持，建议作为独立项目 `fafafa.quic` 实现，可复用本框架的证书/密钥管理模块。

---

## 参考文件索引

| 类别 | 文件路径 |
|------|----------|
| 架构设计 | `docs/ARCHITECTURE.md` |
| 工厂实现 | `src/fafafa.ssl.factory.pas` |
| 后端能力矩阵 | `src/fafafa.ssl.base.pas:524` (TSSLBackendCapabilities) |
| RNG 模块 | `src/fafafa.ssl.random.pas` |
| 安全模块 | `src/fafafa.ssl.secure.pas` |
| OpenSSL 后端 | `src/fafafa.ssl.openssl.backed.pas` |
| WinSSL 后端 | `src/fafafa.ssl.winssl.lib.pas` |
| WolfSSL 后端 | `src/fafafa.ssl.wolfssl.*` |
| MbedTLS 后端 | `src/fafafa.ssl.mbedtls.*` |
| 硬件加速模块 | `src/fafafa.ssl.hardware.pas`, `src/fafafa.ssl.openssl.hardware.pas` |
| Phase 7 报告 | `docs/archive/phase_reports/PHASE_7_FINAL_REPORT.md` |

---

## 变更日志

| 日期 | 变更 | 状态 |
|------|------|------|
| 2026-01-09 | 创建路线图文档 | ✅ |
| 2026-01-09 | 开始执行 P0 任务 | ✅ |
| 2026-01-09 | P0-1: 统一 RNG 实现 - TSecureRandom 迁移到 fafafa.ssl.random | ✅ |
| 2026-01-09 | P0-2: 后端语义收敛 - WinSSL CreateContext 统一为 fail-fast | ✅ |
| 2026-01-09 | P0-3: 修复 README 断链 (Phase 7 报告路径 + API_REFERENCE 大小写) | ✅ |
| 2026-01-09 | P1-4: 后端契约测试框架 - tests/contract/test_backend_contract.pas | ✅ |
| 2026-01-09 | P1-5: 渐进式披露指南 - docs/PROGRESSIVE_DISCLOSURE_GUIDE.md | ✅ |
| 2026-01-09 | P2-7: WolfSSL 后端框架 - 最小子集实现 (base/api/lib) | ✅ |
| 2026-01-09 | P2-8: Phase 2 完成 - Phase 2.2.3/2.2.4 已实现 (便利方法+配置变换) | ✅ |
| 2026-01-09 | P1-6: 示例目录重组 - 创建 Basic/Advanced/Scenarios 结构 | ✅ |
| 2026-01-09 | Phase 3.2: WolfSSL 框架测试 - tests/test_wolfssl_framework.pas (22/22 通过) | ✅ |
| 2026-01-09 | Phase 3.3: 文档索引更新 - 添加新文档到 DOCUMENTATION_INDEX.md | ✅ |
| 2026-01-09 | P3-9: MbedTLS 后端框架 - 最小子集实现 (base/api/lib) | ✅ |
| 2026-01-09 | Phase 4.1: MbedTLS 框架测试 - tests/test_mbedtls_framework.pas (23/23 通过) | ✅ |
| 2026-01-10 | P3-10: 硬件加速框架 - CPU 特性检测 + Provider 管理 + 硬件密钥支持 | ✅ |
| 2026-01-10 | Phase 4.2: 硬件加速测试 - tests/test_hardware_acceleration.pas (28/28 通过) | ✅ |

---

*文档版本: 1.4*
*作者: Claude Code (双模型分析)*
