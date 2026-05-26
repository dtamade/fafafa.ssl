# fafafa.ssl 目标树（项目总控地图）

> **更新**: 2026-05-26
> **愿景**: FreePascal 领域最优秀的 SSL/TLS 框架

---

## 🎯 顶层目标

```
fafafa.ssl = FreePascal 领域最优秀的 SSL/TLS 框架
├── 正确性：所有后端行为一致，安全默认值
├── 先进性：TLS 1.3 + 现代密码学 + 纯 Pascal 实现
├── 优雅性：Builder/Connector API，零配置即安全
├── 可维护性：三层契约体系，清晰的架构边界
└── 性能：Lock-free I/O，Buffer pool，Session cache
```

---

## 📊 后端完成度

| 后端 | TLS 1.2 | TLS 1.3 | 证书验证 | 会话复用 | OCSP | CT | 总评 |
|------|---------|---------|----------|----------|------|----|----|
| OpenSSL | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | 生产就绪 |
| MbedTLS | ✅ | ✅ | ✅ | ✅ | ❌ | ❌ | 生产就绪 |
| WolfSSL | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ | 生产就绪 |
| WinSSL | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ | 生产就绪 |
| FreePascal | 🔴 进行中 | 🟡 基础 | 🔴 简化 | 🟡 基础 | 🟡 框架 | 🟡 框架 | 实验性→生产 |

---

## 🗺️ 当前活跃工作

### FreePascal 后端生产化（主线）

```
docs/plans/2026-05-26-freepascal-tls12-tls13-full-stack-roadmap.md

P0 纯 Pascal 基线 ✅ 完成
├── P1 密码基础 ✅ 完成（AES-GCM, HMAC, HKDF, TLS1.2 PRF）
│   ├── P2 RSA + CBC 兼容 ✅ 完成（AES-CBC, RSA 加密, CBC+HMAC record anti-oracle）
│   ├── P3 X.509 链验证 ✅ 完成（RFC 5280, 签名验证, 主机名/SAN, BasicConstraints）
│   │   └── P4 CRL/OCSP 吊销 ← 下一步
│   └── P5 TLS 1.2 record/handshake
│       └── P6 TLS 1.2 KEX/认证
│           └── P7 TLS 1.2 cipher suites
├── P8 TLS 1.3 回归统一
├── P9 互操作矩阵
└── P10 性能/发布门禁
```

### 已完成的里程碑

- ✅ v1.5.0 发布（2026-05-17）
- ✅ v1.6.0 版本升级 + 安全审计（2026-05-26）
- ✅ 契约体系精简（531→388，三层 manifest）
- ✅ TSSLContextConfig 架构演进（Stage 0-5）
- ✅ 安全审计全部修复（30 findings, Critical→Low 100%）

---

## 📐 架构原则

1. **安全默认值** — TLS 1.3 优先，AEAD 优先，VerifyPeer 默认开启
2. **Fail-closed** — 未知状态拒绝而非接受
3. **Capability 不撒谎** — 能力矩阵必须反映真实 runtime 行为
4. **纯 Pascal 独立** — FreePascal 后端不依赖任何 C 库
5. **常量时间** — 所有密码学比较使用恒定时间操作
6. **TDD 驱动** — 先写 RED 测试，再实现，再 GREEN

---

## 🔄 开发循环

```
每个 Phase:
  1. 写 RED 测试（先定义期望行为）
  2. 运行确认失败
  3. 最小实现
  4. 运行确认通过
  5. python3 scripts/compile_all_modules.py --rebuild
  6. python3 scripts/run_contracts.py --tier core
  7. bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local
  8. git commit
  9. 与 Codex 复盘
```

---

## 📈 质量指标

| 指标 | 当前值 | 目标值 |
|------|--------|--------|
| 编译通过率 | 186/186 (100%) | 维持 100% |
| Core 契约 | 13/13 绿 | 维持全绿 |
| TLS 1.3 gate | 18/18 绿 | 维持全绿 |
| FreePascal 互操作 | 未验证 | 主流站点 100% |
| 性能 vs OpenSSL | 未测量 | ≤ 2-3x |

---

## 📋 版本规划

| 版本 | 内容 | 状态 |
|------|------|------|
| v1.5.0 | 初始多后端发布 | ✅ 已发布 |
| v1.6.0 | 安全审计 + 架构演进 + 契约精简 | 开发中 |
| v1.7.0 | FreePascal P0-P3（密码+证书验证） | 计划中 |
| v1.8.0 | FreePascal P4-P7（TLS 1.2 完整） | 计划中 |
| v2.0.0 | FreePascal 生产就绪 + API surgery | 远期 |
