# fafafa.ssl 文档索引

> 渐进式文档体系 - 从入门到精通

---

## Level 1: 快速入门 (30秒上手)

**目标**: 复制-粘贴-运行，立即看到结果

| 文档 | 描述 | 阅读时间 |
|------|------|----------|
| [README.md](../README.md) | 项目概览和 30 秒示例 | 2 分钟 |
| [GETTING_STARTED.md](GETTING_STARTED.md) | 环境配置和第一个程序 | 5 分钟 |
| [QUICKSTART.md](QUICKSTART.md) | 核心 API 快速参考 | 3 分钟 |
| [COOKBOOK.md](COOKBOOK.md) | **推荐** - 场景化代码片段集合 | 按需查阅 |

**示例代码**:
- `examples/quickstart_complete.pas` - 完整可运行的 TLS 客户端
- `examples/Basic/` - 入门示例
- `examples/Advanced/` - 高级示例
- `examples/Scenarios/` - 场景化示例

---

## Level 2: 日常开发 (常见任务)

**目标**: 解决 80% 的日常开发需求

### 2.1 TLS 连接

| 文档 | 描述 |
|------|------|
| [USER_GUIDE.md](USER_GUIDE.md) | 完整用户指南 |
| [LINUX_QUICKSTART.md](LINUX_QUICKSTART.md) | Linux 平台指南 |
| [WINSSL_QUICKSTART.md](WINSSL_QUICKSTART.md) | Windows 原生 TLS |

### 2.2 证书管理

| 文档 | 描述 |
|------|------|
| [CA_CERTIFICATE_AUTO_LOADING.md](CA_CERTIFICATE_AUTO_LOADING.md) | 系统 CA 自动加载 |
| [SECURITY_GUIDE.md](SECURITY_GUIDE.md) | 安全最佳实践 |

### 2.3 加密操作

| 文档 | 描述 |
|------|------|
| [AEAD_SUPPORT.md](AEAD_SUPPORT.md) | AES-GCM 等认证加密 |
| [AEAD_QUICK_REFERENCE.md](AEAD_QUICK_REFERENCE.md) | AEAD 快速参考 |

### 2.4 错误处理

| 文档 | 描述 |
|------|------|
| [ERROR_HANDLING_BEST_PRACTICES.md](ERROR_HANDLING_BEST_PRACTICES.md) | 错误处理最佳实践 |
| [TROUBLESHOOTING.md](TROUBLESHOOTING.md) | 常见问题排查 |
| [FAQ.md](FAQ.md) | 常见问题解答 |

---

## Level 3: 深入理解 (高级主题)

**目标**: 架构理解、性能优化、贡献代码

### 3.1 架构设计

| 文档 | 描述 |
|------|------|
| [ARCHITECTURE.md](ARCHITECTURE.md) | 四后端架构设计 |
| [API_REFERENCE.md](API_REFERENCE.md) | 完整 API 文档 |
| [API_DESIGN_GUIDE.md](API_DESIGN_GUIDE.md) | API 设计原则 |

### 3.2 后端实现

| 文档 | 描述 |
|------|------|
| [OPENSSL_MODULES.md](OPENSSL_MODULES.md) | OpenSSL 模块详解 |
| [WINSSL_DESIGN.md](WINSSL_DESIGN.md) | WinSSL 设计文档 |
| [WINSSL_USER_GUIDE.md](WINSSL_USER_GUIDE.md) | WinSSL 用户指南 |

### 3.3 迁移和兼容性

| 文档 | 描述 |
|------|------|
| [MIGRATION_GUIDE.md](MIGRATION_GUIDE.md) | 版本迁移指南 |
| [RUST_ALIGNMENT_ROADMAP.md](RUST_ALIGNMENT_ROADMAP.md) | Rust 风格 API 路线图 |

### 3.4 测试和质量

| 文档 | 描述 |
|------|------|
| [TESTING.md](TESTING.md) | 测试指南 |
| [CICD_SETUP.md](CICD_SETUP.md) | CI/CD 配置 |
| [SECURITY_AUDIT.md](SECURITY_AUDIT.md) | 安全审计报告 |

### 3.5 部署和运维

| 文档 | 描述 |
|------|------|
| [DEPLOYMENT_GUIDE.md](DEPLOYMENT_GUIDE.md) | 生产部署指南 |
| [ZERO_DEPENDENCY_DEPLOYMENT.md](ZERO_DEPENDENCY_DEPLOYMENT.md) | 零依赖部署 |
| [MAINTENANCE_PLAN.md](MAINTENANCE_PLAN.md) | 维护计划 |

### 3.6 开发规范

| 文档 | 描述 |
|------|------|
| [CODE_STYLE.md](CODE_STYLE.md) | 代码风格指南 |
| [CODING_STANDARDS.md](CODING_STANDARDS.md) | 编码标准 |
| [RETURN_TYPE_CONVENTIONS.md](RETURN_TYPE_CONVENTIONS.md) | 返回类型约定 |

---

## 按角色导航

### 新用户
1. README.md → 2. GETTING_STARTED.md → 3. USER_GUIDE.md

### 应用开发者
1. QUICKSTART.md → 2. API_REFERENCE.md → 3. ERROR_HANDLING_BEST_PRACTICES.md

### 安全工程师
1. SECURITY_GUIDE.md → 2. SECURITY_AUDIT.md → 3. AEAD_SUPPORT.md

### 贡献者
1. ARCHITECTURE.md → 2. CODE_STYLE.md → 3. TESTING.md

---

## 后端支持状态

| 后端 | 状态 | 文档 |
|------|------|------|
| OpenSSL | **Stable** | [OPENSSL_MODULES.md](OPENSSL_MODULES.md) |
| WinSSL | **Stable** | [WINSSL_USER_GUIDE.md](WINSSL_USER_GUIDE.md) |
| MbedTLS | Preview | [ARCHITECTURE.md](ARCHITECTURE.md) |
| WolfSSL | Preview | [ARCHITECTURE.md](ARCHITECTURE.md) |

---

## 归档文档

历史报告和会话记录位于 `docs/archive/` 目录：
- `archive/phase_reports/` - 阶段完成报告
- `archive/reports/` - 技术报告
- `archive/sessions/` - 开发会话记录

---

*最后更新: 2026-01-11*
