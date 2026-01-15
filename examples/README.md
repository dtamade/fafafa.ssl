# fafafa.ssl 示例程序

本目录包含 fafafa.ssl 的示例程序，按技能水平和场景分类组织。

## 🎯 快速导航

| 目录 | 描述 | 适用场景 |
|------|------|----------|
| **[Basic/](Basic/)** | 入门示例 | 快速原型、简单请求、1-3 行代码 |
| **[Advanced/](Advanced/)** | 高级示例 | 完整控制、多后端、架构定制 |
| **[Scenarios/](Scenarios/)** | 场景化示例 | 按需求查找完整解决方案 |
| **[TLS/](TLS/)** | TLS 连接示例 | TLS 客户端/服务器、会话复用 |
| **[Crypto/](Crypto/)** | 加密示例 | AES-GCM、哈希、密码派生 |
| **[Tests/](Tests/)** | 测试程序 | 模块测试、集成测试 |
| **[Utilities/](Utilities/)** | 工具模块 | TCP 辅助、Socket 封装 |

## 🚀 30秒快速开始

根目录保留了一个完整可运行的示例：

```bash
# 编译
fpc -Fusrc -Fusrc/openssl -Fuexamples quickstart_complete.pas

# 运行
./quickstart_complete
```

## 📋 我应该从哪里开始？

- **"我只想发个 HTTPS 请求"** → [Basic/](Basic/)
- **"我需要双向 TLS / 自定义后端"** → [Advanced/](Advanced/)
- **"我要加密文件 / 数字签名"** → [Scenarios/](Scenarios/)
- **"我要学习 TLS 连接细节"** → [TLS/](TLS/)
- **"我要学习加密操作"** → [Crypto/](Crypto/)

## 📚 推荐 API 风格

### 现代 API（推荐）

```pascal
uses fafafa.ssl.connection.builder, fafafa.ssl.quick;

// TLS 连接 - Fluent Builder
Conn := TSSLConnectionBuilder.Create
  .WithHostname('api.example.com')
  .WithTimeout(30000)
  .BuildClient;

// 证书生成 - Quick API
TSSLQuick.GenerateCertFiles('localhost', 'server.crt', 'server.key');

// 错误处理 - Result 类型（无异常）
if TCryptoUtils.TrySHA256(Data, Hash) then
  ProcessData(Hash);
```

### 底层 API（完整控制）

```pascal
// 工厂模式
Ctx := TSSLFactory.CreateContext(sslCtxClient);
Conn := Ctx.CreateConnection(Socket);
Conn.Connect;
```

## 📖 相关文档

- [文档索引](../docs/INDEX.md) - 渐进式文档导航
- [API 参考](../docs/API_REFERENCE.md) - 完整 API 文档
- [FAQ](../docs/FAQ.md) - 常见问题解答

---

**最后更新**: 2026-01-12
