# Scenario-Based Examples

**适用人群**: 需要解决特定场景问题的开发者
**特点**: 完整的端到端解决方案

## 场景目录

| 场景 | 描述 | 目录 |
|------|------|------|
| Digital Signature | 数字签名和验证 | `../digital_signature/` |
| File Encryption | 文件加密解密 | `../file_encrypt/` |
| HTTPS Client | HTTPS 客户端各种用法 | `../https_client/` |
| HTTPS Server | HTTPS 服务器实现 | `../https_server/` |
| Password Hash | 密码哈希和验证 | `../password_hash/` |
| HMAC Tool | HMAC 消息认证 | `../hmac_tool/` |
| Production | 生产环境配置 | `../production/` |

## 场景选择指南

### 我需要...

| 需求 | 推荐场景 |
|------|----------|
| 发起 HTTPS 请求 | `https_client/` |
| 搭建 HTTPS 服务器 | `https_server/` |
| 加密文件 | `file_encrypt/` |
| 数字签名 | `digital_signature/` |
| 安全存储密码 | `password_hash/` |
| 消息认证 | `hmac_tool/` |
| 生产环境部署 | `production/` |

## 每个场景包含

- `README.md` - 场景说明和使用方法
- 完整的源代码示例
- 测试数据文件（如适用）
- 配置文件示例（如适用）
