# fafafa.ssl 示例程序

本目录包含 fafafa.ssl 的示例程序，覆盖 TLS 连接、证书、加密工具，以及更完整的 HTTPS 客户端/服务器场景。

## 🚀 快速开始（Linux/macOS）

```bash
# 编译一个示例到 examples/bin
mkdir -p examples/bin
fpc -Fu./src -Fu./src/openssl -Fu./examples -Fe./examples/bin examples/hello_ssl.pas

# 运行
./examples/bin/hello_ssl
```

Windows（PowerShell/CMD）可参考同等参数：

```text
fpc -Fu.\src -Fu.\src\openssl -Fu.\examples -Fe.\examples\bin examples\hello_ssl.pas
```

## 🧰 批量编译（推荐）

仓库自带脚本会按目录编译一批典型示例：

```bash
chmod +x examples/compile_test.sh
./examples/compile_test.sh
```

说明：部分示例需要网络（例如连接真实站点）或需要你准备本地证书文件。

## 📌 推荐从这些开始

### 入门

- `examples/hello_ssl.pas`：验证 OpenSSL 环境/版本
- `examples/01_tls_client.pas`：TLS 客户端（需要网络）
- `examples/https_client/https_client_simple.pas`：最小 HTTPS GET
- `examples/02_generate_certificate.pas`：生成自签名证书（测试用途）
- `examples/03_file_encryption.pas`：AES-256-GCM 文件加/解密

### HTTPS 客户端套件

- `examples/https_client/https_client_simple.pas`
- `examples/https_client/https_client_post.pas`
- `examples/https_client/https_client_auth.pas`
- `examples/https_client/https_client_session.pas`

### HTTPS 服务器套件

- `examples/https_server/https_server_simple.pas`
- `examples/https_server/https_server_mtls.pas`
- `examples/https_server/https_server_alpn.pas`

### 其他常用示例

- `examples/04_https_rest_client.pas`：REST 客户端示例
- `examples/05_https_server.pas`：简单 HTTPS 服务器
- `examples/06_digital_signature.pas`：数字签名与验证
- `examples/07_certificate_chain.pas`：证书链/信任验证
- `examples/08_mutual_tls.pas`：双向 TLS（mTLS）
- `examples/09_winssl_fips.pas`：WinSSL FIPS（仅 Windows）
- `examples/10_cert_renewal.pas`：证书续期/轮换示例

## 📖 相关文档

- `docs/QUICKSTART.md`：快速开始
- `docs/GETTING_STARTED.md`：入门（推荐入口）
- `docs/API_REFERENCE.md`：完整 API 参考
- `docs/TROUBLESHOOTING.md`：常见问题排查

---

**最后更新**: 2026-01-16
