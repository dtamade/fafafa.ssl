# fafafa.ssl 30秒示例索引

> **目标**：让开发者在 30 秒内找到并运行第一个示例

## 最快入门路径

### 1. 验证环境（5秒）

```bash
# 编译并运行 hello_ssl
fpc -Mobjfpc -Sh -Fu./src examples/hello_ssl.pas && ./hello_ssl
```

预期输出：
```
[Step 1] Loading OpenSSL library...
         SUCCESS
[Step 2] Get version info...
         Version: OpenSSL 3.x.x ...
```

### 2. 第一个 HTTPS 请求（10秒）

```bash
# 编译并运行 https_simple_get
fpc -Mobjfpc -Sh -Fu./src examples/https_simple_get.pas && ./https_simple_get
```

预期输出：
```
步骤 4: 执行TLS握手...
  ✓ 握手成功
    协议: TLS 1.3
    密码套件: TLS_AES_256_GCM_SHA384
```

### 3. 密码学工具（10秒）

```pascal
// 最小 SHA256 示例
uses fafafa.ssl.crypto.utils;
begin
  WriteLn(TCryptoUtils.SHA256Hex('Hello'));  // 输出: 185f8db32271fe25f561a6fc938b2e26...
end.
```

## 示例分类索引

### 入门级（⭐）

| 示例 | 功能 | 编译命令 |
|------|------|----------|
| `hello_ssl.pas` | 验证 OpenSSL 加载 | `fpc -Fu./src examples/hello_ssl.pas` |
| `https_simple_get.pas` | 最简 HTTPS GET | `fpc -Fu./src examples/https_simple_get.pas` |
| `hash_calculator.pas` | SHA256/MD5 计算 | `fpc -Fu./src examples/hash_calculator.pas` |

### 基础级（⭐⭐）

| 示例 | 功能 | 说明 |
|------|------|------|
| `01_tls_client.pas` | TLS 客户端完整流程 | 含证书验证 |
| `02_generate_certificate.pas` | 自签名证书生成 | 含私钥生成 |
| `03_file_encryption.pas` | 文件加密解密 | AES-256-GCM |
| `04_https_rest_client.pas` | REST API 客户端 | JSON 请求/响应 |

### 进阶级（⭐⭐⭐）

| 示例 | 功能 | 说明 |
|------|------|------|
| `05_https_server.pas` | HTTPS 服务器 | 含证书配置 |
| `06_digital_signature.pas` | 数字签名 | RSA/ECDSA |
| `07_certificate_chain.pas` | 证书链验证 | 含中间证书 |
| `08_mutual_tls.pas` | 双向 TLS | 客户端证书 |

### 生产级（⭐⭐⭐⭐）

| 示例 | 功能 | 说明 |
|------|------|------|
| `production/https_client_simple.pas` | 生产级客户端 | 含错误处理 |
| `production/https_server_simple.pas` | 生产级服务器 | 含日志 |
| `session_resumption_example.pas` | Session 复用 | 性能优化 |
| `example_cert_pinning.pas` | 证书固定 | 安全增强 |

## 按场景选择

### 我想做 HTTPS 客户端

```
入门: https_simple_get.pas
进阶: 04_https_rest_client.pas
生产: production/https_client_simple.pas
```

### 我想做 HTTPS 服务器

```
入门: 05_https_server.pas
进阶: https_server/https_server_simple.pas
生产: production/https_server_simple.pas
```

### 我想做加密/签名

```
哈希: hash_calculator.pas
加密: 03_file_encryption.pas, example_aes_gcm_aead.pas
签名: 06_digital_signature.pas, pkcs7_sign_verify_simple.pas
```

### 我想处理证书

```
生成: 02_generate_certificate.pas
验证: certificate_verification_example.pas
查看: cert_info_viewer.pas
```

## 编译提示

### Linux/macOS

```bash
# 单文件编译
fpc -Mobjfpc -Sh -Fu./src examples/YOUR_EXAMPLE.pas

# 批量编译所有示例
for f in examples/*.pas; do fpc -Mobjfpc -Sh -Fu./src "$f"; done
```

### Windows

```powershell
# 单文件编译
fpc -Mobjfpc -Sh -Fu.\src examples\YOUR_EXAMPLE.pas

# 使用 WinSSL 后端（无需 OpenSSL DLL）
# 示例中使用 .WithBackend(sslWinSSL) 即可
```

## 常见问题

### Q: OpenSSL 加载失败？

```bash
# Linux: 安装 OpenSSL
sudo apt install libssl3 libssl-dev  # Debian/Ubuntu
sudo dnf install openssl openssl-devel  # Fedora/RHEL

# macOS: 使用 Homebrew
brew install openssl@3
```

### Q: 编译找不到单元？

```bash
# 确保 -Fu./src 参数正确
fpc -Mobjfpc -Sh -Fu./src -Fi./src examples/YOUR_EXAMPLE.pas
```

### Q: Windows 上不想装 OpenSSL？

使用 WinSSL 后端，无需额外 DLL：
```pascal
Ctx := TSSLContextBuilder.Create
  .WithBackend(sslWinSSL)  // 使用 Windows 原生 TLS
  .BuildClient;
```

## 下一步

- 详细文档：`docs/guides/QUICKSTART.md`
- API 参考：`docs/reference/`
- 测试用例：`tests/` 目录包含更多使用示例

## Local-first 守护（CI 暂缓场景）

若当前处于 CI 暂缓模式，可直接运行：

```bash
# 本地守护一键门禁（B125）
bash scripts/run_wave_c_local_first_guard_bundle.sh --strict

# 本地守护趋势汇总（B126）
bash scripts/summarize_wave_c_local_guard_history.sh --strict
```

出现异常时：
- 先看 `docs/test_reports/WAVE_C_B127_LOCAL_GUARD_TROUBLESHOOTING_2026-02-09.md`
- 再按手册顺序重跑 B123/B124/B125/B126
