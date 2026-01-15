# HTTPS 服务器示例套件

三个示例帮助你快速搭建 TLS 服务器并验证高级特性：

| 示例 | 文件 | 功能 |
|------|------|------|
| 1 | `https_server_simple.pas` | 最小 HTTPS 服务，返回 JSON |
| 2 | `https_server_mtls.pas` | 双向 TLS，输出客户端证书信息 |
| 3 | `https_server_alpn.pas` | 演示 ALPN 协商，显示选择的协议 |

## 准备证书

示例默认使用 PEM 格式证书，可通过以下方式生成：

```bash
# 方式1: 运行已有示例生成
fpc -Fu../../src -Fu../../src/openssl ../02_generate_certificate.pas
./02_generate_certificate

# 方式2: 脚本
./scripts/local_tls_server.sh
```

生成后将 `server.crt`/`server.key`（以及 mTLS 场景下的 `ca.pem`）放到当前目录即可。

## 编译与运行

```bash
cd examples/https_server
fpc -Fu../../src -Fu../../src/openssl https_server_simple.pas
./https_server_simple 8443 server.crt server.key
```

所有示例均使用同步阻塞方式处理连接，便于阅读理解；如需并发，请在外层添加线程或事件循环。

## 示例速览

### 1. 简单服务器
- 终端输出请求日志
- `GET /health` 返回 `{"status":"ok"}`
- 其他路径返回 `{"message":"Hello from fafafa.ssl"}`

### 2. mTLS 服务器
- 启用 `sslVerifyPeer` + `sslVerifyFailIfNoPeerCert`
- 请求报文中携带客户端证书时，返回其 Subject / Issuer
- 未携带或验证失败将导致握手终止

### 3. ALPN 服务器
- 通过 `Context.SetALPNProtocols('h2,http/1.1')` 设置候选列表
- 响应体中包含 `selected_protocol`
- 可与 `curl --http2`、`nghttp` 等配合测试

> 这些示例专注 TLS 层与 fafafa.ssl API，如需生产级日志、配置、路由，请继续参考 `examples/production/https_server_simple.pas`。
