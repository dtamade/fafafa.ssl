# HTTPS 客户端示例套件

本目录包含 4 个相互独立的小程序，覆盖最常见的 HTTPS 客户端场景：

| 示例 | 文件 | 说明 |
|------|------|------|
| 1 | `https_client_simple.pas` | 最小化 GET 请求，3 行代码即可跑通 |
| 2 | `https_client_post.pas` | 发送 JSON/Form POST，打印响应头/体 |
| 3 | `https_client_auth.pas` | 双向 TLS / 客户端证书认证示例 |
| 4 | `https_client_session.pas` | 演示 TLS 会话复用与性能对比 |

## 快速开始

```bash
cd examples/https_client
fpc -Mobjfpc -Fu../../src -Fu../../src/openssl -Fu../../examples https_client_simple.pas
./https_client_simple https://httpbin.org/get
```

如需一次编译全部示例，可运行仓库根目录的 `examples/compile_test.sh`。

## 示例详情

### 1. https_client_simple
- 默认访问 `https://httpbin.org/get`
- CLI: `./https_client_simple [URL]`
- 立即打印响应长度与前 200 个字符

### 2. https_client_post
- 默认向 `https://httpbin.org/post` 发送 JSON
- CLI: `./https_client_post [URL] [BODY] [CONTENT-TYPE]`
- 自动附带 `X-Demo-Client` 头，输出所有响应头

### 3. https_client_auth
- CLI: `./https_client_auth <URL> <client_cert.pem> <client_key.pem> [ca_bundle.pem]`
- 适合与 `scripts/local_tls_server.sh` 或 `examples/production/https_server_simple.pas` 联合使用
- 启用 `VerifyPeer`，缺少证书会直接退出

### 4. https_client_session
- CLI: `./https_client_session [URL] [count]`
- 对比「每次新建上下文」与「共享上下文 + 会话复用」的平均耗时
- 默认关闭证书验证，运行在生产环境前请加载系统 CA 并启用校验

> 📝 这些示例使用 `TSSLContextBuilder` + `TSSLConnector/TSSLStream`（以及 `fafafa.examples.tcp` 的 socket 辅助）来演示当前推荐用法。如果需要更完整的日志/重试/配置，请参见 `examples/production/` 中的生产级示例。
