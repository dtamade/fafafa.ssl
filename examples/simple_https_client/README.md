# Simple HTTPS Client Example

这是一个展示如何使用 fafafa.ssl 创建 HTTPS 客户端的简单示例。

## 功能展示

1. **Quick HTTPS GET** - 一行代码实现 HTTPS 请求
2. **Manual Connection** - 手动创建连接和发送请求
3. **Custom Context** - 使用自定义 SSL 上下文
4. **Error Handling** - 错误处理示例

## 编译

### 使用 Free Pascal Compiler (FPC)

```bash
cd examples/simple_https_client
fpc -Fu../../src simple_https_client.lpr
```

### 使用 Lazarus

1. 打开 `simple_https_client.lpi`
2. Run → Build (Ctrl+F9)
3. Run → Run (F9)

## 运行

```bash
./simple_https_client
```

## 示例输出

```
================================================================================
              fafafa.ssl - Simple HTTPS Client Example
================================================================================

[Example 1] Quick HTTPS GET Request
------------------------------------
✅ Success!
Response length: 1256 bytes

First 200 characters:
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    <meta charset="utf-8" />
    <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
...

Press ENTER to continue to next example...

[Example 2] Manual HTTPS Connection
------------------------------------
Creating connection to www.google.com:443...
✅ Connected successfully
Using backend: OpenSSL 3.1.0

Sending HTTP request...
Reading response...
✅ Response received
Response length: 15234 bytes

Response headers:
  HTTP/1.1 200 OK
  Date: Thu, 24 Oct 2024 12:00:00 GMT
  Server: gws
  Content-Type: text/html; charset=ISO-8859-1
  ...

Press ENTER to continue to next example...

[Example 3] HTTPS Connection with Custom Context
-------------------------------------------------
Using SSL library: OpenSSL
Version: OpenSSL 3.1.0

Configuring SSL context...
✅ Context configured

Connecting to httpbin.org:443...
✅ Connected and SSL handshake completed

Sending request to /get endpoint...
✅ Response received
Response length: 438 bytes

JSON Response:
{
  "args": {},
  "headers": {
    "Accept": "application/json",
    "Host": "httpbin.org",
    "User-Agent": "fafafa.ssl/1.0"
  },
  "origin": "1.2.3.4",
  "url": "https://httpbin.org/get"
}
...

[Example 4] Error Handling
--------------------------
Attempting to connect to invalid hostname...
✅ Exception caught as expected:
   ESSLError: Connection failed: Name or service not known

Attempting to connect to wrong port...
✅ Exception caught as expected:
   ESSLError: Connection timed out

================================================================================
All examples completed! Press ENTER to exit...
```

## 学习要点

### 1. 最简单的方式

```pascal
var LResponse := TSSLFactory.QuickHTTPSGet('https://www.example.com');
```

这是最简单的方式，适合快速测试或简单场景。

### 2. 更多控制

```pascal
var LConn := TSSLFactory.QuickClientConnection('www.example.com', 443);
LConn.Write('GET / HTTP/1.1'#13#10...);
var LResponse := LConn.ReadAll;
```

提供更多控制，可以自定义请求内容。

### 3. 完全控制

```pascal
var LLib := TSSLFactory.CreateBest;
var LContext := LLib.CreateContext(sslRoleClient);
LContext.SetMinProtocolVersion(sslProtoTLS12);
var LConn := LContext.CreateConnection('www.example.com', 443);
```

完全控制 SSL/TLS 参数，适合企业应用。

## 相关示例

- `certificate_info/` - 证书信息查看
- `file_encryption/` - 文件加密
- `rest_api_client/` - REST API 客户端

## 故障排除

### 问题: OpenSSL 库未找到

确保 OpenSSL 已安装：

**Linux:**
```bash
sudo apt install libssl3  # Ubuntu/Debian
sudo yum install openssl-libs  # CentOS/RHEL
```

**macOS:**
```bash
brew install openssl@3
```

**Windows:**
下载 OpenSSL for Windows 或使用 WinSSL (自动)

### 问题: 证书验证失败

这通常是因为系统 CA 证书未正确配置。临时解决（仅用于测试）：

```pascal
LContext.SetVerifyMode(sslVerifyNone);  // 禁用验证（不安全！）
```

生产环境应使用：
```pascal
LContext.LoadCAFile('/path/to/ca-bundle.crt');
```

## 许可证

MIT License - 查看 LICENSE 文件

