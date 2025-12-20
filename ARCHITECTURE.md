# 🏗️ fafafa.ssl 架构设计

**核心理念**：专注SSL/TLS，暴露Socket，不封装应用层协议

---

## 设计哲学

### ✅ fafafa.ssl 的职责

```
fafafa.ssl = SSL/TLS加密层 + Socket暴露
```

1. **SSL/TLS加密**
   - OpenSSL后端绑定（200+ API）
   - WinSSL后端实现（Windows原生，零依赖）
   - 统一的抽象接口

2. **证书管理**
   - 加载证书（文件/流/对象）
   - 验证证书链
   - 管理证书存储

3. **密码学工具**
   - 哈希函数（MD5/SHA256/SHA512等）
   - 编码/解码（Base64/Hex）
   - 对称/非对称加密

4. **Socket接口**
   - **接收**用户创建的socket
   - **不创建**也不管理socket（这是网络库的职责）
   - 用户可用任何方式创建socket（系统API、Synapse、Indy等）

---

## ❌ fafafa.ssl 不做什么

### 1. 不创建/管理Socket

**为什么？**
- Socket管理是网络库的职责，不是SSL/TLS库的职责
- FreePascal生态已有成熟网络库（Synapse、Indy、lNet）
- 用户可能需要特定的socket配置（超时、缓冲区等）
- 遵循业界最佳实践（OpenSSL、mbedTLS等都不管理socket）

**用户应该如何创建socket？**
- 使用系统API（WinSock2、BSD Socket）
- 使用网络库（Synapse、Indy、lNet等）
- 然后传入`ISSLContext.CreateConnection(aSocket: THandle)`

### 2. 不封装应用层协议

**为什么？**
- 这不是SSL/TLS库的职责
- 避免重复造轮子
- 给用户最大灵活性

**用户应该如何实现协议？**
- 自己构造协议数据
- 通过`ISSLConnection.Write()`发送
- 通过`ISSLConnection.Read()`接收
- 可以实现：HTTP、SMTP、FTP、WebSocket、自定义协议等

---

## 架构层次

```
┌─────────────────────────────────────────────────┐
│      应用层协议（用户实现）                       │
│    HTTP / SMTP / FTP / WebSocket / 自定义...    │
├─────────────────────────────────────────────────┤
│      fafafa.ssl（核心）                         │
│    ISSLConnection.Read/Write（加密传输）        │
│    SSL/TLS 握手 / 证书验证 / 密码学             │
├─────────────────────────────────────────────────┤
│      后端实现                                   │
│    TOpenSSLLibrary / TWinSSLLibrary             │
├─────────────────────────────────────────────────┤
│      Socket层（用户负责）                       │
│    系统API / Synapse / Indy / lNet ...          │
└─────────────────────────────────────────────────┘

重要：fafafa.ssl 只负责中间的加密层！
      Socket由用户创建，协议由用户实现。
```

---

## 典型用法

### 示例1：完整的SSL/TLS连接流程

```pascal
uses
  SysUtils,
  {$IFDEF WINDOWS}WinSock2{$ELSE}Sockets, BaseUnix{$ENDIF},
  fafafa.ssl.factory, fafafa.ssl.abstract.types, fafafa.ssl.abstract.intf;

procedure SSLConnection;
var
  LContext: ISSLContext;
  LConnection: ISSLConnection;
  LSocket: THandle;
  LRequest: string;
  LBuffer: array[0..4095] of Byte;
  LBytesRead: Integer;
begin
  // 1. 创建SSL上下文
  LContext := TSSLFactory.CreateContext(sslCtxClient);
  LContext.SetVerifyMode([sslVerifyPeer]);
  
  // 2. 用户自己创建socket（这里用系统API，也可以用Synapse等）
  {$IFDEF WINDOWS}
  LSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  // ... 连接到服务器 ...
  {$ELSE}
  LSocket := fpSocket(AF_INET, SOCK_STREAM, 0);
  // ... 连接到服务器 ...
  {$ENDIF}
  
  // 3. 将socket传入SSL库（SSL库不创建socket）
  LConnection := LContext.CreateConnection(LSocket);
  LConnection.SetHostname('example.com');
  
  // 4. SSL握手
  if not LConnection.Connect then
    raise Exception.Create('SSL握手失败');
  
  // 5. 发送数据（用户自己构造协议）
  // 这里演示HTTP，但你可以实现任何协议
  LRequest := 
    'GET / HTTP/1.1'#13#10 +
    'Host: example.com'#13#10 +
    'Connection: close'#13#10#13#10;
  LConnection.Write(@LRequest[1], Length(LRequest));
  
  // 6. 读取响应（用户自己解析）
  repeat
    LBytesRead := LConnection.Read(@LBuffer[0], SizeOf(LBuffer));
    if LBytesRead > 0 then
      Write(string(PAnsiChar(@LBuffer[0])));
  until LBytesRead <= 0;
end;
```

**关键点**：
- ✅ 用户创建socket（系统API或网络库）
- ✅ fafafa.ssl接收socket并提供SSL/TLS加密
- ✅ 用户构造应用层协议（HTTP/SMTP/FTP等）
- ✅ 用户解析应用层响应
- ❌ fafafa.ssl **不**创建socket，**不**实现HTTP等协议

---

### 示例2：使用OpenSSL后端

```pascal
var
  LContext: ISSLContext;
begin
  // 指定使用OpenSSL后端
  TSSLFactory.SetDefaultLibrary(sslOpenSSL);
  LContext := TSSLFactory.CreateContext(sslClient);
  // ... 其他代码相同
end;
```

---

### 示例3：Windows零依赖（WinSSL）

```pascal
var
  LContext: ISSLContext;
begin
  // Windows上默认就是WinSSL，无需任何DLL
  LContext := TSSLFactory.CreateContext(sslClient);
  // ... 其他代码相同
end;
```

---

## 设计优势

### 1. 职责单一

- SSL/TLS库只做加密相关的事
- 应用层协议由用户或专门的库处理
- 代码更稳定、更易维护

### 2. 最大灵活性

- 用户可以实现任何协议
- 不被绑定到特定的HTTP客户端实现
- 可以优化特定场景

### 3. 避免重复造轮子

- HTTP有fpHTTPClient、Synapse、Indy等成熟实现
- SMTP有专门的邮件库
- FTP有专门的FTP库
- 让专业的库做专业的事

### 4. 更好的分层

```
用户代码
   ↓
应用层协议库（HTTP/SMTP/...）
   ↓
fafafa.ssl（SSL/TLS + Socket）
   ↓
OS（WinSock/Unix Socket）
```

---

## 与其他库的集成

### 集成 Synapse HTTP

```pascal
uses
  fafafa.ssl.openssl,
  httpsend,
  ssl_openssl;

var
  HTTP: THTTPSend;
begin
  HTTP := THTTPSend.Create;
  try
    // Synapse自动使用OpenSSL
    HTTP.HTTPMethod('GET', 'https://example.com');
    WriteLn(HTTP.Document.DataString);
  finally
    HTTP.Free;
  end;
end;
```

### 集成 fpHTTPClient

```pascal
uses
  fafafa.ssl.openssl,
  fphttpclient;

var
  Client: TFPHTTPClient;
begin
  Client := TFPHTTPClient.Create(nil);
  try
    WriteLn(Client.Get('https://example.com'));
  finally
    Client.Free;
  end;
end;
```

---

## 总结

**fafafa.ssl不是HTTP库，是SSL/TLS库**

它提供：
- ✅ 完整的SSL/TLS加密功能
- ✅ 跨平台的Socket工具
- ✅ 零依赖的Windows支持（WinSSL）
- ✅ 灵活的后端选择（OpenSSL/WinSSL）

它不提供：
- ❌ HTTP客户端（用户自己实现或用其他库）
- ❌ 其他应用层协议（用户自己实现）

这是**深思熟虑的设计决策**，遵循Unix哲学："做好一件事"。

---

**设计者**: fafafa.ssl 开发团队  
**日期**: 2025-11-02  
**状态**: ✅ 已确认并实施

