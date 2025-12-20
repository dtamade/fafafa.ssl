# fafafa.ssl 文件组织架构设计

## 设计理念

fafafa.ssl 是一个分层设计的 SSL/TLS 抽象框架，文件组织遵循**职责分离**和**模块化**原则，确保：

1. **清晰的层次结构** - 抽象层与实现层明确分离
2. **最小化耦合** - 各模块独立，便于维护和扩展
3. **一致性规范** - 所有后端遵循统一的文件命名和组织规范
4. **可扩展性** - 新增后端或功能模块只需遵循现有规范

---

## 文件组织结构

### 第一层：抽象层（框架核心）

```
fafafa.ssl.pas                    // SSL/TLS 门面（Facade），提供统一的入口
fafafa.ssl.types.pas              // 抽象层的通用类型定义
fafafa.ssl.base.pas               // 抽象层的基类和接口定义
fafafa.ssl.factory.pas            // 工厂模式实现，用于创建具体后端实例
fafafa.ssl.certchain.pas          // 证书链管理（跨后端）
fafafa.ssl.log.pas                // 日志系统（跨后端）
fafafa.ssl.ringbuffer.pas         // 环形缓冲区（辅助工具）
```

**职责说明：**
- `fafafa.ssl.pas` - 用户的主要入口点，提供便捷的 API
- `fafafa.ssl.types.pas` - 定义所有后端共享的抽象类型（如 `TSSLVersion`）
- `fafafa.ssl.base.pas` - 定义接口 `IfafaSSLContext` 和抽象基类
- `fafafa.ssl.factory.pas` - 根据平台/配置自动选择合适的后端

---

### 第二层：后端实现层

每个后端遵循统一的命名规范：

```
fafafa.ssl.<backend>.pas              // 后端实现主文件（实现接口、对象、快捷方法）
fafafa.ssl.<backend>.types.pas        // 后端特定的类型定义
fafafa.ssl.<backend>.api.pas          // 后端 API 基础绑定（通用函数）
fafafa.ssl.<backend>.api.<module>.pas // 后端 API 模块拆分（按功能分类）
```

---

### 第二层-A：OpenSSL 后端

#### 核心文件

```
fafafa.ssl.openssl.pas            // OpenSSL 实现主文件
  ├─ 实现 IfafaSSLContext 接口
  ├─ TfafaSSLOpenSSLContext 类
  └─ 便捷方法和辅助函数

fafafa.ssl.openssl.types.pas      // OpenSSL 特定类型
  ├─ SSL_CTX, SSL, BIO 等指针类型
  ├─ 回调函数类型定义
  └─ OpenSSL 常量定义

fafafa.ssl.openssl.api.pas        // OpenSSL 基础 API 绑定
  ├─ SSL_library_init
  ├─ SSL_CTX_new / SSL_new
  ├─ SSL_connect / SSL_accept
  └─ SSL_read / SSL_write
```

#### API 模块拆分文件

按 OpenSSL 功能模块拆分，遵循 `fafafa.ssl.openssl.api.<module>.pas` 命名规范：

```
fafafa.ssl.openssl.api.aead.pas       // AEAD 加密 (GCM, CCM, etc.)
fafafa.ssl.openssl.api.aes.pas        // AES 加密算法
fafafa.ssl.openssl.api.aria.pas       // ARIA 加密算法
fafafa.ssl.openssl.api.asn1.pas       // ASN.1 编解码
fafafa.ssl.openssl.api.async.pas      // 异步操作支持
fafafa.ssl.openssl.api.bio.pas        // BIO（基本 I/O 抽象）
fafafa.ssl.openssl.api.blake2.pas     // BLAKE2 哈希算法
fafafa.ssl.openssl.api.bn.pas         // 大数运算（Big Number）
fafafa.ssl.openssl.api.buffer.pas     // 内存缓冲区管理
fafafa.ssl.openssl.api.chacha.pas     // ChaCha 加密算法
fafafa.ssl.openssl.api.cmac.pas       // CMAC 消息认证码
fafafa.ssl.openssl.api.cms.pas        // CMS 加密消息语法
fafafa.ssl.openssl.api.comp.pas       // 压缩支持
fafafa.ssl.openssl.api.conf.pas       // 配置文件解析
fafafa.ssl.openssl.api.crypto.pas     // 通用加密函数
fafafa.ssl.openssl.api.ct.pas         // 证书透明度
fafafa.ssl.openssl.api.des.pas        // DES 加密算法
fafafa.ssl.openssl.api.dh.pas         // Diffie-Hellman 密钥交换
fafafa.ssl.openssl.api.dsa.pas        // DSA 数字签名
fafafa.ssl.openssl.api.dso.pas        // 动态共享对象
fafafa.ssl.openssl.api.ec.pas         // 椭圆曲线加密
fafafa.ssl.openssl.api.ecdh.pas       // ECDH 密钥交换
fafafa.ssl.openssl.api.ecdsa.pas      // ECDSA 数字签名
fafafa.ssl.openssl.api.engine.pas     // 加密引擎
fafafa.ssl.openssl.api.err.pas        // 错误处理
fafafa.ssl.openssl.api.evp.pas        // EVP 高级加密接口
fafafa.ssl.openssl.api.hmac.pas       // HMAC 消息认证码
fafafa.ssl.openssl.api.kdf.pas        // 密钥派生函数
fafafa.ssl.openssl.api.md.pas         // 消息摘要算法
fafafa.ssl.openssl.api.modes.pas      // 加密模式
fafafa.ssl.openssl.api.obj.pas        // 对象标识符
fafafa.ssl.openssl.api.ocsp.pas       // OCSP 在线证书状态协议
fafafa.ssl.openssl.api.pem.pas        // PEM 文件格式
fafafa.ssl.openssl.api.pkcs.pas       // PKCS 标准
fafafa.ssl.openssl.api.pkcs7.pas      // PKCS#7 加密消息
fafafa.ssl.openssl.api.pkcs12.pas     // PKCS#12 个人信息交换
fafafa.ssl.openssl.api.rand.pas       // 随机数生成
fafafa.ssl.openssl.api.rsa.pas        // RSA 公钥加密
fafafa.ssl.openssl.api.sha.pas        // SHA 哈希算法
fafafa.ssl.openssl.api.ssl.pas        // SSL/TLS 核心协议
fafafa.ssl.openssl.api.stack.pas      // 栈数据结构
fafafa.ssl.openssl.api.x509.pas       // X.509 证书处理
fafafa.ssl.openssl.api.x509v3.pas     // X.509v3 扩展
```

---

### 第二层-B：WinSSL 后端（Windows Schannel）

#### 核心文件

```
fafafa.ssl.winssl.pas             // WinSSL 实现主文件
  ├─ 实现 IfafaSSLContext 接口
  ├─ TfafaSSLWinSSLContext 类
  └─ 便捷方法和辅助函数

fafafa.ssl.winssl.types.pas       // WinSSL 特定类型
  ├─ CredHandle, CtxtHandle
  ├─ SecBuffer, SecBufferDesc
  ├─ SCHANNEL_CRED 结构
  └─ Windows 证书类型

fafafa.ssl.winssl.api.pas         // WinSSL 基础 API 绑定
  ├─ AcquireCredentialsHandleW
  ├─ InitializeSecurityContextW
  ├─ AcceptSecurityContext
  ├─ EncryptMessage / DecryptMessage
  ├─ CertOpenStore / CertCloseStore
  └─ 证书验证相关函数
```

#### API 模块拆分文件（可选，按需扩展）

```
fafafa.ssl.winssl.api.cert.pas    // 证书操作 API
  ├─ CertFindCertificateInStore
  ├─ CertEnumCertificatesInStore
  ├─ CertGetNameStringW
  └─ 证书链验证

fafafa.ssl.winssl.api.cred.pas    // 凭据管理 API
  ├─ AcquireCredentialsHandleW
  ├─ FreeCredentialsHandle
  └─ QueryCredentialsAttributesW

fafafa.ssl.winssl.api.context.pas // 安全上下文 API
  ├─ InitializeSecurityContextW
  ├─ AcceptSecurityContext
  ├─ DeleteSecurityContext
  └─ QueryContextAttributesW

fafafa.ssl.winssl.api.crypto.pas  // 加密解密 API
  ├─ EncryptMessage
  ├─ DecryptMessage
  └─ ApplyControlToken
```

**注意：** WinSSL API 相对集中，初期可将所有 API 放在 `fafafa.ssl.winssl.api.pas` 中，后续根据需要拆分。

---

### 第二层-C：MbedTLS 后端（规划中）

```
fafafa.ssl.mbedtls.pas            // MbedTLS 实现主文件
fafafa.ssl.mbedtls.types.pas      // MbedTLS 特定类型
fafafa.ssl.mbedtls.api.pas        // MbedTLS 基础 API 绑定
fafafa.ssl.mbedtls.api.ssl.pas    // SSL/TLS API
fafafa.ssl.mbedtls.api.x509.pas   // X.509 证书 API
fafafa.ssl.mbedtls.api.cipher.pas // 加密算法 API
...
```

---

### 第二层-D：WolfSSL 后端（规划中）

```
fafafa.ssl.wolfssl.pas            // WolfSSL 实现主文件
fafafa.ssl.wolfssl.types.pas      // WolfSSL 特定类型
fafafa.ssl.wolfssl.api.pas        // WolfSSL 基础 API 绑定
fafafa.ssl.wolfssl.api.ssl.pas    // SSL/TLS API
...
```

---

## 命名规范

### 1. 文件命名规范

- **抽象层文件：** `fafafa.ssl.<module>.pas`
- **后端主文件：** `fafafa.ssl.<backend>.pas`
- **后端类型文件：** `fafafa.ssl.<backend>.types.pas`
- **后端 API 基础：** `fafafa.ssl.<backend>.api.pas`
- **后端 API 模块：** `fafafa.ssl.<backend>.api.<module>.pas`

### 2. Unit 命名规范

Unit 名称与文件名保持一致（去掉 `.pas` 后缀）：

```pascal
unit fafafa.ssl.openssl.api.aes;  // 文件: fafafa.ssl.openssl.api.aes.pas
```

### 3. 类型命名规范

- **接口：** `Ifafa<Interface>` (例: `IfafaSSLContext`)
- **类：** `Tfafa<Class><Backend>` (例: `TfafaSSLOpenSSLContext`)
- **后端特定类型：** 遵循原生 API 命名（例: `SSL_CTX`, `CredHandle`）

---

## 依赖关系

```
抽象层
  └─ fafafa.ssl.pas (门面)
      ├─ depends on: fafafa.ssl.base
      ├─ depends on: fafafa.ssl.factory
      └─ depends on: fafafa.ssl.types

后端层 (OpenSSL)
  └─ fafafa.ssl.openssl.pas
      ├─ implements: IfafaSSLContext (from fafafa.ssl.base)
      ├─ depends on: fafafa.ssl.openssl.types
      └─ depends on: fafafa.ssl.openssl.api.*

后端层 (WinSSL)
  └─ fafafa.ssl.winssl.pas
      ├─ implements: IfafaSSLContext (from fafafa.ssl.base)
      ├─ depends on: fafafa.ssl.winssl.types
      └─ depends on: fafafa.ssl.winssl.api
```

**核心原则：**
- 抽象层 **不依赖** 任何后端实现
- 后端实现 **仅依赖** 抽象层接口
- API 绑定层 **无依赖**（或仅依赖 types）

---

## 使用示例

### 用户代码（使用门面）

```pascal
uses
  fafafa.ssl;  // 只需引用门面

var
  SSLCtx: IfafaSSLContext;
begin
  // 工厂自动选择后端（Windows 上优先 WinSSL）
  SSLCtx := CreateSSLContext;
  SSLCtx.Connect('example.com', 443);
  // ...
end;
```

### 直接使用特定后端

```pascal
uses
  fafafa.ssl.openssl;  // 或 fafafa.ssl.winssl

var
  SSLCtx: TfafaSSLOpenSSLContext;
begin
  SSLCtx := TfafaSSLOpenSSLContext.Create;
  // ...
end;
```

### 使用底层 API（高级用户）

```pascal
uses
  fafafa.ssl.openssl.api.aes,
  fafafa.ssl.winssl.api;

// 直接调用 OpenSSL AES 函数
// 或直接调用 Windows Schannel 函数
```

---

## 重构计划

### 阶段 1：文件重命名和移动

1. 提交当前代码到 Git（保留历史）
2. 按照新规范重命名文件
3. 更新所有 `unit` 声明和 `uses` 引用
4. 编译测试

### 阶段 2：消除重复定义

1. 整合重复的类型定义到统一文件
2. 确保每个类型只在一个地方定义
3. 更新所有引用

### 阶段 3：完善 API 模块拆分

1. 将 OpenSSL 大文件拆分为模块
2. 将 WinSSL API 按需拆分（可选）
3. 确保模块化清晰

### 阶段 4：文档和测试

1. 为每个模块添加详细注释
2. 编写单元测试
3. 更新示例代码

---

## 优势总结

✅ **清晰的层次结构** - 抽象层与实现层分离，便于理解和维护  
✅ **模块化设计** - 每个文件职责单一，易于定位和修改  
✅ **一致性规范** - 所有后端遵循统一命名，降低学习成本  
✅ **可扩展性** - 新增后端只需复制规范，填充实现  
✅ **零依赖部署** - 抽象层可独立编译，后端按需链接  
✅ **跨平台友好** - Windows 可用 WinSSL，Linux/macOS 用 OpenSSL  

---

## 版本历史

- **v1.0** (2025-10-04): 初始架构设计
- 未来版本将支持 MbedTLS、WolfSSL、BoringSSL 等更多后端

---

**文档维护者：** fafafa.ssl 开发团队  
**最后更新：** 2025-10-04
