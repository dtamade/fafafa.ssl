# fafafa.ssl API 清单

**生成日期**: 2026-01-31  
**版本**: Phase C Week 3-4 完成

## 核心接口 (Core Interfaces)

### ISSLContext
**文件**: `src/fafafa.ssl.base.pas`  
**实现**:
- `TOpenSSLContext` (src/fafafa.ssl.openssl.context.pas)
- `TWinSSLContext` (src/fafafa.ssl.winssl.context.pas)

**方法**:
- `LoadCertificate(const AFilePath: string)` - 加载证书文件
- `LoadCertificatePEM(const APEM: string)` - 从 PEM 字符串加载证书
- `LoadPrivateKey(const AFilePath: string; const APassword: string = '')` - 加载私钥
- `LoadPrivateKeyPEM(const APEM: string; const APassword: string = '')` - 从 PEM 字符串加载私钥
- `SetCipherList(const ACiphers: string)` - 设置密码套件列表
- `SetVerifyMode(AMode: TSSLVerifyMode)` - 设置验证模式
- `LoadSystemRootCertificates` - 加载系统根证书
- `CreateConnection(ASocket: THandle): ISSLConnection` - 创建 SSL 连接

### ISSLConnection
**文件**: `src/fafafa.ssl.base.pas`  
**实现**:
- `TOpenSSLConnection` (src/fafafa.ssl.openssl.connection.pas)
- `TWinSSLConnection` (src/fafafa.ssl.winssl.connection.pas)

**方法**:
- `Connect` - 建立 SSL 连接
- `Disconnect` - 断开连接
- `Read(var Buffer; Count: Integer): Integer` - 读取数据
- `Write(const Buffer; Count: Integer): Integer` - 写入数据
- `GetProtocolVersion: TSSLProtocolVersion` - 获取协议版本
- `GetCipherName: string` - 获取密码套件名称
- `IsSessionReused: Boolean` - 检查会话是否复用
- `GetPeerCertificate: ICertificate` - 获取对端证书

**缺失方法** (待实现):
- `GetOCSPStaplingEnabled: Boolean`
- `GetOCSPResponse: TBytes`
- `IsOCSPResponseVerified: Boolean`
- `GetOCSPResponseStatus: string`

### ICertificate
**文件**: `src/fafafa.ssl.base.pas`  
**实现**:
- `TOpenSSLCertificate` (src/fafafa.ssl.openssl.certificate.pas)
- `TWinSSLCertificate` (src/fafafa.ssl.winssl.certificate.pas)

**方法**:
- `LoadFromFile(const AFilePath: string)` - 从文件加载证书
- `LoadFromPEM(const APEM: string)` - 从 PEM 字符串加载
- `ToPEM: string` - 导出为 PEM 格式
- `GetSubject: string` - 获取主题
- `GetIssuer: string` - 获取颁发者
- `GetSerialNumber: string` - 获取序列号
- `GetNotBefore: TDateTime` - 获取生效时间
- `GetNotAfter: TDateTime` - 获取过期时间
- `Verify(AStore: ICertificateStore): Boolean` - 验证证书
- `VerifyHostname(const AHostname: string): Boolean` - 验证主机名

### ICertificateStore
**文件**: `src/fafafa.ssl.base.pas`  
**实现**:
- `TOpenSSLCertificateStore` (src/fafafa.ssl.openssl.certstore.pas)
- `TWinSSLCertificateStore` (src/fafafa.ssl.winssl.certstore.pas)

**方法**:
- `AddCertificate(ACert: ICertificate)` - 添加证书
- `GetCount: Integer` - 获取证书数量
- `GetCertificate(AIndex: Integer): ICertificate` - 获取指定索引的证书
- `LoadSystemRoots` - 加载系统根证书

### ISSLSession
**文件**: `src/fafafa.ssl.base.pas`  
**实现**:
- `TOpenSSLSession` (src/fafafa.ssl.openssl.session.pas)

**方法**:
- `GetSessionData: TBytes` - 获取会话数据
- `SetSessionData(const AData: TBytes)` - 设置会话数据
- `IsValid: Boolean` - 检查会话是否有效

## 高级 API (Advanced APIs)

### TSSLContextBuilder
**文件**: `src/fafafa.ssl.context.builder.pas`  
**用途**: 流式 API 构建 SSL 上下文

**方法**:
- `WithTLS12: ISSLContextBuilder` - 启用 TLS 1.2
- `WithTLS13: ISSLContextBuilder` - 启用 TLS 1.3
- `WithTLS12And13: ISSLContextBuilder` - 启用 TLS 1.2 和 1.3
- `WithModernDefaults: ISSLContextBuilder` - 使用现代默认配置
- `WithVerifyPeer: ISSLContextBuilder` - 启用对端验证
- `WithVerifyNone: ISSLContextBuilder` - 禁用验证
- `WithSystemRoots: ISSLContextBuilder` - 加载系统根证书
- `WithCertificate(const APath: string): ISSLContextBuilder` - 加载证书
- `WithPrivateKey(const APath: string; const APassword: string = ''): ISSLContextBuilder` - 加载私钥
- `BuildClient: ISSLContext` - 构建客户端上下文
- `BuildServer: ISSLContext` - 构建服务器上下文

### TCertificateBuilder
**文件**: `src/fafafa.ssl.cert.builder.pas`  
**用途**: 流式 API 生成证书

**方法**:
- `WithCommonName(const ACN: string): ICertificateBuilder` - 设置通用名称
- `WithOrganization(const AOrg: string): ICertificateBuilder` - 设置组织
- `WithCountry(const ACountry: string): ICertificateBuilder` - 设置国家
- `ValidFor(ADays: Integer): ICertificateBuilder` - 设置有效期
- `WithRSAKey(ABits: Integer): ICertificateBuilder` - 使用 RSA 密钥
- `AsServerCert: ICertificateBuilder` - 标记为服务器证书
- `AsClientCert: ICertificateBuilder` - 标记为客户端证书
- `AddSubjectAltName(const ASAN: string): ICertificateBuilder` - 添加 SAN
- `SelfSigned: IKeyPairWithCertificate` - 生成自签名证书

### TCryptoUtils
**文件**: `src/fafafa.ssl.crypto.utils.pas`  
**用途**: 加密工具类

**方法**:
- `SHA256(const AData: TBytes): TBytes` - SHA-256 哈希
- `SHA256Hex(const AData: string): string` - SHA-256 哈希 (十六进制)
- `SHA512(const AData: TBytes): TBytes` - SHA-512 哈希
- `AES_GCM_Encrypt(const AData, AKey, AIV: TBytes): TBytes` - AES-256-GCM 加密
- `AES_GCM_Decrypt(const AData, AKey, AIV: TBytes): TBytes` - AES-256-GCM 解密
- `SecureRandom(ALength: Integer): TBytes` - 生成安全随机数
- `GenerateKey(ABits: Integer): TBytes` - 生成密钥
- `PBKDF2(const APassword, ASalt: TBytes; AIterations, AKeyLength: Integer): TBytes` - 密钥派生

### TSSLQuick
**文件**: `src/fafafa.ssl.quick.pas`  
**用途**: 快速 API

**方法**:
- `GenerateSelfSigned(const ACommonName: string): IKeyPairWithCertificate` - 快速生成自签名证书
- `GenerateCertFiles(const ACommonName, ACertPath, AKeyPath: string)` - 生成证书文件

## 测试覆盖 (Test Coverage)

### Phase C Week 3-4 新增测试

1. **test_concurrent_connections.pas** - 并发连接测试
   - 4 个并发客户端连接
   - 连接池管理
   - 并发读写操作
   - 资源清理验证

2. **test_security_attacks.pas** - 安全攻击场景测试
   - 协议降级攻击 (3 个测试)
   - 重放攻击 (2 个测试)
   - 中间人攻击 (3 个测试)
   - 证书钉扎绕过 (3 个测试)
   - 时序攻击 (1 个测试)
   - 填充预言攻击 (1 个测试)

3. **test_phase5_complete_handshake.pas** - 完整握手测试 (增强版)
   - 原有 8 个测试场景
   - 新增 5 个失败场景测试:
     - 证书/密钥不匹配检测
     - 无效证书拒绝
     - NULL 指针处理
     - 关闭连接操作
     - 协议版本协商

4. **test_cert_verify.pas** - 证书验证测试 (增强版)
   - 原有 2 个基础测试
   - 新增 6 个失败场景测试:
     - 加载不存在的证书文件
     - 加载无效证书数据
     - 空主机名验证
     - 空证书存储操作
     - 无效索引访问
     - 空存储证书链验证

### 测试统计

- **总测试数**: 235+ 个单元测试
- **集成测试**: 70+ 真实网站验证
- **E2E 场景**: 6 个场景, 83% 通过率
- **模糊测试**: 7 个目标
- **性能基准**: 10 项指标

## 性能指标 (Performance Metrics)

### 加密操作性能 (Phase B 基准测试)

| 操作 | 数据大小 | 吞吐量 (ops/s) | 平均延迟 (ms) |
|------|---------|---------------|--------------|
| SHA-256 | 64B | 1,000,000 | 0.001 |
| SHA-256 | 1KB | 200,000 | 0.005 |
| SHA-512 | 64B | 500,000 | 0.002 |
| AES-256-GCM 加密 | 64B | 90,909 | 0.011 |
| AES-256-GCM 解密 | 64B | 1,000,000 | 0.001 |
| 安全随机数 | 64B | 250,000 | 0.004 |

### TLS 握手性能

| 场景 | 平均延迟 (ms) | 吞吐量 (ops/s) |
|------|--------------|---------------|
| TLS 1.3 握手 | 2649.7 | 0.4 |
| TLS 1.2 握手 | 2842.9 | 0.4 |
| 会话复用 | 2672.3 | 0.4 |

## 部署指南 (Deployment Guide)

### 依赖要求

- **FreePascal**: 3.2.0+
- **OpenSSL**: 1.1.1+ 或 3.0+ (Linux/macOS)
- **WinSSL**: Windows 原生 (零依赖)

### 编译选项

```bash
fpc -B -Mobjfpc -Sh -Fu./src -Fi./src -FU./lib your_app.pas -o./bin/your_app
```

### 运行时配置

- **证书路径**: 使用 `WithSystemRoots` 自动加载系统证书
- **协议版本**: 推荐使用 `WithTLS12And13` 或 `WithModernDefaults`
- **验证模式**: 生产环境必须使用 `WithVerifyPeer`

## 已知限制 (Known Limitations)

1. **OCSP Stapling**: 接口已定义,实现待完成
2. **PKCS#11**: 基础框架已完成,完整实现待完成
3. **test_cert_verify.pas**: 编译失败 (依赖问题),需要修复 PKCS#11 相关依赖

## 下一步计划 (Next Steps)

1. 实现 OCSP Stapling 功能
2. 完成 PKCS#11 集成
3. 修复 test_cert_verify.pas 编译问题
4. 集成所有测试到 CI/CD
5. 生成最终覆盖率报告

---

**文档版本**: 1.0  
**最后更新**: 2026-01-31  
**维护者**: fafafa.ssl 开发团队
