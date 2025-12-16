# fafafa.ssl 项目架构设计

## 1. 项目概览

### 1.1 目标
创建一个统一的 SSL/TLS 抽象层，支持 OpenSSL、WolfSSL、MbedTLS、WinSSL(Schannel) 四种后端实现，为 Pascal 生态提供简单易用的 SSL/TLS 解决方案。

### 1.2 核心原则
- **统一接口**：屏蔽不同库的 API 差异
- **动态选择**：运行时选择后端实现
- **错误透明**：统一的错误处理机制
- **性能优先**：最小化抽象层开销
- **内存安全**：严格的资源管理

## 2. 架构层次设计

```
┌─────────────────────────────────────┐
│          用户应用层                    │
├─────────────────────────────────────┤
│      fafafa.ssl 统一接口层            │  ← 核心抽象层
├─────────────────────────────────────┤
│     后端适配器层 (Adapter Layer)      │
├─────────┬─────────┬─────────┬───────┤
│ OpenSSL │ WolfSSL │ MbedTLS │WinSSL │  ← 原生库封装
│ Wrapper │ Wrapper │ Wrapper │Wrapper│
└─────────┴─────────┴─────────┴───────┘
```

## 3. 核心模块设计

### 3.1 基础类型模块 (`fafafa.ssl.types`)
**职责**：定义所有通用数据类型、枚举、常量、异常类

**关键类型**：
- `TSSLLibraryType`: 后端库类型枚举
- `TSSLProtocolVersion`: 协议版本枚举
- `TSSLVerifyMode`: 证书验证模式
- `TSSLContextType`: 上下文类型（客户端/服务端）
- `ESSLException`: 统一异常类
- `TSSLCertificateInfo`: 证书信息结构
- `TSSLConnectionInfo`: 连接信息结构

### 3.2 核心接口模块 (`fafafa.ssl.intf`)
**职责**：定义所有抽象接口

**主要接口**：
```pascal
ISSLContext = interface
  // 上下文配置和管理
end;

ISSLConnection = interface  
  // SSL连接的建立、数据传输、状态查询
end;

ISSLCertificate = interface
  // 证书加载、验证、信息获取
end;

ISSLLibrary = interface
  // 库的初始化、清理、版本信息
end;
```

### 3.3 工厂管理模块 (`fafafa.ssl.factory`)
**职责**：
- 后端库的动态加载和选择
- 实例创建和生命周期管理
- 库可用性检测

**核心类**：
```pascal
TSSLFactory = class
  class function CreateContext(ALibType: TSSLLibraryType; 
    AContextType: TSSLContextType): ISSLContext;
  class function GetAvailableLibraries: TSSLLibraryTypeSet;
  class function IsLibraryAvailable(ALibType: TSSLLibraryType): Boolean;
end;
```

### 3.4 后端实现模块
- `fafafa.ssl.openssl` - OpenSSL 实现（Linux/macOS 默认）
- `fafafa.ssl.winssl` - Windows Schannel 实现（Windows 默认）
- `fafafa.ssl.wolfssl` / `fafafa.ssl.mbedtls` - 规划中的后端，目前尚未落地；如调用这些库类型，工厂会抛出 `ESSLException` 提醒“暂未实现”。

每个后端模块包含：
```pascal
TXXXSSLContext = class(TInterfacedObject, ISSLContext)
TXXXSSLConnection = class(TInterfacedObject, ISSLConnection)  
TXXXSSLCertificate = class(TInterfacedObject, ISSLCertificate)
TXXXSSLLibrary = class(TInterfacedObject, ISSLLibrary)
```

### 3.5 证书时间与扩展解析策略

为保证不同后端在证书语义上的一致性，时间字段和常用扩展（尤其是 `subjectAltName`）做了统一约定：

- **有效期时间（NotBefore/NotAfter）**  
  - OpenSSL 后端通过 `X509_get_notBefore/After` 取得 `PASN1_TIME`，再委托统一的 `ASN1TimeToDateTime` 工具函数完成解析，避免直接手写 `TM` 结构解析曾经引发的 AV 问题。  
  - WinSSL 后端使用 `FileTimeToSystemTime` → `SystemTimeToDateTime` 解析 `CERT_INFO.NotBefore/NotAfter`，两端最终都返回正常的 `TDateTime`，并在 `TSSLCertificateInfo.NotBefore/NotAfter` 中对齐语义。

- **subjectAltName（SAN）扩展**  
  - OpenSSL 简化后端（`fafafa.ssl.openssl.certificate`）优先通过 `X509_get_ext_d2i(..., NID_subject_alt_name, ...)` 解码为 `GENERAL_NAMES`，再枚举 `GENERAL_NAME` 条目：
  - `GEN_DNS`/`GEN_EMAIL`/`GEN_URI` → 使用 ASN1 字符串工具转换为纯文本域名、邮箱、URI；
  - `GEN_IPADD` → 按字节解析为 IPv4/IPv6 文本（如 `127.0.0.1`、`2001:db8::1`）。
  - 如果运行环境缺少相关 X509v3/stack API，则回退到 `X509V3_EXT_print` 的文本输出并做简单字符串解析（兼容旧实现）。
  - WinSSL 后端（`fafafa.ssl.winssl.certificate`）使用 `CertFindExtension(szOID_SUBJECT_ALT_NAME2, ...)` + `CryptDecodeObject` 解码为 `CERT_ALT_NAME_INFO`，当前已枚举 `CERT_ALT_NAME_DNS_NAME`、`CERT_ALT_NAME_IP_ADDRESS`、`CERT_ALT_NAME_RFC822_NAME`、`CERT_ALT_NAME_URL`，输出纯域名/IP/邮箱/URI，与 OpenSSL 一致。
- 抽象层约定：
  - `ISSLCertificate.GetSubjectAltNames` 始终返回 **不带前缀** 的主机名/IP/邮箱/URI 字符串（例如 `san-test.local`、`example.test`、`127.0.0.1`、`admin@example.test`、`spiffe://mesh/node`），不暴露 `DNS:` / `IP Address:` 等后端格式细节；
  - `TSSLCertificateInfo.SubjectAltNames` 为上述结果的只读快照，两后端语义保持一致，方便上层做主机名匹配或调试输出。

此外，主机名验证统一遵循以下策略：
- OpenSSL 后端直接调用 `X509_check_host`，自动处理 SAN/CN、通配符及 IP/DNS 区分；
- WinSSL 后端在 `VerifyHostname` 中复用 `TSSLUtils.IsIPAddress/IsValidHostname`，遍历 SAN 时将 IP 与域名通配逻辑分离，忽略 Email/URI 条目并在 SAN 未命中时回退到 CN，与 OpenSSL 语义对齐。

## 4. 错误处理架构

### 4.1 四层错误处理机制

1. **原生错误捕获**：捕获底层库的错误码和消息
2. **错误码映射**：将原生错误映射到统一的 `TSSLErrorCode`
3. **上下文信息**：添加操作上下文和堆栈信息
4. **用户友好消息**：提供可读的错误描述

### 4.2 错误信息结构
```pascal
ESSLException = class(Exception)
  ErrorCode: TSSLErrorCode;        // 统一错误码
  LibraryType: TSSLLibraryType;    // 源库类型
  NativeError: Integer;            // 原生错误码
  NativeMessage: string;           // 原生错误消息
  Context: string;                 // 操作上下文
end;
```

### 4.3 错误处理策略
- **即时转换**：底层错误立即转换为统一格式
- **上下文保留**：保持原始错误信息便于调试
- **分级处理**：区分致命错误和可恢复错误
- **日志集成**：自动记录错误详情

## 5. 内存管理策略

### 5.1 资源管理原则
- **RAII模式**：对象构造时获取资源，析构时释放
- **引用计数**：使用接口的自动引用计数
- **异常安全**：确保异常情况下资源正确释放

### 5.2 缓冲区管理
- **统一缓冲区大小**：默认 16KB，可配置
- **零拷贝优化**：尽可能避免数据拷贝
- **内存池**：考虑为频繁分配的小对象使用内存池

## 6. 性能优化设计

### 6.1 延迟加载
- 动态库延迟加载，只在需要时加载
- 上下文延迟初始化，减少启动开销

### 6.2 缓存策略  
- 证书验证结果缓存
- DNS解析结果缓存（如果涉及）
- 会话复用支持

### 6.3 批量操作
- 支持批量证书验证
- 批量数据传输接口

## 7. 线程安全设计

### 7.1 线程安全等级
- **库级别**：确保库的初始化/清理线程安全
- **上下文级别**：上下文可在多线程间共享（只读配置）
- **连接级别**：单个连接不支持并发操作，需要外部同步

### 7.2 同步机制
- 使用 Pascal 的 `TCriticalSection` 保护共享资源
- 原子操作用于简单的计数器和状态标志

## 8. 配置管理

### 8.1 配置层次
```pascal
TSSLConfig = record
  // 全局配置
  DefaultLibraryType: TSSLLibraryType;
  BufferSize: Integer;
  LogLevel: TSSLLogLevel;
  
  // 上下文配置  
  ProtocolVersion: TSSLProtocolVersion;
  VerifyMode: TSSLVerifyMode;
  CertificatePath: string;
  PrivateKeyPath: string;
  
  // 连接配置
  HandshakeTimeout: Integer;
  ReadTimeout: Integer;
  WriteTimeout: Integer;
end;
```

### 8.2 配置来源优先级
1. 代码中显式设置的参数
2. 环境变量
3. 配置文件
4. 默认值

## 9. 测试策略

### 9.1 测试覆盖
- **单元测试**：每个接口方法的功能测试
- **集成测试**：不同后端的兼容性测试
- **压力测试**：并发连接和大数据量测试
- **安全测试**：恶意输入和边界条件测试

### 9.2 模拟测试
- Mock SSL服务器用于客户端测试
- 证书生成工具用于测试不同证书场景
- 网络条件模拟（延迟、丢包、中断）

## 10. 部署和分发

### 10.1 库文件组织
```
fafafa.ssl/
├── bin/           # 编译后的库文件
├── include/       # Pascal 单元文件
├── examples/      # 使用示例
└── docs/          # 文档
```

### 10.2 依赖管理
- 静态链接优先，减少运行时依赖
- 提供动态库版本支持更新
- 清晰的版本兼容性矩阵

## 11. 开发里程碑

### 阶段1：基础架构 (Week 1-2)
- [ ] 创建类型定义 (`fafafa.ssl.types`)
- [ ] 设计核心接口 (`fafafa.ssl.intf`)
- [ ] 实现工厂模式 (`fafafa.ssl.factory`)
- [ ] 建立测试框架

### 阶段2：OpenSSL 后端 (Week 3-4)
- [ ] OpenSSL 绑定和封装
- [ ] 基本 SSL 上下文功能
- [ ] 客户端连接实现
- [ ] 单元测试完成

### 阶段3：WolfSSL 后端 (Week 5-6)
- [ ] WolfSSL 绑定和封装
- [ ] 接口实现和测试
- [ ] 兼容性验证

### 阶段4：MbedTLS 后端 (Week 7-8)
- [ ] MbedTLS 绑定和封装
- [ ] 接口实现和测试
- [ ] 性能对比分析

### 阶段5：WinSSL 后端 (Week 9-10)
- [ ] Windows Schannel API 封装
- [ ] 接口实现和测试
- [ ] Windows 平台优化

### 阶段6：完善和优化 (Week 11-12)
- [ ] 错误处理完善
- [ ] 性能优化和对比分析
- [ ] 文档完善
- [ ] 发布准备

## 12. 风险评估

### 12.1 技术风险
- **库版本兼容性**：不同版本 SSL 库的 API 差异
- **平台差异**：Windows/Linux/macOS 的实现差异
- **性能开销**：抽象层带来的性能损失

### 12.2 缓解策略
- 建立完整的测试矩阵覆盖主要版本组合
- 使用条件编译处理平台差异
- 基准测试持续监控性能影响

---
**文档版本**: 1.0  
**创建时间**: 2025-09-28  
**作者**: fafafa.ssl 开发团队
