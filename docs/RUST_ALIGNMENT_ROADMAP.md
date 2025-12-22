# fafafa.ssl Rust 架构对齐路线图

> 目标：将 fafafa.ssl 库的架构质量提升至与 Rust 生态（rustls、native-tls）同等水平

---

## 一、现状分析摘要

### 1.1 代码规模
| 指标 | 数值 |
|------|------|
| 总代码行数 | 60,621 行 |
| 源模块数 | 109 个 |
| 测试文件数 | 276 个 |
| 测试代码行数 | 78,308 行 |
| 测试:源代码比例 | 1.29:1 |

### 1.2 已实现的 Rust 模式
| Rust 模式 | fafafa.ssl 实现 | 状态 |
|-----------|-----------------|------|
| Result<T, E> | TSSLOperationResult, TSSLDataResult | ✅ 完整 |
| Builder 模式 | ISSLContextBuilder 流式接口 | ✅ 完整 |
| 后端抽象 | OpenSSL/WinSSL 接口分离 | ✅ 完整 |
| 零拷贝视图 | TBytesView 记录 | ✅ 完整 |
| Option<T> | TSecureData<T> | ✅ 完整 |
| 强枚举 | TSSLVersion, TKeyType 等 | ✅ 完整 |
| 单元类型 | TKeySize, TTimeoutDuration | ✅ 完整 |

### 1.3 架构问题（技术债务）
| 问题 | 严重性 | 影响 |
|------|--------|------|
| ISSLContext 接口过大（135方法） | 🔴 高 | 维护困难，违反单一职责 |
| OpenSSL API 模块爆炸（62个） | 🔴 高 | 编译时间长，依赖复杂 |
| OpenSSL/WinSSL 代码重复 | 🟡 中 | 双倍维护成本 |
| 异常类过多（26个） | 🟡 中 | 维护复杂 |
| 缺少模糊测试 | 🟡 中 | 安全风险 |
| 缺少覆盖率报告 | 🟢 低 | 质量不可量化 |

---

## 二、Rust 对标分析

### 2.1 rustls 架构特点

```
rustls 架构
├── client/server          # 角色分离
├── crypto                 # 可插拔 CryptoProvider
├── sign                   # 签名抽象
├── pki_types             # 证书类型
└── quic                   # 协议扩展

关键设计：
- 加密管道模型：I/O 与加密分离
- 类型状态 ConfigBuilder
- 200+ 机器可读错误变体
- 零拷贝 read_buf 接口
```

### 2.2 native-tls 架构特点

```
native-tls 架构
├── TlsConnector          # 客户端统一接口
├── TlsAcceptor           # 服务端统一接口
├── TlsStream             # 流抽象
└── 后端自动选择           # 编译时平台检测

关键设计：
- 最小公共接口
- 安全默认配置
- 平台差异内部处理
```

### 2.3 差距分析

| 特性 | rustls/native-tls | fafafa.ssl | 差距 |
|------|-------------------|------------|------|
| 接口粒度 | 细粒度（5-10方法/接口） | 粗粒度（50-135方法） | 🔴 大 |
| 错误设计 | 枚举 + 非穷尽 | 异常类层次 | 🟡 中 |
| 加密提供者 | 可插拔 ICryptoProvider | 硬编码后端 | 🟡 中 |
| 类型状态 | 编译时状态验证 | 运行时检查 | 🟢 小 |
| 模块组织 | 功能分组 | 每算法一模块 | 🔴 大 |

---

## 三、改进路线图

### Phase 1: 接口重构（优先级：🔴 关键）

#### 1.1 拆分 ISSLContext（135方法 → 6接口）

**目标**：遵循接口隔离原则，每接口 10-20 方法

```
ISSLContext (135 方法)
    ↓ 拆分为
├── ISSLContextCore          # 核心配置（协议版本、选项）
├── ISSLContextCertificate   # 证书管理（加载、验证）
├── ISSLContextCipher        # 密码套件配置
├── ISSLContextSession       # 会话管理
├── ISSLContextCallbacks     # 回调设置
└── ISSLContextFactory       # 连接创建
```

**实施步骤**：
1. 创建新接口声明（保持 ISSLContext 作为聚合接口）
2. 将现有方法分配到新接口
3. 更新实现类继承多接口
4. 逐步迁移调用代码
5. 最终废弃聚合接口

**文件变更**：
- `src/fafafa.ssl.base.pas` - 新增 6 个接口
- `src/fafafa.ssl.openssl.context.pas` - 实现多接口
- `src/fafafa.ssl.winssl.context.pas` - 实现多接口

#### 1.2 拆分 ISSLLibrary（87方法 → 4接口）

```
ISSLLibrary (87 方法)
    ↓ 拆分为
├── ISSLLibraryCore     # 初始化、版本查询
├── ISSLLibraryFactory  # 对象创建
├── ISSLLibraryFeatures # 特性检测
└── ISSLLibraryConfig   # 全局配置
```

---

### Phase 2: 模块重组（优先级：🔴 关键）

#### 2.1 OpenSSL API 分组（62模块 → 8组）

**目标**：按功能域组织，减少编译依赖

```
当前结构（62 个独立模块）：
src/fafafa.ssl.openssl.api.aes.pas
src/fafafa.ssl.openssl.api.des.pas
src/fafafa.ssl.openssl.api.chacha.pas
... (59 more)

目标结构（8 个功能组）：
src/fafafa.ssl.openssl.api/
├── hashing/           # md, sha, sha3, blake2, sm3
│   ├── base.pas      # 公共类型
│   └── algorithms.pas # 所有哈希算法
├── symmetric/         # aes, des, chacha, camellia, sm4
│   ├── base.pas
│   └── algorithms.pas
├── asymmetric/        # rsa, dsa, ec, ed25519
│   ├── base.pas
│   └── algorithms.pas
├── keyexchange/       # dh, ecdh, x25519
├── mac/               # hmac, cmac, gmac
├── kdf/               # pbkdf2, hkdf, scrypt
├── encoding/          # pem, der, pkcs7, pkcs12
└── protocols/         # ssl, ocsp, crl, ct
```

**实施策略**：
- 使用 `{$I include}` 指令合并
- 保持原有公共 API 不变
- 提供兼容性别名单元

#### 2.2 创建模块索引

```pascal
// src/fafafa.ssl.openssl.api.pas - 统一入口
unit fafafa.ssl.openssl.api;
interface
uses
  fafafa.ssl.openssl.api.hashing,
  fafafa.ssl.openssl.api.symmetric,
  fafafa.ssl.openssl.api.asymmetric,
  // ...
```

---

### Phase 3: 错误处理升级（优先级：🟡 重要）

#### 3.1 错误码枚举化

**目标**：减少异常类数量，增加错误码细粒度

```pascal
// 当前：26 个异常类
// 目标：3-5 个异常类 + 细粒度错误码枚举

type
  TSSLErrorCategory = (
    secNone,
    secInitialization,
    secConfiguration,
    secCertificate,
    secConnection,
    secCrypto,
    secSystem
  );

  TSSLErrorDetail = (
    // Initialization (100-199)
    sedLibraryNotFound = 100,
    sedVersionMismatch = 101,
    sedFunctionNotLoaded = 102,

    // Certificate (200-299)
    sedCertExpired = 200,
    sedCertNotYetValid = 201,
    sedCertRevoked = 202,
    sedCertUntrustedRoot = 203,
    sedCertSignatureInvalid = 204,
    // ... 200+ 细粒度错误码
  );

  ESSLError = class(Exception)
    Category: TSSLErrorCategory;
    Detail: TSSLErrorDetail;
    NativeCode: Integer;
    Context: string;
  end;
```

#### 3.2 Result 类型扩展使用

```pascal
// 扩展 TSSLResult<T> 到更多 API
function LoadCertificate(const APath: string): TSSLResult<ISSLCertificate>;
function Connect(const AHost: string; APort: Word): TSSLResult<ISSLConnection>;
function Handshake: TSSLResult<TSSLHandshakeInfo>;
```

---

### Phase 4: 加密提供者抽象（优先级：🟡 重要）

#### 4.1 ICryptoProvider 接口

**目标**：类似 rustls CryptoProvider，支持可插拔加密实现

```pascal
type
  ICryptoProvider = interface
    // 哈希
    function CreateHasher(AAlgorithm: THashAlgorithm): IHasher;
    function Hash(AAlgorithm: THashAlgorithm; const AData: TBytes): TBytes;

    // 对称加密
    function CreateCipher(AAlgorithm: TCipherAlgorithm): ICipher;

    // 非对称
    function GenerateKeyPair(AType: TKeyType; ASize: Integer): IKeyPair;
    function CreateSigner(AAlgorithm: TSignatureAlgorithm): ISigner;

    // 随机数
    function RandomBytes(ACount: Integer): TBytes;

    // 密钥派生
    function DeriveKey(AKDF: TKDF; const AParams: TKDFParams): TBytes;
  end;

  // 内置实现
  TOpenSSLCryptoProvider = class(TInterfacedObject, ICryptoProvider)
  TWinSSLCryptoProvider = class(TInterfacedObject, ICryptoProvider)

  // 用户可自定义
  TCustomCryptoProvider = class(TInterfacedObject, ICryptoProvider)
```

#### 4.2 Provider 注册机制

```pascal
// 全局 Provider 注册
TSSLFactory.RegisterCryptoProvider('openssl', TOpenSSLCryptoProvider);
TSSLFactory.RegisterCryptoProvider('winssl', TWinSSLCryptoProvider);
TSSLFactory.RegisterCryptoProvider('custom', TMyCustomProvider);

// 使用
var Provider := TSSLFactory.GetCryptoProvider('openssl');
```

---

### Phase 5: 测试增强（优先级：🟡 重要）

#### 5.1 模糊测试框架

```pascal
// src/fafafa.ssl.fuzz.pas
type
  TFuzzTarget = procedure(const AInput: TBytes);

  TFuzzer = class
    procedure RegisterTarget(const AName: string; ATarget: TFuzzTarget);
    procedure Run(AIterations: Integer = 10000);
    procedure RunWithCorpus(const ACorpusPath: string);
  end;

// tests/fuzz/fuzz_certificate.pas
procedure FuzzCertificateParse(const AInput: TBytes);
begin
  try
    TCertificateParser.Parse(AInput);
  except
    // 记录但不崩溃
  end;
end;

// 注册模糊测试目标
Fuzzer.RegisterTarget('certificate_parse', @FuzzCertificateParse);
Fuzzer.RegisterTarget('pem_decode', @FuzzPEMDecode);
Fuzzer.RegisterTarget('asn1_parse', @FuzzASN1Parse);
```

#### 5.2 覆盖率集成

```bash
# 使用 gcov 或类似工具
fpc -gw -O- -dCOVERAGE tests/test_all.pas
./tests/bin/test_all
gcov src/*.pas
```

#### 5.3 性能基线建立

```pascal
// tests/benchmarks/baseline.pas
type
  TPerformanceBaseline = record
    RSA2048KeyGen: Double;      // ms
    RSA4096KeyGen: Double;
    AES256GCMEncrypt1MB: Double;
    SHA256Hash1MB: Double;
    TLS12Handshake: Double;
    TLS13Handshake: Double;
  end;

procedure EstablishBaseline;
procedure CompareWithBaseline;
procedure DetectRegression(AThreshold: Double = 0.1);
```

---

### Phase 6: 代码重复消除（优先级：🟢 改进）

#### 6.1 提取公共证书逻辑

```
当前：
  fafafa.ssl.openssl.certificate.pas (1472 行)
  fafafa.ssl.winssl.certificate.pas (1442 行)
  重复率: ~60%

目标：
  fafafa.ssl.cert.base.pas          # 公共抽象基类
  fafafa.ssl.openssl.certificate.pas # OpenSSL 特定
  fafafa.ssl.winssl.certificate.pas  # WinSSL 特定
  减少: ~800 行重复代码
```

#### 6.2 提取公共连接逻辑

```pascal
// src/fafafa.ssl.connection.base.pas
type
  TSSLConnectionBase = class abstract(TInterfacedObject, ISSLConnection)
  protected
    FContext: ISSLContext;
    FHandshakeState: TSSLHandshakeState;
    FConnected: Boolean;
    FTimeout: Integer;

    // 模板方法
    function DoHandshakeInternal: TSSLHandshakeState; virtual; abstract;
    function DoReadInternal(var ABuffer; ACount: Integer): Integer; virtual; abstract;
    function DoWriteInternal(const ABuffer; ACount: Integer): Integer; virtual; abstract;

  public
    // 公共实现
    function IsHandshakeComplete: Boolean;
    function GetConnectionInfo: TSSLConnectionInfo;
    // ...
  end;
```

---

### Phase 7: 配置接口化（优先级：🟢 改进）

#### 7.1 ISSLConfig 接口

```pascal
type
  ISSLConfig = interface
    function GetProtocolVersions: TSSLProtocolVersions;
    procedure SetProtocolVersions(AVersions: TSSLProtocolVersions);

    function GetCipherList: string;
    procedure SetCipherList(const AList: string);

    function GetVerifyMode: TSSLVerifyModes;
    procedure SetVerifyMode(AMode: TSSLVerifyModes);

    // 序列化
    function ToJSON: string;
    procedure FromJSON(const AJSON: string);

    // 克隆
    function Clone: ISSLConfig;
  end;
```

#### 7.2 ISSLLogger 接口

```pascal
type
  TSSLLogLevel = (llDebug, llInfo, llWarning, llError, llCritical);

  ISSLLogger = interface
    procedure Log(ALevel: TSSLLogLevel; const AMessage: string);
    procedure LogException(E: Exception);
    procedure SetMinLevel(ALevel: TSSLLogLevel);
  end;

  // 内置实现
  TConsoleSSLLogger = class(TInterfacedObject, ISSLLogger)
  TFileSSLLogger = class(TInterfacedObject, ISSLLogger)
  TNullSSLLogger = class(TInterfacedObject, ISSLLogger)  // 用于测试
```

---

## 四、实施时间线

### 第一季度（Q1）- 基础重构

| 周次 | 任务 | 交付物 |
|------|------|--------|
| W1-2 | Phase 1.1: 设计新接口 | 接口声明文档 |
| W3-4 | Phase 1.1: 实现接口拆分 | 6 个新接口 |
| W5-6 | Phase 1.2: ISSLLibrary 拆分 | 4 个新接口 |
| W7-8 | Phase 5.2: 覆盖率集成 | CI 覆盖率报告 |
| W9-10 | Phase 3.1: 错误码枚举 | 新错误系统 |
| W11-12 | 测试和文档 | 迁移指南 |

### 第二季度（Q2）- 模块重组

| 周次 | 任务 | 交付物 |
|------|------|--------|
| W1-4 | Phase 2.1: OpenSSL API 分组 | 8 个模块组 |
| W5-6 | Phase 2.2: 模块索引 | 统一入口单元 |
| W7-8 | Phase 5.1: 模糊测试框架 | Fuzz 测试套件 |
| W9-10 | Phase 5.3: 性能基线 | 基准报告 |
| W11-12 | 兼容性测试 | 回归测试通过 |

### 第三季度（Q3）- 高级特性

| 周次 | 任务 | 交付物 |
|------|------|--------|
| W1-4 | Phase 4: ICryptoProvider | 可插拔加密 |
| W5-8 | Phase 6: 代码重复消除 | 减少 1500+ 行 |
| W9-12 | Phase 7: 配置接口化 | ISSLConfig, ISSLLogger |

### 第四季度（Q4）- 稳定化

| 周次 | 任务 | 交付物 |
|------|------|--------|
| W1-4 | 全面测试 | 覆盖率 > 85% |
| W5-8 | 性能优化 | 无回归 |
| W9-12 | 文档完善 | 架构文档、API 文档 |

---

## 五、关键指标目标

| 指标 | 当前值 | 目标值 | 改进 |
|------|--------|--------|------|
| 最大接口方法数 | 135 | ≤25 | -81% |
| OpenSSL 模块数 | 62 | 8 | -87% |
| 代码重复率 | ~60% | <20% | -67% |
| 异常类数量 | 26 | 5 | -81% |
| 测试覆盖率 | 未知 | >85% | 可量化 |
| 模糊测试 | 无 | 10+ 目标 | 新增 |
| 编译时间 | 基准 | -30% | 优化 |

---

## 六、风险与缓解

| 风险 | 可能性 | 影响 | 缓解措施 |
|------|--------|------|----------|
| 接口变更导致 API 不兼容 | 高 | 高 | 保留旧接口作为聚合，逐步废弃 |
| 模块重组破坏编译 | 中 | 高 | 提供兼容性别名单元 |
| 性能回归 | 中 | 中 | 建立基线，自动检测 |
| 测试覆盖下降 | 低 | 中 | CI 强制覆盖率检查 |

---

## 七、成功标准

### 技术标准
- [ ] 所有接口方法数 ≤ 25
- [ ] 测试覆盖率 ≥ 85%
- [ ] 编译时间减少 ≥ 30%
- [ ] 代码重复率 < 20%
- [ ] 模糊测试发现 0 个崩溃

### 质量标准
- [ ] 所有现有测试通过
- [ ] 无 API 破坏性变更（或提供迁移路径）
- [ ] 完整的架构文档
- [ ] Rust 开发者认可的 API 设计

---

## 附录

### A. 参考资料
- [rustls 源码](https://github.com/rustls/rustls)
- [native-tls 源码](https://github.com/sfackler/rust-native-tls)
- [Rust 错误处理最佳实践](https://www.memorysafety.org/blog/rustls-error-handling/)
- [类型状态模式](https://cliffle.com/blog/rust-typestate/)

### B. 相关文件
- `/docs/ARCHITECTURE.md` - 架构设计文档
- `/docs/API_MIGRATION.md` - API 迁移指南
- `/CHANGELOG.md` - 变更日志

---

*文档版本: 1.0*
*创建日期: 2025-12-22*
*作者: Claude Code*
