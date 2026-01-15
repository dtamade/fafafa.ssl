# Phase 2 实施计划 - API 优雅度提升

**开始日期**: 2025-01-19
**预计完成**: 2025-01-25
**目标**: 对标 Rust 类库框架，提升 API 的优雅性、易用性和表达力

## 📋 总体目标

Phase 2 将专注于提升 fafafa.ssl 的 API 设计质量，使其达到 Rust 生态中优秀库的水平。主要关注：

1. **Builder 模式增强** - 预设配置、验证、更流畅的 API
2. **Fluent API 扩展** - 更多链式方法、组合器模式
3. **零拷贝优化** - 减少不必要的内存分配
4. **类型安全改进** - 编译时检查、更强的类型约束

## 🎯 Phase 2.1 - Builder 模式增强

### 目标

改进现有的 Builder 模式，提供更友好的 API 和更强的功能。

### 任务清单

#### 2.1.1 预设配置 (Presets)

类似 Rust 的配置模式：

```pascal
// 开发环境预设
LContext := TSSLContextBuilder.Development
  .WithCertificateFile('dev.crt')
  .BuildServer;

// 生产环境预设
LContext := TSSLContextBuilder.Production
  .WithCertificateFile('prod.crt')
  .BuildServer;

// 严格安全预设
LContext := TSSLContextBuilder.StrictSecurity
  .WithCertificateFile('secure.crt')
  .BuildServer;
```

**预设配置包含**：
- `Development` - 开发环境（宽松验证、详细日志）
- `Production` - 生产环境（严格安全、性能优化）
- `StrictSecurity` - 严格安全（最高安全等级）
- `LegacyCompatibility` - 兼容模式（支持旧协议）

#### 2.1.2 配置验证

在 Build 时验证配置的完整性和一致性：

```pascal
type
  TBuildValidationResult = record
    IsValid: Boolean;
    Warnings: TStringList;  // 警告（不阻止构建）
    Errors: TStringList;    // 错误（阻止构建）
  end;

  ISSLContextBuilder = interface
    // 验证配置（不构建）
    function Validate: TBuildValidationResult;

    // 验证并构建
    function BuildWithValidation(out AResult: TBuildValidationResult): ISSLContext;
  end;
```

#### 2.1.3 配置导入/导出

支持配置的序列化和反序列化：

```pascal
type
  ISSLContextBuilder = interface
    // 导出配置为 JSON
    function ExportToJSON: string;

    // 从 JSON 导入配置
    function ImportFromJSON(const AJSON: string): ISSLContextBuilder;

    // 导出配置为 INI
    function ExportToINI: string;

    // 从 INI 导入配置
    function ImportFromINI(const AINI: string): ISSLContextBuilder;
  end;
```

#### 2.1.4 配置快照和克隆

支持配置的快照和克隆：

```pascal
type
  ISSLContextBuilder = interface
    // 克隆当前配置
    function Clone: ISSLContextBuilder;

    // 重置为默认配置
    function Reset: ISSLContextBuilder;

    // 与另一个配置合并
    function Merge(AOther: ISSLContextBuilder): ISSLContextBuilder;
  end;
```

### 交付成果

- [ ] 4 个预设配置实现
- [ ] 配置验证功能
- [ ] JSON/INI 导入导出
- [ ] 配置克隆和合并
- [ ] 完整的单元测试（20+ 测试）
- [ ] 使用示例

## 🎯 Phase 2.2 - Fluent API 扩展

### 目标

扩展流式 API，支持更复杂的配置场景和更优雅的表达。

### 任务清单

#### 2.2.1 条件配置

支持基于条件的配置：

```pascal
type
  ISSLContextBuilder = interface
    // 条件配置
    function When(ACondition: Boolean): ISSLConditionalBuilder;
    function Unless(ACondition: Boolean): ISSLConditionalBuilder;
  end;

  ISSLConditionalBuilder = interface
    // 条件为真时执行
    function WithOption(AOption: TSSLOption): ISSLConditionalBuilder;
    function WithCipherList(const ACiphers: string): ISSLConditionalBuilder;

    // 返回主 Builder
    function EndWhen: ISSLContextBuilder;
  end;

// 使用示例
LContext := TSSLContextBuilder.Create
  .When(IsProduction)
    .WithTLS13
    .WithStrictVerification
  .EndWhen
  .Unless(IsDebugMode)
    .WithSessionCache(True)
  .EndWhen
  .BuildClient;
```

#### 2.2.2 配置组合器

支持配置的组合和复用：

```pascal
type
  TSSLConfigApplier = reference to procedure(ABuilder: ISSLContextBuilder);

function CommonSecurityConfig: TSSLConfigApplier;
begin
  Result := procedure(ABuilder: ISSLContextBuilder)
  begin
    ABuilder
      .WithTLS12And13
      .WithSafeDefaults
      .WithVerifyPeer;
  end;
end;

// 使用
LContext := TSSLContextBuilder.Create
  .Apply(CommonSecurityConfig)
  .WithCertificateFile('server.crt')
  .BuildServer;
```

#### 2.2.3 批量配置

支持批量设置选项：

```pascal
type
  ISSLContextBuilder = interface
    // 批量设置选项
    function WithOptions(const AOptions: array of TSSLOption): ISSLContextBuilder;

    // 批量设置协议
    function WithProtocols(const AVersions: array of TSSLProtocolVersion): ISSLContextBuilder;

    // 批量设置 CA 证书
    function WithCAFiles(const AFiles: array of string): ISSLContextBuilder;
  end;
```

#### 2.2.4 链式转换

支持配置的链式转换：

```pascal
type
  ISSLContextBuilder = interface
    // 转换为服务器配置
    function AsServer: ISSLServerBuilder;

    // 转换为客户端配置
    function AsClient: ISSLClientBuilder;
  end;

  ISSLServerBuilder = interface(ISSLContextBuilder)
    // 服务器特定配置
    function WithServerNameCallback(ACallback: TSSLServerNameCallback): ISSLServerBuilder;
    function WithALPNSelect(ACallback: TALPNSelectCallback): ISSLServerBuilder;
  end;

  ISSLClientBuilder = interface(ISSLContextBuilder)
    // 客户端特定配置
    function WithServerName(const AName: string): ISSLClientBuilder;
    function WithALPNProtocols(const AProtocols: array of string): ISSLClientBuilder;
  end;
```

### 交付成果

- [ ] 条件配置实现
- [ ] 配置组合器
- [ ] 批量配置方法
- [ ] 链式转换
- [ ] 完整的单元测试（15+ 测试）
- [ ] 使用示例

## 🎯 Phase 2.3 - 零拷贝优化

### 目标

减少不必要的内存分配和拷贝，提升性能关键路径的效率。

### 任务清单

#### 2.3.1 分析内存分配热点

使用性能分析工具识别：
- 频繁的 TBytes 拷贝
- 不必要的字符串转换
- 临时对象分配

#### 2.3.2 引入借用语义

类似 Rust 的借用检查器概念：

```pascal
type
  // 只读视图（不拥有数据）
  TBytesView = record
    Data: PByte;
    Length: Integer;

    function AsBytes: TBytes;  // 需要时才拷贝
  end;

  // 零拷贝哈希
  class function TCryptoUtils.SHA256View(const AView: TBytesView): TBytes;
```

#### 2.3.3 就地操作

支持就地修改而不是返回新数据：

```pascal
type
  TCryptoUtils = class
    // 就地加密（覆盖输入数据）
    class procedure EncryptInPlace(var AData: TBytes; const AKey, AIV: TBytes);

    // 就地解密
    class procedure DecryptInPlace(var AData: TBytes; const AKey, AIV: TBytes);
  end;
```

#### 2.3.4 流式处理

支持大文件的流式处理：

```pascal
type
  IHashStream = interface
    procedure Update(const AData: TBytes);
    procedure UpdateView(const AView: TBytesView);
    function Finalize: TBytes;
  end;

  TCryptoUtils = class
    // 创建流式哈希
    class function CreateHashStream(AAlgo: THashAlgorithm): IHashStream;
  end;

// 使用
var
  LHash: IHashStream;
  LChunk: TBytes;
begin
  LHash := TCryptoUtils.CreateHashStream(haSSHA256);

  while ReadChunk(LChunk) do
    LHash.Update(LChunk);  // 无需保存整个文件

  LResult := LHash.Finalize;
end;
```

### 交付成果

- [ ] 内存分配热点分析报告
- [ ] TBytesView 实现
- [ ] 就地操作方法（5+ 个）
- [ ] 流式处理接口
- [ ] 性能基准测试
- [ ] 性能对比报告

## 🎯 Phase 2.4 - 类型安全改进

### 目标

增强类型安全，在编译时捕获更多错误。

### 任务清单

#### 2.4.1 枚举类型扩展

使用枚举替代魔术数字：

```pascal
type
  TSSLVersion = (
    sslv_TLS10,
    sslv_TLS11,
    sslv_TLS12,
    sslv_TLS13
  );

  THashAlgorithm = (
    ha_MD5,
    ha_SHA1,
    ha_SHA256,
    ha_SHA384,
    ha_SHA512,
    ha_SHA3_256,
    ha_SHA3_512
  );

  TCipherAlgorithm = (
    ca_AES128_GCM,
    ca_AES256_GCM,
    ca_AES128_CBC,
    ca_AES256_CBC,
    ca_ChaCha20_Poly1305
  );
```

#### 2.4.2 泛型封装

使用泛型提供类型安全的 API：

```pascal
type
  TSecureData<T> = record
  private
    FData: T;
    FValid: Boolean;
  public
    class function Create(const AData: T): TSecureData<T>; static;
    function Unwrap: T;
    function IsValid: Boolean;
  end;

// 使用
var
  LKey: TSecureData<TBytes>;
begin
  LKey := TSecureData<TBytes>.Create(GenerateKey);
  if LKey.IsValid then
    Process(LKey.Unwrap);
end;
```

#### 2.4.3 单位类型

使用单位类型避免参数混淆：

```pascal
type
  TKeySize = record
  private
    FBits: Integer;
  public
    class function Bits(ABits: Integer): TKeySize; static;
    class function Bytes(ABytes: Integer): TKeySize; static;
    function ToBits: Integer;
    function ToBytes: Integer;
  end;

// 使用
var
  LKey: TEVP_PKEY;
begin
  LKey := GenerateRSAKey(TKeySize.Bits(2048));  // 明确是位数
  LKey := GenerateRSAKey(TKeySize.Bytes(256));  // 明确是字节数
end;
```

### 交付成果

- [ ] 枚举类型定义（10+ 个）
- [ ] 泛型封装实现
- [ ] 单位类型实现
- [ ] 类型安全测试（10+ 测试）
- [ ] 迁移指南

## 📅 时间表

```
Week 1 (Jan 19-21):
├── Phase 2.1 - Builder 模式增强
│   ├── Day 1: 预设配置实现
│   ├── Day 2: 配置验证
│   └── Day 3: 导入导出、克隆

Week 2 (Jan 22-23):
├── Phase 2.2 - Fluent API 扩展
│   ├── Day 4: 条件配置、组合器
│   └── Day 5: 批量配置、链式转换

Week 3 (Jan 24-25):
├── Phase 2.3 - 零拷贝优化
│   ├── Day 6: 分析、TBytesView、就地操作
│   └── Day 7: 流式处理、性能测试

Optional (if time permits):
└── Phase 2.4 - 类型安全改进
    └── Day 8: 枚举、泛型、单位类型
```

## 📊 成功指标

| 指标 | 目标 |
|------|------|
| 新增 API 方法 | 30+ |
| 单元测试 | 50+ |
| 测试通过率 | 100% |
| 性能提升 | 10-20% |
| API 文档 | 100% 覆盖 |
| 示例程序 | 5+ |

## 🎓 设计原则

### 1. 渐进增强

- 保持向后兼容
- 新 API 作为旧 API 的补充
- 提供迁移路径

### 2. 显式优于隐式

- 配置明确，无隐藏行为
- 错误明确，不吞掉异常
- 默认值合理，但可覆盖

### 3. 组合优于继承

- 使用接口和组合
- 避免深层继承
- 支持配置复用

### 4. 最小惊讶原则

- API 行为符合直觉
- 命名清晰一致
- 遵循 Pascal 惯例

## ✨ 参考资料

### Rust 生态优秀库

1. **rustls** - TLS 库
   - ConfigBuilder 模式
   - 预设配置
   - 零拷贝设计

2. **hyper** - HTTP 库
   - Fluent API
   - Builder 模式
   - 类型安全

3. **tokio** - 异步运行时
   - Builder 模式
   - 配置验证
   - 性能优化

### FreePascal/Delphi 最佳实践

1. **接口设计**
   - 使用接口而不是具体类
   - 引用计数管理生命周期
   - 支持方法链

2. **错误处理**
   - 提供多种错误处理模式
   - 清晰的错误消息
   - 错误恢复机制

3. **性能优化**
   - 避免不必要的拷贝
   - 使用内联函数
   - 缓存频繁计算的结果

---

**状态**: 📋 规划阶段
**下一步**: 开始 Phase 2.1.1 - 预设配置实现
**负责人**: Claude Code
**更新日期**: 2025-01-19
