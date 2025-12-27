# Phase 2.2 实施计划 - Fluent API 扩展

**开始日期**: 2025-12-16
**目标**: 扩展 Fluent API，提供更优雅、更直观的配置方式

## 📋 总览

Phase 2.2 将在 Phase 2.1 的基础上，进一步增强 Builder API 的流畅性和易用性，引入更多 Rust 风格的 API 设计模式。

## 🎯 目标

### 主要目标
1. **条件配置** - 支持条件式配置（When/Unless）
2. **批量配置** - 支持批量设置和配置组合
3. **便利方法** - 提供常用场景的快捷方法
4. **函数式风格** - 支持配置函数和变换

### 与 Rust 对齐
- 类似 Rust 的条件构建模式
- 函数式配置传递
- 更强的类型安全
- 零成本抽象

## 📝 子阶段划分

### Phase 2.2.1 - 条件配置方法
**预计时间**: 2-3 小时

**任务**:
- [ ] 添加 `When` 方法 - 条件式配置
- [ ] 添加 `Unless` 方法 - 反向条件配置
- [ ] 添加 `WhenDevelopment` / `WhenProduction` - 环境条件
- [ ] 编写测试（预计 15-20 个测试）
- [ ] 创建完成报告

**示例**:
```pascal
LBuilder := TSSLContextBuilder.Create
  .When(IsProduction, @ConfigureProduction)
  .Unless(IsDebug, @EnableStrictSecurity)
  .WhenDevelopment(@AllowSelfSigned)
  .BuildClient;
```

### Phase 2.2.2 - 批量配置方法
**预计时间**: 2-3 小时

**任务**:
- [ ] 添加 `Apply` 方法 - 应用配置函数
- [ ] 添加 `ApplyPreset` 方法 - 应用预设但保留现有配置
- [ ] 添加 `Pipe` 方法 - 配置管道
- [ ] 编写测试（预计 15-20 个测试）
- [ ] 创建完成报告

**示例**:
```pascal
LBuilder := TSSLContextBuilder.Create
  .Apply(@ConfigureBaseSecurity)
  .ApplyPreset(TSSLContextBuilder.Production)
  .Pipe(@AddCustomCiphers)
  .BuildServer;
```

### Phase 2.2.3 - 便利方法
**预计时间**: 2-3 小时

**任务**:
- [ ] 添加 `WithCertificateChain` - 证书链配置
- [ ] 添加 `WithMutualTLS` - 双向 TLS 快捷配置
- [ ] 添加 `WithHTTP2` - HTTP/2 ALPN 快捷配置
- [ ] 添加 `WithModernDefaults` - 现代化默认配置
- [ ] 编写测试（预计 15-20 个测试）
- [ ] 创建完成报告

**示例**:
```pascal
LBuilder := TSSLContextBuilder.Create
  .WithCertificateChain([LCert1, LCert2, LCert3])
  .WithMutualTLS(LCAFile, True)
  .WithHTTP2
  .BuildServer;
```

### Phase 2.2.4 - 配置变换和组合
**预计时间**: 2-3 小时

**任务**:
- [ ] 添加 `Transform` 方法 - 配置变换
- [ ] 添加 `Extend` 方法 - 扩展配置
- [ ] 添加 `Override` 方法 - 覆盖特定字段
- [ ] 编写测试（预计 15-20 个测试）
- [ ] 创建完成报告

**示例**:
```pascal
LBuilder := TSSLContextBuilder.Production
  .Transform(@CustomizeForCloud)
  .Extend(LExtraOptions)
  .Override('cipher_list', 'CUSTOM-CIPHER')
  .BuildClient;
```

## 🔧 技术设计

### 1. 条件配置方法签名

```pascal
type
  TBuilderConfigProc = procedure(ABuilder: ISSLContextBuilder) of object;
  TBuilderPredicate = function: Boolean of object;

type
  ISSLContextBuilder = interface
    // 条件配置
    function When(ACondition: Boolean; AConfig: TBuilderConfigProc): ISSLContextBuilder;
    function Unless(ACondition: Boolean; AConfig: TBuilderConfigProc): ISSLContextBuilder;
    function WhenDevelopment(AConfig: TBuilderConfigProc): ISSLContextBuilder;
    function WhenProduction(AConfig: TBuilderConfigProc): ISSLContextBuilder;
  end;
```

### 2. 批量配置方法签名

```pascal
type
  ISSLContextBuilder = interface
    // 批量配置
    function Apply(AConfig: TBuilderConfigProc): ISSLContextBuilder;
    function ApplyPreset(APreset: ISSLContextBuilder): ISSLContextBuilder;
    function Pipe(ATransform: TBuilderConfigProc): ISSLContextBuilder;
  end;
```

### 3. 便利方法签名

```pascal
type
  ISSLContextBuilder = interface
    // 便利方法
    function WithCertificateChain(const ACerts: array of string): ISSLContextBuilder;
    function WithMutualTLS(const ACAFile: string; ARequired: Boolean = True): ISSLContextBuilder;
    function WithHTTP2: ISSLContextBuilder;
    function WithModernDefaults: ISSLContextBuilder;
  end;
```

### 4. 配置变换方法签名

```pascal
type
  TBuilderTransform = function(ABuilder: ISSLContextBuilder): ISSLContextBuilder of object;

type
  ISSLContextBuilder = interface
    // 配置变换
    function Transform(ATransform: TBuilderTransform): ISSLContextBuilder;
    function Extend(const AOptions: array of TSSLOption): ISSLContextBuilder;
    function Override(const AField, AValue: string): ISSLContextBuilder;
  end;
```

## 📊 预期成果

### 代码指标
- **新增方法**: 约 15-18 个
- **实现代码**: 约 400-500 行
- **测试代码**: 约 60-80 个测试
- **测试通过率**: 100%

### 文档产出
- 4 份子阶段完成报告
- 1 份 Phase 2.2 总结报告
- API 使用示例和最佳实践

## 🎓 设计原则

### 1. 零成本抽象
- 条件方法应该编译时优化
- 避免不必要的对象创建
- 内联简单操作

### 2. 类型安全
- 强类型的回调函数
- 编译时检查
- 避免运行时类型转换

### 3. 直观性
- 方法名清晰表达意图
- 符合 Pascal/Delphi 命名习惯
- 与 Rust API 对齐但不生硬照搬

### 4. 向后兼容
- 不破坏现有 API
- 纯粹的增量添加
- 可选使用新功能

## 🔄 与 Rust 对齐

### Rust Builder 条件配置模式

```rust
// Rust
let config = ServerConfig::builder()
    .when(is_prod, |b| b.with_strict_verify())
    .apply(|b| configure_ciphers(b))
    .build();
```

### fafafa.ssl 对应实现

```pascal
// FreePascal
LConfig := TSSLContextBuilder.Create
  .When(IsProd, @ConfigureStrictVerify)
  .Apply(@ConfigureCiphers)
  .BuildServer;
```

## 📅 时间规划

```
Week 1:
  Day 1: Phase 2.2.1 - 条件配置方法
  Day 2: Phase 2.2.2 - 批量配置方法
  Day 3: Phase 2.2.3 - 便利方法
  Day 4: Phase 2.2.4 - 配置变换和组合
  Day 5: 总结和文档完善
```

## ✅ 成功标准

- [ ] 所有计划方法实现完成
- [ ] 测试覆盖率 100%
- [ ] 所有测试通过
- [ ] 完整的文档和示例
- [ ] 与 Rust API 风格对齐
- [ ] 性能无明显下降

## 🚀 后续计划

Phase 2.2 完成后，将进入：
- **Phase 2.3** - Zero-copy 优化
- **Phase 2.4** - 类型状态模式（可选）
- **Phase 3** - 架构完整性审查

---

**Phase 2.2 状态**: 🚧 计划中
**预计完成时间**: 2025-12-16
**负责人**: Claude Code
