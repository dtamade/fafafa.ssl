# WARP.md - 开发规范与协作指导

本文档为 Warp AI 助手提供核心开发规范。详细的项目文档请参考各项目的 README 和 docs/ 目录。

---

## 📋 核心规范

### 1. 基本原则

#### 交流语言
- 使用中文进行所有交流和文档编写

#### 专业态度
- ⚠️ **不要过于顺从，要有自己的专业主见**
- 发现问题时主动提出更好的替代方案
- 基于最佳实践保持专业判断
- 优先考虑代码的可维护性、可扩展性和稳定性

#### 代码改动
- **重大架构改变必须先申请审批**（改变核心数据结构、引入新依赖、改变API接口）
- **优先选择简单方案**（避免过度设计和过早优化）

---

### 2. 代码风格

#### 编码设置
```pascal
{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}  // Windows下输出中文必须
```

#### 命名规则
- **局部变量**：`L` 开头（`LCount`, `LResult`）
- **参数**：`a` 开头（`aFileName`, `aOptions`）
- **类**：`T` 开头（`TWebviewApp`）
- **接口**：`I` 开头（`IWebview`）
- **字段**：`F` 开头（`FConnection`）
- **常量**：全大写+下划线（`MAX_CONNECTIONS`）

#### 格式
- 缩进：2个空格，不用Tab
- 函数：不超过50行
- 文件：避免超过1000行（考虑拆分）

---

### 3. 项目结构

#### 目录组织
```
项目根目录/
├── bin/                    # 二进制输出：模块名.类型.架构.系统
├── lib/                    # 中间文件：lib/$(TargetCPU)-$(TargetOS)/
├── src/                    # 源代码
├── tests/                  # 单元测试
├── examples/               # 示例代码
├── docs/                   # 详细文档
├── README.md              # 项目说明
└── WARP.md               # 本规范
```

#### 输出命名
- Windows: `fafafa.webview.test.x86_64.windows.exe`
- Linux: `fafafa.webview.test.x86_64.linux`
- macOS: `fafafa.webview.test.x86_64.darwin`

---

### 4. 质量要求

#### 必须完成
- [ ] 编写单元测试（`tests/单元名/test_单元名.pas`）
- [ ] 编写使用示例（`examples/功能名/`）
- [ ] 添加代码注释（公共方法、复杂逻辑）
- [ ] 配置.lpi项目文件

#### 错误处理
- 返回 HRESULT 或使用异常
- 添加详细日志
- 提供清晰错误信息

---

### 5. 版本控制

#### 提交格式
```
[类型] 简短描述

类型：新增、修复、优化、重构、文档、测试
示例：[新增] 添加 WebView2 Cookie 管理功能
```

#### 提交原则
- 一次提交只包含一个功能变更
- 提交前确保可编译
- 提交前运行测试

---

### 6. 模块化设计

#### 拆分策略
- 单一职责原则
- 避免单个文件超过1000行
- 通过清晰接口交互

#### 依赖层次
```
应用层 (app, examples)
    ↓
接口层 (interfaces)
    ↓
实现层 (factory, window)
    ↓
平台层 (platform-specific)
    ↓
基础层 (types, consts)
```

**避免循环依赖！**

---

## 🤖 Warp AI 协作范式

### 协作原则

#### ✅ 明确目标
- 好："实现 WebView2 的 Cookie 管理功能"
- 差："优化一些东西"

#### 📊 分阶段推进
1. 规划 → 2. 执行 → 3. 测试 → 4. 文档 → 5. 总结

#### 🎯 最佳实践驱动
不确定下一步时说**"按最佳实践继续"**，AI会自动选择最合理方案。

---

### 🧪 TDD 开发规范（必须遵守）

#### 核心原则：测试驱动开发

**⚠️ 重要：防止上下文抖动和项目推进困难**

#### 1. 开发流程（严格遵守）

```
红 → 绿 → 重构 → 文档
 ↓     ↓      ↓       ↓
测试  实现  优化   记录
```

**完整流程**：

1. **红（Red）** - 先写测试
   - 编写失败的测试用例
   - 明确预期行为
   - 确保测试可以编译但会失败

2. **绿（Green）** - 让测试通过
   - 编写最小可用代码
   - 只为了让测试通过
   - 不追求完美，先让它工作

3. **重构（Refactor）** - 改进代码
   - 消除重复
   - 改进设计
   - 保持测试通过

4. **文档（Document）** - 记录决策
   - 更新文档
   - 记录重要决策
   - 提供使用示例

#### 2. 测试组织规范

```
tests/
├── test_核心功能_模块名.pas          # 单元测试
├── test_integration_场景名.pas      # 集成测试
├── test_regression_bug编号.pas      # 回归测试
└── bin/                             # 测试输出
```

#### 3. 测试命名规范

```pascal
// 测试程序命名
program test_功能名_模块名;

// 测试过程命名
procedure Test功能名_预期行为_条件;
procedure TestAES加密_返回正确密文_使用256位密钥;
procedure TestX509解析_抛出异常_当证书无效;
```

#### 4. 测试结构模板

```pascal
program test_feature_module;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils,
  模块名;

var
  TotalTests, PassedTests, FailedTests: Integer;

procedure Test(const TestName: string; Condition: Boolean);
begin
  Inc(TotalTests);
  Write(TestName + ': ');
  if Condition then
  begin
    WriteLn('PASS');
    Inc(PassedTests);
  end
  else
  begin
    WriteLn('FAIL');
    Inc(FailedTests);
  end;
end;

procedure TestFeature1;
begin
  // Arrange (准备)
  // Act (执行)
  // Assert (断言)
  Test('Feature1 should work', Result = Expected);
end;

begin
  TotalTests := 0;
  PassedTests := 0;
  FailedTests := 0;
  
  WriteLn('Testing: Module Name');
  WriteLn('=' + StringOfChar('=', 50));
  
  TestFeature1;
  TestFeature2;
  
  WriteLn('=' + StringOfChar('=', 50));
  WriteLn(Format('Results: %d/%d passed, %d failed', 
    [PassedTests, TotalTests, FailedTests]));
  
  if FailedTests > 0 then
    ExitCode := 1
  else
    ExitCode := 0;
end.
```

#### 5. 持续验证规范

**每次修改后必须：**

```bash
# 1. 编译测试
fpc -Mobjfpc test_module.pas

# 2. 运行测试
./test_module

# 3. 检查结果
# 期望: 0 failed, exit code 0
```

#### 6. 防止上下文抖动的关键措施

##### ✅ DO（必须做）

1. **先写测试，后写实现**
   - 测试定义了"完成"的标准
   - 避免目标漂移

2. **每个功能都有测试**
   - 新功能：test_new_feature.pas
   - Bug修复：test_regression_bug123.pas
   - 重构：保持所有现有测试通过

3. **保持测试简单明确**
   - 一个测试只验证一个行为
   - 测试名称清晰说明意图
   - AAA模式：Arrange, Act, Assert

4. **增量推进**
   - 小步快跑，频繁验证
   - 每次只改一个测试
   - 保持已有测试通过

5. **记录测试结果**
   - 创建测试报告（如 VALIDATION_REPORT.md）
   - 记录通过/失败的测试
   - 追踪进度

##### ❌ DON'T（禁止做）

1. **❌ 不写测试就写代码**
   - 结果：不知道什么时候算完成
   - 导致：目标不清，反复修改

2. **❌ 跳过失败的测试**
   - 结果：累积技术债务
   - 导致：后续难以维护

3. **❌ 修改代码不运行测试**
   - 结果：引入回归错误
   - 导致：推进困难，反复修复

4. **❌ 一次修改太多**
   - 结果：难以定位问题
   - 导致：上下文混乱

5. **❌ 测试不清晰**
   - 结果：不知道测试什么
   - 导致：假阳性/假阴性

#### 7. 验证检查清单

每个功能完成前检查：

- [ ] ✅ 测试已编写（先写测试）
- [ ] ✅ 测试初始失败（红）
- [ ] ✅ 实现让测试通过（绿）
- [ ] ✅ 代码已重构（改进）
- [ ] ✅ 所有测试通过（验证）
- [ ] ✅ 测试结果已记录（文档）
- [ ] ✅ 示例代码已提供（文档）

#### 8. 批量模块验证流程

对于大型项目（如 fafafa.ssl 的 65 个模块）：

```
阶段1: 规划
├─ 列出所有模块
├─ 按优先级分组
└─ 创建清单文档

阶段2: 批量测试（按优先级）
├─ 创建批量测试程序
├─ 编译验证
├─ 运行验证
└─ 记录结果

阶段3: 详细测试（核心功能）
├─ 功能测试
├─ 边界测试
├─ 错误测试
└─ 性能测试

阶段4: 总结报告
├─ 成功率统计
├─ 问题分类
├─ 修复建议
└─ 生产就绪评估
```

#### 9. 上下文恢复机制

当需要恢复工作时：

```bash
# 1. 查看最新的验证报告
cat COMPLETE_VALIDATION_SUMMARY.md

# 2. 运行所有测试确认当前状态
find tests/ -name "test_*.pas" -exec fpc {} \;
find tests/bin/ -name "test_*" -exec {} \;

# 3. 查看待办事项
grep "TODO\|FIXME\|XXX" -r src/

# 4. 继续下一个优先级
```

#### 10. 质量门禁

**提交代码前必须满足：**

```
✅ 编译通过
✅ 所有测试通过
✅ 测试覆盖率 ≥ 80%（核心功能 100%）
✅ 无编译警告（或已记录原因）
✅ 文档已更新
```

#### 11. 示例：完整TDD循环

```pascal
// ===== 第1步：写测试（红） =====
program test_aes_encryption;
procedure TestAES256_EncryptDecrypt_ReturnsOriginalData;
var
  Key: TBytes;
  PlainText, CipherText, DecryptedText: TBytes;
begin
  // Arrange
  Key := GenerateKey(256);
  PlainText := 'Hello World';
  
  // Act
  CipherText := AES256_Encrypt(PlainText, Key);
  DecryptedText := AES256_Decrypt(CipherText, Key);
  
  // Assert
  Test('AES256 encrypt/decrypt', DecryptedText = PlainText);
end;

// ===== 第2步：实现（绿） =====
// 在 src/crypto.pas 中实现 AES256_Encrypt 和 AES256_Decrypt

// ===== 第3步：重构（改进） =====
// 提取公共代码，改进命名，优化性能

// ===== 第4步：文档（记录） =====
// 更新 README.md，添加使用示例
```

---

### 🎯 TDD 成功的关键

1. **纪律** - 严格遵守红-绿-重构循环
2. **简单** - 测试要简单清晰
3. **快速** - 测试要快速运行
4. **独立** - 测试之间不依赖
5. **可重复** - 每次运行结果一致
6. **自验证** - 自动判断通过/失败
7. **及时** - 立即运行测试

**记住**：测试不是负担，是保护伞。好的测试让你自信前进，而不是小心翼翼。

---

### 有效模式

#### 测试驱动
设计 → 实现 → 测试 → 文档

#### 问题驱动
发现 → 重现 → 诊断 → 解决 → 预防

#### 渐进式开发
- 将大任务分为3-5个小批次
- 每批完成后总结
- 用**"继续"**保持上下文

---

### 实用技巧

#### AI擅长
- ✅ 编写重复性代码（测试、示例）
- ✅ 生成结构化文档
- ✅ 分析大量代码
- ✅ 制定系统化方案

#### 人类擅长
- ✅ 战略决策
- ✅ 优先级判断
- ✅ 创造性思维
- ✅ 质量把关

#### 加速协作
- **"继续"** - 保持当前方向
- **"按最佳实践"** - AI选择最优方案
- **明确反馈** - "好，继续下一个"

---

### 常见陷阱

❌ **过度依赖** - 不加思考接受所有建议  
✅ 理解推理 + 验证决策 + 批判思维

❌ **目标不清** - "帮我看看这个项目"  
✅ 明确目标 + 成功标准 + 分阶段

❌ **忽视测试** - 只写代码不写测试  
✅ 测试驱动 + 每功能有测试

❌ **缺乏文档** - 只写代码不写文档  
✅ 同步文档 + 记录决策 + 提供示例

---

### 核心原则

1. **目标驱动** - 始终知道要达到什么
2. **系统方法** - 用结构化方式解决问题
3. **深度分析** - 找根本原因，不是表象
4. **完整文档** - 记录过程和决策
5. **持续改进** - 反思和优化方式
6. **价值导向** - 聚焦真正重要的事
7. **质量保证** - 测试验证所有产出

**记住**：Warp AI是工具，你是掌舵者。  
最佳协作 = AI能力 × 人类智慧

---

## 📚 项目特定指导

每个项目的详细技术文档请查看：

### fafafa.webview
- `README.md` - 项目概述、快速开始
- `docs/architecture.md` - 架构设计详解
- `docs/DEVELOPMENT.md` - 开发指南
- `examples/` - 丰富的代码示例

### fafafa.ssl
- `README.md` - 项目概述
- `OPENSSL_MODULES.md` - OpenSSL 模块说明
- `OPENSSL3_COMPATIBILITY_STRATEGY.md` - 兼容性策略
- `tests/` - 测试用例和报告

### 通用资源
- [FreePascal 文档](https://www.freepascal.org/docs.html)
- [Lazarus IDE](https://www.lazarus-ide.org/)
- 各项目 docs/ 目录

---

## ✅ 提交前检查清单

- [ ] 代码遵循命名规范（L开头局部变量，a开头参数）
- [ ] Windows平台添加 `{$CODEPAGE UTF8}`（如需输出中文）
- [ ] 编写单元测试
- [ ] 编写使用示例
- [ ] bin/lib目录配置正确
- [ ] 代码有适当注释
- [ ] 所有测试通过
- [ ] 文档已更新
- [ ] 提交信息格式正确

---

## 更新历史

- **2025-10-02**: 添加 TDD 开发规范 - 防止上下文抖动和项目推进困难
- **2025-10-02**: 精简版本 - 移除项目特定内容，保留核心规范
- **2025-10-02**: 整合 fafafa.webview 和 fafafa.ssl 规范
- **2025-09-30**: 添加 Warp AI 协作范式
- **2025-09-29**: 添加模块化和渐进式开发实践

---

**本文档专注于核心规范。详细技术内容请查看各项目的 README 和 docs/ 目录。**
