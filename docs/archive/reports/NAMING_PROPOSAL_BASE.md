# 命名方案：合并为 fafafa.ssl.base.pas

## 提议
将 `types.pas` 和 `interfaces.pas` 合并为 **`fafafa.ssl.base.pas`**

---

## 优点分析

### ✅ 优点 1: 极简设计
```pascal
// 用户只需要一个单元
uses
  fafafa.ssl.base;  // ← 包含所有基础定义（类型+接口）
```

### ✅ 优点 2: 符合 Pascal 传统
```pascal
// RTL 的传统做法
uses
  Classes;   // ← 类型、接口、基础类全在一起
  SysUtils;  // ← 类型、函数、异常类全在一起
  
// 我们的库
uses
  fafafa.ssl.base;  // ← 类型、接口全在一起
```

### ✅ 优点 3: 类型和接口天然相关
```pascal
// 在 base.pas 中
type
  // 类型定义
  TSSLProtocolVersion = (...);
  
  // 使用这些类型的接口
  ISSLContext = interface
    procedure SetProtocolVersions(aVersions: TSSLProtocolVersions);
  end;
```

它们总是一起使用，分开反而不方便。

### ✅ 优点 4: 减少文件数量
```
改进前:
├── fafafa.ssl.abstract.types.pas
├── fafafa.ssl.abstract.intf.pas
├── fafafa.ssl.types.pas        (转发层)
├── fafafa.ssl.intf.pas         (转发层)
└── fafafa.ssl.pas              (主入口)
= 5 个文件

改进后:
├── fafafa.ssl.base.pas         (类型+接口)
└── fafafa.ssl.pas              (主入口)
= 2 个文件  ✅
```

### ✅ 优点 5: 命名更清晰
```
fafafa.ssl.base.pas      ← 一看就知道是"基础定义"
vs
fafafa.ssl.types.pas     ← "类型"
fafafa.ssl.interfaces.pas ← "接口"
```

`base` 更能表达"这是所有东西的基础"。

---

## 潜在顾虑（及解答）

### ❓ 顾虑 1: 文件会不会太大？
**解答**: 不会

```bash
# 两个文件合计约 800-1000 行
# 这在 Pascal 项目中很常见

对比:
- RTL 的 Classes.pas: ~18000 行
- RTL 的 SysUtils.pas: ~5000 行
- 我们的 base.pas: ~1000 行  ✅ 完全可接受
```

### ❓ 顾虑 2: 违反单一职责原则？
**解答**: 不违反

```
单一职责: "提供 fafafa.ssl 的基础定义"
- 类型是基础定义的一部分
- 接口也是基础定义的一部分

它们共同构成一个职责："基础抽象层"
```

### ❓ 顾虑 3: 如果只需要类型不需要接口呢？
**解答**: 这种情况极少

```pascal
// 实际使用中，类型和接口总是一起用的
var
  Ctx: ISSLContext;  // ← 接口
  Ver: TSSLProtocolVersion;  // ← 类型
```

即使真的只需要类型，引入接口定义也没有运行时开销（接口是编译时概念）。

---

## 推荐的最终结构

### 核心文件
```
fafafa.ssl.base.pas           ← 基础定义（类型+接口）
fafafa.ssl.factory.pas        ← 工厂类（创建库实例）
fafafa.ssl.pas                ← 主入口（导出 base + factory + 辅助函数）
```

### 后端实现
```
OpenSSL 后端:
├── fafafa.ssl.openssl.types.pas      ← OpenSSL 特定类型
├── fafafa.ssl.openssl.lib.pas        ← 库管理（实现 ISSLLibrary）
├── fafafa.ssl.openssl.context.pas    ← 上下文（实现 ISSLContext）
├── fafafa.ssl.openssl.connection.pas ← 连接（实现 ISSLConnection）
├── fafafa.ssl.openssl.certificate.pas
├── fafafa.ssl.openssl.certstore.pas
├── fafafa.ssl.openssl.session.pas
└── fafafa.ssl.openssl.api.*.pas      ← API 绑定

WinSSL 后端:
├── fafafa.ssl.winssl.types.pas
├── fafafa.ssl.winssl.lib.pas
├── fafafa.ssl.winssl.context.pas
└── ...

工具类:
├── fafafa.ssl.log.pas
├── fafafa.ssl.utils.pas
└── fafafa.ssl.ringbuffer.pas
```

---

## 用户使用示例

### 场景 1: 简单使用（推荐）
```pascal
uses
  fafafa.ssl;  // ← 一个就够了！导出了 base + factory

var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
begin
  Lib := CreateOpenSSLLibrary;
  Ctx := Lib.CreateContext(sslCtxClient);
end;
```

### 场景 2: 只需要基础定义（不需要实现）
```pascal
uses
  fafafa.ssl.base;  // ← 只有类型和接口定义

// 适用于：
// - 写接口层代码
// - 写抽象工具函数
// - 声明变量但不创建实例
```

### 场景 3: 直接使用 OpenSSL 后端
```pascal
uses
  fafafa.ssl.base,           // ← 基础定义
  fafafa.ssl.openssl.lib;    // ← OpenSSL 实现

var
  Lib: ISSLLibrary;
begin
  Lib := CreateOpenSSLLibrary;
end;
```

### 场景 4: 实现新后端
```pascal
unit fafafa.ssl.mybackend.lib;

uses
  fafafa.ssl.base;  // ← 只需要这一个！

type
  TMyBackendLibrary = class(TInterfacedObject, ISSLLibrary)
    // 实现 ISSLLibrary 接口
  end;
```

---

## 文件内容组织

### fafafa.ssl.base.pas 结构
```pascal
unit fafafa.ssl.base;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes;

// ============================================================================
// Part 1: 基础类型定义
// ============================================================================

type
  // 回调类型
  TSSLProc = procedure of object;
  TSSLLogCallback = procedure(aLevel: TSSLLogLevel; const aMessage: string) of object;
  
  // 枚举类型
  TSSLLibraryType = (sslOpenSSL, sslWinSSL, sslAutoDetect);
  TSSLProtocolVersion = (sslProtocolSSL3, sslProtocolTLS10, ...);
  TSSLVerifyMode = (sslVerifyNone, sslVerifyPeer, ...);
  
  // 结构类型
  TSSLConfig = record
    // ...
  end;
  
  TSSLStatistics = record
    // ...
  end;

// ============================================================================
// Part 2: 接口定义
// ============================================================================

type
  // 前向声明
  ISSLLibrary = interface;
  ISSLContext = interface;
  ISSLConnection = interface;
  ISSLCertificate = interface;
  ISSLCertificateStore = interface;
  ISSLSession = interface;
  
  // 接口实现
  ISSLLibrary = interface
    ['{...}']
    // ...
  end;
  
  ISSLContext = interface
    ['{...}']
    // ...
  end;
  
  // ... 其他接口

// ============================================================================
// Part 3: 辅助类型
// ============================================================================

type
  TSSLCertificateArray = array of ISSLCertificate;
  TSSLContextArray = array of ISSLContext;

// ============================================================================
// Part 4: 辅助函数声明
// ============================================================================

function SSLErrorToString(aError: TSSLErrorCode): string;
function ProtocolVersionToString(aVersion: TSSLProtocolVersion): string;
function LibraryTypeToString(aLibType: TSSLLibraryType): string;

implementation

// ============================================================================
// 辅助函数实现
// ============================================================================

function SSLErrorToString(aError: TSSLErrorCode): string;
begin
  // ...
end;

// ...

end.
```

---

## 迁移步骤

### 步骤 1: 创建 base.pas（合并内容）
```bash
cd src

# 创建新文件
cat > fafafa.ssl.base.pas << 'EOF'
{
  fafafa.ssl.base - SSL/TLS 基础定义
  
  包含:
  - 所有类型定义（枚举、记录、集合等）
  - 所有接口定义（ISSLLibrary, ISSLContext 等）
  - 辅助函数
}

unit fafafa.ssl.base;
// ... (合并 abstract.types 和 abstract.intf 的内容)
EOF
```

### 步骤 2: 更新主入口文件
```pascal
// fafafa.ssl.pas
unit fafafa.ssl;

interface

uses
  fafafa.ssl.base,     // ← 基础定义
  fafafa.ssl.factory;  // ← 工厂函数

// 重新导出所有内容
type
  // 从 base 导出
  TSSLLibraryType = fafafa.ssl.base.TSSLLibraryType;
  ISSLLibrary = fafafa.ssl.base.ISSLLibrary;
  // ...

// 从 factory 导出
function CreateOpenSSLLibrary: ISSLLibrary;
function CreateWinSSLLibrary: ISSLLibrary;
```

### 步骤 3: 删除旧文件
```bash
# 删除被合并的文件
rm fafafa.ssl.abstract.types.pas
rm fafafa.ssl.abstract.intf.pas

# 删除转发层
rm fafafa.ssl.types.pas
rm fafafa.ssl.intf.pas

# 删除废弃文件
rm -f *.bak *_new.pas *_old.pas
```

### 步骤 4: 全局替换 uses 子句
```bash
# 在所有 .pas 文件中替换
fafafa.ssl.abstract.types  → fafafa.ssl.base
fafafa.ssl.abstract.intf   → fafafa.ssl.base
fafafa.ssl.types           → fafafa.ssl.base
fafafa.ssl.intf            → fafafa.ssl.base
```

---

## 其他 Pascal 库的参考

### Free Pascal RTL
```
Classes.pas        ← 类型 + 类 + 接口都在一起
SysUtils.pas       ← 类型 + 函数 + 异常都在一起
```

### Indy (Internet Direct)
```
IdGlobal.pas       ← 全局类型和常量
IdTypes.pas        ← 基础类型定义
```

### Synapse
```
blcksock.pas       ← 基础类型 + 主要类都在一起
```

### 我们的设计
```
fafafa.ssl.base.pas  ← 基础类型 + 接口都在一起  ✅ 符合传统
```

---

## 对比：三种方案

### 方案 1: 分离（不推荐）
```
fafafa.ssl.types.pas
fafafa.ssl.interfaces.pas

缺点: 总是一起用，分开没必要
```

### 方案 2: 保留 abstract 前缀（不推荐）
```
fafafa.ssl.abstract.types.pas
fafafa.ssl.abstract.intf.pas

缺点: "abstract" 让新手困惑
```

### 方案 3: 合并为 base.pas（✅ 推荐）
```
fafafa.ssl.base.pas

优点:
✅ 简洁（一个文件）
✅ 清晰（base = 基础）
✅ 实用（类型和接口总是一起用）
✅ 符合 Pascal 传统
```

---

## 结论

### ✅ 强烈推荐 `fafafa.ssl.base.pas`

**理由**:
1. **用户友好**: 只需 `uses fafafa.ssl.base` 就有所有基础定义
2. **符合传统**: 与 RTL 的设计理念一致
3. **实用性强**: 类型和接口总是一起使用
4. **命名直观**: "base" 一看就知道是基础
5. **维护简单**: 减少文件数量，减少依赖关系

**文件数量对比**:
```
当前: 5 个文件（abstract.types, abstract.intf, types, intf, main）
改进: 2 个文件（base, main）
```

**这是最优方案！** 🎯



