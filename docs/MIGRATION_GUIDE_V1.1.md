# fafafa.ssl v1.1 迁移指南

**版本**: v1.0.0 → v1.1.0
**发布日期**: 2026-02-05
**重要性**: 低（仅影响自定义后端开发者）

---

## 概述

v1.1.0 引入了架构改进，将 `GetNativeHandle` 方法从核心接口移至可选接口 `ISSLNativeHandleAccess`。此变更旨在：

1. **清晰化抽象层** - 核心接口不再暴露 C 库特定实现细节
2. **支持纯 Pascal 后端** - 为未来的 FreePascal 原生 TLS 实现铺平道路
3. **提升类型安全** - 通过接口查询机制防止类型错误

**向后兼容性**: ✅ 对于标准用户代码，此变更完全向后兼容，无需任何修改。

---

## 谁会受到影响？

### ✅ 不受影响（99% 用户）

如果您的代码仅使用以下方式：

```pascal
// 标准用法 - 完全不受影响
Lib := TSSLFactory.CreateLibrary(sslOpenSSL);
Ctx := Lib.CreateContext(sslCtxClient);
Conn := Ctx.CreateConnection(Socket);
Cert := Lib.CreateCertificate;

// 使用接口方法 - 完全不受影响
Ctx.SetProtocolVersions(sslProtocolTLS12, sslProtocolTLS13);
Conn.Connect;
Cert.LoadFromFile('cert.pem');
```

**您无需任何修改！** 继续使用即可。

### ⚠️ 可能受影响（1% 高级用户）

如果您的代码符合以下情况之一：

1. **实现了自定义 TLS 后端**
2. **直接调用了 `GetNativeHandle` 方法**
3. **需要访问底层 C 库句柄**（如 OpenSSL `SSL*` 指针）

请继续阅读迁移步骤。

---

## 核心变更

### 变更前（v1.0.0）

```pascal
// GetNativeHandle 在核心接口中
ISSLContext = interface
  function GetNativeHandle: Pointer;  // 所有后端都必须实现
  // ... 其他方法
end;

ISSLConnection = interface
  function GetNativeHandle: Pointer;
  // ... 其他方法
end;

// 使用方式
Ctx := Factory.CreateContext(...);
SSL_CTX := PSSL_CTX(Ctx.GetNativeHandle);  // 直接调用
```

### 变更后（v1.1.0）

```pascal
// GetNativeHandle 移至可选接口
ISSLContext = interface
  // GetNativeHandle 已移除
  // ... 其他方法（不变）
end;

// 新增可选接口
ISSLNativeHandleAccess = interface
  ['{B2C4E6F8-1A2B-3C4D-5E6F-7A8B9C0D1E2F}']
  function GetNativeHandle: Pointer;
  function GetBackendType: TSSLLibraryType;
  function IsNativeHandleValid: Boolean;
end;

// 使用方式 - 需要接口查询
Ctx := Factory.CreateContext(...);
if Supports(Ctx, ISSLNativeHandleAccess, NativeAccess) then
  SSL_CTX := PSSL_CTX(NativeAccess.GetNativeHandle);
```

---

## 迁移步骤

### 场景 1：标准应用代码（99% 用户）

**无需任何修改！** 您的代码继续正常工作。

### 场景 2：访问原生句柄（高级用户）

#### 2.1 简单迁移（推荐）

使用后端提供的辅助函数：

**之前**:
```pascal
uses
  fafafa.ssl.base,
  fafafa.ssl.openssl.api.ssl;

var
  Ctx: ISSLContext;
  SSL_CTX: PSSL_CTX;
begin
  Ctx := Factory.CreateContext(sslCtxClient, sslOpenSSL);
  SSL_CTX := PSSL_CTX(Ctx.GetNativeHandle);  // 旧方式
  // 使用 SSL_CTX
end;
```

**之后**:
```pascal
uses
  fafafa.ssl.base,
  fafafa.ssl.openssl.api.ssl,
  fafafa.ssl.openssl.native_handle;  // 新增

var
  Ctx: ISSLContext;
  SSL_CTX: PSSL_CTX;
begin
  Ctx := Factory.CreateContext(sslCtxClient, sslOpenSSL);

  // 方式 1: 使用辅助函数（推荐）
  SSL_CTX := PSSL_CTX(GetNativeHandleSafe(Ctx, 'MyCode.DoSomething'));

  // 方式 2: 可选检查
  if TryGetNativeHandle(Ctx, Pointer(SSL_CTX)) then
    // 使用 SSL_CTX
  else
    WriteLn('This backend does not provide native handles');
end;
```

#### 2.2 完整迁移（最佳实践）

手动进行接口查询：

```pascal
uses
  fafafa.ssl.base;

var
  Ctx: ISSLContext;
  NativeAccess: ISSLNativeHandleAccess;
  SSL_CTX: PSSL_CTX;
begin
  Ctx := Factory.CreateContext(sslCtxClient, sslOpenSSL);

  // 检查是否支持原生句柄访问
  if not Supports(Ctx, ISSLNativeHandleAccess, NativeAccess) then
  begin
    WriteLn('This backend does not support native handle access');
    Exit;
  end;

  // 检查句柄是否有效
  if not NativeAccess.IsNativeHandleValid then
  begin
    WriteLn('Native handle is not valid');
    Exit;
  end;

  // 获取原生句柄
  SSL_CTX := PSSL_CTX(NativeAccess.GetNativeHandle);

  // 现在可以安全使用 SSL_CTX
  WriteLn('Backend type: ', GetEnumName(TypeInfo(TSSLLibraryType),
                                         Ord(NativeAccess.GetBackendType)));
end;
```

### 场景 3：实现自定义后端

如果您实现了自定义 TLS 后端，需要决定是否支持原生句柄访问。

#### 3.1 基于 C 库的后端（需要实现）

```pascal
type
  TMyCustomSSLContext = class(TInterfacedObject, ISSLContext, ISSLNativeHandleAccess)
  private
    FNativeCtx: Pointer;  // 您的 C 库上下文
  public
    // ISSLContext 方法（保持不变）
    // ...

    // ISSLNativeHandleAccess 方法（新增）
    function GetNativeHandle: Pointer;
    function GetBackendType: TSSLLibraryType;
    function IsNativeHandleValid: Boolean;
  end;

function TMyCustomSSLContext.GetNativeHandle: Pointer;
begin
  Result := FNativeCtx;
end;

function TMyCustomSSLContext.GetBackendType: TSSLLibraryType;
begin
  Result := sslCustom;  // 或您的后端类型
end;

function TMyCustomSSLContext.IsNativeHandleValid: Boolean;
begin
  Result := (FNativeCtx <> nil);
end;
```

#### 3.2 纯 Pascal 后端（无需实现）

```pascal
type
  TPureFreePascalSSLContext = class(TInterfacedObject, ISSLContext)
    // ✅ 无需实现 ISSLNativeHandleAccess
    // ✅ 无需返回假的 nil 句柄
    // ✅ 架构完全清晰
  private
    FConfig: TPascalTLSConfig;  // 纯 Pascal 数据结构
  public
    // 仅实现 ISSLContext 方法
    // ...
  end;
```

---

## 辅助函数参考

每个后端提供了两个辅助函数用于安全访问原生句柄：

### GetNativeHandleSafe

**签名**:
```pascal
function GetNativeHandleSafe(const AObject: IInterface;
                              const AContextMsg: string): Pointer;
```

**说明**:
- 安全获取原生句柄
- 如果对象不支持 `ISSLNativeHandleAccess`，抛出异常
- 如果句柄为 `nil`，抛出异常
- `AContextMsg` 用于提供错误上下文（如 `'MyClass.MyMethod'`）

**示例**:
```pascal
uses fafafa.ssl.openssl.native_handle;

SSL_CTX := PSSL_CTX(GetNativeHandleSafe(Ctx, 'TMyApp.Initialize'));
```

### TryGetNativeHandle

**签名**:
```pascal
function TryGetNativeHandle(const AObject: IInterface;
                             out AHandle: Pointer): Boolean;
```

**说明**:
- 尝试获取原生句柄
- 返回 `True` 如果成功，`False` 如果对象不支持或句柄为 `nil`
- 不抛出异常

**示例**:
```pascal
uses fafafa.ssl.openssl.native_handle;

if TryGetNativeHandle(Ctx, Pointer(SSL_CTX)) then
  // 使用 SSL_CTX
else
  WriteLn('Native handle not available');
```

### 可用的辅助单元

| 后端 | 辅助单元 |
|------|---------|
| OpenSSL | `fafafa.ssl.openssl.native_handle` |
| WinSSL | `fafafa.ssl.winssl.native_handle` |
| MbedTLS | `fafafa.ssl.mbedtls.native_handle` |
| WolfSSL | `fafafa.ssl.wolfssl.native_handle` |

---

## ISSLNativeHandleAccess 接口参考

```pascal
ISSLNativeHandleAccess = interface
  ['{B2C4E6F8-1A2B-3C4D-5E6F-7A8B9C0D1E2F}']

  {** 获取后端原生句柄（如 SSL*, CredHandle, mbedtls_ssl_context*）
      @returns 原生句柄指针，仅供高级用户或后端内部使用 *}
  function GetNativeHandle: Pointer;

  {** 获取后端类型
      @returns 后端类型枚举值 *}
  function GetBackendType: TSSLLibraryType;

  {** 检查原生句柄是否有效
      @returns True 如果句柄有效且可用 *}
  function IsNativeHandleValid: Boolean;
end;
```

---

## 常见问题 (FAQ)

### Q1: 为什么要做这个变更？

**A**: 主要原因：
1. **架构清晰** - 核心接口不应暴露特定实现细节
2. **支持纯 Pascal** - 为未来的 FreePascal 原生 TLS 后端铺平道路
3. **类型安全** - 通过接口查询防止运行时类型错误

### Q2: 这会破坏我的代码吗？

**A**: 对于 99% 的用户，**不会**。只有直接调用 `GetNativeHandle` 的高级用户需要迁移。

### Q3: 我该如何知道我的后端是否支持原生句柄？

**A**: 使用 `Supports` 函数检查：
```pascal
if Supports(Ctx, ISSLNativeHandleAccess) then
  WriteLn('Supports native handles')
else
  WriteLn('Pure Pascal backend');
```

### Q4: 性能有影响吗？

**A**: **没有**。`Supports` 接口查询是 FreePascal 编译器内置机制，开销可忽略（<1ns）。

### Q5: 我能同时使用支持和不支持原生句柄的后端吗？

**A**: **可以**。这正是此设计的优势。您的代码可以动态检查并适配：
```pascal
if Supports(Ctx, ISSLNativeHandleAccess, NativeAccess) then
  // 使用 C 库特定功能
else
  // 使用标准接口方法
```

### Q6: 未来的纯 Pascal 后端什么时候发布？

**A**: 这是一个长期项目（2-3 年）。参见长期路线图文档。

---

## 升级检查清单

- [ ] 检查代码中是否有直接调用 `GetNativeHandle` 的地方
- [ ] 如果有，决定迁移方式（辅助函数 vs 手动查询）
- [ ] 添加相应的 `uses` 子句（如 `fafafa.ssl.openssl.native_handle`）
- [ ] 更新调用代码
- [ ] 编译测试
- [ ] 运行测试套件
- [ ] 更新相关文档

---

## 获取帮助

如果您在迁移过程中遇到问题：

1. **查看示例**: `examples/` 目录中的示例已更新
2. **阅读测试**: `tests/` 目录展示了各种使用场景
3. **提交 Issue**: https://github.com/your-org/fafafa.ssl/issues
4. **查看完整报告**: `.claude/plans/refactoring-completion-report.md`

---

## 总结

v1.1.0 的架构改进为 fafafa.ssl 的长远发展奠定了基础。对于绝大多数用户，此变更是透明的；对于高级用户，迁移过程简单明了。

感谢您使用 fafafa.ssl！

---

**文档版本**: 1.0
**最后更新**: 2026-02-05
**作者**: fafafa.ssl 团队
