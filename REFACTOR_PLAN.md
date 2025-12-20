# 严格按命名风格重构计划

## 命名风格规范

```
fafafa.模块名.pas                    ← 模块主入口
fafafa.模块名.base.pas               ← 模块基础定义（类型+接口）
fafafa.模块名.子模块名.pas           ← 子模块实现
fafafa.模块名.子模块名.base.pas      ← 子模块基础定义
```

---

## 📋 当前文件分析

### ✅ 已符合规范的文件

#### 核心模块
```
fafafa.ssl.pas                    ✅ 主入口
fafafa.ssl.factory.pas            ✅ 工厂函数
fafafa.ssl.log.pas                ✅ 日志系统
fafafa.ssl.utils.pas              ✅ 工具函数
fafafa.ssl.ringbuffer.pas         ✅ 环形缓冲区
fafafa.ssl.certchain.pas          ✅ 证书链验证
```

#### OpenSSL 子模块
```
fafafa.ssl.openssl.lib.pas        ✅ 库管理
fafafa.ssl.openssl.context.pas    ✅ SSL 上下文
fafafa.ssl.openssl.connection.pas ✅ SSL 连接
fafafa.ssl.openssl.certificate.pas✅ 证书
fafafa.ssl.openssl.certstore.pas  ✅ 证书存储
fafafa.ssl.openssl.session.pas    ✅ 会话管理
```

#### OpenSSL API 子模块
```
fafafa.ssl.openssl.api.core.pas   ✅ 核心 API
fafafa.ssl.openssl.api.ssl.pas    ✅ SSL API
fafafa.ssl.openssl.api.x509.pas   ✅ X509 API
fafafa.ssl.openssl.api.bio.pas    ✅ BIO API
fafafa.ssl.openssl.api.evp.pas    ✅ EVP API
fafafa.ssl.openssl.api.err.pas    ✅ 错误 API
fafafa.ssl.openssl.api.bn.pas     ✅ 大数 API
fafafa.ssl.openssl.api.asn1.pas   ✅ ASN1 API
fafafa.ssl.openssl.api.*.pas      ✅ 其他 API 文件（共 60+ 个）
```

#### WinSSL 子模块
```
fafafa.ssl.winssl.lib.pas         ✅ 库管理
fafafa.ssl.winssl.context.pas     ✅ 上下文
fafafa.ssl.winssl.connection.pas  ✅ 连接
fafafa.ssl.winssl.certificate.pas ✅ 证书
fafafa.ssl.winssl.certstore.pas   ✅ 证书存储
fafafa.ssl.winssl.api.pas         ✅ API
fafafa.ssl.winssl.errors.pas      ✅ 错误处理
fafafa.ssl.winssl.utils.pas       ✅ 工具函数
fafafa.ssl.winssl.enterprise.pas  ✅ 企业功能
fafafa.ssl.winssl.optimized.pas   ✅ 优化版本
```

### ❌ 需要处理的文件

#### 1. 转发层（需删除）
```
fafafa.ssl.abstract.types.pas     ❌ 抽象类型层（合并到 base.pas）
fafafa.ssl.abstract.intf.pas      ❌ 抽象接口层（合并到 base.pas）
fafafa.ssl.types.pas              ❌ 转发层（删除）
fafafa.ssl.intf.pas               ❌ 转发层（删除）
```

#### 2. 定位不清（需决策）
```
fafafa.ssl.openssl.pas            ⚠️  整合层？（可能是转发，建议删除或重命名）
fafafa.ssl.openssl.types.pas      ⚠️  OpenSSL 类型（考虑重命名为 base.pas）
fafafa.ssl.winssl.pas             ⚠️  整合层？（可能是转发，建议删除或重命名）
fafafa.ssl.winssl.types.pas       ⚠️  WinSSL 类型（考虑重命名为 base.pas）
fafafa.ssl.openssl.api.pas        ⚠️  API 整合？（可能是转发）
```

#### 3. 废弃文件（需删除）
```
fafafa.ssl.openssl.certstore_new.pas  ❌ 临时文件
*.bak                                  ❌ 备份文件
*_new.pas                              ❌ 临时文件
*_old.pas                              ❌ 临时文件
*.o, *.ppu                             ❌ 编译产物
```

---

## 🔧 重构步骤

### 阶段 1: 清理废弃文件

```bash
cd src

# 删除临时和备份文件
rm -f *.bak *_new.pas *_old.pas *.o *.ppu

# 确认删除
ls *.bak *_new.pas *_old.pas 2>/dev/null || echo "✓ 清理完成"
```

### 阶段 2: 合并核心基础文件

#### 2.1 创建 fafafa.ssl.base.pas

合并以下文件内容：
- `fafafa.ssl.abstract.types.pas` → 类型定义
- `fafafa.ssl.abstract.intf.pas` → 接口定义

```pascal
unit fafafa.ssl.base;

{$mode objfpc}{$H+}{$J-}

interface

{ ============================================================================ }
{ 类型定义 }
{ ============================================================================ }

type
  // SSL/TLS 版本
  TSSLVersion = (
    sslv2, sslv3, 
    tlsv1, tlsv1_1, tlsv1_2, tlsv1_3,
    sslDefault
  );

  // SSL 上下文类型
  TSSLContextType = (
    sslCtxClient,
    sslCtxServer,
    sslCtxClientServer
  );

  // ... 其他类型定义 ...

{ ============================================================================ }
{ 接口定义 }
{ ============================================================================ }

type
  // 前向声明
  ISSLContext = interface;
  ISSLConnection = interface;
  ISSLCertificate = interface;
  ISSLCertificateStore = interface;
  ISSLSession = interface;

  // SSL 库接口
  ISSLLibrary = interface
    ['{GUID-HERE}']
    function Initialize: Boolean;
    function Finalize: Boolean;
    function CreateContext(aType: TSSLContextType): ISSLContext;
    // ...
  end;

  // SSL 上下文接口
  ISSLContext = interface
    ['{GUID-HERE}']
    function SetMinProtocolVersion(aVersion: TSSLVersion): Boolean;
    function SetMaxProtocolVersion(aVersion: TSSLVersion): Boolean;
    // ...
  end;

  // ... 其他接口定义 ...

implementation

end.
```

#### 2.2 删除冗余文件

```bash
cd src
rm -f fafafa.ssl.abstract.types.pas
rm -f fafafa.ssl.abstract.intf.pas
rm -f fafafa.ssl.types.pas
rm -f fafafa.ssl.intf.pas
```

### 阶段 3: 决策子模块 base.pas

#### 3.1 检查 fafafa.ssl.openssl.types.pas

```bash
# 查看文件内容
head -50 src/fafafa.ssl.openssl.types.pas

# 如果包含接口定义，重命名为 base.pas
# 如果只有类型定义，保持 types.pas
```

**决策**:
- 如果既有类型又有接口 → 重命名为 `fafafa.ssl.openssl.base.pas`
- 如果只有类型 → 保持 `fafafa.ssl.openssl.types.pas`

#### 3.2 检查 fafafa.ssl.winssl.types.pas

同样的决策逻辑。

#### 3.3 检查整合层文件

```bash
# 检查这些文件是否只是转发
wc -l src/fafafa.ssl.openssl.pas
wc -l src/fafafa.ssl.winssl.pas
wc -l src/fafafa.ssl.openssl.api.pas

# 如果只是转发（< 50 行），建议删除
# 如果有实际实现，保留
```

### 阶段 4: 全局替换 uses 子句

```bash
cd src

# 替换核心基础引用
sed -i 's/fafafa\.ssl\.abstract\.types/fafafa.ssl.base/g' *.pas
sed -i 's/fafafa\.ssl\.abstract\.intf/fafafa.ssl.base/g' *.pas
sed -i 's/fafafa\.ssl\.types,/fafafa.ssl.base,/g' *.pas
sed -i 's/fafafa\.ssl\.intf,/fafafa.ssl.base,/g' *.pas

# 如果重命名了子模块 base.pas，相应替换
# sed -i 's/fafafa\.ssl\.openssl\.types/fafafa.ssl.openssl.base/g' *.pas
# sed -i 's/fafafa\.ssl\.winssl\.types/fafafa.ssl.winssl.base/g' *.pas
```

### 阶段 5: 更新主入口文件

`fafafa.ssl.pas` 应该重新导出：

```pascal
unit fafafa.ssl;

{$mode objfpc}{$H+}{$J-}

interface

uses
  fafafa.ssl.base,      // 所有类型和接口
  fafafa.ssl.factory;   // 工厂函数

// 重新导出基础定义
type
  TSSLVersion = fafafa.ssl.base.TSSLVersion;
  TSSLContextType = fafafa.ssl.base.TSSLContextType;
  // ... 其他类型 ...

  ISSLLibrary = fafafa.ssl.base.ISSLLibrary;
  ISSLContext = fafafa.ssl.base.ISSLContext;
  // ... 其他接口 ...

// 重新导出工厂函数
function CreateOpenSSLLibrary: ISSLLibrary;
function CreateWinSSLLibrary: ISSLLibrary;

implementation

function CreateOpenSSLLibrary: ISSLLibrary;
begin
  Result := fafafa.ssl.factory.CreateOpenSSLLibrary;
end;

function CreateWinSSLLibrary: ISSLLibrary;
begin
  Result := fafafa.ssl.factory.CreateWinSSLLibrary;
end;

end.
```

### 阶段 6: 编译测试

```bash
cd tests

# 编译所有测试
lazbuild test_basic.lpi
lazbuild test_context.lpi
lazbuild test_certificate.lpi
lazbuild test_real_usage.lpi

# 运行测试
./test_basic
./test_context
./test_certificate
./test_real_usage
```

---

## 📁 重构后的最终结构

```
src/
├── fafafa.ssl.pas                    ← 主入口（重新导出）
├── fafafa.ssl.base.pas               ← 基础定义（类型+接口）★ 新创建
├── fafafa.ssl.factory.pas            ← 工厂函数
├── fafafa.ssl.log.pas                ← 日志系统
├── fafafa.ssl.utils.pas              ← 工具函数
├── fafafa.ssl.ringbuffer.pas         ← 环形缓冲区
├── fafafa.ssl.certchain.pas          ← 证书链验证
│
├── fafafa.ssl.openssl.pas            ← OpenSSL 整合（可选）⚠️ 待决策
├── fafafa.ssl.openssl.types.pas      ← OpenSSL 类型 或
├── fafafa.ssl.openssl.base.pas       ← OpenSSL 基础 ⚠️ 待决策
├── fafafa.ssl.openssl.lib.pas        ← 库管理
├── fafafa.ssl.openssl.context.pas    ← 上下文
├── fafafa.ssl.openssl.connection.pas ← 连接
├── fafafa.ssl.openssl.certificate.pas← 证书
├── fafafa.ssl.openssl.certstore.pas  ← 证书存储
├── fafafa.ssl.openssl.session.pas    ← 会话管理
│
├── fafafa.ssl.openssl.api.pas        ← API 整合（可选）⚠️ 待决策
├── fafafa.ssl.openssl.api.core.pas   ← 核心 API
├── fafafa.ssl.openssl.api.ssl.pas    ← SSL API
├── fafafa.ssl.openssl.api.x509.pas   ← X509 API
├── fafafa.ssl.openssl.api.bio.pas    ← BIO API
├── fafafa.ssl.openssl.api.evp.pas    ← EVP API
├── ... (60+ API 文件)
│
├── fafafa.ssl.winssl.pas             ← WinSSL 整合（可选）⚠️ 待决策
├── fafafa.ssl.winssl.types.pas       ← WinSSL 类型 或
├── fafafa.ssl.winssl.base.pas        ← WinSSL 基础 ⚠️ 待决策
├── fafafa.ssl.winssl.lib.pas         ← 库管理
├── fafafa.ssl.winssl.context.pas     ← 上下文
├── fafafa.ssl.winssl.connection.pas  ← 连接
├── fafafa.ssl.winssl.certificate.pas ← 证书
├── fafafa.ssl.winssl.certstore.pas   ← 证书存储
├── fafafa.ssl.winssl.api.pas         ← API
├── fafafa.ssl.winssl.errors.pas      ← 错误处理
├── fafafa.ssl.winssl.utils.pas       ← 工具函数
├── fafafa.ssl.winssl.enterprise.pas  ← 企业功能
└── fafafa.ssl.winssl.optimized.pas   ← 优化版本
```

---

## ⚠️ 需要人工决策的问题

### 问题 1: fafafa.ssl.openssl.pas 作用？

```bash
# 检查文件
cat src/fafafa.ssl.openssl.pas
```

**选项**:
- A. 如果只是转发/整合 → **删除**
- B. 如果有实际实现 → **保留**
- C. 如果是入口点 → **重命名文档说明**

### 问题 2: fafafa.ssl.openssl.types.pas 内容？

```bash
# 检查是否包含接口
grep -n "interface\[" src/fafafa.ssl.openssl.types.pas
```

**选项**:
- A. 只有类型 → 保持 `fafafa.ssl.openssl.types.pas`
- B. 有类型+接口 → 重命名为 `fafafa.ssl.openssl.base.pas`

### 问题 3: fafafa.ssl.openssl.api.pas 作用？

```bash
# 检查文件
cat src/fafafa.ssl.openssl.api.pas
```

**选项**:
- A. 如果是整合所有 API → **可保留，但需文档说明**
- B. 如果只是转发 → **删除**

---

## 📝 执行清单

- [ ] 阶段 1: 清理废弃文件（*.bak, *_new, *_old）
- [ ] 阶段 2: 合并核心基础文件
  - [ ] 创建 `fafafa.ssl.base.pas`
  - [ ] 删除 `fafafa.ssl.abstract.types.pas`
  - [ ] 删除 `fafafa.ssl.abstract.intf.pas`
  - [ ] 删除 `fafafa.ssl.types.pas`
  - [ ] 删除 `fafafa.ssl.intf.pas`
- [ ] 阶段 3: 决策子模块
  - [ ] 检查 `fafafa.ssl.openssl.pas`
  - [ ] 检查 `fafafa.ssl.openssl.types.pas`
  - [ ] 检查 `fafafa.ssl.openssl.api.pas`
  - [ ] 检查 `fafafa.ssl.winssl.pas`
  - [ ] 检查 `fafafa.ssl.winssl.types.pas`
- [ ] 阶段 4: 全局替换 uses 子句
- [ ] 阶段 5: 更新主入口文件
- [ ] 阶段 6: 编译测试
- [ ] 阶段 7: 运行测试验证

---

## 开始执行？

我已准备好按此计划执行重构。确认开始吗？



