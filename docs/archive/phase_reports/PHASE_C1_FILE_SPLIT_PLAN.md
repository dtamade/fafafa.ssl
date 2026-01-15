# Phase C.1: 大文件拆分计划

**日期**: 2025-10-24  
**目标**: 将 `fafafa.ssl.openssl.pas` (3,261 行) 拆分成符合 WARP.md 规范的多个模块（每个 <1000 行）

## 当前状态分析

### 文件大小
- **`src/fafafa.ssl.openssl.pas`**: 3,261 行
- **问题**: 严重超过 WARP.md 规定的 1000 行限制

### 主要组成部分

| 类名 | 功能 | 估计行数 | 拆分优先级 |
|------|------|----------|------------|
| `TOpenSSLLibrary` | 库管理、初始化、配置 | ~400 | 高 |
| `TOpenSSLContext` | SSL 上下文、协议、密码 | ~500 | 高 |
| `TOpenSSLCertificate` | 证书加载、解析、验证 | ~900 | 高 |
| `TOpenSSLCertificateStore` | 证书存储管理 | ~300 | 中 |
| `TOpenSSLConnection` | SSL 连接、握手、I/O | ~700 | 高 |
| 工具函数 | 错误处理、转换、注册 | ~400 | 中 |

## 拆分策略

### 目标结构

```
src/
├── fafafa.ssl.openssl.pas           # 主入口模块（重新导出）
├── fafafa.ssl.openssl.lib.pas       # TOpenSSLLibrary
├── fafafa.ssl.openssl.context.pas   # TOpenSSLContext
├── fafafa.ssl.openssl.certificate.pas # TOpenSSLCertificate
├── fafafa.ssl.openssl.store.pas     # TOpenSSLCertificateStore
├── fafafa.ssl.openssl.connection.pas # TOpenSSLConnection
└── fafafa.ssl.openssl.utils.pas     # 工具函数
```

### 拆分原则
1. **按类拆分**: 每个类一个单独的文件
2. **保持接口**: 保持抽象接口实现不变
3. **最小依赖**: 减少模块间循环依赖
4. **向后兼容**: 主模块重新导出，确保现有代码不受影响

## 详细拆分计划

### 阶段 1: 创建新模块文件 ✅（当前）

#### 1.1 `fafafa.ssl.openssl.lib.pas`
**内容**:
- `TOpenSSLLibrary` 类完整定义
- 库初始化/清理逻辑
- 版本信息查询
- 统计信息管理
- 日志回调

**依赖**:
```pascal
uses
  fafafa.ssl.abstract.types,
  fafafa.ssl.abstract.intf,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.err;
```

**导出**:
- `TOpenSSLLibrary` 类

---

#### 1.2 `fafafa.ssl.openssl.context.pas`
**内容**:
- `TOpenSSLContext` 类完整定义
- SSL 上下文配置
- 协议版本设置
- 密码套件配置
- 证书加载
- 会话管理

**依赖**:
```pascal
uses
  fafafa.ssl.abstract.types,
  fafafa.ssl.abstract.intf,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api.ssl,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.lib;  // 需要 TOpenSSLLibrary
```

**导出**:
- `TOpenSSLContext` 类

---

#### 1.3 `fafafa.ssl.openssl.certificate.pas`
**内容**:
- `TOpenSSLCertificate` 类完整定义
- 证书加载/保存（PEM/DER）
- 证书信息提取
- 证书验证（基础和增强）
- 扩展解析
- 指纹计算

**依赖**:
```pascal
uses
  fafafa.ssl.abstract.types,
  fafafa.ssl.abstract.intf,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.x509v3,
  fafafa.ssl.openssl.api.pem,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.evp;
```

**导出**:
- `TOpenSSLCertificate` 类

---

#### 1.4 `fafafa.ssl.openssl.store.pas`
**内容**:
- `TOpenSSLCertificateStore` 类完整定义
- 证书存储创建/销毁
- 证书添加/删除/查找
- CA 证书管理
- 系统存储访问

**依赖**:
```pascal
uses
  fafafa.ssl.abstract.types,
  fafafa.ssl.abstract.intf,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.certificate;  // 需要 TOpenSSLCertificate
```

**导出**:
- `TOpenSSLCertificateStore` 类

---

#### 1.5 `fafafa.ssl.openssl.connection.pas`
**内容**:
- `TOpenSSLConnection` 类完整定义
- SSL 连接创建
- 握手执行
- 数据读写
- 连接状态查询
- 对等证书获取

**依赖**:
```pascal
uses
  fafafa.ssl.abstract.types,
  fafafa.ssl.abstract.intf,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api.ssl,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.context,      // 需要 TOpenSSLContext
  fafafa.ssl.openssl.certificate;  // 需要 TOpenSSLCertificate
```

**导出**:
- `TOpenSSLConnection` 类

---

#### 1.6 `fafafa.ssl.openssl.utils.pas`
**内容**:
- **错误处理**:
  - `GetOpenSSLError`
  - `GetOpenSSLErrorString`
  - `ClearOpenSSLErrors`
  - `ClassifyOpenSSLError`
  - `GetOpenSSLErrorCategory`
  - `GetFriendlyErrorMessage`

- **证书工具**:
  - `LoadCertificateFromFile`
  - `LoadCertificateFromMemory`
  - `LoadPrivateKeyFromFile`
  - `LoadPrivateKeyFromMemory`
  - `VerifyCertificate`
  - `GetCertificateInfo`

- **协议工具**:
  - `ProtocolToOpenSSL`
  - `OpenSSLToProtocol`
  - `GetProtocolName`

- **库辅助**:
  - `OpenSSLAvailable`
  - `LoadOpenSSL`
  - `UnloadOpenSSL`
  - `GetOpenSSLVersion`
  - `GetOpenSSLVersionNumber`

**依赖**:
```pascal
uses
  fafafa.ssl.abstract.types,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.err,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.pem,
  fafafa.ssl.openssl.api.bio;
```

**导出**:
- 所有工具函数

---

#### 1.7 `fafafa.ssl.openssl.pas` (重构后)
**角色**: 主入口模块，重新导出所有子模块

**内容**:
```pascal
unit fafafa.ssl.openssl;

{$mode objfpc}{$H+}

interface

uses
  // 导入所有子模块
  fafafa.ssl.openssl.lib,
  fafafa.ssl.openssl.context,
  fafafa.ssl.openssl.certificate,
  fafafa.ssl.openssl.store,
  fafafa.ssl.openssl.connection,
  fafafa.ssl.openssl.utils,
  
  // 导出抽象接口
  fafafa.ssl.abstract.types,
  fafafa.ssl.abstract.intf;

type
  // 重新导出类型
  TOpenSSLLibrary = fafafa.ssl.openssl.lib.TOpenSSLLibrary;
  TOpenSSLContext = fafafa.ssl.openssl.context.TOpenSSLContext;
  TOpenSSLCertificate = fafafa.ssl.openssl.certificate.TOpenSSLCertificate;
  TOpenSSLCertificateStore = fafafa.ssl.openssl.store.TOpenSSLCertificateStore;
  TOpenSSLConnection = fafafa.ssl.openssl.connection.TOpenSSLConnection;

// 重新导出工具函数
function OpenSSLAvailable: Boolean;
function LoadOpenSSL(const aLibraryPath: string = ''): Boolean;
procedure UnloadOpenSSL;
function GetOpenSSLVersion: string;
// ... (所有工具函数)

implementation

// 转发到子模块实现
function OpenSSLAvailable: Boolean;
begin
  Result := fafafa.ssl.openssl.utils.OpenSSLAvailable;
end;

// ... (转发所有函数)

initialization
  RegisterOpenSSLBackend;

finalization
  UnregisterOpenSSLBackend;

end.
```

---

### 阶段 2: 依赖关系处理

#### 循环依赖避免
1. **`lib` → 其他**: `TOpenSSLLibrary` 不依赖其他类
2. **`context` → `lib`**: 可以安全依赖
3. **`certificate` → 无**: 独立实现
4. **`store` → `certificate`**: 用于类型转换
5. **`connection` → `context`, `certificate`**: 需要这两个类
6. **`utils` → 所有**: 仅用于类型声明，不产生循环依赖

**解决方案**: 使用接口（`ISSLContext`, `ISSLCertificate`）而非直接类引用

---

### 阶段 3: 测试验证

#### 编译测试
```bash
# 编译每个新模块
fpc -Fusrc src\fafafa.ssl.openssl.lib.pas
fpc -Fusrc src\fafafa.ssl.openssl.context.pas
# ... (所有模块)

# 编译主模块
fpc -Fusrc src\fafafa.ssl.openssl.pas

# 编译示例
fpc -Fusrc examples\hello_ssl.pas
```

#### 功能测试
```bash
# 运行现有测试套件
.\run_all_tests.ps1

# 验证通过率不下降
# 目标: 保持 P1: 97.9%, P2: 93.6%, P3: 95.7%
```

---

### 阶段 4: 文档更新

#### 更新文件
- `ARCHITECTURE_FILE_ORGANIZATION.md` - 更新模块结构图
- `README.md` - 更新模块说明
- 各新模块 - 添加头部注释

---

## 风险与挑战

### 风险
1. **循环依赖**: 类之间可能存在相互引用
   - **缓解**: 使用接口，延迟绑定
2. **编译错误**: 大规模重构可能引入编译错误
   - **缓解**: 逐步拆分，频繁编译验证
3. **测试失败**: 可能影响现有功能
   - **缓解**: 每次拆分后运行完整测试
4. **向后兼容**: 现有代码可能需要修改
   - **缓解**: 保持主模块重新导出

### 挑战
1. **类间依赖**: `TOpenSSLLibrary` 被其他类引用
   - **解决**: 传递 `ISSLLibrary` 接口
2. **工厂方法**: `CreateContext`, `CreateCertificate` 需要跨模块
   - **解决**: 在各自模块实现，主模块转发
3. **全局状态**: OpenSSL 库加载状态
   - **解决**: 保留在 `lib.pas` 中

---

## 时间估算

| 任务 | 估计时间 | 优先级 |
|------|----------|--------|
| 创建 `lib.pas` | 2 小时 | 高 |
| 创建 `context.pas` | 2 小时 | 高 |
| 创建 `certificate.pas` | 3 小时 | 高 |
| 创建 `store.pas` | 1 小时 | 中 |
| 创建 `connection.pas` | 2.5 小时 | 高 |
| 创建 `utils.pas` | 1.5 小时 | 中 |
| 重构主模块 | 1 小时 | 高 |
| 编译测试 | 1 小时 | 高 |
| 功能测试 | 1.5 小时 | 高 |
| 文档更新 | 1 小时 | 中 |
| **总计** | **17 小时** | - |

---

## 执行计划

### Day 1 (8 小时)
- ✅ 创建拆分计划文档
- 创建 `lib.pas` (2h)
- 创建 `context.pas` (2h)
- 开始 `certificate.pas` (3h)
- 编译测试 (1h)

### Day 2 (6 小时)
- 完成 `certificate.pas`
- 创建 `store.pas` (1h)
- 创建 `connection.pas` (2.5h)
- 创建 `utils.pas` (1.5h)
- 编译测试 (1h)

### Day 3 (3 小时)
- 重构主模块 (1h)
- 功能测试 (1.5h)
- 文档更新 (0.5h)

---

## 成功标准

1. ✅ 所有新模块文件大小 < 1000 行
2. ✅ 编译无错误无警告
3. ✅ 测试通过率保持或提升
4. ✅ 现有代码无需修改（或仅需微调）
5. ✅ 文档完整更新

---

## 下一步

- [ ] 开始创建 `fafafa.ssl.openssl.lib.pas`
- [ ] 逐步拆分其他模块
- [ ] 持续测试验证
- [ ] 提交到 Git 并推送

---

**状态**: 📝 计划完成，准备执行  
**预计完成日期**: 2025-10-27 (3 天)

