# WinSSL Windows 验证检查清单

**创建日期**: 2025-10-28  
**目的**: Windows 环境验证准备  
**基于**: WINSSL_CODE_ANALYSIS_2025-10-28.md

---

## 📋 验证前准备

### 环境要求

- [ ] **Windows 版本**: Windows 7+ (推荐 Windows 10/11)
- [ ] **Free Pascal**: 3.3.1 或更高版本
- [ ] **Lazarus**: 可选，用于 IDE 调试
- [ ] **管理员权限**: 用于访问证书存储（部分测试）

### 必需工具

- [ ] 终端/命令提示符
- [ ] 文本编辑器
- [ ] 网络连接（用于 HTTPS 测试）

---

## 🔧 阶段 1: 编译验证（估计 1-2 小时）

### 1.1 单模块编译测试

**命令**:
```cmd
cd src
fpc -Twin64 -Mobjfpc -Scghi fafafa.ssl.winssl.api.pas
fpc -Twin64 -Mobjfpc -Scghi fafafa.ssl.winssl.types.pas
fpc -Twin64 -Mobjfpc -Scghi fafafa.ssl.winssl.utils.pas
fpc -Twin64 -Mobjfpc -Scghi fafafa.ssl.winssl.errors.pas
fpc -Twin64 -Mobjfpc -Scghi fafafa.ssl.winssl.enterprise.pas
fpc -Twin64 -Mobjfpc -Scghi fafafa.ssl.winssl.optimized.pas
fpc -Twin64 -Mobjfpc -Scghi fafafa.ssl.winssl.lib.pas
fpc -Twin64 -Mobjfpc -Scghi fafafa.ssl.winssl.context.pas
fpc -Twin64 -Mobjfpc -Scghi fafafa.ssl.winssl.certificate.pas
fpc -Twin64 -Mobjfpc -Scghi fafafa.ssl.winssl.certstore.pas
fpc -Twin64 -Mobjfpc -Scghi fafafa.ssl.winssl.connection.pas
```

**检查清单**:
- [ ] 所有 11 个模块编译成功
- [ ] 零编译错误
- [ ] 零编译警告
- [ ] 无依赖问题

**预期问题**:
- ⚠️ 可能的 Date/Time 函数问题（SecondsBetween）
- ⚠️ Windows API 版本兼容性

### 1.2 记录编译结果

**模块编译状态表**:

| 模块 | 状态 | 错误数 | 警告数 | 备注 |
|------|------|--------|--------|------|
| api.pas | ⏳ | - | - | |
| types.pas | ⏳ | - | - | |
| utils.pas | ⏳ | - | - | |
| errors.pas | ⏳ | - | - | |
| enterprise.pas | ⏳ | - | - | |
| optimized.pas | ⏳ | - | - | |
| lib.pas | ⏳ | - | - | |
| context.pas | ⏳ | - | - | |
| certificate.pas | ⏳ | - | - | |
| certstore.pas | ⏳ | - | - | |
| connection.pas | ⏳ | - | - | |

---

## 🧪 阶段 2: 单元测试（估计 2-3 小时）

### 2.1 基础 API 测试

**测试程序**: `tests/test_winssl_api_basic.pas`

- [ ] 编译测试程序
- [ ] 运行测试
- [ ] 记录结果

**预期结果**:
```
Testing WinSSL API...
✓ AcquireCredentialsHandle loaded
✓ InitializeSecurityContext loaded
✓ DecryptMessage loaded
✓ EncryptMessage loaded
...
```

### 2.2 库初始化测试

**测试代码**:
```pascal
program test_winssl_lib_init;
uses
  fafafa.ssl.winssl.lib;

var
  Lib: ISSLLibrary;
begin
  Lib := CreateWinSSLLibrary;
  if Lib.Initialize then
  begin
    WriteLn('✓ WinSSL initialized successfully');
    WriteLn('  Version: ', Lib.GetVersionString);
    WriteLn('  TLS 1.2: ', Lib.IsProtocolSupported(sslProtocolTLS12));
    WriteLn('  TLS 1.3: ', Lib.IsProtocolSupported(sslProtocolTLS13));
    Lib.Finalize;
  end
  else
    WriteLn('✗ WinSSL initialization failed');
end.
```

**检查清单**:
- [ ] 库初始化成功
- [ ] 版本字符串正确
- [ ] TLS 1.2 支持检测正确
- [ ] TLS 1.3 支持检测正确

### 2.3 上下文创建测试

**测试代码**:
```pascal
program test_winssl_context;
uses
  fafafa.ssl.winssl.lib, fafafa.ssl.abstract.intf;

var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
begin
  Lib := CreateWinSSLLibrary;
  Lib.Initialize;
  
  Ctx := Lib.CreateContext(sslCtxClient);
  if Ctx <> nil then
  begin
    WriteLn('✓ Context created');
    Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    Ctx.SetServerName('www.google.com');
    WriteLn('✓ Context configured');
  end;
  
  Lib.Finalize;
end.
```

**检查清单**:
- [ ] 创建客户端上下文成功
- [ ] 设置协议版本成功
- [ ] 设置 SNI 成功

---

## 🌐 阶段 3: TLS 握手测试（估计 2-3 小时）

### 3.1 基础握手测试

**测试程序**: `tests/test_winssl_handshake_debug.pas`

**测试目标**: www.google.com:443

**检查清单**:
- [ ] TCP 连接建立
- [ ] 发送 Client Hello
- [ ] 接收 Server Hello
- [ ] 握手迭代处理
- [ ] 握手成功完成
- [ ] 获取协议版本
- [ ] 获取密码套件

**预期输出**:
```
[DEBUG] Creating socket...
[DEBUG] Connecting to www.google.com:443...
[DEBUG] TCP connected
[DEBUG] Acquiring credentials...
[DEBUG] Starting TLS handshake...
[DEBUG] Sent Client Hello (XXX bytes)
[DEBUG] Received data (XXX bytes)
[DEBUG] Handshake iteration 1...
...
✓ TLS handshake completed
  Protocol: TLS 1.3
  Cipher: TLS_AES_256_GCM_SHA384
```

### 3.2 多站点握手测试

**测试站点**:
- [ ] www.google.com:443
- [ ] api.github.com:443
- [ ] www.microsoft.com:443
- [ ] www.cloudflare.com:443

**记录**:
| 站点 | 协议 | 密码套件 | 耗时 | 状态 |
|------|------|----------|------|------|
| google.com | ⏳ | ⏳ | ⏳ | ⏳ |
| github.com | ⏳ | ⏳ | ⏳ | ⏳ |
| microsoft.com | ⏳ | ⏳ | ⏳ | ⏳ |
| cloudflare.com | ⏳ | ⏳ | ⏳ | ⏳ |

---

## 📡 阶段 4: HTTPS 客户端测试（估计 2-3 小时）

### 4.1 完整 HTTPS 客户端测试

**测试程序**: `tests/test_winssl_https_client.pas`

**功能测试**:
- [ ] GET 请求
- [ ] 接收响应
- [ ] 状态码解析
- [ ] HTTP 头解析
- [ ] 响应体读取
- [ ] 连接关闭

**预期输出**:
```
Testing HTTPS client with WinSSL...
✓ Connected to www.google.com
✓ Sent HTTP GET request
✓ Received response
  Status: 200 OK
  Content-Length: XXXX
✓ Read response body
✓ Connection closed
```

### 4.2 真实场景测试

#### 场景 1: 下载小文件
- [ ] 下载 robots.txt
- [ ] 验证内容
- [ ] 检查性能

#### 场景 2: 下载大文件
- [ ] 下载 1MB+ 文件
- [ ] 检查数据完整性
- [ ] 测试稳定性

#### 场景 3: REST API 调用
- [ ] GET api.github.com/users/octocat
- [ ] 解析 JSON 响应
- [ ] 验证数据

---

## 🔐 阶段 5: 证书功能测试（估计 1-2 小时）

### 5.1 证书存储访问

**测试程序**: `tests/test_winssl_certificate.pas`

**测试内容**:
- [ ] 打开 ROOT 证书存储
- [ ] 枚举证书
- [ ] 读取证书信息
- [ ] 计算指纹

**预期结果**:
```
Testing Windows certificate stores...
✓ Opened ROOT store
  Certificates found: XXX
✓ Opened MY store
  Certificates found: XXX
✓ Read certificate info
  Subject: CN=...
  Issuer: CN=...
  Valid from: ...
  Valid to: ...
✓ Calculated SHA-256 fingerprint
```

### 5.2 证书验证测试

**测试内容**:
- [ ] 证书链构建
- [ ] 证书链验证
- [ ] 主机名验证
- [ ] 扩展解析

---

## 🏢 阶段 6: 企业功能测试（可选，1 小时）

### 6.1 企业策略检测

**测试内容**:
- [ ] 检测 FIPS 模式
- [ ] 读取组策略
- [ ] 访问企业证书存储

### 6.2 证书存储集成

**测试内容**:
- [ ] 从 MY 存储加载证书
- [ ] 从 CA 存储加载证书

---

## ⚠️ 阶段 7: 已知 TODO 功能测试（估计 1 小时）

### 7.1 Context 功能测试（预期失败）

**测试 TODO 功能**:
- [ ] LoadCertificate(fileName) - **预期失败**（TODO）
- [ ] LoadPrivateKey(fileName) - **预期失败**（TODO）
- [ ] LoadCAFile - **预期失败**（TODO）
- [ ] SetALPNProtocols - **预期失败**（TODO）
- [ ] SetSessionCacheMode - **预期失败**（TODO）

**记录**:
```
✗ LoadCertificate - TODO (expected)
✗ LoadPrivateKey - TODO (expected)
✗ LoadCAFile - TODO (expected)
✗ SetALPNProtocols - TODO (expected)
✗ SetSessionCacheMode - TODO (expected)
```

### 7.2 Connection 功能测试（预期部分失败）

**测试 TODO 功能**:
- [ ] Renegotiate - **预期失败**（TODO）
- [ ] WantRead - **预期失败**（TODO）
- [ ] WantWrite - **预期失败**（TODO）
- [ ] GetPeerCertificate - **可能返回 nil**（TODO）

---

## 🖥️ 阶段 8: 服务器模式测试（可选，2-3 小时）

### 8.1 基础服务器测试

**测试代码**:
```pascal
// 创建服务器上下文
Ctx := Lib.CreateContext(sslCtxServer);
Conn := Ctx.CreateConnection(ClientSocket);
if Conn.Accept then
  WriteLn('✓ Server accepted connection')
else
  WriteLn('✗ Server accept failed');
```

**检查清单**:
- [ ] 创建服务器上下文
- [ ] 监听连接
- [ ] 接受客户端连接
- [ ] 服务器端握手
- [ ] 数据传输

**注意**: 服务器模式可能需要证书加载（TODO 功能），可能无法完全测试。

---

## 📊 阶段 9: 压力和稳定性测试（可选，2-3 小时）

### 9.1 连续连接测试

**测试**: 连续 100 次连接到同一站点

- [ ] 所有连接成功
- [ ] 无内存泄漏
- [ ] 性能稳定

### 9.2 并发连接测试

**测试**: 同时连接 10 个不同站点

- [ ] 所有连接成功
- [ ] 线程安全
- [ ] 资源正确释放

### 9.3 长时间运行测试

**测试**: 运行 30 分钟持续测试

- [ ] 无崩溃
- [ ] 无内存泄漏
- [ ] 性能稳定

---

## 📝 验证报告模板

### 执行摘要

**验证日期**: ____________  
**验证环境**: Windows XX, FPC X.X.X  
**验证人**: ____________

**总体结果**: ✅ / ⚠️ / ❌

| 阶段 | 通过率 | 状态 |
|------|--------|------|
| 1. 编译验证 | __/11 | ⏳ |
| 2. 单元测试 | __/__ | ⏳ |
| 3. TLS 握手 | __/__ | ⏳ |
| 4. HTTPS 客户端 | __/__ | ⏳ |
| 5. 证书功能 | __/__ | ⏳ |
| 6. 企业功能 | __/__ | ⏳ |
| 7. TODO 功能 | __/__ | ⏳ |
| 8. 服务器模式 | __/__ | ⏳ |

### 关键发现

**成功项**:
1. _____________
2. _____________
3. _____________

**失败项**:
1. _____________
2. _____________
3. _____________

**TODO 确认**:
- ✗ LoadCertificate - 确认未实现
- ✗ LoadPrivateKey - 确认未实现
- ✗ ALPN - 确认未实现
- ...

### 问题清单

| 优先级 | 问题 | 模块 | 严重性 |
|--------|------|------|--------|
| P0 | _______________ | _____ | Critical |
| P1 | _______________ | _____ | High |
| P2 | _______________ | _____ | Medium |

### 建议的下一步

1. _____________
2. _____________
3. _____________

---

## 🎯 成功标准

### 最低标准（必须达成）

- ✅ 所有模块编译成功（11/11）
- ✅ 库初始化成功
- ✅ 上下文创建成功
- ✅ TLS 握手成功（至少 1 个站点）
- ✅ HTTPS GET 成功
- ✅ 证书存储访问成功

### 理想标准（期望达成）

- ✅ 所有模块编译无警告
- ✅ TLS 握手成功（4/4 站点）
- ✅ HTTPS 客户端测试全部通过
- ✅ 证书功能测试全部通过
- ✅ 连续 100 次连接稳定

### 额外标准（可选）

- ✅ 服务器模式基本可用
- ✅ 企业功能测试通过
- ✅ 压力测试通过

---

## ⚡ 快速验证命令（5 分钟）

**最小验证**:
```cmd
cd tests
fpc test_winssl_api_basic.pas
test_winssl_api_basic.exe

fpc test_winssl_handshake_debug.pas
test_winssl_handshake_debug.exe

fpc test_winssl_https_client.pas
test_winssl_https_client.exe
```

**预期**: 所有测试应该全部通过或大部分通过

---

## 📋 验证后行动

### 如果验证成功 ✅

1. 生成验证报告
2. 更新 CURRENT_STATUS.md
3. 修复发现的小问题
4. 进入文档完善阶段

### 如果发现问题 ⚠️

1. 记录所有问题
2. 按优先级排序
3. 修复 P0 和 P1 问题
4. 重新验证

### 如果验证失败 ❌

1. 详细记录失败原因
2. 评估修复工作量
3. 制定修复计划
4. 考虑调整项目范围

---

**检查清单创建**: 2025-10-28  
**状态**: 准备就绪  
**下一步**: 等待 Windows 环境

---

**记住**: 这是一次诚实的验证，目的是确认 WinSSL 的真实可用性，不要害怕发现问题！

