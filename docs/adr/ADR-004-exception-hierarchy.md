# ADR-004: 统一异常层次结构

## 状态
已接受 (2025-12-26)

## 上下文
早期版本使用多种错误处理方式：
- 返回布尔值 + 错误码
- 抛出通用 `Exception`
- 使用 `TSSLResult` 记录

这导致：
- 错误处理不一致
- 难以区分错误类型
- 丢失上下文信息

## 决策

建立统一的异常层次结构：

```
ESSLException (基类)
├── ESSLInitializationException  - 初始化失败
├── ESSLConnectionException      - 连接错误
├── ESSLHandshakeException       - 握手失败
├── ESSLCertificateException     - 证书问题
│   ├── ESSLCertificateLoadException
│   ├── ESSLCertificateVerifyException
│   └── ESSLCertificateExpiredException
├── ESSLKeyException             - 密钥问题
├── ESSLConfigurationException   - 配置错误
├── ESSLTimeoutException         - 超时
├── ESSLResourceException        - 资源问题 (内存等)
├── ESSLInvalidArgument          - 参数无效
├── ESSLFileNotFoundException    - 文件未找到
└── ESSLPlatformNotSupportedException - 平台不支持
```

### 异常属性

```pascal
ESSLException = class(Exception)
private
  FErrorCode: TSSLErrorCode;
  FContext: string;
  FNativeError: Integer;
  FBackend: TSSLBackendType;
public
  constructor CreateWithContext(const AMessage: string;
    AErrorCode: TSSLErrorCode; const AContext: string;
    ANativeError: Integer = 0; ABackend: TSSLBackendType = sslUnknown);

  property ErrorCode: TSSLErrorCode read FErrorCode;
  property Context: string read FContext;       // 方法名
  property NativeError: Integer read FNativeError; // OpenSSL/WinSSL 错误码
  property Backend: TSSLBackendType read FBackend;
end;
```

### 辅助函数

```pascal
// 快速抛出异常
procedure RaiseSSLCertError(const AMessage, AContext: string);
procedure RaiseSSLInitError(const AMessage, AContext: string);
procedure RaiseInvalidParameter(const AParamName: string);
procedure RaiseMemoryError(const AOperation: string);
procedure RaiseUnsupported(const AFeature: string);
```

## 后果

### 正面
- 类型化异常便于精确捕获
- 保留完整错误上下文
- 与 Delphi/FPC 异常模型一致
- 便于日志记录和调试

### 负面
- 需要迁移现有代码
- 增加异常类数量
- 性能开销（异常对象创建）

### 风险
- 过度使用异常降低性能
- 遗漏异常导致崩溃

## 使用模式

### 正确用法
```pascal
try
  Context.LoadCertificate('cert.pem');
except
  on E: ESSLCertificateLoadException do
    Log.Error('证书加载失败: %s', [E.Message]);
  on E: ESSLFileNotFoundException do
    Log.Error('文件未找到: %s', [E.Message]);
  on E: ESSLException do
    Log.Error('SSL 错误 [%d]: %s', [Ord(E.ErrorCode), E.Message]);
end;
```

### 结合 Result 类型
对于需要详细错误信息的场景，可使用 `TSSLResult<T>`：
```pascal
function TryConnect: TSSLResult<ISSLConnection>;
begin
  try
    Result := TSSLResult<ISSLConnection>.Success(DoConnect);
  except
    on E: ESSLException do
      Result := TSSLResult<ISSLConnection>.Failure(E.ErrorCode, E.Message);
  end;
end;
```

## 参考
- `src/fafafa.ssl.exceptions.pas` - 异常定义
- `src/fafafa.ssl.errors.pas` - 错误码定义
- `src/fafafa.ssl.result.pas` - Result 类型
