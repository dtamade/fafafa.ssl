unit fafafa.ssl.exceptions;

{$mode objfpc}{$H+}

{**
 * SSL/TLS异常类定义
 * 
 * 提供统一的异常层次结构，用于fafafa.ssl库的错误处理
 * 
 * Version: 2.0
 * 改进: 添加调用上下文支持、TSSLErrorCode集成、细粒度异常类型
 *}

interface

uses
  SysUtils,
  fafafa.ssl.base;  // 引入 TSSLErrorCode 等类型定义

type
  {**
   * SSL/TLS异常基类（增强版）
   * 
   * 所有fafafa.ssl相关异常都继承自此类
   * 提供错误码、调用上下文、原生错误等详细信息
   *}
  ESSLException = class(Exception)
  private
    FErrorCode: TSSLErrorCode;
    FLibraryType: TSSLLibraryType;
    FNativeError: Integer;
    FContext: string;
    FComponent: string;  // 保留向后兼容
  public
    constructor Create(const AMessage: string); overload;
    constructor CreateWithCode(const AMessage: string; AErrorCode: Cardinal); overload;  // 向后兼容
    constructor CreateFmt(const AMessage: string; const AArgs: array of const); overload;
    
    {** 新的构造函数 - 提供完整上下文 *}
    constructor CreateWithContext(
      const AMessage: string;
      AErrorCode: TSSLErrorCode;
      const AContext: string;
      ANativeError: Integer = 0;
      ALibraryType: TSSLLibraryType = sslAutoDetect
    );
    
    { 标准化错误码（fafafa.ssl 定义） }
    property ErrorCode: TSSLErrorCode read FErrorCode;
    
    { 原生库错误码（OpenSSL ERR_get_error()、WinSSL GetLastError() 等） }
    property NativeError: Integer read FNativeError;
    
    { 调用上下文（例如: 'TOpenSSLContext.LoadCertificate'） }
    property Context: string read FContext;
    
    { 库类型（OpenSSL、WinSSL 等） }
    property LibraryType: TSSLLibraryType read FLibraryType;
    
    { 发生错误的组件名称（向后兼容） }
    property Component: string read FComponent write FComponent;
  end;

  {**
   * 初始化和配置相关异常
   *}
  ESSLInitError = class(ESSLException);
  ESSLInitializationException = class(ESSLInitError);  // 新增别名
  ESSLConfigurationException = class(ESSLException);
  ESSLPlatformNotSupportedException = class(ESSLConfigurationException);

  {**
   * 加密操作错误
   *}
  ESSLCryptoError = class(ESSLException);
  ESSLKeyException = class(ESSLCryptoError);
  ESSLKeyDerivationException = class(ESSLCryptoError);
  ESSLEncryptionException = class(ESSLCryptoError);
  ESSLDecryptionException = class(ESSLCryptoError);

  {**
   * 证书相关错误（细分）
   *}
  ESSLCertError = class(ESSLException);
  ESSLCertificateException = class(ESSLCertError);  // 新增别名
  ESSLCertificateLoadException = class(ESSLCertError);
  ESSLCertificateParseException = class(ESSLCertError);
  ESSLCertificateVerificationException = class(ESSLCertError);
  ESSLCertificateExpiredException = class(ESSLCertificateVerificationException);

  {**
   * 连接和网络相关错误
   *}
  ESSLNetworkError = class(ESSLException);
  ESSLConnectionException = class(ESSLNetworkError);  // 新增别名
  ESSLHandshakeException = class(ESSLConnectionException);
  ESSLProtocolException = class(ESSLConnectionException);

  {**
   * 参数和资源错误
   *}
  ESSLInvalidArgument = class(ESSLException);
  ESSLResourceException = class(ESSLException);
  ESSLOutOfMemoryException = class(ESSLResourceException);
  ESSLFileNotFoundException = class(ESSLException);

  {**
   * 操作系统错误
   *}
  ESSLSystemError = class(ESSLException)
  private
    FSystemErrorCode: Integer;
  public
    constructor CreateWithSysCode(const AMessage: string; ASystemErrorCode: Integer);
    
    { 操作系统错误码 }
    property SystemErrorCode: Integer read FSystemErrorCode;
  end;

{**
 * 从OpenSSL错误队列获取错误信息
 * @return 格式化的错误消息字符串
 *}
function GetOpenSSLErrorString: string;

{**
 * 获取最后一个OpenSSL错误码
 * @return OpenSSL错误码（如果没有则返回0）
 *}
function GetLastOpenSSLError: Cardinal;

{**
 * 检查OpenSSL操作结果并在失败时抛出异常
 * @param AResult OpenSSL函数返回值（通常1表示成功）
 * @param AOperation 操作名称（用于错误消息）
 * @raises ESSLCryptoError 如果操作失败
 *}
procedure CheckOpenSSLResult(AResult: Integer; const AOperation: string);

{**
 * 便捷的异常抛出函数（自动获取OpenSSL错误）
 *}
procedure RaiseSSLInitError(const AMessage: string; const AContext: string = '');
procedure RaiseSSLCertError(const AMessage: string; const AContext: string = '');
procedure RaiseSSLConnectionError(const AMessage: string; const AContext: string = '');
procedure RaiseSSLCryptoError(const AMessage: string; const AContext: string = '');
procedure RaiseSSLConfigError(const AMessage: string; const AContext: string = '');

implementation

uses
  fafafa.ssl.openssl.api;

{ ESSLException }

constructor ESSLException.Create(const AMessage: string);
begin
  inherited Create(AMessage);
  FErrorCode := sslErrNone;
  FNativeError := 0;
  FContext := '';
  FComponent := '';
  FLibraryType := sslAutoDetect;
end;

constructor ESSLException.CreateWithCode(const AMessage: string; AErrorCode: Cardinal);
var
  LFullMessage: string;
begin
  LFullMessage := AMessage;
  if AErrorCode <> 0 then
    LFullMessage := Format('%s (OpenSSL error: 0x%x)', [AMessage, AErrorCode]);
  inherited Create(LFullMessage);
  FErrorCode := sslErrOther;  // 旧版本不区分具体类型
  FNativeError := Integer(AErrorCode);
  FContext := '';
  FComponent := '';
  FLibraryType := sslOpenSSL;
end;

constructor ESSLException.CreateFmt(const AMessage: string; const AArgs: array of const);
begin
  inherited CreateFmt(AMessage, AArgs);
  FErrorCode := sslErrNone;
  FNativeError := 0;
  FContext := '';
  FComponent := '';
  FLibraryType := sslAutoDetect;
end;

constructor ESSLException.CreateWithContext(
  const AMessage: string;
  AErrorCode: TSSLErrorCode;
  const AContext: string;
  ANativeError: Integer;
  ALibraryType: TSSLLibraryType
);
var
  LFullMessage: string;
begin
  LFullMessage := AMessage;
  
  // 添加上下文信息
  if AContext <> '' then
    LFullMessage := Format('[%s] %s', [AContext, LFullMessage]);
    
  // 添加原生错误码（如果有）
  if ANativeError <> 0 then
  begin
    case ALibraryType of
      sslOpenSSL:
        LFullMessage := Format('%s (OpenSSL error: 0x%x)', [LFullMessage, ANativeError]);
      sslWinSSL:
        LFullMessage := Format('%s (WinSSL error: %d)', [LFullMessage, ANativeError]);
    else
      LFullMessage := Format('%s (Native error: %d)', [LFullMessage, ANativeError]);
    end;
  end;
  
  inherited Create(LFullMessage);
  FErrorCode := AErrorCode;
  FNativeError := ANativeError;
  FContext := AContext;
  FComponent := '';  // 可以从 Context 解析
  FLibraryType := ALibraryType;
end;

{ ESSLSystemError }

constructor ESSLSystemError.CreateWithSysCode(const AMessage: string; ASystemErrorCode: Integer);
begin
  inherited CreateFmt('%s (System error: %d)', [AMessage, ASystemErrorCode]);
  FSystemErrorCode := ASystemErrorCode;
end;

{ Helper functions }

function GetOpenSSLErrorString: string;
var
  LErrorCode: Cardinal;
  LErrorBuf: array[0..255] of AnsiChar;
begin
  Result := '';
  LErrorCode := ERR_get_error();
  
  while LErrorCode <> 0 do
  begin
    ERR_error_string_n(LErrorCode, @LErrorBuf[0], SizeOf(LErrorBuf));
    if Result <> '' then
      Result := Result + '; ';
    Result := Result + string(LErrorBuf);
    LErrorCode := ERR_get_error();
  end;
  
  if Result = '' then
    Result := 'Unknown OpenSSL error';
end;

function GetLastOpenSSLError: Cardinal;
begin
  Result := ERR_peek_error();  // 查看但不清除错误队列
  if Result = 0 then
    Result := ERR_get_error();  // 如果没有peek到，则获取并清除
end;

procedure CheckOpenSSLResult(AResult: Integer; const AOperation: string);
var
  LErrorMsg: string;
  LErrorCode: Cardinal;
begin
  if AResult <> 1 then
  begin
    LErrorCode := GetLastOpenSSLError;
    LErrorMsg := GetOpenSSLErrorString;
    raise ESSLCryptoError.CreateWithContext(
      Format('%s failed: %s', [AOperation, LErrorMsg]),
      sslErrOther,  // 使用通用错误码
      AOperation,
      Integer(LErrorCode),
      sslOpenSSL
    );
  end;
end;

{ 便捷异常抛出函数 }

procedure RaiseSSLInitError(const AMessage: string; const AContext: string);
var
  LNativeError: Cardinal;
begin
  LNativeError := GetLastOpenSSLError;
  raise ESSLInitializationException.CreateWithContext(
    AMessage,
    sslErrNotInitialized,  // base.pas中已有
    AContext,
    Integer(LNativeError),
    sslOpenSSL
  );
end;

procedure RaiseSSLCertError(const AMessage: string; const AContext: string);
var
  LNativeError: Cardinal;
begin
  LNativeError := GetLastOpenSSLError;
  raise ESSLCertificateLoadException.CreateWithContext(
    AMessage,
    sslErrLoadFailed,  // base.pas中已有
    AContext,
    Integer(LNativeError),
    sslOpenSSL
  );
end;

procedure RaiseSSLConnectionError(const AMessage: string; const AContext: string);
var
  LNativeError: Cardinal;
begin
  LNativeError := GetLastOpenSSLError;
  raise ESSLConnectionException.CreateWithContext(
    AMessage,
    sslErrConnection,  // base.pas中已有
    AContext,
    Integer(LNativeError),
    sslOpenSSL
  );
end;

procedure RaiseSSLCryptoError(const AMessage: string; const AContext: string);
var
  LNativeError: Cardinal;
begin
  LNativeError := GetLastOpenSSLError;
  raise ESSLCryptoError.CreateWithContext(
    AMessage,
    sslErrOther,  // 使用通用错误码
    AContext,
    Integer(LNativeError),
    sslOpenSSL
  );
end;

procedure RaiseSSLConfigError(const AMessage: string; const AContext: string);
begin
  raise ESSLConfigurationException.CreateWithContext(
    AMessage,
    sslErrConfiguration,  // base.pas中已有
    AContext,
    0,
    sslAutoDetect
  );
end;

end.
