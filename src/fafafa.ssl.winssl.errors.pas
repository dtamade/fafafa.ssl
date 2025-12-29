unit fafafa.ssl.winssl.errors;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  SysUtils, Classes, Windows,
  fafafa.ssl.base,           // P2: TSSLErrorCode 统一错误码
  fafafa.ssl.winssl.base;

type
  { 错误级别 }
  TSSLErrorLevel = (
    sslErrorDebug,      // 调试信息
    sslErrorInfo,       // 一般信息
    sslErrorWarning,    // 警告
    sslErrorError,      // 错误
    sslErrorFatal       // 致命错误
  );
  
  { 错误记录 }
  TSSLErrorInfo = record
    Level: TSSLErrorLevel;
    Code: DWORD;
    Message: string;
    Context: string;
    Timestamp: TDateTime;
  end;
  
  {**
   * ISSLErrorHandler - 错误处理器接口
   * @stable 1.0
   * @locked 2025-12-24
   * @breaking-change-policy Requires major version bump
   *}
  ISSLErrorHandler = interface
    ['{F4B2C8E6-9A3D-4F2E-8B5C-1E7F9D4A6B2C}']
    procedure HandleError(const AErrorInfo: TSSLErrorInfo);
  end;
  
  { 默认错误处理器 - 输出到日志文件 }
  TSSLFileErrorHandler = class(TInterfacedObject, ISSLErrorHandler)
  private
    FLogFile: TextFile;
    FLogFileName: string;
    FEnabled: Boolean;
    
  public
    constructor Create(const ALogFileName: string);
    destructor Destroy; override;
    
    procedure HandleError(const AErrorInfo: TSSLErrorInfo);
    procedure SetEnabled(AEnabled: Boolean);
  end;
  
  { 控制台错误处理器 }
  TSSLConsoleErrorHandler = class(TInterfacedObject, ISSLErrorHandler)
  public
    procedure HandleError(const AErrorInfo: TSSLErrorInfo);
  end;

{ 错误码映射函数 }

{ 获取友好的错误消息 (中文) }
function GetFriendlyErrorMessageCN(AErrorCode: DWORD): string;

{ 获取友好的错误消息 (英文) }
function GetFriendlyErrorMessageEN(AErrorCode: DWORD): string;

{ 获取系统错误消息 }
function GetSystemErrorMessage(AErrorCode: DWORD): string;

{ 格式化错误信息 }
function FormatErrorInfo(const AErrorInfo: TSSLErrorInfo): string;

{ 全局错误日志函数 }

{ 记录错误 }
procedure LogError(ALevel: TSSLErrorLevel; ACode: DWORD; 
  const AMessage, AContext: string);

{ 设置全局错误处理器 }
procedure SetGlobalErrorHandler(AHandler: ISSLErrorHandler);

{ 启用/禁用错误日志 }
procedure EnableErrorLogging(AEnabled: Boolean);

{ P2: WinSSL 错误码到 TSSLErrorCode 的映射 }
function ClassifyWinSSLError(AErrorCode: DWORD): TSSLErrorCode;

{ P2: 获取 WinSSL 错误分类名称 }
function GetWinSSLErrorCategory(AErrorCode: DWORD): string;

implementation

var
  GErrorHandler: ISSLErrorHandler = nil;
  GErrorLoggingEnabled: Boolean = False;

{ TSSLFileErrorHandler }

constructor TSSLFileErrorHandler.Create(const ALogFileName: string);
begin
  inherited Create;
  FLogFileName := ALogFileName;
  FEnabled := True;
  
  try
    AssignFile(FLogFile, FLogFileName);
    if FileExists(FLogFileName) then
      Append(FLogFile)
    else
      Rewrite(FLogFile);
  except
    FEnabled := False;
  end;
end;

destructor TSSLFileErrorHandler.Destroy;
begin
  if FEnabled then
  begin
    try
      CloseFile(FLogFile);
    except
      on E: Exception do
      begin
        // P1-2.4: CloseFile failure is non-critical; OS will close on process exit
        {$IFDEF DEBUG}
        WriteLn('[DEBUG] fafafa.ssl.winssl.errors: CloseFile failed: ', E.Message);
        {$ENDIF}
      end;
    end;
  end;
  inherited Destroy;
end;

procedure TSSLFileErrorHandler.HandleError(const AErrorInfo: TSSLErrorInfo);
begin
  if not FEnabled then
    Exit;
    
  try
    WriteLn(FLogFile, FormatErrorInfo(AErrorInfo));
    Flush(FLogFile);
  except
    FEnabled := False;
  end;
end;

procedure TSSLFileErrorHandler.SetEnabled(AEnabled: Boolean);
begin
  FEnabled := AEnabled;
end;

{ TSSLConsoleErrorHandler }

procedure TSSLConsoleErrorHandler.HandleError(const AErrorInfo: TSSLErrorInfo);
begin
  WriteLn(ErrOutput, FormatErrorInfo(AErrorInfo));
end;

{ 错误码映射函数 }

function GetFriendlyErrorMessageCN(AErrorCode: DWORD): string;
begin
  case LONG(AErrorCode) of
    // Security 错误 (常用)
    LONG(SEC_E_OK):
      Result := '操作成功';
    LONG(SEC_I_CONTINUE_NEEDED):
      Result := '握手需要继续';
    LONG(SEC_E_INCOMPLETE_MESSAGE):
      Result := '消息不完整';
    LONG(SEC_E_INVALID_TOKEN):
      Result := '无效的令牌';
    LONG(SEC_E_INVALID_HANDLE):
      Result := '无效的句柄';
    LONG(SEC_E_UNTRUSTED_ROOT):
      Result := '不受信任的根证书';
    LONG(SEC_E_CERT_EXPIRED):
      Result := '证书已过期';
    LONG(SEC_E_ALGORITHM_MISMATCH):
      Result := '算法不匹配';
      
    // 证书错误
    LONG(CERT_E_EXPIRED):
      Result := '证书已过期';
    LONG(CERT_E_UNTRUSTEDROOT):
      Result := '证书链到不受信任的根';
    LONG(CERT_E_WRONG_USAGE):
      Result := '证书用途错误';
    LONG(CERT_E_REVOKED):
      Result := '证书已被吊销';
    LONG(CERT_E_REVOCATION_FAILURE):
      Result := '吊销检查失败';
    LONG(CERT_E_CN_NO_MATCH):
      Result := '证书通用名不匹配';
    LONG(CERT_E_INVALID_NAME):
      Result := '证书名称无效';
    LONG(TRUST_E_CERT_SIGNATURE):
      Result := '证书签名无效';
      
    // Windows 错误
    LONG(ERROR_INVALID_PARAMETER):
      Result := '参数无效';
    LONG(ERROR_NOT_ENOUGH_MEMORY):
      Result := '内存不足';
    LONG(ERROR_INVALID_HANDLE):
      Result := '句柄无效';
    LONG(ERROR_NOT_SUPPORTED):
      Result := '不支持的操作';
    LONG(ERROR_ACCESS_DENIED):
      Result := '访问被拒绝';
      
  else
    Result := Format('未知错误 (0x%x)', [AErrorCode]);
  end;
end;

function GetFriendlyErrorMessageEN(AErrorCode: DWORD): string;
begin
  case LONG(AErrorCode) of
    LONG(SEC_E_OK):
      Result := 'Operation successful';
    LONG(SEC_I_CONTINUE_NEEDED):
      Result := 'Handshake continues';
    LONG(SEC_E_INCOMPLETE_MESSAGE):
      Result := 'Incomplete message';
    LONG(SEC_E_INVALID_TOKEN):
      Result := 'Invalid token';
    LONG(SEC_E_INVALID_HANDLE):
      Result := 'Invalid handle';
    LONG(SEC_E_UNTRUSTED_ROOT):
      Result := 'Untrusted root certificate';
    LONG(SEC_E_CERT_EXPIRED):
      Result := 'Certificate expired';
    LONG(SEC_E_ALGORITHM_MISMATCH):
      Result := 'Algorithm mismatch';
      
    LONG(CERT_E_EXPIRED):
      Result := 'Certificate has expired';
    LONG(CERT_E_UNTRUSTEDROOT):
      Result := 'Certificate chain to untrusted root';
    LONG(CERT_E_WRONG_USAGE):
      Result := 'Certificate has wrong usage';
    LONG(CERT_E_REVOKED):
      Result := 'Certificate has been revoked';
    LONG(CERT_E_REVOCATION_FAILURE):
      Result := 'Revocation check failed';
    LONG(CERT_E_CN_NO_MATCH):
      Result := 'Certificate common name does not match';
    LONG(CERT_E_INVALID_NAME):
      Result := 'Certificate name is invalid';
    LONG(TRUST_E_CERT_SIGNATURE):
      Result := 'Certificate signature is invalid';
      
    LONG(ERROR_INVALID_PARAMETER):
      Result := 'Invalid parameter';
    LONG(ERROR_NOT_ENOUGH_MEMORY):
      Result := 'Not enough memory';
    LONG(ERROR_INVALID_HANDLE):
      Result := 'Invalid handle';
    LONG(ERROR_NOT_SUPPORTED):
      Result := 'Operation not supported';
    LONG(ERROR_ACCESS_DENIED):
      Result := 'Access denied';
      
  else
    Result := Format('Unknown error (0x%x)', [AErrorCode]);
  end;
end;

function GetSystemErrorMessage(AErrorCode: DWORD): string;
var
  LBuffer: array[0..1023] of Char;
  LSize: DWORD;
begin
  LSize := FormatMessage(
    FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS,
    nil,
    AErrorCode,
    0,
    @LBuffer[0],
    SizeOf(LBuffer),
    nil
  );
  
  if LSize > 0 then
  begin
    SetString(Result, PChar(@LBuffer[0]), LSize);
    Result := Trim(Result);
  end
  else
    Result := Format('Error code: 0x%x', [AErrorCode]);
end;

function FormatErrorInfo(const AErrorInfo: TSSLErrorInfo): string;
const
  LEVEL_STR: array[TSSLErrorLevel] of string = (
    'DEBUG', 'INFO', 'WARNING', 'ERROR', 'FATAL'
  );
begin
  Result := Format('[%s] %s | %s | Code: 0x%x | %s',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss', AErrorInfo.Timestamp),
    LEVEL_STR[AErrorInfo.Level],
    AErrorInfo.Context,
    AErrorInfo.Code,
    AErrorInfo.Message]);
end;

{ 全局错误日志函数 }

procedure LogError(ALevel: TSSLErrorLevel; ACode: DWORD; 
  const AMessage, AContext: string);
var
  LErrorInfo: TSSLErrorInfo;
begin
  if not GErrorLoggingEnabled then
    Exit;
    
  if GErrorHandler = nil then
    Exit;
    
  LErrorInfo.Level := ALevel;
  LErrorInfo.Code := ACode;
  LErrorInfo.Message := AMessage;
  LErrorInfo.Context := AContext;
  LErrorInfo.Timestamp := Now;
  
  GErrorHandler.HandleError(LErrorInfo);
end;

procedure SetGlobalErrorHandler(AHandler: ISSLErrorHandler);
begin
  GErrorHandler := AHandler;
end;

procedure EnableErrorLogging(AEnabled: Boolean);
begin
  GErrorLoggingEnabled := AEnabled;
end;

{ P2: WinSSL 错误码到 TSSLErrorCode 的映射
  与 OpenSSL ClassifyOpenSSLError 保持一致的分类逻辑 }
function ClassifyWinSSLError(AErrorCode: DWORD): TSSLErrorCode;
begin
  case LONG(AErrorCode) of
    // 成功状态
    LONG(SEC_E_OK):
      Result := sslErrNone;

    // 握手/协议错误
    LONG(SEC_I_CONTINUE_NEEDED),
    LONG(SEC_I_COMPLETE_NEEDED),
    LONG(SEC_I_COMPLETE_AND_CONTINUE):
      Result := sslErrNone;  // 这些是正常的握手状态

    LONG(SEC_E_INCOMPLETE_MESSAGE):
      Result := sslErrWouldBlock;

    LONG(SEC_E_INVALID_TOKEN),
    LONG(SEC_E_MESSAGE_ALTERED),
    LONG(SEC_E_OUT_OF_SEQUENCE):
      Result := sslErrProtocol;

    LONG(SEC_E_ALGORITHM_MISMATCH),
    LONG(SEC_E_UNSUPPORTED_FUNCTION):
      Result := sslErrUnsupported;

    // 证书错误
    LONG(SEC_E_UNTRUSTED_ROOT),
    LONG(CERT_E_UNTRUSTEDROOT):
      Result := sslErrCertificateUntrusted;

    LONG(SEC_E_CERT_EXPIRED),
    LONG(CERT_E_EXPIRED):
      Result := sslErrCertificateExpired;

    LONG(CERT_E_REVOKED):
      Result := sslErrCertificateRevoked;

    LONG(CERT_E_REVOCATION_FAILURE):
      Result := sslErrVerificationFailed;

    LONG(CERT_E_CN_NO_MATCH),
    LONG(CERT_E_INVALID_NAME):
      Result := sslErrHostnameMismatch;

    LONG(CERT_E_WRONG_USAGE):
      Result := sslErrCertificate;

    LONG(TRUST_E_CERT_SIGNATURE):
      Result := sslErrVerificationFailed;

    LONG(SEC_E_CERT_UNKNOWN),
    LONG(SEC_E_WRONG_PRINCIPAL):
      Result := sslErrCertificate;

    // 句柄/参数错误
    LONG(SEC_E_INVALID_HANDLE),
    LONG(ERROR_INVALID_HANDLE):
      Result := sslErrInvalidParam;

    LONG(ERROR_INVALID_PARAMETER),
    LONG(SEC_E_INVALID_PARAMETER):
      Result := sslErrInvalidParam;

    // 内存错误
    LONG(ERROR_NOT_ENOUGH_MEMORY),
    LONG(SEC_E_INSUFFICIENT_MEMORY):
      Result := sslErrMemory;

    // 初始化错误
    LONG(SEC_E_SECPKG_NOT_FOUND),
    LONG(SEC_E_NOT_OWNER):
      Result := sslErrNotInitialized;

    // I/O 错误
    LONG(SEC_E_CONTEXT_EXPIRED):
      Result := sslErrConnection;

    // 访问错误
    LONG(ERROR_ACCESS_DENIED),
    LONG(SEC_E_LOGON_DENIED):
      Result := sslErrConfiguration;

    // 不支持
    LONG(ERROR_NOT_SUPPORTED):
      Result := sslErrUnsupported;

  else
    // 未知错误映射到通用错误
    Result := sslErrOther;
  end;
end;

{ P2: 获取 WinSSL 错误分类名称 }
function GetWinSSLErrorCategory(AErrorCode: DWORD): string;
begin
  // 根据错误码范围判断分类
  case LONG(AErrorCode) of
    // SEC_E_* 和 SEC_I_* 错误 (0x80090000 - 0x8009FFFF)
    LONG($80090000)..LONG($8009FFFF):
      Result := 'SSPI';

    // CERT_E_* 错误 (0x800B0100 - 0x800B01FF)
    LONG($800B0100)..LONG($800B01FF):
      Result := 'CERT';

    // TRUST_E_* 错误 (0x800B0000 - 0x800B00FF)
    LONG($800B0000)..LONG($800B00FF):
      Result := 'TRUST';

    // Windows 系统错误
    0..65535:
      Result := 'WIN32';

  else
    Result := 'UNKNOWN';
  end;
end;

initialization
  GErrorLoggingEnabled := False;

finalization
  GErrorHandler := nil;

end.

