unit fafafa.ssl.winssl.errors;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Windows,
  fafafa.ssl.winssl.types;

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
  
  { 错误处理器接口 }
  ISSLErrorHandler = interface
    ['{F4B2C8E6-9A3D-4F2E-8B5C-1E7F9D4A6B2C}']
    procedure HandleError(const aErrorInfo: TSSLErrorInfo);
  end;
  
  { 默认错误处理器 - 输出到日志文件 }
  TSSLFileErrorHandler = class(TInterfacedObject, ISSLErrorHandler)
  private
    FLogFile: TextFile;
    FLogFileName: string;
    FEnabled: Boolean;
    
  public
    constructor Create(const aLogFileName: string);
    destructor Destroy; override;
    
    procedure HandleError(const aErrorInfo: TSSLErrorInfo);
    procedure SetEnabled(aEnabled: Boolean);
  end;
  
  { 控制台错误处理器 }
  TSSLConsoleErrorHandler = class(TInterfacedObject, ISSLErrorHandler)
  public
    procedure HandleError(const aErrorInfo: TSSLErrorInfo);
  end;

{ 错误码映射函数 }

{ 获取友好的错误消息 (中文) }
function GetFriendlyErrorMessageCN(aErrorCode: DWORD): string;

{ 获取友好的错误消息 (英文) }
function GetFriendlyErrorMessageEN(aErrorCode: DWORD): string;

{ 获取系统错误消息 }
function GetSystemErrorMessage(aErrorCode: DWORD): string;

{ 格式化错误信息 }
function FormatErrorInfo(const aErrorInfo: TSSLErrorInfo): string;

{ 全局错误日志函数 }

{ 记录错误 }
procedure LogError(aLevel: TSSLErrorLevel; aCode: DWORD; 
  const aMessage, aContext: string);

{ 设置全局错误处理器 }
procedure SetGlobalErrorHandler(aHandler: ISSLErrorHandler);

{ 启用/禁用错误日志 }
procedure EnableErrorLogging(aEnabled: Boolean);

implementation

var
  GErrorHandler: ISSLErrorHandler = nil;
  GErrorLoggingEnabled: Boolean = False;

{ TSSLFileErrorHandler }

constructor TSSLFileErrorHandler.Create(const aLogFileName: string);
begin
  inherited Create;
  FLogFileName := aLogFileName;
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
      // 忽略关闭错误
    end;
  end;
  inherited Destroy;
end;

procedure TSSLFileErrorHandler.HandleError(const aErrorInfo: TSSLErrorInfo);
begin
  if not FEnabled then
    Exit;
    
  try
    WriteLn(FLogFile, FormatErrorInfo(aErrorInfo));
    Flush(FLogFile);
  except
    FEnabled := False;
  end;
end;

procedure TSSLFileErrorHandler.SetEnabled(aEnabled: Boolean);
begin
  FEnabled := aEnabled;
end;

{ TSSLConsoleErrorHandler }

procedure TSSLConsoleErrorHandler.HandleError(const aErrorInfo: TSSLErrorInfo);
begin
  WriteLn(ErrOutput, FormatErrorInfo(aErrorInfo));
end;

{ 错误码映射函数 }

function GetFriendlyErrorMessageCN(aErrorCode: DWORD): string;
begin
  case LONG(aErrorCode) of
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
    Result := Format('未知错误 (0x%x)', [aErrorCode]);
  end;
end;

function GetFriendlyErrorMessageEN(aErrorCode: DWORD): string;
begin
  case LONG(aErrorCode) of
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
    Result := Format('Unknown error (0x%x)', [aErrorCode]);
  end;
end;

function GetSystemErrorMessage(aErrorCode: DWORD): string;
var
  LBuffer: array[0..1023] of Char;
  LSize: DWORD;
begin
  LSize := FormatMessage(
    FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS,
    nil,
    aErrorCode,
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
    Result := Format('Error code: 0x%x', [aErrorCode]);
end;

function FormatErrorInfo(const aErrorInfo: TSSLErrorInfo): string;
const
  LEVEL_STR: array[TSSLErrorLevel] of string = (
    'DEBUG', 'INFO', 'WARNING', 'ERROR', 'FATAL'
  );
begin
  Result := Format('[%s] %s | %s | Code: 0x%x | %s',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss', aErrorInfo.Timestamp),
     LEVEL_STR[aErrorInfo.Level],
     aErrorInfo.Context,
     aErrorInfo.Code,
     aErrorInfo.Message]);
end;

{ 全局错误日志函数 }

procedure LogError(aLevel: TSSLErrorLevel; aCode: DWORD; 
  const aMessage, aContext: string);
var
  LErrorInfo: TSSLErrorInfo;
begin
  if not GErrorLoggingEnabled then
    Exit;
    
  if GErrorHandler = nil then
    Exit;
    
  LErrorInfo.Level := aLevel;
  LErrorInfo.Code := aCode;
  LErrorInfo.Message := aMessage;
  LErrorInfo.Context := aContext;
  LErrorInfo.Timestamp := Now;
  
  GErrorHandler.HandleError(LErrorInfo);
end;

procedure SetGlobalErrorHandler(aHandler: ISSLErrorHandler);
begin
  GErrorHandler := aHandler;
end;

procedure EnableErrorLogging(aEnabled: Boolean);
begin
  GErrorLoggingEnabled := aEnabled;
end;

initialization
  GErrorLoggingEnabled := False;

finalization
  GErrorHandler := nil;

end.

