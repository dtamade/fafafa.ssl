{**
 * Unit: fafafa.ssl.wolfssl.context
 * Purpose: WolfSSL 上下文实现
 *
 * 实现 ISSLContext 接口的 WolfSSL 后端。
 * 负责 WOLFSSL_CTX 管理和连接创建。
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-10
 *}

unit fafafa.ssl.wolfssl.context;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.errors,
  fafafa.ssl.exceptions,
  fafafa.ssl.wolfssl.base,
  fafafa.ssl.wolfssl.api;

type
  { TWolfSSLContext - WolfSSL 上下文类 }
  TWolfSSLContext = class(TInterfacedObject, ISSLContext)
  private
    FLibrary: ISSLLibrary;
    FContextType: TSSLContextType;
    FWolfSSLCtx: PWOLFSSL_CTX;
    FProtocolVersions: TSSLProtocolVersions;
    FVerifyMode: TSSLVerifyModes;
    FVerifyDepth: Integer;
    FServerName: string;
    FCipherList: string;
    FCipherSuites: string;
    FALPNProtocols: string;
    FSessionCacheEnabled: Boolean;
    FSessionTimeout: Integer;
    FSessionCacheSize: Integer;
    FOptions: TSSLOptions;
    FCertVerifyFlags: TSSLCertVerifyFlags;

    // 回调
    FVerifyCallback: TSSLVerifyCallback;
    FPasswordCallback: TSSLPasswordCallback;
    FInfoCallback: TSSLInfoCallback;

    function GetWolfSSLMethod: PWOLFSSL_METHOD;
    procedure ApplyVerifyMode;
    procedure RequireValidContext(const AMethodName: string);

  public
    constructor Create(ALibrary: ISSLLibrary; AType: TSSLContextType);
    destructor Destroy; override;

    { ISSLContext - 基本配置 }
    function GetContextType: TSSLContextType;
    procedure SetProtocolVersions(AVersions: TSSLProtocolVersions);
    function GetProtocolVersions: TSSLProtocolVersions;

    { ISSLContext - 证书和密钥管理 }
    procedure LoadCertificate(const AFileName: string); overload;
    procedure LoadCertificate(AStream: TStream); overload;
    procedure LoadCertificate(ACert: ISSLCertificate); overload;
    procedure LoadPrivateKey(const AFileName: string; const APassword: string = ''); overload;
    procedure LoadPrivateKey(AStream: TStream; const APassword: string = ''); overload;
    procedure LoadCertificatePEM(const APEM: string);
    procedure LoadPrivateKeyPEM(const APEM: string; const APassword: string = '');
    procedure LoadCAFile(const AFileName: string);
    procedure LoadCAPath(const APath: string);
    procedure SetCertificateStore(AStore: ISSLCertificateStore);

    { ISSLContext - 验证配置 }
    procedure SetVerifyMode(AMode: TSSLVerifyModes);
    function GetVerifyMode: TSSLVerifyModes;
    procedure SetVerifyDepth(ADepth: Integer);
    function GetVerifyDepth: Integer;
    procedure SetVerifyCallback(ACallback: TSSLVerifyCallback);

    { ISSLContext - 密码套件配置 }
    procedure SetCipherList(const ACipherList: string);
    function GetCipherList: string;
    procedure SetCipherSuites(const ACipherSuites: string);
    function GetCipherSuites: string;

    { ISSLContext - 会话管理 }
    procedure SetSessionCacheMode(AEnabled: Boolean);
    function GetSessionCacheMode: Boolean;
    procedure SetSessionTimeout(ATimeout: Integer);
    function GetSessionTimeout: Integer;
    procedure SetSessionCacheSize(ASize: Integer);
    function GetSessionCacheSize: Integer;

    { ISSLContext - 高级选项 }
    procedure SetOptions(const AOptions: TSSLOptions);
    function GetOptions: TSSLOptions;
    procedure SetServerName(const AServerName: string);
    function GetServerName: string;
    procedure SetALPNProtocols(const AProtocols: string);
    function GetALPNProtocols: string;

    { ISSLContext - 证书验证标志 }
    procedure SetCertVerifyFlags(AFlags: TSSLCertVerifyFlags);
    function GetCertVerifyFlags: TSSLCertVerifyFlags;

    { ISSLContext - 回调设置 }
    procedure SetPasswordCallback(ACallback: TSSLPasswordCallback);
    procedure SetInfoCallback(ACallback: TSSLInfoCallback);

    { ISSLContext - 创建连接 }
    function CreateConnection(ASocket: THandle): ISSLConnection; overload;
    function CreateConnection(AStream: TStream): ISSLConnection; overload;

    { ISSLContext - 状态查询 }
    function IsValid: Boolean;
    function GetNativeHandle: Pointer;

    { 便利方法 }
    procedure ConfigureSecureDefaults;
  end;

implementation

uses
  fafafa.ssl.wolfssl.certificate,
  fafafa.ssl.wolfssl.session;

{ Forward declaration for connection - will be implemented separately }
type
  TWolfSSLConnection = class(TInterfacedObject, ISSLConnection)
  private
    FContext: TWolfSSLContext;
    FWolfSSL: PWOLFSSL;
    FSocket: THandle;
    FServerName: string;
    FALPNProtocols: string;
    FNegotiatedALPN: string;
    FHandshakeComplete: Boolean;
    FTimeout: Integer;
    FBlocking: Boolean;
  public
    constructor Create(AContext: TWolfSSLContext; ASocket: THandle);
    destructor Destroy; override;

    { ISSLConnection - 基本操作 }
    function Connect: Boolean;
    function Accept: Boolean;
    function Shutdown: Boolean;
    procedure Close;
    function DoHandshake: TSSLHandshakeState;
    function IsHandshakeComplete: Boolean;
    function Renegotiate: Boolean;
    function Read(var ABuffer; ACount: Integer): Integer;
    function Write(const ABuffer; ACount: Integer): Integer;
    function ReadString(out AStr: string): Boolean;
    function WriteString(const AStr: string): Boolean;
    function WantRead: Boolean;
    function WantWrite: Boolean;

    { ISSLConnection - 错误处理 }
    function GetError(ARetCode: Integer): TSSLErrorCode;
    function GetLastError: Integer;
    function GetLastErrorString: string;

    { ISSLConnection - 连接信息 }
    function GetConnectionInfo: TSSLConnectionInfo;
    function GetProtocolVersion: TSSLProtocolVersion;
    function GetCipherName: string;
    function IsConnected: Boolean;

    { ISSLConnection - 证书 }
    function GetPeerCertificate: ISSLCertificate;
    function GetPeerCertificateChain: TSSLCertificateArray;
    function GetVerifyResult: Integer;
    function GetVerifyResultString: string;

    { ISSLConnection - 会话 }
    function GetSession: ISSLSession;
    procedure SetSession(ASession: ISSLSession);
    function IsSessionReused: Boolean;

    { ISSLConnection - SNI/ALPN }
    procedure SetServerName(const AServerName: string);
    function GetServerName: string;
    function GetSelectedALPNProtocol: string;
    function GetNegotiatedProtocol: TSSLProtocolVersion;
    function GetNegotiatedCipher: string;
    function GetNegotiatedALPN: string;

    { ISSLConnection - 状态 }
    function GetState: string;
    function GetStateString: string;

    { ISSLConnection - 超时和阻塞 }
    procedure SetTimeout(ATimeout: Integer);
    function GetTimeout: Integer;
    procedure SetBlocking(ABlocking: Boolean);
    function GetBlocking: Boolean;

    { ISSLConnection - 上下文 }
    function GetContext: ISSLContext;

    { ISSLConnection - 原生句柄 }
    function GetNativeHandle: Pointer;
  end;

{ TWolfSSLContext }

constructor TWolfSSLContext.Create(ALibrary: ISSLLibrary; AType: TSSLContextType);
var
  LMethod: PWOLFSSL_METHOD;
begin
  inherited Create;
  FLibrary := ALibrary;
  FContextType := AType;
  FWolfSSLCtx := nil;
  FProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
  FVerifyMode := [sslVerifyPeer];
  FVerifyDepth := SSL_DEFAULT_VERIFY_DEPTH;
  FServerName := '';
  FCipherList := '';
  FCipherSuites := '';
  FALPNProtocols := '';
  FSessionCacheEnabled := True;
  FSessionTimeout := SSL_DEFAULT_SESSION_TIMEOUT;
  FSessionCacheSize := SSL_DEFAULT_SESSION_CACHE_SIZE;
  FOptions := [ssoEnableSNI];
  FCertVerifyFlags := [];
  FVerifyCallback := nil;
  FPasswordCallback := nil;
  FInfoCallback := nil;

  // 创建 WolfSSL 上下文
  LMethod := GetWolfSSLMethod;
  if LMethod = nil then
    raise ESSLException.Create('Failed to get WolfSSL method for context type');

  if not Assigned(wolfSSL_CTX_new) then
    raise ESSLException.Create('wolfSSL_CTX_new not available');

  FWolfSSLCtx := wolfSSL_CTX_new(LMethod);
  if FWolfSSLCtx = nil then
    raise ESSLException.Create('Failed to create WolfSSL context');

  // 应用默认验证模式
  ApplyVerifyMode;
end;

destructor TWolfSSLContext.Destroy;
begin
  if FWolfSSLCtx <> nil then
  begin
    if Assigned(wolfSSL_CTX_free) then
      wolfSSL_CTX_free(FWolfSSLCtx);
    FWolfSSLCtx := nil;
  end;
  FLibrary := nil;
  inherited Destroy;
end;

function TWolfSSLContext.GetWolfSSLMethod: PWOLFSSL_METHOD;
begin
  Result := nil;

  case FContextType of
    sslCtxClient:
      begin
        // 优先使用 TLS 1.3，回退到 TLS 1.2
        if (sslProtocolTLS13 in FProtocolVersions) and Assigned(wolfTLSv1_3_client_method) then
          Result := wolfTLSv1_3_client_method()
        else if Assigned(wolfSSLv23_client_method) then
          Result := wolfSSLv23_client_method()
        else if Assigned(wolfTLSv1_2_client_method) then
          Result := wolfTLSv1_2_client_method();
      end;

    sslCtxServer:
      begin
        if (sslProtocolTLS13 in FProtocolVersions) and Assigned(wolfTLSv1_3_server_method) then
          Result := wolfTLSv1_3_server_method()
        else if Assigned(wolfSSLv23_server_method) then
          Result := wolfSSLv23_server_method()
        else if Assigned(wolfTLSv1_2_server_method) then
          Result := wolfTLSv1_2_server_method();
      end;

    sslCtxBoth:
      begin
        // 使用通用方法
        if Assigned(wolfSSLv23_client_method) then
          Result := wolfSSLv23_client_method();
      end;
  end;
end;

procedure TWolfSSLContext.ApplyVerifyMode;
var
  LMode: Integer;
begin
  if FWolfSSLCtx = nil then Exit;
  if not Assigned(wolfSSL_CTX_set_verify) then Exit;

  LMode := WOLFSSL_VERIFY_NONE;

  if sslVerifyPeer in FVerifyMode then
    LMode := WOLFSSL_VERIFY_PEER;

  if sslVerifyFailIfNoPeerCert in FVerifyMode then
    LMode := LMode or WOLFSSL_VERIFY_FAIL_IF_NO_PEER_CERT;

  wolfSSL_CTX_set_verify(FWolfSSLCtx, LMode, nil);
end;

procedure TWolfSSLContext.RequireValidContext(const AMethodName: string);
begin
  if FWolfSSLCtx = nil then
    raise ESSLException.CreateFmt('%s: WolfSSL context is not valid', [AMethodName]);
end;

function TWolfSSLContext.GetContextType: TSSLContextType;
begin
  Result := FContextType;
end;

procedure TWolfSSLContext.SetProtocolVersions(AVersions: TSSLProtocolVersions);
begin
  FProtocolVersions := AVersions;
  // WolfSSL 协议版本在创建时确定，运行时更改需要重建上下文
end;

function TWolfSSLContext.GetProtocolVersions: TSSLProtocolVersions;
begin
  Result := FProtocolVersions;
end;

{ 证书和密钥管理 }

procedure TWolfSSLContext.LoadCertificate(const AFileName: string);
begin
  RequireValidContext('LoadCertificate');

  if not FileExists(AFileName) then
    raise ESSLCertError.CreateFmt('Certificate file not found: %s', [AFileName]);

  if not Assigned(wolfSSL_CTX_use_certificate_file) then
    raise ESSLCertError.Create('wolfSSL_CTX_use_certificate_file not available');

  if wolfSSL_CTX_use_certificate_file(FWolfSSLCtx, PAnsiChar(AnsiString(AFileName)),
       WOLFSSL_FILETYPE_PEM) <> WOLFSSL_SUCCESS then
    raise ESSLCertError.CreateFmt('Failed to load certificate: %s', [AFileName]);
end;

procedure TWolfSSLContext.LoadCertificate(AStream: TStream);
begin
  RequireValidContext('LoadCertificate');
  // WolfSSL 不直接支持从流加载，需要先保存到临时文件
  raise ESSLException.Create('Loading certificate from stream not yet implemented for WolfSSL');
end;

procedure TWolfSSLContext.LoadCertificate(ACert: ISSLCertificate);
begin
  RequireValidContext('LoadCertificate');
  raise ESSLException.Create('Loading certificate from ISSLCertificate not yet implemented for WolfSSL');
end;

procedure TWolfSSLContext.LoadPrivateKey(const AFileName: string; const APassword: string);
begin
  RequireValidContext('LoadPrivateKey');

  if not FileExists(AFileName) then
    raise ESSLCertError.CreateFmt('Private key file not found: %s', [AFileName]);

  if not Assigned(wolfSSL_CTX_use_PrivateKey_file) then
    raise ESSLCertError.Create('wolfSSL_CTX_use_PrivateKey_file not available');

  // 注意：WolfSSL 密码回调需要单独设置
  if wolfSSL_CTX_use_PrivateKey_file(FWolfSSLCtx, PAnsiChar(AnsiString(AFileName)),
       WOLFSSL_FILETYPE_PEM) <> WOLFSSL_SUCCESS then
    raise ESSLCertError.CreateFmt('Failed to load private key: %s', [AFileName]);
end;

procedure TWolfSSLContext.LoadPrivateKey(AStream: TStream; const APassword: string);
begin
  RequireValidContext('LoadPrivateKey');
  raise ESSLException.Create('Loading private key from stream not yet implemented for WolfSSL');
end;

procedure TWolfSSLContext.LoadCertificatePEM(const APEM: string);
begin
  RequireValidContext('LoadCertificatePEM');
  raise ESSLException.Create('Loading certificate from PEM string not yet implemented for WolfSSL');
end;

procedure TWolfSSLContext.LoadPrivateKeyPEM(const APEM: string; const APassword: string);
begin
  RequireValidContext('LoadPrivateKeyPEM');
  raise ESSLException.Create('Loading private key from PEM string not yet implemented for WolfSSL');
end;

procedure TWolfSSLContext.LoadCAFile(const AFileName: string);
begin
  RequireValidContext('LoadCAFile');

  if not FileExists(AFileName) then
    raise ESSLCertError.CreateFmt('CA file not found: %s', [AFileName]);

  if not Assigned(wolfSSL_CTX_load_verify_locations) then
    raise ESSLCertError.Create('wolfSSL_CTX_load_verify_locations not available');

  if wolfSSL_CTX_load_verify_locations(FWolfSSLCtx, PAnsiChar(AnsiString(AFileName)), nil) <> WOLFSSL_SUCCESS then
    raise ESSLCertError.CreateFmt('Failed to load CA file: %s', [AFileName]);
end;

procedure TWolfSSLContext.LoadCAPath(const APath: string);
begin
  RequireValidContext('LoadCAPath');

  if not DirectoryExists(APath) then
    raise ESSLCertError.CreateFmt('CA path not found: %s', [APath]);

  if not Assigned(wolfSSL_CTX_load_verify_locations) then
    raise ESSLCertError.Create('wolfSSL_CTX_load_verify_locations not available');

  if wolfSSL_CTX_load_verify_locations(FWolfSSLCtx, nil, PAnsiChar(AnsiString(APath))) <> WOLFSSL_SUCCESS then
    raise ESSLCertError.CreateFmt('Failed to load CA path: %s', [APath]);
end;

procedure TWolfSSLContext.SetCertificateStore(AStore: ISSLCertificateStore);
begin
  RequireValidContext('SetCertificateStore');
  // WolfSSL 使用不同的证书存储机制
  raise ESSLException.Create('SetCertificateStore not yet implemented for WolfSSL');
end;

{ 验证配置 }

procedure TWolfSSLContext.SetVerifyMode(AMode: TSSLVerifyModes);
begin
  FVerifyMode := AMode;
  ApplyVerifyMode;
end;

function TWolfSSLContext.GetVerifyMode: TSSLVerifyModes;
begin
  Result := FVerifyMode;
end;

procedure TWolfSSLContext.SetVerifyDepth(ADepth: Integer);
begin
  FVerifyDepth := ADepth;
  // WolfSSL 验证深度通过其他方式设置
end;

function TWolfSSLContext.GetVerifyDepth: Integer;
begin
  Result := FVerifyDepth;
end;

procedure TWolfSSLContext.SetVerifyCallback(ACallback: TSSLVerifyCallback);
begin
  FVerifyCallback := ACallback;
end;

{ 密码套件配置 }

procedure TWolfSSLContext.SetCipherList(const ACipherList: string);
begin
  FCipherList := ACipherList;
  // WolfSSL 密码套件设置需要额外 API
end;

function TWolfSSLContext.GetCipherList: string;
begin
  Result := FCipherList;
end;

procedure TWolfSSLContext.SetCipherSuites(const ACipherSuites: string);
begin
  FCipherSuites := ACipherSuites;
end;

function TWolfSSLContext.GetCipherSuites: string;
begin
  Result := FCipherSuites;
end;

{ 会话管理 }

procedure TWolfSSLContext.SetSessionCacheMode(AEnabled: Boolean);
begin
  FSessionCacheEnabled := AEnabled;
end;

function TWolfSSLContext.GetSessionCacheMode: Boolean;
begin
  Result := FSessionCacheEnabled;
end;

procedure TWolfSSLContext.SetSessionTimeout(ATimeout: Integer);
begin
  FSessionTimeout := ATimeout;
end;

function TWolfSSLContext.GetSessionTimeout: Integer;
begin
  Result := FSessionTimeout;
end;

procedure TWolfSSLContext.SetSessionCacheSize(ASize: Integer);
begin
  FSessionCacheSize := ASize;
end;

function TWolfSSLContext.GetSessionCacheSize: Integer;
begin
  Result := FSessionCacheSize;
end;

{ 高级选项 }

procedure TWolfSSLContext.SetOptions(const AOptions: TSSLOptions);
begin
  FOptions := AOptions;
end;

function TWolfSSLContext.GetOptions: TSSLOptions;
begin
  Result := FOptions;
end;

procedure TWolfSSLContext.SetServerName(const AServerName: string);
begin
  FServerName := AServerName;
end;

function TWolfSSLContext.GetServerName: string;
begin
  Result := FServerName;
end;

procedure TWolfSSLContext.SetALPNProtocols(const AProtocols: string);
begin
  FALPNProtocols := AProtocols;
end;

function TWolfSSLContext.GetALPNProtocols: string;
begin
  Result := FALPNProtocols;
end;

procedure TWolfSSLContext.SetCertVerifyFlags(AFlags: TSSLCertVerifyFlags);
begin
  FCertVerifyFlags := AFlags;
end;

function TWolfSSLContext.GetCertVerifyFlags: TSSLCertVerifyFlags;
begin
  Result := FCertVerifyFlags;
end;

procedure TWolfSSLContext.SetPasswordCallback(ACallback: TSSLPasswordCallback);
begin
  FPasswordCallback := ACallback;
end;

procedure TWolfSSLContext.SetInfoCallback(ACallback: TSSLInfoCallback);
begin
  FInfoCallback := ACallback;
end;

{ 创建连接 }

function TWolfSSLContext.CreateConnection(ASocket: THandle): ISSLConnection;
begin
  RequireValidContext('CreateConnection');
  Result := TWolfSSLConnection.Create(Self, ASocket);
end;

function TWolfSSLContext.CreateConnection(AStream: TStream): ISSLConnection;
begin
  RequireValidContext('CreateConnection');
  raise ESSLException.Create('Stream-based connections not yet implemented for WolfSSL');
end;

{ 状态查询 }

function TWolfSSLContext.IsValid: Boolean;
begin
  Result := FWolfSSLCtx <> nil;
end;

function TWolfSSLContext.GetNativeHandle: Pointer;
begin
  Result := FWolfSSLCtx;
end;

procedure TWolfSSLContext.ConfigureSecureDefaults;
begin
  // 配置安全默认值
  FProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
  FVerifyMode := [sslVerifyPeer];
  FVerifyDepth := 4;
  ApplyVerifyMode;
end;

{ TWolfSSLConnection }

constructor TWolfSSLConnection.Create(AContext: TWolfSSLContext; ASocket: THandle);
begin
  inherited Create;
  FContext := AContext;
  FSocket := ASocket;
  FWolfSSL := nil;
  FServerName := AContext.FServerName;
  FALPNProtocols := AContext.FALPNProtocols;
  FNegotiatedALPN := '';
  FHandshakeComplete := False;
  FTimeout := 30000;
  FBlocking := True;

  if not Assigned(wolfSSL_new) then
    raise ESSLException.Create('wolfSSL_new not available');

  FWolfSSL := wolfSSL_new(AContext.FWolfSSLCtx);
  if FWolfSSL = nil then
    raise ESSLException.Create('Failed to create WolfSSL connection');

  // 设置文件描述符
  if Assigned(wolfSSL_set_fd) then
    wolfSSL_set_fd(FWolfSSL, Integer(FSocket));

  // 设置 SNI
  if (FServerName <> '') and Assigned(wolfSSL_UseSNI) then
    wolfSSL_UseSNI(FWolfSSL, 0, PAnsiChar(AnsiString(FServerName)), Length(FServerName));

  // 设置 ALPN 协议
  if (FALPNProtocols <> '') and Assigned(wolfSSL_UseALPN) then
    wolfSSL_UseALPN(FWolfSSL, PAnsiChar(AnsiString(FALPNProtocols)),
      Length(FALPNProtocols), 0);  // 0 = WOLFSSL_ALPN_CONTINUE_ON_MISMATCH
end;

destructor TWolfSSLConnection.Destroy;
begin
  if FWolfSSL <> nil then
  begin
    if Assigned(wolfSSL_free) then
      wolfSSL_free(FWolfSSL);
    FWolfSSL := nil;
  end;
  inherited Destroy;
end;

function TWolfSSLConnection.Connect: Boolean;
var
  LResult: Integer;
begin
  Result := False;
  if FWolfSSL = nil then Exit;
  if not Assigned(wolfSSL_connect) then Exit;

  LResult := wolfSSL_connect(FWolfSSL);
  FHandshakeComplete := LResult = WOLFSSL_SUCCESS;
  Result := FHandshakeComplete;
end;

function TWolfSSLConnection.Accept: Boolean;
var
  LResult: Integer;
begin
  Result := False;
  if FWolfSSL = nil then Exit;
  if not Assigned(wolfSSL_accept) then Exit;

  LResult := wolfSSL_accept(FWolfSSL);
  FHandshakeComplete := LResult = WOLFSSL_SUCCESS;
  Result := FHandshakeComplete;
end;

function TWolfSSLConnection.Shutdown: Boolean;
var
  LResult: Integer;
begin
  Result := False;
  if FWolfSSL = nil then Exit;
  if not Assigned(wolfSSL_shutdown) then Exit;

  LResult := wolfSSL_shutdown(FWolfSSL);
  Result := LResult >= 0;
end;

procedure TWolfSSLConnection.Close;
begin
  Shutdown;
end;

function TWolfSSLConnection.DoHandshake: TSSLHandshakeState;
begin
  if FHandshakeComplete then
    Result := sslHsCompleted
  else if Connect then
    Result := sslHsCompleted
  else
    Result := sslHsFailed;
end;

function TWolfSSLConnection.IsHandshakeComplete: Boolean;
begin
  Result := FHandshakeComplete;
end;

function TWolfSSLConnection.Renegotiate: Boolean;
begin
  Result := False;  // WolfSSL 重新协商需要额外实现
end;

function TWolfSSLConnection.Read(var ABuffer; ACount: Integer): Integer;
begin
  Result := -1;
  if FWolfSSL = nil then Exit;
  if not Assigned(wolfSSL_read) then Exit;

  Result := wolfSSL_read(FWolfSSL, @ABuffer, ACount);
end;

function TWolfSSLConnection.Write(const ABuffer; ACount: Integer): Integer;
begin
  Result := -1;
  if FWolfSSL = nil then Exit;
  if not Assigned(wolfSSL_write) then Exit;

  Result := wolfSSL_write(FWolfSSL, @ABuffer, ACount);
end;

function TWolfSSLConnection.ReadString(out AStr: string): Boolean;
var
  LBuf: array[0..4095] of Byte;
  LRead: Integer;
begin
  Result := False;
  AStr := '';
  LRead := Read(LBuf, SizeOf(LBuf));
  if LRead > 0 then
  begin
    SetString(AStr, PAnsiChar(@LBuf[0]), LRead);
    Result := True;
  end;
end;

function TWolfSSLConnection.WriteString(const AStr: string): Boolean;
var
  LWritten: Integer;
begin
  Result := False;
  if AStr = '' then Exit(True);
  LWritten := Write(AStr[1], Length(AStr));
  Result := LWritten = Length(AStr);
end;

function TWolfSSLConnection.WantRead: Boolean;
begin
  Result := GetLastError = WOLFSSL_ERROR_WANT_READ;
end;

function TWolfSSLConnection.WantWrite: Boolean;
begin
  Result := GetLastError = WOLFSSL_ERROR_WANT_WRITE;
end;

function TWolfSSLConnection.GetError(ARetCode: Integer): TSSLErrorCode;
var
  LErr: Integer;
begin
  if FWolfSSL = nil then
    Exit(sslErrGeneral);
  LErr := GetLastError;
  Result := WolfSSLErrorToSSLError(LErr);
end;

function TWolfSSLConnection.GetLastError: Integer;
begin
  Result := 0;
  if (FWolfSSL <> nil) and Assigned(wolfSSL_get_error) then
    Result := wolfSSL_get_error(FWolfSSL, 0);
end;

function TWolfSSLConnection.GetLastErrorString: string;
var
  LError: Integer;
  LBuf: array[0..255] of AnsiChar;
begin
  Result := '';
  LError := GetLastError;
  if (LError <> 0) and Assigned(wolfSSL_ERR_error_string) then
  begin
    FillChar(LBuf, SizeOf(LBuf), 0);
    wolfSSL_ERR_error_string(LError, @LBuf[0]);
    Result := string(PAnsiChar(@LBuf[0]));
  end;
end;

function TWolfSSLConnection.GetConnectionInfo: TSSLConnectionInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.ProtocolVersion := GetProtocolVersion;
  Result.CipherSuite := GetCipherName;
end;

function TWolfSSLConnection.GetProtocolVersion: TSSLProtocolVersion;
begin
  Result := sslProtocolTLS12;  // 默认值
end;

function TWolfSSLConnection.GetCipherName: string;
begin
  Result := '';
end;

function TWolfSSLConnection.IsConnected: Boolean;
begin
  Result := (FWolfSSL <> nil) and FHandshakeComplete;
end;

function TWolfSSLConnection.GetPeerCertificate: ISSLCertificate;
begin
  Result := nil;
end;

function TWolfSSLConnection.GetPeerCertificateChain: TSSLCertificateArray;
begin
  SetLength(Result, 0);
end;

function TWolfSSLConnection.GetVerifyResult: Integer;
begin
  Result := 0;
end;

function TWolfSSLConnection.GetVerifyResultString: string;
begin
  Result := '';
end;

function TWolfSSLConnection.GetSession: ISSLSession;
begin
  Result := TWolfSSLSession.FromConnection(FWolfSSL);
end;

procedure TWolfSSLConnection.SetSession(ASession: ISSLSession);
var
  LSession: PWOLFSSL_SESSION;
begin
  if ASession = nil then Exit;
  if FWolfSSL = nil then Exit;
  if not Assigned(wolfSSL_set_session) then Exit;

  LSession := PWOLFSSL_SESSION(ASession.GetNativeHandle);
  if LSession <> nil then
    wolfSSL_set_session(FWolfSSL, LSession);
end;

function TWolfSSLConnection.IsSessionReused: Boolean;
begin
  Result := False;
  if FWolfSSL = nil then Exit;
  if not Assigned(wolfSSL_session_reused) then Exit;

  Result := wolfSSL_session_reused(FWolfSSL) = 1;
end;

procedure TWolfSSLConnection.SetServerName(const AServerName: string);
begin
  FServerName := AServerName;
  if (FWolfSSL <> nil) and (FServerName <> '') and Assigned(wolfSSL_UseSNI) then
    wolfSSL_UseSNI(FWolfSSL, 0, PAnsiChar(AnsiString(FServerName)), Length(FServerName));
end;

function TWolfSSLConnection.GetServerName: string;
begin
  Result := FServerName;
end;

function TWolfSSLConnection.GetSelectedALPNProtocol: string;
var
  LProtocol: PAnsiChar;
  LSize: Word;
begin
  Result := '';
  if FWolfSSL = nil then Exit;

  // 如果已经缓存了协商结果，直接返回
  if FNegotiatedALPN <> '' then
  begin
    Result := FNegotiatedALPN;
    Exit;
  end;

  // 从 WolfSSL 获取协商的 ALPN 协议
  if Assigned(wolfSSL_ALPN_GetProtocol) then
  begin
    LProtocol := nil;
    LSize := 0;
    if wolfSSL_ALPN_GetProtocol(FWolfSSL, @LProtocol, @LSize) = WOLFSSL_SUCCESS then
    begin
      if (LProtocol <> nil) and (LSize > 0) then
      begin
        SetString(FNegotiatedALPN, LProtocol, LSize);
        Result := FNegotiatedALPN;
      end;
    end;
  end;
end;

function TWolfSSLConnection.GetNegotiatedProtocol: TSSLProtocolVersion;
var
  LVersion: PAnsiChar;
begin
  Result := sslProtocolTLS12;  // 默认值
  if FWolfSSL = nil then Exit;

  if Assigned(wolfSSL_get_version) then
  begin
    LVersion := wolfSSL_get_version(FWolfSSL);
    if LVersion <> nil then
    begin
      if Pos('TLSv1.3', string(LVersion)) > 0 then
        Result := sslProtocolTLS13
      else if Pos('TLSv1.2', string(LVersion)) > 0 then
        Result := sslProtocolTLS12
      else if Pos('TLSv1.1', string(LVersion)) > 0 then
        Result := sslProtocolTLS11
      else if Pos('TLSv1', string(LVersion)) > 0 then
        Result := sslProtocolTLS10;
    end;
  end;
end;

function TWolfSSLConnection.GetNegotiatedCipher: string;
var
  LCipher: Pointer;
  LName: PAnsiChar;
begin
  Result := '';
  if FWolfSSL = nil then Exit;

  if Assigned(wolfSSL_get_current_cipher) and Assigned(wolfSSL_CIPHER_get_name) then
  begin
    LCipher := wolfSSL_get_current_cipher(FWolfSSL);
    if LCipher <> nil then
    begin
      LName := wolfSSL_CIPHER_get_name(LCipher);
      if LName <> nil then
        Result := string(LName);
    end;
  end;
end;

function TWolfSSLConnection.GetNegotiatedALPN: string;
begin
  Result := GetSelectedALPNProtocol;
end;

function TWolfSSLConnection.GetState: string;
begin
  if FHandshakeComplete then
    Result := 'CONNECTED'
  else
    Result := 'DISCONNECTED';
end;

function TWolfSSLConnection.GetStateString: string;
begin
  Result := GetState;
end;

procedure TWolfSSLConnection.SetTimeout(ATimeout: Integer);
begin
  FTimeout := ATimeout;
end;

function TWolfSSLConnection.GetTimeout: Integer;
begin
  Result := FTimeout;
end;

procedure TWolfSSLConnection.SetBlocking(ABlocking: Boolean);
begin
  FBlocking := ABlocking;
end;

function TWolfSSLConnection.GetBlocking: Boolean;
begin
  Result := FBlocking;
end;

function TWolfSSLConnection.GetContext: ISSLContext;
begin
  Result := FContext;
end;

function TWolfSSLConnection.GetNativeHandle: Pointer;
begin
  Result := FWolfSSL;
end;

end.
