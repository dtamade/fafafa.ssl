{**
 * Unit: fafafa.ssl.mbedtls.context
 * Purpose: MbedTLS 上下文实现
 *
 * 实现 ISSLContext 接口的 MbedTLS 后端。
 * 负责 SSL 配置管理和连接创建。
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-10
 *}

unit fafafa.ssl.mbedtls.context;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.errors,
  fafafa.ssl.exceptions,
  fafafa.ssl.mbedtls.base,
  fafafa.ssl.mbedtls.api;

type
  { TMbedTLSContext - MbedTLS 上下文类 }
  TMbedTLSContext = class(TInterfacedObject, ISSLContext)
  private
    FLibrary: ISSLLibrary;
    FContextType: TSSLContextType;
    FSSLConfig: Pmbedtls_ssl_config;
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

    // 证书和密钥
    FCertChain: Pmbedtls_x509_crt;
    FPrivateKey: Pmbedtls_pk_context;
    FCACerts: Pmbedtls_x509_crt;

    procedure AllocateConfig;
    procedure FreeConfig;
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

    { 内部访问 }
    property SSLConfig: Pmbedtls_ssl_config read FSSLConfig;
  end;

implementation

uses
  fafafa.ssl.mbedtls.certificate,
  fafafa.ssl.mbedtls.session;

const
  // MbedTLS structure sizes - use large buffers for safety
  // Actual sizes depend on MbedTLS compile-time configuration
  MBEDTLS_SSL_CONFIG_SIZE = 8192;
  MBEDTLS_X509_CRT_SIZE = 16384;
  MBEDTLS_PK_CONTEXT_SIZE = 2048;

{ TMbedTLSContext }

constructor TMbedTLSContext.Create(ALibrary: ISSLLibrary; AType: TSSLContextType);
begin
  inherited Create;
  FLibrary := ALibrary;
  FContextType := AType;
  FSSLConfig := nil;
  FCertChain := nil;
  FPrivateKey := nil;
  FCACerts := nil;
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

  AllocateConfig;
  ApplyVerifyMode;
end;

destructor TMbedTLSContext.Destroy;
begin
  FreeConfig;
  FLibrary := nil;
  inherited Destroy;
end;

procedure TMbedTLSContext.AllocateConfig;
var
  LEndpoint: Integer;
begin
  if FSSLConfig <> nil then
    FreeConfig;

  // 分配 SSL 配置
  GetMem(FSSLConfig, MBEDTLS_SSL_CONFIG_SIZE);
  FillChar(FSSLConfig^, MBEDTLS_SSL_CONFIG_SIZE, 0);

  if Assigned(mbedtls_ssl_config_init) then
    mbedtls_ssl_config_init(FSSLConfig);

  // 设置端点类型
  case FContextType of
    sslCtxClient: LEndpoint := MBEDTLS_SSL_IS_CLIENT;
    sslCtxServer: LEndpoint := MBEDTLS_SSL_IS_SERVER;
  else
    LEndpoint := MBEDTLS_SSL_IS_CLIENT;
  end;

  // 配置默认值
  if Assigned(mbedtls_ssl_config_defaults) then
    mbedtls_ssl_config_defaults(FSSLConfig, LEndpoint,
      MBEDTLS_SSL_TRANSPORT_STREAM, MBEDTLS_SSL_PRESET_DEFAULT);
end;

procedure TMbedTLSContext.FreeConfig;
begin
  // 释放证书链
  if FCertChain <> nil then
  begin
    if Assigned(mbedtls_x509_crt_free) then
      mbedtls_x509_crt_free(FCertChain);
    FreeMem(FCertChain);
    FCertChain := nil;
  end;

  // 释放私钥
  if FPrivateKey <> nil then
  begin
    if Assigned(mbedtls_pk_free) then
      mbedtls_pk_free(FPrivateKey);
    FreeMem(FPrivateKey);
    FPrivateKey := nil;
  end;

  // 释放 CA 证书
  if FCACerts <> nil then
  begin
    if Assigned(mbedtls_x509_crt_free) then
      mbedtls_x509_crt_free(FCACerts);
    FreeMem(FCACerts);
    FCACerts := nil;
  end;

  // 释放 SSL 配置
  if FSSLConfig <> nil then
  begin
    if Assigned(mbedtls_ssl_config_free) then
      mbedtls_ssl_config_free(FSSLConfig);
    FreeMem(FSSLConfig);
    FSSLConfig := nil;
  end;
end;

procedure TMbedTLSContext.ApplyVerifyMode;
var
  LMode: Integer;
begin
  if FSSLConfig = nil then Exit;
  if not Assigned(mbedtls_ssl_conf_authmode) then Exit;

  if sslVerifyNone in FVerifyMode then
    LMode := MBEDTLS_SSL_VERIFY_NONE
  else if sslVerifyPeer in FVerifyMode then
  begin
    if sslVerifyFailIfNoPeerCert in FVerifyMode then
      LMode := MBEDTLS_SSL_VERIFY_REQUIRED
    else
      LMode := MBEDTLS_SSL_VERIFY_OPTIONAL;
  end
  else
    LMode := MBEDTLS_SSL_VERIFY_NONE;

  mbedtls_ssl_conf_authmode(FSSLConfig, LMode);
end;

procedure TMbedTLSContext.RequireValidContext(const AMethodName: string);
begin
  if FSSLConfig = nil then
    raise ESSLException.CreateFmt('%s: MbedTLS context is not valid', [AMethodName]);
end;

function TMbedTLSContext.GetContextType: TSSLContextType;
begin
  Result := FContextType;
end;

procedure TMbedTLSContext.SetProtocolVersions(AVersions: TSSLProtocolVersions);
begin
  FProtocolVersions := AVersions;
end;

function TMbedTLSContext.GetProtocolVersions: TSSLProtocolVersions;
begin
  Result := FProtocolVersions;
end;

{ 证书和密钥管理 }

procedure TMbedTLSContext.LoadCertificate(const AFileName: string);
begin
  RequireValidContext('LoadCertificate');

  if not FileExists(AFileName) then
    raise ESSLCertError.CreateFmt('Certificate file not found: %s', [AFileName]);

  // 分配证书链
  if FCertChain = nil then
  begin
    GetMem(FCertChain, MBEDTLS_X509_CRT_SIZE);
    FillChar(FCertChain^, MBEDTLS_X509_CRT_SIZE, 0);
    if Assigned(mbedtls_x509_crt_init) then
      mbedtls_x509_crt_init(FCertChain);
  end;

  if not Assigned(mbedtls_x509_crt_parse_file) then
    raise ESSLCertError.Create('mbedtls_x509_crt_parse_file not available');

  if mbedtls_x509_crt_parse_file(FCertChain, PAnsiChar(AnsiString(AFileName))) <> 0 then
    raise ESSLCertError.CreateFmt('Failed to load certificate: %s', [AFileName]);
end;

procedure TMbedTLSContext.LoadCertificate(AStream: TStream);
begin
  RequireValidContext('LoadCertificate');
  raise ESSLException.Create('Loading certificate from stream not yet implemented for MbedTLS');
end;

procedure TMbedTLSContext.LoadCertificate(ACert: ISSLCertificate);
begin
  RequireValidContext('LoadCertificate');
  raise ESSLException.Create('Loading certificate from ISSLCertificate not yet implemented for MbedTLS');
end;

procedure TMbedTLSContext.LoadPrivateKey(const AFileName: string; const APassword: string);
begin
  RequireValidContext('LoadPrivateKey');

  if not FileExists(AFileName) then
    raise ESSLCertError.CreateFmt('Private key file not found: %s', [AFileName]);

  // 分配私钥上下文
  if FPrivateKey = nil then
  begin
    GetMem(FPrivateKey, MBEDTLS_PK_CONTEXT_SIZE);
    FillChar(FPrivateKey^, MBEDTLS_PK_CONTEXT_SIZE, 0);
    if Assigned(mbedtls_pk_init) then
      mbedtls_pk_init(FPrivateKey);
  end;

  if not Assigned(mbedtls_pk_parse_keyfile) then
    raise ESSLCertError.Create('mbedtls_pk_parse_keyfile not available');

  if mbedtls_pk_parse_keyfile(FPrivateKey, PAnsiChar(AnsiString(AFileName)),
       PAnsiChar(AnsiString(APassword)), nil, nil) <> 0 then
    raise ESSLCertError.CreateFmt('Failed to load private key: %s', [AFileName]);
end;

procedure TMbedTLSContext.LoadPrivateKey(AStream: TStream; const APassword: string);
begin
  RequireValidContext('LoadPrivateKey');
  raise ESSLException.Create('Loading private key from stream not yet implemented for MbedTLS');
end;

procedure TMbedTLSContext.LoadCertificatePEM(const APEM: string);
begin
  RequireValidContext('LoadCertificatePEM');
  raise ESSLException.Create('Loading certificate from PEM string not yet implemented for MbedTLS');
end;

procedure TMbedTLSContext.LoadPrivateKeyPEM(const APEM: string; const APassword: string);
begin
  RequireValidContext('LoadPrivateKeyPEM');
  raise ESSLException.Create('Loading private key from PEM string not yet implemented for MbedTLS');
end;

procedure TMbedTLSContext.LoadCAFile(const AFileName: string);
begin
  RequireValidContext('LoadCAFile');

  if not FileExists(AFileName) then
    raise ESSLCertError.CreateFmt('CA file not found: %s', [AFileName]);

  // 分配 CA 证书
  if FCACerts = nil then
  begin
    GetMem(FCACerts, MBEDTLS_X509_CRT_SIZE);
    FillChar(FCACerts^, MBEDTLS_X509_CRT_SIZE, 0);
    if Assigned(mbedtls_x509_crt_init) then
      mbedtls_x509_crt_init(FCACerts);
  end;

  if not Assigned(mbedtls_x509_crt_parse_file) then
    raise ESSLCertError.Create('mbedtls_x509_crt_parse_file not available');

  if mbedtls_x509_crt_parse_file(FCACerts, PAnsiChar(AnsiString(AFileName))) <> 0 then
    raise ESSLCertError.CreateFmt('Failed to load CA file: %s', [AFileName]);

  // 设置 CA 链
  if Assigned(mbedtls_ssl_conf_ca_chain) then
    mbedtls_ssl_conf_ca_chain(FSSLConfig, FCACerts, nil);
end;

procedure TMbedTLSContext.LoadCAPath(const APath: string);
begin
  RequireValidContext('LoadCAPath');

  if not DirectoryExists(APath) then
    raise ESSLCertError.CreateFmt('CA path not found: %s', [APath]);

  // 分配 CA 证书
  if FCACerts = nil then
  begin
    GetMem(FCACerts, MBEDTLS_X509_CRT_SIZE);
    FillChar(FCACerts^, MBEDTLS_X509_CRT_SIZE, 0);
    if Assigned(mbedtls_x509_crt_init) then
      mbedtls_x509_crt_init(FCACerts);
  end;

  if not Assigned(mbedtls_x509_crt_parse_path) then
    raise ESSLCertError.Create('mbedtls_x509_crt_parse_path not available');

  if mbedtls_x509_crt_parse_path(FCACerts, PAnsiChar(AnsiString(APath))) < 0 then
    raise ESSLCertError.CreateFmt('Failed to load CA path: %s', [APath]);

  if Assigned(mbedtls_ssl_conf_ca_chain) then
    mbedtls_ssl_conf_ca_chain(FSSLConfig, FCACerts, nil);
end;

procedure TMbedTLSContext.SetCertificateStore(AStore: ISSLCertificateStore);
begin
  RequireValidContext('SetCertificateStore');
  raise ESSLException.Create('SetCertificateStore not yet implemented for MbedTLS');
end;

{ 验证配置 }

procedure TMbedTLSContext.SetVerifyMode(AMode: TSSLVerifyModes);
begin
  FVerifyMode := AMode;
  ApplyVerifyMode;
end;

function TMbedTLSContext.GetVerifyMode: TSSLVerifyModes;
begin
  Result := FVerifyMode;
end;

procedure TMbedTLSContext.SetVerifyDepth(ADepth: Integer);
begin
  FVerifyDepth := ADepth;
end;

function TMbedTLSContext.GetVerifyDepth: Integer;
begin
  Result := FVerifyDepth;
end;

procedure TMbedTLSContext.SetVerifyCallback(ACallback: TSSLVerifyCallback);
begin
  FVerifyCallback := ACallback;
end;

{ 密码套件配置 }

procedure TMbedTLSContext.SetCipherList(const ACipherList: string);
begin
  FCipherList := ACipherList;
end;

function TMbedTLSContext.GetCipherList: string;
begin
  Result := FCipherList;
end;

procedure TMbedTLSContext.SetCipherSuites(const ACipherSuites: string);
begin
  FCipherSuites := ACipherSuites;
end;

function TMbedTLSContext.GetCipherSuites: string;
begin
  Result := FCipherSuites;
end;

{ 会话管理 }

procedure TMbedTLSContext.SetSessionCacheMode(AEnabled: Boolean);
begin
  FSessionCacheEnabled := AEnabled;
end;

function TMbedTLSContext.GetSessionCacheMode: Boolean;
begin
  Result := FSessionCacheEnabled;
end;

procedure TMbedTLSContext.SetSessionTimeout(ATimeout: Integer);
begin
  FSessionTimeout := ATimeout;
end;

function TMbedTLSContext.GetSessionTimeout: Integer;
begin
  Result := FSessionTimeout;
end;

procedure TMbedTLSContext.SetSessionCacheSize(ASize: Integer);
begin
  FSessionCacheSize := ASize;
end;

function TMbedTLSContext.GetSessionCacheSize: Integer;
begin
  Result := FSessionCacheSize;
end;

{ 高级选项 }

procedure TMbedTLSContext.SetOptions(const AOptions: TSSLOptions);
begin
  FOptions := AOptions;
end;

function TMbedTLSContext.GetOptions: TSSLOptions;
begin
  Result := FOptions;
end;

procedure TMbedTLSContext.SetServerName(const AServerName: string);
begin
  FServerName := AServerName;
end;

function TMbedTLSContext.GetServerName: string;
begin
  Result := FServerName;
end;

procedure TMbedTLSContext.SetALPNProtocols(const AProtocols: string);
begin
  FALPNProtocols := AProtocols;
end;

function TMbedTLSContext.GetALPNProtocols: string;
begin
  Result := FALPNProtocols;
end;

procedure TMbedTLSContext.SetCertVerifyFlags(AFlags: TSSLCertVerifyFlags);
begin
  FCertVerifyFlags := AFlags;
end;

function TMbedTLSContext.GetCertVerifyFlags: TSSLCertVerifyFlags;
begin
  Result := FCertVerifyFlags;
end;

procedure TMbedTLSContext.SetPasswordCallback(ACallback: TSSLPasswordCallback);
begin
  FPasswordCallback := ACallback;
end;

procedure TMbedTLSContext.SetInfoCallback(ACallback: TSSLInfoCallback);
begin
  FInfoCallback := ACallback;
end;

{ 创建连接 }

function TMbedTLSContext.CreateConnection(ASocket: THandle): ISSLConnection;
begin
  RequireValidContext('CreateConnection');
  raise ESSLException.Create('CreateConnection not yet implemented for MbedTLS');
end;

function TMbedTLSContext.CreateConnection(AStream: TStream): ISSLConnection;
begin
  RequireValidContext('CreateConnection');
  raise ESSLException.Create('Stream-based connections not yet implemented for MbedTLS');
end;

{ 状态查询 }

function TMbedTLSContext.IsValid: Boolean;
begin
  Result := FSSLConfig <> nil;
end;

function TMbedTLSContext.GetNativeHandle: Pointer;
begin
  Result := FSSLConfig;
end;

procedure TMbedTLSContext.ConfigureSecureDefaults;
begin
  FProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
  FVerifyMode := [sslVerifyPeer];
  FVerifyDepth := 4;
  ApplyVerifyMode;
end;

end.
