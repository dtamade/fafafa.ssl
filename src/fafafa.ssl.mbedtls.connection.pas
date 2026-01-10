{**
 * Unit: fafafa.ssl.mbedtls.connection
 * Purpose: MbedTLS 连接实现
 *
 * 实现 ISSLConnection 接口的 MbedTLS 后端。
 * 负责 TLS 握手、数据传输和连接管理。
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-10
 *}

unit fafafa.ssl.mbedtls.connection;

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
  TMbedTLSContext = class;

  { TMbedTLSConnection - MbedTLS 连接类 }
  TMbedTLSConnection = class(TInterfacedObject, ISSLConnection)
  private
    FContext: TMbedTLSContext;
    FSSLContext: Pmbedtls_ssl_context;
    FSocket: THandle;
    FServerName: string;
    FALPNProtocols: string;
    FNegotiatedALPN: string;
    FHandshakeComplete: Boolean;
    FTimeout: Integer;
    FBlocking: Boolean;

    procedure AllocateSSLContext;
    procedure FreeSSLContext;

  public
    constructor Create(AContext: TMbedTLSContext; ASocket: THandle);
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

implementation

uses
  fafafa.ssl.mbedtls.context,
  fafafa.ssl.mbedtls.certificate,
  fafafa.ssl.mbedtls.session;

const
  MBEDTLS_SSL_CONTEXT_SIZE = 1024;

{ TMbedTLSConnection }

constructor TMbedTLSConnection.Create(AContext: TMbedTLSContext; ASocket: THandle);
begin
  inherited Create;
  FContext := AContext;
  FSocket := ASocket;
  FSSLContext := nil;
  FServerName := '';
  FALPNProtocols := '';
  FNegotiatedALPN := '';
  FHandshakeComplete := False;
  FTimeout := 30000;
  FBlocking := True;

  AllocateSSLContext;
end;

destructor TMbedTLSConnection.Destroy;
begin
  FreeSSLContext;
  inherited Destroy;
end;

procedure TMbedTLSConnection.AllocateSSLContext;
begin
  if FSSLContext <> nil then
    FreeSSLContext;

  GetMem(FSSLContext, MBEDTLS_SSL_CONTEXT_SIZE);
  FillChar(FSSLContext^, MBEDTLS_SSL_CONTEXT_SIZE, 0);

  if Assigned(mbedtls_ssl_init) then
    mbedtls_ssl_init(FSSLContext);
end;

procedure TMbedTLSConnection.FreeSSLContext;
begin
  if FSSLContext <> nil then
  begin
    if Assigned(mbedtls_ssl_free) then
      mbedtls_ssl_free(FSSLContext);
    FreeMem(FSSLContext);
    FSSLContext := nil;
  end;
end;

function TMbedTLSConnection.Connect: Boolean;
var
  LResult: Integer;
begin
  Result := False;
  if FSSLContext = nil then Exit;
  if not Assigned(mbedtls_ssl_handshake) then Exit;

  LResult := mbedtls_ssl_handshake(FSSLContext);
  FHandshakeComplete := LResult = 0;
  Result := FHandshakeComplete;
end;

function TMbedTLSConnection.Accept: Boolean;
begin
  Result := Connect;
end;

function TMbedTLSConnection.Shutdown: Boolean;
var
  LResult: Integer;
begin
  Result := False;
  if FSSLContext = nil then Exit;
  if not Assigned(mbedtls_ssl_close_notify) then Exit;

  LResult := mbedtls_ssl_close_notify(FSSLContext);
  Result := LResult >= 0;
end;

procedure TMbedTLSConnection.Close;
begin
  Shutdown;
end;

function TMbedTLSConnection.DoHandshake: TSSLHandshakeState;
begin
  if FHandshakeComplete then
    Result := sslHsCompleted
  else if Connect then
    Result := sslHsCompleted
  else
    Result := sslHsFailed;
end;

function TMbedTLSConnection.IsHandshakeComplete: Boolean;
begin
  Result := FHandshakeComplete;
end;

function TMbedTLSConnection.Renegotiate: Boolean;
begin
  Result := False;
end;

function TMbedTLSConnection.Read(var ABuffer; ACount: Integer): Integer;
begin
  Result := -1;
  if FSSLContext = nil then Exit;
  if not Assigned(mbedtls_ssl_read) then Exit;

  Result := mbedtls_ssl_read(FSSLContext, @ABuffer, ACount);
end;

function TMbedTLSConnection.Write(const ABuffer; ACount: Integer): Integer;
begin
  Result := -1;
  if FSSLContext = nil then Exit;
  if not Assigned(mbedtls_ssl_write) then Exit;

  Result := mbedtls_ssl_write(FSSLContext, @ABuffer, ACount);
end;

function TMbedTLSConnection.ReadString(out AStr: string): Boolean;
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

function TMbedTLSConnection.WriteString(const AStr: string): Boolean;
var
  LWritten: Integer;
begin
  Result := False;
  if AStr = '' then Exit(True);
  LWritten := Write(AStr[1], Length(AStr));
  Result := LWritten = Length(AStr);
end;

function TMbedTLSConnection.WantRead: Boolean;
begin
  Result := GetLastError = MBEDTLS_ERR_SSL_WANT_READ;
end;

function TMbedTLSConnection.WantWrite: Boolean;
begin
  Result := GetLastError = MBEDTLS_ERR_SSL_WANT_WRITE;
end;

function TMbedTLSConnection.GetError(ARetCode: Integer): TSSLErrorCode;
begin
  Result := MbedTLSErrorToSSLError(ARetCode);
end;

function TMbedTLSConnection.GetLastError: Integer;
begin
  Result := 0;
end;

function TMbedTLSConnection.GetLastErrorString: string;
begin
  Result := '';
end;

function TMbedTLSConnection.GetConnectionInfo: TSSLConnectionInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.ProtocolVersion := GetProtocolVersion;
  Result.CipherSuite := GetCipherName;
end;

function TMbedTLSConnection.GetProtocolVersion: TSSLProtocolVersion;
begin
  Result := sslProtocolTLS12;
end;

function TMbedTLSConnection.GetCipherName: string;
begin
  Result := '';
  if FSSLContext = nil then Exit;
  if not Assigned(mbedtls_ssl_get_ciphersuite) then Exit;

  Result := string(mbedtls_ssl_get_ciphersuite(FSSLContext));
end;

function TMbedTLSConnection.IsConnected: Boolean;
begin
  Result := (FSSLContext <> nil) and FHandshakeComplete;
end;

function TMbedTLSConnection.GetPeerCertificate: ISSLCertificate;
begin
  Result := nil;
end;

function TMbedTLSConnection.GetPeerCertificateChain: TSSLCertificateArray;
begin
  SetLength(Result, 0);
end;

function TMbedTLSConnection.GetVerifyResult: Integer;
begin
  Result := 0;
  if FSSLContext = nil then Exit;
  if not Assigned(mbedtls_ssl_get_verify_result) then Exit;

  Result := mbedtls_ssl_get_verify_result(FSSLContext);
end;

function TMbedTLSConnection.GetVerifyResultString: string;
begin
  Result := '';
end;

function TMbedTLSConnection.GetSession: ISSLSession;
begin
  Result := TMbedTLSSession.FromContext(FSSLContext);
end;

procedure TMbedTLSConnection.SetSession(ASession: ISSLSession);
begin
  if ASession = nil then Exit;
  if FSSLContext = nil then Exit;
  if not Assigned(mbedtls_ssl_set_session) then Exit;

  mbedtls_ssl_set_session(FSSLContext, Pmbedtls_ssl_session(ASession.GetNativeHandle));
end;

function TMbedTLSConnection.IsSessionReused: Boolean;
begin
  Result := False;
end;

procedure TMbedTLSConnection.SetServerName(const AServerName: string);
begin
  FServerName := AServerName;
  if (FSSLContext <> nil) and (FServerName <> '') and Assigned(mbedtls_ssl_set_hostname) then
    mbedtls_ssl_set_hostname(FSSLContext, PAnsiChar(AnsiString(FServerName)));
end;

function TMbedTLSConnection.GetServerName: string;
begin
  Result := FServerName;
end;

function TMbedTLSConnection.GetSelectedALPNProtocol: string;
begin
  Result := FNegotiatedALPN;
  if (Result = '') and (FSSLContext <> nil) and Assigned(mbedtls_ssl_get_alpn_protocol) then
  begin
    Result := string(mbedtls_ssl_get_alpn_protocol(FSSLContext));
    FNegotiatedALPN := Result;
  end;
end;

function TMbedTLSConnection.GetNegotiatedProtocol: TSSLProtocolVersion;
begin
  Result := GetProtocolVersion;
end;

function TMbedTLSConnection.GetNegotiatedCipher: string;
begin
  Result := GetCipherName;
end;

function TMbedTLSConnection.GetNegotiatedALPN: string;
begin
  Result := GetSelectedALPNProtocol;
end;

function TMbedTLSConnection.GetState: string;
begin
  if FHandshakeComplete then
    Result := 'CONNECTED'
  else
    Result := 'DISCONNECTED';
end;

function TMbedTLSConnection.GetStateString: string;
begin
  Result := GetState;
end;

procedure TMbedTLSConnection.SetTimeout(ATimeout: Integer);
begin
  FTimeout := ATimeout;
end;

function TMbedTLSConnection.GetTimeout: Integer;
begin
  Result := FTimeout;
end;

procedure TMbedTLSConnection.SetBlocking(ABlocking: Boolean);
begin
  FBlocking := ABlocking;
end;

function TMbedTLSConnection.GetBlocking: Boolean;
begin
  Result := FBlocking;
end;

function TMbedTLSConnection.GetContext: ISSLContext;
begin
  Result := nil;
end;

function TMbedTLSConnection.GetNativeHandle: Pointer;
begin
  Result := FSSLContext;
end;

end.
