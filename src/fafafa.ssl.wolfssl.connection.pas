{**
 * Unit: fafafa.ssl.wolfssl.connection
 * Purpose: WolfSSL standalone compatibility shim
 *
 * 保留历史公开类名 `TWolfSSLConnection`，但不再维护第二套完整连接实现。
 * 当前实现是一个薄兼容层：直接委托给 runtime path
 * `ISSLContext.CreateConnection(...)` 创建的真实 WolfSSL 连接对象。
 * 兼容策略：保留公开类名作为历史入口，但 runtime 真相源固定在 `fafafa.ssl.wolfssl.context`。
 *
 * 这样可以避免 standalone 单元与 runtime path 在 SNI / connection info /
 * native handle 等语义上再次漂移。
 *}

unit fafafa.ssl.wolfssl.connection;

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
  TWolfSSLConnection = class(TInterfacedObject, ISSLConnection, ISSLClientConnection,
    ISSLNativeHandleAccess)
  private
    FInner: ISSLConnection;
    FClientConnection: ISSLClientConnection;
    FNativeAccess: ISSLNativeHandleAccess;

    procedure RequireWolfSSLContext(AContext: ISSLContext; const AMethodName: string);
    procedure InitializeWithSocket(AContext: ISSLContext; ASocket: THandle);
    procedure InitializeWithStream(AContext: ISSLContext; AStream: TStream);
    function GetNativeWolfSSL: PWOLFSSL;
  public
    constructor Create(AContext: ISSLContext; ASocket: THandle); overload;
    constructor Create(AContext: ISSLContext; AStream: TStream); overload;
    destructor Destroy; override;

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
    function GetError(ARet: Integer): TSSLErrorCode;
    function GetConnectionInfo: TSSLConnectionInfo;
    function GetProtocolVersion: TSSLProtocolVersion;
    function GetCipherName: string;
    function GetPeerCertificate: ISSLCertificate;
    function GetPeerCertificateChain: TSSLCertificateArray;
    function GetVerifyResult: Integer;
    function GetVerifyResultString: string;
    function GetSession: ISSLSession;
    procedure SetSession(ASession: ISSLSession);
    function IsSessionReused: Boolean;
    function GetSelectedALPNProtocol: string;
    function IsConnected: Boolean;
    function GetState: string;
    function GetStateString: string;
    procedure SetTimeout(ATimeout: Integer);
    function GetTimeout: Integer;
    procedure SetBlocking(ABlocking: Boolean);
    function GetBlocking: Boolean;
    function GetContext: ISSLContext;
    function GetHealthStatus: TSSLHealthStatus;
    function IsHealthy: Boolean;
    function GetDiagnosticInfo: TSSLDiagnosticInfo;
    function GetPerformanceMetrics: TSSLPerformanceMetrics;
    function GetOCSPStaplingEnabled: Boolean;
    function GetOCSPResponse: TBytes;
    function IsOCSPResponseVerified: Boolean;
    function GetOCSPResponseStatus: string;

    procedure SetServerName(const AServerName: string);
    function GetServerName: string;

    function GetNativeHandle: Pointer;
    function GetBackendType: TSSLLibraryType;
    function IsNativeHandleValid: Boolean;

    function GetNegotiatedProtocol: TSSLProtocolVersion;
    function GetNegotiatedCipher: string;
    function GetNegotiatedALPN: string;
    function GetLastError: Integer;
    function GetLastErrorString: string;
  end;

implementation

procedure TWolfSSLConnection.RequireWolfSSLContext(AContext: ISSLContext; const AMethodName: string);
var
  LContextNative: ISSLNativeHandleAccess;
begin
  if AContext = nil then
    raise ESSLException.CreateWithContext(
      'WolfSSL context is required',
      sslErrInvalidParam,
      AMethodName
    );

  if not Supports(AContext, ISSLNativeHandleAccess, LContextNative) or
    (LContextNative.GetBackendType <> sslWolfSSL) then
    raise ESSLException.CreateWithContext(
      'WolfSSL context is required',
      sslErrInvalidParam,
      AMethodName
    );
end;

procedure TWolfSSLConnection.InitializeWithSocket(AContext: ISSLContext; ASocket: THandle);
begin
  RequireWolfSSLContext(AContext, 'TWolfSSLConnection.Create');
  FInner := AContext.CreateConnection(ASocket);

  if FInner = nil then
    raise ESSLException.CreateWithContext(
      'Failed to create WolfSSL runtime connection',
      sslErrConnection,
      'TWolfSSLConnection.Create'
    );

  if not Supports(FInner, ISSLClientConnection, FClientConnection) then
    raise ESSLException.CreateWithContext(
      'WolfSSL runtime connection does not support per-connection server name',
      sslErrUnsupported,
      'TWolfSSLConnection.Create'
    );

  if not Supports(FInner, ISSLNativeHandleAccess, FNativeAccess) then
    raise ESSLException.CreateWithContext(
      'WolfSSL runtime connection does not expose native handle access',
      sslErrUnsupported,
      'TWolfSSLConnection.Create'
    );
end;

procedure TWolfSSLConnection.InitializeWithStream(AContext: ISSLContext; AStream: TStream);
begin
  RequireWolfSSLContext(AContext, 'TWolfSSLConnection.Create');
  FInner := AContext.CreateConnection(AStream);

  if FInner = nil then
    raise ESSLException.CreateWithContext(
      'Failed to create WolfSSL runtime connection',
      sslErrConnection,
      'TWolfSSLConnection.Create'
    );

  if not Supports(FInner, ISSLClientConnection, FClientConnection) then
    raise ESSLException.CreateWithContext(
      'WolfSSL runtime connection does not support per-connection server name',
      sslErrUnsupported,
      'TWolfSSLConnection.Create'
    );

  if not Supports(FInner, ISSLNativeHandleAccess, FNativeAccess) then
    raise ESSLException.CreateWithContext(
      'WolfSSL runtime connection does not expose native handle access',
      sslErrUnsupported,
      'TWolfSSLConnection.Create'
    );
end;

function TWolfSSLConnection.GetNativeWolfSSL: PWOLFSSL;
begin
  Result := PWOLFSSL(GetNativeHandle);
end;

constructor TWolfSSLConnection.Create(AContext: ISSLContext; ASocket: THandle);
begin
  inherited Create;
  InitializeWithSocket(AContext, ASocket);
end;

constructor TWolfSSLConnection.Create(AContext: ISSLContext; AStream: TStream);
begin
  inherited Create;
  InitializeWithStream(AContext, AStream);
end;

destructor TWolfSSLConnection.Destroy;
begin
  FNativeAccess := nil;
  FClientConnection := nil;
  FInner := nil;
  inherited Destroy;
end;

function TWolfSSLConnection.Connect: Boolean;
begin
  Result := FInner.Connect;
end;

function TWolfSSLConnection.Accept: Boolean;
begin
  Result := FInner.Accept;
end;

function TWolfSSLConnection.Shutdown: Boolean;
begin
  Result := FInner.Shutdown;
end;

procedure TWolfSSLConnection.Close;
begin
  FInner.Close;
end;

function TWolfSSLConnection.DoHandshake: TSSLHandshakeState;
begin
  Result := FInner.DoHandshake;
end;

function TWolfSSLConnection.IsHandshakeComplete: Boolean;
begin
  Result := FInner.IsHandshakeComplete;
end;

function TWolfSSLConnection.Renegotiate: Boolean;
begin
  Result := FInner.Renegotiate;
end;

function TWolfSSLConnection.Read(var ABuffer; ACount: Integer): Integer;
begin
  Result := FInner.Read(ABuffer, ACount);
end;

function TWolfSSLConnection.Write(const ABuffer; ACount: Integer): Integer;
begin
  Result := FInner.Write(ABuffer, ACount);
end;

function TWolfSSLConnection.ReadString(out AStr: string): Boolean;
begin
  Result := FInner.ReadString(AStr);
end;

function TWolfSSLConnection.WriteString(const AStr: string): Boolean;
begin
  Result := FInner.WriteString(AStr);
end;

function TWolfSSLConnection.WantRead: Boolean;
begin
  Result := FInner.WantRead;
end;

function TWolfSSLConnection.WantWrite: Boolean;
begin
  Result := FInner.WantWrite;
end;

function TWolfSSLConnection.GetError(ARet: Integer): TSSLErrorCode;
begin
  Result := FInner.GetError(ARet);
end;

function TWolfSSLConnection.GetConnectionInfo: TSSLConnectionInfo;
begin
  Result := FInner.GetConnectionInfo;
end;

function TWolfSSLConnection.GetProtocolVersion: TSSLProtocolVersion;
begin
  Result := FInner.GetProtocolVersion;
end;

function TWolfSSLConnection.GetCipherName: string;
begin
  Result := FInner.GetCipherName;
end;

function TWolfSSLConnection.GetPeerCertificate: ISSLCertificate;
begin
  Result := FInner.GetPeerCertificate;
end;

function TWolfSSLConnection.GetPeerCertificateChain: TSSLCertificateArray;
begin
  Result := FInner.GetPeerCertificateChain;
end;

function TWolfSSLConnection.GetVerifyResult: Integer;
begin
  Result := FInner.GetVerifyResult;
end;

function TWolfSSLConnection.GetVerifyResultString: string;
begin
  Result := FInner.GetVerifyResultString;
end;

function TWolfSSLConnection.GetSession: ISSLSession;
begin
  Result := FInner.GetSession;
end;

procedure TWolfSSLConnection.SetSession(ASession: ISSLSession);
begin
  FInner.SetSession(ASession);
end;

function TWolfSSLConnection.IsSessionReused: Boolean;
begin
  Result := FInner.IsSessionReused;
end;

function TWolfSSLConnection.GetSelectedALPNProtocol: string;
begin
  Result := FInner.GetSelectedALPNProtocol;
end;

function TWolfSSLConnection.IsConnected: Boolean;
begin
  Result := FInner.IsConnected;
end;

function TWolfSSLConnection.GetState: string;
begin
  Result := FInner.GetState;
end;

function TWolfSSLConnection.GetStateString: string;
begin
  Result := FInner.GetStateString;
end;

procedure TWolfSSLConnection.SetTimeout(ATimeout: Integer);
begin
  FInner.SetTimeout(ATimeout);
end;

function TWolfSSLConnection.GetTimeout: Integer;
begin
  Result := FInner.GetTimeout;
end;

procedure TWolfSSLConnection.SetBlocking(ABlocking: Boolean);
begin
  FInner.SetBlocking(ABlocking);
end;

function TWolfSSLConnection.GetBlocking: Boolean;
begin
  Result := FInner.GetBlocking;
end;

function TWolfSSLConnection.GetContext: ISSLContext;
begin
  Result := FInner.GetContext;
end;

function TWolfSSLConnection.GetHealthStatus: TSSLHealthStatus;
begin
  Result := FInner.GetHealthStatus;
end;

function TWolfSSLConnection.IsHealthy: Boolean;
begin
  Result := FInner.IsHealthy;
end;

function TWolfSSLConnection.GetDiagnosticInfo: TSSLDiagnosticInfo;
begin
  Result := FInner.GetDiagnosticInfo;
end;

function TWolfSSLConnection.GetPerformanceMetrics: TSSLPerformanceMetrics;
begin
  Result := FInner.GetPerformanceMetrics;
end;

function TWolfSSLConnection.GetOCSPStaplingEnabled: Boolean;
begin
  Result := FInner.GetOCSPStaplingEnabled;
end;

function TWolfSSLConnection.GetOCSPResponse: TBytes;
begin
  Result := FInner.GetOCSPResponse;
end;

function TWolfSSLConnection.IsOCSPResponseVerified: Boolean;
begin
  Result := FInner.IsOCSPResponseVerified;
end;

function TWolfSSLConnection.GetOCSPResponseStatus: string;
begin
  Result := FInner.GetOCSPResponseStatus;
end;

procedure TWolfSSLConnection.SetServerName(const AServerName: string);
begin
  FClientConnection.SetServerName(AServerName);
end;

function TWolfSSLConnection.GetServerName: string;
begin
  Result := FClientConnection.GetServerName;
end;

function TWolfSSLConnection.GetNativeHandle: Pointer;
begin
  Result := FNativeAccess.GetNativeHandle;
end;

function TWolfSSLConnection.GetBackendType: TSSLLibraryType;
begin
  Result := FNativeAccess.GetBackendType;
end;

function TWolfSSLConnection.IsNativeHandleValid: Boolean;
begin
  Result := FNativeAccess.IsNativeHandleValid;
end;

function TWolfSSLConnection.GetNegotiatedProtocol: TSSLProtocolVersion;
begin
  Result := GetProtocolVersion;
end;

function TWolfSSLConnection.GetNegotiatedCipher: string;
begin
  Result := GetCipherName;
end;

function TWolfSSLConnection.GetNegotiatedALPN: string;
begin
  Result := GetSelectedALPNProtocol;
end;

function TWolfSSLConnection.GetLastError: Integer;
var
  LNativeSSL: PWOLFSSL;
begin
  Result := 0;
  LNativeSSL := GetNativeWolfSSL;
  if (LNativeSSL <> nil) and Assigned(wolfSSL_get_error) then
    Result := wolfSSL_get_error(LNativeSSL, 0);
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

end.
