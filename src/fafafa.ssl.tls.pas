{
  fafafa.ssl.tls - Rust 风格 TLS 门面（Connector/Acceptor + Stream）

  目标:
    - 提供更贴近 rustls / tokio-rustls 的使用体验：
      * TSSLConnector：客户端连接器
      * TSSLAcceptor：服务端接收器
      * TSSLStream：将 ISSLConnection 封装为 TStream

  重要语义:
    - SNI/hostname 属于“连接级别”配置。
      使用 ISSLClientConnection.SetServerName，而不是把 hostname 存在共享 ISSLContext 上。

  版本: 1.0
  创建: 2025-12-31
}

unit fafafa.ssl.tls;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.exceptions;

type
  { TSSLStream - 将 ISSLConnection 封装为 TStream }
  TSSLStream = class(TStream)
  private
    FConnection: ISSLConnection;
  public
    constructor Create(AConnection: ISSLConnection);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;

    procedure Close;

    property Connection: ISSLConnection read FConnection;
  end;

  { TSSLConnector - 客户端连接器（Rust 风格门面） }
  TSSLConnector = record
  private
    FContext: ISSLContext;
    FTimeout: Integer;
    FBlocking: Boolean;
    FSession: ISSLSession;
    FSessionReuse: Boolean;

    procedure ApplyClientOptions(AConn: ISSLConnection; const AServerName: string);
  public
    class function FromContext(AContext: ISSLContext): TSSLConnector; static;

    function WithTimeout(AMs: Integer): TSSLConnector;
    function WithBlocking(ABlocking: Boolean): TSSLConnector;
    function WithSession(ASession: ISSLSession): TSSLConnector;
    function WithSessionReuse(AEnabled: Boolean): TSSLConnector;

    function ConnectSocket(ASocket: THandle; const AServerName: string): TSSLStream;
    function TryConnectSocket(ASocket: THandle; const AServerName: string;
      out AStream: TSSLStream): TSSLOperationResult;

    function ConnectStream(ATransport: TStream; const AServerName: string): TSSLStream;
    function TryConnectStream(ATransport: TStream; const AServerName: string;
      out AStream: TSSLStream): TSSLOperationResult;
  end;

  { TSSLAcceptor - 服务端接收器（Rust 风格门面） }
  TSSLAcceptor = record
  private
    FContext: ISSLContext;
    FTimeout: Integer;
    FBlocking: Boolean;

    procedure ApplyServerOptions(AConn: ISSLConnection);
  public
    class function FromContext(AContext: ISSLContext): TSSLAcceptor; static;

    function WithTimeout(AMs: Integer): TSSLAcceptor;
    function WithBlocking(ABlocking: Boolean): TSSLAcceptor;

    function AcceptSocket(ASocket: THandle): TSSLStream;
    function TryAcceptSocket(ASocket: THandle; out AStream: TSSLStream): TSSLOperationResult;

    function AcceptStream(ATransport: TStream): TSSLStream;
    function TryAcceptStream(ATransport: TStream; out AStream: TSSLStream): TSSLOperationResult;
  end;

implementation

{ TSSLStream }

constructor TSSLStream.Create(AConnection: ISSLConnection);
begin
  inherited Create;
  if AConnection = nil then
    raise ESSLException.CreateWithContext(
      'SSL connection is required',
      sslErrInvalidParam,
      'TSSLStream.Create'
    );
  FConnection := AConnection;
end;

destructor TSSLStream.Destroy;
begin
  try
    Close;
  except
    // best-effort cleanup
  end;
  inherited Destroy;
end;

function TSSLStream.Read(var Buffer; Count: Longint): Longint;
var
  R: Integer;
begin
  if FConnection = nil then
    Exit(0);

  R := FConnection.Read(Buffer, Count);
  if R < 0 then
    raise ESSLConnectionException.CreateWithContext(
      'TLS read failed',
      FConnection.GetError(R),
      'TSSLStream.Read'
    );
  Result := R;
end;

function TSSLStream.Write(const Buffer; Count: Longint): Longint;
var
  R: Integer;
begin
  if FConnection = nil then
    Exit(0);

  R := FConnection.Write(Buffer, Count);
  if R < 0 then
    raise ESSLConnectionException.CreateWithContext(
      'TLS write failed',
      FConnection.GetError(R),
      'TSSLStream.Write'
    );
  Result := R;
end;

function TSSLStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  // TLS/SSL 连接是流式的，不支持 seek。
  raise EStreamError.Create('TLS stream is not seekable');
end;

procedure TSSLStream.Close;
begin
  if FConnection <> nil then
  begin
    try
      FConnection.Shutdown;
    except
      // ignore
    end;
    try
      FConnection.Close;
    except
      // ignore
    end;
  end;
end;

{ TSSLConnector }

class function TSSLConnector.FromContext(AContext: ISSLContext): TSSLConnector;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.FContext := AContext;
  Result.FTimeout := SSL_DEFAULT_HANDSHAKE_TIMEOUT;
  Result.FBlocking := True;
  Result.FSession := nil;
  Result.FSessionReuse := True;
end;

function TSSLConnector.WithTimeout(AMs: Integer): TSSLConnector;
begin
  Result := Self;
  Result.FTimeout := AMs;
end;

function TSSLConnector.WithBlocking(ABlocking: Boolean): TSSLConnector;
begin
  Result := Self;
  Result.FBlocking := ABlocking;
end;

function TSSLConnector.WithSession(ASession: ISSLSession): TSSLConnector;
begin
  Result := Self;
  Result.FSession := ASession;
end;

function TSSLConnector.WithSessionReuse(AEnabled: Boolean): TSSLConnector;
begin
  Result := Self;
  Result.FSessionReuse := AEnabled;
end;

procedure TSSLConnector.ApplyClientOptions(AConn: ISSLConnection; const AServerName: string);
var
  ClientConn: ISSLClientConnection;
begin
  if AConn = nil then
    Exit;

  AConn.SetTimeout(FTimeout);
  AConn.SetBlocking(FBlocking);

  if FSessionReuse and (FSession <> nil) then
    AConn.SetSession(FSession);

  if AServerName <> '' then
  begin
    if Supports(AConn, ISSLClientConnection, ClientConn) then
      ClientConn.SetServerName(AServerName)
    else
      raise ESSLException.CreateWithContext(
        'Backend does not support per-connection server name',
        sslErrUnsupported,
        'TSSLConnector.ApplyClientOptions'
      );
  end;
end;

function TSSLConnector.TryConnectSocket(ASocket: THandle; const AServerName: string;
  out AStream: TSSLStream): TSSLOperationResult;
var
  Conn: ISSLConnection;
  VerifyRes: Integer;
  VerifyStr: string;
begin
  AStream := nil;

  if FContext = nil then
    Exit(TSSLOperationResult.Err(sslErrInvalidParam, 'Context is required'));

  if ASocket = 0 then
    Exit(TSSLOperationResult.Err(sslErrInvalidParam, 'Socket is required'));

  try
    Conn := FContext.CreateConnection(ASocket);
    if Conn = nil then
      Exit(TSSLOperationResult.Err(sslErrConnection, 'Failed to create connection'));

    ApplyClientOptions(Conn, AServerName);

    if not Conn.Connect then
    begin
      VerifyRes := Conn.GetVerifyResult;
      VerifyStr := Conn.GetVerifyResultString;

      try
        Conn.Close;
      except
        // best-effort cleanup
      end;

      if VerifyRes <> 0 then
        Exit(TSSLOperationResult.Err(sslErrVerificationFailed, 'Client handshake failed: ' + VerifyStr));
      Exit(TSSLOperationResult.Err(sslErrHandshake, 'Client handshake failed: ' + VerifyStr));
    end;

    AStream := TSSLStream.Create(Conn);
    Result := TSSLOperationResult.Ok;
  except
    on E: ESSLException do
      Result := TSSLOperationResult.Err(E.ErrorCode, E.Message);
    on E: Exception do
      Result := TSSLOperationResult.Err(sslErrGeneral, E.Message);
  end;
end;

function TSSLConnector.ConnectSocket(ASocket: THandle; const AServerName: string): TSSLStream;
var
  R: TSSLOperationResult;
begin
  R := TryConnectSocket(ASocket, AServerName, Result);
  if not R.Success then
    raise ESSLConnectionException.CreateWithContext(
      R.ErrorMessage,
      R.ErrorCode,
      'TSSLConnector.ConnectSocket'
    );
end;

function TSSLConnector.TryConnectStream(ATransport: TStream; const AServerName: string;
  out AStream: TSSLStream): TSSLOperationResult;
var
  Conn: ISSLConnection;
  VerifyRes: Integer;
  VerifyStr: string;
begin
  AStream := nil;

  if FContext = nil then
    Exit(TSSLOperationResult.Err(sslErrInvalidParam, 'Context is required'));

  if ATransport = nil then
    Exit(TSSLOperationResult.Err(sslErrInvalidParam, 'Stream is required'));

  try
    Conn := FContext.CreateConnection(ATransport);
    if Conn = nil then
      Exit(TSSLOperationResult.Err(sslErrConnection, 'Failed to create connection'));

    ApplyClientOptions(Conn, AServerName);

    if not Conn.Connect then
    begin
      VerifyRes := Conn.GetVerifyResult;
      VerifyStr := Conn.GetVerifyResultString;

      try
        Conn.Close;
      except
        // best-effort cleanup
      end;

      if VerifyRes <> 0 then
        Exit(TSSLOperationResult.Err(sslErrVerificationFailed, 'Client handshake failed: ' + VerifyStr));
      Exit(TSSLOperationResult.Err(sslErrHandshake, 'Client handshake failed: ' + VerifyStr));
    end;

    AStream := TSSLStream.Create(Conn);
    Result := TSSLOperationResult.Ok;
  except
    on E: ESSLException do
      Result := TSSLOperationResult.Err(E.ErrorCode, E.Message);
    on E: Exception do
      Result := TSSLOperationResult.Err(sslErrGeneral, E.Message);
  end;
end;

function TSSLConnector.ConnectStream(ATransport: TStream; const AServerName: string): TSSLStream;
var
  R: TSSLOperationResult;
begin
  R := TryConnectStream(ATransport, AServerName, Result);
  if not R.Success then
    raise ESSLConnectionException.CreateWithContext(
      R.ErrorMessage,
      R.ErrorCode,
      'TSSLConnector.ConnectStream'
    );
end;

{ TSSLAcceptor }

class function TSSLAcceptor.FromContext(AContext: ISSLContext): TSSLAcceptor;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.FContext := AContext;
  Result.FTimeout := SSL_DEFAULT_HANDSHAKE_TIMEOUT;
  Result.FBlocking := True;
end;

function TSSLAcceptor.WithTimeout(AMs: Integer): TSSLAcceptor;
begin
  Result := Self;
  Result.FTimeout := AMs;
end;

function TSSLAcceptor.WithBlocking(ABlocking: Boolean): TSSLAcceptor;
begin
  Result := Self;
  Result.FBlocking := ABlocking;
end;

procedure TSSLAcceptor.ApplyServerOptions(AConn: ISSLConnection);
begin
  if AConn = nil then
    Exit;

  AConn.SetTimeout(FTimeout);
  AConn.SetBlocking(FBlocking);
end;

function TSSLAcceptor.TryAcceptSocket(ASocket: THandle; out AStream: TSSLStream): TSSLOperationResult;
var
  Conn: ISSLConnection;
  VerifyRes: Integer;
  VerifyStr: string;
begin
  AStream := nil;

  if FContext = nil then
    Exit(TSSLOperationResult.Err(sslErrInvalidParam, 'Context is required'));

  if ASocket = 0 then
    Exit(TSSLOperationResult.Err(sslErrInvalidParam, 'Socket is required'));

  try
    Conn := FContext.CreateConnection(ASocket);
    if Conn = nil then
      Exit(TSSLOperationResult.Err(sslErrConnection, 'Failed to create connection'));

    ApplyServerOptions(Conn);

    if not Conn.Accept then
    begin
      VerifyRes := Conn.GetVerifyResult;
      VerifyStr := Conn.GetVerifyResultString;

      try
        Conn.Close;
      except
        // best-effort cleanup
      end;

      if VerifyRes <> 0 then
        Exit(TSSLOperationResult.Err(sslErrVerificationFailed, 'Server accept failed: ' + VerifyStr));
      Exit(TSSLOperationResult.Err(sslErrHandshake, 'Server accept failed: ' + VerifyStr));
    end;

    AStream := TSSLStream.Create(Conn);
    Result := TSSLOperationResult.Ok;
  except
    on E: ESSLException do
      Result := TSSLOperationResult.Err(E.ErrorCode, E.Message);
    on E: Exception do
      Result := TSSLOperationResult.Err(sslErrGeneral, E.Message);
  end;
end;

function TSSLAcceptor.AcceptSocket(ASocket: THandle): TSSLStream;
var
  R: TSSLOperationResult;
begin
  R := TryAcceptSocket(ASocket, Result);
  if not R.Success then
    raise ESSLConnectionException.CreateWithContext(
      R.ErrorMessage,
      R.ErrorCode,
      'TSSLAcceptor.AcceptSocket'
    );
end;

function TSSLAcceptor.TryAcceptStream(ATransport: TStream; out AStream: TSSLStream): TSSLOperationResult;
var
  Conn: ISSLConnection;
  VerifyRes: Integer;
  VerifyStr: string;
begin
  AStream := nil;

  if FContext = nil then
    Exit(TSSLOperationResult.Err(sslErrInvalidParam, 'Context is required'));

  if ATransport = nil then
    Exit(TSSLOperationResult.Err(sslErrInvalidParam, 'Stream is required'));

  try
    Conn := FContext.CreateConnection(ATransport);
    if Conn = nil then
      Exit(TSSLOperationResult.Err(sslErrConnection, 'Failed to create connection'));

    ApplyServerOptions(Conn);

    if not Conn.Accept then
    begin
      VerifyRes := Conn.GetVerifyResult;
      VerifyStr := Conn.GetVerifyResultString;

      try
        Conn.Close;
      except
        // best-effort cleanup
      end;

      if VerifyRes <> 0 then
        Exit(TSSLOperationResult.Err(sslErrVerificationFailed, 'Server accept failed: ' + VerifyStr));
      Exit(TSSLOperationResult.Err(sslErrHandshake, 'Server accept failed: ' + VerifyStr));
    end;

    AStream := TSSLStream.Create(Conn);
    Result := TSSLOperationResult.Ok;
  except
    on E: ESSLException do
      Result := TSSLOperationResult.Err(E.ErrorCode, E.Message);
    on E: Exception do
      Result := TSSLOperationResult.Err(sslErrGeneral, E.Message);
  end;
end;

function TSSLAcceptor.AcceptStream(ATransport: TStream): TSSLStream;
var
  R: TSSLOperationResult;
begin
  R := TryAcceptStream(ATransport, Result);
  if not R.Success then
    raise ESSLConnectionException.CreateWithContext(
      R.ErrorMessage,
      R.ErrorCode,
      'TSSLAcceptor.AcceptStream'
    );
end;

end.
