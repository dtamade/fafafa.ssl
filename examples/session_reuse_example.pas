program session_reuse_example;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{ TLS 会话复用示例

  - 演示 in-process 的会话复用：GetSession / SetSession
  - 同时保存 Serialize() 的字节到文件（用于调试/持久化）

  注意:
  - 目前公共 API 没有“创建空 ISSLSession 实例”的后端无关工厂方法，
    因此从文件 Deserialize 到一个全新会话对象需要后端特定实现。
  - 推荐在进程内缓存 ISSLSession 对象并复用。
}

uses
  SysUtils, Classes, DateUtils,
  fafafa.ssl,
  fafafa.ssl.context.builder,
  fafafa.examples.tcp;

const
  BUFFER_SIZE = 8192;
  SESSION_FILE = 'saved_session.dat';

function BuildRequest(const AHost, APath: string): RawByteString;
begin
  Result := 'GET ' + APath + ' HTTP/1.1' + #13#10 +
            'Host: ' + AHost + #13#10 +
            'User-Agent: fafafa.ssl-session-reuse/1.0' + #13#10 +
            'Connection: close' + #13#10 +
            #13#10;
end;

function ReadSome(AConn: ISSLConnection): Integer;
var
  Buf: array[0..BUFFER_SIZE - 1] of Byte;
  N: Integer;
begin
  Result := 0;
  repeat
    N := AConn.Read(Buf[0], SizeOf(Buf));
    if N > 0 then
      Inc(Result, N);
  until N <= 0;
end;

function ConnectAndRequest(AContext: ISSLContext; const AHost, APath: string; APort: Word;
  AReuseSession: Boolean; var ACachedSession: ISSLSession; out ASessionReused: Boolean): Integer;
var
  Sock: TSocketHandle;
  Conn: ISSLConnection;
  ClientConn: ISSLClientConnection;
  Req: RawByteString;
  StartTime: TDateTime;
  Session: ISSLSession;
begin
  Result := -1;
  ASessionReused := False;

  Sock := INVALID_SOCKET;
  try
    Sock := ConnectTCP(AHost, APort);

    Conn := AContext.CreateConnection(THandle(Sock));
    if Conn = nil then
      Exit;

    // per-connection SNI/hostname
    if Supports(Conn, ISSLClientConnection, ClientConn) then
      ClientConn.SetServerName(AHost);

    if AReuseSession and (ACachedSession <> nil) then
      Conn.SetSession(ACachedSession);

    StartTime := Now;
    if not Conn.Connect then
      Exit;

    Req := BuildRequest(AHost, APath);
    if Length(Req) > 0 then
      Conn.Write(Req[1], Length(Req));

    ReadSome(Conn);

    Result := MilliSecondsBetween(Now, StartTime);

    try
      ASessionReused := Conn.IsSessionReused;
    except
      ASessionReused := False;
    end;

    Session := Conn.GetSession;
    if (ACachedSession = nil) and (Session <> nil) then
      ACachedSession := Session;

    Conn.Shutdown;
  finally
    CloseSocket(Sock);
  end;
end;

procedure SaveSessionToFile(ASession: ISSLSession);
var
  Data: TBytes;
  FS: TFileStream;
begin
  if ASession = nil then
    Exit;

  Data := ASession.Serialize;
  if Length(Data) = 0 then
    Exit;

  FS := TFileStream.Create(SESSION_FILE, fmCreate);
  try
    FS.WriteBuffer(Data[0], Length(Data));
  finally
    FS.Free;
  end;
end;

procedure ShowLoadSessionNote;
var
  FS: TFileStream;
  Data: TBytes;
begin
  WriteLn;
  WriteLn('=== Load Session Note ===');

  if not FileExists(SESSION_FILE) then
  begin
    WriteLn('No session file found: ', SESSION_FILE);
    Exit;
  end;

  FS := TFileStream.Create(SESSION_FILE, fmOpenRead);
  try
    SetLength(Data, FS.Size);
    if FS.Size > 0 then
      FS.ReadBuffer(Data[0], FS.Size);
  finally
    FS.Free;
  end;

  WriteLn('Loaded ', Length(Data), ' bytes from ', SESSION_FILE);
  WriteLn('NOTE: Creating a fresh ISSLSession instance for Deserialize is backend-specific today.');
  WriteLn('      Recommended: keep the ISSLSession object in memory and reuse it via SetSession().');
end;

procedure Run;
var
  Ctx: ISSLContext;
  Session: ISSLSession;
  Reused: Boolean;
  T1, T2: Integer;
  Clone: ISSLSession;
begin
  // Build a client context with system roots
  Ctx := TSSLContextBuilder.Create
    .WithTLS12And13
    .WithVerifyPeer
    .WithSystemRoots
    .BuildClient;

  // Session cache settings (best-effort; backend dependent)
  Ctx.SetSessionCacheMode(True);
  Ctx.SetSessionCacheSize(256);
  Ctx.SetSessionTimeout(600);

  Session := nil;

  WriteLn('==================================');
  WriteLn('TLS Session Reuse (in-process)');
  WriteLn('==================================');
  WriteLn;

  WriteLn('=== First connection (cold) ===');
  T1 := ConnectAndRequest(Ctx, 'www.google.com', '/', 443, False, Session, Reused);
  WriteLn('Duration: ', T1, ' ms, SessionReused: ', BoolToStr(Reused, True));

  if Session <> nil then
  begin
    WriteLn('Session ID: ', Session.GetID);
    SaveSessionToFile(Session);
    WriteLn('Saved serialized session to ', SESSION_FILE);

    Clone := Session.Clone;
    if Clone <> nil then
      WriteLn('Clone ID: ', Clone.GetID);
  end
  else
    WriteLn('No session returned by backend');

  WriteLn;
  WriteLn('=== Second connection (try resume) ===');
  T2 := ConnectAndRequest(Ctx, 'www.google.com', '/', 443, True, Session, Reused);
  WriteLn('Duration: ', T2, ' ms, SessionReused: ', BoolToStr(Reused, True));

  ShowLoadSessionNote;
end;

var
  NetErr: string;
begin
  if not InitNetwork(NetErr) then
  begin
    WriteLn('Network init failed: ', NetErr);
    Halt(1);
  end;

  try
    Run;
  finally
    CleanupNetwork;
  end;

  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
end.

