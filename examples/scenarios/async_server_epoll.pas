{**
 * Example: Async TLS Server with epoll
 * Purpose: 展示如何将 fafafa.ssl 集成到高性能事件循环
 *
 * 此示例演示:
 * - 非阻塞 TLS 连接
 * - epoll 事件循环集成
 * - WantRead/WantWrite 处理
 * - 多连接并发处理
 *
 * 适用场景: C10K+ 高并发服务器
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-11
 *}

program async_server_epoll;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  BaseUnix, Unix, Sockets,
  {$ENDIF}
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.openssl.backed;

const
  MAX_EVENTS = 64;
  LISTEN_PORT = 8443;
  BUFFER_SIZE = 4096;

  // epoll 常量
  EPOLLIN  = $001;
  EPOLLOUT = $004;
  EPOLLET  = $80000000;  // Edge-triggered
  EPOLL_CTL_ADD = 1;
  EPOLL_CTL_DEL = 2;
  EPOLL_CTL_MOD = 3;

type
  {$IFDEF UNIX}
  TEpollEvent = packed record
    events: Cardinal;
    data: record
      case Integer of
        0: (ptr: Pointer);
        1: (fd: Integer);
        2: (u32: Cardinal);
        3: (u64: QWord);
    end;
  end;
  PEpollEvent = ^TEpollEvent;
  {$ENDIF}

  // 连接状态
  TConnectionState = (
    csAccepting,      // 等待 TLS 握手
    csHandshaking,    // TLS 握手进行中
    csConnected,      // 已连接，可读写
    csClosing         // 正在关闭
  );

  // 客户端连接记录
  PClientConnection = ^TClientConnection;
  TClientConnection = record
    Socket: THandle;
    SSLConn: ISSLConnection;
    State: TConnectionState;
    ReadBuffer: array[0..BUFFER_SIZE-1] of Byte;
    ReadPos: Integer;
    WriteBuffer: array[0..BUFFER_SIZE-1] of Byte;
    WritePos: Integer;
    WriteLen: Integer;
  end;

var
  GLib: ISSLLibrary;
  GCtx: ISSLContext;
  GEpollFd: Integer;
  GListenSocket: THandle;
  GConnections: array of PClientConnection;
  GRunning: Boolean;

{$IFDEF UNIX}
// epoll 系统调用
function epoll_create(size: Integer): Integer; cdecl; external 'c' name 'epoll_create';
function epoll_ctl(epfd, op, fd: Integer; event: PEpollEvent): Integer; cdecl; external 'c' name 'epoll_ctl';
function epoll_wait(epfd: Integer; events: PEpollEvent; maxevents, timeout: Integer): Integer; cdecl; external 'c' name 'epoll_wait';
{$ENDIF}

procedure Log(const AMsg: string);
begin
  WriteLn('[', FormatDateTime('hh:nn:ss.zzz', Now), '] ', AMsg);
end;

function SetNonBlocking(ASocket: THandle): Boolean;
{$IFDEF UNIX}
var
  LFlags: Integer;
{$ENDIF}
begin
  {$IFDEF UNIX}
  LFlags := FpFcntl(ASocket, F_GETFL, 0);
  if LFlags = -1 then Exit(False);
  Result := FpFcntl(ASocket, F_SETFL, LFlags or O_NONBLOCK) <> -1;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

{$IFDEF UNIX}
function AddToEpoll(ASocket: THandle; AEvents: Cardinal; AData: Pointer): Boolean;
var
  LEv: TEpollEvent;
begin
  LEv.events := AEvents;
  LEv.data.ptr := AData;
  Result := epoll_ctl(GEpollFd, EPOLL_CTL_ADD, Integer(ASocket), @LEv) = 0;
end;

function ModifyEpoll(ASocket: THandle; AEvents: Cardinal; AData: Pointer): Boolean;
var
  LEv: TEpollEvent;
begin
  LEv.events := AEvents;
  LEv.data.ptr := AData;
  Result := epoll_ctl(GEpollFd, EPOLL_CTL_MOD, Integer(ASocket), @LEv) = 0;
end;

procedure RemoveFromEpoll(ASocket: THandle);
begin
  epoll_ctl(GEpollFd, EPOLL_CTL_DEL, Integer(ASocket), nil);
end;
{$ENDIF}

procedure FreeConnection(AConn: PClientConnection);
begin
  if AConn = nil then Exit;

  {$IFDEF UNIX}
  RemoveFromEpoll(AConn^.Socket);
  {$ENDIF}

  if AConn^.SSLConn <> nil then
  begin
    AConn^.SSLConn.Shutdown;
    AConn^.SSLConn := nil;
  end;

  if AConn^.Socket > 0 then
    CloseSocket(AConn^.Socket);

  Dispose(AConn);
end;

// 处理 TLS 握手（非阻塞）
function HandleHandshake(AConn: PClientConnection): Boolean;
begin
  Result := False;
  if AConn^.SSLConn = nil then Exit;

  // 尝试完成握手
  if AConn^.SSLConn.Accept then
  begin
    AConn^.State := csConnected;
    Log(Format('Client %d: TLS handshake complete', [AConn^.Socket]));
    Result := True;
  end
  else
  begin
    // 检查是否需要更多数据
    if AConn^.SSLConn.WantRead then
    begin
      {$IFDEF UNIX}
      ModifyEpoll(AConn^.Socket, EPOLLIN or EPOLLET, AConn);
      {$ENDIF}
      Result := True;  // 继续等待
    end
    else if AConn^.SSLConn.WantWrite then
    begin
      {$IFDEF UNIX}
      ModifyEpoll(AConn^.Socket, EPOLLOUT or EPOLLET, AConn);
      {$ENDIF}
      Result := True;  // 继续等待
    end
    else
    begin
      Log(Format('Client %d: Handshake failed - %s',
        [AConn^.Socket, AConn^.SSLConn.GetLastErrorString]));
    end;
  end;
end;

// 处理数据读取（非阻塞）
procedure HandleRead(AConn: PClientConnection);
var
  LBytesRead: Integer;
  LResponse: string;
begin
  if AConn^.SSLConn = nil then Exit;

  LBytesRead := AConn^.SSLConn.Read(AConn^.ReadBuffer[AConn^.ReadPos],
    BUFFER_SIZE - AConn^.ReadPos);

  if LBytesRead > 0 then
  begin
    Inc(AConn^.ReadPos, LBytesRead);
    Log(Format('Client %d: Read %d bytes', [AConn^.Socket, LBytesRead]));

    // 简单 HTTP 响应
    LResponse := 'HTTP/1.1 200 OK'#13#10 +
                 'Content-Type: text/plain'#13#10 +
                 'Connection: close'#13#10 +
                 'Content-Length: 13'#13#10#13#10 +
                 'Hello, TLS!'#13#10;

    Move(LResponse[1], AConn^.WriteBuffer[0], Length(LResponse));
    AConn^.WriteLen := Length(LResponse);
    AConn^.WritePos := 0;

    {$IFDEF UNIX}
    ModifyEpoll(AConn^.Socket, EPOLLOUT or EPOLLET, AConn);
    {$ENDIF}
  end
  else if AConn^.SSLConn.WantRead then
  begin
    // 需要更多数据，继续等待
  end
  else
  begin
    // 连接关闭或错误
    AConn^.State := csClosing;
  end;
end;

// 处理数据写入（非阻塞）
procedure HandleWrite(AConn: PClientConnection);
var
  LBytesWritten: Integer;
begin
  if AConn^.SSLConn = nil then Exit;
  if AConn^.WritePos >= AConn^.WriteLen then
  begin
    AConn^.State := csClosing;
    Exit;
  end;

  LBytesWritten := AConn^.SSLConn.Write(
    AConn^.WriteBuffer[AConn^.WritePos],
    AConn^.WriteLen - AConn^.WritePos);

  if LBytesWritten > 0 then
  begin
    Inc(AConn^.WritePos, LBytesWritten);
    Log(Format('Client %d: Wrote %d bytes', [AConn^.Socket, LBytesWritten]));

    if AConn^.WritePos >= AConn^.WriteLen then
      AConn^.State := csClosing;
  end
  else if AConn^.SSLConn.WantWrite then
  begin
    // 需要等待可写
  end
  else
  begin
    AConn^.State := csClosing;
  end;
end;

// 接受新连接
{$IFDEF UNIX}
function AcceptConnection: PClientConnection;
var
  LClientSocket: THandle;
  LClientAddr: TInetSockAddr;
  LAddrLen: TSockLen;
  LConn: PClientConnection;
begin
  Result := nil;
  LAddrLen := SizeOf(LClientAddr);

  LClientSocket := fpAccept(GListenSocket, @LClientAddr, @LAddrLen);
  if LClientSocket < 0 then Exit;

  // 设置非阻塞
  if not SetNonBlocking(LClientSocket) then
  begin
    CloseSocket(LClientSocket);
    Exit;
  end;

  // 创建连接记录
  New(LConn);
  FillChar(LConn^, SizeOf(TClientConnection), 0);
  LConn^.Socket := LClientSocket;
  LConn^.State := csAccepting;

  // 创建 SSL 连接
  LConn^.SSLConn := GCtx.CreateConnection(LClientSocket);
  if LConn^.SSLConn = nil then
  begin
    CloseSocket(LClientSocket);
    Dispose(LConn);
    Exit;
  end;

  // 设置非阻塞模式
  LConn^.SSLConn.SetBlocking(False);
  LConn^.State := csHandshaking;

  // 添加到 epoll
  if not AddToEpoll(LClientSocket, EPOLLIN or EPOLLET, LConn) then
  begin
    LConn^.SSLConn := nil;
    CloseSocket(LClientSocket);
    Dispose(LConn);
    Exit;
  end;

  Log(Format('Client %d: New connection accepted', [LClientSocket]));
  Result := LConn;
end;
{$ENDIF}

// 创建监听 socket
function CreateListenSocket(APort: Integer): THandle;
{$IFDEF UNIX}
var
  LSockAddr: TInetSockAddr;
  LOptVal: Integer;
{$ENDIF}
begin
  {$IFDEF UNIX}
  Result := fpSocket(AF_INET, SOCK_STREAM, 0);
  if Result < 0 then
  begin
    Log('Failed to create socket');
    Exit(-1);
  end;

  // SO_REUSEADDR
  LOptVal := 1;
  fpSetSockOpt(Result, SOL_SOCKET, SO_REUSEADDR, @LOptVal, SizeOf(LOptVal));

  // 绑定
  FillChar(LSockAddr, SizeOf(LSockAddr), 0);
  LSockAddr.sin_family := AF_INET;
  LSockAddr.sin_port := htons(APort);
  LSockAddr.sin_addr.s_addr := 0;  // INADDR_ANY

  if fpBind(Result, @LSockAddr, SizeOf(LSockAddr)) < 0 then
  begin
    Log('Failed to bind socket');
    CloseSocket(Result);
    Exit(-1);
  end;

  // 监听
  if fpListen(Result, 128) < 0 then
  begin
    Log('Failed to listen');
    CloseSocket(Result);
    Exit(-1);
  end;

  // 设置非阻塞
  if not SetNonBlocking(Result) then
  begin
    Log('Failed to set non-blocking');
    CloseSocket(Result);
    Exit(-1);
  end;
  {$ELSE}
  Result := -1;
  {$ENDIF}
end;

// 初始化 SSL
function InitializeSSL(const ACertFile, AKeyFile: string): Boolean;
begin
  Result := False;

  // 创建 SSL 库实例
  GLib := CreateOpenSSLLibrary;
  if GLib = nil then
  begin
    Log('Failed to create SSL library');
    Exit;
  end;

  if not GLib.Initialize then
  begin
    Log('Failed to initialize SSL library');
    Exit;
  end;

  Log('SSL Library: ' + GLib.GetVersionString);

  // 创建服务器上下文
  GCtx := GLib.CreateContext(sslCtxServer);
  if GCtx = nil then
  begin
    Log('Failed to create SSL context');
    Exit;
  end;

  // 加载证书和私钥
  if not GCtx.LoadCertificate(ACertFile) then
  begin
    Log('Failed to load certificate: ' + ACertFile);
    Exit;
  end;

  if not GCtx.LoadPrivateKey(AKeyFile) then
  begin
    Log('Failed to load private key: ' + AKeyFile);
    Exit;
  end;

  Log('SSL context initialized with certificate');
  Result := True;
end;

// 主事件循环
{$IFDEF UNIX}
procedure RunEventLoop;
var
  LEvents: array[0..MAX_EVENTS-1] of TEpollEvent;
  LNumEvents, I: Integer;
  LConn: PClientConnection;
  LListenData: Pointer;
begin
  // 将监听 socket 添加到 epoll
  LListenData := nil;  // 用 nil 标识监听 socket
  if not AddToEpoll(GListenSocket, EPOLLIN, LListenData) then
  begin
    Log('Failed to add listen socket to epoll');
    Exit;
  end;

  Log(Format('Server listening on port %d', [LISTEN_PORT]));
  Log('Press Ctrl+C to stop');
  WriteLn('');

  while GRunning do
  begin
    LNumEvents := epoll_wait(GEpollFd, @LEvents[0], MAX_EVENTS, 1000);

    if LNumEvents < 0 then
    begin
      if fpGetErrno = ESysEINTR then Continue;
      Log('epoll_wait error');
      Break;
    end;

    for I := 0 to LNumEvents - 1 do
    begin
      if LEvents[I].data.ptr = nil then
      begin
        // 监听 socket 有新连接
        repeat
          LConn := AcceptConnection;
        until LConn = nil;
      end
      else
      begin
        // 客户端连接事件
        LConn := PClientConnection(LEvents[I].data.ptr);

        case LConn^.State of
          csHandshaking:
            if not HandleHandshake(LConn) then
              FreeConnection(LConn);

          csConnected:
          begin
            if (LEvents[I].events and EPOLLIN) <> 0 then
              HandleRead(LConn);
            if (LEvents[I].events and EPOLLOUT) <> 0 then
              HandleWrite(LConn);
          end;

          csClosing:
            FreeConnection(LConn);
        end;

        // 检查是否需要关闭
        if (LConn <> nil) and (LConn^.State = csClosing) then
          FreeConnection(LConn);
      end;
    end;
  end;
end;
{$ENDIF}

// 信号处理
{$IFDEF UNIX}
procedure SignalHandler(ASig: cint); cdecl;
begin
  if ASig = SIGINT then
  begin
    WriteLn('');
    Log('Received SIGINT, shutting down...');
    GRunning := False;
  end;
end;
{$ENDIF}

// 主程序
begin
  WriteLn('===========================================');
  WriteLn(' Async TLS Server with epoll');
  WriteLn(' fafafa.ssl Example');
  WriteLn('===========================================');
  WriteLn('');

  {$IFNDEF UNIX}
  WriteLn('This example requires Unix/Linux (epoll)');
  WriteLn('For Windows, use IOCP instead.');
  Halt(1);
  {$ENDIF}

  {$IFDEF UNIX}
  // 设置信号处理
  fpSignal(SIGINT, @SignalHandler);

  // 初始化 SSL
  if not InitializeSSL('server.crt', 'server.key') then
  begin
    Log('SSL initialization failed');
    Halt(1);
  end;

  // 创建 epoll 实例
  GEpollFd := epoll_create(MAX_EVENTS);
  if GEpollFd < 0 then
  begin
    Log('Failed to create epoll instance');
    Halt(1);
  end;

  // 创建监听 socket
  GListenSocket := CreateListenSocket(LISTEN_PORT);
  if GListenSocket < 0 then
  begin
    Log('Failed to create listen socket');
    Halt(1);
  end;

  // 运行事件循环
  GRunning := True;
  RunEventLoop;

  // 清理
  Log('Cleaning up...');
  CloseSocket(GListenSocket);
  FpClose(GEpollFd);

  if GLib <> nil then
    GLib.Finalize;

  Log('Server stopped');
  {$ENDIF}
end.
