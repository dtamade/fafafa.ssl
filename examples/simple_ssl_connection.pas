program simple_ssl_connection;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{
  这个示例展示 fafafa.ssl 的推荐使用方式：

  1) 用户自己创建 TCP socket（这里使用 fafafa.examples.tcp 跨平台封装）
  2) 将 socket 交给 fafafa.ssl 建立 TLS
  3) 应用层协议（如 HTTP）仍由用户处理
}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.context.builder,
  fafafa.examples.tcp;

const
  TARGET_HOST = 'example.com';
  TARGET_PORT = 443;

procedure SimpleSSLConnection;
var
  LContext: ISSLContext;
  LConnector: TSSLConnector;
  LTLS: TSSLStream;
  LSocket: TSocketHandle;
  LRequest: RawByteString;
  LBuffer: array[0..4095] of Byte;
  LBytesRead: Longint;
  LChunk: string;
  LNetErr: string;
begin
  WriteLn('╔══════════════════════════════════════════════════════════════╗');
  WriteLn('║   简单 SSL/TLS 连接示例                                     ║');
  WriteLn('║   展示如何结合 socket 和 fafafa.ssl                          ║');
  WriteLn('╚══════════════════════════════════════════════════════════════╝');
  WriteLn;

  if not InitNetwork(LNetErr) then
    raise Exception.Create('网络初始化失败: ' + LNetErr);

  LSocket := INVALID_SOCKET;
  LTLS := nil;
  try
    WriteLn('[1/5] 创建 TCP 连接...');
    LSocket := ConnectTCP(TARGET_HOST, TARGET_PORT);
    WriteLn('      ✓ TCP 连接已建立');
    WriteLn;

    WriteLn('[2/5] 创建 TLS 上下文...');
    LContext := TSSLContextBuilder.Create
      .WithTLS12And13
      .WithVerifyPeer
      .WithSystemRoots
      .BuildClient;
    WriteLn('      ✓ TLS 上下文已创建');
    WriteLn;

    WriteLn('[3/5] 将 socket 交给 fafafa.ssl 并握手...');
    LConnector := TSSLConnector.FromContext(LContext).WithTimeout(15000);
    LTLS := LConnector.ConnectSocket(THandle(LSocket), TARGET_HOST);
    WriteLn('      ✓ TLS 握手成功');
    WriteLn('      协议: ', ProtocolVersionToString(LTLS.Connection.GetProtocolVersion));
    WriteLn('      密码套件: ', LTLS.Connection.GetCipherName);
    WriteLn;

    WriteLn('[4/5] 发送 HTTP 请求...');
    LRequest :=
      'GET / HTTP/1.1'#13#10 +
      'Host: ' + TARGET_HOST + #13#10 +
      'User-Agent: fafafa.ssl-example/1.0'#13#10 +
      'Connection: close'#13#10 +
      #13#10;

    if Length(LRequest) > 0 then
      LTLS.WriteBuffer(LRequest[1], Length(LRequest));
    WriteLn('      ✓ 请求已发送 (', Length(LRequest), ' 字节)');
    WriteLn;

    WriteLn('[5/5] 接收响应...');
    repeat
      LBytesRead := LTLS.Read(LBuffer[0], SizeOf(LBuffer));
      if LBytesRead > 0 then
      begin
        SetString(LChunk, PAnsiChar(@LBuffer[0]), LBytesRead);
        Write(LChunk);
      end;
    until LBytesRead <= 0;
    WriteLn;
    WriteLn('      ✓ 响应已接收');
    WriteLn;

    WriteLn('════════════════════════════════════════════════════════════');
    WriteLn('总结：');
    WriteLn('  1. 用户创建并管理 socket');
    WriteLn('  2. fafafa.ssl 专注于 TLS 握手与加密传输');
    WriteLn('  3. 应用层协议逻辑保持在业务侧');
    WriteLn('════════════════════════════════════════════════════════════');
  finally
    if LTLS <> nil then
      LTLS.Free;
    CloseSocket(LSocket);
    CleanupNetwork;
  end;
end;

begin
  WriteLn;
  try
    SimpleSSLConnection;
  except
    on E: Exception do
    begin
      WriteLn('❌ 错误: ', E.Message);
      Halt(1);
    end;
  end;
end.
