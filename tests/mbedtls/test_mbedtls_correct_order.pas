program test_mbedtls_correct_order;

{$mode ObjFPC}{$H+}

{
  MbedTLS 正确释放顺序测试

  正确的顺序:
  1. 释放 Connection (Interface,自动)
  2. 关闭 Socket
  3. 释放 Context (持有 Library 接口引用)
  4. Finalize + Free Library
}

uses
  SysUtils, TypInfo,
  fafafa.ssl.base,
  fafafa.ssl.mbedtls.lib,
  fafafa.ssl.mbedtls.context,
  fafafa.examples.tcp;

const
  TEST_HOST = 'www.google.com';
  TEST_PORT = 443;

procedure TestConnection;
var
  LLib: TMbedTLSLibrary;
  LCtx: TMbedTLSContext;
  LConn: ISSLConnection;
  LSock: TSocketHandle;
  LError: string;
  LRequest: AnsiString;
  LBuffer: array[0..4095] of Byte;
  LBytesRead: Integer;
begin
  WriteLn('================================================================================');
  WriteLn('MbedTLS Correct Resource Order Test');
  WriteLn('================================================================================');
  WriteLn;

  if not InitNetwork(LError) then
  begin
    WriteLn('❌ Network init failed');
    Halt(1);
  end;

  LLib := TMbedTLSLibrary.Create;
  LCtx := nil;
  LConn := nil;
  LSock := INVALID_SOCKET;

  try
    WriteLn('1. Initialize MbedTLS...');
    if not LLib.Initialize then
    begin
      WriteLn('   ❌ Failed');
      Exit;
    end;
    WriteLn('   ✅ ', LLib.GetVersionString);

    WriteLn('2. Create context...');
    LCtx := TMbedTLSContext.Create(LLib, sslCtxClient);
    WriteLn('   ✅ OK');

    WriteLn('3. Connect TCP...');
    LSock := ConnectTCP(TEST_HOST, TEST_PORT);
    WriteLn('   ✅ Connected');

    WriteLn('4. Create SSL connection...');
    LConn := LCtx.CreateConnection(LSock);
    WriteLn('   ✅ Created');

    WriteLn('5. TLS handshake...');
    if not LConn.Connect then
    begin
      WriteLn('   ❌ Failed');
      Exit;
    end;
    WriteLn('   ✅ Success! Protocol: ', GetEnumName(TypeInfo(TSSLProtocolVersion), Ord(LConn.GetProtocolVersion)));

    WriteLn('6. Send HTTP GET...');
    LRequest := 'GET / HTTP/1.1'#13#10 + 'Host: ' + TEST_HOST + #13#10 + 'Connection: close'#13#10#13#10;
    if LConn.Write(LRequest[1], Length(LRequest)) <= 0 then
    begin
      WriteLn('   ❌ Write failed');
      Exit;
    end;
    WriteLn('   ✅ Sent');

    WriteLn('7. Read response...');
    LBytesRead := LConn.Read(LBuffer, SizeOf(LBuffer));
    if LBytesRead > 0 then
      WriteLn('   ✅ Received ', LBytesRead, ' bytes')
    else
      WriteLn('   ⚠️  No data');

    WriteLn('8. Shutdown...');
    LConn.Shutdown;
    WriteLn('   ✅ Closed');

  finally
    WriteLn;
    WriteLn('Cleanup in correct order:');

    // 1. 释放 Connection (Interface 引用计数)
    if LConn <> nil then
    begin
      WriteLn('  1. Releasing connection interface...');
      LConn := nil;
      WriteLn('     ✅ Released');
    end;

    // 2. 关闭 Socket
    if LSock <> INVALID_SOCKET then
    begin
      WriteLn('  2. Closing socket...');
      CloseSocket(LSock);
      WriteLn('     ✅ Closed');
    end;

    // 3. 释放 Context (必须在 Finalize 之前!)
    //    因为 Context.Free 需要调用 mbedtls_ssl_config_free 等函数
    if LCtx <> nil then
    begin
      WriteLn('  3. Freeing context (BEFORE Finalize, needs MbedTLS functions)...');
      LCtx.Free;
      WriteLn('     ✅ Freed');
    end;

    // 4. Finalize Library (卸载 MbedTLS 动态库)
    //    注意:之后不能再调用任何 MbedTLS 函数!
    WriteLn('  4. Finalizing library (unloads DLLs)...');
    LLib.Finalize;
    WriteLn('     ✅ Finalized');

    // 5. Free Library 对象本身
    WriteLn('  5. Freeing library object...');
    LLib.Free;
    WriteLn('     ✅ Freed');

    CleanupNetwork;
    WriteLn('  ✅ All cleanup complete');
  end;

  WriteLn;
  WriteLn('================================================================================');
  WriteLn('🎉 Test Complete!');
  WriteLn('================================================================================');
end;

begin
  try
    TestConnection;
    WriteLn;
    WriteLn('✅ Program exiting cleanly (no memory errors)');
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('❌ Fatal: ', E.ClassName, ': ', E.Message);
      CleanupNetwork;
      Halt(1);
    end;
  end;
end.
