program test_mbedtls_connection;

{$mode ObjFPC}{$H+}

{
  MbedTLS TLS 连接测试

  测试场景:
  1. 连接公共 HTTPS 服务器
  2. 验证 TLS 1.2 握手
  3. 验证 TLS 1.3 握手 (如支持)
  4. 简单 HTTP GET 请求
}

uses
  SysUtils, Classes, Sockets,
  fafafa.ssl.base,
  fafafa.ssl.mbedtls.lib,
  fafafa.ssl.mbedtls.context,
  fafafa.ssl.mbedtls.connection;

const
  TEST_HOST = 'www.google.com';
  TEST_PORT = 443;

procedure TestSimpleConnection;
var
  LLib: TMbedTLSLibrary;
  LCtx: TMbedTLSContext;
  LConn: TMbedTLSConnection;
  LSock: TInetSocket;
  LRequest: string;
  LBuffer: array[0..4095] of Byte;
  LBytesRead: Integer;
begin
  WriteLn('================================================================================');
  WriteLn('MbedTLS Simple Connection Test');
  WriteLn('================================================================================');
  WriteLn;

  // 1. 初始化 Library
  WriteLn('1. Initializing MbedTLS library...');
  LLib := TMbedTLSLibrary.Create;
  if not LLib.Initialize then
  begin
    WriteLn('❌ Failed to initialize MbedTLS');
    LLib.Free;
    Halt(1);
  end;
  WriteLn('   ✅ Initialized: ', LLib.GetVersionString);
  WriteLn;

  // 2. 创建 Context
  WriteLn('2. Creating SSL context...');
  LCtx := TMbedTLSContext.Create(LLib, sslCtxClient);
  WriteLn('   ✅ Context created');
  WriteLn;

  // 3. 创建 TCP Socket
  WriteLn('3. Creating TCP socket...');
  LSock := TInetSocket.Create(TEST_HOST, TEST_PORT);
  try
    LSock.Connect;
    WriteLn('   ✅ Connected to ', TEST_HOST, ':', TEST_PORT);
    WriteLn;

    // 4. 创建 SSL Connection
    WriteLn('4. Creating SSL connection...');
    LConn := TMbedTLSConnection.Create(LCtx);
    try
      WriteLn('   ✅ SSL connection created');

      // 5. 设置 SNI
      WriteLn('5. Setting SNI...');
      if LConn.SetServerName(TEST_HOST) then
        WriteLn('   ✅ SNI set: ', TEST_HOST)
      else
        WriteLn('   ⚠️  SNI not set');
      WriteLn;

      // 6. TLS 握手
      WriteLn('6. Performing TLS handshake...');
      if LConn.Connect(LSock.Handle) then
      begin
        WriteLn('   ✅ Handshake successful!');
        WriteLn('   Protocol: ', Ord(LConn.GetProtocolVersion));
        WriteLn('   Cipher: ', LConn.GetCipherName);
        WriteLn;

        // 7. 发送 HTTP GET
        WriteLn('7. Sending HTTP GET request...');
        LRequest := 'GET / HTTP/1.1'#13#10 +
                    'Host: ' + TEST_HOST + #13#10 +
                    'Connection: close'#13#10#13#10;

        if LConn.Write(LRequest[1], Length(LRequest)) > 0 then
        begin
          WriteLn('   ✅ Request sent (', Length(LRequest), ' bytes)');
          WriteLn;

          // 8. 读取响应
          WriteLn('8. Reading response...');
          LBytesRead := LConn.Read(LBuffer, SizeOf(LBuffer));
          if LBytesRead > 0 then
          begin
            WriteLn('   ✅ Response received (', LBytesRead, ' bytes)');
            WriteLn('   First 100 bytes:');
            WriteLn('   ', Copy(string(PAnsiChar(@LBuffer[0])), 1, 100));
            WriteLn;
          end
          else
            WriteLn('   ⚠️  No response data');
        end
        else
          WriteLn('   ❌ Failed to send request');

        // 9. 关闭连接
        WriteLn('9. Closing SSL connection...');
        LConn.Shutdown;
        WriteLn('   ✅ SSL connection closed');
      end
      else
      begin
        WriteLn('   ❌ Handshake failed');
        WriteLn('   Error: ', LConn.GetLastErrorString);
      end;

    finally
      LConn.Free;
    end;

  finally
    LSock.Free;
  end;

  WriteLn;
  WriteLn('10. Finalizing library...');
  LLib.Finalize;
  LLib.Free;
  WriteLn('    ✅ Library finalized');
  WriteLn;

  WriteLn('================================================================================');
  WriteLn('🎉 Connection Test Complete!');
  WriteLn('================================================================================');
end;

begin
  try
    TestSimpleConnection;
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('❌ Fatal error: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
