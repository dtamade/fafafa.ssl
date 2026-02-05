program test_mbedtls_simple_connection;

{$mode ObjFPC}{$H+}

{
  MbedTLS 简单 TLS 连接测试

  测试流程:
  1. 使用 TMbedTLSLibrary 创建库
  2. 创建 Client Context
  3. 连接 www.google.com:443
  4. 执行 TLS 握手
  5. 简单验证连接状态
}

uses
  SysUtils, TypInfo,
  fafafa.ssl.base,
  fafafa.ssl.mbedtls.lib,
  fafafa.ssl.mbedtls.context,
  fafafa.ssl.mbedtls.connection,
  fafafa.examples.tcp;

const
  TEST_HOST = 'www.google.com';
  TEST_PORT = 443;

procedure TestSimpleConnection;
var
  LLib: TMbedTLSLibrary;
  LCtx: TMbedTLSContext;
  LConn: ISSLConnection;
  LSock: TSocketHandle;
  LError: string;
begin
  WriteLn('================================================================================');
  WriteLn('MbedTLS Simple Connection Test');
  WriteLn('================================================================================');
  WriteLn;

  // 1. 初始化网络
  WriteLn('1. Initializing network...');
  if not InitNetwork(LError) then
  begin
    WriteLn('   ❌ Failed to initialize network: ', LError);
    Halt(1);
  end;
  WriteLn('   ✅ Network initialized');
  WriteLn;

  // 2. 初始化 MbedTLS
  WriteLn('2. Initializing MbedTLS library...');
  LLib := TMbedTLSLibrary.Create;
  try
    if not LLib.Initialize then
    begin
      WriteLn('   ❌ Failed to initialize MbedTLS');
      CleanupNetwork;
      Halt(1);
    end;
    WriteLn('   ✅ Initialized: ', LLib.GetVersionString);
    WriteLn;

    // 3. 创建 Context
    WriteLn('3. Creating SSL context...');
    LCtx := TMbedTLSContext.Create(LLib, sslCtxClient);
    WriteLn('   ✅ Context created');
    WriteLn;

    // 4. 创建 TCP Socket
    WriteLn('4. Connecting to ', TEST_HOST, ':', TEST_PORT, '...');
    try
      LSock := ConnectTCP(TEST_HOST, TEST_PORT);
      WriteLn('   ✅ TCP connected');
      WriteLn;
    except
      on E: Exception do
      begin
        WriteLn('   ❌ Failed to connect: ', E.Message);
        LLib.Finalize;
        CleanupNetwork;
        Halt(1);
      end;
    end;

    try
      // 5. 创建 SSL Connection
      WriteLn('5. Creating SSL connection...');
      LConn := LCtx.CreateConnection;
      WriteLn('   ✅ SSL connection created');
      WriteLn;

      // 6. 设置 SNI
      WriteLn('6. Setting SNI...');
      if LConn.SetServerName(TEST_HOST) then
        WriteLn('   ✅ SNI set: ', TEST_HOST)
      else
        WriteLn('   ⚠️  SNI not set');
      WriteLn;

      // 7. 绑定 Socket
      WriteLn('7. Binding socket...');
      if LConn.Attach(LSock) then
      begin
        WriteLn('   ✅ Socket attached');
        WriteLn;

        // 8. TLS 握手
        WriteLn('8. Performing TLS handshake...');
        if LConn.Connect then
        begin
          WriteLn('   ✅ Handshake successful!');
          WriteLn('   Protocol Version: ', GetEnumName(TypeInfo(TSSLProtocolVersion), Ord(LConn.GetProtocolVersion)));
          WriteLn('   Cipher: ', LConn.GetCipherName);
          WriteLn('   Peer Certificate: ', LConn.GetPeerCertificate <> nil);
          WriteLn('   Verify Result: ', LConn.GetVerifyResult);
          WriteLn;

          // 9. 关闭连接
          WriteLn('9. Closing connection...');
          LConn.Shutdown;
          WriteLn('   ✅ SSL connection closed');
        end
        else
        begin
          WriteLn('   ❌ Handshake failed');
          WriteLn('   Error: ', LConn.GetLastErrorString);
        end;
      end
      else
        WriteLn('   ❌ Failed to attach socket');

    finally
      CloseSocket(LSock);
    end;

    WriteLn;
    WriteLn('10. Finalizing library...');
    LLib.Finalize;
    WriteLn('    ✅ Library finalized');

  finally
    LLib.Free;
    CleanupNetwork;
  end;

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
      CleanupNetwork;
      Halt(1);
    end;
  end;
end.
