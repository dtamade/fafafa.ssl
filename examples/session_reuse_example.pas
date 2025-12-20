program session_reuse_example;

{$mode objfpc}{$H+}

{
  会话复用示例
  演示如何保存和恢复SSL/TLS会话以提高性能
}

uses
  SysUtils, Classes,
  fafafa.ssl.factory,
  fafafa.ssl.base;

procedure SaveSessionExample(const Hostname: string; Port: Word);
var
  SSLLib: ISSLLibrary;
  Context: ISSLContext;
  Connection: ISSLConnection;
  Session: ISSLSession;
  SessionData: TBytes;
  SessionFile: TFileStream;
  SocketHandle: THandle;
begin
  WriteLn('==================================');
  WriteLn('Save Session Example');
  WriteLn('==================================');
  WriteLn;
  
  // 1. 初始化SSL库
  SSLLib := TSSLFactory.GetLibrary(sslAutoDetect);
  if not SSLLib.Initialize then
  begin
    WriteLn('Failed to initialize SSL library');
    Exit;
  end;
  
  WriteLn('✓ SSL Library initialized');
  WriteLn;
  
  // 2. 创建上下文
  Context := TSSLFactory.CreateContext(sslCtxClient, sslAutoDetect);
  if Context = nil then
  begin
    WriteLn('Failed to create context');
    Exit;
  end;
  
  // 设置协议版本
  Context.SetProtocolVersion([sslProtocolTLS12, sslProtocolTLS13]);
  WriteLn('✓ Context created');
  WriteLn;
  
  // 3. 创建socket连接（这里需要用户实现socket连接）
  WriteLn('Note: In a real application, you would:');
  WriteLn('  1. Create a socket connection to ', Hostname, ':', Port);
  WriteLn('  2. Pass the socket handle to CreateConnection()');
  WriteLn('  3. Perform SSL handshake with Connect()');
  WriteLn('  4. Get the session with GetSession()');
  WriteLn;
  
  // 示例代码（实际使用时需要真实的socket）
  {
  SocketHandle := SocketConnect(Hostname, Port);
  Connection := Context.CreateConnection(SocketHandle);
  
  if Connection.Connect then
  begin
    WriteLn('✓ SSL handshake completed');
    
    // 4. 获取会话
    Session := Connection.GetSession;
    if Session <> nil then
    begin
      WriteLn('✓ Session obtained');
      WriteLn('  Session ID: ', Session.GetID);
      WriteLn('  Timeout: ', Session.GetTimeout, ' seconds');
      WriteLn('  Protocol: ', ProtocolVersionToString(Session.GetProtocolVersion));
      WriteLn('  Cipher: ', Session.GetCipherName);
      WriteLn;
      
      // 5. 序列化会话
      SessionData := Session.Serialize;
      WriteLn('✓ Session serialized: ', Length(SessionData), ' bytes');
      
      // 6. 保存到文件
      try
        SessionFile := TFileStream.Create('saved_session.dat', fmCreate);
        try
          SessionFile.Write(SessionData[0], Length(SessionData));
          WriteLn('✓ Session saved to saved_session.dat');
        finally
          SessionFile.Free;
        end;
      except
        on E: Exception do
          WriteLn('✗ Failed to save session: ', E.Message);
      end;
    end;
    
    Connection.Shutdown;
  end;
  }
  
  WriteLn('==================================');
end;

procedure LoadSessionExample(const Hostname: string; Port: Word);
var
  SSLLib: ISSLLibrary;
  Context: ISSLContext;
  Connection: ISSLConnection;
  Session: ISSLSession;
  SessionData: TBytes;
  SessionFile: TFileStream;
  SocketHandle: THandle;
begin
  WriteLn;
  WriteLn('==================================');
  WriteLn('Load Session Example');
  WriteLn('==================================');
  WriteLn;
  
  if not FileExists('saved_session.dat') then
  begin
    WriteLn('Note: saved_session.dat not found');
    WriteLn('      Run SaveSessionExample first to create a session');
    Exit;
  end;
  
  // 1. 初始化
  SSLLib := TSSLFactory.GetLibrary(sslAutoDetect);
  if not SSLLib.Initialize then Exit;
  
  Context := TSSLFactory.CreateContext(sslCtxClient, sslAutoDetect);
  if Context = nil then Exit;
  
  WriteLn('✓ SSL Library and Context initialized');
  WriteLn;
  
  // 2. 从文件加载会话数据
  try
    SessionFile := TFileStream.Create('saved_session.dat', fmOpenRead);
    try
      SetLength(SessionData, SessionFile.Size);
      SessionFile.Read(SessionData[0], SessionFile.Size);
      WriteLn('✓ Session data loaded: ', Length(SessionData), ' bytes');
    finally
      SessionFile.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('✗ Failed to load session: ', E.Message);
      Exit;
    end;
  end;
  
  // 3. 反序列化会话
  Session := SSLLib.CreateSession;
  if Session.Deserialize(SessionData) then
  begin
    WriteLn('✓ Session deserialized');
    WriteLn('  Session ID: ', Session.GetID);
    WriteLn('  Is Valid: ', Session.IsValid);
    WriteLn('  Is Resumable: ', Session.IsResumable);
    WriteLn;
    
    // 4. 创建连接并设置会话（示例）
    WriteLn('Note: In a real application, you would:');
    WriteLn('  1. Create a new socket connection');
    WriteLn('  2. Create SSL connection with the socket');
    WriteLn('  3. Set the session with SetSession()');
    WriteLn('  4. Connect() - this should resume the session');
    WriteLn;
    
    {
    SocketHandle := SocketConnect(Hostname, Port);
    Connection := Context.CreateConnection(SocketHandle);
    Connection.SetSession(Session);
    
    if Connection.Connect then
    begin
      WriteLn('✓ Session resumed successfully');
      WriteLn('  - No full handshake required');
      WriteLn('  - Connection established faster');
    end;
    }
  end
  else
    WriteLn('✗ Failed to deserialize session');
  
  WriteLn('==================================');
end;

procedure SessionCloneExample;
var
  SSLLib: ISSLLibrary;
  Session1, Session2: ISSLSession;
begin
  WriteLn;
  WriteLn('==================================');
  WriteLn('Session Clone Example');
  WriteLn('==================================');
  WriteLn;
  
  SSLLib := TSSLFactory.GetLibrary(sslAutoDetect);
  if not SSLLib.Initialize then Exit;
  
  // 创建一个会话（实际使用中从连接获取）
  Session1 := SSLLib.CreateSession;
  
  WriteLn('Cloning session...');
  Session2 := Session1.Clone;
  
  if Session2 <> nil then
  begin
    WriteLn('✓ Session cloned successfully');
    WriteLn('  - Original and clone are independent');
    WriteLn('  - Can be used in different connections');
    WriteLn('  - Useful for connection pooling');
  end;
  
  WriteLn;
  WriteLn('==================================');
end;

begin
  try
    WriteLn('SSL/TLS Session Reuse Examples');
    WriteLn('==============================');
    WriteLn;
    WriteLn('Session reuse improves performance by avoiding');
    WriteLn('full SSL/TLS handshake on subsequent connections');
    WriteLn;
    
    SaveSessionExample('example.com', 443);
    LoadSessionExample('example.com', 443);
    SessionCloneExample;
    
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
  
  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
end.

