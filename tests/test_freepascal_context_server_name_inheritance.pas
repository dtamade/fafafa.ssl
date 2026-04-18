program test_freepascal_context_server_name_inheritance;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.context.builder,
  fafafa.ssl.freepascal.lib;

var
  GTestsPassed: Integer = 0;
  GTestsFailed: Integer = 0;

procedure Assert(ACondition: Boolean; const AMessage: string);
begin
  if ACondition then
  begin
    Inc(GTestsPassed);
    WriteLn('  PASS: ', AMessage);
  end
  else
  begin
    Inc(GTestsFailed);
    WriteLn('  FAIL: ', AMessage);
  end;
end;

procedure TestHeader(const AName: string);
begin
  WriteLn;
  WriteLn('=== ', AName, ' ===');
end;

procedure Test_BuilderContextServerName_InheritedBySocketConnection;
var
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  ClientConn: ISSLClientConnection;
begin
  TestHeader('Builder context server name is inherited by socket connection');

  Ctx := TSSLContextBuilder.Create
    .WithBackend(sslFreePascal)
    .WithTLS13
    .WithSNI('ctx.example.com')
    .BuildClient;

  Conn := Ctx.CreateConnection(THandle(-1));
  ClientConn := Conn as ISSLClientConnection;

  Assert(ClientConn.GetServerName = 'ctx.example.com',
    'Socket connection inherits context server name from builder');
end;

procedure Test_DirectContextServerName_InheritedByStreamConnection;
var
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  ClientConn: ISSLClientConnection;
  Stream: TMemoryStream;
begin
  TestHeader('Direct context server name is inherited by stream connection');

  Ctx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
  // INTENTIONAL_COMPAT: legacy context-level SNI coverage. This regression
  // keeps the deprecated direct-context path observable across backends.
  Ctx.SetServerName('stream.example.com');

  Stream := TMemoryStream.Create;
  try
    Conn := Ctx.CreateConnection(Stream);
    ClientConn := Conn as ISSLClientConnection;

    Assert(ClientConn.GetServerName = 'stream.example.com',
      'Stream connection inherits context server name from direct context API');

    ClientConn := nil;
    Conn := nil;
  finally
    Stream.Free;
  end;
end;

begin
  try
    Test_BuilderContextServerName_InheritedBySocketConnection;
    Test_DirectContextServerName_InheritedByStreamConnection;

    WriteLn;
    WriteLn('Tests Passed: ', GTestsPassed);
    WriteLn('Tests Failed: ', GTestsFailed);

    if GTestsFailed > 0 then
      Halt(1);

    WriteLn('All tests passed.');
  except
    on E: Exception do
    begin
      WriteLn('FATAL: ', E.Message);
      Halt(1);
    end;
  end;
end.
