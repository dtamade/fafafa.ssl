program example_tls_client;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

uses
  SysUtils, Classes,
  fafafa.ssl,
  fafafa.ssl.context.builder,
  fafafa.examples.tcp;

const
  BUFFER_SIZE = 4096;

procedure ShowUsage;
begin
  WriteLn('Usage: ', ExtractFileName(ParamStr(0)), ' [host] [port]');
  WriteLn('Default: www.example.com 443');
end;

function ReadAllStream(AStream: TStream): RawByteString;
var
  Buf: array[0..BUFFER_SIZE - 1] of Byte;
  N: Longint;
  Mem: TMemoryStream;
begin
  Result := '';
  Mem := TMemoryStream.Create;
  try
    repeat
      N := AStream.Read(Buf[0], SizeOf(Buf));
      if N > 0 then
        Mem.WriteBuffer(Buf[0], N);
    until N = 0;

    if Mem.Size > 0 then
    begin
      SetLength(Result, Mem.Size);
      Mem.Position := 0;
      Mem.ReadBuffer(Result[1], Mem.Size);
    end;
  finally
    Mem.Free;
  end;
end;

procedure TestHTTPSConnection(const Host: string; Port: Word);
var
  Sock: TSocketHandle;
  Ctx: ISSLContext;
  Connector: TSSLConnector;
  TLS: TSSLStream;
  Request: RawByteString;
  Response: RawByteString;
  Cert: ISSLCertificate;
begin
  WriteLn('========================================');
  WriteLn('HTTPS connection test: ', Host, ':', Port);
  WriteLn('========================================');
  WriteLn;

  Sock := INVALID_SOCKET;
  TLS := nil;
  try
    // 1) TCP
    WriteLn('1) TCP connect...');
    Sock := ConnectTCP(Host, Port);
    WriteLn('   ✓ Connected');

    // 2) TLS context
    WriteLn('2) Build TLS context (VerifyPeer + SystemRoots)...');
    Ctx := TSSLContextBuilder.Create
      .WithTLS12And13
      .WithVerifyPeer
      .WithSystemRoots
      .BuildClient;
    WriteLn('   ✓ Context created');

    // 3) TLS handshake
    WriteLn('3) TLS handshake...');
    Connector := TSSLConnector.FromContext(Ctx).WithTimeout(15000);
    TLS := Connector.ConnectSocket(THandle(Sock), Host);
    WriteLn('   ✓ Handshake done');
    WriteLn('   Protocol: ', ProtocolVersionToString(TLS.Connection.GetProtocolVersion));
    WriteLn('   Cipher: ', TLS.Connection.GetCipherName);
    WriteLn;

    // 4) Peer certificate
    Cert := TLS.Connection.GetPeerCertificate;
    if Cert <> nil then
    begin
      WriteLn('4) Peer certificate:');
      WriteLn('   Subject: ', Cert.GetSubject);
      WriteLn('   Issuer : ', Cert.GetIssuer);
      WriteLn('   Valid  : ', DateTimeToStr(Cert.GetNotBefore), ' .. ', DateTimeToStr(Cert.GetNotAfter));
      WriteLn;
    end;

    // 5) HTTP request
    WriteLn('5) HTTP GET / ...');
    Request := 'GET / HTTP/1.1' + #13#10 +
               'Host: ' + Host + #13#10 +
               'User-Agent: example_tls_client/1.0' + #13#10 +
               'Connection: close' + #13#10 +
               #13#10;

    if Length(Request) > 0 then
      TLS.WriteBuffer(Request[1], Length(Request));

    Response := ReadAllStream(TLS);

    WriteLn('Received ', Length(Response), ' bytes');
    WriteLn;
    WriteLn('Response preview (first 600 chars):');
    if Length(Response) > 600 then
      WriteLn(Copy(string(Response), 1, 600), '...')
    else
      WriteLn(string(Response));

  finally
    if TLS <> nil then
      TLS.Free;
    CloseSocket(Sock);
  end;
end;

var
  Host: string;
  Port: Word;
  NetErr: string;
begin
  if (ParamCount = 1) and ((ParamStr(1) = '--help') or (ParamStr(1) = '-h')) then
  begin
    ShowUsage;
    Halt(0);
  end;

  if ParamCount >= 1 then
    Host := ParamStr(1)
  else
    Host := 'www.example.com';

  if ParamCount >= 2 then
    Port := StrToIntDef(ParamStr(2), 443)
  else
    Port := 443;

  if not InitNetwork(NetErr) then
  begin
    WriteLn('Network init failed: ', NetErr);
    Halt(1);
  end;

  try
    TestHTTPSConnection(Host, Port);
  finally
    CleanupNetwork;
  end;

  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
end.
