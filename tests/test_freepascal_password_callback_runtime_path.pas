program test_freepascal_password_callback_runtime_path;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  Classes,
  Process,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.context.builder,
  fafafa.examples.tcp;

type
  TScriptedDuplexStream = class(TStream)
  private
    FReadBuffer: TBytes;
    FReadPos: Integer;
    FWriteBuffer: TBytes;
  public
    constructor Create(const AReadBuffer: TBytes);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function CapturedWriteBuffer: TBytes;
  end;

  TPasswordCallbackHolder = class
  private
    FPassword: string;
    FCalls: Integer;
  public
    constructor Create(const APassword: string);
    function ProvidePassword(var APassword: string; const AIsRetry: Boolean): Boolean;
    property Calls: Integer read FCalls;
  end;

  TEncryptedKeyServerResponderStream = class(TScriptedDuplexStream)
  private
    FEncryptedKeyPEM: string;
    FEncryptedKeyFile: string;
    FUseBuilder: Boolean;
    FUseCallback: Boolean;
    FGenerated: Boolean;
    FCallbackCalls: Integer;
  public
    constructor Create(const AEncryptedKeyPEM, AEncryptedKeyFile: string;
      AUseBuilder, AUseCallback: Boolean);
    function Write(const Buffer; Count: Longint): Longint; override;
    property CallbackCalls: Integer read FCallbackCalls;
  end;

constructor TScriptedDuplexStream.Create(const AReadBuffer: TBytes);
begin
  inherited Create;
  FReadBuffer := Copy(AReadBuffer, 0, Length(AReadBuffer));
  FReadPos := 0;
  SetLength(FWriteBuffer, 0);
end;

function TScriptedDuplexStream.Read(var Buffer; Count: Longint): Longint;
var
  LRemaining: Integer;
begin
  LRemaining := Length(FReadBuffer) - FReadPos;
  if LRemaining <= 0 then
    Exit(0);

  Result := Count;
  if Result > LRemaining then
    Result := LRemaining;
  if Result > 0 then
    Move(FReadBuffer[FReadPos], Buffer, Result);
  Inc(FReadPos, Result);
end;

function TScriptedDuplexStream.Write(const Buffer; Count: Longint): Longint;
var
  LOldLen: Integer;
begin
  LOldLen := Length(FWriteBuffer);
  SetLength(FWriteBuffer, LOldLen + Count);
  if Count > 0 then
    Move(Buffer, FWriteBuffer[LOldLen], Count);
  Result := Count;
end;

function TScriptedDuplexStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: FReadPos := Offset;
    soCurrent: Inc(FReadPos, Offset);
    soEnd: FReadPos := Length(FReadBuffer) + Offset;
  end;

  if FReadPos < 0 then
    FReadPos := 0;
  if FReadPos > Length(FReadBuffer) then
    FReadPos := Length(FReadBuffer);
  Result := FReadPos;
end;

function TScriptedDuplexStream.CapturedWriteBuffer: TBytes;
begin
  Result := Copy(FWriteBuffer, 0, Length(FWriteBuffer));
end;

constructor TPasswordCallbackHolder.Create(const APassword: string);
begin
  inherited Create;
  FPassword := APassword;
  FCalls := 0;
end;

function TPasswordCallbackHolder.ProvidePassword(var APassword: string;
  const AIsRetry: Boolean): Boolean;
begin
  Inc(FCalls);
  APassword := FPassword;
  Result := True;
end;

function RunCommand(const ACmd: string; const AArgs: array of string;
  out AOutput: string): Integer;
var
  LProcess: TProcess;
  LStrings: TStringList;
  I: Integer;
begin
  AOutput := '';
  LProcess := TProcess.Create(nil);
  LStrings := TStringList.Create;
  try
    LProcess.Executable := ACmd;
    for I := 0 to High(AArgs) do
      LProcess.Parameters.Add(AArgs[I]);
    LProcess.Options := [poWaitOnExit, poUsePipes, poStderrToOutPut];
    LProcess.Execute;
    LStrings.LoadFromStream(LProcess.Output);
    AOutput := LStrings.Text;
    Result := LProcess.ExitCode;
  finally
    LStrings.Free;
    LProcess.Free;
  end;
end;

procedure Fail(const AMessage: string);
begin
  WriteLn('❌ ', AMessage);
  Halt(1);
end;

procedure AssertTrue(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
    Fail(AMessage);
end;

function CreateTrustedStoreFromFile(const AFileName: string): ISSLCertificateStore;
var
  LTrustedCert: ISSLCertificate;
begin
  Result := TSSLFactory.CreateCertificateStore(sslFreePascal);
  AssertTrue(Result <> nil, 'Trusted store should be created');

  LTrustedCert := TSSLFactory.CreateCertificate(sslFreePascal);
  AssertTrue(LTrustedCert <> nil, 'Trusted certificate instance should be created');
  AssertTrue(LTrustedCert.LoadFromFile(AFileName),
    'Trusted certificate should load from file');
  AssertTrue(Result.AddCertificate(LTrustedCert),
    'Trusted store should accept trusted certificate');
end;

function BuildServerFlightFromClientHello(
  const AClientHello: TBytes;
  const AEncryptedKeyPEM, AEncryptedKeyFile: string;
  AUseBuilder, AUseCallback: Boolean;
  out ACallbackCalls: Integer
): TBytes;
var
  LServerCtx: ISSLContext;
  LServerConn: ISSLConnection;
  LServerTransport: TScriptedDuplexStream;
  LHolder: TPasswordCallbackHolder;
begin
  ACallbackCalls := 0;

  if AUseBuilder then
    LServerCtx := TSSLContextBuilder.Create
      .WithBackend(sslFreePascal)
      .WithCertificate('tests/certificate/test_certs/signer_cert.pem')
      .WithPrivateKey(AEncryptedKeyFile, 'secret')
      .BuildServer
  else
  begin
    LServerCtx := TSSLFactory.CreateContext(sslCtxServer, sslFreePascal);
    AssertTrue(LServerCtx <> nil, 'FreePascal server context should be created');
    LServerCtx.SetPreferredVersion(sslProtocolTLS13);
    LServerCtx.LoadCertificate('tests/certificate/test_certs/signer_cert.pem');
    LHolder := nil;
    try
      if AUseCallback then
      begin
        LHolder := TPasswordCallbackHolder.Create('secret');
        LServerCtx.SetPasswordCallback(@LHolder.ProvidePassword);
      end;
      LServerCtx.LoadPrivateKeyPEM(AEncryptedKeyPEM);
      if LHolder <> nil then
        ACallbackCalls := LHolder.Calls;
    finally
      LHolder.Free;
    end;
  end;

  AssertTrue(LServerCtx <> nil, 'Encrypted-key server context should be created');
  LServerCtx.SetALPNProtocols('h2,http/1.1');

  LServerTransport := TScriptedDuplexStream.Create(AClientHello);
  try
    LServerConn := LServerCtx.CreateConnection(LServerTransport);
    AssertTrue(LServerConn <> nil, 'Server connection should be created');
    if LServerConn.Accept then
      raise Exception.Create('Server accept should not fully complete in scripted one-way harness');
    Result := LServerTransport.CapturedWriteBuffer;
  finally
    LServerTransport.Free;
  end;
end;

constructor TEncryptedKeyServerResponderStream.Create(const AEncryptedKeyPEM,
  AEncryptedKeyFile: string; AUseBuilder, AUseCallback: Boolean);
begin
  inherited Create(nil);
  FEncryptedKeyPEM := AEncryptedKeyPEM;
  FEncryptedKeyFile := AEncryptedKeyFile;
  FUseBuilder := AUseBuilder;
  FUseCallback := AUseCallback;
  FGenerated := False;
  FCallbackCalls := 0;
end;

function TEncryptedKeyServerResponderStream.Write(const Buffer; Count: Longint): Longint;
var
  LData: TBytes;
begin
  Result := inherited Write(Buffer, Count);
  if FGenerated or (Count <= 0) then
    Exit;

  SetLength(LData, Count);
  Move(Buffer, LData[0], Count);
  FReadBuffer := BuildServerFlightFromClientHello(
    LData,
    FEncryptedKeyPEM,
    FEncryptedKeyFile,
    FUseBuilder,
    FUseCallback,
    FCallbackCalls
  );
  FReadPos := 0;
  FGenerated := True;
end;

function GenerateEncryptedKeyPEM(const APassword: string; AUseLegacyPEM: Boolean;
  out AFileName: string): string;
var
  LOutput: string;
  LStrings: TStringList;
begin
  AFileName := GetTempDir + 'fafafa_fp_encrypted_key_' + IntToStr(GetProcessID) + '.pem';
  if AUseLegacyPEM then
    AssertTrue(
      RunCommand(
        'openssl',
        [
          'pkey', '-in', 'tests/certificate/test_certs/signer_key.pem',
          '-traditional', '-aes-256-cbc',
          '-passout', 'pass:' + APassword,
          '-out', AFileName
        ],
        LOutput
      ) = 0,
      'OpenSSL should generate legacy encrypted PEM key fixture: ' + LOutput
    )
  else
    AssertTrue(
      RunCommand(
        'openssl',
        [
          'pkcs8', '-topk8',
          '-v2', 'aes-256-cbc',
          '-v2prf', 'hmacWithSHA256',
          '-iter', '2048',
          '-passout', 'pass:' + APassword,
          '-in', 'tests/certificate/test_certs/signer_key.pem',
          '-out', AFileName
        ],
        LOutput
      ) = 0,
      'OpenSSL should generate encrypted PKCS#8 key fixture: ' + LOutput
    );
  LStrings := TStringList.Create;
  try
    LStrings.LoadFromFile(AFileName);
    Result := LStrings.Text;
  finally
    LStrings.Free;
  end;
end;

procedure RunPasswordRuntimeCase(const ACaseName: string; AUseBuilder, AUseCallback,
  AUseLegacyPEM: Boolean);
var
  LClientCtx: ISSLContext;
  LClientConn: ISSLConnection;
  LClientTransport: TEncryptedKeyServerResponderStream;
  LEncryptedKeyPEM: string;
  LEncryptedKeyFile: string;
begin
  LEncryptedKeyPEM := GenerateEncryptedKeyPEM('secret', AUseLegacyPEM, LEncryptedKeyFile);

  LClientCtx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
  AssertTrue(LClientCtx <> nil, 'FreePascal client context should be created');
  LClientCtx.SetPreferredVersion(sslProtocolTLS13);
  LClientCtx.SetALPNProtocols('h2,http/1.1');
  LClientCtx.SetCertificateStore(CreateTrustedStoreFromFile('tests/certificate/test_certs/ca_cert.pem'));
  LClientCtx.SetCertVerifyFlags([sslCertVerifyDefault, sslCertVerifyIgnoreHostname]);

  LClientTransport := TEncryptedKeyServerResponderStream.Create(
    LEncryptedKeyPEM,
    LEncryptedKeyFile,
    AUseBuilder,
    AUseCallback
  );
  try
    LClientConn := LClientCtx.CreateConnection(LClientTransport);
    AssertTrue(LClientConn <> nil, 'Client connection should be created');
    AssertTrue(LClientConn.Connect,
      ACaseName + ' should complete handshake: ' +
      LClientConn.GetVerifyResultString);
    if AUseCallback then
      AssertTrue(LClientTransport.CallbackCalls > 0,
        ACaseName + ' should invoke password callback');
  finally
    LClientTransport.Free;
    DeleteFile(LEncryptedKeyFile);
  end;
end;

begin
  WriteLn('Testing FreePascal password callback runtime path...');
  RunPasswordRuntimeCase('Builder PKCS#8 password path', True, False, False);
  RunPasswordRuntimeCase('Context PKCS#8 callback path', False, True, False);
  RunPasswordRuntimeCase('Builder legacy PEM password path', True, False, True);
  RunPasswordRuntimeCase('Context legacy PEM callback path', False, True, True);
  WriteLn('✅ FreePascal password callback runtime path checks passed');
end.
