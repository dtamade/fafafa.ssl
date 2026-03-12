program test_freepascal_password_protected_private_key_truth;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  Classes,
  Process,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.exceptions;

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

function GenerateEncryptedKeyPEM(const APassword: string; out AFileName: string): string;
var
  LOutput: string;
  LStrings: TStringList;
begin
  AFileName := GetTempDir + 'fafafa_fp_encrypted_key_truth_' + IntToStr(GetProcessID) + '.pem';
  AssertTrue(
    RunCommand(
      'openssl',
      [
        'pkcs8',
        '-topk8',
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

procedure TestCapabilityTruth;
var
  LLib: ISSLLibrary;
  LCaps: TSSLBackendCapabilities;
begin
  LLib := TSSLFactory.GetLibrary(sslFreePascal);
  AssertTrue(LLib <> nil, 'FreePascal library should be available');
  LCaps := LLib.GetCapabilities;
  AssertTrue(LCaps.SupportsPasswordProtectedKeys,
    'FreePascal capability should claim password-protected private key support');
  AssertTrue(
    Pos('non-aes legacy pem cipher families remain unsupported', LowerCase(LCaps.KnownIssues)) > 0,
    'KnownIssues should document the remaining unsupported legacy encrypted PEM cipher families'
  );
end;

procedure TestWrongPasswordFailsExplicitly;
var
  LCtx: ISSLContext;
  LEncryptedKeyPEM: string;
  LEncryptedKeyFile: string;
  LRaised: Boolean;
  LMessage: string;
begin
  LEncryptedKeyPEM := GenerateEncryptedKeyPEM('secret', LEncryptedKeyFile);
  LCtx := TSSLFactory.CreateContext(sslCtxServer, sslFreePascal);
  AssertTrue(LCtx <> nil, 'FreePascal context should be created');

  LRaised := False;
  LMessage := '';
  try
    LCtx.LoadPrivateKeyPEM(LEncryptedKeyPEM, 'wrong-secret');
  except
    on E: ESSLException do
    begin
      LRaised := True;
      LMessage := E.Message;
      AssertTrue(
        (E.ErrorCode = sslErrLoadFailed) or (E.ErrorCode = sslErrConfiguration),
        'Wrong password should surface load/configuration error semantics'
      );
    end;
  end;

  DeleteFile(LEncryptedKeyFile);
  AssertTrue(LRaised, 'Wrong encrypted private key password should fail explicitly');
  AssertTrue(
    (Pos('decrypt', LowerCase(LMessage)) > 0) or (Pos('password', LowerCase(LMessage)) > 0),
    'Wrong password failure should mention decrypt/password semantics'
  );
end;

begin
  WriteLn('Testing FreePascal password-protected private key truth...');
  TestCapabilityTruth;
  TestWrongPasswordFailsExplicitly;
  WriteLn('✅ FreePascal password-protected private key truth checks passed');
end.
