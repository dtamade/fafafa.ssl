program test_openssl_cert_verify_cache_policy;

{$mode ObjFPC}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.context.builder,
  fafafa.ssl.openssl.lib;

var
  GPass: Integer = 0;
  GFail: Integer = 0;

procedure AssertTrue(ACondition: Boolean; const AMessage: string);
begin
  if ACondition then
  begin
    Inc(GPass);
    WriteLn('  [PASS] ', AMessage);
  end
  else
  begin
    Inc(GFail);
    WriteLn('  [FAIL] ', AMessage);
  end;
end;

function ReadAllText(const AFileName: string): string;
var
  LStream: TFileStream;
  LBytes: TBytes;
begin
  Result := '';
  if not FileExists(AFileName) then
    Exit;

  LStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(LBytes, LStream.Size);
    if Length(LBytes) > 0 then
      LStream.ReadBuffer(LBytes[0], Length(LBytes));
    Result := TEncoding.UTF8.GetString(LBytes);
  finally
    LStream.Free;
  end;
end;

procedure TestBuilderPersistsPolicyOption;
var
  LBuilder: ISSLContextBuilder;
  LContext: ISSLContext;
  LResult: TSSLOperationResult;
begin
  WriteLn('--- Test: builder persists valid-hit refresh skip policy option');

  LBuilder := TSSLContextBuilder.CreateWithSafeDefaults
    .WithCertVerifyCache(True)
    .WithCertVerifyCacheSkipValidHitRefresh(True);
  LResult := LBuilder.TryBuildClient(LContext);

  AssertTrue(LResult.IsOk,
    'TryBuildClient should succeed when valid-hit refresh skip policy is enabled');

  if LResult.IsOk and (LContext <> nil) then
  begin
    AssertTrue(ssoEnableCertVerifyCache in LContext.GetOptions,
      'Context options should include cert verify cache');
    AssertTrue(ssoSkipCertVerifyCacheValidHitRefresh in LContext.GetOptions,
      'Context options should include valid-hit refresh skip policy');
  end;
end;

procedure TestOpenSSLConnectionHasPolicyBranch;
var
  LSourcePath: string;
  LSource: string;
  LLower: string;
begin
  WriteLn('--- Test: OpenSSL connection implements policy branch contract');

  LSourcePath := 'src/fafafa.ssl.openssl.connection.pas';
  AssertTrue(FileExists(LSourcePath),
    'OpenSSL connection source should exist for policy contract');

  LSource := ReadAllText(LSourcePath);
  LLower := LowerCase(LSource);

  AssertTrue(Pos('ssoskipcertverifycachevalidhitrefresh in connectionoptions', LLower) > 0,
    'OpenSSL connection should read valid-hit refresh skip option from context options');
  AssertTrue(Pos('cert verify cache hit (valid result), skipping x509_verify_cert', LLower) > 0,
    'OpenSSL connection should have skip-refresh log branch for valid cache hit');
  AssertTrue(Pos('cert verify cache hit (valid result), refreshing x509_verify_cert', LLower) > 0,
    'OpenSSL connection should keep refresh branch when skip policy is off');
end;

begin
  WriteLn('fafafa.ssl - OpenSSL cert verify cache policy contract');

  TestBuilderPersistsPolicyOption;
  TestOpenSSLConnectionHasPolicyBranch;

  WriteLn;
  WriteLn('Total tests: ', GPass + GFail);
  WriteLn('Passed: ', GPass);
  WriteLn('Failed: ', GFail);

  if GFail = 0 then
  begin
    WriteLn('✅ openssl cert verify cache policy contract passed');
    Halt(0);
  end
  else
  begin
    WriteLn('❌ openssl cert verify cache policy contract failed');
    Halt(1);
  end;
end.
