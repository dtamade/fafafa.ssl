program test_tls13_encrypted_extensions_parser;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.tls13.wire,
  fafafa.ssl.tls13.encryptedextensions;

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

procedure AssertEqualsStr(const AExpected, AActual, AMessage: string);
begin
  if AExpected <> AActual then
    Fail(Format('%s (expected="%s" actual="%s")', [AMessage, AExpected, AActual]));
end;

function BuildEncryptedExtensionsWithALPN(const AProtocol: string): TBytes;
var
  LProtocolBytes: TBytes;
  LExtData: TBytes;
  LExtensions: TBytes;
  LBody: TBytes;
  I: Integer;
begin
  Result := nil;
  SetLength(LProtocolBytes, Length(AProtocol));
  for I := 1 to Length(AProtocol) do
    LProtocolBytes[I - 1] := Byte(Ord(AProtocol[I]) and $FF);

  SetLength(LExtData, 0);
  AppendUInt16(LExtData, Word(Length(LProtocolBytes) + 1));
  AppendByte(LExtData, Byte(Length(LProtocolBytes)));
  AppendBytes(LExtData, LProtocolBytes);

  SetLength(LExtensions, 0);
  AppendUInt16(LExtensions, TLS_EXTENSION_ALPN);
  AppendUInt16(LExtensions, Word(Length(LExtData)));
  AppendBytes(LExtensions, LExtData);

  SetLength(LBody, 0);
  AppendUInt16(LBody, Word(Length(LExtensions)));
  AppendBytes(LBody, LExtensions);

  AppendByte(Result, TLS_HANDSHAKE_TYPE_ENCRYPTED_EXTENSIONS);
  AppendUInt24(Result, Length(LBody));
  AppendBytes(Result, LBody);
end;

procedure TestParseEncryptedExtensionsALPN;
var
  LHandshake: TBytes;
  LInfo: TTLS13EncryptedExtensionsInfo;
  LError: string;
begin
  LHandshake := BuildEncryptedExtensionsWithALPN('h2');
  AssertTrue(TryParseEncryptedExtensionsFromHandshake(LHandshake, LInfo, LError),
    'EncryptedExtensions parse failed: ' + LError);
  AssertTrue(LInfo.Valid, 'EncryptedExtensions should be valid');
  AssertTrue(LInfo.HasALPN, 'EncryptedExtensions should carry ALPN');
  AssertEqualsStr('h2', LInfo.SelectedALPNProtocol, 'Selected ALPN mismatch');
end;

procedure TestRejectMalformedEncryptedExtensionsALPN;
var
  LHandshake: TBytes;
  LInfo: TTLS13EncryptedExtensionsInfo;
  LError: string;
begin
  LHandshake := BuildEncryptedExtensionsWithALPN('h2');
  LHandshake[11] := 0;
  AssertTrue(not TryParseEncryptedExtensionsFromHandshake(LHandshake, LInfo, LError),
    'Malformed EncryptedExtensions ALPN should fail');
  AssertTrue(Pos('ALPN', LError) > 0,
    'Malformed EncryptedExtensions should report ALPN-related error');
end;

begin
  WriteLn('Testing TLS 1.3 EncryptedExtensions parser...');
  TestParseEncryptedExtensionsALPN;
  TestRejectMalformedEncryptedExtensionsALPN;
  WriteLn('✅ TLS 1.3 EncryptedExtensions parser checks passed');
end.
