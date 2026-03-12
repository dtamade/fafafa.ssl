{**
 * Unit: fafafa.ssl.tls13.clienthello
 * Purpose: TLS 1.3 ClientHello 构建器（纯 Pascal）
 *}

unit fafafa.ssl.tls13.clienthello;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  fafafa.ssl.base;

function BuildTLS13ClientHelloHandshake(
  const AServerName: string;
  const AALPNProtocols: string;
  const AKeyShare: TBytes
): TBytes;
function BuildTLS13ClientHelloHandshakeWithCipherSuites(
  const AServerName: string;
  const AALPNProtocols: string;
  const AKeyShare: TBytes;
  const ACipherSuites: array of Word
): TBytes;

function BuildTLS13ClientHelloHandshakeWithPSK(
  const AServerName: string;
  const AALPNProtocols: string;
  const AKeyShare: TBytes;
  const ARandom: TBytes;
  const ALegacySessionID: TBytes;
  ACipherSuite: Word;
  const APSKIdentity: TBytes;
  APSKObfuscatedTicketAge: Cardinal;
  const APSKBinder: TBytes
): TBytes;

function BuildTLS13ClientHelloRecord(
  const AServerName: string;
  const AALPNProtocols: string;
  const AKeyShare: TBytes
): TBytes;

implementation

uses
  fafafa.ssl.errors,
  fafafa.ssl.random,
  fafafa.ssl.tls13.wire;

type
  TALPNProtocolArray = array of AnsiString;

function BytesFromAnsi(const AValue: AnsiString): TBytes;
begin
  Result := nil;
  SetLength(Result, Length(AValue));
  if Length(AValue) > 0 then
    Move(AValue[1], Result[0], Length(AValue));
end;

function IsAsciiWhitespace(AChar: Char): Boolean;
begin
  Result := (AChar = ' ') or (AChar = #9) or (AChar = #10) or (AChar = #13);
end;

function ParseALPNList(const AALPNProtocols: string): TALPNProtocolArray;
var
  I: Integer;
  LStart, LStop: Integer;
  LValue: string;
  LCount: Integer;
begin
  Result := nil;
  SetLength(Result, 0);
  LCount := 0;
  LStart := 1;

  for I := 1 to Length(AALPNProtocols) + 1 do
  begin
    if (I <= Length(AALPNProtocols)) and (AALPNProtocols[I] <> ',') then
      Continue;

    LStop := I - 1;
    while (LStart <= LStop) and IsAsciiWhitespace(AALPNProtocols[LStart]) do
      Inc(LStart);
    while (LStop >= LStart) and IsAsciiWhitespace(AALPNProtocols[LStop]) do
      Dec(LStop);

    if LStop >= LStart then
    begin
      LValue := Copy(AALPNProtocols, LStart, LStop - LStart + 1);
      SetLength(Result, LCount + 1);
      Result[LCount] := AnsiString(LValue);
      Inc(LCount);
    end;

    LStart := I + 1;
  end;
end;

function BuildExtensionHeader(AType: Word; const AData: TBytes): TBytes;
begin
  Result := nil;
  SetLength(Result, 0);
  AppendUInt16(Result, AType);
  AppendUInt16(Result, Word(Length(AData)));
  AppendBytes(Result, AData);
end;

function BuildExtensionServerName(const AServerName: string): TBytes;
var
  LHostBytes: TBytes;
  LListData: TBytes;
begin
  if AServerName = '' then
  begin
    Result := nil;
    SetLength(Result, 0);
    Exit;
  end;

  LHostBytes := BytesFromAnsi(AnsiString(AServerName));
  SetLength(LListData, 0);
  AppendByte(LListData, 0); // host_name
  AppendUInt16(LListData, Word(Length(LHostBytes)));
  AppendBytes(LListData, LHostBytes);

  SetLength(Result, 0);
  AppendUInt16(Result, Word(Length(LListData)));
  AppendBytes(Result, LListData);

  Result := BuildExtensionHeader(TLS_EXTENSION_SERVER_NAME, Result);
end;

function BuildExtensionALPN(const AALPNProtocols: string): TBytes;
var
  LProtocols: TALPNProtocolArray;
  LListData, LProtoBytes: TBytes;
  I: Integer;
begin
  LProtocols := ParseALPNList(AALPNProtocols);
  if Length(LProtocols) = 0 then
  begin
    Result := nil;
    SetLength(Result, 0);
    Exit;
  end;

  SetLength(LListData, 0);
  for I := 0 to High(LProtocols) do
  begin
    LProtoBytes := BytesFromAnsi(LProtocols[I]);
    if Length(LProtoBytes) > 255 then
      RaiseInvalidParameter('ALPNProtocolLength');

    AppendByte(LListData, Byte(Length(LProtoBytes)));
    AppendBytes(LListData, LProtoBytes);
  end;

  SetLength(Result, 0);
  AppendUInt16(Result, Word(Length(LListData)));
  AppendBytes(Result, LListData);

  Result := BuildExtensionHeader(TLS_EXTENSION_ALPN, Result);
end;

function BuildExtensionSupportedVersions: TBytes;
var
  LData: TBytes;
begin
  SetLength(LData, 0);
  AppendByte(LData, 2); // 长度 = 2
  AppendUInt16(LData, TLS13_VERSION);
  Result := BuildExtensionHeader(TLS_EXTENSION_SUPPORTED_VERSIONS, LData);
end;

function BuildExtensionSupportedGroups: TBytes;
var
  LData: TBytes;
begin
  SetLength(LData, 0);
  AppendUInt16(LData, 4); // 两个组
  AppendUInt16(LData, TLS13_GROUP_X25519);
  AppendUInt16(LData, TLS13_GROUP_SECP256R1);
  Result := BuildExtensionHeader(TLS_EXTENSION_SUPPORTED_GROUPS, LData);
end;

function BuildExtensionSignatureAlgorithms: TBytes;
var
  LData: TBytes;
begin
  SetLength(LData, 0);
  AppendUInt16(LData, 6); // 3 * 2 字节
  AppendUInt16(LData, $0403); // ecdsa_secp256r1_sha256
  AppendUInt16(LData, $0804); // rsa_pss_rsae_sha256
  AppendUInt16(LData, $0401); // rsa_pkcs1_sha256
  Result := BuildExtensionHeader(TLS_EXTENSION_SIGNATURE_ALGORITHMS, LData);
end;

function BuildExtensionPSKKeyExchangeModes: TBytes;
var
  LData: TBytes;
begin
  SetLength(LData, 0);
  AppendByte(LData, 1);
  AppendByte(LData, 1); // psk_dhe_ke
  Result := BuildExtensionHeader(TLS_EXTENSION_PSK_KEY_EXCHANGE_MODES, LData);
end;

function BuildExtensionKeyShare(const AKeyShare: TBytes): TBytes;
var
  LEntry, LData: TBytes;
begin
  if Length(AKeyShare) = 0 then
    RaiseInvalidParameter('TLS13KeyShare');

  SetLength(LEntry, 0);
  AppendUInt16(LEntry, TLS13_GROUP_X25519);
  AppendUInt16(LEntry, Word(Length(AKeyShare)));
  AppendBytes(LEntry, AKeyShare);

  SetLength(LData, 0);
  AppendUInt16(LData, Word(Length(LEntry)));
  AppendBytes(LData, LEntry);

  Result := BuildExtensionHeader(TLS_EXTENSION_KEY_SHARE, LData);
end;

function BuildExtensionPreSharedKey(
  const APSKIdentity: TBytes;
  APSKObfuscatedTicketAge: Cardinal;
  const APSKBinder: TBytes
): TBytes;
var
  LIdentities, LBinders, LData: TBytes;
begin
  if (Length(APSKIdentity) = 0) or (Length(APSKBinder) = 0) then
  begin
    Result := nil;
    Exit;
  end;

  SetLength(LIdentities, 0);
  AppendUInt16(LIdentities, Word(Length(APSKIdentity)));
  AppendBytes(LIdentities, APSKIdentity);
  AppendByte(LIdentities, Byte((APSKObfuscatedTicketAge shr 24) and $FF));
  AppendByte(LIdentities, Byte((APSKObfuscatedTicketAge shr 16) and $FF));
  AppendByte(LIdentities, Byte((APSKObfuscatedTicketAge shr 8) and $FF));
  AppendByte(LIdentities, Byte(APSKObfuscatedTicketAge and $FF));

  SetLength(LBinders, 0);
  AppendByte(LBinders, Byte(Length(APSKBinder)));
  AppendBytes(LBinders, APSKBinder);

  SetLength(LData, 0);
  AppendUInt16(LData, Word(Length(LIdentities)));
  AppendBytes(LData, LIdentities);
  AppendUInt16(LData, Word(Length(LBinders)));
  AppendBytes(LData, LBinders);

  Result := BuildExtensionHeader(TLS_EXTENSION_PRE_SHARED_KEY, LData);
end;

function BuildTLS13ClientHelloBodyWithCipherSuites(
  const AServerName: string;
  const AALPNProtocols: string;
  const AKeyShare: TBytes;
  const ACipherSuites: array of Word
): TBytes;
var
  LRandom, LSessionId: TBytes;
  LCipherSuites: TBytes;
  LCompressionMethods: TBytes;
  LExtensions: TBytes;
  LExt: TBytes;
  I: Integer;
begin
  Result := nil;
  LRandom := GenerateSecureRandomBytes(32);
  LSessionId := GenerateSecureRandomBytes(32);

  SetLength(LCipherSuites, 0);
  for I := 0 to High(ACipherSuites) do
    AppendUInt16(LCipherSuites, ACipherSuites[I]);
  if Length(LCipherSuites) = 0 then
    AppendUInt16(LCipherSuites, TLS13_CIPHER_CHACHA20_POLY1305_SHA256);

  SetLength(LCompressionMethods, 0);
  AppendByte(LCompressionMethods, 1);
  AppendByte(LCompressionMethods, 0);

  SetLength(LExtensions, 0);

  LExt := BuildExtensionServerName(AServerName);
  AppendBytes(LExtensions, LExt);

  LExt := BuildExtensionSupportedVersions;
  AppendBytes(LExtensions, LExt);

  LExt := BuildExtensionSupportedGroups;
  AppendBytes(LExtensions, LExt);

  LExt := BuildExtensionSignatureAlgorithms;
  AppendBytes(LExtensions, LExt);

  LExt := BuildExtensionPSKKeyExchangeModes;
  AppendBytes(LExtensions, LExt);

  LExt := BuildExtensionKeyShare(AKeyShare);
  AppendBytes(LExtensions, LExt);

  LExt := BuildExtensionALPN(AALPNProtocols);
  AppendBytes(LExtensions, LExt);

  SetLength(Result, 0);
  AppendUInt16(Result, TLS_LEGACY_VERSION);
  AppendBytes(Result, LRandom);
  AppendByte(Result, Byte(Length(LSessionId)));
  AppendBytes(Result, LSessionId);
  AppendUInt16(Result, Word(Length(LCipherSuites)));
  AppendBytes(Result, LCipherSuites);
  AppendBytes(Result, LCompressionMethods);
  AppendUInt16(Result, Word(Length(LExtensions)));
  AppendBytes(Result, LExtensions);
end;

function BuildTLS13ClientHelloBody(
  const AServerName: string;
  const AALPNProtocols: string;
  const AKeyShare: TBytes
): TBytes;
var
  LDefaultCipherSuites: array[0..0] of Word;
begin
  LDefaultCipherSuites[0] := TLS13_CIPHER_CHACHA20_POLY1305_SHA256;
  Result := BuildTLS13ClientHelloBodyWithCipherSuites(
    AServerName,
    AALPNProtocols,
    AKeyShare,
    LDefaultCipherSuites
  );
end;

function BuildTLS13ClientHelloBodyWithPSK(
  const AServerName: string;
  const AALPNProtocols: string;
  const AKeyShare: TBytes;
  const ARandom: TBytes;
  const ALegacySessionID: TBytes;
  ACipherSuite: Word;
  const APSKIdentity: TBytes;
  APSKObfuscatedTicketAge: Cardinal;
  const APSKBinder: TBytes
): TBytes;
var
  LCipherSuites: TBytes;
  LCompressionMethods: TBytes;
  LExtensions: TBytes;
  LExt: TBytes;
begin
  Result := nil;
  SetLength(LCipherSuites, 0);
  AppendUInt16(LCipherSuites, ACipherSuite);

  SetLength(LCompressionMethods, 0);
  AppendByte(LCompressionMethods, 1);
  AppendByte(LCompressionMethods, 0);

  SetLength(LExtensions, 0);
  LExt := BuildExtensionServerName(AServerName);
  AppendBytes(LExtensions, LExt);
  LExt := BuildExtensionSupportedVersions;
  AppendBytes(LExtensions, LExt);
  LExt := BuildExtensionSupportedGroups;
  AppendBytes(LExtensions, LExt);
  LExt := BuildExtensionSignatureAlgorithms;
  AppendBytes(LExtensions, LExt);
  LExt := BuildExtensionPSKKeyExchangeModes;
  AppendBytes(LExtensions, LExt);
  LExt := BuildExtensionKeyShare(AKeyShare);
  AppendBytes(LExtensions, LExt);
  LExt := BuildExtensionALPN(AALPNProtocols);
  AppendBytes(LExtensions, LExt);
  LExt := BuildExtensionPreSharedKey(APSKIdentity, APSKObfuscatedTicketAge, APSKBinder);
  AppendBytes(LExtensions, LExt);

  SetLength(Result, 0);
  AppendUInt16(Result, TLS_LEGACY_VERSION);
  AppendBytes(Result, ARandom);
  AppendByte(Result, Byte(Length(ALegacySessionID)));
  AppendBytes(Result, ALegacySessionID);
  AppendUInt16(Result, Word(Length(LCipherSuites)));
  AppendBytes(Result, LCipherSuites);
  AppendBytes(Result, LCompressionMethods);
  AppendUInt16(Result, Word(Length(LExtensions)));
  AppendBytes(Result, LExtensions);
end;

function BuildTLS13ClientHelloHandshake(
  const AServerName: string;
  const AALPNProtocols: string;
  const AKeyShare: TBytes
): TBytes;
var
  LBody: TBytes;
begin
  Result := nil;
  LBody := BuildTLS13ClientHelloBody(AServerName, AALPNProtocols, AKeyShare);

  SetLength(Result, 0);
  AppendByte(Result, TLS_HANDSHAKE_TYPE_CLIENT_HELLO);
  AppendUInt24(Result, Length(LBody));
  AppendBytes(Result, LBody);
end;

function BuildTLS13ClientHelloHandshakeWithCipherSuites(
  const AServerName: string;
  const AALPNProtocols: string;
  const AKeyShare: TBytes;
  const ACipherSuites: array of Word
): TBytes;
var
  LBody: TBytes;
begin
  Result := nil;
  LBody := BuildTLS13ClientHelloBodyWithCipherSuites(
    AServerName,
    AALPNProtocols,
    AKeyShare,
    ACipherSuites
  );

  SetLength(Result, 0);
  AppendByte(Result, TLS_HANDSHAKE_TYPE_CLIENT_HELLO);
  AppendUInt24(Result, Length(LBody));
  AppendBytes(Result, LBody);
end;

function BuildTLS13ClientHelloHandshakeWithPSK(
  const AServerName: string;
  const AALPNProtocols: string;
  const AKeyShare: TBytes;
  const ARandom: TBytes;
  const ALegacySessionID: TBytes;
  ACipherSuite: Word;
  const APSKIdentity: TBytes;
  APSKObfuscatedTicketAge: Cardinal;
  const APSKBinder: TBytes
): TBytes;
var
  LBody: TBytes;
begin
  Result := nil;
  LBody := BuildTLS13ClientHelloBodyWithPSK(
    AServerName,
    AALPNProtocols,
    AKeyShare,
    ARandom,
    ALegacySessionID,
    ACipherSuite,
    APSKIdentity,
    APSKObfuscatedTicketAge,
    APSKBinder
  );

  AppendByte(Result, TLS_HANDSHAKE_TYPE_CLIENT_HELLO);
  AppendUInt24(Result, Length(LBody));
  AppendBytes(Result, LBody);
end;

function BuildTLS13ClientHelloRecord(
  const AServerName: string;
  const AALPNProtocols: string;
  const AKeyShare: TBytes
): TBytes;
var
  LHandshake: TBytes;
begin
  LHandshake := BuildTLS13ClientHelloHandshake(AServerName, AALPNProtocols, AKeyShare);
  Result := BuildTLSPlaintext(TLS_CONTENT_TYPE_HANDSHAKE, LHandshake);
end;

end.
