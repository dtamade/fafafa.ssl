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

type
  TTLS13ClientHelloPSKOffer = record
    Valid: Boolean;
    AllowEarlyData: Boolean;
    Identity: TBytes;
    ObfuscatedTicketAge: Cardinal;
    Binder: TBytes;
  end;

function BuildTLS13ClientHelloHandshake(
  const AServerName: string;
  const AALPNProtocols: string;
  const AKeyShare: TBytes;
  AIncludeStatusRequest: Boolean = False;
  AIncludeSignedCertificateTimestamp: Boolean = False
): TBytes;

function BuildTLS13ClientHelloRecord(
  const AServerName: string;
  const AALPNProtocols: string;
  const AKeyShare: TBytes;
  AIncludeStatusRequest: Boolean = False;
  AIncludeSignedCertificateTimestamp: Boolean = False
): TBytes;

function BuildTLS13ClientHelloHandshakeWithPSK(
  const AServerName: string;
  const AALPNProtocols: string;
  const AKeyShare: TBytes;
  const APSKOffer: TTLS13ClientHelloPSKOffer;
  out APartialHandshake: TBytes;
  AIncludeStatusRequest: Boolean = False;
  AIncludeSignedCertificateTimestamp: Boolean = False
): TBytes;

function BuildTLS13ClientHelloHandshakeWithComputedPSKBinder(
  const AServerName: string;
  const AALPNProtocols: string;
  const AKeyShare: TBytes;
  ACipherSuite: Word;
  const APSKIdentity: TBytes;
  AObfuscatedTicketAge: Cardinal;
  const AResumptionPSK: TBytes;
  out APartialHandshake: TBytes;
  AAllowEarlyData: Boolean = False;
  AIncludeStatusRequest: Boolean = False;
  AIncludeSignedCertificateTimestamp: Boolean = False
): TBytes;

function BuildTLS13ClientHelloRecordWithPSK(
  const AServerName: string;
  const AALPNProtocols: string;
  const AKeyShare: TBytes;
  const APSKOffer: TTLS13ClientHelloPSKOffer;
  out APartialHandshake: TBytes;
  AIncludeStatusRequest: Boolean = False;
  AIncludeSignedCertificateTimestamp: Boolean = False
): TBytes;

implementation

uses
  fafafa.ssl.errors,
  fafafa.ssl.random,
  fafafa.ssl.tls13.keyschedule,
  fafafa.ssl.tls13.wire;

type
  TALPNProtocolArray = array of AnsiString;

procedure AppendUInt32(var ADest: TBytes; AValue: Cardinal);
begin
  AppendByte(ADest, Byte((AValue shr 24) and $FF));
  AppendByte(ADest, Byte((AValue shr 16) and $FF));
  AppendByte(ADest, Byte((AValue shr 8) and $FF));
  AppendByte(ADest, Byte(AValue and $FF));
end;

procedure AppendDefaultTLS13CipherSuites(var ADest: TBytes);
begin
  AppendUInt16(ADest, TLS13_CIPHER_AES_256_GCM_SHA384);
  AppendUInt16(ADest, TLS13_CIPHER_CHACHA20_POLY1305_SHA256);
  AppendUInt16(ADest, TLS13_CIPHER_AES_128_GCM_SHA256);
end;

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

function BuildExtensionStatusRequest: TBytes;
var
  LData: TBytes;
begin
  SetLength(LData, 0);
  AppendByte(LData, TLS_CERT_STATUS_TYPE_OCSP);
  AppendUInt16(LData, 0);
  AppendUInt16(LData, 0);
  Result := BuildExtensionHeader(TLS_EXTENSION_STATUS_REQUEST, LData);
end;

function BuildExtensionSignedCertificateTimestamp: TBytes;
var
  LData: TBytes;
begin
  SetLength(LData, 0);
  Result := BuildExtensionHeader(TLS_EXTENSION_SIGNED_CERTIFICATE_TIMESTAMP, LData);
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
  AppendUInt16(LData, 12); // 6 * 2 字节
  AppendUInt16(LData, TLS13_SIG_ECDSA_SECP256R1_SHA256);
  AppendUInt16(LData, TLS13_SIG_RSA_PSS_RSAE_SHA256);
  AppendUInt16(LData, TLS13_SIG_RSA_PSS_RSAE_SHA384);
  AppendUInt16(LData, TLS13_SIG_RSA_PKCS1_SHA256);
  AppendUInt16(LData, TLS13_SIG_RSA_PKCS1_SHA384);
  AppendUInt16(LData, TLS13_SIG_RSA_PSS_PSS_SHA384);
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

function BuildExtensionEarlyData: TBytes;
var
  LData: TBytes;
begin
  SetLength(LData, 0);
  Result := BuildExtensionHeader(TLS_EXTENSION_EARLY_DATA, LData);
end;

function BuildExtensionPreSharedKey(
  const APSKOffer: TTLS13ClientHelloPSKOffer;
  const ABinder: TBytes
): TBytes;
var
  LIdentityEntry: TBytes;
  LIdentities: TBytes;
  LBinders: TBytes;
  LData: TBytes;
begin
  if not APSKOffer.Valid then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  SetLength(LIdentityEntry, 0);
  AppendUInt16(LIdentityEntry, Word(Length(APSKOffer.Identity)));
  AppendBytes(LIdentityEntry, APSKOffer.Identity);
  AppendUInt32(LIdentityEntry, APSKOffer.ObfuscatedTicketAge);

  SetLength(LIdentities, 0);
  AppendUInt16(LIdentities, Word(Length(LIdentityEntry)));
  AppendBytes(LIdentities, LIdentityEntry);

  SetLength(LBinders, 0);
  AppendByte(LBinders, Byte(Length(ABinder)));
  AppendBytes(LBinders, ABinder);
  LData := nil;
  SetLength(LData, 0);
  AppendUInt16(LData, Word(Length(LBinders)));
  AppendBytes(LData, LBinders);

  Result := nil;
  SetLength(Result, 0);
  AppendBytes(Result, LIdentities);
  AppendBytes(Result, LData);

  Result := BuildExtensionHeader(TLS_EXTENSION_PRE_SHARED_KEY, Result);
end;

function BuildTLS13ClientHelloBody(
  const AServerName: string;
  const AALPNProtocols: string;
  const AKeyShare: TBytes;
  AIncludeStatusRequest: Boolean;
  AIncludeSignedCertificateTimestamp: Boolean
): TBytes;
var
  LRandom, LSessionId: TBytes;
  LCipherSuites: TBytes;
  LCompressionMethods: TBytes;
  LExtensions: TBytes;
  LExt: TBytes;
begin
  LRandom := GenerateSecureRandomBytes(32);
  LSessionId := GenerateSecureRandomBytes(32);

  SetLength(LCipherSuites, 0);
  AppendDefaultTLS13CipherSuites(LCipherSuites);

  SetLength(LCompressionMethods, 0);
  AppendByte(LCompressionMethods, 1);
  AppendByte(LCompressionMethods, 0);

  SetLength(LExtensions, 0);

  LExt := BuildExtensionServerName(AServerName);
  AppendBytes(LExtensions, LExt);

  if AIncludeStatusRequest then
  begin
    LExt := BuildExtensionStatusRequest;
    AppendBytes(LExtensions, LExt);
  end;

  if AIncludeSignedCertificateTimestamp then
  begin
    LExt := BuildExtensionSignedCertificateTimestamp;
    AppendBytes(LExtensions, LExt);
  end;

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

function BuildTLS13ClientHelloBodyWithPSKCore(
  const AServerName: string;
  const AALPNProtocols: string;
  const AKeyShare: TBytes;
  const APSKOffer: TTLS13ClientHelloPSKOffer;
  const ARandom: TBytes;
  const ASessionId: TBytes;
  out APartialBody: TBytes;
  AIncludeStatusRequest: Boolean;
  AIncludeSignedCertificateTimestamp: Boolean
): TBytes;
var
  LCipherSuites: TBytes;
  LCompressionMethods: TBytes;
  LBaseExtensions: TBytes;
  LExt: TBytes;
  LZeroBinder: TBytes;
  LPartialPSKExtension: TBytes;
  LFinalPSKExtension: TBytes;
begin
  SetLength(LCipherSuites, 0);
  AppendDefaultTLS13CipherSuites(LCipherSuites);

  SetLength(LCompressionMethods, 0);
  AppendByte(LCompressionMethods, 1);
  AppendByte(LCompressionMethods, 0);

  SetLength(LBaseExtensions, 0);

  LExt := BuildExtensionServerName(AServerName);
  AppendBytes(LBaseExtensions, LExt);

  if AIncludeStatusRequest then
  begin
    LExt := BuildExtensionStatusRequest;
    AppendBytes(LBaseExtensions, LExt);
  end;

  if AIncludeSignedCertificateTimestamp then
  begin
    LExt := BuildExtensionSignedCertificateTimestamp;
    AppendBytes(LBaseExtensions, LExt);
  end;

  LExt := BuildExtensionSupportedVersions;
  AppendBytes(LBaseExtensions, LExt);

  LExt := BuildExtensionSupportedGroups;
  AppendBytes(LBaseExtensions, LExt);

  LExt := BuildExtensionSignatureAlgorithms;
  AppendBytes(LBaseExtensions, LExt);

  LExt := BuildExtensionPSKKeyExchangeModes;
  AppendBytes(LBaseExtensions, LExt);

  LExt := BuildExtensionKeyShare(AKeyShare);
  AppendBytes(LBaseExtensions, LExt);

  LExt := BuildExtensionALPN(AALPNProtocols);
  AppendBytes(LBaseExtensions, LExt);

  if APSKOffer.Valid and APSKOffer.AllowEarlyData then
  begin
    LExt := BuildExtensionEarlyData;
    AppendBytes(LBaseExtensions, LExt);
  end;

  SetLength(APartialBody, 0);
  AppendUInt16(APartialBody, TLS_LEGACY_VERSION);
  AppendBytes(APartialBody, ARandom);
  AppendByte(APartialBody, Byte(Length(ASessionId)));
  AppendBytes(APartialBody, ASessionId);
  AppendUInt16(APartialBody, Word(Length(LCipherSuites)));
  AppendBytes(APartialBody, LCipherSuites);
  AppendBytes(APartialBody, LCompressionMethods);

  if APSKOffer.Valid then
  begin
    SetLength(LZeroBinder, Length(APSKOffer.Binder));
    if Length(LZeroBinder) > 0 then
      FillChar(LZeroBinder[0], Length(LZeroBinder), 0);
    LPartialPSKExtension := BuildExtensionPreSharedKey(APSKOffer, LZeroBinder);
    LFinalPSKExtension := BuildExtensionPreSharedKey(APSKOffer, APSKOffer.Binder);

    AppendUInt16(APartialBody, Word(Length(LBaseExtensions) + Length(LPartialPSKExtension)));
    AppendBytes(APartialBody, LBaseExtensions);
    AppendBytes(APartialBody, LPartialPSKExtension);

    SetLength(Result, 0);
    AppendUInt16(Result, TLS_LEGACY_VERSION);
    AppendBytes(Result, ARandom);
    AppendByte(Result, Byte(Length(ASessionId)));
    AppendBytes(Result, ASessionId);
    AppendUInt16(Result, Word(Length(LCipherSuites)));
    AppendBytes(Result, LCipherSuites);
    AppendBytes(Result, LCompressionMethods);
    AppendUInt16(Result, Word(Length(LBaseExtensions) + Length(LFinalPSKExtension)));
    AppendBytes(Result, LBaseExtensions);
    AppendBytes(Result, LFinalPSKExtension);
    Exit;
  end;

  AppendUInt16(APartialBody, Word(Length(LBaseExtensions)));
  AppendBytes(APartialBody, LBaseExtensions);
  Result := Copy(APartialBody);
end;

function BuildTLS13ClientHelloBodyWithPSK(
  const AServerName: string;
  const AALPNProtocols: string;
  const AKeyShare: TBytes;
  const APSKOffer: TTLS13ClientHelloPSKOffer;
  out APartialBody: TBytes;
  AIncludeStatusRequest: Boolean;
  AIncludeSignedCertificateTimestamp: Boolean
): TBytes;
var
  LRandom: TBytes;
  LSessionId: TBytes;
begin
  LRandom := GenerateSecureRandomBytes(32);
  LSessionId := GenerateSecureRandomBytes(32);
  Result := BuildTLS13ClientHelloBodyWithPSKCore(
    AServerName,
    AALPNProtocols,
    AKeyShare,
    APSKOffer,
    LRandom,
    LSessionId,
    APartialBody,
    AIncludeStatusRequest,
    AIncludeSignedCertificateTimestamp
  );
end;

function BuildTLS13ClientHelloHandshake(
  const AServerName: string;
  const AALPNProtocols: string;
  const AKeyShare: TBytes;
  AIncludeStatusRequest: Boolean;
  AIncludeSignedCertificateTimestamp: Boolean
): TBytes;
var
  LBody: TBytes;
begin
  LBody := BuildTLS13ClientHelloBody(
    AServerName,
    AALPNProtocols,
    AKeyShare,
    AIncludeStatusRequest,
    AIncludeSignedCertificateTimestamp
  );

  SetLength(Result, 0);
  AppendByte(Result, TLS_HANDSHAKE_TYPE_CLIENT_HELLO);
  AppendUInt24(Result, Length(LBody));
  AppendBytes(Result, LBody);
end;

function BuildTLS13ClientHelloRecord(
  const AServerName: string;
  const AALPNProtocols: string;
  const AKeyShare: TBytes;
  AIncludeStatusRequest: Boolean;
  AIncludeSignedCertificateTimestamp: Boolean
): TBytes;
var
  LHandshake: TBytes;
begin
  LHandshake := BuildTLS13ClientHelloHandshake(
    AServerName,
    AALPNProtocols,
    AKeyShare,
    AIncludeStatusRequest,
    AIncludeSignedCertificateTimestamp
  );
  Result := BuildTLSPlaintext(TLS_CONTENT_TYPE_HANDSHAKE, LHandshake);
end;

function BuildTLS13ClientHelloHandshakeWithPSK(
  const AServerName: string;
  const AALPNProtocols: string;
  const AKeyShare: TBytes;
  const APSKOffer: TTLS13ClientHelloPSKOffer;
  out APartialHandshake: TBytes;
  AIncludeStatusRequest: Boolean;
  AIncludeSignedCertificateTimestamp: Boolean
): TBytes;
var
  LBody: TBytes;
  LPartialBody: TBytes;
begin
  LBody := BuildTLS13ClientHelloBodyWithPSK(
    AServerName,
    AALPNProtocols,
    AKeyShare,
    APSKOffer,
    LPartialBody,
    AIncludeStatusRequest,
    AIncludeSignedCertificateTimestamp
  );

  SetLength(APartialHandshake, 0);
  AppendByte(APartialHandshake, TLS_HANDSHAKE_TYPE_CLIENT_HELLO);
  AppendUInt24(APartialHandshake, Length(LPartialBody));
  AppendBytes(APartialHandshake, LPartialBody);

  SetLength(Result, 0);
  AppendByte(Result, TLS_HANDSHAKE_TYPE_CLIENT_HELLO);
  AppendUInt24(Result, Length(LBody));
  AppendBytes(Result, LBody);
end;

function BuildTLS13ClientHelloHandshakeWithComputedPSKBinder(
  const AServerName: string;
  const AALPNProtocols: string;
  const AKeyShare: TBytes;
  ACipherSuite: Word;
  const APSKIdentity: TBytes;
  AObfuscatedTicketAge: Cardinal;
  const AResumptionPSK: TBytes;
  out APartialHandshake: TBytes;
  AAllowEarlyData: Boolean;
  AIncludeStatusRequest: Boolean;
  AIncludeSignedCertificateTimestamp: Boolean
): TBytes;
var
  LOffer: TTLS13ClientHelloPSKOffer;
  LPartialBody: TBytes;
  LBody: TBytes;
  LRandom: TBytes;
  LSessionId: TBytes;
begin
  FillChar(LOffer, SizeOf(LOffer), 0);
  LOffer.Valid := True;
  LOffer.Identity := Copy(APSKIdentity);
  LOffer.ObfuscatedTicketAge := AObfuscatedTicketAge;
  LOffer.AllowEarlyData := AAllowEarlyData;
  SetLength(LOffer.Binder, Length(AResumptionPSK));
  if Length(LOffer.Binder) > 0 then
    FillChar(LOffer.Binder[0], Length(LOffer.Binder), 0);

  LRandom := GenerateSecureRandomBytes(32);
  LSessionId := GenerateSecureRandomBytes(32);
  LBody := BuildTLS13ClientHelloBodyWithPSKCore(
    AServerName,
    AALPNProtocols,
    AKeyShare,
    LOffer,
    LRandom,
    LSessionId,
    LPartialBody,
    AIncludeStatusRequest,
    AIncludeSignedCertificateTimestamp
  );

  SetLength(APartialHandshake, 0);
  AppendByte(APartialHandshake, TLS_HANDSHAKE_TYPE_CLIENT_HELLO);
  AppendUInt24(APartialHandshake, Length(LPartialBody));
  AppendBytes(APartialHandshake, LPartialBody);

  LOffer.Binder := TLS13ComputePSKBinderForCipherSuite(
    ACipherSuite,
    AResumptionPSK,
    APartialHandshake
  );
  LBody := BuildTLS13ClientHelloBodyWithPSKCore(
    AServerName,
    AALPNProtocols,
    AKeyShare,
    LOffer,
    LRandom,
    LSessionId,
    LPartialBody,
    AIncludeStatusRequest,
    AIncludeSignedCertificateTimestamp
  );

  SetLength(Result, 0);
  AppendByte(Result, TLS_HANDSHAKE_TYPE_CLIENT_HELLO);
  AppendUInt24(Result, Length(LBody));
  AppendBytes(Result, LBody);
end;

function BuildTLS13ClientHelloRecordWithPSK(
  const AServerName: string;
  const AALPNProtocols: string;
  const AKeyShare: TBytes;
  const APSKOffer: TTLS13ClientHelloPSKOffer;
  out APartialHandshake: TBytes;
  AIncludeStatusRequest: Boolean;
  AIncludeSignedCertificateTimestamp: Boolean
): TBytes;
var
  LHandshake: TBytes;
begin
  LHandshake := BuildTLS13ClientHelloHandshakeWithPSK(
    AServerName,
    AALPNProtocols,
    AKeyShare,
    APSKOffer,
    APartialHandshake,
    AIncludeStatusRequest,
    AIncludeSignedCertificateTimestamp
  );
  Result := BuildTLSPlaintext(TLS_CONTENT_TYPE_HANDSHAKE, LHandshake);
end;

end.
