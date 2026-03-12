{**
 * Unit: fafafa.ssl.tls12.clienthello
 * Purpose: TLS 1.2 ClientHello builder（纯 Pascal）
 *}

unit fafafa.ssl.tls12.clienthello;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils;

function BuildTLS12ClientHelloHandshake(
  const AServerName: string;
  const AALPNProtocols: string;
  const ACipherSuites: array of Word
): TBytes;
function BuildTLS12ClientHelloHandshakeWithTicket(
  const AServerName: string;
  const AALPNProtocols: string;
  const ACipherSuites: array of Word;
  const ASessionTicket: TBytes
): TBytes;

function BuildTLS12ClientHelloHandshakeWithParams(
  const AServerName: string;
  const AALPNProtocols: string;
  const AClientRandom: TBytes;
  const ASessionID: TBytes;
  const ACipherSuites: array of Word
): TBytes;
function BuildTLS12ClientHelloHandshakeWithParamsAndTicket(
  const AServerName: string;
  const AALPNProtocols: string;
  const AClientRandom: TBytes;
  const ASessionID: TBytes;
  const ACipherSuites: array of Word;
  const ASessionTicket: TBytes
): TBytes;

function BuildTLS12ClientHelloRecord(
  const AServerName: string;
  const AALPNProtocols: string;
  const ACipherSuites: array of Word
): TBytes;
function BuildTLS12ClientHelloRecordWithTicket(
  const AServerName: string;
  const AALPNProtocols: string;
  const ACipherSuites: array of Word;
  const ASessionTicket: TBytes
): TBytes;

function BuildTLS12ClientHelloRecordWithParams(
  const AServerName: string;
  const AALPNProtocols: string;
  const AClientRandom: TBytes;
  const ASessionID: TBytes;
  const ACipherSuites: array of Word
): TBytes;
function BuildTLS12ClientHelloRecordWithParamsAndTicket(
  const AServerName: string;
  const AALPNProtocols: string;
  const AClientRandom: TBytes;
  const ASessionID: TBytes;
  const ACipherSuites: array of Word;
  const ASessionTicket: TBytes
): TBytes;

implementation

uses
  fafafa.ssl.errors,
  fafafa.ssl.random,
  fafafa.ssl.tls12.wire;

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
  AppendUInt16(Result, AType);
  AppendUInt16(Result, Word(Length(AData)));
  AppendBytes(Result, AData);
end;

function BuildExtensionServerName(const AServerName: string): TBytes;
var
  LHostBytes: TBytes;
  LListData: TBytes;
  LData: TBytes;
begin
  if AServerName = '' then
    Exit(nil);

  LHostBytes := BytesFromAnsi(AnsiString(AServerName));
  LListData := nil;
  AppendByte(LListData, 0);
  AppendUInt16(LListData, Word(Length(LHostBytes)));
  AppendBytes(LListData, LHostBytes);

  LData := nil;
  AppendUInt16(LData, Word(Length(LListData)));
  AppendBytes(LData, LListData);

  Result := BuildExtensionHeader(TLS_EXTENSION_SERVER_NAME, LData);
end;

function BuildExtensionALPN(const AALPNProtocols: string): TBytes;
var
  LProtocols: TALPNProtocolArray;
  LProtoBytes: TBytes;
  LListData: TBytes;
  LData: TBytes;
  I: Integer;
begin
  LProtocols := ParseALPNList(AALPNProtocols);
  if Length(LProtocols) = 0 then
    Exit(nil);

  LListData := nil;
  for I := 0 to High(LProtocols) do
  begin
    LProtoBytes := BytesFromAnsi(LProtocols[I]);
    if Length(LProtoBytes) > 255 then
      RaiseInvalidParameter('TLS12ALPNProtocolLength');
    AppendByte(LListData, Byte(Length(LProtoBytes)));
    AppendBytes(LListData, LProtoBytes);
  end;

  LData := nil;
  AppendUInt16(LData, Word(Length(LListData)));
  AppendBytes(LData, LListData);
  Result := BuildExtensionHeader(TLS_EXTENSION_ALPN, LData);
end;

function BuildExtensionSessionTicket(const ASessionTicket: TBytes): TBytes;
begin
  Result := BuildExtensionHeader(TLS_EXTENSION_SESSION_TICKET, ASessionTicket);
end;

function BuildExtensionSupportedGroups: TBytes;
var
  LData: TBytes;
begin
  LData := nil;
  AppendUInt16(LData, 4);
  AppendUInt16(LData, TLS_GROUP_X25519);
  AppendUInt16(LData, TLS_GROUP_SECP256R1);
  Result := BuildExtensionHeader(TLS_EXTENSION_SUPPORTED_GROUPS, LData);
end;

function BuildExtensionECPointFormats: TBytes;
var
  LData: TBytes;
begin
  LData := nil;
  AppendByte(LData, 1);
  AppendByte(LData, TLS_EC_POINT_FORMAT_UNCOMPRESSED);
  Result := BuildExtensionHeader(TLS_EXTENSION_EC_POINT_FORMATS, LData);
end;

function BuildExtensionSignatureAlgorithms: TBytes;
var
  LData: TBytes;
begin
  LData := nil;
  AppendUInt16(LData, 8);
  AppendUInt16(LData, TLS_SIG_RSA_PSS_RSAE_SHA256);
  AppendUInt16(LData, TLS_SIG_RSA_PKCS1_SHA256);
  AppendUInt16(LData, TLS_SIG_RSA_PKCS1_SHA512);
  AppendUInt16(LData, TLS_SIG_ECDSA_SECP256R1_SHA256);
  Result := BuildExtensionHeader(TLS_EXTENSION_SIGNATURE_ALGORITHMS, LData);
end;

function BuildCipherSuitesVector(const ACipherSuites: array of Word): TBytes;
var
  I: Integer;
begin
  if Length(ACipherSuites) = 0 then
    RaiseInvalidParameter('TLS12CipherSuites');

  Result := nil;
  for I := 0 to High(ACipherSuites) do
    AppendUInt16(Result, ACipherSuites[I]);
end;

function BuildTLS12ClientHelloBody(
  const AServerName: string;
  const AALPNProtocols: string;
  const AClientRandom: TBytes;
  const ASessionID: TBytes;
  const ACipherSuites: array of Word;
  const ASessionTicket: TBytes
): TBytes;
var
  LCipherSuites: TBytes;
  LCompressionMethods: TBytes;
  LExtensions: TBytes;
  LExt: TBytes;
begin
  if Length(AClientRandom) <> TLS12_RANDOM_SIZE then
    RaiseInvalidParameter('TLS12ClientRandom');
  if Length(ASessionID) > 32 then
    RaiseInvalidParameter('TLS12SessionID');

  Result := nil;
  AppendUInt16(Result, TLS12_VERSION);
  AppendBytes(Result, AClientRandom);

  AppendByte(Result, Byte(Length(ASessionID)));
  AppendBytes(Result, ASessionID);

  LCipherSuites := BuildCipherSuitesVector(ACipherSuites);
  AppendUInt16(Result, Word(Length(LCipherSuites)));
  AppendBytes(Result, LCipherSuites);

  LCompressionMethods := nil;
  AppendByte(LCompressionMethods, 1);
  AppendByte(LCompressionMethods, TLS_COMPRESSION_NULL);
  AppendBytes(Result, LCompressionMethods);

  LExtensions := nil;

  LExt := BuildExtensionServerName(AServerName);
  AppendBytes(LExtensions, LExt);

  LExt := BuildExtensionALPN(AALPNProtocols);
  AppendBytes(LExtensions, LExt);

  LExt := BuildExtensionSupportedGroups;
  AppendBytes(LExtensions, LExt);

  LExt := BuildExtensionECPointFormats;
  AppendBytes(LExtensions, LExt);

  LExt := BuildExtensionSignatureAlgorithms;
  AppendBytes(LExtensions, LExt);

  LExt := BuildExtensionSessionTicket(ASessionTicket);
  AppendBytes(LExtensions, LExt);

  AppendUInt16(Result, Word(Length(LExtensions)));
  AppendBytes(Result, LExtensions);
end;

function BuildTLS12ClientHelloHandshake(
  const AServerName: string;
  const AALPNProtocols: string;
  const ACipherSuites: array of Word
): TBytes;
var
  LClientRandom: TBytes;
  LSessionID: TBytes;
begin
  LClientRandom := GenerateSecureRandomBytes(TLS12_RANDOM_SIZE);
  LSessionID := GenerateSecureRandomBytes(32);
  Result := BuildTLS12ClientHelloHandshakeWithParams(
    AServerName,
    AALPNProtocols,
    LClientRandom,
    LSessionID,
    ACipherSuites
  );
end;

function BuildTLS12ClientHelloHandshakeWithTicket(
  const AServerName: string;
  const AALPNProtocols: string;
  const ACipherSuites: array of Word;
  const ASessionTicket: TBytes
): TBytes;
var
  LClientRandom: TBytes;
  LSessionID: TBytes;
begin
  LClientRandom := GenerateSecureRandomBytes(TLS12_RANDOM_SIZE);
  LSessionID := GenerateSecureRandomBytes(32);
  Result := BuildTLS12ClientHelloHandshakeWithParamsAndTicket(
    AServerName,
    AALPNProtocols,
    LClientRandom,
    LSessionID,
    ACipherSuites,
    ASessionTicket
  );
end;

function BuildTLS12ClientHelloHandshakeWithParams(
  const AServerName: string;
  const AALPNProtocols: string;
  const AClientRandom: TBytes;
  const ASessionID: TBytes;
  const ACipherSuites: array of Word
): TBytes;
var
  LBody: TBytes;
begin
  Result := BuildTLS12ClientHelloHandshakeWithParamsAndTicket(
    AServerName,
    AALPNProtocols,
    AClientRandom,
    ASessionID,
    ACipherSuites,
    nil
  );
end;

function BuildTLS12ClientHelloHandshakeWithParamsAndTicket(
  const AServerName: string;
  const AALPNProtocols: string;
  const AClientRandom: TBytes;
  const ASessionID: TBytes;
  const ACipherSuites: array of Word;
  const ASessionTicket: TBytes
): TBytes;
var
  LBody: TBytes;
begin
  LBody := BuildTLS12ClientHelloBody(
    AServerName,
    AALPNProtocols,
    AClientRandom,
    ASessionID,
    ACipherSuites,
    ASessionTicket
  );

  Result := nil;
  AppendByte(Result, TLS_HANDSHAKE_TYPE_CLIENT_HELLO);
  AppendUInt24(Result, Length(LBody));
  AppendBytes(Result, LBody);
end;

function BuildTLS12ClientHelloRecord(
  const AServerName: string;
  const AALPNProtocols: string;
  const ACipherSuites: array of Word
): TBytes;
begin
  Result := BuildTLS12Plaintext(
    TLS_CONTENT_TYPE_HANDSHAKE,
    BuildTLS12ClientHelloHandshake(AServerName, AALPNProtocols, ACipherSuites)
  );
end;

function BuildTLS12ClientHelloRecordWithTicket(
  const AServerName: string;
  const AALPNProtocols: string;
  const ACipherSuites: array of Word;
  const ASessionTicket: TBytes
): TBytes;
begin
  Result := BuildTLS12Plaintext(
    TLS_CONTENT_TYPE_HANDSHAKE,
    BuildTLS12ClientHelloHandshakeWithTicket(AServerName, AALPNProtocols, ACipherSuites, ASessionTicket)
  );
end;

function BuildTLS12ClientHelloRecordWithParams(
  const AServerName: string;
  const AALPNProtocols: string;
  const AClientRandom: TBytes;
  const ASessionID: TBytes;
  const ACipherSuites: array of Word
): TBytes;
begin
  Result := BuildTLS12ClientHelloRecordWithParamsAndTicket(
    AServerName,
    AALPNProtocols,
    AClientRandom,
    ASessionID,
    ACipherSuites,
    nil
  );
end;

function BuildTLS12ClientHelloRecordWithParamsAndTicket(
  const AServerName: string;
  const AALPNProtocols: string;
  const AClientRandom: TBytes;
  const ASessionID: TBytes;
  const ACipherSuites: array of Word;
  const ASessionTicket: TBytes
): TBytes;
begin
  Result := BuildTLS12Plaintext(
    TLS_CONTENT_TYPE_HANDSHAKE,
    BuildTLS12ClientHelloHandshakeWithParamsAndTicket(
      AServerName,
      AALPNProtocols,
      AClientRandom,
      ASessionID,
      ACipherSuites,
      ASessionTicket
    )
  );
end;

end.
