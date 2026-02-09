{**
 * Unit: fafafa.ssl.tls13.clienthello.parser
 * Purpose: TLS 1.3 ClientHello 解析器（纯 Pascal）
 *
 * 说明：
 * - 只解析单个 Handshake message（含 4 字节握手头）
 * - 当前聚焦 TLS 1.3 协商关键字段：versions / cipher_suites / key_share / signature_algorithms
 * - key_share 优先选择 X25519；若无 X25519 则保留首个条目用于上层报错
 *}

unit fafafa.ssl.tls13.clienthello.parser;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  fafafa.ssl.tls13.wire;

type
  TTLS13WordArray = array of Word;

  TTLS13ClientHelloInfo = record
    Valid: Boolean;
    LegacyVersion: Word;
    Random: TBytes;
    LegacySessionID: TBytes;
    CipherSuites: TTLS13WordArray;
    SupportedVersions: TTLS13WordArray;
    SignatureAlgorithms: TTLS13WordArray;
    HasSupportedVersions: Boolean;
    HasSignatureAlgorithms: Boolean;
    HasKeyShare: Boolean;
    KeyShareGroup: Word;
    KeyShareLength: Word;
    PeerKeyShare: TBytes;
  end;

function TryParseTLS13ClientHelloFromHandshake(
  const AHandshake: TBytes;
  out AInfo: TTLS13ClientHelloInfo;
  out AError: string
): Boolean;

function TLS13ClientHelloSupportsVersion(const AInfo: TTLS13ClientHelloInfo; AVersion: Word): Boolean;
function TLS13ClientHelloOffersCipherSuite(const AInfo: TTLS13ClientHelloInfo; ACipherSuite: Word): Boolean;
function TLS13ClientHelloOffersSignatureScheme(const AInfo: TTLS13ClientHelloInfo; ASignatureScheme: Word): Boolean;

implementation

procedure InitClientHelloInfo(out AInfo: TTLS13ClientHelloInfo);
begin
  FillChar(AInfo, SizeOf(AInfo), 0);
  SetLength(AInfo.Random, 0);
  SetLength(AInfo.LegacySessionID, 0);
  SetLength(AInfo.CipherSuites, 0);
  SetLength(AInfo.SupportedVersions, 0);
  SetLength(AInfo.SignatureAlgorithms, 0);
  SetLength(AInfo.PeerKeyShare, 0);
end;

function TLS13ClientHelloSupportsVersion(const AInfo: TTLS13ClientHelloInfo; AVersion: Word): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AInfo.SupportedVersions) do
    if AInfo.SupportedVersions[I] = AVersion then
      Exit(True);
  Result := False;
end;

function TLS13ClientHelloOffersCipherSuite(const AInfo: TTLS13ClientHelloInfo; ACipherSuite: Word): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AInfo.CipherSuites) do
    if AInfo.CipherSuites[I] = ACipherSuite then
      Exit(True);
  Result := False;
end;

function TLS13ClientHelloOffersSignatureScheme(const AInfo: TTLS13ClientHelloInfo; ASignatureScheme: Word): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AInfo.SignatureAlgorithms) do
    if AInfo.SignatureAlgorithms[I] = ASignatureScheme then
      Exit(True);
  Result := False;
end;

procedure ParseSupportedVersionsExtension(
  const AHandshake: TBytes;
  ADataOffset, ADataLength: Integer;
  var AInfo: TTLS13ClientHelloInfo;
  out AError: string
);
var
  LListLength: Integer;
  LOffset: Integer;
  LCount: Integer;
  I: Integer;
begin
  AError := '';

  if ADataLength < 1 then
  begin
    AError := 'supported_versions extension is too short';
    Exit;
  end;

  LListLength := AHandshake[ADataOffset];
  if LListLength + 1 <> ADataLength then
  begin
    AError := 'supported_versions length mismatch';
    Exit;
  end;

  if (LListLength and 1) <> 0 then
  begin
    AError := 'supported_versions vector length must be even';
    Exit;
  end;

  LCount := LListLength div 2;
  SetLength(AInfo.SupportedVersions, LCount);
  LOffset := ADataOffset + 1;
  for I := 0 to LCount - 1 do
  begin
    AInfo.SupportedVersions[I] := ReadUInt16(AHandshake, LOffset);
    Inc(LOffset, 2);
  end;

  AInfo.HasSupportedVersions := True;
end;

procedure ParseSignatureAlgorithmsExtension(
  const AHandshake: TBytes;
  ADataOffset, ADataLength: Integer;
  var AInfo: TTLS13ClientHelloInfo;
  out AError: string
);
var
  LListLength: Integer;
  LOffset: Integer;
  LCount: Integer;
  I: Integer;
begin
  AError := '';

  if ADataLength < 2 then
  begin
    AError := 'signature_algorithms extension is too short';
    Exit;
  end;

  LListLength := ReadUInt16(AHandshake, ADataOffset);
  if LListLength <> ADataLength - 2 then
  begin
    AError := 'signature_algorithms length mismatch';
    Exit;
  end;

  if (LListLength < 2) or ((LListLength and 1) <> 0) then
  begin
    AError := 'signature_algorithms vector length is invalid';
    Exit;
  end;

  LCount := LListLength div 2;
  SetLength(AInfo.SignatureAlgorithms, LCount);
  LOffset := ADataOffset + 2;
  for I := 0 to LCount - 1 do
  begin
    AInfo.SignatureAlgorithms[I] := ReadUInt16(AHandshake, LOffset);
    Inc(LOffset, 2);
  end;

  AInfo.HasSignatureAlgorithms := True;
end;

procedure ParseKeyShareExtension(
  const AHandshake: TBytes;
  ADataOffset, ADataLength: Integer;
  var AInfo: TTLS13ClientHelloInfo;
  out AError: string
);
var
  LClientSharesLength: Integer;
  LOffset: Integer;
  LEndPos: Integer;
  LGroup: Word;
  LKeyLen: Word;
  LFoundAny: Boolean;
  LFoundX25519: Boolean;
begin
  AError := '';

  if ADataLength < 2 then
  begin
    AError := 'key_share extension is too short';
    Exit;
  end;

  LClientSharesLength := ReadUInt16(AHandshake, ADataOffset);
  if LClientSharesLength <> ADataLength - 2 then
  begin
    AError := 'key_share client_shares length mismatch';
    Exit;
  end;

  LOffset := ADataOffset + 2;
  LEndPos := LOffset + LClientSharesLength;
  LFoundAny := False;
  LFoundX25519 := False;

  while LOffset + 4 <= LEndPos do
  begin
    LGroup := ReadUInt16(AHandshake, LOffset);
    LKeyLen := ReadUInt16(AHandshake, LOffset + 2);
    Inc(LOffset, 4);

    if LOffset + Integer(LKeyLen) > LEndPos then
    begin
      AError := 'key_share entry exceeds extension boundary';
      Exit;
    end;

    if not LFoundAny then
    begin
      AInfo.KeyShareGroup := LGroup;
      AInfo.KeyShareLength := LKeyLen;
      SetLength(AInfo.PeerKeyShare, Integer(LKeyLen));
      if LKeyLen > 0 then
        Move(AHandshake[LOffset], AInfo.PeerKeyShare[0], Integer(LKeyLen));
      LFoundAny := True;
    end;

    if (LGroup = TLS13_GROUP_X25519) and (not LFoundX25519) then
    begin
      AInfo.KeyShareGroup := LGroup;
      AInfo.KeyShareLength := LKeyLen;
      SetLength(AInfo.PeerKeyShare, Integer(LKeyLen));
      if LKeyLen > 0 then
        Move(AHandshake[LOffset], AInfo.PeerKeyShare[0], Integer(LKeyLen));
      LFoundX25519 := True;
    end;

    Inc(LOffset, Integer(LKeyLen));
  end;

  if LOffset <> LEndPos then
  begin
    AError := 'key_share extension has trailing bytes';
    Exit;
  end;

  AInfo.HasKeyShare := LFoundAny;
end;

function TryParseTLS13ClientHelloFromHandshake(
  const AHandshake: TBytes;
  out AInfo: TTLS13ClientHelloInfo;
  out AError: string
): Boolean;
var
  LOffset: Integer;
  LBodyLength: Cardinal;
  LBodyEnd: Integer;
  LSessionIdLen: Integer;
  LCipherSuitesLength: Integer;
  LCipherCount: Integer;
  LCompressionMethodsLength: Integer;
  LExtensionsLength: Integer;
  LExtensionsEnd: Integer;
  LExtType: Word;
  LExtLen: Word;
  LExtDataOffset: Integer;
  I: Integer;
  LExtError: string;
begin
  InitClientHelloInfo(AInfo);
  AError := '';
  Result := False;

  if Length(AHandshake) < 4 then
  begin
    AError := 'Handshake message is too short';
    Exit;
  end;

  if AHandshake[0] <> TLS_HANDSHAKE_TYPE_CLIENT_HELLO then
  begin
    AError := 'Handshake message is not ClientHello';
    Exit;
  end;

  LBodyLength := ReadUInt24(AHandshake, 1);
  if LBodyLength > Cardinal(High(Integer) - 4) then
  begin
    AError := 'ClientHello length is too large';
    Exit;
  end;

  LBodyEnd := 4 + Integer(LBodyLength);
  if Length(AHandshake) <> LBodyEnd then
  begin
    AError := 'ClientHello body length mismatch';
    Exit;
  end;

  LOffset := 4;

  if LOffset + 2 > LBodyEnd then
  begin
    AError := 'Missing legacy_version';
    Exit;
  end;
  AInfo.LegacyVersion := ReadUInt16(AHandshake, LOffset);
  Inc(LOffset, 2);

  if LOffset + 32 > LBodyEnd then
  begin
    AError := 'Missing random bytes';
    Exit;
  end;
  SetLength(AInfo.Random, 32);
  Move(AHandshake[LOffset], AInfo.Random[0], 32);
  Inc(LOffset, 32);

  if LOffset + 1 > LBodyEnd then
  begin
    AError := 'Missing legacy_session_id length';
    Exit;
  end;
  LSessionIdLen := AHandshake[LOffset];
  Inc(LOffset);
  if LOffset + LSessionIdLen > LBodyEnd then
  begin
    AError := 'legacy_session_id exceeds ClientHello body';
    Exit;
  end;
  SetLength(AInfo.LegacySessionID, LSessionIdLen);
  if LSessionIdLen > 0 then
    Move(AHandshake[LOffset], AInfo.LegacySessionID[0], LSessionIdLen);
  Inc(LOffset, LSessionIdLen);

  if LOffset + 2 > LBodyEnd then
  begin
    AError := 'Missing cipher_suites length';
    Exit;
  end;
  LCipherSuitesLength := ReadUInt16(AHandshake, LOffset);
  Inc(LOffset, 2);
  if (LCipherSuitesLength < 2) or ((LCipherSuitesLength and 1) <> 0) then
  begin
    AError := 'cipher_suites length is invalid';
    Exit;
  end;
  if LOffset + LCipherSuitesLength > LBodyEnd then
  begin
    AError := 'cipher_suites exceeds ClientHello body';
    Exit;
  end;
  LCipherCount := LCipherSuitesLength div 2;
  SetLength(AInfo.CipherSuites, LCipherCount);
  for I := 0 to LCipherCount - 1 do
  begin
    AInfo.CipherSuites[I] := ReadUInt16(AHandshake, LOffset);
    Inc(LOffset, 2);
  end;

  if LOffset + 1 > LBodyEnd then
  begin
    AError := 'Missing legacy_compression_methods length';
    Exit;
  end;
  LCompressionMethodsLength := AHandshake[LOffset];
  Inc(LOffset);
  if LOffset + LCompressionMethodsLength > LBodyEnd then
  begin
    AError := 'legacy_compression_methods exceeds ClientHello body';
    Exit;
  end;
  Inc(LOffset, LCompressionMethodsLength);

  if LOffset + 2 > LBodyEnd then
  begin
    AError := 'Missing extensions length';
    Exit;
  end;
  LExtensionsLength := ReadUInt16(AHandshake, LOffset);
  Inc(LOffset, 2);
  LExtensionsEnd := LOffset + LExtensionsLength;
  if LExtensionsEnd <> LBodyEnd then
  begin
    AError := 'extensions length mismatch';
    Exit;
  end;

  while LOffset + 4 <= LExtensionsEnd do
  begin
    LExtType := ReadUInt16(AHandshake, LOffset);
    LExtLen := ReadUInt16(AHandshake, LOffset + 2);
    Inc(LOffset, 4);

    if LOffset + Integer(LExtLen) > LExtensionsEnd then
    begin
      AError := 'Extension length exceeds extension block';
      Exit;
    end;

    LExtDataOffset := LOffset;

    case LExtType of
      TLS_EXTENSION_SUPPORTED_VERSIONS:
        begin
          ParseSupportedVersionsExtension(AHandshake, LExtDataOffset, LExtLen, AInfo, LExtError);
          if LExtError <> '' then
          begin
            AError := LExtError;
            Exit;
          end;
        end;

      TLS_EXTENSION_SIGNATURE_ALGORITHMS:
        begin
          ParseSignatureAlgorithmsExtension(AHandshake, LExtDataOffset, LExtLen, AInfo, LExtError);
          if LExtError <> '' then
          begin
            AError := LExtError;
            Exit;
          end;
        end;

      TLS_EXTENSION_KEY_SHARE:
        begin
          ParseKeyShareExtension(AHandshake, LExtDataOffset, LExtLen, AInfo, LExtError);
          if LExtError <> '' then
          begin
            AError := LExtError;
            Exit;
          end;
        end;
    end;

    Inc(LOffset, Integer(LExtLen));
  end;

  if LOffset <> LExtensionsEnd then
  begin
    AError := 'Extension block has trailing bytes';
    Exit;
  end;

  AInfo.Valid := True;
  Result := True;
end;

end.
