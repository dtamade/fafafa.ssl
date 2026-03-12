{**
 * Unit: fafafa.ssl.tls12.serverhello.parser
 * Purpose: TLS 1.2 server-flight parser foundation
 *}

unit fafafa.ssl.tls12.serverhello.parser;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  fafafa.ssl.tls12.wire;

type
  TTLS12CertificateList = array of TBytes;

  TTLS12ServerHelloInfo = record
    Valid: Boolean;
    ServerVersion: Word;
    Random: TBytes;
    SessionID: TBytes;
    SelectedCipherSuite: Word;
    CompressionMethod: Byte;
    SelectedALPNProtocol: string;
  end;

  TTLS12ServerKeyExchangeInfo = record
    Valid: Boolean;
    CurveType: Byte;
    NamedCurve: Word;
    PublicKey: TBytes;
    SignatureAlgorithm: Word;
    Signature: TBytes;
  end;

  TTLS12NewSessionTicketInfo = record
    Valid: Boolean;
    TicketLifetimeHint: Cardinal;
    Ticket: TBytes;
  end;

function TryExtractTLS12HandshakePayloadFromRecord(const ARecord: TBytes; out AHandshake: TBytes): Boolean;
function TryParseTLS12ServerHelloFromHandshake(const AHandshake: TBytes; out AInfo: TTLS12ServerHelloInfo): Boolean;
function TryParseTLS12CertificateFromHandshake(
  const AHandshake: TBytes;
  out ACertificates: TTLS12CertificateList;
  out AError: string
): Boolean;
function TryParseTLS12ServerKeyExchangeECDHERSAFromHandshake(
  const AHandshake: TBytes;
  out AInfo: TTLS12ServerKeyExchangeInfo;
  out AError: string
): Boolean;
function TryParseTLS12ServerHelloDoneFromHandshake(const AHandshake: TBytes): Boolean;
function TryParseTLS12NewSessionTicketFromHandshake(
  const AHandshake: TBytes;
  out AInfo: TTLS12NewSessionTicketInfo;
  out AError: string
): Boolean;

implementation

procedure InitServerHelloInfo(out AInfo: TTLS12ServerHelloInfo);
begin
  FillChar(AInfo, SizeOf(AInfo), 0);
  SetLength(AInfo.Random, 0);
  SetLength(AInfo.SessionID, 0);
end;

procedure InitServerKeyExchangeInfo(out AInfo: TTLS12ServerKeyExchangeInfo);
begin
  FillChar(AInfo, SizeOf(AInfo), 0);
  SetLength(AInfo.PublicKey, 0);
  SetLength(AInfo.Signature, 0);
end;

function TryExtractTLS12HandshakePayloadFromRecord(const ARecord: TBytes; out AHandshake: TBytes): Boolean;
var
  LHeader: TTLS12RecordHeader;
  LLen: Integer;
begin
  SetLength(AHandshake, 0);
  Result := False;

  if not ParseTLS12RecordHeader(ARecord, LHeader) then
    Exit;
  if LHeader.ContentType <> TLS_CONTENT_TYPE_HANDSHAKE then
    Exit;

  LLen := LHeader.Length;
  if Length(ARecord) < 5 + LLen then
    Exit;

  SetLength(AHandshake, LLen);
  if LLen > 0 then
    Move(ARecord[5], AHandshake[0], LLen);
  Result := True;
end;

function TryParseTLS12ServerHelloFromHandshake(const AHandshake: TBytes; out AInfo: TTLS12ServerHelloInfo): Boolean;
var
  LOffset: Integer;
  LBodyLen: Cardinal;
  LExtTotalLen: Integer;
  LExtEnd: Integer;
  LExtType, LExtLen: Word;
  LExtDataStart: Integer;
  LSessionIdLen: Integer;
  LALPNListLen: Word;
  LProtoLen: Integer;
begin
  InitServerHelloInfo(AInfo);
  Result := False;

  if Length(AHandshake) < 4 then
    Exit;
  if AHandshake[0] <> TLS_HANDSHAKE_TYPE_SERVER_HELLO then
    Exit;

  LBodyLen := ReadUInt24(AHandshake, 1);
  if Length(AHandshake) <> 4 + Integer(LBodyLen) then
    Exit;

  LOffset := 4;
  if LOffset + 2 + 32 + 1 > Length(AHandshake) then
    Exit;
  AInfo.ServerVersion := ReadUInt16(AHandshake, LOffset);
  Inc(LOffset, 2);

  SetLength(AInfo.Random, 32);
  Move(AHandshake[LOffset], AInfo.Random[0], 32);
  Inc(LOffset, 32);

  LSessionIdLen := AHandshake[LOffset];
  Inc(LOffset);
  if LOffset + LSessionIdLen > Length(AHandshake) then
    Exit;
  SetLength(AInfo.SessionID, LSessionIdLen);
  if LSessionIdLen > 0 then
    Move(AHandshake[LOffset], AInfo.SessionID[0], LSessionIdLen);
  Inc(LOffset, LSessionIdLen);

  AInfo.SelectedCipherSuite := ReadUInt16(AHandshake, LOffset);
  Inc(LOffset, 2);
  AInfo.CompressionMethod := AHandshake[LOffset];
  Inc(LOffset);

  if LOffset = Length(AHandshake) then
  begin
    AInfo.Valid := True;
    Result := True;
    Exit;
  end;
  if LOffset + 2 > Length(AHandshake) then
    Exit;

  LExtTotalLen := ReadUInt16(AHandshake, LOffset);
  Inc(LOffset, 2);
  LExtEnd := LOffset + LExtTotalLen;
  if LExtEnd > Length(AHandshake) then
    Exit;

  while LOffset + 4 <= LExtEnd do
  begin
    LExtType := ReadUInt16(AHandshake, LOffset);
    LExtLen := ReadUInt16(AHandshake, LOffset + 2);
    Inc(LOffset, 4);
    if LOffset + LExtLen > LExtEnd then
      Exit;

    LExtDataStart := LOffset;
    case LExtType of
      TLS_EXTENSION_ALPN:
        begin
          if LExtLen < 3 then
            Exit;
          LALPNListLen := ReadUInt16(AHandshake, LExtDataStart);
          if Integer(LALPNListLen) <> Integer(LExtLen) - 2 then
            Exit;
          LProtoLen := AHandshake[LExtDataStart + 2];
          if (LProtoLen <= 0) or (LProtoLen + 3 > LExtLen) then
            Exit;
          SetString(AInfo.SelectedALPNProtocol, PChar(@AHandshake[LExtDataStart + 3]), LProtoLen);
        end;
    end;

    Inc(LOffset, LExtLen);
  end;

  if LOffset <> LExtEnd then
    Exit;

  AInfo.Valid := True;
  Result := True;
end;

function TryParseTLS12CertificateFromHandshake(
  const AHandshake: TBytes;
  out ACertificates: TTLS12CertificateList;
  out AError: string
): Boolean;
var
  LBodyLen: Cardinal;
  LOffset: Integer;
  LListLen: Cardinal;
  LListEnd: Integer;
  LCertLen: Cardinal;
  LCount: Integer;
begin
  Result := False;
  AError := '';
  SetLength(ACertificates, 0);

  if Length(AHandshake) < 7 then
  begin
    AError := 'Certificate handshake too short';
    Exit;
  end;
  if AHandshake[0] <> TLS_HANDSHAKE_TYPE_CERTIFICATE then
  begin
    AError := 'Unexpected handshake type for Certificate';
    Exit;
  end;

  LBodyLen := ReadUInt24(AHandshake, 1);
  if Length(AHandshake) <> 4 + Integer(LBodyLen) then
  begin
    AError := 'Certificate handshake length mismatch';
    Exit;
  end;

  LOffset := 4;
  LListLen := ReadUInt24(AHandshake, LOffset);
  Inc(LOffset, 3);
  LListEnd := LOffset + Integer(LListLen);
  if LListEnd <> Length(AHandshake) then
  begin
    AError := 'Certificate list length mismatch';
    Exit;
  end;

  LCount := 0;
  while LOffset < LListEnd do
  begin
    if LOffset + 3 > LListEnd then
    begin
      AError := 'Certificate entry header truncated';
      Exit;
    end;

    LCertLen := ReadUInt24(AHandshake, LOffset);
    Inc(LOffset, 3);
    if LOffset + Integer(LCertLen) > LListEnd then
    begin
      AError := 'Certificate entry exceeds list';
      Exit;
    end;

    SetLength(ACertificates, LCount + 1);
    SetLength(ACertificates[LCount], LCertLen);
    if LCertLen > 0 then
      Move(AHandshake[LOffset], ACertificates[LCount][0], LCertLen);
    Inc(LOffset, Integer(LCertLen));
    Inc(LCount);
  end;

  Result := True;
end;

function TryParseTLS12ServerKeyExchangeECDHERSAFromHandshake(
  const AHandshake: TBytes;
  out AInfo: TTLS12ServerKeyExchangeInfo;
  out AError: string
): Boolean;
var
  LBodyLen: Cardinal;
  LOffset: Integer;
  LPublicKeyLen: Integer;
  LSignatureLen: Integer;
begin
  InitServerKeyExchangeInfo(AInfo);
  Result := False;
  AError := '';

  if Length(AHandshake) < 4 then
  begin
    AError := 'ServerKeyExchange handshake too short';
    Exit;
  end;
  if AHandshake[0] <> TLS_HANDSHAKE_TYPE_SERVER_KEY_EXCHANGE then
  begin
    AError := 'Unexpected handshake type for ServerKeyExchange';
    Exit;
  end;

  LBodyLen := ReadUInt24(AHandshake, 1);
  if Length(AHandshake) <> 4 + Integer(LBodyLen) then
  begin
    AError := 'ServerKeyExchange handshake length mismatch';
    Exit;
  end;

  LOffset := 4;
  AInfo.CurveType := AHandshake[LOffset];
  Inc(LOffset);
  AInfo.NamedCurve := ReadUInt16(AHandshake, LOffset);
  Inc(LOffset, 2);

  LPublicKeyLen := AHandshake[LOffset];
  Inc(LOffset);
  if LOffset + LPublicKeyLen > Length(AHandshake) then
  begin
    AError := 'ServerKeyExchange public key exceeds body';
    Exit;
  end;
  SetLength(AInfo.PublicKey, LPublicKeyLen);
  if LPublicKeyLen > 0 then
    Move(AHandshake[LOffset], AInfo.PublicKey[0], LPublicKeyLen);
  Inc(LOffset, LPublicKeyLen);

  AInfo.SignatureAlgorithm := ReadUInt16(AHandshake, LOffset);
  Inc(LOffset, 2);

  LSignatureLen := ReadUInt16(AHandshake, LOffset);
  Inc(LOffset, 2);
  if LOffset + LSignatureLen > Length(AHandshake) then
  begin
    AError := 'ServerKeyExchange signature exceeds body';
    Exit;
  end;
  SetLength(AInfo.Signature, LSignatureLen);
  if LSignatureLen > 0 then
    Move(AHandshake[LOffset], AInfo.Signature[0], LSignatureLen);
  Inc(LOffset, LSignatureLen);

  if LOffset <> Length(AHandshake) then
  begin
    AError := 'ServerKeyExchange trailing bytes mismatch';
    Exit;
  end;

  AInfo.Valid := True;
  Result := True;
end;

function TryParseTLS12ServerHelloDoneFromHandshake(const AHandshake: TBytes): Boolean;
begin
  Result :=
    (Length(AHandshake) = 4) and
    (AHandshake[0] = TLS_HANDSHAKE_TYPE_SERVER_HELLO_DONE) and
    (ReadUInt24(AHandshake, 1) = 0);
end;

function TryParseTLS12NewSessionTicketFromHandshake(
  const AHandshake: TBytes;
  out AInfo: TTLS12NewSessionTicketInfo;
  out AError: string
): Boolean;
var
  LBodyLen: Cardinal;
  LOffset: Integer;
  LTicketLen: Word;
begin
  Result := False;
  AError := '';
  FillChar(AInfo, SizeOf(AInfo), 0);
  SetLength(AInfo.Ticket, 0);

  if Length(AHandshake) < 10 then
  begin
    AError := 'NewSessionTicket handshake too short';
    Exit;
  end;
  if AHandshake[0] <> TLS_HANDSHAKE_TYPE_NEW_SESSION_TICKET then
  begin
    AError := 'Unexpected handshake type for NewSessionTicket';
    Exit;
  end;

  LBodyLen := ReadUInt24(AHandshake, 1);
  if Length(AHandshake) <> 4 + Integer(LBodyLen) then
  begin
    AError := 'NewSessionTicket length mismatch';
    Exit;
  end;

  LOffset := 4;
  AInfo.TicketLifetimeHint :=
    (Cardinal(AHandshake[LOffset]) shl 24) or
    (Cardinal(AHandshake[LOffset + 1]) shl 16) or
    (Cardinal(AHandshake[LOffset + 2]) shl 8) or
    Cardinal(AHandshake[LOffset + 3]);
  Inc(LOffset, 4);

  LTicketLen := ReadUInt16(AHandshake, LOffset);
  Inc(LOffset, 2);
  if (LTicketLen = 0) or (LOffset + LTicketLen <> Length(AHandshake)) then
  begin
    AError := 'NewSessionTicket ticket length mismatch';
    Exit;
  end;

  SetLength(AInfo.Ticket, LTicketLen);
  Move(AHandshake[LOffset], AInfo.Ticket[0], LTicketLen);
  AInfo.Valid := True;
  Result := True;
end;

end.
