{**
 * Unit: fafafa.ssl.tls13.parser
 * Purpose: TLS 1.3 ServerHello 解析器（纯 Pascal）
 *}

unit fafafa.ssl.tls13.parser;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  fafafa.ssl.tls13.wire;

type
  TTLS13ServerHelloInfo = record
    Valid: Boolean;
    LegacyVersion: Word;
    SelectedVersion: Word;
    SelectedCipherSuite: Word;
    HasKeyShare: Boolean;
    KeyShareGroup: Word;
    KeyShareLength: Word;
    PeerKeyShare: TBytes;
  end;

function TryExtractHandshakePayloadFromRecord(const ARecord: TBytes; out AHandshake: TBytes): Boolean;
function TryParseServerHelloFromHandshake(const AHandshake: TBytes; out AInfo: TTLS13ServerHelloInfo): Boolean;

implementation

procedure InitInfo(out AInfo: TTLS13ServerHelloInfo);
begin
  FillChar(AInfo, SizeOf(AInfo), 0);
  AInfo.SelectedVersion := TLS_LEGACY_VERSION;
  SetLength(AInfo.PeerKeyShare, 0);
end;

function TryExtractHandshakePayloadFromRecord(const ARecord: TBytes; out AHandshake: TBytes): Boolean;
var
  LHeader: TTLSRecordHeader;
  LLen: Integer;
begin
  SetLength(AHandshake, 0);
  Result := False;

  if not ParseTLSRecordHeader(ARecord, LHeader) then
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

function TryParseServerHelloFromHandshake(const AHandshake: TBytes; out AInfo: TTLS13ServerHelloInfo): Boolean;
var
  LOffset: Integer;
  LMsgType: Byte;
  LBodyLen: Cardinal;
  LSessionIdLen: Integer;
  LExtTotalLen: Integer;
  LExtEnd: Integer;
  LExtType, LExtLen: Word;
  LExtDataStart: Integer;
  LPeerShareLen: Integer;
begin
  InitInfo(AInfo);
  Result := False;

  if Length(AHandshake) < 4 then
    Exit;

  LMsgType := AHandshake[0];
  if LMsgType <> TLS_HANDSHAKE_TYPE_SERVER_HELLO then
    Exit;

  LBodyLen := ReadUInt24(AHandshake, 1);
  if Length(AHandshake) < 4 + Integer(LBodyLen) then
    Exit;

  LOffset := 4;

  // legacy_version
  if LOffset + 2 > Length(AHandshake) then
    Exit;
  AInfo.LegacyVersion := ReadUInt16(AHandshake, LOffset);
  Inc(LOffset, 2);

  // random
  if LOffset + 32 > Length(AHandshake) then
    Exit;
  Inc(LOffset, 32);

  // legacy_session_id_echo
  if LOffset + 1 > Length(AHandshake) then
    Exit;
  LSessionIdLen := AHandshake[LOffset];
  Inc(LOffset);
  if LOffset + LSessionIdLen > Length(AHandshake) then
    Exit;
  Inc(LOffset, LSessionIdLen);

  // cipher_suite
  if LOffset + 2 > Length(AHandshake) then
    Exit;
  AInfo.SelectedCipherSuite := ReadUInt16(AHandshake, LOffset);
  Inc(LOffset, 2);

  // legacy_compression_method
  if LOffset + 1 > Length(AHandshake) then
    Exit;
  Inc(LOffset, 1);

  // extensions
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
      TLS_EXTENSION_SUPPORTED_VERSIONS:
        begin
          if LExtLen = 2 then
            AInfo.SelectedVersion := ReadUInt16(AHandshake, LExtDataStart)
          else
            Exit;
        end;

      TLS_EXTENSION_KEY_SHARE:
        begin
          if LExtLen < 4 then
            Exit;

          AInfo.HasKeyShare := True;
          AInfo.KeyShareGroup := ReadUInt16(AHandshake, LExtDataStart);
          AInfo.KeyShareLength := ReadUInt16(AHandshake, LExtDataStart + 2);

          LPeerShareLen := Integer(AInfo.KeyShareLength);
          if LPeerShareLen <> Integer(LExtLen) - 4 then
            Exit;

          SetLength(AInfo.PeerKeyShare, LPeerShareLen);
          if LPeerShareLen > 0 then
            Move(AHandshake[LExtDataStart + 4], AInfo.PeerKeyShare[0], LPeerShareLen);
        end;
    end;

    Inc(LOffset, LExtLen);
  end;

  if LOffset <> LExtEnd then
    Exit;

  AInfo.Valid := True;
  Result := True;
end;

end.
