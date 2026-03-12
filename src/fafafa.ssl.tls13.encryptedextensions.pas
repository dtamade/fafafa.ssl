{**
 * Unit: fafafa.ssl.tls13.encryptedextensions
 * Purpose: TLS 1.3 EncryptedExtensions parser（当前聚焦 ALPN）
 *}

unit fafafa.ssl.tls13.encryptedextensions;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  fafafa.ssl.tls13.wire;

type
  TTLS13EncryptedExtensionsInfo = record
    Valid: Boolean;
    HasALPN: Boolean;
    SelectedALPNProtocol: string;
  end;

function TryParseEncryptedExtensionsFromHandshake(
  const AHandshake: TBytes;
  out AInfo: TTLS13EncryptedExtensionsInfo;
  out AError: string
): Boolean;

implementation

function TryParseEncryptedExtensionsFromHandshake(
  const AHandshake: TBytes;
  out AInfo: TTLS13EncryptedExtensionsInfo;
  out AError: string
): Boolean;
var
  LOffset: Integer;
  LBodyLength: Cardinal;
  LBodyEnd: Integer;
  LExtensionsLength: Integer;
  LExtensionsEnd: Integer;
  LExtType: Word;
  LExtLen: Word;
  LListLength: Integer;
  LProtoLen: Integer;
begin
  AInfo := Default(TTLS13EncryptedExtensionsInfo);
  AError := '';
  Result := False;

  if Length(AHandshake) < 4 then
  begin
    AError := 'EncryptedExtensions handshake is too short';
    Exit;
  end;

  if AHandshake[0] <> TLS_HANDSHAKE_TYPE_ENCRYPTED_EXTENSIONS then
  begin
    AError := 'Handshake message is not EncryptedExtensions';
    Exit;
  end;

  LBodyLength := ReadUInt24(AHandshake, 1);
  if LBodyLength > Cardinal(High(Integer) - 4) then
  begin
    AError := 'EncryptedExtensions length is too large';
    Exit;
  end;

  LBodyEnd := 4 + Integer(LBodyLength);
  if Length(AHandshake) <> LBodyEnd then
  begin
    AError := 'EncryptedExtensions body length mismatch';
    Exit;
  end;

  LOffset := 4;
  if LOffset + 2 > LBodyEnd then
  begin
    AError := 'Missing EncryptedExtensions length';
    Exit;
  end;

  LExtensionsLength := ReadUInt16(AHandshake, LOffset);
  Inc(LOffset, 2);
  LExtensionsEnd := LOffset + LExtensionsLength;
  if LExtensionsEnd <> LBodyEnd then
  begin
    AError := 'EncryptedExtensions length mismatch';
    Exit;
  end;

  while LOffset + 4 <= LExtensionsEnd do
  begin
    LExtType := ReadUInt16(AHandshake, LOffset);
    LExtLen := ReadUInt16(AHandshake, LOffset + 2);
    Inc(LOffset, 4);

    if LOffset + Integer(LExtLen) > LExtensionsEnd then
    begin
      AError := 'EncryptedExtensions entry exceeds extension block';
      Exit;
    end;

    if LExtType = TLS_EXTENSION_ALPN then
    begin
      if LExtLen < 3 then
      begin
        AError := 'ALPN extension is too short';
        Exit;
      end;

      LListLength := ReadUInt16(AHandshake, LOffset);
      if LListLength <> Integer(LExtLen) - 2 then
      begin
        AError := 'ALPN list length mismatch';
        Exit;
      end;

      LProtoLen := AHandshake[LOffset + 2];
      if (LProtoLen <= 0) or (LProtoLen <> LListLength - 1) or
        (LOffset + 3 + LProtoLen <> LOffset + Integer(LExtLen)) then
      begin
        AError := 'ALPN selected protocol encoding is invalid';
        Exit;
      end;

      SetLength(AInfo.SelectedALPNProtocol, LProtoLen);
      Move(AHandshake[LOffset + 3], AInfo.SelectedALPNProtocol[1], LProtoLen);
      AInfo.HasALPN := True;
    end;

    Inc(LOffset, Integer(LExtLen));
  end;

  if LOffset <> LExtensionsEnd then
  begin
    AError := 'EncryptedExtensions block has trailing bytes';
    Exit;
  end;

  AInfo.Valid := True;
  Result := True;
end;

end.
