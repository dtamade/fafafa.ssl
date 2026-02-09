{**
 * Unit: fafafa.ssl.tls13.posthandshake
 * Purpose: TLS 1.3 后握手消息解析（当前实现 NewSessionTicket/KeyUpdate）
 *
 * 纯 Pascal 实现，不依赖外部 TLS 库。
 * 输入为完整 Handshake 消息（包含 4 字节握手头）。
 *}

unit fafafa.ssl.tls13.posthandshake;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils;

type
  TTLS13NewSessionTicket = record
    Valid: Boolean;
    TicketLifetime: Cardinal;
    TicketAgeAdd: Cardinal;
    TicketNonce: TBytes;
    Ticket: TBytes;
    Extensions: TBytes;
  end;

  TTLS13KeyUpdateInfo = record
    Valid: Boolean;
    RequestUpdate: Boolean;
  end;

procedure InitTLS13NewSessionTicket(out ATicket: TTLS13NewSessionTicket);
procedure InitTLS13KeyUpdateInfo(out AInfo: TTLS13KeyUpdateInfo);

function TryParseTLS13NewSessionTicket(
  const AHandshakeMessage: TBytes;
  out ATicket: TTLS13NewSessionTicket;
  out AError: string
): Boolean;

function TryParseTLS13KeyUpdate(
  const AHandshakeMessage: TBytes;
  out AInfo: TTLS13KeyUpdateInfo;
  out AError: string
): Boolean;

implementation

uses
  fafafa.ssl.tls13.wire;

function ReadUInt32BE(const AData: TBytes; AOffset: Integer): Cardinal;
begin
  if (AOffset < 0) or (AOffset + 3 >= Length(AData)) then
    raise Exception.Create('Invalid uint32 offset');

  Result :=
    (Cardinal(AData[AOffset]) shl 24) or
    (Cardinal(AData[AOffset + 1]) shl 16) or
    (Cardinal(AData[AOffset + 2]) shl 8) or
    Cardinal(AData[AOffset + 3]);
end;

procedure InitTLS13NewSessionTicket(out ATicket: TTLS13NewSessionTicket);
begin
  FillChar(ATicket, SizeOf(ATicket), 0);
  SetLength(ATicket.TicketNonce, 0);
  SetLength(ATicket.Ticket, 0);
  SetLength(ATicket.Extensions, 0);
end;

procedure InitTLS13KeyUpdateInfo(out AInfo: TTLS13KeyUpdateInfo);
begin
  FillChar(AInfo, SizeOf(AInfo), 0);
end;

function TryParseTLS13NewSessionTicket(
  const AHandshakeMessage: TBytes;
  out ATicket: TTLS13NewSessionTicket;
  out AError: string
): Boolean;
var
  LBodyLen: Cardinal;
  LOffset: Integer;
  LNonceLen: Integer;
  LTicketLen: Integer;
  LExtLen: Integer;
begin
  InitTLS13NewSessionTicket(ATicket);
  AError := '';
  Result := False;

  if Length(AHandshakeMessage) < 4 then
  begin
    AError := 'Handshake message is too short';
    Exit;
  end;

  if AHandshakeMessage[0] <> TLS_HANDSHAKE_TYPE_NEW_SESSION_TICKET then
  begin
    AError := Format('Unexpected handshake type %d for NewSessionTicket parser', [AHandshakeMessage[0]]);
    Exit;
  end;

  LBodyLen := ReadUInt24(AHandshakeMessage, 1);
  if Length(AHandshakeMessage) <> 4 + Integer(LBodyLen) then
  begin
    AError := Format(
      'NewSessionTicket length mismatch (expected=%d actual=%d)',
      [4 + Integer(LBodyLen), Length(AHandshakeMessage)]
    );
    Exit;
  end;

  LOffset := 4;

  if LOffset + 8 > Length(AHandshakeMessage) then
  begin
    AError := 'NewSessionTicket missing lifetime/age_add';
    Exit;
  end;

  ATicket.TicketLifetime := ReadUInt32BE(AHandshakeMessage, LOffset);
  Inc(LOffset, 4);
  ATicket.TicketAgeAdd := ReadUInt32BE(AHandshakeMessage, LOffset);
  Inc(LOffset, 4);

  if LOffset + 1 > Length(AHandshakeMessage) then
  begin
    AError := 'NewSessionTicket missing ticket_nonce length';
    Exit;
  end;

  LNonceLen := AHandshakeMessage[LOffset];
  Inc(LOffset);

  if LOffset + LNonceLen > Length(AHandshakeMessage) then
  begin
    AError := 'NewSessionTicket ticket_nonce length exceeds message';
    Exit;
  end;

  SetLength(ATicket.TicketNonce, LNonceLen);
  if LNonceLen > 0 then
    Move(AHandshakeMessage[LOffset], ATicket.TicketNonce[0], LNonceLen);
  Inc(LOffset, LNonceLen);

  if LOffset + 2 > Length(AHandshakeMessage) then
  begin
    AError := 'NewSessionTicket missing ticket length';
    Exit;
  end;

  LTicketLen := ReadUInt16(AHandshakeMessage, LOffset);
  Inc(LOffset, 2);

  if LTicketLen <= 0 then
  begin
    AError := 'NewSessionTicket ticket length must be > 0';
    Exit;
  end;

  if LOffset + LTicketLen > Length(AHandshakeMessage) then
  begin
    AError := 'NewSessionTicket ticket length exceeds message';
    Exit;
  end;

  SetLength(ATicket.Ticket, LTicketLen);
  Move(AHandshakeMessage[LOffset], ATicket.Ticket[0], LTicketLen);
  Inc(LOffset, LTicketLen);

  if LOffset + 2 > Length(AHandshakeMessage) then
  begin
    AError := 'NewSessionTicket missing extension length';
    Exit;
  end;

  LExtLen := ReadUInt16(AHandshakeMessage, LOffset);
  Inc(LOffset, 2);

  if LOffset + LExtLen <> Length(AHandshakeMessage) then
  begin
    AError := 'NewSessionTicket extensions length mismatch';
    Exit;
  end;

  SetLength(ATicket.Extensions, LExtLen);
  if LExtLen > 0 then
    Move(AHandshakeMessage[LOffset], ATicket.Extensions[0], LExtLen);

  ATicket.Valid := True;
  Result := True;
end;

function TryParseTLS13KeyUpdate(
  const AHandshakeMessage: TBytes;
  out AInfo: TTLS13KeyUpdateInfo;
  out AError: string
): Boolean;
var
  LBodyLen: Cardinal;
  LRequest: Byte;
begin
  InitTLS13KeyUpdateInfo(AInfo);
  AError := '';
  Result := False;

  if Length(AHandshakeMessage) < 4 then
  begin
    AError := 'Handshake message is too short';
    Exit;
  end;

  if AHandshakeMessage[0] <> TLS_HANDSHAKE_TYPE_KEY_UPDATE then
  begin
    AError := Format('Unexpected handshake type %d for KeyUpdate parser', [AHandshakeMessage[0]]);
    Exit;
  end;

  LBodyLen := ReadUInt24(AHandshakeMessage, 1);
  if Length(AHandshakeMessage) <> 4 + Integer(LBodyLen) then
  begin
    AError := Format(
      'KeyUpdate length mismatch (expected=%d actual=%d)',
      [4 + Integer(LBodyLen), Length(AHandshakeMessage)]
    );
    Exit;
  end;

  if LBodyLen <> 1 then
  begin
    AError := Format('Invalid KeyUpdate body length %d', [Integer(LBodyLen)]);
    Exit;
  end;

  LRequest := AHandshakeMessage[4];
  case LRequest of
    0:
      AInfo.RequestUpdate := False;
    1:
      AInfo.RequestUpdate := True;
  else
    begin
      AError := Format('Invalid KeyUpdate request value %d', [LRequest]);
      Exit;
    end;
  end;

  AInfo.Valid := True;
  Result := True;
end;

end.
