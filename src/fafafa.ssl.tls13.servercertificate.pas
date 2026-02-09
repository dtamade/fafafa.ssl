{**
 * Unit: fafafa.ssl.tls13.servercertificate
 * Purpose: TLS 1.3 Certificate（server）消息构建器（纯 Pascal）
 *
 * 输入支持：
 * - 单个 DER 证书二进制
 * - PEM 文本（可含多张 CERTIFICATE）
 *}

unit fafafa.ssl.tls13.servercertificate;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils;

type
  TTLS13CertificateArray = array of TBytes;

function TryParseCertificateBlob(
  const ACertificateBlob: TBytes;
  out ACertificates: TTLS13CertificateArray;
  out AError: string
): Boolean;

function TryExtractLeafCertificateDERFromBlob(
  const ACertificateBlob: TBytes;
  out ALeafCertificateDER: TBytes;
  out AError: string
): Boolean;

function TryBuildTLS13ServerCertificateHandshake(
  const ACertificateBlob: TBytes;
  out AHandshake: TBytes;
  out AError: string
): Boolean;

implementation

uses
  fafafa.ssl.pem,
  fafafa.ssl.tls13.wire;

function BytesToAnsiString(const AData: TBytes): AnsiString;
begin
  SetLength(Result, Length(AData));
  if Length(AData) > 0 then
    Move(AData[0], Result[1], Length(AData));
end;

function BlobLooksLikePEM(const ACertificateBlob: TBytes): Boolean;
var
  LText: AnsiString;
begin
  LText := BytesToAnsiString(ACertificateBlob);
  Result := Pos('-----BEGIN', string(LText)) > 0;
end;

function TryParseCertificateBlob(
  const ACertificateBlob: TBytes;
  out ACertificates: TTLS13CertificateArray;
  out AError: string
): Boolean;
var
  LReader: TPEMReader;
  LBlocks: TPEMBlockArray;
  I: Integer;
  LText: AnsiString;
begin
  SetLength(ACertificates, 0);
  AError := '';
  Result := False;

  if Length(ACertificateBlob) = 0 then
  begin
    AError := 'Certificate blob is empty';
    Exit;
  end;

  if BlobLooksLikePEM(ACertificateBlob) then
  begin
    LReader := TPEMReader.Create;
    try
      LText := BytesToAnsiString(ACertificateBlob);
      try
        LReader.LoadFromString(string(LText));
      except
        on E: Exception do
        begin
          AError := 'Failed to parse PEM certificate blob: ' + E.Message;
          Exit;
        end;
      end;

      LBlocks := LReader.GetCertificates;
      if Length(LBlocks) = 0 then
      begin
        AError := 'No CERTIFICATE block found in PEM blob';
        Exit;
      end;

      SetLength(ACertificates, Length(LBlocks));
      for I := 0 to High(LBlocks) do
      begin
        if Length(LBlocks[I].Data) = 0 then
        begin
          AError := Format('PEM certificate block #%d is empty', [I + 1]);
          Exit;
        end;
        ACertificates[I] := Copy(LBlocks[I].Data, 0, Length(LBlocks[I].Data));
      end;
    finally
      LReader.Free;
    end;
  end
  else
  begin
    SetLength(ACertificates, 1);
    ACertificates[0] := Copy(ACertificateBlob, 0, Length(ACertificateBlob));
  end;

  Result := True;
end;

function TryExtractLeafCertificateDERFromBlob(
  const ACertificateBlob: TBytes;
  out ALeafCertificateDER: TBytes;
  out AError: string
): Boolean;
var
  LCerts: TTLS13CertificateArray;
begin
  SetLength(ALeafCertificateDER, 0);
  Result := False;

  if not TryParseCertificateBlob(ACertificateBlob, LCerts, AError) then
    Exit;

  if Length(LCerts) = 0 then
  begin
    AError := 'No certificate found in blob';
    Exit;
  end;

  ALeafCertificateDER := Copy(LCerts[0], 0, Length(LCerts[0]));
  Result := True;
end;

function TryBuildTLS13ServerCertificateHandshake(
  const ACertificateBlob: TBytes;
  out AHandshake: TBytes;
  out AError: string
): Boolean;
var
  LCertificates: TTLS13CertificateArray;
  LCertificateList: TBytes;
  LEntry: TBytes;
  LBody: TBytes;
  I: Integer;
  LCertLen: Integer;
begin
  SetLength(AHandshake, 0);
  AError := '';
  Result := False;

  if not TryParseCertificateBlob(ACertificateBlob, LCertificates, AError) then
    Exit;

  SetLength(LCertificateList, 0);
  for I := 0 to High(LCertificates) do
  begin
    LCertLen := Length(LCertificates[I]);
    if (LCertLen <= 0) or (LCertLen > $FFFFFF) then
    begin
      AError := Format('Certificate #%d length is invalid for TLS 1.3: %d', [I + 1, LCertLen]);
      Exit;
    end;

    SetLength(LEntry, 0);
    AppendUInt24(LEntry, LCertLen);
    AppendBytes(LEntry, LCertificates[I]);
    AppendUInt16(LEntry, 0); // extensions = empty

    AppendBytes(LCertificateList, LEntry);
  end;

  if Length(LCertificateList) > $FFFFFF then
  begin
    AError := 'Certificate list is too large for TLS 1.3';
    Exit;
  end;

  SetLength(LBody, 0);
  AppendByte(LBody, 0); // certificate_request_context length = 0
  AppendUInt24(LBody, Length(LCertificateList));
  AppendBytes(LBody, LCertificateList);

  SetLength(AHandshake, 0);
  AppendByte(AHandshake, TLS_HANDSHAKE_TYPE_CERTIFICATE);
  AppendUInt24(AHandshake, Length(LBody));
  AppendBytes(AHandshake, LBody);

  Result := True;
end;

end.
