program test_tls13_servercertverify;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  Classes,
  fafafa.ssl.tls13.bigint,
  fafafa.ssl.tls13.wire,
  fafafa.ssl.tls13.clienthello,
  fafafa.ssl.tls13.clienthello.parser,
  fafafa.ssl.pem,
  fafafa.ssl.tls13.servercertverify;

function LoadFileBytes(const AFileName: string): TBytes;
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, LStream.Size);
    if LStream.Size > 0 then
      LStream.ReadBuffer(Result[0], LStream.Size);
  finally
    LStream.Free;
  end;
end;

function LoadFileText(const AFileName: string): string;
var
  LLines: TStringList;
begin
  LLines := TStringList.Create;
  try
    LLines.LoadFromFile(AFileName);
    Result := LLines.Text;
  finally
    LLines.Free;
  end;
end;

function TryReadDERLength(const AData: TBytes; var AOffset: Integer; out ALength: Integer): Boolean;
var
  LFirst: Byte;
  LCount: Integer;
  I: Integer;
begin
  ALength := 0;
  Result := False;

  if (AOffset < 0) or (AOffset >= Length(AData)) then
    Exit;

  LFirst := AData[AOffset];
  Inc(AOffset);

  if (LFirst and $80) = 0 then
  begin
    ALength := LFirst;
    Exit(True);
  end;

  LCount := LFirst and $7F;
  if (LCount <= 0) or (LCount > 4) or (AOffset + LCount > Length(AData)) then
    Exit;

  ALength := 0;
  for I := 1 to LCount do
  begin
    ALength := (ALength shl 8) or AData[AOffset];
    Inc(AOffset);
  end;

  Result := True;
end;

function TryLocatePKCS1IntegerFieldValue(
  const ADER: TBytes;
  AFieldIndex: Integer;
  out AValueOffset: Integer;
  out AValueLength: Integer
): Boolean;
var
  LOffset: Integer;
  LSeqLength: Integer;
  LSeqEnd: Integer;
  LCurrentField: Integer;
begin
  AValueOffset := -1;
  AValueLength := 0;
  Result := False;

  if (AFieldIndex < 0) or (Length(ADER) < 4) then
    Exit;

  LOffset := 0;
  if ADER[LOffset] <> $30 then
    Exit;
  Inc(LOffset);

  if not TryReadDERLength(ADER, LOffset, LSeqLength) then
    Exit;

  LSeqEnd := LOffset + LSeqLength;
  if LSeqEnd > Length(ADER) then
    Exit;

  LCurrentField := 0;
  while LOffset < LSeqEnd do
  begin
    if ADER[LOffset] <> $02 then
      Exit;
    Inc(LOffset);

    if not TryReadDERLength(ADER, LOffset, AValueLength) then
      Exit;

    if (AValueLength < 0) or (LOffset + AValueLength > LSeqEnd) then
      Exit;

    if LCurrentField = AFieldIndex then
    begin
      AValueOffset := LOffset;
      Exit(True);
    end;

    Inc(LOffset, AValueLength);
    Inc(LCurrentField);
  end;
end;

function TryLocatePKCS1PrivateExponentValue(
  const ADER: TBytes;
  out AValueOffset: Integer;
  out AValueLength: Integer
): Boolean;
begin
  Result := TryLocatePKCS1IntegerFieldValue(ADER, 3, AValueOffset, AValueLength);
end;

function TryLocatePKCS1PrimePValue(
  const ADER: TBytes;
  out AValueOffset: Integer;
  out AValueLength: Integer
): Boolean;
begin
  Result := TryLocatePKCS1IntegerFieldValue(ADER, 4, AValueOffset, AValueLength);
end;

function TryLocatePKCS1PrimeQValue(
  const ADER: TBytes;
  out AValueOffset: Integer;
  out AValueLength: Integer
): Boolean;
begin
  Result := TryLocatePKCS1IntegerFieldValue(ADER, 5, AValueOffset, AValueLength);
end;

function TryLocatePKCS1DPValue(
  const ADER: TBytes;
  out AValueOffset: Integer;
  out AValueLength: Integer
): Boolean;
begin
  Result := TryLocatePKCS1IntegerFieldValue(ADER, 6, AValueOffset, AValueLength);
end;

function TryLocatePKCS1DQValue(
  const ADER: TBytes;
  out AValueOffset: Integer;
  out AValueLength: Integer
): Boolean;
begin
  Result := TryLocatePKCS1IntegerFieldValue(ADER, 7, AValueOffset, AValueLength);
end;

function TryLocatePKCS1QInvValue(
  const ADER: TBytes;
  out AValueOffset: Integer;
  out AValueLength: Integer
): Boolean;
begin
  Result := TryLocatePKCS1IntegerFieldValue(ADER, 8, AValueOffset, AValueLength);
end;

function TryMutatePKCS1FieldLSB(
  const ASourceDER: TBytes;
  AFieldIndex: Integer;
  AXorMask: Byte;
  AForceOdd: Boolean;
  out ADestDER: TBytes
): Boolean;
var
  LValueOffset: Integer;
  LValueLength: Integer;
begin
  SetLength(ADestDER, 0);
  Result := False;

  if not TryLocatePKCS1IntegerFieldValue(ASourceDER, AFieldIndex, LValueOffset, LValueLength) then
    Exit;
  if LValueLength <= 0 then
    Exit;

  ADestDER := Copy(ASourceDER, 0, Length(ASourceDER));
  ADestDER[LValueOffset + LValueLength - 1] := ADestDER[LValueOffset + LValueLength - 1] xor AXorMask;
  if AForceOdd then
    ADestDER[LValueOffset + LValueLength - 1] := ADestDER[LValueOffset + LValueLength - 1] or $01;

  Result := True;
end;

function TrySetPKCS1FieldToConstant(
  const ASourceDER: TBytes;
  AFieldIndex: Integer;
  AConstant: Byte;
  out ADestDER: TBytes
): Boolean;
var
  LValueOffset: Integer;
  LValueLength: Integer;
begin
  SetLength(ADestDER, 0);
  Result := False;

  if not TryLocatePKCS1IntegerFieldValue(ASourceDER, AFieldIndex, LValueOffset, LValueLength) then
    Exit;
  if LValueLength <= 0 then
    Exit;

  ADestDER := Copy(ASourceDER, 0, Length(ASourceDER));
  FillChar(ADestDER[LValueOffset], LValueLength, 0);
  ADestDER[LValueOffset + LValueLength - 1] := AConstant;

  Result := True;
end;

function TryCopyPKCS1FieldValue(
  const ASourceDER: TBytes;
  AFromFieldIndex: Integer;
  AToFieldIndex: Integer;
  out ADestDER: TBytes
): Boolean;
var
  LFromOffset: Integer;
  LFromLength: Integer;
  LToOffset: Integer;
  LToLength: Integer;
  LCopyLength: Integer;
begin
  SetLength(ADestDER, 0);
  Result := False;

  if not TryLocatePKCS1IntegerFieldValue(ASourceDER, AFromFieldIndex, LFromOffset, LFromLength) then
    Exit;
  if not TryLocatePKCS1IntegerFieldValue(ASourceDER, AToFieldIndex, LToOffset, LToLength) then
    Exit;
  if (LFromLength <= 0) or (LToLength <= 0) then
    Exit;

  ADestDER := Copy(ASourceDER, 0, Length(ASourceDER));
  FillChar(ADestDER[LToOffset], LToLength, 0);

  LCopyLength := LFromLength;
  if LCopyLength > LToLength then
    LCopyLength := LToLength;

  Move(
    ASourceDER[LFromOffset + LFromLength - LCopyLength],
    ADestDER[LToOffset + LToLength - LCopyLength],
    LCopyLength
  );

  Result := True;
end;

function TryMutatePKCS1PrivateExponent(const ASourceDER: TBytes; out ADestDER: TBytes): Boolean;
begin
  Result := TryMutatePKCS1FieldLSB(ASourceDER, 3, $01, False, ADestDER);
end;

function TryMutatePKCS1PrimeP(const ASourceDER: TBytes; out ADestDER: TBytes): Boolean;
begin
  Result := TryMutatePKCS1FieldLSB(ASourceDER, 4, $02, True, ADestDER);
end;

function TryMutatePKCS1PrimeQ(const ASourceDER: TBytes; out ADestDER: TBytes): Boolean;
begin
  Result := TryMutatePKCS1FieldLSB(ASourceDER, 5, $02, True, ADestDER);
end;

function TryMutatePKCS1DP(const ASourceDER: TBytes; out ADestDER: TBytes): Boolean;
begin
  Result := TryMutatePKCS1FieldLSB(ASourceDER, 6, $04, False, ADestDER);
end;

function TryMutatePKCS1DQ(const ASourceDER: TBytes; out ADestDER: TBytes): Boolean;
begin
  Result := TryMutatePKCS1FieldLSB(ASourceDER, 7, $04, False, ADestDER);
end;

function TryMutatePKCS1QInv(const ASourceDER: TBytes; out ADestDER: TBytes): Boolean;
begin
  Result := TryMutatePKCS1FieldLSB(ASourceDER, 8, $02, False, ADestDER);
end;

function TrySetPKCS1PrimePToOne(const ASourceDER: TBytes; out ADestDER: TBytes): Boolean;
begin
  Result := TrySetPKCS1FieldToConstant(ASourceDER, 4, 1, ADestDER);
end;

function TrySetPKCS1PrimeQEqualPrimeP(const ASourceDER: TBytes; out ADestDER: TBytes): Boolean;
begin
  Result := TryCopyPKCS1FieldValue(ASourceDER, 4, 5, ADestDER);
end;

function TrySetPKCS1DPToZero(const ASourceDER: TBytes; out ADestDER: TBytes): Boolean;
begin
  Result := TrySetPKCS1FieldToConstant(ASourceDER, 6, 0, ADestDER);
end;

function TrySetPKCS1DQToZero(const ASourceDER: TBytes; out ADestDER: TBytes): Boolean;
begin
  Result := TrySetPKCS1FieldToConstant(ASourceDER, 7, 0, ADestDER);
end;

function TrySetPKCS1QInvToZero(const ASourceDER: TBytes; out ADestDER: TBytes): Boolean;
begin
  Result := TrySetPKCS1FieldToConstant(ASourceDER, 8, 0, ADestDER);
end;

function TryLocatePKCS8PrivateKeyOctetStringValue(
  const ADER: TBytes;
  out AValueOffset: Integer;
  out AValueLength: Integer
): Boolean;
var
  LOffset: Integer;
  LSeqLength: Integer;
  LChildLength: Integer;
begin
  AValueOffset := -1;
  AValueLength := 0;
  Result := False;

  if Length(ADER) < 8 then
    Exit;

  LOffset := 0;
  if ADER[LOffset] <> $30 then
    Exit;
  Inc(LOffset);
  if not TryReadDERLength(ADER, LOffset, LSeqLength) then
    Exit;
  if LOffset + LSeqLength > Length(ADER) then
    Exit;

  if (LOffset >= Length(ADER)) or (ADER[LOffset] <> $02) then
    Exit;
  Inc(LOffset);
  if not TryReadDERLength(ADER, LOffset, LChildLength) then
    Exit;
  Inc(LOffset, LChildLength);

  if (LOffset >= Length(ADER)) or (ADER[LOffset] <> $30) then
    Exit;
  Inc(LOffset);
  if not TryReadDERLength(ADER, LOffset, LChildLength) then
    Exit;
  Inc(LOffset, LChildLength);

  if (LOffset >= Length(ADER)) or (ADER[LOffset] <> $04) then
    Exit;
  Inc(LOffset);
  if not TryReadDERLength(ADER, LOffset, AValueLength) then
    Exit;
  if LOffset + AValueLength > Length(ADER) then
    Exit;

  AValueOffset := LOffset;
  Result := True;
end;

function TryExtractFirstPrivateKeyDER(
  const APEMBlob: TBytes;
  out ADER: TBytes;
  out AType: TPEMType
): Boolean;
var
  LReader: TPEMReader;
  LBlocks: TPEMBlockArray;
  LText: string;
  I: Integer;
begin
  SetLength(ADER, 0);
  AType := pemUnknown;
  Result := False;

  LReader := TPEMReader.Create;
  try
    LText := TEncoding.ANSI.GetString(APEMBlob);
    LReader.LoadFromString(LText);
    LBlocks := LReader.GetPrivateKeys;
    for I := 0 to High(LBlocks) do
    begin
      if LBlocks[I].IsEncrypted then
        Continue;
      if not (LBlocks[I].BlockType in [pemPrivateKey, pemRSAPrivateKey]) then
        Continue;

      ADER := Copy(LBlocks[I].Data, 0, Length(LBlocks[I].Data));
      AType := LBlocks[I].BlockType;
      Exit(Length(ADER) > 0);
    end;
  finally
    LReader.Free;
  end;
end;

function BuildMutatedPrivateKeyBlob(const APEMBlob: TBytes): TBytes;
var
  LDER: TBytes;
  LMutatedPKCS1: TBytes;
  LInnerPKCS1: TBytes;
  LType: TPEMType;
  LOffset: Integer;
  LLength: Integer;
begin
  SetLength(Result, 0);

  if not TryExtractFirstPrivateKeyDER(APEMBlob, LDER, LType) then
    Exit;

  case LType of
    pemRSAPrivateKey:
      begin
        if TryMutatePKCS1PrivateExponent(LDER, Result) then
          Exit;
      end;

    pemPrivateKey:
      begin
        if not TryLocatePKCS8PrivateKeyOctetStringValue(LDER, LOffset, LLength) then
          Exit;

        LInnerPKCS1 := Copy(LDER, LOffset, LLength);
        if not TryMutatePKCS1PrivateExponent(LInnerPKCS1, LMutatedPKCS1) then
          Exit;

        if Length(LMutatedPKCS1) <> LLength then
          Exit;

        Result := Copy(LDER, 0, Length(LDER));
        Move(LMutatedPKCS1[0], Result[LOffset], LLength);
      end;
  end;
end;

function BuildMutatedPrimePPrivateKeyBlob(const APEMBlob: TBytes): TBytes;
var
  LDER: TBytes;
  LMutatedPKCS1: TBytes;
  LInnerPKCS1: TBytes;
  LType: TPEMType;
  LOffset: Integer;
  LLength: Integer;
begin
  SetLength(Result, 0);

  if not TryExtractFirstPrivateKeyDER(APEMBlob, LDER, LType) then
    Exit;

  case LType of
    pemRSAPrivateKey:
      begin
        if TryMutatePKCS1PrimeP(LDER, Result) then
          Exit;
      end;

    pemPrivateKey:
      begin
        if not TryLocatePKCS8PrivateKeyOctetStringValue(LDER, LOffset, LLength) then
          Exit;

        LInnerPKCS1 := Copy(LDER, LOffset, LLength);
        if not TryMutatePKCS1PrimeP(LInnerPKCS1, LMutatedPKCS1) then
          Exit;

        if Length(LMutatedPKCS1) <> LLength then
          Exit;

        Result := Copy(LDER, 0, Length(LDER));
        Move(LMutatedPKCS1[0], Result[LOffset], LLength);
      end;
  end;
end;

function BuildMutatedPrimePAndPrivateExponentPrivateKeyBlob(const APEMBlob: TBytes): TBytes;
var
  LDER: TBytes;
  LMutatedPKCS1: TBytes;
  LMutatedPKCS1B: TBytes;
  LInnerPKCS1: TBytes;
  LType: TPEMType;
  LOffset: Integer;
  LLength: Integer;
begin
  SetLength(Result, 0);

  if not TryExtractFirstPrivateKeyDER(APEMBlob, LDER, LType) then
    Exit;

  case LType of
    pemRSAPrivateKey:
      begin
        if not TryMutatePKCS1PrimeP(LDER, LMutatedPKCS1) then
          Exit;
        if not TryMutatePKCS1PrivateExponent(LMutatedPKCS1, LMutatedPKCS1B) then
          Exit;
        Result := LMutatedPKCS1B;
      end;

    pemPrivateKey:
      begin
        if not TryLocatePKCS8PrivateKeyOctetStringValue(LDER, LOffset, LLength) then
          Exit;

        LInnerPKCS1 := Copy(LDER, LOffset, LLength);
        if not TryMutatePKCS1PrimeP(LInnerPKCS1, LMutatedPKCS1) then
          Exit;
        if not TryMutatePKCS1PrivateExponent(LMutatedPKCS1, LMutatedPKCS1B) then
          Exit;

        if Length(LMutatedPKCS1B) <> LLength then
          Exit;

        Result := Copy(LDER, 0, Length(LDER));
        Move(LMutatedPKCS1B[0], Result[LOffset], LLength);
      end;
  end;
end;

function TryMutatePrivateExponentInAnyPKCS1DER(const ADER: TBytes; out AMutatedDER: TBytes): Boolean;
var
  LValueOffset: Integer;
  LValueLength: Integer;
begin
  SetLength(AMutatedDER, 0);
  Result := False;

  if not TryLocatePKCS1PrivateExponentValue(ADER, LValueOffset, LValueLength) then
    Exit;
  if LValueLength <= 0 then
    Exit;

  AMutatedDER := Copy(ADER, 0, Length(ADER));
  AMutatedDER[LValueOffset + LValueLength - 1] := AMutatedDER[LValueOffset + LValueLength - 1] xor $01;
  Result := True;
end;

function TryMutatePrivateExponentInDERKey(const ADERKeyBlob: TBytes; out AMutatedKeyDER: TBytes): Boolean;
var
  LPKCS1Offset: Integer;
  LPKCS1Length: Integer;
  LInnerPKCS1: TBytes;
  LMutatedPKCS1: TBytes;
begin
  SetLength(AMutatedKeyDER, 0);
  Result := False;

  if TryMutatePrivateExponentInAnyPKCS1DER(ADERKeyBlob, AMutatedKeyDER) then
    Exit(True);

  if not TryLocatePKCS8PrivateKeyOctetStringValue(ADERKeyBlob, LPKCS1Offset, LPKCS1Length) then
    Exit;

  LInnerPKCS1 := Copy(ADERKeyBlob, LPKCS1Offset, LPKCS1Length);
  if not TryMutatePrivateExponentInAnyPKCS1DER(LInnerPKCS1, LMutatedPKCS1) then
    Exit;
  if Length(LMutatedPKCS1) <> LPKCS1Length then
    Exit;

  AMutatedKeyDER := Copy(ADERKeyBlob, 0, Length(ADERKeyBlob));
  Move(LMutatedPKCS1[0], AMutatedKeyDER[LPKCS1Offset], LPKCS1Length);
  Result := True;
end;
function BuildMutatedPrimeQPrivateKeyBlob(const APEMBlob: TBytes): TBytes;
var
  LDER: TBytes;
  LMutatedPKCS1: TBytes;
  LInnerPKCS1: TBytes;
  LType: TPEMType;
  LOffset: Integer;
  LLength: Integer;
begin
  SetLength(Result, 0);

  if not TryExtractFirstPrivateKeyDER(APEMBlob, LDER, LType) then
    Exit;

  case LType of
    pemRSAPrivateKey:
      begin
        if TryMutatePKCS1PrimeQ(LDER, Result) then
          Exit;
      end;

    pemPrivateKey:
      begin
        if not TryLocatePKCS8PrivateKeyOctetStringValue(LDER, LOffset, LLength) then
          Exit;

        LInnerPKCS1 := Copy(LDER, LOffset, LLength);
        if not TryMutatePKCS1PrimeQ(LInnerPKCS1, LMutatedPKCS1) then
          Exit;

        if Length(LMutatedPKCS1) <> LLength then
          Exit;

        Result := Copy(LDER, 0, Length(LDER));
        Move(LMutatedPKCS1[0], Result[LOffset], LLength);
      end;
  end;
end;

function BuildMutatedDPPrivateKeyBlob(const APEMBlob: TBytes): TBytes;
var
  LDER: TBytes;
  LMutatedPKCS1: TBytes;
  LInnerPKCS1: TBytes;
  LType: TPEMType;
  LOffset: Integer;
  LLength: Integer;
begin
  SetLength(Result, 0);

  if not TryExtractFirstPrivateKeyDER(APEMBlob, LDER, LType) then
    Exit;

  case LType of
    pemRSAPrivateKey:
      begin
        if TryMutatePKCS1DP(LDER, Result) then
          Exit;
      end;

    pemPrivateKey:
      begin
        if not TryLocatePKCS8PrivateKeyOctetStringValue(LDER, LOffset, LLength) then
          Exit;

        LInnerPKCS1 := Copy(LDER, LOffset, LLength);
        if not TryMutatePKCS1DP(LInnerPKCS1, LMutatedPKCS1) then
          Exit;

        if Length(LMutatedPKCS1) <> LLength then
          Exit;

        Result := Copy(LDER, 0, Length(LDER));
        Move(LMutatedPKCS1[0], Result[LOffset], LLength);
      end;
  end;
end;

function BuildMutatedDQPrivateKeyBlob(const APEMBlob: TBytes): TBytes;
var
  LDER: TBytes;
  LMutatedPKCS1: TBytes;
  LInnerPKCS1: TBytes;
  LType: TPEMType;
  LOffset: Integer;
  LLength: Integer;
begin
  SetLength(Result, 0);

  if not TryExtractFirstPrivateKeyDER(APEMBlob, LDER, LType) then
    Exit;

  case LType of
    pemRSAPrivateKey:
      begin
        if TryMutatePKCS1DQ(LDER, Result) then
          Exit;
      end;

    pemPrivateKey:
      begin
        if not TryLocatePKCS8PrivateKeyOctetStringValue(LDER, LOffset, LLength) then
          Exit;

        LInnerPKCS1 := Copy(LDER, LOffset, LLength);
        if not TryMutatePKCS1DQ(LInnerPKCS1, LMutatedPKCS1) then
          Exit;

        if Length(LMutatedPKCS1) <> LLength then
          Exit;

        Result := Copy(LDER, 0, Length(LDER));
        Move(LMutatedPKCS1[0], Result[LOffset], LLength);
      end;
  end;
end;

function BuildMutatedQInvPrivateKeyBlob(const APEMBlob: TBytes): TBytes;
var
  LDER: TBytes;
  LMutatedPKCS1: TBytes;
  LInnerPKCS1: TBytes;
  LType: TPEMType;
  LOffset: Integer;
  LLength: Integer;
begin
  SetLength(Result, 0);

  if not TryExtractFirstPrivateKeyDER(APEMBlob, LDER, LType) then
    Exit;

  case LType of
    pemRSAPrivateKey:
      begin
        if TryMutatePKCS1QInv(LDER, Result) then
          Exit;
      end;

    pemPrivateKey:
      begin
        if not TryLocatePKCS8PrivateKeyOctetStringValue(LDER, LOffset, LLength) then
          Exit;

        LInnerPKCS1 := Copy(LDER, LOffset, LLength);
        if not TryMutatePKCS1QInv(LInnerPKCS1, LMutatedPKCS1) then
          Exit;

        if Length(LMutatedPKCS1) <> LLength then
          Exit;

        Result := Copy(LDER, 0, Length(LDER));
        Move(LMutatedPKCS1[0], Result[LOffset], LLength);
      end;
  end;
end;

function BuildPrimePIsOnePrivateKeyBlob(const APEMBlob: TBytes): TBytes;
var
  LDER: TBytes;
  LMutatedPKCS1: TBytes;
  LInnerPKCS1: TBytes;
  LType: TPEMType;
  LOffset: Integer;
  LLength: Integer;
begin
  SetLength(Result, 0);

  if not TryExtractFirstPrivateKeyDER(APEMBlob, LDER, LType) then
    Exit;

  case LType of
    pemRSAPrivateKey:
      begin
        if TrySetPKCS1PrimePToOne(LDER, Result) then
          Exit;
      end;

    pemPrivateKey:
      begin
        if not TryLocatePKCS8PrivateKeyOctetStringValue(LDER, LOffset, LLength) then
          Exit;

        LInnerPKCS1 := Copy(LDER, LOffset, LLength);
        if not TrySetPKCS1PrimePToOne(LInnerPKCS1, LMutatedPKCS1) then
          Exit;

        if Length(LMutatedPKCS1) <> LLength then
          Exit;

        Result := Copy(LDER, 0, Length(LDER));
        Move(LMutatedPKCS1[0], Result[LOffset], LLength);
      end;
  end;
end;

function BuildPrimeQEqualPrimePPrivateKeyBlob(const APEMBlob: TBytes): TBytes;
var
  LDER: TBytes;
  LMutatedPKCS1: TBytes;
  LInnerPKCS1: TBytes;
  LType: TPEMType;
  LOffset: Integer;
  LLength: Integer;
begin
  SetLength(Result, 0);

  if not TryExtractFirstPrivateKeyDER(APEMBlob, LDER, LType) then
    Exit;

  case LType of
    pemRSAPrivateKey:
      begin
        if TrySetPKCS1PrimeQEqualPrimeP(LDER, Result) then
          Exit;
      end;

    pemPrivateKey:
      begin
        if not TryLocatePKCS8PrivateKeyOctetStringValue(LDER, LOffset, LLength) then
          Exit;

        LInnerPKCS1 := Copy(LDER, LOffset, LLength);
        if not TrySetPKCS1PrimeQEqualPrimeP(LInnerPKCS1, LMutatedPKCS1) then
          Exit;

        if Length(LMutatedPKCS1) <> LLength then
          Exit;

        Result := Copy(LDER, 0, Length(LDER));
        Move(LMutatedPKCS1[0], Result[LOffset], LLength);
      end;
  end;
end;

function BuildDPZeroPrivateKeyBlob(const APEMBlob: TBytes): TBytes;
var
  LDER: TBytes;
  LMutatedPKCS1: TBytes;
  LInnerPKCS1: TBytes;
  LType: TPEMType;
  LOffset: Integer;
  LLength: Integer;
begin
  SetLength(Result, 0);

  if not TryExtractFirstPrivateKeyDER(APEMBlob, LDER, LType) then
    Exit;

  case LType of
    pemRSAPrivateKey:
      begin
        if TrySetPKCS1DPToZero(LDER, Result) then
          Exit;
      end;

    pemPrivateKey:
      begin
        if not TryLocatePKCS8PrivateKeyOctetStringValue(LDER, LOffset, LLength) then
          Exit;

        LInnerPKCS1 := Copy(LDER, LOffset, LLength);
        if not TrySetPKCS1DPToZero(LInnerPKCS1, LMutatedPKCS1) then
          Exit;

        if Length(LMutatedPKCS1) <> LLength then
          Exit;

        Result := Copy(LDER, 0, Length(LDER));
        Move(LMutatedPKCS1[0], Result[LOffset], LLength);
      end;
  end;
end;

function BuildDQZeroPrivateKeyBlob(const APEMBlob: TBytes): TBytes;
var
  LDER: TBytes;
  LMutatedPKCS1: TBytes;
  LInnerPKCS1: TBytes;
  LType: TPEMType;
  LOffset: Integer;
  LLength: Integer;
begin
  SetLength(Result, 0);

  if not TryExtractFirstPrivateKeyDER(APEMBlob, LDER, LType) then
    Exit;

  case LType of
    pemRSAPrivateKey:
      begin
        if TrySetPKCS1DQToZero(LDER, Result) then
          Exit;
      end;

    pemPrivateKey:
      begin
        if not TryLocatePKCS8PrivateKeyOctetStringValue(LDER, LOffset, LLength) then
          Exit;

        LInnerPKCS1 := Copy(LDER, LOffset, LLength);
        if not TrySetPKCS1DQToZero(LInnerPKCS1, LMutatedPKCS1) then
          Exit;

        if Length(LMutatedPKCS1) <> LLength then
          Exit;

        Result := Copy(LDER, 0, Length(LDER));
        Move(LMutatedPKCS1[0], Result[LOffset], LLength);
      end;
  end;
end;

function BuildQInvZeroPrivateKeyBlob(const APEMBlob: TBytes): TBytes;
var
  LDER: TBytes;
  LMutatedPKCS1: TBytes;
  LInnerPKCS1: TBytes;
  LType: TPEMType;
  LOffset: Integer;
  LLength: Integer;
begin
  SetLength(Result, 0);

  if not TryExtractFirstPrivateKeyDER(APEMBlob, LDER, LType) then
    Exit;

  case LType of
    pemRSAPrivateKey:
      begin
        if TrySetPKCS1QInvToZero(LDER, Result) then
          Exit;
      end;

    pemPrivateKey:
      begin
        if not TryLocatePKCS8PrivateKeyOctetStringValue(LDER, LOffset, LLength) then
          Exit;

        LInnerPKCS1 := Copy(LDER, LOffset, LLength);
        if not TrySetPKCS1QInvToZero(LInnerPKCS1, LMutatedPKCS1) then
          Exit;

        if Length(LMutatedPKCS1) <> LLength then
          Exit;

        Result := Copy(LDER, 0, Length(LDER));
        Move(LMutatedPKCS1[0], Result[LOffset], LLength);
      end;
  end;
end;

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

procedure AssertEqualsWord(AExpected, AActual: Word; const AMessage: string);
begin
  if AExpected <> AActual then
    Fail(Format('%s (expected=0x%.4x actual=0x%.4x)', [AMessage, AExpected, AActual]));
end;

procedure AssertEqualsInt(AExpected, AActual: Integer; const AMessage: string);
begin
  if AExpected <> AActual then
    Fail(Format('%s (expected=%d actual=%d)', [AMessage, AExpected, AActual]));
end;

procedure AssertContains(const AText, ASubText, AMessage: string);
begin
  if Pos(ASubText, AText) <= 0 then
    Fail(AMessage + ' (missing: ' + ASubText + ')');
end;

procedure AssertEqualsQWord(AExpected, AActual: QWord; const AMessage: string);
begin
  if AExpected <> AActual then
    Fail(Format('%s (expected=%d actual=%d)', [AMessage, AExpected, AActual]));
end;

function BytesToQWord(const AData: TBytes): QWord;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Length(AData) - 1 do
    Result := (Result shl 8) or QWord(AData[I]);
end;

function QWordToBytes(AValue: QWord): TBytes;
var
  LValue: QWord;
  LCount: Integer;
  I: Integer;
begin
  if AValue = 0 then
  begin
    SetLength(Result, 1);
    Result[0] := 0;
    Exit;
  end;

  LValue := AValue;
  LCount := 0;
  while LValue > 0 do
  begin
    Inc(LCount);
    LValue := LValue shr 8;
  end;

  SetLength(Result, LCount);
  LValue := AValue;
  for I := LCount - 1 downto 0 do
  begin
    Result[I] := Byte(LValue and $FF);
    LValue := LValue shr 8;
  end;
end;

function PowModQWord(ABase, AExponent, AModulus: QWord): QWord;
var
  LBase: QWord;
  LResult: QWord;
  LExponent: QWord;
begin
  if AModulus = 0 then
    Exit(0);

  LBase := ABase mod AModulus;
  LExponent := AExponent;
  LResult := 1 mod AModulus;

  while LExponent > 0 do
  begin
    if (LExponent and 1) = 1 then
      LResult := (LResult * LBase) mod AModulus;
    LBase := (LBase * LBase) mod AModulus;
    LExponent := LExponent shr 1;
  end;

  Result := LResult;
end;

procedure AssertBigIntModMatchesQWord(AValue, AModulus: QWord; const ALabel: string);
var
  LOut: TBytes;
  LErr: string;
  LExpected: QWord;
begin
  AssertTrue(
    TryBigIntModFromUnsignedBytes(QWordToBytes(AValue), QWordToBytes(AModulus), LOut, LErr),
    ALabel + ': mod operation failed: ' + LErr
  );
  LExpected := AValue mod AModulus;
  AssertEqualsQWord(LExpected, BytesToQWord(LOut), ALabel + ': mod result mismatch');
end;

procedure AssertBigIntModMulMatchesQWord(ALeft, ARight, AModulus: QWord; const ALabel: string);
var
  LOut: TBytes;
  LErr: string;
  LExpected: QWord;
begin
  AssertTrue(
    TryBigIntModMulFromUnsignedBytes(QWordToBytes(ALeft), QWordToBytes(ARight), QWordToBytes(AModulus), LOut, LErr),
    ALabel + ': modmul operation failed: ' + LErr
  );
  LExpected := ((ALeft mod AModulus) * (ARight mod AModulus)) mod AModulus;
  AssertEqualsQWord(LExpected, BytesToQWord(LOut), ALabel + ': modmul result mismatch');
end;

procedure AssertBigIntModExpMatchesQWord(ABase, AExponent, AModulus: QWord; const ALabel: string);
var
  LOut: TBytes;
  LErr: string;
  LExpected: QWord;
begin
  AssertTrue(
    TryBigIntModExpFromUnsignedBytes(QWordToBytes(ABase), QWordToBytes(AExponent), QWordToBytes(AModulus), LOut, LErr),
    ALabel + ': modexp operation failed: ' + LErr
  );
  LExpected := PowModQWord(ABase, AExponent, AModulus);
  AssertEqualsQWord(LExpected, BytesToQWord(LOut), ALabel + ': modexp result mismatch');
end;

procedure AssertBigIntSubModMatchesQWord(ALeft, ARight, AModulus: QWord; const ALabel: string);
var
  LOut: TBytes;
  LErr: string;
  LLeftReduced: QWord;
  LRightReduced: QWord;
  LExpected: QWord;
begin
  AssertTrue(
    TryBigIntSubtractModuloFromUnsignedBytes(QWordToBytes(ALeft), QWordToBytes(ARight), QWordToBytes(AModulus), LOut, LErr),
    ALabel + ': subtract-mod operation failed: ' + LErr
  );

  LLeftReduced := ALeft mod AModulus;
  LRightReduced := ARight mod AModulus;
  if LLeftReduced >= LRightReduced then
    LExpected := LLeftReduced - LRightReduced
  else
    LExpected := AModulus - (LRightReduced - LLeftReduced);

  AssertEqualsQWord(LExpected, BytesToQWord(LOut), ALabel + ': subtract-mod result mismatch');
end;

function BuildPKCS8WithContextAttribute(const ADER: TBytes): TBytes;
const
  ATTRS_TAIL: array[0..21] of Byte = (
    $A0, $14,
      $31, $12,
        $30, $10,
          $06, $09, $2A, $86, $48, $86, $F7, $0D, $01, $09, $14,
          $31, $03,
            $0C, $01, $41
  );
var
  LOffset: Integer;
  LSeqLen: Integer;
  LSeqLenBytes: Integer;
  LNewSeqLen: Integer;
  LNewLenBytes: Integer;
  LTmp: Integer;
  I: Integer;
  LPayloadOffset: Integer;
begin
  SetLength(Result, 0);
  if Length(ADER) < 4 then
    Exit;
  if ADER[0] <> $30 then
    Exit;

  LOffset := 1;
  if not TryReadDERLength(ADER, LOffset, LSeqLen) then
    Exit;

  LSeqLenBytes := LOffset - 1;
  LNewSeqLen := LSeqLen + Length(ATTRS_TAIL);

  if LNewSeqLen < $80 then
    LNewLenBytes := 1
  else if LNewSeqLen <= $FF then
    LNewLenBytes := 2
  else if LNewSeqLen <= $FFFF then
    LNewLenBytes := 3
  else
    Exit;

  SetLength(Result, 1 + LNewLenBytes + LSeqLen + Length(ATTRS_TAIL));
  Result[0] := $30;

  if LNewLenBytes = 1 then
    Result[1] := Byte(LNewSeqLen)
  else
  begin
    Result[1] := $80 or Byte(LNewLenBytes - 1);
    LTmp := LNewSeqLen;
    for I := LNewLenBytes - 1 downto 1 do
    begin
      Result[1 + I] := Byte(LTmp and $FF);
      LTmp := LTmp shr 8;
    end;
  end;

  LPayloadOffset := 1 + LNewLenBytes;
  Move(ADER[1 + LSeqLenBytes], Result[LPayloadOffset], LSeqLen);
  Move(ATTRS_TAIL[0], Result[LPayloadOffset + LSeqLen], Length(ATTRS_TAIL));
end;

function BuildPEMPrivateKeyWithLeadingJunk(const APEMBlob: TBytes): TBytes;
const
  JUNK_PREFIX = 'random-header:ignore-me'#13#10'still-junk'#13#10;
var
  LJunkBytes: TBytes;
begin
  LJunkBytes := TEncoding.ASCII.GetBytes(JUNK_PREFIX);
  SetLength(Result, Length(LJunkBytes) + Length(APEMBlob));
  if Length(LJunkBytes) > 0 then
    Move(LJunkBytes[0], Result[0], Length(LJunkBytes));
  if Length(APEMBlob) > 0 then
    Move(APEMBlob[0], Result[Length(LJunkBytes)], Length(APEMBlob));
end;

function BuildPEMWithMultiplePrivateKeys(const APEMBlobA, APEMBlobB: TBytes): TBytes;
const
  SEP = LineEnding + LineEnding;
var
  LSepBytes: TBytes;
  LOffset: Integer;
begin
  LSepBytes := TEncoding.ASCII.GetBytes(SEP);
  SetLength(Result, Length(APEMBlobA) + Length(LSepBytes) + Length(APEMBlobB));
  LOffset := 0;
  if Length(APEMBlobA) > 0 then
  begin
    Move(APEMBlobA[0], Result[LOffset], Length(APEMBlobA));
    Inc(LOffset, Length(APEMBlobA));
  end;
  if Length(LSepBytes) > 0 then
  begin
    Move(LSepBytes[0], Result[LOffset], Length(LSepBytes));
    Inc(LOffset, Length(LSepBytes));
  end;
  if Length(APEMBlobB) > 0 then
    Move(APEMBlobB[0], Result[LOffset], Length(APEMBlobB));
end;

procedure TestSelectSchemeFromClientHello;
var
  LClientKeyShare: TBytes;
  LHandshake: TBytes;
  LInfo: TTLS13ClientHelloInfo;
  LErr: string;
  LScheme: Word;
begin
  SetLength(LClientKeyShare, 32);
  FillChar(LClientKeyShare[0], 32, $33);

  LHandshake := BuildTLS13ClientHelloHandshake('localhost', '', LClientKeyShare);
  AssertTrue(TryParseTLS13ClientHelloFromHandshake(LHandshake, LInfo, LErr),
    'Parse ClientHello failed: ' + LErr);

  AssertTrue(TrySelectTLS13ServerCertificateVerifyScheme(LInfo, LScheme, LErr),
    'Scheme selection failed: ' + LErr);
  AssertEqualsWord(TLS13_SIG_RSA_PSS_RSAE_SHA256, LScheme,
    'Scheme selector should prioritize rsa_pss_rsae_sha256');
end;

procedure TestBuildCertVerifyInput;
const
  CONTEXT = 'TLS 1.3, server CertificateVerify';
var
  LHash: TBytes;
  LInput: TBytes;
  I: Integer;
  LOffset: Integer;
begin
  SetLength(LHash, 32);
  for I := 0 to 31 do
    LHash[I] := Byte(I);

  LInput := BuildTLS13ServerCertificateVerifyInputSHA256(LHash);
  AssertEqualsInt(64 + Length(CONTEXT) + 1 + 32, Length(LInput), 'CertificateVerify input length mismatch');

  for I := 0 to 63 do
    AssertTrue(LInput[I] = $20, 'CertificateVerify input must start with 64 spaces');

  LOffset := 64;
  for I := 1 to Length(CONTEXT) do
    AssertTrue(LInput[LOffset + I - 1] = Byte(Ord(CONTEXT[I])), 'Context string byte mismatch');

  AssertTrue(LInput[64 + Length(CONTEXT)] = 0, 'Context separator must be 0x00');
  for I := 0 to 31 do
    AssertTrue(LInput[64 + Length(CONTEXT) + 1 + I] = LHash[I], 'Transcript hash bytes mismatch');
end;

procedure TestBuildCertificateVerifyHandshake;
var
  LSignature: TBytes;
  LHandshake: TBytes;
  LLen: Cardinal;
begin
  SetLength(LSignature, 16);
  FillChar(LSignature[0], 16, $AB);

  LHandshake := BuildTLS13CertificateVerifyHandshake(TLS13_SIG_RSA_PSS_RSAE_SHA256, LSignature);

  AssertTrue(Length(LHandshake) = 4 + 2 + 2 + 16, 'CertificateVerify handshake total length mismatch');
  AssertTrue(LHandshake[0] = TLS_HANDSHAKE_TYPE_CERTIFICATE_VERIFY, 'Handshake type mismatch');

  LLen := ReadUInt24(LHandshake, 1);
  AssertEqualsInt(2 + 2 + 16, Integer(LLen), 'CertificateVerify body length mismatch');
  AssertEqualsWord(TLS13_SIG_RSA_PSS_RSAE_SHA256, ReadUInt16(LHandshake, 4), 'Signature scheme mismatch');
  AssertEqualsWord(16, ReadUInt16(LHandshake, 6), 'Signature length field mismatch');
end;

procedure TestPlaceholderSignature;
var
  LHash: TBytes;
  LSigA, LSigB: TBytes;
  I: Integer;
  LDiff: Integer;
begin
  SetLength(LHash, 32);
  for I := 0 to 31 do
    LHash[I] := Byte($90 + I);

  LSigA := BuildTLS13PlaceholderSignatureFromTranscriptHash(LHash, 64);
  LSigB := BuildTLS13PlaceholderSignatureFromTranscriptHash(LHash, 64);

  AssertEqualsInt(64, Length(LSigA), 'Placeholder signature length mismatch');
  AssertEqualsInt(64, Length(LSigB), 'Placeholder signature length mismatch');

  LDiff := 0;
  for I := 0 to 63 do
  begin
    if LSigA[I] <> LSigB[I] then
      Inc(LDiff);
  end;
  AssertEqualsInt(0, LDiff, 'Placeholder signature should be deterministic for same input');
end;

procedure TestRealRSASignature;
var
  LKeyBlob: TBytes;
  LTranscriptHash: TBytes;
  LInput: TBytes;
  LSigPSSA, LSigPSSB: TBytes;
  LSigPKCS1A, LSigPKCS1B: TBytes;
  LErr: string;
  I, LDiff: Integer;
begin
  LKeyBlob := LoadFileBytes('tests/certificate/test_certs/signer_key.pem');
  AssertTrue(Length(LKeyBlob) > 0, 'Signer key blob should not be empty');

  SetLength(LTranscriptHash, 32);
  for I := 0 to 31 do
    LTranscriptHash[I] := Byte($21 + I);

  LInput := BuildTLS13ServerCertificateVerifyInputSHA256(LTranscriptHash);

  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PSS_RSAE_SHA256,
      LKeyBlob,
      LInput,
      LSigPSSA,
      LErr
    ),
    'RSA-PSS signing failed: ' + LErr
  );
  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PSS_RSAE_SHA256,
      LKeyBlob,
      LInput,
      LSigPSSB,
      LErr
    ),
    'RSA-PSS signing failed on second call: ' + LErr
  );
  AssertEqualsInt(256, Length(LSigPSSA), 'RSA-PSS signature length should match 2048-bit key');
  AssertEqualsInt(256, Length(LSigPSSB), 'RSA-PSS signature length should match 2048-bit key');

  LDiff := 0;
  for I := 0 to Length(LSigPSSA) - 1 do
    if LSigPSSA[I] <> LSigPSSB[I] then
      Inc(LDiff);
  AssertTrue(LDiff > 0, 'RSA-PSS signatures should vary because of randomized salt');

  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      LKeyBlob,
      LInput,
      LSigPKCS1A,
      LErr
    ),
    'RSA-PKCS1 signing failed: ' + LErr
  );
  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      LKeyBlob,
      LInput,
      LSigPKCS1B,
      LErr
    ),
    'RSA-PKCS1 signing failed on second call: ' + LErr
  );
  AssertEqualsInt(256, Length(LSigPKCS1A), 'RSA-PKCS1 signature length should match 2048-bit key');
  AssertEqualsInt(256, Length(LSigPKCS1B), 'RSA-PKCS1 signature length should match 2048-bit key');

  LDiff := 0;
  for I := 0 to Length(LSigPKCS1A) - 1 do
    if LSigPKCS1A[I] <> LSigPKCS1B[I] then
      Inc(LDiff);
  AssertEqualsInt(0, LDiff, 'RSA-PKCS1 signatures should be deterministic for same input/key');
end;

procedure TestSignerUnitHasNoExternalBigIntDependency;
var
  LSource: string;
begin
  LSource := LowerCase(LoadFileText('src/fafafa.ssl.tls13.servercertverify.pas'));

  AssertTrue(
    Pos('fafafa.ssl.openssl', LSource) = 0,
    'TLS13 server CertificateVerify signer must not depend on OpenSSL units'
  );
  AssertTrue(
    Pos('gmp', LSource) = 0,
    'TLS13 server CertificateVerify signer must not depend on GMP'
  );
end;

procedure TestRSASignatureFallsBackWhenPrivateExponentCorrupted;
var
  LKeyBlob: TBytes;
  LMutatedKeyDER: TBytes;
  LTranscriptHash: TBytes;
  LInput: TBytes;
  LSigOriginal: TBytes;
  LSigMutated: TBytes;
  LErr: string;
  I: Integer;
  LDiff: Integer;
begin
  LKeyBlob := LoadFileBytes('tests/certificate/test_certs/signer_key.pem');
  AssertTrue(Length(LKeyBlob) > 0, 'Signer key blob should not be empty');

  LMutatedKeyDER := BuildMutatedPrivateKeyBlob(LKeyBlob);
  AssertTrue(Length(LMutatedKeyDER) > 0, 'Failed to produce DER key with corrupted privateExponent');

  SetLength(LTranscriptHash, 32);
  for I := 0 to 31 do
    LTranscriptHash[I] := Byte($A0 + I);

  LInput := BuildTLS13ServerCertificateVerifyInputSHA256(LTranscriptHash);

  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      LKeyBlob,
      LInput,
      LSigOriginal,
      LErr
    ),
    'RSA-PKCS1 signing with original key failed: ' + LErr
  );

  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      LMutatedKeyDER,
      LInput,
      LSigMutated,
      LErr
    ),
    'RSA-PKCS1 signing with mutated key failed: ' + LErr
  );

  AssertEqualsInt(Length(LSigOriginal), Length(LSigMutated), 'Signature length mismatch');

  LDiff := 0;
  for I := 0 to Length(LSigOriginal) - 1 do
    if LSigOriginal[I] <> LSigMutated[I] then
      Inc(LDiff);

  AssertTrue(LDiff > 0,
    'Corrupted privateExponent should force exponent fallback and change RSA-PKCS1 signature');
end;

procedure TestRSASignatureFallsBackWhenCRTInconsistent;
var
  LKeyBlob: TBytes;
  LMutatedPKeyDER: TBytes;
  LTranscriptHash: TBytes;
  LInput: TBytes;
  LSigOriginal: TBytes;
  LSigMutated: TBytes;
  LErr: string;
  I: Integer;
  LDiff: Integer;
begin
  LKeyBlob := LoadFileBytes('tests/certificate/test_certs/signer_key.pem');
  AssertTrue(Length(LKeyBlob) > 0, 'Signer key blob should not be empty');

  LMutatedPKeyDER := BuildMutatedPrimePPrivateKeyBlob(LKeyBlob);
  AssertTrue(Length(LMutatedPKeyDER) > 0, 'Failed to produce DER key with corrupted prime p');

  SetLength(LTranscriptHash, 32);
  for I := 0 to 31 do
    LTranscriptHash[I] := Byte($B0 + I);

  LInput := BuildTLS13ServerCertificateVerifyInputSHA256(LTranscriptHash);

  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      LKeyBlob,
      LInput,
      LSigOriginal,
      LErr
    ),
    'RSA-PKCS1 signing with original key failed: ' + LErr
  );

  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      LMutatedPKeyDER,
      LInput,
      LSigMutated,
      LErr
    ),
    'RSA-PKCS1 signing with CRT-inconsistent key should fallback and still succeed: ' + LErr
  );

  AssertEqualsInt(Length(LSigOriginal), Length(LSigMutated), 'Signature length mismatch');

  LDiff := 0;
  for I := 0 to Length(LSigOriginal) - 1 do
    if LSigOriginal[I] <> LSigMutated[I] then
      Inc(LDiff);

  AssertEqualsInt(0, LDiff,
    'Corrupted prime p should not change signature when signer falls back to private exponent path');
end;

procedure TestRSASignatureUsesCorruptedExponentWhenCRTBroken;
var
  LKeyBlob: TBytes;
  LMutatedBothDER: TBytes;
  LTranscriptHash: TBytes;
  LInput: TBytes;
  LSigOriginal: TBytes;
  LSig: TBytes;
  LErr: string;
  I: Integer;
  LDiff: Integer;
begin
  LKeyBlob := LoadFileBytes('tests/certificate/test_certs/signer_key.pem');
  AssertTrue(Length(LKeyBlob) > 0, 'Signer key blob should not be empty');

  LMutatedBothDER := BuildMutatedPrimePAndPrivateExponentPrivateKeyBlob(LKeyBlob);
  AssertTrue(Length(LMutatedBothDER) > 0, 'Failed to produce DER key with corrupted prime p + private exponent');

  SetLength(LTranscriptHash, 32);
  for I := 0 to 31 do
    LTranscriptHash[I] := Byte($C0 + I);

  LInput := BuildTLS13ServerCertificateVerifyInputSHA256(LTranscriptHash);

  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      LKeyBlob,
      LInput,
      LSigOriginal,
      LErr
    ),
    'RSA-PKCS1 signing with original key failed: ' + LErr
  );

  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      LMutatedBothDER,
      LInput,
      LSig,
      LErr
    ),
    'Signing should still succeed via fallback exponent path: ' + LErr
  );

  AssertEqualsInt(Length(LSigOriginal), Length(LSig), 'Signature length mismatch');

  LDiff := 0;
  for I := 0 to Length(LSigOriginal) - 1 do
    if LSigOriginal[I] <> LSig[I] then
      Inc(LDiff);

  AssertTrue(LDiff > 0,
    'Corrupted private exponent should produce a different signature when CRT is broken and fallback is used');
end;

procedure AssertFallbackSignatureMatchesValid(
  const AMutatedKeyDER: TBytes;
  const ATestLabel: string
);
var
  LKeyBlob: TBytes;
  LTranscriptHash: TBytes;
  LInput: TBytes;
  LValidSig: TBytes;
  LMutatedSig: TBytes;
  LErr: string;
  LDiff: Integer;
  I: Integer;
begin
  LKeyBlob := LoadFileBytes('tests/certificate/test_certs/signer_key.pem');
  AssertTrue(Length(AMutatedKeyDER) > 0, ATestLabel + ': failed to build mutated key DER');

  SetLength(LTranscriptHash, 32);
  for I := 0 to 31 do
    LTranscriptHash[I] := Byte($4D + I);
  LInput := BuildTLS13ServerCertificateVerifyInputSHA256(LTranscriptHash);

  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      LKeyBlob,
      LInput,
      LValidSig,
      LErr
    ),
    ATestLabel + ': baseline signing failed: ' + LErr
  );

  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      AMutatedKeyDER,
      LInput,
      LMutatedSig,
      LErr
    ),
    ATestLabel + ': mutated signing should fallback and succeed: ' + LErr
  );

  AssertEqualsInt(Length(LValidSig), Length(LMutatedSig), ATestLabel + ': signature length mismatch');
  LDiff := 0;
  for I := 0 to Length(LValidSig) - 1 do
    if LValidSig[I] <> LMutatedSig[I] then
      Inc(LDiff);
  AssertEqualsInt(0, LDiff, ATestLabel + ': fallback signature should match valid signature');
end;

procedure AssertFallbackErrorContainsCRTReason(
  const AMutatedKeyDER: TBytes;
  const AExpectedReason: string;
  const ATestLabel: string
);
var
  LTranscriptHash: TBytes;
  LInput: TBytes;
  LSig: TBytes;
  LErr: string;
  I: Integer;
begin
  AssertTrue(Length(AMutatedKeyDER) > 0, ATestLabel + ': failed to build mutated key DER');

  SetLength(LTranscriptHash, 32);
  for I := 0 to 31 do
    LTranscriptHash[I] := Byte($53 + I);
  LInput := BuildTLS13ServerCertificateVerifyInputSHA256(LTranscriptHash);

  AssertTrue(
    not TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      AMutatedKeyDER,
      LInput,
      LSig,
      LErr
    ),
    ATestLabel + ': expected fallback failure with structured diagnostic'
  );
  AssertContains(LErr, 'E_TLS13_SIGNER_FALLBACK_FAILED', ATestLabel + ': missing fallback error code');
  AssertContains(LErr, 'crt_reason=' + AExpectedReason, ATestLabel + ': missing CRT reason detail');
  AssertContains(LErr, 'exp_reason=', ATestLabel + ': missing exponent reason detail');
end;

procedure AssertFallbackRetainsCRTReasonInSuccessPath(
  const AMutatedKeyDER: TBytes;
  const AExpectedReason: string;
  const ATestLabel: string
);
var
  LTranscriptHash: TBytes;
  LInput: TBytes;
  LSig: TBytes;
  LErr: string;
  I: Integer;
begin
  AssertTrue(Length(AMutatedKeyDER) > 0, ATestLabel + ': failed to build mutated key DER');

  SetLength(LTranscriptHash, 32);
  for I := 0 to 31 do
    LTranscriptHash[I] := Byte($58 + I);
  LInput := BuildTLS13ServerCertificateVerifyInputSHA256(LTranscriptHash);

  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      AMutatedKeyDER,
      LInput,
      LSig,
      LErr
    ),
    ATestLabel + ': mutated signing should fallback and succeed: ' + LErr
  );
  AssertTrue(Length(LSig) > 0, ATestLabel + ': fallback-success signature should not be empty');
end;

procedure TestRSASignatureFallsBackWhenPrimeQInconsistent;
var
  LKeyBlob: TBytes;
  LMutated: TBytes;
begin
  LKeyBlob := LoadFileBytes('tests/certificate/test_certs/signer_key.pem');
  LMutated := BuildMutatedPrimeQPrivateKeyBlob(LKeyBlob);
  AssertFallbackSignatureMatchesValid(LMutated, 'prime-q-inconsistent');
end;

procedure TestRSASignatureFallsBackWhenDPInconsistent;
var
  LKeyBlob: TBytes;
  LMutated: TBytes;
begin
  LKeyBlob := LoadFileBytes('tests/certificate/test_certs/signer_key.pem');
  LMutated := BuildMutatedDPPrivateKeyBlob(LKeyBlob);
  AssertFallbackSignatureMatchesValid(LMutated, 'dp-inconsistent');
end;

procedure TestRSASignatureFallsBackWhenDQInconsistent;
var
  LKeyBlob: TBytes;
  LMutated: TBytes;
begin
  LKeyBlob := LoadFileBytes('tests/certificate/test_certs/signer_key.pem');
  LMutated := BuildMutatedDQPrivateKeyBlob(LKeyBlob);
  AssertFallbackSignatureMatchesValid(LMutated, 'dq-inconsistent');
end;

procedure TestRSASignatureFallsBackWhenQInvInconsistent;
var
  LKeyBlob: TBytes;
  LMutated: TBytes;
begin
  LKeyBlob := LoadFileBytes('tests/certificate/test_certs/signer_key.pem');
  LMutated := BuildMutatedQInvPrivateKeyBlob(LKeyBlob);
  AssertFallbackSignatureMatchesValid(LMutated, 'qinv-inconsistent');
end;

procedure TestRSASignatureFallbackErrorWhenPrimePIsOneAndExponentCorrupted;
var
  LKeyBlob: TBytes;
  LMutated: TBytes;
begin
  LKeyBlob := LoadFileBytes('tests/certificate/test_certs/signer_key.pem');
  LMutated := BuildPrimePIsOnePrivateKeyBlob(LKeyBlob);
  AssertFallbackRetainsCRTReasonInSuccessPath(LMutated, 'RSA CRT validation failed: p/q must be > 1', 'prime-p-is-one');
end;

procedure TestRSASignatureFallbackErrorWhenPrimeQEqualsPrimePAndExponentCorrupted;
var
  LKeyBlob: TBytes;
  LMutated: TBytes;
begin
  LKeyBlob := LoadFileBytes('tests/certificate/test_certs/signer_key.pem');
  LMutated := BuildPrimeQEqualPrimePPrivateKeyBlob(LKeyBlob);
  AssertFallbackRetainsCRTReasonInSuccessPath(LMutated, 'RSA CRT validation failed: p and q must be distinct', 'p-equals-q');
end;

procedure TestRSASignatureFallbackErrorWhenDPZeroAndExponentCorrupted;
var
  LKeyBlob: TBytes;
  LMutated: TBytes;
begin
  LKeyBlob := LoadFileBytes('tests/certificate/test_certs/signer_key.pem');
  LMutated := BuildDPZeroPrivateKeyBlob(LKeyBlob);
  AssertFallbackRetainsCRTReasonInSuccessPath(LMutated, 'RSA CRT validation failed: dp/dq must be non-zero', 'dp-zero');
end;

procedure TestRSASignatureFallbackErrorWhenDQZeroAndExponentCorrupted;
var
  LKeyBlob: TBytes;
  LMutated: TBytes;
begin
  LKeyBlob := LoadFileBytes('tests/certificate/test_certs/signer_key.pem');
  LMutated := BuildDQZeroPrivateKeyBlob(LKeyBlob);
  AssertFallbackRetainsCRTReasonInSuccessPath(LMutated, 'RSA CRT validation failed: dp/dq must be non-zero', 'dq-zero');
end;

procedure TestRSASignatureFallbackErrorWhenQInvZeroAndExponentCorrupted;
var
  LKeyBlob: TBytes;
  LMutated: TBytes;
begin
  LKeyBlob := LoadFileBytes('tests/certificate/test_certs/signer_key.pem');
  LMutated := BuildQInvZeroPrivateKeyBlob(LKeyBlob);
  AssertFallbackRetainsCRTReasonInSuccessPath(LMutated, 'RSA CRT validation failed: qInv is inconsistent with q mod p', 'qinv-zero');
end;

procedure TestBigIntEvenModulusAndZeroExponent;
var
  LOut: TBytes;
  LErr: string;
begin
  AssertTrue(TryBigIntModExpFromUnsignedBytes([$0D], [$03], [$0A], LOut, LErr),
    'BigInt even modulus modexp fallback failed: ' + LErr);
  AssertEqualsInt(1, Length(LOut), 'BigInt even modulus modexp length mismatch');
  AssertEqualsInt(7, LOut[0], '13^3 mod 10 should be 7');

  AssertTrue(TryBigIntModMulFromUnsignedBytes([$07], [$09], [$0A], LOut, LErr),
    'BigInt even modulus modmul fallback failed: ' + LErr);
  AssertEqualsInt(1, Length(LOut), 'BigInt even modulus modmul length mismatch');
  AssertEqualsInt(3, LOut[0], '7*9 mod 10 should be 3');

  AssertTrue(TryBigIntModExpFromUnsignedBytes([$2A], [$00], [$01], LOut, LErr),
    'BigInt exp=0 reduction failed: ' + LErr);
  AssertEqualsInt(1, Length(LOut), 'BigInt exp=0 mod=1 length mismatch');
  AssertEqualsInt(0, LOut[0], 'base^0 mod 1 should be 0');
end;

procedure TestBigIntCrossByteVector;
var
  LOut: TBytes;
  LErr: string;
begin
  AssertTrue(TryBigIntModMulFromUnsignedBytes([$FF, $00, $01], [$02], [$01, $00, $01], LOut, LErr),
    'BigInt cross-byte vector multiply failed: ' + LErr);
  AssertEqualsInt(2, Length(LOut), 'BigInt cross-byte vector result length mismatch');
  AssertEqualsInt($FE, LOut[0], 'BigInt cross-byte vector high byte mismatch');
  AssertEqualsInt($05, LOut[1], 'BigInt cross-byte vector low byte mismatch');
end;

procedure TestBigIntQWordVectorSuite;
begin
  AssertBigIntModMatchesQWord($00, $11, 'mod-zero');
  AssertBigIntModMatchesQWord($10, $11, 'mod-less-than-modulus');
  AssertBigIntModMatchesQWord($11, $11, 'mod-equals-modulus');
  AssertBigIntModMatchesQWord($1234, $11, 'mod-small-divisor');
  AssertBigIntModMatchesQWord($123456, $10001, 'mod-rsa65537');
  AssertBigIntModMatchesQWord($FFFFFFFF, $7FFFFFFF, 'mod-32bit-max');
  AssertBigIntModMatchesQWord(High(QWord), $FFFFFFFF, 'mod-64bit-by-32bit');
  AssertBigIntModMatchesQWord(High(QWord) - QWord($0E), $FFFFFFFB, 'mod-prime-ish');
  AssertBigIntModMatchesQWord($123456789ABCDEF0, $100000000, 'mod-power-of-two');

  AssertBigIntModMulMatchesQWord($00, $1234, $11, 'modmul-zero-left');
  AssertBigIntModMulMatchesQWord($1234, $00, $11, 'modmul-zero-right');
  AssertBigIntModMulMatchesQWord($01, $1234, $11, 'modmul-one-left');
  AssertBigIntModMulMatchesQWord($1234, $01, $11, 'modmul-one-right');
  AssertBigIntModMulMatchesQWord($1234, $5678, $FFFF, 'modmul-16bit');
  AssertBigIntModMulMatchesQWord($123456, $654321, $10001, 'modmul-rsa65537');
  AssertBigIntModMulMatchesQWord($FFFFFFFF, $FFFFFFFD, $7FFFFFFF, 'modmul-32bit-max');
  AssertBigIntModMulMatchesQWord($ABCDEF01, $12345678, $FFFFFFFB, 'modmul-random-a');
  AssertBigIntModMulMatchesQWord(QWord($FFFFFFFF) shl 32, $00000000FFFFFFFF, $FFFFFFFF, 'modmul-crosslimb');

  AssertBigIntModExpMatchesQWord($02, $00, $11, 'modexp-exp-zero');
  AssertBigIntModExpMatchesQWord($00, $05, $11, 'modexp-base-zero');
  AssertBigIntModExpMatchesQWord($01, $1234, $11, 'modexp-base-one');
  AssertBigIntModExpMatchesQWord($02, $10, $11, 'modexp-power-two');
  AssertBigIntModExpMatchesQWord($1234, $03, $10001, 'modexp-rsa65537');
  AssertBigIntModExpMatchesQWord($DEADBEEF, $11, $FFFFFFFB, 'modexp-random-a');
  AssertBigIntModExpMatchesQWord($123456789, $12345, $7FFFFFFF, 'modexp-large-exp');
  AssertBigIntModExpMatchesQWord($FFFFFFFF, $FFFFFFFF, $FFFFFFFB, 'modexp-max-32');

  AssertBigIntSubModMatchesQWord($05, $03, $11, 'submod-no-wrap');
  AssertBigIntSubModMatchesQWord($03, $05, $11, 'submod-wrap');
  AssertBigIntSubModMatchesQWord($11, $01, $11, 'submod-left-equals-mod');
  AssertBigIntSubModMatchesQWord($1234, $5678, $10001, 'submod-rsa65537');
  AssertBigIntSubModMatchesQWord($FFFFFFFF, $FFFFFFFE, $7FFFFFFF, 'submod-32bit');
  AssertBigIntSubModMatchesQWord($ABCDEF01, $12345678, $FFFFFFFB, 'submod-random-a');
end;

procedure TestBigIntErrorSurface;
var
  LOut: TBytes;
  LErr: string;
begin
  AssertTrue(
    not TryBigIntModFromUnsignedBytes([$01], [$00], LOut, LErr),
    'mod zero modulus should fail'
  );
  AssertContains(LErr, 'Modulus is zero', 'mod zero-modulus message mismatch');

  AssertTrue(
    not TryBigIntModExpFromUnsignedBytes([$02], [$03], [$00], LOut, LErr),
    'modexp zero modulus should fail'
  );
  AssertContains(LErr, 'Modulus is zero', 'modexp zero-modulus message mismatch');

  AssertTrue(
    not TryBigIntModMulFromUnsignedBytes([$02], [$03], [$00], LOut, LErr),
    'modmul zero modulus should fail'
  );
  AssertContains(LErr, 'Modulus is zero', 'modmul zero-modulus message mismatch');

  AssertTrue(
    not TryBigIntSubtractModuloFromUnsignedBytes([$02], [$03], [$00], LOut, LErr),
    'submod zero modulus should fail'
  );
  AssertContains(LErr, 'Modulus is zero', 'submod zero-modulus message mismatch');

  AssertTrue(
    not TryBigIntToFixedLengthFromUnsignedBytes([$01], 0, LOut, LErr),
    'fixed length zero should fail'
  );
  AssertContains(LErr, 'RSA output length is invalid', 'fixed-length invalid message mismatch');

  AssertTrue(
    not TryBigIntToFixedLengthFromUnsignedBytes([$01], -1, LOut, LErr),
    'fixed length negative should fail'
  );
  AssertContains(LErr, 'RSA output length is invalid', 'fixed-length negative message mismatch');

  AssertTrue(
    not TryBigIntToFixedLengthFromUnsignedBytes([$01, $00], 1, LOut, LErr),
    'fixed length overflow should fail'
  );
  AssertContains(LErr, 'RSA output does not fit target length', 'fixed-length overflow message mismatch');
end;

procedure TestBigIntLeadingZeroNormalization;
var
  LOut: TBytes;
  LErr: string;
begin
  AssertTrue(
    TryBigIntModFromUnsignedBytes([$00, $00, $01, $23], [$00, $00, $00, $10], LOut, LErr),
    'leading-zero mod should succeed: ' + LErr
  );
  AssertEqualsQWord($03, BytesToQWord(LOut), 'leading-zero mod mismatch');

  AssertTrue(
    TryBigIntModMulFromUnsignedBytes([$00, $00, $01], [$00, $02], [$00, $11], LOut, LErr),
    'leading-zero modmul should succeed: ' + LErr
  );
  AssertEqualsQWord($02, BytesToQWord(LOut), 'leading-zero modmul mismatch');

  AssertTrue(
    TryBigIntModExpFromUnsignedBytes([$00, $02], [$00, $04], [$00, $11], LOut, LErr),
    'leading-zero modexp should succeed: ' + LErr
  );
  AssertEqualsQWord($10, BytesToQWord(LOut), 'leading-zero modexp mismatch');

  AssertTrue(
    TryBigIntSubtractModuloFromUnsignedBytes([$00, $03], [$00, $05], [$00, $11], LOut, LErr),
    'leading-zero submod should succeed: ' + LErr
  );
  AssertEqualsQWord($0F, BytesToQWord(LOut), 'leading-zero submod mismatch');
end;

procedure TestBigIntFixedLengthExactFit;
var
  LOut: TBytes;
  LErr: string;
begin
  AssertTrue(
    TryBigIntToFixedLengthFromUnsignedBytes([$00, $00, $01, $23], 4, LOut, LErr),
    'fixed-length exact fit should succeed: ' + LErr
  );
  AssertEqualsInt(4, Length(LOut), 'fixed-length output length mismatch');
  AssertEqualsInt($00, LOut[0], 'fixed-length byte[0] mismatch');
  AssertEqualsInt($00, LOut[1], 'fixed-length byte[1] mismatch');
  AssertEqualsInt($01, LOut[2], 'fixed-length byte[2] mismatch');
  AssertEqualsInt($23, LOut[3], 'fixed-length byte[3] mismatch');

  AssertTrue(
    TryBigIntToFixedLengthFromUnsignedBytes([$01, $23], 4, LOut, LErr),
    'fixed-length left-pad should succeed: ' + LErr
  );
  AssertEqualsInt($00, LOut[0], 'fixed-length left-pad byte[0] mismatch');
  AssertEqualsInt($00, LOut[1], 'fixed-length left-pad byte[1] mismatch');
  AssertEqualsInt($01, LOut[2], 'fixed-length left-pad byte[2] mismatch');
  AssertEqualsInt($23, LOut[3], 'fixed-length left-pad byte[3] mismatch');
end;

procedure TestRSAModExpExponentGuard;
var
  LSig: TBytes;
  LErr: string;
begin
  AssertTrue(
    not TryRSAModExpSignPurePascal([$01], [$00, $11], [$01, $00, $00, $00, $00], LSig, LErr),
    'RSA oversized exponent should be rejected'
  );
  AssertContains(LErr, 'unreasonably large', 'RSA oversized exponent message mismatch');
end;

procedure TestBigIntRejectsNonCoprimeRSARepresentative;
var
  LSig: TBytes;
  LErr: string;
begin
  AssertTrue(
    not TryRSAModExpSignPurePascal([$06], [$0C], [$03], LSig, LErr),
    'RSA pure Pascal should reject non-coprime message representative'
  );
  AssertContains(LErr, 'not coprime', 'Expected coprime rejection message');
end;

procedure TestTinyModulusDefense;
var
  LSig: TBytes;
  LErr: string;
begin
  AssertTrue(
    not TryRSAModExpSignPurePascal([$01], [$02], [$01], LSig, LErr),
    'Tiny modulus input should be rejected'
  );
  AssertContains(LErr, 'must be odd', 'Tiny modulus defense should fail with odd-modulus validation error');
end;

procedure TestRSASignatureWithDERPrivateKey;
var
  LPemBlob: TBytes;
  LDER: TBytes;
  LType: TPEMType;
  LTranscriptHash: TBytes;
  LInput: TBytes;
  LSig: TBytes;
  LErr: string;
  I: Integer;
begin
  LPemBlob := LoadFileBytes('tests/certificate/test_certs/signer_key.pem');
  AssertTrue(TryExtractFirstPrivateKeyDER(LPemBlob, LDER, LType), 'Failed to extract DER from PEM private key');

  SetLength(LTranscriptHash, 32);
  for I := 0 to 31 do
    LTranscriptHash[I] := Byte($33 + I);
  LInput := BuildTLS13ServerCertificateVerifyInputSHA256(LTranscriptHash);

  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      LDER,
      LInput,
      LSig,
      LErr
    ),
    'RSA-PKCS1 signing with DER key failed: ' + LErr
  );
  AssertEqualsInt(256, Length(LSig), 'RSA-PKCS1 DER signature length should match 2048-bit key');
end;

procedure TestRSASignatureWithPKCS8Attributes;
var
  LPemBlob: TBytes;
  LDER: TBytes;
  LMutatedDER: TBytes;
  LType: TPEMType;
  LTranscriptHash: TBytes;
  LInput: TBytes;
  LSigA: TBytes;
  LSigB: TBytes;
  LErr: string;
  I: Integer;
  LDiff: Integer;
begin
  LPemBlob := LoadFileBytes('tests/certificate/test_certs/signer_key.pem');
  AssertTrue(TryExtractFirstPrivateKeyDER(LPemBlob, LDER, LType), 'Failed to extract DER from PEM private key');
  AssertTrue(LType = pemPrivateKey, 'Expected PKCS#8 PRIVATE KEY input');

  LMutatedDER := BuildPKCS8WithContextAttribute(LDER);
  AssertTrue(Length(LMutatedDER) > 0, 'Failed to build PKCS#8 key with context attribute');

  SetLength(LTranscriptHash, 32);
  for I := 0 to 31 do
    LTranscriptHash[I] := Byte($44 + I);
  LInput := BuildTLS13ServerCertificateVerifyInputSHA256(LTranscriptHash);

  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      LDER,
      LInput,
      LSigA,
      LErr
    ),
    'RSA-PKCS1 signing with original PKCS#8 DER failed: ' + LErr
  );

  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      LMutatedDER,
      LInput,
      LSigB,
      LErr
    ),
    'RSA-PKCS1 signing with attributed PKCS#8 DER failed: ' + LErr
  );

  AssertEqualsInt(Length(LSigA), Length(LSigB), 'PKCS#8 attributed signature length mismatch');
  LDiff := 0;
  for I := 0 to Length(LSigA) - 1 do
    if LSigA[I] <> LSigB[I] then
      Inc(LDiff);
  AssertEqualsInt(0, LDiff, 'PKCS#8 attributes should not change RSA-PKCS1 signature result');
end;

procedure TestRSASignatureWithPEMLeadingJunk;
var
  LKeyBlob: TBytes;
  LMutatedPEM: TBytes;
  LTranscriptHash: TBytes;
  LInput: TBytes;
  LSig: TBytes;
  LErr: string;
  I: Integer;
begin
  LKeyBlob := LoadFileBytes('tests/certificate/test_certs/signer_key.pem');
  LMutatedPEM := BuildPEMPrivateKeyWithLeadingJunk(LKeyBlob);

  SetLength(LTranscriptHash, 32);
  for I := 0 to 31 do
    LTranscriptHash[I] := Byte($73 + I);
  LInput := BuildTLS13ServerCertificateVerifyInputSHA256(LTranscriptHash);

  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      LMutatedPEM,
      LInput,
      LSig,
      LErr
    ),
    'RSA-PKCS1 signing with leading PEM junk failed: ' + LErr
  );
  AssertEqualsInt(256, Length(LSig), 'PEM leading-junk signature length mismatch');
end;

procedure TestRSASignatureUsesFirstUsableRSAKeyBlock;
var
  LKeyBlobA: TBytes;
  LKeyBlobB: TBytes;
  LCombinedPEM: TBytes;
  LTranscriptHash: TBytes;
  LInput: TBytes;
  LSigA: TBytes;
  LSigCombined: TBytes;
  LErr: string;
  I: Integer;
  LDiff: Integer;
begin
  LKeyBlobA := LoadFileBytes('tests/certificate/test_certs/signer_key.pem');
  LKeyBlobB := LoadFileBytes('tests/certificate/test_certs/recipient_key.pem');
  LCombinedPEM := BuildPEMWithMultiplePrivateKeys(LKeyBlobA, LKeyBlobB);

  SetLength(LTranscriptHash, 32);
  for I := 0 to 31 do
    LTranscriptHash[I] := Byte($77 + I);
  LInput := BuildTLS13ServerCertificateVerifyInputSHA256(LTranscriptHash);

  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      LKeyBlobA,
      LInput,
      LSigA,
      LErr
    ),
    'RSA-PKCS1 signing with base key failed: ' + LErr
  );

  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      LCombinedPEM,
      LInput,
      LSigCombined,
      LErr
    ),
    'RSA-PKCS1 signing with multi-key PEM failed: ' + LErr
  );

  AssertEqualsInt(Length(LSigA), Length(LSigCombined), 'Multi-key PEM signature length mismatch');
  LDiff := 0;
  for I := 0 to Length(LSigA) - 1 do
    if LSigA[I] <> LSigCombined[I] then
      Inc(LDiff);
  AssertEqualsInt(0, LDiff, 'Signer should use first usable RSA key block in PEM material');
end;

procedure TestRSASignatureWith1024BitKeyLength;
var
  LKeyBlob: TBytes;
  LTranscriptHash: TBytes;
  LInput: TBytes;
  LSig: TBytes;
  LErr: string;
  I: Integer;
begin
  LKeyBlob := LoadFileBytes('tests/certificate/test_certs/signer_key_1024.pem');
  AssertTrue(Length(LKeyBlob) > 0, 'Fixture 1024-bit key blob should not be empty');

  SetLength(LTranscriptHash, 32);
  for I := 0 to 31 do
    LTranscriptHash[I] := Byte($7A + I);
  LInput := BuildTLS13ServerCertificateVerifyInputSHA256(LTranscriptHash);

  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      LKeyBlob,
      LInput,
      LSig,
      LErr
    ),
    'RSA-PKCS1 signing with generated 1024-bit key failed: ' + LErr
  );
  AssertEqualsInt(128, Length(LSig), 'Generated 1024-bit key should produce 128-byte signature');
end;

procedure TestRSASignatureRejectsPEMWithoutPrivateKeyBlock;
var
  LCertBlob: TBytes;
  LTranscriptHash: TBytes;
  LInput: TBytes;
  LSig: TBytes;
  LErr: string;
  I: Integer;
begin
  LCertBlob := LoadFileBytes('tests/certificate/test_certs/signer_cert.pem');

  SetLength(LTranscriptHash, 32);
  for I := 0 to 31 do
    LTranscriptHash[I] := Byte($6A + I);
  LInput := BuildTLS13ServerCertificateVerifyInputSHA256(LTranscriptHash);

  AssertTrue(
    not TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      LCertBlob,
      LInput,
      LSig,
      LErr
    ),
    'PEM certificate blob should not be accepted as private key material'
  );
  AssertContains(LErr, 'No private key block found in PEM blob', 'Expected missing-private-key-block diagnostic');
end;

procedure TestRSASignatureErrorMessagesAreStable;
var
  LSig: TBytes;
  LErr: string;
begin
  AssertTrue(
    not TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      [],
      [$01],
      LSig,
      LErr
    ),
    'Signer should fail for empty key material'
  );
  AssertContains(LErr, 'Private key material is empty', 'Error message for empty key should remain stable');

  AssertTrue(
    not TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      [$00, $01, $02],
      [$01],
      LSig,
      LErr
    ),
    'Signer should fail for malformed DER'
  );
  AssertContains(LErr, 'Unsupported DER private key format', 'Malformed DER error message should remain stable');
end;

procedure TestRSASignatureKeySizeConsistency;
var
  LKeyBlob: TBytes;
  LTranscriptHash: TBytes;
  LInput: TBytes;
  LSig: TBytes;
  LErr: string;
  I: Integer;
begin
  LKeyBlob := LoadFileBytes('tests/certificate/test_certs/signer_key.pem');

  SetLength(LTranscriptHash, 32);
  for I := 0 to 31 do
    LTranscriptHash[I] := Byte($55 + I);
  LInput := BuildTLS13ServerCertificateVerifyInputSHA256(LTranscriptHash);

  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      LKeyBlob,
      LInput,
      LSig,
      LErr
    ),
    'RSA-PKCS1 signing failed for key-size consistency check: ' + LErr
  );
  AssertEqualsInt(256, Length(LSig), 'Signer key must produce 256-byte signature for 2048-bit modulus');
end;

procedure TestFallbackErrorCodeOnDoubleFailure;
var
  LKeyBlob: TBytes;
  LMutatedBothDER: TBytes;
  LTranscriptHash: TBytes;
  LInput: TBytes;
  LSig: TBytes;
  LErr: string;
  I: Integer;
begin
  LKeyBlob := LoadFileBytes('tests/certificate/test_certs/signer_key.pem');
  LMutatedBothDER := BuildMutatedPrimePAndPrivateExponentPrivateKeyBlob(LKeyBlob);
  AssertTrue(Length(LMutatedBothDER) > 0, 'Failed to produce DER key with corrupted prime p + private exponent');

  SetLength(LTranscriptHash, 32);
  for I := 0 to 31 do
    LTranscriptHash[I] := Byte($66 + I);
  LInput := BuildTLS13ServerCertificateVerifyInputSHA256(LTranscriptHash);

  AssertTrue(
    not TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      LMutatedBothDER,
      LInput,
      LSig,
      LErr
    ),
    'Double-corrupted key should fail signing'
  );
  AssertContains(LErr, 'E_TLS13_SIGNER_FALLBACK_FAILED', 'Fallback failure should expose structured error code');
  AssertContains(LErr, 'crt_reason=', 'Fallback failure should contain CRT reason');
  AssertContains(LErr, 'exp_reason=', 'Fallback failure should contain exponent reason');
end;

procedure TestFallbackErrorCodeFromDirectSignerCall;
var
  LSig: TBytes;
  LErr: string;
begin
  AssertTrue(
    not TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      [],
      [$01],
      LSig,
      LErr
    ),
    'Direct signer call with empty key should fail'
  );
  AssertContains(LErr, 'Private key material is empty', 'Expected private-key-empty diagnostic');
end;

begin
  WriteLn('Testing TLS 1.3 server CertificateVerify helpers...');

  TestSelectSchemeFromClientHello;
  TestBuildCertVerifyInput;
  TestBuildCertificateVerifyHandshake;
  TestPlaceholderSignature;
  TestSignerUnitHasNoExternalBigIntDependency;
  TestBigIntEvenModulusAndZeroExponent;
  TestBigIntCrossByteVector;
  TestBigIntQWordVectorSuite;
  TestBigIntErrorSurface;
  TestBigIntLeadingZeroNormalization;
  TestBigIntFixedLengthExactFit;
  TestRSAModExpExponentGuard;
  TestBigIntRejectsNonCoprimeRSARepresentative;
  TestTinyModulusDefense;
  TestRSASignatureWithDERPrivateKey;
  TestRSASignatureWithPKCS8Attributes;
  TestRSASignatureWithPEMLeadingJunk;
  TestRSASignatureUsesFirstUsableRSAKeyBlock;
  TestRSASignatureWith1024BitKeyLength;
  TestRSASignatureErrorMessagesAreStable;
  TestRSASignatureRejectsPEMWithoutPrivateKeyBlock;
  TestRSASignatureKeySizeConsistency;
  TestRSASignatureFallsBackWhenPrivateExponentCorrupted;
  TestRSASignatureFallsBackWhenCRTInconsistent;
  TestRSASignatureFallsBackWhenPrimeQInconsistent;
  TestRSASignatureFallsBackWhenDPInconsistent;
  TestRSASignatureFallsBackWhenDQInconsistent;
  TestRSASignatureFallsBackWhenQInvInconsistent;
  TestRSASignatureUsesCorruptedExponentWhenCRTBroken;
  TestRSASignatureFallbackErrorWhenPrimePIsOneAndExponentCorrupted;
  TestRSASignatureFallbackErrorWhenPrimeQEqualsPrimePAndExponentCorrupted;
  TestRSASignatureFallbackErrorWhenDPZeroAndExponentCorrupted;
  TestRSASignatureFallbackErrorWhenDQZeroAndExponentCorrupted;
  TestRSASignatureFallbackErrorWhenQInvZeroAndExponentCorrupted;
  TestFallbackErrorCodeFromDirectSignerCall;
  TestRealRSASignature;

  WriteLn('✅ TLS 1.3 server CertificateVerify helper checks passed');
end.
