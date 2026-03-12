unit fafafa.ssl.freepascal.keydecrypt;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base;

function TryDecryptPrivateKeyMaterial(
  const AData: TBytes;
  const APassword: string;
  out ADecrypted: TBytes;
  out AWasEncrypted: Boolean;
  out AError: string
): Boolean;

implementation

uses
  fafafa.ssl.pem,
  fafafa.ssl.asn1,
  fafafa.ssl.errors,
  fafafa.ssl.encoding,
  fafafa.ssl.crypto.hash,
  fafafa.ssl.tls13.primitives;

const
  OID_PBES2 = '1.2.840.113549.1.5.13';
  OID_PBKDF2 = '1.2.840.113549.1.5.12';
  OID_HMAC_WITH_SHA256 = '1.2.840.113549.2.9';
  OID_RSA_ENCRYPTION = '1.2.840.113549.1.1.1';
  OID_EC_PUBLIC_KEY = '1.2.840.10045.2.1';
  OID_AES_128_CBC = '2.16.840.1.101.3.4.1.2';
  OID_AES_192_CBC = '2.16.840.1.101.3.4.1.22';
  OID_AES_256_CBC = '2.16.840.1.101.3.4.1.42';

type
  TAESState = array[0..15] of Byte;

  TEncryptedPrivateKeyInfo = record
    CipherOID: string;
    PRFOID: string;
    Salt: TBytes;
    Iterations: Integer;
    IV: TBytes;
    EncryptedData: TBytes;
  end;

var
  AES_SBOX: array[0..255] of Byte;
  AES_INV_SBOX: array[0..255] of Byte;
  AES_TABLES_READY: Boolean = False;

function CopyBytes(const AData: TBytes): TBytes;
begin
  Result := nil;
  Result := Copy(AData, 0, Length(AData));
end;

function BytesFromUTF8(const AValue: string): TBytes;
var
  LText: UTF8String;
begin
  Result := nil;
  LText := UTF8Encode(AValue);
  SetLength(Result, Length(LText));
  if Length(LText) > 0 then
    Move(LText[1], Result[0], Length(LText));
end;

function BytesEqual(const ALeft, ARight: TBytes): Boolean;
var
  I: Integer;
begin
  if Length(ALeft) <> Length(ARight) then
    Exit(False);
  for I := 0 to High(ALeft) do
    if ALeft[I] <> ARight[I] then
      Exit(False);
  Result := True;
end;

function BlobLooksLikePEM(const AData: TBytes): Boolean;
var
  LText: AnsiString;
begin
  if Length(AData) = 0 then
    Exit(False);
  SetLength(LText, Length(AData));
  Move(AData[0], LText[1], Length(AData));
  Result := Pos('-----BEGIN ', string(LText)) > 0;
end;

function RotateLeft8(AValue: Byte; ACount: Integer): Byte; inline;
begin
  Result := ((AValue shl ACount) or (AValue shr (8 - ACount))) and $FF;
end;

function GFMultiply(AValue, AFactor: Byte): Byte;
var
  LResult: Byte;
  LA: Byte;
  LB: Byte;
  I: Integer;
begin
  LResult := 0;
  LA := AValue;
  LB := AFactor;
  for I := 0 to 7 do
  begin
    if (LB and 1) <> 0 then
      LResult := LResult xor LA;
    if (LA and $80) <> 0 then
      LA := (LA shl 1) xor $1B
    else
      LA := LA shl 1;
    LB := LB shr 1;
  end;
  Result := LResult;
end;

function GFPower(AValue: Byte; AExponent: Integer): Byte;
var
  LBase: Byte;
  LResult: Byte;
  LExp: Integer;
begin
  LBase := AValue;
  LResult := 1;
  LExp := AExponent;
  while LExp > 0 do
  begin
    if (LExp and 1) <> 0 then
      LResult := GFMultiply(LResult, LBase);
    LBase := GFMultiply(LBase, LBase);
    LExp := LExp shr 1;
  end;
  Result := LResult;
end;

function GFInverse(AValue: Byte): Byte;
begin
  if AValue = 0 then
    Exit(0);
  Result := GFPower(AValue, 254);
end;

function ComputeAESSubByte(AValue: Byte): Byte;
var
  LInv: Byte;
begin
  LInv := GFInverse(AValue);
  Result := $63 xor LInv xor RotateLeft8(LInv, 1) xor RotateLeft8(LInv, 2) xor
    RotateLeft8(LInv, 3) xor RotateLeft8(LInv, 4);
end;

procedure EnsureAESTables;
var
  I: Integer;
begin
  if AES_TABLES_READY then
    Exit;
  for I := 0 to 255 do
  begin
    AES_SBOX[I] := ComputeAESSubByte(Byte(I));
    AES_INV_SBOX[AES_SBOX[I]] := Byte(I);
  end;
  AES_TABLES_READY := True;
end;

procedure RotateWordLeft(var AWord: array of Byte);
var
  LFirst: Byte;
begin
  LFirst := AWord[0];
  AWord[0] := AWord[1];
  AWord[1] := AWord[2];
  AWord[2] := AWord[3];
  AWord[3] := LFirst;
end;

function AESRcon(AIndex: Integer): Byte;
var
  I: Integer;
begin
  Result := 1;
  if AIndex <= 1 then
    Exit;
  for I := 2 to AIndex do
    Result := GFMultiply(Result, 2);
end;

procedure AESAddRoundKey(var AState: TAESState; const ARoundKeys: TBytes; ARound: Integer);
var
  I: Integer;
  LOffset: Integer;
begin
  LOffset := ARound * 16;
  for I := 0 to 15 do
    AState[I] := AState[I] xor ARoundKeys[LOffset + I];
end;

procedure AESInvSubBytes(var AState: TAESState);
var
  I: Integer;
begin
  for I := 0 to 15 do
    AState[I] := AES_INV_SBOX[AState[I]];
end;

procedure AESInvShiftRows(var AState: TAESState);
var
  LTemp: TAESState;
begin
  LTemp := AState;
  AState[1] := LTemp[13];
  AState[5] := LTemp[1];
  AState[9] := LTemp[5];
  AState[13] := LTemp[9];

  AState[2] := LTemp[10];
  AState[6] := LTemp[14];
  AState[10] := LTemp[2];
  AState[14] := LTemp[6];

  AState[3] := LTemp[7];
  AState[7] := LTemp[11];
  AState[11] := LTemp[15];
  AState[15] := LTemp[3];
end;

procedure AESInvMixColumns(var AState: TAESState);
var
  I: Integer;
  A0, A1, A2, A3: Byte;
begin
  for I := 0 to 3 do
  begin
    A0 := AState[I * 4];
    A1 := AState[I * 4 + 1];
    A2 := AState[I * 4 + 2];
    A3 := AState[I * 4 + 3];

    AState[I * 4] :=
      GFMultiply(A0, $0E) xor GFMultiply(A1, $0B) xor GFMultiply(A2, $0D) xor GFMultiply(A3, $09);
    AState[I * 4 + 1] :=
      GFMultiply(A0, $09) xor GFMultiply(A1, $0E) xor GFMultiply(A2, $0B) xor GFMultiply(A3, $0D);
    AState[I * 4 + 2] :=
      GFMultiply(A0, $0D) xor GFMultiply(A1, $09) xor GFMultiply(A2, $0E) xor GFMultiply(A3, $0B);
    AState[I * 4 + 3] :=
      GFMultiply(A0, $0B) xor GFMultiply(A1, $0D) xor GFMultiply(A2, $09) xor GFMultiply(A3, $0E);
  end;
end;

function ExpandAESKey(const AKey: TBytes; out ARoundKeys: TBytes; out ANr: Integer): Boolean;
var
  LNk: Integer;
  LWords: Integer;
  I: Integer;
  LTemp: array[0..3] of Byte;
begin
  EnsureAESTables;
  Result := False;
  SetLength(ARoundKeys, 0);

  case Length(AKey) of
    16: ANr := 10;
    24: ANr := 12;
    32: ANr := 14;
  else
    Exit;
  end;

  LNk := Length(AKey) div 4;
  LWords := 4 * (ANr + 1);
  SetLength(ARoundKeys, LWords * 4);
  Move(AKey[0], ARoundKeys[0], Length(AKey));

  I := LNk;
  while I < LWords do
  begin
    Move(ARoundKeys[(I - 1) * 4], LTemp[0], 4);

    if (I mod LNk) = 0 then
    begin
      RotateWordLeft(LTemp);
      LTemp[0] := AES_SBOX[LTemp[0]];
      LTemp[1] := AES_SBOX[LTemp[1]];
      LTemp[2] := AES_SBOX[LTemp[2]];
      LTemp[3] := AES_SBOX[LTemp[3]];
      LTemp[0] := LTemp[0] xor AESRcon(I div LNk);
    end
    else if (LNk > 6) and ((I mod LNk) = 4) then
    begin
      LTemp[0] := AES_SBOX[LTemp[0]];
      LTemp[1] := AES_SBOX[LTemp[1]];
      LTemp[2] := AES_SBOX[LTemp[2]];
      LTemp[3] := AES_SBOX[LTemp[3]];
    end;

    ARoundKeys[I * 4] := ARoundKeys[(I - LNk) * 4] xor LTemp[0];
    ARoundKeys[I * 4 + 1] := ARoundKeys[(I - LNk) * 4 + 1] xor LTemp[1];
    ARoundKeys[I * 4 + 2] := ARoundKeys[(I - LNk) * 4 + 2] xor LTemp[2];
    ARoundKeys[I * 4 + 3] := ARoundKeys[(I - LNk) * 4 + 3] xor LTemp[3];
    Inc(I);
  end;

  Result := True;
end;

function AESDecryptBlock(const ABlock, AKey: TBytes; out APlain: TBytes): Boolean;
var
  LState: TAESState;
  LRoundKeys: TBytes;
  LNr: Integer;
  LRound: Integer;
begin
  Result := False;
  SetLength(APlain, 0);
  if Length(ABlock) <> 16 then
    Exit;
  if not ExpandAESKey(AKey, LRoundKeys, LNr) then
    Exit;

  Move(ABlock[0], LState[0], 16);
  AESAddRoundKey(LState, LRoundKeys, LNr);

  for LRound := LNr - 1 downto 1 do
  begin
    AESInvShiftRows(LState);
    AESInvSubBytes(LState);
    AESAddRoundKey(LState, LRoundKeys, LRound);
    AESInvMixColumns(LState);
  end;

  AESInvShiftRows(LState);
  AESInvSubBytes(LState);
  AESAddRoundKey(LState, LRoundKeys, 0);

  SetLength(APlain, 16);
  Move(LState[0], APlain[0], 16);
  Result := True;
end;

function RemovePKCS7Padding(const AData: TBytes; out AResult: TBytes): Boolean;
var
  LPad: Integer;
  I: Integer;
begin
  Result := False;
  SetLength(AResult, 0);
  if Length(AData) = 0 then
    Exit;
  LPad := AData[High(AData)];
  if (LPad <= 0) or (LPad > 16) or (LPad > Length(AData)) then
    Exit;
  for I := Length(AData) - LPad to High(AData) do
    if AData[I] <> Byte(LPad) then
      Exit;
  SetLength(AResult, Length(AData) - LPad);
  if Length(AResult) > 0 then
    Move(AData[0], AResult[0], Length(AResult));
  Result := True;
end;

function TryAESCBCCipherDecrypt(
  const AEncrypted, AKey, AIV: TBytes;
  out APlaintext: TBytes;
  out AError: string
): Boolean;
var
  LBlock: TBytes;
  LDecrypted: TBytes;
  LPrev: TBytes;
  LOffset: Integer;
  I: Integer;
  LCombined: TBytes;
begin
  Result := False;
  SetLength(APlaintext, 0);
  AError := '';

  if (Length(AEncrypted) = 0) or ((Length(AEncrypted) mod 16) <> 0) then
  begin
    AError := 'AES-CBC encrypted payload length must be a positive multiple of 16';
    Exit;
  end;
  if Length(AIV) <> 16 then
  begin
    AError := 'AES-CBC IV length must be 16 bytes';
    Exit;
  end;

  LPrev := CopyBytes(AIV);
  SetLength(LCombined, Length(AEncrypted));
  LOffset := 0;
  while LOffset < Length(AEncrypted) do
  begin
    LBlock := Copy(AEncrypted, LOffset, 16);
    if not AESDecryptBlock(LBlock, AKey, LDecrypted) then
    begin
      AError := 'AES-CBC block decrypt failed';
      Exit;
    end;
    for I := 0 to 15 do
      LCombined[LOffset + I] := LDecrypted[I] xor LPrev[I];
    LPrev := LBlock;
    Inc(LOffset, 16);
  end;

  if not RemovePKCS7Padding(LCombined, APlaintext) then
  begin
    AError := 'AES-CBC plaintext padding is invalid';
    Exit;
  end;

  Result := True;
end;

function PBKDF2_SHA256(
  const APassword, ASalt: TBytes;
  AIterations, AKeyLength: Integer
): TBytes;
var
  LBlocks: Integer;
  LBlockIndex: Integer;
  LOffset: Integer;
  LCopyLen: Integer;
  LIndexBytes: TBytes;
  LU: TBytes;
  LT: TBytes;
  LInput: TBytes;
  I, J: Integer;
begin
  Result := nil;
  if AIterations <= 0 then
    RaiseInvalidParameter('PBKDF2Iterations');
  if AKeyLength <= 0 then
    RaiseInvalidParameter('PBKDF2KeyLength');

  LBlocks := (AKeyLength + 31) div 32;
  SetLength(Result, AKeyLength);
  LOffset := 0;

  for LBlockIndex := 1 to LBlocks do
  begin
    SetLength(LIndexBytes, 4);
    LIndexBytes[0] := Byte((LBlockIndex shr 24) and $FF);
    LIndexBytes[1] := Byte((LBlockIndex shr 16) and $FF);
    LIndexBytes[2] := Byte((LBlockIndex shr 8) and $FF);
    LIndexBytes[3] := Byte(LBlockIndex and $FF);
    LInput := CopyBytes(ASalt);
    SetLength(LInput, Length(LInput) + 4);
    Move(LIndexBytes[0], LInput[Length(ASalt)], 4);

    LU := HMAC_SHA256(APassword, LInput);
    LT := CopyBytes(LU);
    for I := 2 to AIterations do
    begin
      LU := HMAC_SHA256(APassword, LU);
      for J := 0 to High(LT) do
        LT[J] := LT[J] xor LU[J];
    end;

    LCopyLen := 32;
    if (LOffset + LCopyLen) > AKeyLength then
      LCopyLen := AKeyLength - LOffset;
    Move(LT[0], Result[LOffset], LCopyLen);
    Inc(LOffset, LCopyLen);
  end;
end;

function CipherKeyLengthFromOID(const AOID: string): Integer;
begin
  if AOID = OID_AES_128_CBC then
    Exit(16);
  if AOID = OID_AES_192_CBC then
    Exit(24);
  if AOID = OID_AES_256_CBC then
    Exit(32);
  Result := 0;
end;

function LooksLikeSupportedPrivateKeyDER(const AData: TBytes): Boolean;
var
  LReader: TASN1Reader;
  LRoot: TASN1Node;
  LAlgNode: TASN1Node;
  LOID: string;
begin
  Result := False;
  LReader := nil;
  LRoot := nil;
  try
    LReader := TASN1Reader.Create(AData);
    LRoot := LReader.Parse;
    if (LRoot = nil) or (not LRoot.IsSequence) then
      Exit(False);

    if (LRoot.ChildCount >= 9) and LRoot.GetChild(0).IsInteger and
      LRoot.GetChild(1).IsInteger and LRoot.GetChild(3).IsInteger then
      Exit(True);

    if (LRoot.ChildCount >= 2) and LRoot.GetChild(0).IsInteger and
      LRoot.GetChild(1).IsOctetString then
      Exit(True);

    if (LRoot.ChildCount >= 3) and LRoot.GetChild(0).IsInteger and
      LRoot.GetChild(1).IsSequence and LRoot.GetChild(2).IsOctetString then
    begin
      LAlgNode := LRoot.GetChild(1);
      if (LAlgNode.ChildCount >= 1) and LAlgNode.GetChild(0).IsOID then
      begin
        LOID := LAlgNode.GetChild(0).AsOID;
        if (LOID = OID_RSA_ENCRYPTION) or (LOID = OID_EC_PUBLIC_KEY) then
          Exit(True);
      end;
    end;
  finally
    LRoot.Free;
    LReader.Free;
  end;
end;

function LegacyPEMCipherKeyLength(const AAlgorithm: string): Integer;
var
  LUpper: string;
begin
  LUpper := UpperCase(Trim(AAlgorithm));
  if LUpper = 'AES-128-CBC' then
    Exit(16);
  if LUpper = 'AES-192-CBC' then
    Exit(24);
  if LUpper = 'AES-256-CBC' then
    Exit(32);
  Result := 0;
end;

function EVPBytesToKeyMD5(
  const APassword, ASalt: TBytes;
  AKeyLength: Integer
): TBytes;
var
  LBlock: TBytes;
  LInput: TBytes;
  LOffset: Integer;
  LCopyLen: Integer;
begin
  Result := nil;
  if AKeyLength <= 0 then
    RaiseInvalidParameter('LegacyPEMKeyLength');

  SetLength(Result, AKeyLength);
  SetLength(LBlock, 0);
  LOffset := 0;
  while LOffset < AKeyLength do
  begin
    LInput := CopyBytes(LBlock);
    SetLength(LInput, Length(LInput) + Length(APassword) + Length(ASalt));
    if Length(APassword) > 0 then
      Move(APassword[0], LInput[Length(LBlock)], Length(APassword));
    if Length(ASalt) > 0 then
      Move(ASalt[0], LInput[Length(LBlock) + Length(APassword)], Length(ASalt));
    LBlock := MD5(LInput);
    LCopyLen := Length(LBlock);
    if (LOffset + LCopyLen) > AKeyLength then
      LCopyLen := AKeyLength - LOffset;
    Move(LBlock[0], Result[LOffset], LCopyLen);
    Inc(LOffset, LCopyLen);
  end;
end;

function TryGetPEMHeaderValue(AHeaders: TStringList; const AName: string;
  out AValue: string): Boolean;
var
  I: Integer;
  LPrefix: string;
  LLine: string;
begin
  Result := False;
  AValue := '';
  if AHeaders = nil then
    Exit;

  LPrefix := UpperCase(AName) + ':';
  for I := 0 to AHeaders.Count - 1 do
  begin
    LLine := Trim(AHeaders[I]);
    if Pos(LPrefix, UpperCase(LLine)) = 1 then
    begin
      AValue := Trim(Copy(LLine, Length(AName) + 2, MaxInt));
      Exit(True);
    end;
  end;
end;

function TryDecryptLegacyPEMBlock(
  const ABlock: TPEMBlock;
  const APassword: string;
  out ADecryptedDER: TBytes;
  out AError: string
): Boolean;
var
  LDEKInfo: string;
  LParts: TStringArray;
  LAlgorithm: string;
  LIV: TBytes;
  LSalt: TBytes;
  LPasswordBytes: TBytes;
  LKey: TBytes;
  LKeyLength: Integer;
begin
  SetLength(ADecryptedDER, 0);
  AError := '';
  Result := False;

  if not TryGetPEMHeaderValue(ABlock.Headers, 'DEK-Info', LDEKInfo) then
  begin
    AError := 'Legacy encrypted PEM is missing DEK-Info header';
    Exit;
  end;

  LParts := LDEKInfo.Split([',']);
  if Length(LParts) <> 2 then
  begin
    AError := 'Legacy encrypted PEM DEK-Info header is invalid';
    Exit;
  end;

  LAlgorithm := Trim(LParts[0]);
  LKeyLength := LegacyPEMCipherKeyLength(LAlgorithm);
  if LKeyLength = 0 then
  begin
    AError := 'Legacy encrypted PEM cipher is unsupported: ' + LAlgorithm;
    Exit;
  end;

  try
    LIV := TEncodingUtils.HexToBytes(Trim(LParts[1]));
  except
    on E: Exception do
    begin
      AError := 'Legacy encrypted PEM IV is invalid: ' + E.Message;
      Exit;
    end;
  end;

  if Length(LIV) <> 16 then
  begin
    AError := 'Legacy encrypted PEM AES-CBC IV must be 16 bytes';
    Exit;
  end;

  SetLength(LSalt, 8);
  Move(LIV[0], LSalt[0], 8);
  LPasswordBytes := BytesFromUTF8(APassword);
  LKey := EVPBytesToKeyMD5(LPasswordBytes, LSalt, LKeyLength);

  if not TryAESCBCCipherDecrypt(ABlock.Data, LKey, LIV, ADecryptedDER, AError) then
    Exit(False);
  if not LooksLikeSupportedPrivateKeyDER(ADecryptedDER) then
  begin
    AError := 'Decrypted legacy PEM private key payload is invalid or password is incorrect';
    SetLength(ADecryptedDER, 0);
    Exit(False);
  end;

  Result := True;
end;

function TryParsePBKDF2Params(
  AParamsNode: TASN1Node;
  out ASalt: TBytes;
  out AIterations: Integer;
  out APRFOID: string;
  out AError: string
): Boolean;
var
  LNode: TASN1Node;
begin
  Result := False;
  SetLength(ASalt, 0);
  AIterations := 0;
  APRFOID := '';
  AError := '';

  if (AParamsNode = nil) or (not AParamsNode.IsSequence) or (AParamsNode.ChildCount < 2) then
  begin
    AError := 'PBKDF2 parameters are invalid';
    Exit;
  end;

  LNode := AParamsNode.GetChild(0);
  if not LNode.IsOctetString then
  begin
    AError := 'PBKDF2 salt must be OCTET STRING';
    Exit;
  end;
  ASalt := LNode.AsOctetString;

  LNode := AParamsNode.GetChild(1);
  if not LNode.IsInteger then
  begin
    AError := 'PBKDF2 iteration count must be INTEGER';
    Exit;
  end;
  AIterations := LNode.AsInteger;
  if AIterations <= 0 then
  begin
    AError := 'PBKDF2 iteration count must be positive';
    Exit;
  end;

  if AParamsNode.ChildCount >= 4 then
  begin
    LNode := AParamsNode.GetChild(3);
    if (not LNode.IsSequence) or (LNode.ChildCount < 1) or (not LNode.GetChild(0).IsOID) then
    begin
      AError := 'PBKDF2 PRF parameters are invalid';
      Exit;
    end;
    APRFOID := LNode.GetChild(0).AsOID;
  end
  else
    APRFOID := OID_HMAC_WITH_SHA256;

  Result := True;
end;

function TryParseEncryptedPrivateKeyInfo(
  const ADER: TBytes;
  out AInfo: TEncryptedPrivateKeyInfo;
  out AError: string
): Boolean;
var
  LReader: TASN1Reader;
  LRoot: TASN1Node;
  LAlgNode: TASN1Node;
  LPBES2Node: TASN1Node;
  LKDFNode: TASN1Node;
  LEncNode: TASN1Node;
begin
  AInfo := Default(TEncryptedPrivateKeyInfo);
  AError := '';
  Result := False;
  LReader := nil;
  LRoot := nil;
  try
    LReader := TASN1Reader.Create(ADER);
    LRoot := LReader.Parse;
    if (LRoot = nil) or (not LRoot.IsSequence) or (LRoot.ChildCount < 2) then
    begin
      AError := 'EncryptedPrivateKeyInfo root is invalid';
      Exit;
    end;

    LAlgNode := LRoot.GetChild(0);
    if (not LAlgNode.IsSequence) or (LAlgNode.ChildCount < 2) or
      (not LAlgNode.GetChild(0).IsOID) then
    begin
      AError := 'EncryptedPrivateKeyInfo algorithm identifier is invalid';
      Exit;
    end;

    if LAlgNode.GetChild(0).AsOID <> OID_PBES2 then
    begin
      AError := 'Only PBES2 encrypted private keys are supported';
      Exit;
    end;

    LPBES2Node := LAlgNode.GetChild(1);
    if (not LPBES2Node.IsSequence) or (LPBES2Node.ChildCount < 2) then
    begin
      AError := 'PBES2 parameters are invalid';
      Exit;
    end;

    LKDFNode := LPBES2Node.GetChild(0);
    if (not LKDFNode.IsSequence) or (LKDFNode.ChildCount < 2) or
      (not LKDFNode.GetChild(0).IsOID) then
    begin
      AError := 'PBES2 KDF identifier is invalid';
      Exit;
    end;
    if LKDFNode.GetChild(0).AsOID <> OID_PBKDF2 then
    begin
      AError := 'Only PBKDF2 key derivation is supported';
      Exit;
    end;
    if not TryParsePBKDF2Params(LKDFNode.GetChild(1), AInfo.Salt, AInfo.Iterations, AInfo.PRFOID, AError) then
      Exit;

    LEncNode := LPBES2Node.GetChild(1);
    if (not LEncNode.IsSequence) or (LEncNode.ChildCount < 2) or
      (not LEncNode.GetChild(0).IsOID) or (not LEncNode.GetChild(1).IsOctetString) then
    begin
      AError := 'PBES2 encryption scheme is invalid';
      Exit;
    end;
    AInfo.CipherOID := LEncNode.GetChild(0).AsOID;
    AInfo.IV := LEncNode.GetChild(1).AsOctetString;

    if not LRoot.GetChild(1).IsOctetString then
    begin
      AError := 'Encrypted private key payload must be OCTET STRING';
      Exit;
    end;
    AInfo.EncryptedData := LRoot.GetChild(1).AsOctetString;

    Result := True;
  finally
    LRoot.Free;
    LReader.Free;
  end;
end;

function TryDecryptEncryptedPrivateKeyDER(
  const AEncryptedDER: TBytes;
  const APassword: string;
  out APrivateKeyDER: TBytes;
  out AError: string
): Boolean;
var
  LInfo: TEncryptedPrivateKeyInfo;
  LPasswordBytes: TBytes;
  LKey: TBytes;
  LKeyLength: Integer;
begin
  SetLength(APrivateKeyDER, 0);
  AError := '';
  Result := False;

  if not TryParseEncryptedPrivateKeyInfo(AEncryptedDER, LInfo, AError) then
    Exit;

  if LInfo.PRFOID <> OID_HMAC_WITH_SHA256 then
  begin
    AError := 'Only PBKDF2-HMAC-SHA256 encrypted private keys are supported';
    Exit;
  end;

  LKeyLength := CipherKeyLengthFromOID(LInfo.CipherOID);
  if LKeyLength = 0 then
  begin
    AError := 'Only AES-CBC encrypted private keys are supported';
    Exit;
  end;

  if Length(LInfo.IV) <> 16 then
  begin
    AError := 'AES-CBC IV must be 16 bytes';
    Exit;
  end;

  LPasswordBytes := BytesFromUTF8(APassword);
  LKey := PBKDF2_SHA256(LPasswordBytes, LInfo.Salt, LInfo.Iterations, LKeyLength);
  if not TryAESCBCCipherDecrypt(LInfo.EncryptedData, LKey, LInfo.IV, APrivateKeyDER, AError) then
    Exit(False);
  if not LooksLikeSupportedPrivateKeyDER(APrivateKeyDER) then
  begin
    AError := 'Decrypted private key payload is invalid or password is incorrect';
    SetLength(APrivateKeyDER, 0);
    Exit(False);
  end;

  Result := True;
end;

function TryExtractEncryptedPrivateKeyDERFromPEM(
  const AData: TBytes;
  out AEncryptedDER: TBytes;
  out ALegacyBlock: TPEMBlock;
  out AError: string
): Boolean;
var
  LReader: TPEMReader;
  LBlocks: TPEMBlockArray;
  LText: AnsiString;
  I: Integer;
begin
  SetLength(AEncryptedDER, 0);
  ALegacyBlock.BlockType := pemUnknown;
  ALegacyBlock.TypeString := '';
  SetLength(ALegacyBlock.Data, 0);
  ALegacyBlock.Headers := nil;
  AError := '';
  Result := False;

  LReader := TPEMReader.Create;
  try
    SetLength(LText, Length(AData));
    if Length(AData) > 0 then
      Move(AData[0], LText[1], Length(AData));
    LReader.LoadFromString(string(LText));
    LBlocks := LReader.GetPrivateKeys;
    if Length(LBlocks) = 0 then
    begin
      AError := 'No private key block found in PEM material';
      Exit;
    end;

    for I := 0 to High(LBlocks) do
    begin
      if LBlocks[I].BlockType = pemEncryptedPrivateKey then
      begin
        AEncryptedDER := CopyBytes(LBlocks[I].Data);
        Exit(True);
      end;
      if LBlocks[I].IsEncrypted then
      begin
        ALegacyBlock.BlockType := LBlocks[I].BlockType;
        ALegacyBlock.TypeString := LBlocks[I].TypeString;
        ALegacyBlock.Data := CopyBytes(LBlocks[I].Data);
        if LBlocks[I].Headers <> nil then
        begin
          ALegacyBlock.Headers := TStringList.Create;
          ALegacyBlock.Headers.Assign(LBlocks[I].Headers);
        end;
        Exit(False);
      end;
    end;
  finally
    LReader.Free;
  end;
end;

function TryDecryptPrivateKeyMaterial(
  const AData: TBytes;
  const APassword: string;
  out ADecrypted: TBytes;
  out AWasEncrypted: Boolean;
  out AError: string
): Boolean;
var
  LEncryptedDER: TBytes;
  LInfo: TEncryptedPrivateKeyInfo;
  LLegacyBlock: TPEMBlock;
begin
  AError := '';
  AWasEncrypted := False;
  ADecrypted := CopyBytes(AData);

  if BlobLooksLikePEM(AData) then
  begin
    if not TryExtractEncryptedPrivateKeyDERFromPEM(AData, LEncryptedDER, LLegacyBlock, AError) then
    begin
      AWasEncrypted := True;
      if LLegacyBlock.BlockType <> pemUnknown then
        Exit(TryDecryptLegacyPEMBlock(LLegacyBlock, APassword, ADecrypted, AError));
      if AError = '' then
        Exit(True);
      Exit(False);
    end;
    AWasEncrypted := True;
    Exit(TryDecryptEncryptedPrivateKeyDER(LEncryptedDER, APassword, ADecrypted, AError));
  end;

  if not TryParseEncryptedPrivateKeyInfo(AData, LInfo, AError) then
  begin
    AError := '';
    Exit(True);
  end;

  AWasEncrypted := True;
  Result := TryDecryptEncryptedPrivateKeyDER(AData, APassword, ADecrypted, AError);
end;

end.
