unit fafafa.ssl.crypto.aesgcm;

{$mode objfpc}{$H+}{$J-}

interface

uses
  SysUtils;

function PurePascalAESGCMEncrypt(
  const AKey, AIV, APlaintext, AAAD: TBytes;
  out ACiphertext, ATag: TBytes
): Boolean;

function PurePascalAESGCMDecrypt(
  const AKey, AIV, ACiphertext, ATag, AAAD: TBytes;
  out APlaintext: TBytes
): Boolean;

implementation

uses
  fafafa.ssl.crypto.constant_time;

type
  TAESBlock = array[0..15] of Byte;
  TAESExpandedKey = array[0..59] of UInt32;

const
  AES_BLOCK_SIZE = 16;
  GCM_TAG_SIZE = 16;

var
  SBox: array[0..255] of Byte = (
    $63,$7c,$77,$7b,$f2,$6b,$6f,$c5,$30,$01,$67,$2b,$fe,$d7,$ab,$76,
    $ca,$82,$c9,$7d,$fa,$59,$47,$f0,$ad,$d4,$a2,$af,$9c,$a4,$72,$c0,
    $b7,$fd,$93,$26,$36,$3f,$f7,$cc,$34,$a5,$e5,$f1,$71,$d8,$31,$15,
    $04,$c7,$23,$c3,$18,$96,$05,$9a,$07,$12,$80,$e2,$eb,$27,$b2,$75,
    $09,$83,$2c,$1a,$1b,$6e,$5a,$a0,$52,$3b,$d6,$b3,$29,$e3,$2f,$84,
    $53,$d1,$00,$ed,$20,$fc,$b1,$5b,$6a,$cb,$be,$39,$4a,$4c,$58,$cf,
    $d0,$ef,$aa,$fb,$43,$4d,$33,$85,$45,$f9,$02,$7f,$50,$3c,$9f,$a8,
    $51,$a3,$40,$8f,$92,$9d,$38,$f5,$bc,$b6,$da,$21,$10,$ff,$f3,$d2,
    $cd,$0c,$13,$ec,$5f,$97,$44,$17,$c4,$a7,$7e,$3d,$64,$5d,$19,$73,
    $60,$81,$4f,$dc,$22,$2a,$90,$88,$46,$ee,$b8,$14,$de,$5e,$0b,$db,
    $e0,$32,$3a,$0a,$49,$06,$24,$5c,$c2,$d3,$ac,$62,$91,$95,$e4,$79,
    $e7,$c8,$37,$6d,$8d,$d5,$4e,$a9,$6c,$56,$f4,$ea,$65,$7a,$ae,$08,
    $ba,$78,$25,$2e,$1c,$a6,$b4,$c6,$e8,$dd,$74,$1f,$4b,$bd,$8b,$8a,
    $70,$3e,$b5,$66,$48,$03,$f6,$0e,$61,$35,$57,$b9,$86,$c1,$1d,$9e,
    $e1,$f8,$98,$11,$69,$d9,$8e,$94,$9b,$1e,$87,$e9,$ce,$55,$28,$df,
    $8c,$a1,$89,$0d,$bf,$e6,$42,$68,$41,$99,$2d,$0f,$b0,$54,$bb,$16
  );

  Rcon: array[0..9] of UInt32 = (
    $01000000, $02000000, $04000000, $08000000, $10000000,
    $20000000, $40000000, $80000000, $1b000000, $36000000
  );

function SubWord(W: UInt32): UInt32; inline;
begin
  Result := (UInt32(SBox[(W shr 24) and $FF]) shl 24) or
            (UInt32(SBox[(W shr 16) and $FF]) shl 16) or
            (UInt32(SBox[(W shr 8) and $FF]) shl 8) or
            UInt32(SBox[W and $FF]);
end;

function RotWord(W: UInt32): UInt32; inline;
begin
  Result := (W shl 8) or (W shr 24);
end;

procedure AESKeyExpand(const AKey: TBytes; out AExpandedKey: TAESExpandedKey; out ANr: Integer);
var
  Nk, I: Integer;
  Temp: UInt32;
begin
  case Length(AKey) of
    16: begin Nk := 4; ANr := 10; end;
    24: begin Nk := 6; ANr := 12; end;
    32: begin Nk := 8; ANr := 14; end;
  else
    ANr := 0;
    Exit;
  end;

  for I := 0 to Nk - 1 do
    AExpandedKey[I] := (UInt32(AKey[I*4]) shl 24) or (UInt32(AKey[I*4+1]) shl 16) or
                       (UInt32(AKey[I*4+2]) shl 8) or UInt32(AKey[I*4+3]);

  for I := Nk to (ANr + 1) * 4 - 1 do
  begin
    Temp := AExpandedKey[I - 1];
    if (I mod Nk) = 0 then
      Temp := SubWord(RotWord(Temp)) xor Rcon[(I div Nk) - 1]
    else if (Nk > 6) and ((I mod Nk) = 4) then
      Temp := SubWord(Temp);
    AExpandedKey[I] := AExpandedKey[I - Nk] xor Temp;
  end;
end;

procedure AESEncryptBlock(const AInput: TAESBlock; out AOutput: TAESBlock;
  const AExpandedKey: TAESExpandedKey; ANr: Integer);
var
  S: array[0..15] of Byte;
  T: array[0..15] of Byte;
  I, R: Integer;
  A, B, C, D: Byte;

  function XTime(X: Byte): Byte; inline;
  begin
    Result := (X shl 1) xor (((X shr 7) and 1) * $1B);
  end;

begin
  for I := 0 to 15 do
    S[I] := AInput[I] xor Byte(AExpandedKey[I div 4] shr (24 - (I mod 4) * 8));

  for R := 1 to ANr - 1 do
  begin
    for I := 0 to 15 do
      S[I] := SBox[S[I]];

    T[0] := S[0]; T[1] := S[5]; T[2] := S[10]; T[3] := S[15];
    T[4] := S[4]; T[5] := S[9]; T[6] := S[14]; T[7] := S[3];
    T[8] := S[8]; T[9] := S[13]; T[10] := S[2]; T[11] := S[7];
    T[12] := S[12]; T[13] := S[1]; T[14] := S[6]; T[15] := S[11];

    for I := 0 to 3 do
    begin
      A := T[I*4]; B := T[I*4+1]; C := T[I*4+2]; D := T[I*4+3];
      S[I*4]   := XTime(A) xor XTime(B) xor B xor C xor D;
      S[I*4+1] := A xor XTime(B) xor XTime(C) xor C xor D;
      S[I*4+2] := A xor B xor XTime(C) xor XTime(D) xor D;
      S[I*4+3] := XTime(A) xor A xor B xor C xor XTime(D);
    end;

    for I := 0 to 15 do
      S[I] := S[I] xor Byte(AExpandedKey[R * 4 + I div 4] shr (24 - (I mod 4) * 8));
  end;

  for I := 0 to 15 do
    S[I] := SBox[S[I]];

  T[0] := S[0]; T[1] := S[5]; T[2] := S[10]; T[3] := S[15];
  T[4] := S[4]; T[5] := S[9]; T[6] := S[14]; T[7] := S[3];
  T[8] := S[8]; T[9] := S[13]; T[10] := S[2]; T[11] := S[7];
  T[12] := S[12]; T[13] := S[1]; T[14] := S[6]; T[15] := S[11];

  for I := 0 to 15 do
    AOutput[I] := T[I] xor Byte(AExpandedKey[ANr * 4 + I div 4] shr (24 - (I mod 4) * 8));
end;

procedure GHASHMultiply(var AX: TAESBlock; const AH: TAESBlock);
var
  V: array[0..15] of Byte;
  Z: array[0..15] of Byte;
  I, J, K: Integer;
  Bit: Byte;
  Carry: Byte;
begin
  Move(AH[0], V[0], 16);
  FillChar(Z[0], 16, 0);

  for I := 0 to 15 do
    for J := 7 downto 0 do
    begin
      Bit := (AX[I] shr J) and 1;
      if Bit = 1 then
        for K := 0 to 15 do
          Z[K] := Z[K] xor V[K];

      Carry := V[15] and 1;
      for K := 15 downto 1 do
        V[K] := (V[K] shr 1) or ((V[K-1] and 1) shl 7);
      V[0] := V[0] shr 1;
      if Carry = 1 then
        V[0] := V[0] xor $E1;
    end;

  Move(Z[0], AX[0], 16);
end;

procedure GHASHUpdate(var AState: TAESBlock; const AH: TAESBlock; const AData: TBytes; AOffset, ALen: Integer);
var
  I, J, Blocks, Rem: Integer;
  Block: TAESBlock;
begin
  Blocks := ALen div 16;
  Rem := ALen mod 16;

  for I := 0 to Blocks - 1 do
  begin
    for J := 0 to 15 do
      AState[J] := AState[J] xor AData[AOffset + I * 16 + J];
    GHASHMultiply(AState, AH);
  end;

  if Rem > 0 then
  begin
    FillChar(Block[0], 16, 0);
    Move(AData[AOffset + Blocks * 16], Block[0], Rem);
    for J := 0 to 15 do
      AState[J] := AState[J] xor Block[J];
    GHASHMultiply(AState, AH);
  end;
end;

procedure IncrementCounter(var ACounter: TAESBlock);
var
  C: UInt32;
begin
  C := (UInt32(ACounter[12]) shl 24) or (UInt32(ACounter[13]) shl 16) or
       (UInt32(ACounter[14]) shl 8) or UInt32(ACounter[15]);
  Inc(C);
  ACounter[12] := Byte(C shr 24);
  ACounter[13] := Byte(C shr 16);
  ACounter[14] := Byte(C shr 8);
  ACounter[15] := Byte(C);
end;

procedure GCTR(const AExpandedKey: TAESExpandedKey; ANr: Integer;
  const AICB: TAESBlock; const AInput: TBytes; AInputOffset, AInputLen: Integer;
  var AOutput: TBytes; AOutputOffset: Integer);
var
  CB, EncCB: TAESBlock;
  I, J, Blocks, Rem: Integer;
begin
  if AInputLen = 0 then Exit;

  Move(AICB[0], CB[0], 16);
  Blocks := AInputLen div 16;
  Rem := AInputLen mod 16;

  for I := 0 to Blocks - 1 do
  begin
    AESEncryptBlock(CB, EncCB, AExpandedKey, ANr);
    for J := 0 to 15 do
      AOutput[AOutputOffset + I * 16 + J] := AInput[AInputOffset + I * 16 + J] xor EncCB[J];
    IncrementCounter(CB);
  end;

  if Rem > 0 then
  begin
    AESEncryptBlock(CB, EncCB, AExpandedKey, ANr);
    for J := 0 to Rem - 1 do
      AOutput[AOutputOffset + Blocks * 16 + J] := AInput[AInputOffset + Blocks * 16 + J] xor EncCB[J];
  end;
end;

function PurePascalAESGCMEncrypt(
  const AKey, AIV, APlaintext, AAAD: TBytes;
  out ACiphertext, ATag: TBytes
): Boolean;
var
  ExpandedKey: TAESExpandedKey;
  Nr: Integer;
  H, J0, S: TAESBlock;
  ZeroBlock: TAESBlock;
  LenBlock: TAESBlock;
  TagBlock, EncJ0: TAESBlock;
  ICB: TAESBlock;
  PlainLen, AADLen: Integer;
  I: Integer;
  AADBits, CTBits: UInt64;
begin
  Result := False;
  SetLength(ACiphertext, 0);
  SetLength(ATag, 0);

  if not (Length(AKey) in [16, 24, 32]) then Exit;
  if Length(AIV) <> 12 then Exit;

  AESKeyExpand(AKey, ExpandedKey, Nr);
  if Nr = 0 then Exit;

  FillChar(ZeroBlock[0], 16, 0);
  AESEncryptBlock(ZeroBlock, H, ExpandedKey, Nr);

  FillChar(J0[0], 16, 0);
  Move(AIV[0], J0[0], 12);
  J0[15] := 1;

  PlainLen := Length(APlaintext);
  AADLen := Length(AAAD);

  SetLength(ACiphertext, PlainLen);

  if PlainLen > 0 then
  begin
    Move(J0[0], ICB[0], 16);
    IncrementCounter(ICB);
    GCTR(ExpandedKey, Nr, ICB, APlaintext, 0, PlainLen, ACiphertext, 0);
  end;

  FillChar(S[0], 16, 0);
  if AADLen > 0 then
    GHASHUpdate(S, H, AAAD, 0, AADLen);
  if PlainLen > 0 then
    GHASHUpdate(S, H, ACiphertext, 0, PlainLen);

  FillChar(LenBlock[0], 16, 0);
  AADBits := UInt64(AADLen) * 8;
  CTBits := UInt64(PlainLen) * 8;
  LenBlock[0] := Byte(AADBits shr 56);
  LenBlock[1] := Byte(AADBits shr 48);
  LenBlock[2] := Byte(AADBits shr 40);
  LenBlock[3] := Byte(AADBits shr 32);
  LenBlock[4] := Byte(AADBits shr 24);
  LenBlock[5] := Byte(AADBits shr 16);
  LenBlock[6] := Byte(AADBits shr 8);
  LenBlock[7] := Byte(AADBits);
  LenBlock[8] := Byte(CTBits shr 56);
  LenBlock[9] := Byte(CTBits shr 48);
  LenBlock[10] := Byte(CTBits shr 40);
  LenBlock[11] := Byte(CTBits shr 32);
  LenBlock[12] := Byte(CTBits shr 24);
  LenBlock[13] := Byte(CTBits shr 16);
  LenBlock[14] := Byte(CTBits shr 8);
  LenBlock[15] := Byte(CTBits);

  for I := 0 to 15 do
    S[I] := S[I] xor LenBlock[I];
  GHASHMultiply(S, H);

  AESEncryptBlock(J0, EncJ0, ExpandedKey, Nr);
  SetLength(ATag, GCM_TAG_SIZE);
  for I := 0 to 15 do
    ATag[I] := S[I] xor EncJ0[I];

  Result := True;
end;

function PurePascalAESGCMDecrypt(
  const AKey, AIV, ACiphertext, ATag, AAAD: TBytes;
  out APlaintext: TBytes
): Boolean;
var
  ExpandedKey: TAESExpandedKey;
  Nr: Integer;
  H, J0, S: TAESBlock;
  ZeroBlock: TAESBlock;
  LenBlock: TAESBlock;
  ComputedTag, EncJ0: TAESBlock;
  ICB: TAESBlock;
  CTLen, AADLen: Integer;
  I: Integer;
  TagBytes: TBytes;
  AADBits, CTBits: UInt64;
begin
  Result := False;
  SetLength(APlaintext, 0);

  if not (Length(AKey) in [16, 24, 32]) then Exit;
  if Length(AIV) <> 12 then Exit;
  if Length(ATag) <> GCM_TAG_SIZE then Exit;

  AESKeyExpand(AKey, ExpandedKey, Nr);
  if Nr = 0 then Exit;

  FillChar(ZeroBlock[0], 16, 0);
  AESEncryptBlock(ZeroBlock, H, ExpandedKey, Nr);

  FillChar(J0[0], 16, 0);
  Move(AIV[0], J0[0], 12);
  J0[15] := 1;

  CTLen := Length(ACiphertext);
  AADLen := Length(AAAD);

  FillChar(S[0], 16, 0);
  if AADLen > 0 then
    GHASHUpdate(S, H, AAAD, 0, AADLen);
  if CTLen > 0 then
    GHASHUpdate(S, H, ACiphertext, 0, CTLen);

  FillChar(LenBlock[0], 16, 0);
  AADBits := UInt64(AADLen) * 8;
  CTBits := UInt64(CTLen) * 8;
  LenBlock[0] := Byte(AADBits shr 56);
  LenBlock[1] := Byte(AADBits shr 48);
  LenBlock[2] := Byte(AADBits shr 40);
  LenBlock[3] := Byte(AADBits shr 32);
  LenBlock[4] := Byte(AADBits shr 24);
  LenBlock[5] := Byte(AADBits shr 16);
  LenBlock[6] := Byte(AADBits shr 8);
  LenBlock[7] := Byte(AADBits);
  LenBlock[8] := Byte(CTBits shr 56);
  LenBlock[9] := Byte(CTBits shr 48);
  LenBlock[10] := Byte(CTBits shr 40);
  LenBlock[11] := Byte(CTBits shr 32);
  LenBlock[12] := Byte(CTBits shr 24);
  LenBlock[13] := Byte(CTBits shr 16);
  LenBlock[14] := Byte(CTBits shr 8);
  LenBlock[15] := Byte(CTBits);

  for I := 0 to 15 do
    S[I] := S[I] xor LenBlock[I];
  GHASHMultiply(S, H);

  AESEncryptBlock(J0, EncJ0, ExpandedKey, Nr);
  SetLength(TagBytes, GCM_TAG_SIZE);
  for I := 0 to 15 do
    TagBytes[I] := S[I] xor EncJ0[I];

  if TConstantTime.CompareBytes(TagBytes, ATag) <> 1 then
    Exit(False);

  SetLength(APlaintext, CTLen);
  if CTLen > 0 then
  begin
    Move(J0[0], ICB[0], 16);
    IncrementCounter(ICB);
    GCTR(ExpandedKey, Nr, ICB, ACiphertext, 0, CTLen, APlaintext, 0);
  end;

  Result := True;
end;

end.
