{**
 * Unit: fafafa.ssl.tls12.prf
 * Purpose: TLS 1.2 PRF / master secret / key block helpers
 *}

unit fafafa.ssl.tls12.prf;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils;

function TLS12PRF_SHA256(
  const ASecret: TBytes;
  const ALabel: string;
  const ASeed: TBytes;
  ALength: Integer
): TBytes;

function TLS12PRF_SHA384(
  const ASecret: TBytes;
  const ALabel: string;
  const ASeed: TBytes;
  ALength: Integer
): TBytes;

function TLS12MasterSecret_SHA256(
  const APreMasterSecret, AClientRandom, AServerRandom: TBytes
): TBytes;

function TLS12MasterSecret_SHA384(
  const APreMasterSecret, AClientRandom, AServerRandom: TBytes
): TBytes;

function TLS12KeyBlock_SHA256(
  const AMasterSecret, AServerRandom, AClientRandom: TBytes;
  ALength: Integer
): TBytes;

function TLS12KeyBlock_SHA384(
  const AMasterSecret, AServerRandom, AClientRandom: TBytes;
  ALength: Integer
): TBytes;

implementation

uses
  fafafa.ssl.errors,
  fafafa.ssl.tls12.wire,
  fafafa.ssl.tls13.primitives;

const
  TLS12_SHA256_DIGEST_SIZE = 32;
  TLS12_SHA384_DIGEST_SIZE = 48;

function CopyBytes(const AData: TBytes): TBytes;
begin
  Result := nil;
  SetLength(Result, Length(AData));
  if Length(AData) > 0 then
    Move(AData[0], Result[0], Length(AData));
end;

function ConcatBytes(const ALeft, ARight: TBytes): TBytes;
var
  LLeftLen, LRightLen: Integer;
begin
  Result := nil;
  LLeftLen := Length(ALeft);
  LRightLen := Length(ARight);
  SetLength(Result, LLeftLen + LRightLen);

  if LLeftLen > 0 then
    Move(ALeft[0], Result[0], LLeftLen);
  if LRightLen > 0 then
    Move(ARight[0], Result[LLeftLen], LRightLen);
end;

function BytesFromAnsi(const AValue: AnsiString): TBytes;
begin
  Result := nil;
  SetLength(Result, Length(AValue));
  if Length(AValue) > 0 then
    Move(AValue[1], Result[0], Length(AValue));
end;

function TLS12PHashSHA256(const ASecret, ASeed: TBytes; ALength: Integer): TBytes;
var
  LA, LChunk, LOutput: TBytes;
begin
  if ALength < 0 then
    RaiseInvalidParameter('TLS12PHashLength');
  if ALength = 0 then
  begin
    Result := nil;
    Exit;
  end;

  LOutput := nil;
  LA := CopyBytes(ASeed);
  while Length(LOutput) < ALength do
  begin
    LA := HMAC_SHA256(ASecret, LA);
    LChunk := HMAC_SHA256(ASecret, ConcatBytes(LA, ASeed));
    AppendBytes(LOutput, LChunk);
  end;
  SetLength(LOutput, ALength);
  Result := LOutput;
end;

function TLS12PHashSHA384(const ASecret, ASeed: TBytes; ALength: Integer): TBytes;
var
  LA, LChunk, LOutput: TBytes;
begin
  if ALength < 0 then
    RaiseInvalidParameter('TLS12PHashLength');
  if ALength = 0 then
  begin
    Result := nil;
    Exit;
  end;

  LOutput := nil;
  LA := CopyBytes(ASeed);
  while Length(LOutput) < ALength do
  begin
    LA := HMAC_SHA384(ASecret, LA);
    LChunk := HMAC_SHA384(ASecret, ConcatBytes(LA, ASeed));
    AppendBytes(LOutput, LChunk);
  end;
  SetLength(LOutput, ALength);
  Result := LOutput;
end;

function TLS12PRF_SHA256(
  const ASecret: TBytes;
  const ALabel: string;
  const ASeed: TBytes;
  ALength: Integer
): TBytes;
var
  LLabelSeed: TBytes;
begin
  LLabelSeed := ConcatBytes(BytesFromAnsi(AnsiString(ALabel)), ASeed);
  Result := TLS12PHashSHA256(ASecret, LLabelSeed, ALength);
end;

function TLS12PRF_SHA384(
  const ASecret: TBytes;
  const ALabel: string;
  const ASeed: TBytes;
  ALength: Integer
): TBytes;
var
  LLabelSeed: TBytes;
begin
  LLabelSeed := ConcatBytes(BytesFromAnsi(AnsiString(ALabel)), ASeed);
  Result := TLS12PHashSHA384(ASecret, LLabelSeed, ALength);
end;

function TLS12MasterSecret_SHA256(
  const APreMasterSecret, AClientRandom, AServerRandom: TBytes
): TBytes;
begin
  Result := TLS12PRF_SHA256(
    APreMasterSecret,
    'master secret',
    ConcatBytes(AClientRandom, AServerRandom),
    TLS12_MASTER_SECRET_LENGTH
  );
end;

function TLS12MasterSecret_SHA384(
  const APreMasterSecret, AClientRandom, AServerRandom: TBytes
): TBytes;
begin
  Result := TLS12PRF_SHA384(
    APreMasterSecret,
    'master secret',
    ConcatBytes(AClientRandom, AServerRandom),
    TLS12_MASTER_SECRET_LENGTH
  );
end;

function TLS12KeyBlock_SHA256(
  const AMasterSecret, AServerRandom, AClientRandom: TBytes;
  ALength: Integer
): TBytes;
begin
  Result := TLS12PRF_SHA256(
    AMasterSecret,
    'key expansion',
    ConcatBytes(AServerRandom, AClientRandom),
    ALength
  );
end;

function TLS12KeyBlock_SHA384(
  const AMasterSecret, AServerRandom, AClientRandom: TBytes;
  ALength: Integer
): TBytes;
begin
  Result := TLS12PRF_SHA384(
    AMasterSecret,
    'key expansion',
    ConcatBytes(AServerRandom, AClientRandom),
    ALength
  );
end;

end.
