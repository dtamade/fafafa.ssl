{**
 * Unit: fafafa.ssl.tls13.finished
 * Purpose: TLS 1.3 Finished verify_data 计算/校验（SHA-256 路径）
 *}

unit fafafa.ssl.tls13.finished;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils;

function TLS13FinishedKeySHA256(const ATrafficSecret: TBytes): TBytes;
function TLS13ComputeFinishedVerifyDataSHA256(const AFinishedKey, ATranscriptHash: TBytes): TBytes;
function TLS13ComputeFinishedVerifyDataFromTrafficSecretSHA256(
  const ATrafficSecret, ATranscriptHash: TBytes
): TBytes;
function TLS13VerifyFinishedSHA256(
  const ATrafficSecret, ATranscriptHash, APeerVerifyData: TBytes
): Boolean;

implementation

uses
  fafafa.ssl.errors,
  fafafa.ssl.crypto.constant_time,
  fafafa.ssl.tls13.primitives;

const
  TLS13_SHA256_HASH_SIZE = 32;

function TLS13FinishedKeySHA256(const ATrafficSecret: TBytes): TBytes;
var
  LEmpty: TBytes;
begin
  if Length(ATrafficSecret) <> TLS13_SHA256_HASH_SIZE then
    RaiseInvalidParameter('TLS13TrafficSecret');

  SetLength(LEmpty, 0);
  Result := TLS13_HKDF_Expand_Label_SHA256(
    ATrafficSecret,
    'finished',
    LEmpty,
    TLS13_SHA256_HASH_SIZE
  );
end;

function TLS13ComputeFinishedVerifyDataSHA256(const AFinishedKey, ATranscriptHash: TBytes): TBytes;
begin
  if Length(AFinishedKey) <> TLS13_SHA256_HASH_SIZE then
    RaiseInvalidParameter('TLS13FinishedKey');
  if Length(ATranscriptHash) <> TLS13_SHA256_HASH_SIZE then
    RaiseInvalidParameter('TLS13TranscriptHash');

  Result := HMAC_SHA256(AFinishedKey, ATranscriptHash);
end;

function TLS13ComputeFinishedVerifyDataFromTrafficSecretSHA256(
  const ATrafficSecret, ATranscriptHash: TBytes
): TBytes;
var
  LFinishedKey: TBytes;
begin
  LFinishedKey := TLS13FinishedKeySHA256(ATrafficSecret);
  Result := TLS13ComputeFinishedVerifyDataSHA256(LFinishedKey, ATranscriptHash);
end;

function TLS13VerifyFinishedSHA256(
  const ATrafficSecret, ATranscriptHash, APeerVerifyData: TBytes
): Boolean;
var
  LExpected: TBytes;
begin
  LExpected := TLS13ComputeFinishedVerifyDataFromTrafficSecretSHA256(
    ATrafficSecret,
    ATranscriptHash
  );

  Result := TConstantTime.CompareBytes(LExpected, APeerVerifyData) = 1;
end;

end.
