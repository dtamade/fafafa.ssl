{**
 * Unit: fafafa.ssl.tls12.finished
 * Purpose: TLS 1.2 Finished verify_data helpers
 *}

unit fafafa.ssl.tls12.finished;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils;

const
  TLS12_FINISHED_VERIFY_DATA_LENGTH = 12;

function TLS12ComputeClientFinishedVerifyData_SHA256(
  const AMasterSecret: TBytes;
  const ATranscriptData: TBytes
): TBytes;

function TLS12ComputeServerFinishedVerifyData_SHA256(
  const AMasterSecret: TBytes;
  const ATranscriptData: TBytes
): TBytes;

function TLS12ComputeClientFinishedVerifyData_SHA384(
  const AMasterSecret: TBytes;
  const ATranscriptData: TBytes
): TBytes;

function TLS12ComputeServerFinishedVerifyData_SHA384(
  const AMasterSecret: TBytes;
  const ATranscriptData: TBytes
): TBytes;

implementation

uses
  fafafa.ssl.crypto.hash,
  fafafa.ssl.tls12.prf;

function TLS12ComputeFinishedVerifyData_SHA256(
  const AMasterSecret: TBytes;
  const ALabel: string;
  const ATranscriptData: TBytes
): TBytes;
var
  LTranscriptHash: TBytes;
begin
  LTranscriptHash := SHA256(ATranscriptData);
  Result := TLS12PRF_SHA256(
    AMasterSecret,
    ALabel,
    LTranscriptHash,
    TLS12_FINISHED_VERIFY_DATA_LENGTH
  );
end;

function TLS12ComputeClientFinishedVerifyData_SHA256(
  const AMasterSecret: TBytes;
  const ATranscriptData: TBytes
): TBytes;
begin
  Result := TLS12ComputeFinishedVerifyData_SHA256(
    AMasterSecret,
    'client finished',
    ATranscriptData
  );
end;

function TLS12ComputeServerFinishedVerifyData_SHA256(
  const AMasterSecret: TBytes;
  const ATranscriptData: TBytes
): TBytes;
begin
  Result := TLS12ComputeFinishedVerifyData_SHA256(
    AMasterSecret,
    'server finished',
    ATranscriptData
  );
end;

function TLS12ComputeFinishedVerifyData_SHA384(
  const AMasterSecret: TBytes;
  const ALabel: string;
  const ATranscriptData: TBytes
): TBytes;
var
  LTranscriptHash: TBytes;
begin
  LTranscriptHash := SHA384(ATranscriptData);
  Result := TLS12PRF_SHA384(
    AMasterSecret,
    ALabel,
    LTranscriptHash,
    TLS12_FINISHED_VERIFY_DATA_LENGTH
  );
end;

function TLS12ComputeClientFinishedVerifyData_SHA384(
  const AMasterSecret: TBytes;
  const ATranscriptData: TBytes
): TBytes;
begin
  Result := TLS12ComputeFinishedVerifyData_SHA384(
    AMasterSecret,
    'client finished',
    ATranscriptData
  );
end;

function TLS12ComputeServerFinishedVerifyData_SHA384(
  const AMasterSecret: TBytes;
  const ATranscriptData: TBytes
): TBytes;
begin
  Result := TLS12ComputeFinishedVerifyData_SHA384(
    AMasterSecret,
    'server finished',
    ATranscriptData
  );
end;

end.
