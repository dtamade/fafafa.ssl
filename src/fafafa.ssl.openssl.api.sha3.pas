{******************************************************************************}
{                                                                              }
{  fafafa.ssl - OpenSSL SHA3 Module                                          }
{                                                                              }
{  Copyright (c) 2024 fafafa                                                  }
{                                                                              }
{******************************************************************************}

unit fafafa.ssl.openssl.api.sha3;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api.consts;

const
  SHA3_224_DIGEST_LENGTH = 28;
  SHA3_256_DIGEST_LENGTH = 32;
  SHA3_384_DIGEST_LENGTH = 48;
  SHA3_512_DIGEST_LENGTH = 64;
  
  SHAKE128_DIGEST_LENGTH = 16;
  SHAKE256_DIGEST_LENGTH = 32;
  
  KECCAK_MDSIZE = 200;
  KECCAK1600_WIDTH = 1600;

type
  // Keccak/SHA3 context
  PKECCAK1600_CTX = ^KECCAK1600_CTX;
  KECCAK1600_CTX = record
    A: array[0..4, 0..4] of UInt64;
    block: record
      case Integer of
        0: (b: array[0..KECCAK_MDSIZE-1] of Byte);
        1: (q: array[0..(KECCAK_MDSIZE div 8)-1] of UInt64);
    end;
    bufsz: NativeUInt;
    pad: Byte;
    mdlen: NativeUInt;
    rsiz: NativeUInt;
    md_size: NativeUInt;
  end;
  
  SHA3_CTX = KECCAK1600_CTX;
  PSHA3_CTX = ^SHA3_CTX;
  
  SHAKE_CTX = KECCAK1600_CTX;
  PSHAKE_CTX = ^SHAKE_CTX;

type
  // SHA3-224 functions
  TSHA3_224_Init = function(c: PSHA3_CTX): Integer; cdecl;
  TSHA3_224_Update = function(c: PSHA3_CTX; const data: Pointer; len: NativeUInt): Integer; cdecl;
  TSHA3_224_Final = function(md: PByte; c: PSHA3_CTX): Integer; cdecl;
  TSHA3_224 = function(const d: PByte; n: NativeUInt; md: PByte): PByte; cdecl;
  
  // SHA3-256 functions
  TSHA3_256_Init = function(c: PSHA3_CTX): Integer; cdecl;
  TSHA3_256_Update = function(c: PSHA3_CTX; const data: Pointer; len: NativeUInt): Integer; cdecl;
  TSHA3_256_Final = function(md: PByte; c: PSHA3_CTX): Integer; cdecl;
  TSHA3_256 = function(const d: PByte; n: NativeUInt; md: PByte): PByte; cdecl;
  
  // SHA3-384 functions
  TSHA3_384_Init = function(c: PSHA3_CTX): Integer; cdecl;
  TSHA3_384_Update = function(c: PSHA3_CTX; const data: Pointer; len: NativeUInt): Integer; cdecl;
  TSHA3_384_Final = function(md: PByte; c: PSHA3_CTX): Integer; cdecl;
  TSHA3_384 = function(const d: PByte; n: NativeUInt; md: PByte): PByte; cdecl;
  
  // SHA3-512 functions
  TSHA3_512_Init = function(c: PSHA3_CTX): Integer; cdecl;
  TSHA3_512_Update = function(c: PSHA3_CTX; const data: Pointer; len: NativeUInt): Integer; cdecl;
  TSHA3_512_Final = function(md: PByte; c: PSHA3_CTX): Integer; cdecl;
  TSHA3_512 = function(const d: PByte; n: NativeUInt; md: PByte): PByte; cdecl;
  
  // SHAKE128 functions
  TSHAKE128_Init = function(c: PSHAKE_CTX): Integer; cdecl;
  TSHAKE128_Update = function(c: PSHAKE_CTX; const data: Pointer; len: NativeUInt): Integer; cdecl;
  TSHAKE128_Final = function(md: PByte; c: PSHAKE_CTX): Integer; cdecl;
  TSHAKE128_FinalXOF = function(c: PSHAKE_CTX): Integer; cdecl;
  TSHAKE128_Squeeze = function(c: PSHAKE_CTX; out_: PByte; outlen: NativeUInt): Integer; cdecl;
  TSHAKE128 = function(const d: PByte; n: NativeUInt; md: PByte; mdlen: NativeUInt): PByte; cdecl;
  
  // SHAKE256 functions
  TSHAKE256_Init = function(c: PSHAKE_CTX): Integer; cdecl;
  TSHAKE256_Update = function(c: PSHAKE_CTX; const data: Pointer; len: NativeUInt): Integer; cdecl;
  TSHAKE256_Final = function(md: PByte; c: PSHAKE_CTX): Integer; cdecl;
  TSHAKE256_FinalXOF = function(c: PSHAKE_CTX): Integer; cdecl;
  TSHAKE256_Squeeze = function(c: PSHAKE_CTX; out_: PByte; outlen: NativeUInt): Integer; cdecl;
  TSHAKE256 = function(const d: PByte; n: NativeUInt; md: PByte; mdlen: NativeUInt): PByte; cdecl;
  
  // KMAC functions (Keccak Message Authentication Code)
  TKMAC128 = function(const key: PByte; keylen: NativeUInt; const custom: PByte; customlen: NativeUInt;
                      const msg: PByte; msglen: NativeUInt; out_: PByte; outlen: NativeUInt): Integer; cdecl;
  TKMAC256 = function(const key: PByte; keylen: NativeUInt; const custom: PByte; customlen: NativeUInt;
                      const msg: PByte; msglen: NativeUInt; out_: PByte; outlen: NativeUInt): Integer; cdecl;

var
  // SHA3-224 function pointers
  SHA3_224_Init: TSHA3_224_Init = nil;
  SHA3_224_Update: TSHA3_224_Update = nil;
  SHA3_224_Final: TSHA3_224_Final = nil;
  SHA3_224: TSHA3_224 = nil;
  
  // SHA3-256 function pointers
  SHA3_256_Init: TSHA3_256_Init = nil;
  SHA3_256_Update: TSHA3_256_Update = nil;
  SHA3_256_Final: TSHA3_256_Final = nil;
  SHA3_256: TSHA3_256 = nil;
  
  // SHA3-384 function pointers
  SHA3_384_Init: TSHA3_384_Init = nil;
  SHA3_384_Update: TSHA3_384_Update = nil;
  SHA3_384_Final: TSHA3_384_Final = nil;
  SHA3_384: TSHA3_384 = nil;
  
  // SHA3-512 function pointers
  SHA3_512_Init: TSHA3_512_Init = nil;
  SHA3_512_Update: TSHA3_512_Update = nil;
  SHA3_512_Final: TSHA3_512_Final = nil;
  SHA3_512: TSHA3_512 = nil;
  
  // SHAKE128 function pointers
  SHAKE128_Init: TSHAKE128_Init = nil;
  SHAKE128_Update: TSHAKE128_Update = nil;
  SHAKE128_Final: TSHAKE128_Final = nil;
  SHAKE128_FinalXOF: TSHAKE128_FinalXOF = nil;
  SHAKE128_Squeeze: TSHAKE128_Squeeze = nil;
  SHAKE128: TSHAKE128 = nil;
  
  // SHAKE256 function pointers
  SHAKE256_Init: TSHAKE256_Init = nil;
  SHAKE256_Update: TSHAKE256_Update = nil;
  SHAKE256_Final: TSHAKE256_Final = nil;
  SHAKE256_FinalXOF: TSHAKE256_FinalXOF = nil;
  SHAKE256_Squeeze: TSHAKE256_Squeeze = nil;
  SHAKE256: TSHAKE256 = nil;
  
  // KMAC function pointers
  KMAC128: TKMAC128 = nil;
  KMAC256: TKMAC256 = nil;

// Helper functions
function LoadSHA3Functions(ALibHandle: THandle): Boolean;
procedure UnloadSHA3Functions;
function IsSHA3Loaded: Boolean;

// High-level helper functions
function SHA3_224Hash(const Data: TBytes): TBytes;
function SHA3_256Hash(const Data: TBytes): TBytes;
function SHA3_384Hash(const Data: TBytes): TBytes;
function SHA3_512Hash(const Data: TBytes): TBytes;
function SHAKE128Hash(const Data: TBytes; OutLen: Integer): TBytes;
function SHAKE256Hash(const Data: TBytes; OutLen: Integer): TBytes;

implementation

uses
  fafafa.ssl.openssl.api;

var
  GSHA3Loaded: Boolean = False;

function LoadSHA3Functions(ALibHandle: THandle): Boolean;
begin
  Result := False;
  
  if ALibHandle = 0 then Exit;
  
  // Load SHA3-224 functions
  SHA3_224_Init := GetProcAddress(ALibHandle, 'SHA3_224_Init');
  SHA3_224_Update := GetProcAddress(ALibHandle, 'SHA3_224_Update');
  SHA3_224_Final := GetProcAddress(ALibHandle, 'SHA3_224_Final');
  SHA3_224 := GetProcAddress(ALibHandle, 'SHA3_224');
  
  // Load SHA3-256 functions
  SHA3_256_Init := GetProcAddress(ALibHandle, 'SHA3_256_Init');
  SHA3_256_Update := GetProcAddress(ALibHandle, 'SHA3_256_Update');
  SHA3_256_Final := GetProcAddress(ALibHandle, 'SHA3_256_Final');
  SHA3_256 := GetProcAddress(ALibHandle, 'SHA3_256');
  
  // Load SHA3-384 functions
  SHA3_384_Init := GetProcAddress(ALibHandle, 'SHA3_384_Init');
  SHA3_384_Update := GetProcAddress(ALibHandle, 'SHA3_384_Update');
  SHA3_384_Final := GetProcAddress(ALibHandle, 'SHA3_384_Final');
  SHA3_384 := GetProcAddress(ALibHandle, 'SHA3_384');
  
  // Load SHA3-512 functions
  SHA3_512_Init := GetProcAddress(ALibHandle, 'SHA3_512_Init');
  SHA3_512_Update := GetProcAddress(ALibHandle, 'SHA3_512_Update');
  SHA3_512_Final := GetProcAddress(ALibHandle, 'SHA3_512_Final');
  SHA3_512 := GetProcAddress(ALibHandle, 'SHA3_512');
  
  // Load SHAKE128 functions
  SHAKE128_Init := GetProcAddress(ALibHandle, 'SHAKE128_Init');
  SHAKE128_Update := GetProcAddress(ALibHandle, 'SHAKE128_Update');
  SHAKE128_Final := GetProcAddress(ALibHandle, 'SHAKE128_Final');
  SHAKE128_FinalXOF := GetProcAddress(ALibHandle, 'SHAKE128_FinalXOF');
  SHAKE128_Squeeze := GetProcAddress(ALibHandle, 'SHAKE128_Squeeze');
  SHAKE128 := GetProcAddress(ALibHandle, 'SHAKE128');
  
  // Load SHAKE256 functions
  SHAKE256_Init := GetProcAddress(ALibHandle, 'SHAKE256_Init');
  SHAKE256_Update := GetProcAddress(ALibHandle, 'SHAKE256_Update');
  SHAKE256_Final := GetProcAddress(ALibHandle, 'SHAKE256_Final');
  SHAKE256_FinalXOF := GetProcAddress(ALibHandle, 'SHAKE256_FinalXOF');
  SHAKE256_Squeeze := GetProcAddress(ALibHandle, 'SHAKE256_Squeeze');
  SHAKE256 := GetProcAddress(ALibHandle, 'SHAKE256');
  
  // Load KMAC functions
  KMAC128 := GetProcAddress(ALibHandle, 'KMAC128');
  KMAC256 := GetProcAddress(ALibHandle, 'KMAC256');
  
  GSHA3Loaded := True;
  Result := True;
end;

procedure UnloadSHA3Functions;
begin
  SHA3_224_Init := nil;
  SHA3_224_Update := nil;
  SHA3_224_Final := nil;
  SHA3_224 := nil;
  
  SHA3_256_Init := nil;
  SHA3_256_Update := nil;
  SHA3_256_Final := nil;
  SHA3_256 := nil;
  
  SHA3_384_Init := nil;
  SHA3_384_Update := nil;
  SHA3_384_Final := nil;
  SHA3_384 := nil;
  
  SHA3_512_Init := nil;
  SHA3_512_Update := nil;
  SHA3_512_Final := nil;
  SHA3_512 := nil;
  
  SHAKE128_Init := nil;
  SHAKE128_Update := nil;
  SHAKE128_Final := nil;
  SHAKE128_FinalXOF := nil;
  SHAKE128_Squeeze := nil;
  SHAKE128 := nil;
  
  SHAKE256_Init := nil;
  SHAKE256_Update := nil;
  SHAKE256_Final := nil;
  SHAKE256_FinalXOF := nil;
  SHAKE256_Squeeze := nil;
  SHAKE256 := nil;
  
  KMAC128 := nil;
  KMAC256 := nil;
  
  GSHA3Loaded := False;
end;

function IsSHA3Loaded: Boolean;
begin
  Result := GSHA3Loaded;
end;

function SHA3_224Hash(const Data: TBytes): TBytes;
var
  ctx: SHA3_CTX;
begin
  SetLength(Result, SHA3_224_DIGEST_LENGTH);
  if Assigned(SHA3_224_Init) and Assigned(SHA3_224_Update) and Assigned(SHA3_224_Final) then
  begin
    SHA3_224_Init(@ctx);
    if Length(Data) > 0 then
      SHA3_224_Update(@ctx, @Data[0], Length(Data));
    SHA3_224_Final(@Result[0], @ctx);
  end
  else
    FillChar(Result[0], SHA3_224_DIGEST_LENGTH, 0);
end;

function SHA3_256Hash(const Data: TBytes): TBytes;
var
  ctx: SHA3_CTX;
begin
  SetLength(Result, SHA3_256_DIGEST_LENGTH);
  if Assigned(SHA3_256_Init) and Assigned(SHA3_256_Update) and Assigned(SHA3_256_Final) then
  begin
    SHA3_256_Init(@ctx);
    if Length(Data) > 0 then
      SHA3_256_Update(@ctx, @Data[0], Length(Data));
    SHA3_256_Final(@Result[0], @ctx);
  end
  else
    FillChar(Result[0], SHA3_256_DIGEST_LENGTH, 0);
end;

function SHA3_384Hash(const Data: TBytes): TBytes;
var
  ctx: SHA3_CTX;
begin
  SetLength(Result, SHA3_384_DIGEST_LENGTH);
  if Assigned(SHA3_384_Init) and Assigned(SHA3_384_Update) and Assigned(SHA3_384_Final) then
  begin
    SHA3_384_Init(@ctx);
    if Length(Data) > 0 then
      SHA3_384_Update(@ctx, @Data[0], Length(Data));
    SHA3_384_Final(@Result[0], @ctx);
  end
  else
    FillChar(Result[0], SHA3_384_DIGEST_LENGTH, 0);
end;

function SHA3_512Hash(const Data: TBytes): TBytes;
var
  ctx: SHA3_CTX;
begin
  SetLength(Result, SHA3_512_DIGEST_LENGTH);
  if Assigned(SHA3_512_Init) and Assigned(SHA3_512_Update) and Assigned(SHA3_512_Final) then
  begin
    SHA3_512_Init(@ctx);
    if Length(Data) > 0 then
      SHA3_512_Update(@ctx, @Data[0], Length(Data));
    SHA3_512_Final(@Result[0], @ctx);
  end
  else
    FillChar(Result[0], SHA3_512_DIGEST_LENGTH, 0);
end;

function SHAKE128Hash(const Data: TBytes; OutLen: Integer): TBytes;
var
  ctx: SHAKE_CTX;
begin
  SetLength(Result, OutLen);
  if Assigned(SHAKE128_Init) and Assigned(SHAKE128_Update) and 
    Assigned(SHAKE128_FinalXOF) and Assigned(SHAKE128_Squeeze) then
  begin
    SHAKE128_Init(@ctx);
    if Length(Data) > 0 then
      SHAKE128_Update(@ctx, @Data[0], Length(Data));
    SHAKE128_FinalXOF(@ctx);
    SHAKE128_Squeeze(@ctx, @Result[0], OutLen);
  end
  else
    FillChar(Result[0], OutLen, 0);
end;

function SHAKE256Hash(const Data: TBytes; OutLen: Integer): TBytes;
var
  ctx: SHAKE_CTX;
begin
  SetLength(Result, OutLen);
  if Assigned(SHAKE256_Init) and Assigned(SHAKE256_Update) and 
    Assigned(SHAKE256_FinalXOF) and Assigned(SHAKE256_Squeeze) then
  begin
    SHAKE256_Init(@ctx);
    if Length(Data) > 0 then
      SHAKE256_Update(@ctx, @Data[0], Length(Data));
    SHAKE256_FinalXOF(@ctx);
    SHAKE256_Squeeze(@ctx, @Result[0], OutLen);
  end
  else
    FillChar(Result[0], OutLen, 0);
end;

end.