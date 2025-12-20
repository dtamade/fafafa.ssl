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
  fafafa.ssl.openssl.api.consts,
  fafafa.ssl.openssl.loader;

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

const
  { SHA3 函数绑定数组 }
  SHA3_FUNCTION_BINDINGS: array[0..27] of TFunctionBinding = (
    // SHA3-224 functions
    (Name: 'SHA3_224_Init';    FuncPtr: @SHA3_224_Init;    Required: False),
    (Name: 'SHA3_224_Update';  FuncPtr: @SHA3_224_Update;  Required: False),
    (Name: 'SHA3_224_Final';   FuncPtr: @SHA3_224_Final;   Required: False),
    (Name: 'SHA3_224';         FuncPtr: @SHA3_224;         Required: False),
    // SHA3-256 functions
    (Name: 'SHA3_256_Init';    FuncPtr: @SHA3_256_Init;    Required: False),
    (Name: 'SHA3_256_Update';  FuncPtr: @SHA3_256_Update;  Required: False),
    (Name: 'SHA3_256_Final';   FuncPtr: @SHA3_256_Final;   Required: False),
    (Name: 'SHA3_256';         FuncPtr: @SHA3_256;         Required: False),
    // SHA3-384 functions
    (Name: 'SHA3_384_Init';    FuncPtr: @SHA3_384_Init;    Required: False),
    (Name: 'SHA3_384_Update';  FuncPtr: @SHA3_384_Update;  Required: False),
    (Name: 'SHA3_384_Final';   FuncPtr: @SHA3_384_Final;   Required: False),
    (Name: 'SHA3_384';         FuncPtr: @SHA3_384;         Required: False),
    // SHA3-512 functions
    (Name: 'SHA3_512_Init';    FuncPtr: @SHA3_512_Init;    Required: False),
    (Name: 'SHA3_512_Update';  FuncPtr: @SHA3_512_Update;  Required: False),
    (Name: 'SHA3_512_Final';   FuncPtr: @SHA3_512_Final;   Required: False),
    (Name: 'SHA3_512';         FuncPtr: @SHA3_512;         Required: False),
    // SHAKE128 functions
    (Name: 'SHAKE128_Init';     FuncPtr: @SHAKE128_Init;     Required: False),
    (Name: 'SHAKE128_Update';   FuncPtr: @SHAKE128_Update;   Required: False),
    (Name: 'SHAKE128_Final';    FuncPtr: @SHAKE128_Final;    Required: False),
    (Name: 'SHAKE128_FinalXOF'; FuncPtr: @SHAKE128_FinalXOF; Required: False),
    (Name: 'SHAKE128_Squeeze';  FuncPtr: @SHAKE128_Squeeze;  Required: False),
    (Name: 'SHAKE128';          FuncPtr: @SHAKE128;          Required: False),
    // SHAKE256 functions
    (Name: 'SHAKE256_Init';     FuncPtr: @SHAKE256_Init;     Required: False),
    (Name: 'SHAKE256_Update';   FuncPtr: @SHAKE256_Update;   Required: False),
    (Name: 'SHAKE256_Final';    FuncPtr: @SHAKE256_Final;    Required: False),
    (Name: 'SHAKE256_FinalXOF'; FuncPtr: @SHAKE256_FinalXOF; Required: False),
    (Name: 'SHAKE256_Squeeze';  FuncPtr: @SHAKE256_Squeeze;  Required: False),
    (Name: 'SHAKE256';          FuncPtr: @SHAKE256;          Required: False)
  );

  { KMAC 函数绑定数组 }
  KMAC_FUNCTION_BINDINGS: array[0..1] of TFunctionBinding = (
    (Name: 'KMAC128'; FuncPtr: @KMAC128; Required: False),
    (Name: 'KMAC256'; FuncPtr: @KMAC256; Required: False)
  );

function LoadSHA3Functions(ALibHandle: THandle): Boolean;
begin
  Result := False;

  if ALibHandle = 0 then Exit;

  // 使用 TOpenSSLLoader.LoadFunctions 批量加载
  TOpenSSLLoader.LoadFunctions(ALibHandle, SHA3_FUNCTION_BINDINGS);
  TOpenSSLLoader.LoadFunctions(ALibHandle, KMAC_FUNCTION_BINDINGS);

  TOpenSSLLoader.SetModuleLoaded(osmSHA3, True);
  Result := True;
end;

procedure UnloadSHA3Functions;
begin
  // 使用 TOpenSSLLoader.ClearFunctions 批量清除
  TOpenSSLLoader.ClearFunctions(SHA3_FUNCTION_BINDINGS);
  TOpenSSLLoader.ClearFunctions(KMAC_FUNCTION_BINDINGS);

  TOpenSSLLoader.SetModuleLoaded(osmSHA3, False);
end;

function IsSHA3Loaded: Boolean;
begin
  Result := TOpenSSLLoader.IsModuleLoaded(osmSHA3);
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