{******************************************************************************}
{                                                                              }
{  fafafa.ssl - OpenSSL SHA Module                                           }
{                                                                              }
{  Copyright (c) 2024 fafafa                                                  }
{                                                                              }
{******************************************************************************}

unit fafafa.ssl.openssl.api.sha;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api.consts;

const
  SHA_DIGEST_LENGTH = 20;
  SHA_LBLOCK = 16;
  SHA_CBLOCK = 64;
  
  SHA224_DIGEST_LENGTH = 28;
  SHA256_DIGEST_LENGTH = 32;
  SHA256_CBLOCK = 64;
  
  SHA384_DIGEST_LENGTH = 48;
  SHA512_DIGEST_LENGTH = 64;
  SHA512_CBLOCK = 128;
  
  SHA512_224_DIGEST_LENGTH = 28;
  SHA512_256_DIGEST_LENGTH = 32;

type
  // SHA-1 context
  PSHA_CTX = ^SHA_CTX;
  SHA_CTX = record
    h0, h1, h2, h3, h4: Cardinal;
    Nl, Nh: Cardinal;
    data: array[0..SHA_LBLOCK-1] of Cardinal;
    num: Cardinal;
  end;
  
  // SHA-256 context
  PSHA256_CTX = ^SHA256_CTX;
  SHA256_CTX = record
    h: array[0..7] of Cardinal;
    Nl, Nh: Cardinal;
    data: array[0..SHA_LBLOCK-1] of Cardinal;
    num: Cardinal;
    md_len: Cardinal;
  end;
  
  // SHA-512 context
  PSHA512_CTX = ^SHA512_CTX;
  SHA512_CTX = record
    h: array[0..7] of UInt64;
    Nl, Nh: UInt64;
    u: record
      case Integer of
        0: (d: array[0..SHA_LBLOCK-1] of UInt64);
        1: (p: array[0..SHA512_CBLOCK-1] of Byte);
    end;
    num: Cardinal;
    md_len: Cardinal;
  end;
  
  // SHA-224 uses SHA256_CTX
  SHA224_CTX = SHA256_CTX;
  PSHA224_CTX = ^SHA224_CTX;
  
  // SHA-384 uses SHA512_CTX
  SHA384_CTX = SHA512_CTX;
  PSHA384_CTX = ^SHA384_CTX;
  
  // SHA-512/224 uses SHA512_CTX
  SHA512_224_CTX = SHA512_CTX;
  PSHA512_224_CTX = ^SHA512_224_CTX;
  
  // SHA-512/256 uses SHA512_CTX
  SHA512_256_CTX = SHA512_CTX;
  PSHA512_256_CTX = ^SHA512_256_CTX;

type
  // SHA-1 functions
  TSHA1_Init = function(c: PSHA_CTX): Integer; cdecl;
  TSHA1_Update = function(c: PSHA_CTX; const data: Pointer; len: NativeUInt): Integer; cdecl;
  TSHA1_Final = function(md: PByte; c: PSHA_CTX): Integer; cdecl;
  TSHA1 = function(const d: PByte; n: NativeUInt; md: PByte): PByte; cdecl;
  TSHA1_Transform = procedure(c: PSHA_CTX; const data: PByte); cdecl;
  
  // SHA-224 functions
  TSHA224_Init = function(c: PSHA256_CTX): Integer; cdecl;
  TSHA224_Update = function(c: PSHA256_CTX; const data: Pointer; len: NativeUInt): Integer; cdecl;
  TSHA224_Final = function(md: PByte; c: PSHA256_CTX): Integer; cdecl;
  TSHA224 = function(const d: PByte; n: NativeUInt; md: PByte): PByte; cdecl;
  
  // SHA-256 functions
  TSHA256_Init = function(c: PSHA256_CTX): Integer; cdecl;
  TSHA256_Update = function(c: PSHA256_CTX; const data: Pointer; len: NativeUInt): Integer; cdecl;
  TSHA256_Final = function(md: PByte; c: PSHA256_CTX): Integer; cdecl;
  TSHA256 = function(const d: PByte; n: NativeUInt; md: PByte): PByte; cdecl;
  TSHA256_Transform = procedure(c: PSHA256_CTX; const data: PByte); cdecl;
  
  // SHA-384 functions
  TSHA384_Init = function(c: PSHA512_CTX): Integer; cdecl;
  TSHA384_Update = function(c: PSHA512_CTX; const data: Pointer; len: NativeUInt): Integer; cdecl;
  TSHA384_Final = function(md: PByte; c: PSHA512_CTX): Integer; cdecl;
  TSHA384 = function(const d: PByte; n: NativeUInt; md: PByte): PByte; cdecl;
  
  // SHA-512 functions
  TSHA512_Init = function(c: PSHA512_CTX): Integer; cdecl;
  TSHA512_Update = function(c: PSHA512_CTX; const data: Pointer; len: NativeUInt): Integer; cdecl;
  TSHA512_Final = function(md: PByte; c: PSHA512_CTX): Integer; cdecl;
  TSHA512 = function(const d: PByte; n: NativeUInt; md: PByte): PByte; cdecl;
  TSHA512_Transform = procedure(c: PSHA512_CTX; const data: PByte); cdecl;
  
  // SHA-512/224 functions
  TSHA512_224_Init = function(c: PSHA512_CTX): Integer; cdecl;
  TSHA512_224_Update = function(c: PSHA512_CTX; const data: Pointer; len: NativeUInt): Integer; cdecl;
  TSHA512_224_Final = function(md: PByte; c: PSHA512_CTX): Integer; cdecl;
  TSHA512_224 = function(const d: PByte; n: NativeUInt; md: PByte): PByte; cdecl;
  
  // SHA-512/256 functions
  TSHA512_256_Init = function(c: PSHA512_CTX): Integer; cdecl;
  TSHA512_256_Update = function(c: PSHA512_CTX; const data: Pointer; len: NativeUInt): Integer; cdecl;
  TSHA512_256_Final = function(md: PByte; c: PSHA512_CTX): Integer; cdecl;
  TSHA512_256 = function(const d: PByte; n: NativeUInt; md: PByte): PByte; cdecl;

var
  // SHA-1 function pointers
  SHA1_Init: TSHA1_Init = nil;
  SHA1_Update: TSHA1_Update = nil;
  SHA1_Final: TSHA1_Final = nil;
  SHA1: TSHA1 = nil;
  SHA1_Transform: TSHA1_Transform = nil;
  
  // SHA-224 function pointers
  SHA224_Init: TSHA224_Init = nil;
  SHA224_Update: TSHA224_Update = nil;
  SHA224_Final: TSHA224_Final = nil;
  SHA224: TSHA224 = nil;
  
  // SHA-256 function pointers
  SHA256_Init: TSHA256_Init = nil;
  SHA256_Update: TSHA256_Update = nil;
  SHA256_Final: TSHA256_Final = nil;
  SHA256: TSHA256 = nil;
  SHA256_Transform: TSHA256_Transform = nil;
  
  // SHA-384 function pointers
  SHA384_Init: TSHA384_Init = nil;
  SHA384_Update: TSHA384_Update = nil;
  SHA384_Final: TSHA384_Final = nil;
  SHA384: TSHA384 = nil;
  
  // SHA-512 function pointers
  SHA512_Init: TSHA512_Init = nil;
  SHA512_Update: TSHA512_Update = nil;
  SHA512_Final: TSHA512_Final = nil;
  SHA512: TSHA512 = nil;
  SHA512_Transform: TSHA512_Transform = nil;
  
  // SHA-512/224 function pointers
  SHA512_224_Init: TSHA512_224_Init = nil;
  SHA512_224_Update: TSHA512_224_Update = nil;
  SHA512_224_Final: TSHA512_224_Final = nil;
  SHA512_224: TSHA512_224 = nil;
  
  // SHA-512/256 function pointers
  SHA512_256_Init: TSHA512_256_Init = nil;
  SHA512_256_Update: TSHA512_256_Update = nil;
  SHA512_256_Final: TSHA512_256_Final = nil;
  SHA512_256: TSHA512_256 = nil;

// Helper functions
function LoadSHAFunctions(ALibHandle: THandle): Boolean;
procedure UnloadSHAFunctions;
function IsSHALoaded: Boolean;

// High-level helper functions
function SHA1Hash(const Data: TBytes): TBytes;
function SHA256Hash(const Data: TBytes): TBytes;
function SHA384Hash(const Data: TBytes): TBytes;
function SHA512Hash(const Data: TBytes): TBytes;
function SHA1HashString(const S: string): string;
function SHA256HashString(const S: string): string;
function SHA384HashString(const S: string): string;
function SHA512HashString(const S: string): string;

implementation

uses
  fafafa.ssl.openssl.api;

var
  GSHALoaded: Boolean = False;

function LoadSHAFunctions(ALibHandle: THandle): Boolean;
begin
  Result := False;
  
  if ALibHandle = 0 then Exit;
  
  // Load SHA-1 functions
  Pointer(SHA1_Init) := GetProcAddress(ALibHandle, 'SHA1_Init');
  Pointer(SHA1_Update) := GetProcAddress(ALibHandle, 'SHA1_Update');
  Pointer(SHA1_Final) := GetProcAddress(ALibHandle, 'SHA1_Final');
  Pointer(SHA1) := GetProcAddress(ALibHandle, 'SHA1');
  Pointer(SHA1_Transform) := GetProcAddress(ALibHandle, 'SHA1_Transform');
  
  // Load SHA-224 functions
  Pointer(SHA224_Init) := GetProcAddress(ALibHandle, 'SHA224_Init');
  Pointer(SHA224_Update) := GetProcAddress(ALibHandle, 'SHA224_Update');
  Pointer(SHA224_Final) := GetProcAddress(ALibHandle, 'SHA224_Final');
  Pointer(SHA224) := GetProcAddress(ALibHandle, 'SHA224');
  
  // Load SHA-256 functions
  Pointer(SHA256_Init) := GetProcAddress(ALibHandle, 'SHA256_Init');
  Pointer(SHA256_Update) := GetProcAddress(ALibHandle, 'SHA256_Update');
  Pointer(SHA256_Final) := GetProcAddress(ALibHandle, 'SHA256_Final');
  Pointer(SHA256) := GetProcAddress(ALibHandle, 'SHA256');
  Pointer(SHA256_Transform) := GetProcAddress(ALibHandle, 'SHA256_Transform');
  
  // Load SHA-384 functions
  Pointer(SHA384_Init) := GetProcAddress(ALibHandle, 'SHA384_Init');
  Pointer(SHA384_Update) := GetProcAddress(ALibHandle, 'SHA384_Update');
  Pointer(SHA384_Final) := GetProcAddress(ALibHandle, 'SHA384_Final');
  Pointer(SHA384) := GetProcAddress(ALibHandle, 'SHA384');
  
  // Load SHA-512 functions
  Pointer(SHA512_Init) := GetProcAddress(ALibHandle, 'SHA512_Init');
  Pointer(SHA512_Update) := GetProcAddress(ALibHandle, 'SHA512_Update');
  Pointer(SHA512_Final) := GetProcAddress(ALibHandle, 'SHA512_Final');
  Pointer(SHA512) := GetProcAddress(ALibHandle, 'SHA512');
  Pointer(SHA512_Transform) := GetProcAddress(ALibHandle, 'SHA512_Transform');
  
  // Load SHA-512/224 functions
  Pointer(SHA512_224_Init) := GetProcAddress(ALibHandle, 'SHA512_224_Init');
  Pointer(SHA512_224_Update) := GetProcAddress(ALibHandle, 'SHA512_224_Update');
  Pointer(SHA512_224_Final) := GetProcAddress(ALibHandle, 'SHA512_224_Final');
  Pointer(SHA512_224) := GetProcAddress(ALibHandle, 'SHA512_224');
  
  // Load SHA-512/256 functions
  Pointer(SHA512_256_Init) := GetProcAddress(ALibHandle, 'SHA512_256_Init');
  Pointer(SHA512_256_Update) := GetProcAddress(ALibHandle, 'SHA512_256_Update');
  Pointer(SHA512_256_Final) := GetProcAddress(ALibHandle, 'SHA512_256_Final');
  Pointer(SHA512_256) := GetProcAddress(ALibHandle, 'SHA512_256');
  
  GSHALoaded := True;
  Result := True;
end;

procedure UnloadSHAFunctions;
begin
  SHA1_Init := nil;
  SHA1_Update := nil;
  SHA1_Final := nil;
  SHA1 := nil;
  SHA1_Transform := nil;
  
  SHA224_Init := nil;
  SHA224_Update := nil;
  SHA224_Final := nil;
  SHA224 := nil;
  
  SHA256_Init := nil;
  SHA256_Update := nil;
  SHA256_Final := nil;
  SHA256 := nil;
  SHA256_Transform := nil;
  
  SHA384_Init := nil;
  SHA384_Update := nil;
  SHA384_Final := nil;
  SHA384 := nil;
  
  SHA512_Init := nil;
  SHA512_Update := nil;
  SHA512_Final := nil;
  SHA512 := nil;
  SHA512_Transform := nil;
  
  SHA512_224_Init := nil;
  SHA512_224_Update := nil;
  SHA512_224_Final := nil;
  SHA512_224 := nil;
  
  SHA512_256_Init := nil;
  SHA512_256_Update := nil;
  SHA512_256_Final := nil;
  SHA512_256 := nil;
  
  GSHALoaded := False;
end;

function IsSHALoaded: Boolean;
begin
  Result := GSHALoaded;
end;

// Helper function implementations
function SHA1Hash(const Data: TBytes): TBytes;
var
  ctx: SHA_CTX;
begin
  SetLength(Result, SHA_DIGEST_LENGTH);
  if Assigned(SHA1_Init) and Assigned(SHA1_Update) and Assigned(SHA1_Final) then
  begin
    SHA1_Init(@ctx);
    if Length(Data) > 0 then
      SHA1_Update(@ctx, @Data[0], Length(Data));
    SHA1_Final(@Result[0], @ctx);
  end
  else
    FillChar(Result[0], SHA_DIGEST_LENGTH, 0);
end;

function SHA256Hash(const Data: TBytes): TBytes;
var
  ctx: SHA256_CTX;
begin
  SetLength(Result, SHA256_DIGEST_LENGTH);
  if Assigned(SHA256_Init) and Assigned(SHA256_Update) and Assigned(SHA256_Final) then
  begin
    SHA256_Init(@ctx);
    if Length(Data) > 0 then
      SHA256_Update(@ctx, @Data[0], Length(Data));
    SHA256_Final(@Result[0], @ctx);
  end
  else
    FillChar(Result[0], SHA256_DIGEST_LENGTH, 0);
end;

function SHA384Hash(const Data: TBytes): TBytes;
var
  ctx: SHA512_CTX;
begin
  SetLength(Result, SHA384_DIGEST_LENGTH);
  if Assigned(SHA384_Init) and Assigned(SHA384_Update) and Assigned(SHA384_Final) then
  begin
    SHA384_Init(@ctx);
    if Length(Data) > 0 then
      SHA384_Update(@ctx, @Data[0], Length(Data));
    SHA384_Final(@Result[0], @ctx);
  end
  else
    FillChar(Result[0], SHA384_DIGEST_LENGTH, 0);
end;

function SHA512Hash(const Data: TBytes): TBytes;
var
  ctx: SHA512_CTX;
begin
  SetLength(Result, SHA512_DIGEST_LENGTH);
  if Assigned(SHA512_Init) and Assigned(SHA512_Update) and Assigned(SHA512_Final) then
  begin
    SHA512_Init(@ctx);
    if Length(Data) > 0 then
      SHA512_Update(@ctx, @Data[0], Length(Data));
    SHA512_Final(@Result[0], @ctx);
  end
  else
    FillChar(Result[0], SHA512_DIGEST_LENGTH, 0);
end;

function BytesToHex(const Bytes: TBytes): string;
const
  HexChars: array[0..15] of Char = '0123456789abcdef';
var
  I: Integer;
begin
  SetLength(Result, Length(Bytes) * 2);
  for I := 0 to High(Bytes) do
  begin
    Result[I * 2 + 1] := HexChars[Bytes[I] shr 4];
    Result[I * 2 + 2] := HexChars[Bytes[I] and $0F];
  end;
end;

function SHA1HashString(const S: string): string;
var
  Data: TBytes;
begin
  Data := TEncoding.UTF8.GetBytes(S);
  Result := BytesToHex(SHA1Hash(Data));
end;

function SHA256HashString(const S: string): string;
var
  Data: TBytes;
begin
  Data := TEncoding.UTF8.GetBytes(S);
  Result := BytesToHex(SHA256Hash(Data));
end;

function SHA384HashString(const S: string): string;
var
  Data: TBytes;
begin
  Data := TEncoding.UTF8.GetBytes(S);
  Result := BytesToHex(SHA384Hash(Data));
end;

function SHA512HashString(const S: string): string;
var
  Data: TBytes;
begin
  Data := TEncoding.UTF8.GetBytes(S);
  Result := BytesToHex(SHA512Hash(Data));
end;

end.