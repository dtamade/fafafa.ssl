{******************************************************************************}
{                                                                              }
{  fafafa.ssl - OpenSSL MD (Message Digest) Module                           }
{                                                                              }
{  Copyright (c) 2024 fafafa                                                  }
{                                                                              }
{******************************************************************************}

unit fafafa.ssl.openssl.api.md;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api.consts;

const
  MD4_DIGEST_LENGTH = 16;
  MD4_CBLOCK = 64;
  MD4_LBLOCK = 16;
  
  MD5_DIGEST_LENGTH = 16;
  MD5_CBLOCK = 64;
  MD5_LBLOCK = 16;
  
  MDC2_BLOCK = 8;
  MDC2_DIGEST_LENGTH = 16;
  
  RIPEMD160_DIGEST_LENGTH = 20;
  RIPEMD160_CBLOCK = 64;
  RIPEMD160_LBLOCK = 16;

type
  // DES types needed for MDC2
  DES_cblock = array[0..7] of Byte;
  // MD4 context
  PMD4_CTX = ^MD4_CTX;
  MD4_CTX = record
    A, B, C, D: Cardinal;
    Nl, Nh: Cardinal;
    data: array[0..MD4_LBLOCK-1] of Cardinal;
    num: Cardinal;
  end;
  
  // MD5 context
  PMD5_CTX = ^MD5_CTX;
  MD5_CTX = record
    A, B, C, D: Cardinal;
    Nl, Nh: Cardinal;
    data: array[0..MD5_LBLOCK-1] of Cardinal;
    num: Cardinal;
  end;
  
  // MDC2 context
  PMDC2_CTX = ^MDC2_CTX;
  MDC2_CTX = record
    num: Cardinal;
    data: array[0..MDC2_BLOCK-1] of Byte;
    h: DES_cblock;
    hh: DES_cblock;
    pad_type: Integer;
  end;
  
  // RIPEMD160 context
  PRIPEMD160_CTX = ^RIPEMD160_CTX;
  RIPEMD160_CTX = record
    A, B, C, D, E: Cardinal;
    Nl, Nh: Cardinal;
    data: array[0..RIPEMD160_LBLOCK-1] of Cardinal;
    num: Cardinal;
  end;

type
  // MD4 functions
  TMD4_Init = function(c: PMD4_CTX): Integer; cdecl;
  TMD4_Update = function(c: PMD4_CTX; const data: Pointer; len: NativeUInt): Integer; cdecl;
  TMD4_Final = function(md: PByte; c: PMD4_CTX): Integer; cdecl;
  TMD4 = function(const d: PByte; n: NativeUInt; md: PByte): PByte; cdecl;
  TMD4_Transform = procedure(c: PMD4_CTX; const b: PByte); cdecl;
  
  // MD5 functions
  TMD5_Init = function(c: PMD5_CTX): Integer; cdecl;
  TMD5_Update = function(c: PMD5_CTX; const data: Pointer; len: NativeUInt): Integer; cdecl;
  TMD5_Final = function(md: PByte; c: PMD5_CTX): Integer; cdecl;
  TMD5 = function(const d: PByte; n: NativeUInt; md: PByte): PByte; cdecl;
  TMD5_Transform = procedure(c: PMD5_CTX; const b: PByte); cdecl;
  
  // MDC2 functions
  TMDC2_Init = function(c: PMDC2_CTX): Integer; cdecl;
  TMDC2_Update = function(c: PMDC2_CTX; const data: Pointer; len: NativeUInt): Integer; cdecl;
  TMDC2_Final = function(md: PByte; c: PMDC2_CTX): Integer; cdecl;
  TMDC2 = function(const d: PByte; n: NativeUInt; md: PByte): PByte; cdecl;
  
  // RIPEMD160 functions
  TRIPEMD160_Init = function(c: PRIPEMD160_CTX): Integer; cdecl;
  TRIPEMD160_Update = function(c: PRIPEMD160_CTX; const data: Pointer; len: NativeUInt): Integer; cdecl;
  TRIPEMD160_Final = function(md: PByte; c: PRIPEMD160_CTX): Integer; cdecl;
  TRIPEMD160 = function(const d: PByte; n: NativeUInt; md: PByte): PByte; cdecl;
  TRIPEMD160_Transform = procedure(c: PRIPEMD160_CTX; const b: PByte); cdecl;

var
  // MD4 function pointers
  MD4_Init: TMD4_Init = nil;
  MD4_Update: TMD4_Update = nil;
  MD4_Final: TMD4_Final = nil;
  MD4: TMD4 = nil;
  MD4_Transform: TMD4_Transform = nil;
  
  // MD5 function pointers
  MD5_Init: TMD5_Init = nil;
  MD5_Update: TMD5_Update = nil;
  MD5_Final: TMD5_Final = nil;
  MD5: TMD5 = nil;
  MD5_Transform: TMD5_Transform = nil;
  
  // MDC2 function pointers
  MDC2_Init: TMDC2_Init = nil;
  MDC2_Update: TMDC2_Update = nil;
  MDC2_Final: TMDC2_Final = nil;
  MDC2: TMDC2 = nil;
  
  // RIPEMD160 function pointers
  RIPEMD160_Init: TRIPEMD160_Init = nil;
  RIPEMD160_Update: TRIPEMD160_Update = nil;
  RIPEMD160_Final: TRIPEMD160_Final = nil;
  RIPEMD160: TRIPEMD160 = nil;
  RIPEMD160_Transform: TRIPEMD160_Transform = nil;

// Helper functions
function LoadMDFunctions(ALibHandle: THandle): Boolean;
procedure UnloadMDFunctions;
function IsMDLoaded: Boolean;

// High-level helper functions
function MD4Hash(const Data: TBytes): TBytes;
function MD5Hash(const Data: TBytes): TBytes;
function MDC2Hash(const Data: TBytes): TBytes;
function RIPEMD160Hash(const Data: TBytes): TBytes;
function MD4HashString(const S: string): string;
function MD5HashString(const S: string): string;
function MDC2HashString(const S: string): string;
function RIPEMD160HashString(const S: string): string;

implementation

uses
  fafafa.ssl.openssl.api;

var
  GMDLoaded: Boolean = False;

function LoadMDFunctions(ALibHandle: THandle): Boolean;
begin
  Result := False;
  
  if ALibHandle = 0 then Exit;
  
  // Load MD4 functions
  Pointer(MD4_Init) := GetProcAddress(ALibHandle, 'MD4_Init');
  Pointer(MD4_Update) := GetProcAddress(ALibHandle, 'MD4_Update');
  Pointer(MD4_Final) := GetProcAddress(ALibHandle, 'MD4_Final');
  Pointer(MD4) := GetProcAddress(ALibHandle, 'MD4');
  Pointer(MD4_Transform) := GetProcAddress(ALibHandle, 'MD4_Transform');
  
  // Load MD5 functions
  Pointer(MD5_Init) := GetProcAddress(ALibHandle, 'MD5_Init');
  Pointer(MD5_Update) := GetProcAddress(ALibHandle, 'MD5_Update');
  Pointer(MD5_Final) := GetProcAddress(ALibHandle, 'MD5_Final');
  Pointer(MD5) := GetProcAddress(ALibHandle, 'MD5');
  Pointer(MD5_Transform) := GetProcAddress(ALibHandle, 'MD5_Transform');
  
  // Load MDC2 functions
  Pointer(MDC2_Init) := GetProcAddress(ALibHandle, 'MDC2_Init');
  Pointer(MDC2_Update) := GetProcAddress(ALibHandle, 'MDC2_Update');
  Pointer(MDC2_Final) := GetProcAddress(ALibHandle, 'MDC2_Final');
  Pointer(MDC2) := GetProcAddress(ALibHandle, 'MDC2');
  
  // Load RIPEMD160 functions
  Pointer(RIPEMD160_Init) := GetProcAddress(ALibHandle, 'RIPEMD160_Init');
  Pointer(RIPEMD160_Update) := GetProcAddress(ALibHandle, 'RIPEMD160_Update');
  Pointer(RIPEMD160_Final) := GetProcAddress(ALibHandle, 'RIPEMD160_Final');
  Pointer(RIPEMD160) := GetProcAddress(ALibHandle, 'RIPEMD160');
  Pointer(RIPEMD160_Transform) := GetProcAddress(ALibHandle, 'RIPEMD160_Transform');
  
  GMDLoaded := True;
  Result := True;
end;

procedure UnloadMDFunctions;
begin
  MD4_Init := nil;
  MD4_Update := nil;
  MD4_Final := nil;
  MD4 := nil;
  MD4_Transform := nil;
  
  MD5_Init := nil;
  MD5_Update := nil;
  MD5_Final := nil;
  MD5 := nil;
  MD5_Transform := nil;
  
  MDC2_Init := nil;
  MDC2_Update := nil;
  MDC2_Final := nil;
  MDC2 := nil;
  
  RIPEMD160_Init := nil;
  RIPEMD160_Update := nil;
  RIPEMD160_Final := nil;
  RIPEMD160 := nil;
  RIPEMD160_Transform := nil;
  
  GMDLoaded := False;
end;

function IsMDLoaded: Boolean;
begin
  Result := GMDLoaded;
end;

// Helper function implementations
function MD4Hash(const Data: TBytes): TBytes;
var
  ctx: MD4_CTX;
begin
  SetLength(Result, MD4_DIGEST_LENGTH);
  if Assigned(MD4_Init) and Assigned(MD4_Update) and Assigned(MD4_Final) then
  begin
    MD4_Init(@ctx);
    if Length(Data) > 0 then
      MD4_Update(@ctx, @Data[0], Length(Data));
    MD4_Final(@Result[0], @ctx);
  end
  else
    FillChar(Result[0], MD4_DIGEST_LENGTH, 0);
end;

function MD5Hash(const Data: TBytes): TBytes;
var
  ctx: MD5_CTX;
begin
  SetLength(Result, MD5_DIGEST_LENGTH);
  if Assigned(MD5_Init) and Assigned(MD5_Update) and Assigned(MD5_Final) then
  begin
    MD5_Init(@ctx);
    if Length(Data) > 0 then
      MD5_Update(@ctx, @Data[0], Length(Data));
    MD5_Final(@Result[0], @ctx);
  end
  else
    FillChar(Result[0], MD5_DIGEST_LENGTH, 0);
end;

function MDC2Hash(const Data: TBytes): TBytes;
var
  ctx: MDC2_CTX;
begin
  SetLength(Result, MDC2_DIGEST_LENGTH);
  if Assigned(MDC2_Init) and Assigned(MDC2_Update) and Assigned(MDC2_Final) then
  begin
    MDC2_Init(@ctx);
    if Length(Data) > 0 then
      MDC2_Update(@ctx, @Data[0], Length(Data));
    MDC2_Final(@Result[0], @ctx);
  end
  else
    FillChar(Result[0], MDC2_DIGEST_LENGTH, 0);
end;

function RIPEMD160Hash(const Data: TBytes): TBytes;
var
  ctx: RIPEMD160_CTX;
begin
  SetLength(Result, RIPEMD160_DIGEST_LENGTH);
  if Assigned(RIPEMD160_Init) and Assigned(RIPEMD160_Update) and Assigned(RIPEMD160_Final) then
  begin
    RIPEMD160_Init(@ctx);
    if Length(Data) > 0 then
      RIPEMD160_Update(@ctx, @Data[0], Length(Data));
    RIPEMD160_Final(@Result[0], @ctx);
  end
  else
    FillChar(Result[0], RIPEMD160_DIGEST_LENGTH, 0);
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

function MD4HashString(const S: string): string;
var
  Data: TBytes;
begin
  Data := TEncoding.UTF8.GetBytes(S);
  Result := BytesToHex(MD4Hash(Data));
end;

function MD5HashString(const S: string): string;
var
  Data: TBytes;
begin
  Data := TEncoding.UTF8.GetBytes(S);
  Result := BytesToHex(MD5Hash(Data));
end;

function MDC2HashString(const S: string): string;
var
  Data: TBytes;
begin
  Data := TEncoding.UTF8.GetBytes(S);
  Result := BytesToHex(MDC2Hash(Data));
end;

function RIPEMD160HashString(const S: string): string;
var
  Data: TBytes;
begin
  Data := TEncoding.UTF8.GetBytes(S);
  Result := BytesToHex(RIPEMD160Hash(Data));
end;

end.
