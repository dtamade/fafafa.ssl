{******************************************************************************}
{                                                                              }
{  fafafa.ssl - OpenSSL AES Module                                           }
{                                                                              }
{  Copyright (c) 2024 fafafa                                                  }
{                                                                              }
{******************************************************************************}

unit fafafa.ssl.openssl.api.aes;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api.consts;

const
  AES_MAXNR = 14;
  AES_BLOCK_SIZE = 16;
  
  // AES encryption/decryption mode constants
  C_AES_ENCRYPT = 1;
  C_AES_DECRYPT = 0;

type
  // AES key structure
  PAES_KEY = ^AES_KEY;
  AES_KEY = record
    rd_key: array[0..(4 * (AES_MAXNR + 1))-1] of Cardinal;
    rounds: Integer;
  end;
  
  // AES-GCM context
  Pgcm128_context = ^gcm128_context;
  gcm128_context = record end; // Opaque structure
  
  // AES-XTS context
  Pxts128_context = ^xts128_context;
  xts128_context = record end; // Opaque structure

type
  // AES key setup functions
  TAES_set_encrypt_key = function(const userKey: PByte; const bits: Integer; key: PAES_KEY): Integer; cdecl;
  TAES_set_decrypt_key = function(const userKey: PByte; const bits: Integer; key: PAES_KEY): Integer; cdecl;
  
  // AES ECB mode
  TAES_ecb_encrypt = procedure(const in_: PByte; out_: PByte; const key: PAES_KEY; const enc: Integer); cdecl;
  
  // AES CBC mode
  TAES_cbc_encrypt = procedure(const in_: PByte; out_: PByte; length: NativeUInt; 
                                const key: PAES_KEY; ivec: PByte; const enc: Integer); cdecl;
  
  // AES CFB mode
  TAES_cfb128_encrypt = procedure(const in_: PByte; out_: PByte; length: NativeUInt;
                                  const key: PAES_KEY; ivec: PByte; num: PInteger; const enc: Integer); cdecl;
  TAES_cfb1_encrypt = procedure(const in_: PByte; out_: PByte; length: NativeUInt;
                                const key: PAES_KEY; ivec: PByte; num: PInteger; const enc: Integer); cdecl;
  TAES_cfb8_encrypt = procedure(const in_: PByte; out_: PByte; length: NativeUInt;
                                const key: PAES_KEY; ivec: PByte; num: PInteger; const enc: Integer); cdecl;
  
  // AES OFB mode
  TAES_ofb128_encrypt = procedure(const in_: PByte; out_: PByte; length: NativeUInt;
                                  const key: PAES_KEY; ivec: PByte; num: PInteger); cdecl;
  
  // AES CTR mode
  TAES_ctr128_encrypt = procedure(const in_: PByte; out_: PByte; length: NativeUInt;
                                  const key: PAES_KEY; ivec: PByte; ecount_buf: PByte; 
                                  num: PCardinal); cdecl;
  
  // AES IGE mode
  TAES_ige_encrypt = procedure(const in_: PByte; out_: PByte; length: NativeUInt;
                                const key: PAES_KEY; ivec: PByte; const enc: Integer); cdecl;
  TAES_bi_ige_encrypt = procedure(const in_: PByte; out_: PByte; length: NativeUInt;
                                  const key: PAES_KEY; const key2: PAES_KEY; 
                                  const ivec: PByte; const enc: Integer); cdecl;
  
  // AES Wrap mode
  TAES_wrap_key = function(key: PAES_KEY; const iv: PByte; out_: PByte; 
                          const in_: PByte; inlen: Cardinal): Integer; cdecl;
  TAES_unwrap_key = function(key: PAES_KEY; const iv: PByte; out_: PByte;
                            const in_: PByte; inlen: Cardinal): Integer; cdecl;
  
  // AES hardware acceleration options
  TAES_options = function: PAnsiChar; cdecl;
  
  // Low-level AES functions
  TAES_encrypt = procedure(const in_: PByte; out_: PByte; const key: PAES_KEY); cdecl;
  TAES_decrypt = procedure(const in_: PByte; out_: PByte; const key: PAES_KEY); cdecl;

var
  // Function pointers
  AES_set_encrypt_key: TAES_set_encrypt_key = nil;
  AES_set_decrypt_key: TAES_set_decrypt_key = nil;
  AES_ecb_encrypt: TAES_ecb_encrypt = nil;
  AES_cbc_encrypt: TAES_cbc_encrypt = nil;
  AES_cfb128_encrypt: TAES_cfb128_encrypt = nil;
  AES_cfb1_encrypt: TAES_cfb1_encrypt = nil;
  AES_cfb8_encrypt: TAES_cfb8_encrypt = nil;
  AES_ofb128_encrypt: TAES_ofb128_encrypt = nil;
  AES_ctr128_encrypt: TAES_ctr128_encrypt = nil;
  AES_ige_encrypt: TAES_ige_encrypt = nil;
  AES_bi_ige_encrypt: TAES_bi_ige_encrypt = nil;
  AES_wrap_key: TAES_wrap_key = nil;
  AES_unwrap_key: TAES_unwrap_key = nil;
  AES_options: TAES_options = nil;
  AES_encrypt: TAES_encrypt = nil;
  AES_decrypt: TAES_decrypt = nil;

// Helper functions
function LoadAESFunctions(ALibHandle: THandle): Boolean;
procedure UnloadAESFunctions;
function IsAESLoaded: Boolean;

// High-level helper functions
function AESEncryptECB(const Data: TBytes; const Key: TBytes): TBytes;
function AESDecryptECB(const Data: TBytes; const Key: TBytes): TBytes;
function AESEncryptCBC(const Data: TBytes; const Key: TBytes; const IV: TBytes): TBytes;
function AESDecryptCBC(const Data: TBytes; const Key: TBytes; const IV: TBytes): TBytes;
function AESEncryptCTR(const Data: TBytes; const Key: TBytes; const IV: TBytes): TBytes;
function AESDecryptCTR(const Data: TBytes; const Key: TBytes; const IV: TBytes): TBytes;

implementation

uses
  fafafa.ssl.openssl.api;

var
  GAESLoaded: Boolean = False;

function LoadAESFunctions(ALibHandle: THandle): Boolean;
begin
  Result := False;
  
  if ALibHandle = 0 then Exit;
  
  Pointer(AES_set_encrypt_key) := GetProcAddress(ALibHandle, 'AES_set_encrypt_key');
  Pointer(AES_set_decrypt_key) := GetProcAddress(ALibHandle, 'AES_set_decrypt_key');
  Pointer(AES_ecb_encrypt) := GetProcAddress(ALibHandle, 'AES_ecb_encrypt');
  Pointer(AES_cbc_encrypt) := GetProcAddress(ALibHandle, 'AES_cbc_encrypt');
  Pointer(AES_cfb128_encrypt) := GetProcAddress(ALibHandle, 'AES_cfb128_encrypt');
  Pointer(AES_cfb1_encrypt) := GetProcAddress(ALibHandle, 'AES_cfb1_encrypt');
  Pointer(AES_cfb8_encrypt) := GetProcAddress(ALibHandle, 'AES_cfb8_encrypt');
  Pointer(AES_ofb128_encrypt) := GetProcAddress(ALibHandle, 'AES_ofb128_encrypt');
  Pointer(AES_ctr128_encrypt) := GetProcAddress(ALibHandle, 'AES_ctr128_encrypt');
  Pointer(AES_ige_encrypt) := GetProcAddress(ALibHandle, 'AES_ige_encrypt');
  Pointer(AES_bi_ige_encrypt) := GetProcAddress(ALibHandle, 'AES_bi_ige_encrypt');
  Pointer(AES_wrap_key) := GetProcAddress(ALibHandle, 'AES_wrap_key');
  Pointer(AES_unwrap_key) := GetProcAddress(ALibHandle, 'AES_unwrap_key');
  Pointer(AES_options) := GetProcAddress(ALibHandle, 'AES_options');
  Pointer(AES_encrypt) := GetProcAddress(ALibHandle, 'AES_encrypt');
  Pointer(AES_decrypt) := GetProcAddress(ALibHandle, 'AES_decrypt');
  
  GAESLoaded := True;
  Result := True;
end;

procedure UnloadAESFunctions;
begin
  AES_set_encrypt_key := nil;
  AES_set_decrypt_key := nil;
  AES_ecb_encrypt := nil;
  AES_cbc_encrypt := nil;
  AES_cfb128_encrypt := nil;
  AES_cfb1_encrypt := nil;
  AES_cfb8_encrypt := nil;
  AES_ofb128_encrypt := nil;
  AES_ctr128_encrypt := nil;
  AES_ige_encrypt := nil;
  AES_bi_ige_encrypt := nil;
  AES_wrap_key := nil;
  AES_unwrap_key := nil;
  AES_options := nil;
  AES_encrypt := nil;
  AES_decrypt := nil;
  
  GAESLoaded := False;
end;

function IsAESLoaded: Boolean;
begin
  Result := GAESLoaded;
end;

function GetAESKeyBits(const Key: TBytes): Integer;
begin
  case Length(Key) of
    16: Result := 128;
    24: Result := 192;
    32: Result := 256;
  else
    Result := 0;
  end;
end;

function AESEncryptECB(const Data: TBytes; const Key: TBytes): TBytes;
var
  aesKey: AES_KEY;
  i, blocks: Integer;
  inblock, outblock: array[0..AES_BLOCK_SIZE-1] of Byte;
  bits: Integer;
begin
  Result := nil;
  bits := GetAESKeyBits(Key);
  if (bits = 0) or not Assigned(AES_set_encrypt_key) or not Assigned(AES_ecb_encrypt) then Exit;
  
  if AES_set_encrypt_key(@Key[0], bits, @aesKey) <> 0 then Exit;
  
  blocks := (Length(Data) + AES_BLOCK_SIZE - 1) div AES_BLOCK_SIZE;
  SetLength(Result, blocks * AES_BLOCK_SIZE);
  
  for i := 0 to blocks - 1 do
  begin
    FillChar(inblock, AES_BLOCK_SIZE, 0);
    if i * AES_BLOCK_SIZE + AES_BLOCK_SIZE <= Length(Data) then
      Move(Data[i * AES_BLOCK_SIZE], inblock[0], AES_BLOCK_SIZE)
    else
      Move(Data[i * AES_BLOCK_SIZE], inblock[0], Length(Data) - i * AES_BLOCK_SIZE);
    
    AES_ecb_encrypt(@inblock[0], @outblock[0], @aesKey, C_AES_ENCRYPT);
    Move(outblock[0], Result[i * AES_BLOCK_SIZE], AES_BLOCK_SIZE);
  end;
end;

function AESDecryptECB(const Data: TBytes; const Key: TBytes): TBytes;
var
  aesKey: AES_KEY;
  i, blocks: Integer;
  inblock, outblock: array[0..AES_BLOCK_SIZE-1] of Byte;
  bits: Integer;
begin
  Result := nil;
  bits := GetAESKeyBits(Key);
  if (bits = 0) or (Length(Data) mod AES_BLOCK_SIZE <> 0) or
    not Assigned(AES_set_decrypt_key) or not Assigned(AES_ecb_encrypt) then Exit;
  
  if AES_set_decrypt_key(@Key[0], bits, @aesKey) <> 0 then Exit;
  
  blocks := Length(Data) div AES_BLOCK_SIZE;
  SetLength(Result, blocks * AES_BLOCK_SIZE);
  
  for i := 0 to blocks - 1 do
  begin
    Move(Data[i * AES_BLOCK_SIZE], inblock[0], AES_BLOCK_SIZE);
    AES_ecb_encrypt(@inblock[0], @outblock[0], @aesKey, C_AES_DECRYPT);
    Move(outblock[0], Result[i * AES_BLOCK_SIZE], AES_BLOCK_SIZE);
  end;
end;

function AESEncryptCBC(const Data: TBytes; const Key: TBytes; const IV: TBytes): TBytes;
var
  aesKey: AES_KEY;
  ivec: array[0..AES_BLOCK_SIZE-1] of Byte;
  bits, padLen: Integer;
begin
  Result := nil;
  bits := GetAESKeyBits(Key);
  if (bits = 0) or (Length(IV) <> AES_BLOCK_SIZE) or 
    not Assigned(AES_set_encrypt_key) or not Assigned(AES_cbc_encrypt) then Exit;
  
  if AES_set_encrypt_key(@Key[0], bits, @aesKey) <> 0 then Exit;
  
  // Add PKCS7 padding
  padLen := AES_BLOCK_SIZE - (Length(Data) mod AES_BLOCK_SIZE);
  SetLength(Result, Length(Data) + padLen);
  if Length(Data) > 0 then
    Move(Data[0], Result[0], Length(Data));
  FillChar(Result[Length(Data)], padLen, padLen);
  
  Move(IV[0], ivec[0], AES_BLOCK_SIZE);
  AES_cbc_encrypt(@Result[0], @Result[0], Length(Result), @aesKey, @ivec[0], C_AES_ENCRYPT);
end;

function AESDecryptCBC(const Data: TBytes; const Key: TBytes; const IV: TBytes): TBytes;
var
  aesKey: AES_KEY;
  ivec: array[0..AES_BLOCK_SIZE-1] of Byte;
  bits, padLen: Integer;
begin
  Result := nil;
  bits := GetAESKeyBits(Key);
  if (bits = 0) or (Length(IV) <> AES_BLOCK_SIZE) or (Length(Data) mod AES_BLOCK_SIZE <> 0) or
    not Assigned(AES_set_decrypt_key) or not Assigned(AES_cbc_encrypt) then Exit;
  
  if AES_set_decrypt_key(@Key[0], bits, @aesKey) <> 0 then Exit;
  
  SetLength(Result, Length(Data));
  Move(Data[0], Result[0], Length(Data));
  Move(IV[0], ivec[0], AES_BLOCK_SIZE);
  
  AES_cbc_encrypt(@Result[0], @Result[0], Length(Result), @aesKey, @ivec[0], C_AES_DECRYPT);
  
  // Remove PKCS7 padding
  if Length(Result) > 0 then
  begin
    padLen := Result[Length(Result) - 1];
    if (padLen > 0) and (padLen <= AES_BLOCK_SIZE) then
      SetLength(Result, Length(Result) - padLen);
  end;
end;

function AESEncryptCTR(const Data: TBytes; const Key: TBytes; const IV: TBytes): TBytes;
var
  aesKey: AES_KEY;
  ivec: array[0..AES_BLOCK_SIZE-1] of Byte;
  ecount: array[0..AES_BLOCK_SIZE-1] of Byte;
  num: Cardinal;
  bits: Integer;
begin
  Result := nil;
  bits := GetAESKeyBits(Key);
  if (bits = 0) or (Length(IV) <> AES_BLOCK_SIZE) or 
    not Assigned(AES_set_encrypt_key) or not Assigned(AES_ctr128_encrypt) then Exit;
  
  if AES_set_encrypt_key(@Key[0], bits, @aesKey) <> 0 then Exit;
  
  SetLength(Result, Length(Data));
  if Length(Data) > 0 then
    Move(Data[0], Result[0], Length(Data));
  
  Move(IV[0], ivec[0], AES_BLOCK_SIZE);
  FillChar(ecount, AES_BLOCK_SIZE, 0);
  num := 0;
  
  AES_ctr128_encrypt(@Result[0], @Result[0], Length(Result), @aesKey, @ivec[0], @ecount[0], @num);
end;

function AESDecryptCTR(const Data: TBytes; const Key: TBytes; const IV: TBytes): TBytes;
begin
  // CTR mode decryption is the same as encryption
  Result := AESEncryptCTR(Data, Key, IV);
end;

end.