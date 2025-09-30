{******************************************************************************}
{                                                                              }
{  fafafa.ssl - OpenSSL DES Module                                           }
{                                                                              }
{  Copyright (c) 2024 fafafa                                                  }
{                                                                              }
{******************************************************************************}

unit fafafa.ssl.openssl.des;

{$mode Delphi}
{$H+}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.types,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.consts;

const
  DES_ENCRYPT = 1;
  DES_DECRYPT = 0;
  
  DES_CBC_MODE = 0;
  DES_PCBC_MODE = 1;
  
  DES_KEY_SZ = 8;
  DES_SCHEDULE_SZ = 16;
  
  DES_BLOCK = 8;

type
  DES_LONG = Cardinal;
  PDES_LONG = ^DES_LONG;
  // DES key types
  DES_cblock = array[0..7] of Byte;
  PDES_cblock = ^DES_cblock;
  const_DES_cblock = DES_cblock;
  Pconst_DES_cblock = ^const_DES_cblock;
  
  // DES key schedule
  PDES_key_schedule = ^DES_key_schedule;
  DES_key_schedule = record
    ks: array[0..15] of record
      deslong: array[0..1] of DES_LONG;
    end;
  end;
  
  const_DES_key_schedule = DES_key_schedule;

type
  // DES functions
  TDES_options = function: PAnsiChar; cdecl;
  TDES_ecb3_encrypt = procedure(const input: Pconst_DES_cblock; output: PDES_cblock;
                                 ks1: PDES_key_schedule; ks2: PDES_key_schedule;
                                 ks3: PDES_key_schedule; enc: Integer); cdecl;
                                 
  TDES_crypt = function(buf: PAnsiChar; salt: PAnsiChar): PAnsiChar; cdecl;
  TDES_fcrypt = function(buf: PAnsiChar; salt: PAnsiChar; ret: PAnsiChar): PAnsiChar; cdecl;
  
  TDES_ecb_encrypt = procedure(const input: Pconst_DES_cblock; output: PDES_cblock;
                                ks: PDES_key_schedule; enc: Integer); cdecl;
                                
  TDES_ncbc_encrypt = procedure(const input: PByte; output: PByte; length: LongInt;
                                 schedule: PDES_key_schedule; ivec: PDES_cblock; enc: Integer); cdecl;
                                 
  TDES_xcbc_encrypt = procedure(const input: PByte; output: PByte; length: LongInt;
                                 schedule: PDES_key_schedule; ivec: PDES_cblock;
                                 const inw: Pconst_DES_cblock; const outw: Pconst_DES_cblock;
                                 enc: Integer); cdecl;
                                 
  TDES_cfb_encrypt = procedure(const in_: PByte; out_: PByte; numbits: Integer;
                                length: LongInt; schedule: PDES_key_schedule;
                                ivec: PDES_cblock; enc: Integer); cdecl;
                                
  TDES_ecb2_encrypt = procedure(const input: Pconst_DES_cblock; output: PDES_cblock;
                                 ks1: PDES_key_schedule; ks2: PDES_key_schedule;
                                 enc: Integer); cdecl;
                                 
  TDES_ede2_cbc_encrypt = procedure(const input: PByte; output: PByte; length: LongInt;
                                     ks1: PDES_key_schedule; ks2: PDES_key_schedule;
                                     ivec: PDES_cblock; enc: Integer); cdecl;
                                     
  TDES_ede2_cfb64_encrypt = procedure(const in_: PByte; out_: PByte; length: LongInt;
                                       ks1: PDES_key_schedule; ks2: PDES_key_schedule;
                                       ivec: PDES_cblock; num: PInteger; enc: Integer); cdecl;
                                       
  TDES_ede2_ofb64_encrypt = procedure(const in_: PByte; out_: PByte; length: LongInt;
                                       ks1: PDES_key_schedule; ks2: PDES_key_schedule;
                                       ivec: PDES_cblock; num: PInteger); cdecl;
                                       
  TDES_ede3_cbc_encrypt = procedure(const input: PByte; output: PByte; length: LongInt;
                                     ks1: PDES_key_schedule; ks2: PDES_key_schedule;
                                     ks3: PDES_key_schedule; ivec: PDES_cblock; enc: Integer); cdecl;
                                     
  TDES_ede3_cfb64_encrypt = procedure(const in_: PByte; out_: PByte; length: LongInt;
                                       ks1: PDES_key_schedule; ks2: PDES_key_schedule;
                                       ks3: PDES_key_schedule; ivec: PDES_cblock;
                                       num: PInteger; enc: Integer); cdecl;
                                       
  TDES_ede3_cfb_encrypt = procedure(const in_: PByte; out_: PByte; numbits: Integer;
                                     length: LongInt; ks1: PDES_key_schedule;
                                     ks2: PDES_key_schedule; ks3: PDES_key_schedule;
                                     ivec: PDES_cblock; enc: Integer); cdecl;
                                     
  TDES_ede3_ofb64_encrypt = procedure(const in_: PByte; out_: PByte; length: LongInt;
                                       ks1: PDES_key_schedule; ks2: PDES_key_schedule;
                                       ks3: PDES_key_schedule; ivec: PDES_cblock;
                                       num: PInteger); cdecl;
                                       
  TDES_enc_read = function(fd: Integer; buf: Pointer; len: Integer;
                           sched: PDES_key_schedule; iv: PDES_cblock): Integer; cdecl;
                           
  TDES_enc_write = function(fd: Integer; const buf: Pointer; len: Integer;
                            sched: PDES_key_schedule; iv: PDES_cblock): Integer; cdecl;
                            
  TDES_set_odd_parity = procedure(key: PDES_cblock); cdecl;
  TDES_check_key_parity = function(const key: Pconst_DES_cblock): Integer; cdecl;
  TDES_is_weak_key = function(const key: Pconst_DES_cblock): Integer; cdecl;
  
  TDES_set_key = function(const key: Pconst_DES_cblock; schedule: PDES_key_schedule): Integer; cdecl;
  TDES_key_sched = function(const key: Pconst_DES_cblock; schedule: PDES_key_schedule): Integer; cdecl;
  TDES_set_key_checked = function(const key: Pconst_DES_cblock; schedule: PDES_key_schedule): Integer; cdecl;
  TDES_set_key_unchecked = procedure(const key: Pconst_DES_cblock; schedule: PDES_key_schedule); cdecl;
  
  TDES_string_to_key = procedure(const str: PAnsiChar; key: PDES_cblock); cdecl;
  TDES_string_to_2keys = procedure(const str: PAnsiChar; key1: PDES_cblock; key2: PDES_cblock); cdecl;
  
  TDES_cfb64_encrypt = procedure(const in_: PByte; out_: PByte; length: LongInt;
                                  schedule: PDES_key_schedule; ivec: PDES_cblock;
                                  num: PInteger; enc: Integer); cdecl;
                                  
  TDES_ofb64_encrypt = procedure(const in_: PByte; out_: PByte; length: LongInt;
                                  schedule: PDES_key_schedule; ivec: PDES_cblock;
                                  num: PInteger); cdecl;
                                  
  TDES_pcbc_encrypt = procedure(const input: PByte; output: PByte; length: LongInt;
                                 schedule: PDES_key_schedule; ivec: PDES_cblock; enc: Integer); cdecl;
                                 
  TDES_quad_cksum = function(const input: PByte; output: PDES_cblock; length: LongInt;
                             out_count: Integer; seed: PDES_cblock): DES_LONG; cdecl;
                             
  TDES_random_key = function(ret: PDES_cblock): Integer; cdecl;
  
  TDES_encrypt1 = procedure(data: PDES_LONG; ks: PDES_key_schedule; enc: Integer); cdecl;
  TDES_encrypt2 = procedure(data: PDES_LONG; ks: PDES_key_schedule; enc: Integer); cdecl;
  TDES_encrypt3 = procedure(data: PDES_LONG; ks1: PDES_key_schedule;
                           ks2: PDES_key_schedule; ks3: PDES_key_schedule); cdecl;
  TDES_decrypt3 = procedure(data: PDES_LONG; ks1: PDES_key_schedule;
                           ks2: PDES_key_schedule; ks3: PDES_key_schedule); cdecl;

var
  // DES function pointers
  DES_options: TDES_options = nil;
  DES_ecb3_encrypt: TDES_ecb3_encrypt = nil;
  DES_crypt: TDES_crypt = nil;
  DES_fcrypt: TDES_fcrypt = nil;
  DES_ecb_encrypt: TDES_ecb_encrypt = nil;
  DES_ncbc_encrypt: TDES_ncbc_encrypt = nil;
  DES_xcbc_encrypt: TDES_xcbc_encrypt = nil;
  DES_cfb_encrypt: TDES_cfb_encrypt = nil;
  DES_ecb2_encrypt: TDES_ecb2_encrypt = nil;
  DES_ede2_cbc_encrypt: TDES_ede2_cbc_encrypt = nil;
  DES_ede2_cfb64_encrypt: TDES_ede2_cfb64_encrypt = nil;
  DES_ede2_ofb64_encrypt: TDES_ede2_ofb64_encrypt = nil;
  DES_ede3_cbc_encrypt: TDES_ede3_cbc_encrypt = nil;
  DES_ede3_cfb64_encrypt: TDES_ede3_cfb64_encrypt = nil;
  DES_ede3_cfb_encrypt: TDES_ede3_cfb_encrypt = nil;
  DES_ede3_ofb64_encrypt: TDES_ede3_ofb64_encrypt = nil;
  DES_enc_read: TDES_enc_read = nil;
  DES_enc_write: TDES_enc_write = nil;
  DES_set_odd_parity: TDES_set_odd_parity = nil;
  DES_check_key_parity: TDES_check_key_parity = nil;
  DES_is_weak_key: TDES_is_weak_key = nil;
  DES_set_key: TDES_set_key = nil;
  DES_key_sched: TDES_key_sched = nil;
  DES_set_key_checked: TDES_set_key_checked = nil;
  DES_set_key_unchecked: TDES_set_key_unchecked = nil;
  DES_string_to_key: TDES_string_to_key = nil;
  DES_string_to_2keys: TDES_string_to_2keys = nil;
  DES_cfb64_encrypt: TDES_cfb64_encrypt = nil;
  DES_ofb64_encrypt: TDES_ofb64_encrypt = nil;
  DES_pcbc_encrypt: TDES_pcbc_encrypt = nil;
  DES_quad_cksum: TDES_quad_cksum = nil;
  DES_random_key: TDES_random_key = nil;
  DES_encrypt1: TDES_encrypt1 = nil;
  DES_encrypt2: TDES_encrypt2 = nil;
  DES_encrypt3: TDES_encrypt3 = nil;
  DES_decrypt3: TDES_decrypt3 = nil;

// Helper functions
function LoadDESFunctions(ALibHandle: THandle): Boolean;
procedure UnloadDESFunctions;
function IsDESLoaded: Boolean;

// High-level helper functions
function DESEncrypt(const Data: TBytes; const Key: TBytes): TBytes;
function DESDecrypt(const Data: TBytes; const Key: TBytes): TBytes;
function DES3Encrypt(const Data: TBytes; const Key1, Key2, Key3: TBytes): TBytes;
function DES3Decrypt(const Data: TBytes; const Key1, Key2, Key3: TBytes): TBytes;

implementation

uses
  fafafa.ssl.openssl.api;

var
  GDESLoaded: Boolean = False;

function LoadDESFunctions(ALibHandle: THandle): Boolean;
begin
  Result := False;
  
  if ALibHandle = 0 then Exit;
  
  DES_options := GetProcAddress(ALibHandle, 'DES_options');
  DES_ecb3_encrypt := GetProcAddress(ALibHandle, 'DES_ecb3_encrypt');
  DES_crypt := GetProcAddress(ALibHandle, 'DES_crypt');
  DES_fcrypt := GetProcAddress(ALibHandle, 'DES_fcrypt');
  DES_ecb_encrypt := GetProcAddress(ALibHandle, 'DES_ecb_encrypt');
  DES_ncbc_encrypt := GetProcAddress(ALibHandle, 'DES_ncbc_encrypt');
  DES_xcbc_encrypt := GetProcAddress(ALibHandle, 'DES_xcbc_encrypt');
  DES_cfb_encrypt := GetProcAddress(ALibHandle, 'DES_cfb_encrypt');
  DES_ecb2_encrypt := GetProcAddress(ALibHandle, 'DES_ecb2_encrypt');
  DES_ede2_cbc_encrypt := GetProcAddress(ALibHandle, 'DES_ede2_cbc_encrypt');
  DES_ede2_cfb64_encrypt := GetProcAddress(ALibHandle, 'DES_ede2_cfb64_encrypt');
  DES_ede2_ofb64_encrypt := GetProcAddress(ALibHandle, 'DES_ede2_ofb64_encrypt');
  DES_ede3_cbc_encrypt := GetProcAddress(ALibHandle, 'DES_ede3_cbc_encrypt');
  DES_ede3_cfb64_encrypt := GetProcAddress(ALibHandle, 'DES_ede3_cfb64_encrypt');
  DES_ede3_cfb_encrypt := GetProcAddress(ALibHandle, 'DES_ede3_cfb_encrypt');
  DES_ede3_ofb64_encrypt := GetProcAddress(ALibHandle, 'DES_ede3_ofb64_encrypt');
  DES_enc_read := GetProcAddress(ALibHandle, 'DES_enc_read');
  DES_enc_write := GetProcAddress(ALibHandle, 'DES_enc_write');
  DES_set_odd_parity := GetProcAddress(ALibHandle, 'DES_set_odd_parity');
  DES_check_key_parity := GetProcAddress(ALibHandle, 'DES_check_key_parity');
  DES_is_weak_key := GetProcAddress(ALibHandle, 'DES_is_weak_key');
  DES_set_key := GetProcAddress(ALibHandle, 'DES_set_key');
  DES_key_sched := GetProcAddress(ALibHandle, 'DES_key_sched');
  DES_set_key_checked := GetProcAddress(ALibHandle, 'DES_set_key_checked');
  DES_set_key_unchecked := GetProcAddress(ALibHandle, 'DES_set_key_unchecked');
  DES_string_to_key := GetProcAddress(ALibHandle, 'DES_string_to_key');
  DES_string_to_2keys := GetProcAddress(ALibHandle, 'DES_string_to_2keys');
  DES_cfb64_encrypt := GetProcAddress(ALibHandle, 'DES_cfb64_encrypt');
  DES_ofb64_encrypt := GetProcAddress(ALibHandle, 'DES_ofb64_encrypt');
  DES_pcbc_encrypt := GetProcAddress(ALibHandle, 'DES_pcbc_encrypt');
  DES_quad_cksum := GetProcAddress(ALibHandle, 'DES_quad_cksum');
  DES_random_key := GetProcAddress(ALibHandle, 'DES_random_key');
  DES_encrypt1 := GetProcAddress(ALibHandle, 'DES_encrypt1');
  DES_encrypt2 := GetProcAddress(ALibHandle, 'DES_encrypt2');
  DES_encrypt3 := GetProcAddress(ALibHandle, 'DES_encrypt3');
  DES_decrypt3 := GetProcAddress(ALibHandle, 'DES_decrypt3');
  
  GDESLoaded := True;
  Result := True;
end;

procedure UnloadDESFunctions;
begin
  DES_options := nil;
  DES_ecb3_encrypt := nil;
  DES_crypt := nil;
  DES_fcrypt := nil;
  DES_ecb_encrypt := nil;
  DES_ncbc_encrypt := nil;
  DES_xcbc_encrypt := nil;
  DES_cfb_encrypt := nil;
  DES_ecb2_encrypt := nil;
  DES_ede2_cbc_encrypt := nil;
  DES_ede2_cfb64_encrypt := nil;
  DES_ede2_ofb64_encrypt := nil;
  DES_ede3_cbc_encrypt := nil;
  DES_ede3_cfb64_encrypt := nil;
  DES_ede3_cfb_encrypt := nil;
  DES_ede3_ofb64_encrypt := nil;
  DES_enc_read := nil;
  DES_enc_write := nil;
  DES_set_odd_parity := nil;
  DES_check_key_parity := nil;
  DES_is_weak_key := nil;
  DES_set_key := nil;
  DES_key_sched := nil;
  DES_set_key_checked := nil;
  DES_set_key_unchecked := nil;
  DES_string_to_key := nil;
  DES_string_to_2keys := nil;
  DES_cfb64_encrypt := nil;
  DES_ofb64_encrypt := nil;
  DES_pcbc_encrypt := nil;
  DES_quad_cksum := nil;
  DES_random_key := nil;
  DES_encrypt1 := nil;
  DES_encrypt2 := nil;
  DES_encrypt3 := nil;
  DES_decrypt3 := nil;
  
  GDESLoaded := False;
end;

function IsDESLoaded: Boolean;
begin
  Result := GDESLoaded;
end;

function DESEncrypt(const Data: TBytes; const Key: TBytes): TBytes;
var
  ks: DES_key_schedule;
  cblock: DES_cblock;
  outblock: DES_cblock;
  i, blocks: Integer;
begin
  Result := nil;
  if (Length(Key) < 8) or not Assigned(DES_set_key_unchecked) or 
     not Assigned(DES_ecb_encrypt) then Exit;
  
  Move(Key[0], cblock[0], 8);
  DES_set_key_unchecked(@cblock, @ks);
  
  blocks := (Length(Data) + 7) div 8;
  SetLength(Result, blocks * 8);
  
  for i := 0 to blocks - 1 do
  begin
    FillChar(cblock, 8, 0);
    if i * 8 + 8 <= Length(Data) then
      Move(Data[i * 8], cblock[0], 8)
    else
      Move(Data[i * 8], cblock[0], Length(Data) - i * 8);
    
    DES_ecb_encrypt(@cblock, @outblock, @ks, DES_ENCRYPT);
    Move(outblock[0], Result[i * 8], 8);
  end;
end;

function DESDecrypt(const Data: TBytes; const Key: TBytes): TBytes;
var
  ks: DES_key_schedule;
  cblock: DES_cblock;
  outblock: DES_cblock;
  i, blocks: Integer;
begin
  Result := nil;
  if (Length(Key) < 8) or (Length(Data) mod 8 <> 0) or 
     not Assigned(DES_set_key_unchecked) or not Assigned(DES_ecb_encrypt) then Exit;
  
  Move(Key[0], cblock[0], 8);
  DES_set_key_unchecked(@cblock, @ks);
  
  blocks := Length(Data) div 8;
  SetLength(Result, blocks * 8);
  
  for i := 0 to blocks - 1 do
  begin
    Move(Data[i * 8], cblock[0], 8);
    DES_ecb_encrypt(@cblock, @outblock, @ks, DES_DECRYPT);
    Move(outblock[0], Result[i * 8], 8);
  end;
end;

function DES3Encrypt(const Data: TBytes; const Key1, Key2, Key3: TBytes): TBytes;
var
  ks1, ks2, ks3: DES_key_schedule;
  cblock1, cblock2, cblock3: DES_cblock;
  inblock, outblock: DES_cblock;
  i, blocks: Integer;
begin
  Result := nil;
  if (Length(Key1) < 8) or (Length(Key2) < 8) or (Length(Key3) < 8) or
     not Assigned(DES_set_key_unchecked) or not Assigned(DES_ecb3_encrypt) then Exit;
  
  Move(Key1[0], cblock1[0], 8);
  Move(Key2[0], cblock2[0], 8);
  Move(Key3[0], cblock3[0], 8);
  
  DES_set_key_unchecked(@cblock1, @ks1);
  DES_set_key_unchecked(@cblock2, @ks2);
  DES_set_key_unchecked(@cblock3, @ks3);
  
  blocks := (Length(Data) + 7) div 8;
  SetLength(Result, blocks * 8);
  
  for i := 0 to blocks - 1 do
  begin
    FillChar(inblock, 8, 0);
    if i * 8 + 8 <= Length(Data) then
      Move(Data[i * 8], inblock[0], 8)
    else
      Move(Data[i * 8], inblock[0], Length(Data) - i * 8);
    
    DES_ecb3_encrypt(@inblock, @outblock, @ks1, @ks2, @ks3, DES_ENCRYPT);
    Move(outblock[0], Result[i * 8], 8);
  end;
end;

function DES3Decrypt(const Data: TBytes; const Key1, Key2, Key3: TBytes): TBytes;
var
  ks1, ks2, ks3: DES_key_schedule;
  cblock1, cblock2, cblock3: DES_cblock;
  inblock, outblock: DES_cblock;
  i, blocks: Integer;
begin
  Result := nil;
  if (Length(Key1) < 8) or (Length(Key2) < 8) or (Length(Key3) < 8) or
     (Length(Data) mod 8 <> 0) or not Assigned(DES_set_key_unchecked) or 
     not Assigned(DES_ecb3_encrypt) then Exit;
  
  Move(Key1[0], cblock1[0], 8);
  Move(Key2[0], cblock2[0], 8);
  Move(Key3[0], cblock3[0], 8);
  
  DES_set_key_unchecked(@cblock1, @ks1);
  DES_set_key_unchecked(@cblock2, @ks2);
  DES_set_key_unchecked(@cblock3, @ks3);
  
  blocks := Length(Data) div 8;
  SetLength(Result, blocks * 8);
  
  for i := 0 to blocks - 1 do
  begin
    Move(Data[i * 8], inblock[0], 8);
    DES_ecb3_encrypt(@inblock, @outblock, @ks1, @ks2, @ks3, DES_DECRYPT);
    Move(outblock[0], Result[i * 8], 8);
  end;
end;

end.