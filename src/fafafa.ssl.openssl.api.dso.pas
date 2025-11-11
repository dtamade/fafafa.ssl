{******************************************************************************}
{                                                                              }
{  fafafa.ssl.openssl.api.dso - OpenSSL DSO Module Pascal Binding                 }
{                                                                              }
{  Copyright (c) 2024 fafafa                                                  }
{                                                                              }
{******************************************************************************}
unit fafafa.ssl.openssl.api.dso;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api.consts;

type
  { DSO types }
  DSO = record end;
  PDSO = ^DSO;
  
  DSO_METHOD = record end;
  PDSO_METHOD = ^DSO_METHOD;
  
  { DSO flags }
const
  DSO_FLAG_NO_NAME_TRANSLATION = $01;
  DSO_FLAG_NAME_TRANSLATION_EXT_ONLY = $02;
  DSO_FLAG_UPCASE_SYMBOL = $10;
  DSO_FLAG_GLOBAL_SYMBOLS = $20;
  
  { DSO control commands }
  DSO_CTRL_GET_FLAGS = 1;
  DSO_CTRL_SET_FLAGS = 2;
  DSO_CTRL_OR_FLAGS = 3;

type  
  { DSO callback types }
  TDSO_name_converter_func = function(dso: PDSO; const from: PChar): PChar; cdecl;
  TDSO_merger_func = function(dso: PDSO; const filespec1, filespec2: PChar): PChar; cdecl;
  
  { Function pointers for dynamic loading }
  TDSONew = function: PDSO; cdecl;
  TDSOFree = function(dso: PDSO): TOpenSSLInt; cdecl;
  TDSOUpRef = function(dso: PDSO): TOpenSSLInt; cdecl;
  TDSOLoad = function(dso: PDSO; const filename: PChar; meth: PDSO_METHOD; flags: TOpenSSLInt): PDSO; cdecl;
  TDSOBind = function(dso: PDSO; const symname: PChar): Pointer; cdecl;
  TDSOUnbind = function(dso: PDSO; const symname: PChar): TOpenSSLInt; cdecl;
  TDSOCtrl = function(dso: PDSO; cmd: TOpenSSLInt; larg: TOpenSSLLong; parg: Pointer): TOpenSSLLong; cdecl;
  TDSOGetFilename = function(dso: PDSO): PChar; cdecl;
  TDSOSetFilename = function(dso: PDSO; const filename: PChar): TOpenSSLInt; cdecl;
  TDSOConvertFilename = function(dso: PDSO; const filename: PChar): PChar; cdecl;
  TDSOGetLoadedFilename = function(dso: PDSO): PChar; cdecl;
  TDSOSetDefaultMethod = function(meth: PDSO_METHOD): PDSO_METHOD; cdecl;
  TDSOGetDefaultMethod = function: PDSO_METHOD; cdecl;
  TDSOGetMethod = function(dso: PDSO): PDSO_METHOD; cdecl;
  TDSOSetMethod = function(dso: PDSO; meth: PDSO_METHOD): PDSO_METHOD; cdecl;
  TDSONewMethod = function: PDSO_METHOD; cdecl;
  TDSOFreeMethod = procedure(meth: PDSO_METHOD); cdecl;
  TDSOPathcheck = function(const path: PChar): TOpenSSLInt; cdecl;
  TDSODlfcn = function: PDSO_METHOD; cdecl;
  TDSOWin32 = function: PDSO_METHOD; cdecl;
  TDSOVms = function: PDSO_METHOD; cdecl;
  
var
  { Function variables }
  DSO_new: TDSONew = nil;
  DSO_free: TDSOFree = nil;
  DSO_up_ref: TDSOUpRef = nil;
  DSO_load: TDSOLoad = nil;
  DSO_bind_var: TDSOBind = nil;
  DSO_bind_func: TDSOBind = nil;
  DSO_unbind_var: TDSOUnbind = nil;
  DSO_unbind_func: TDSOUnbind = nil;
  DSO_ctrl: TDSOCtrl = nil;
  DSO_get_filename: TDSOGetFilename = nil;
  DSO_set_filename: TDSOSetFilename = nil;
  DSO_convert_filename: TDSOConvertFilename = nil;
  DSO_get_loaded_filename: TDSOGetLoadedFilename = nil;
  DSO_set_default_method: TDSOSetDefaultMethod = nil;
  DSO_get_default_method: TDSOGetDefaultMethod = nil;
  DSO_get_method: TDSOGetMethod = nil;
  DSO_set_method: TDSOSetMethod = nil;
  DSO_new_method: TDSONewMethod = nil;
  DSO_free_method: TDSOFreeMethod = nil;
  DSO_pathbyaddr: TDSOPathcheck = nil;
  DSO_dlfcn: TDSODlfcn = nil;
  DSO_METHOD_win32: TDSOWin32 = nil;
  DSO_METHOD_vms: TDSOVms = nil;

{ Load/Unload functions }
function LoadDSO(const ALibCrypto: THandle): Boolean;
procedure UnloadDSO;

{ Helper functions }
function DSOLoadLibrary(const LibraryPath: string): PDSO;
function DSOGetSymbol(dso: PDSO; const SymbolName: string): Pointer;
procedure DSOFreeLibrary(dso: PDSO);
function DSOGetFlags(dso: PDSO): TOpenSSLInt;
function DSOSetFlags(dso: PDSO; Flags: TOpenSSLInt): Boolean;

implementation

uses
  fafafa.ssl.openssl.api.utils;

var
  DSOLoaded: Boolean = False;

function LoadDSO(const ALibCrypto: THandle): Boolean;
begin
  Result := False;
  if DSOLoaded then Exit(True);
  if ALibCrypto = 0 then Exit;

  { Load basic functions }
  DSO_new := TDSONew(GetProcAddress(ALibCrypto, 'DSO_new'));
  DSO_free := TDSOFree(GetProcAddress(ALibCrypto, 'DSO_free'));
  DSO_up_ref := TDSOUpRef(GetProcAddress(ALibCrypto, 'DSO_up_ref'));
  DSO_load := TDSOLoad(GetProcAddress(ALibCrypto, 'DSO_load'));
  
  { Load binding functions }
  DSO_bind_var := TDSOBind(GetProcAddress(ALibCrypto, 'DSO_bind_var'));
  DSO_bind_func := TDSOBind(GetProcAddress(ALibCrypto, 'DSO_bind_func'));
  DSO_unbind_var := TDSOUnbind(GetProcAddress(ALibCrypto, 'DSO_unbind_var'));
  DSO_unbind_func := TDSOUnbind(GetProcAddress(ALibCrypto, 'DSO_unbind_func'));
  
  { Load control and info functions }
  DSO_ctrl := TDSOCtrl(GetProcAddress(ALibCrypto, 'DSO_ctrl'));
  DSO_get_filename := TDSOGetFilename(GetProcAddress(ALibCrypto, 'DSO_get_filename'));
  DSO_set_filename := TDSOSetFilename(GetProcAddress(ALibCrypto, 'DSO_set_filename'));
  DSO_convert_filename := TDSOConvertFilename(GetProcAddress(ALibCrypto, 'DSO_convert_filename'));
  DSO_get_loaded_filename := TDSOGetLoadedFilename(GetProcAddress(ALibCrypto, 'DSO_get_loaded_filename'));
  
  { Load method functions }
  DSO_set_default_method := TDSOSetDefaultMethod(GetProcAddress(ALibCrypto, 'DSO_set_default_method'));
  DSO_get_default_method := TDSOGetDefaultMethod(GetProcAddress(ALibCrypto, 'DSO_get_default_method'));
  DSO_get_method := TDSOGetMethod(GetProcAddress(ALibCrypto, 'DSO_get_method'));
  DSO_set_method := TDSOSetMethod(GetProcAddress(ALibCrypto, 'DSO_set_method'));
  DSO_new_method := TDSONewMethod(GetProcAddress(ALibCrypto, 'DSO_new_method'));
  DSO_free_method := TDSOFreeMethod(GetProcAddress(ALibCrypto, 'DSO_METHOD_free'));
  
  { Load platform specific functions }
  DSO_pathbyaddr := TDSOPathcheck(GetProcAddress(ALibCrypto, 'DSO_pathbyaddr'));
  DSO_dlfcn := TDSODlfcn(GetProcAddress(ALibCrypto, 'DSO_METHOD_dlfcn'));
  DSO_METHOD_win32 := TDSOWin32(GetProcAddress(ALibCrypto, 'DSO_METHOD_win32'));
  DSO_METHOD_vms := TDSOVms(GetProcAddress(ALibCrypto, 'DSO_METHOD_vms'));

  Result := Assigned(DSO_new) and Assigned(DSO_free);
  DSOLoaded := Result;
end;

procedure UnloadDSO;
begin
  if not DSOLoaded then Exit;

  DSO_new := nil;
  DSO_free := nil;
  DSO_up_ref := nil;
  DSO_load := nil;
  DSO_bind_var := nil;
  DSO_bind_func := nil;
  DSO_unbind_var := nil;
  DSO_unbind_func := nil;
  DSO_ctrl := nil;
  DSO_get_filename := nil;
  DSO_set_filename := nil;
  DSO_convert_filename := nil;
  DSO_get_loaded_filename := nil;
  DSO_set_default_method := nil;
  DSO_get_default_method := nil;
  DSO_get_method := nil;
  DSO_set_method := nil;
  DSO_new_method := nil;
  DSO_free_method := nil;
  DSO_pathbyaddr := nil;
  DSO_dlfcn := nil;
  DSO_METHOD_win32 := nil;
  DSO_METHOD_vms := nil;

  DSOLoaded := False;
end;

{ Helper functions }

function DSOLoadLibrary(const LibraryPath: string): PDSO;
var
  Method: PDSO_METHOD;
begin
  Result := nil;
  if not Assigned(DSO_new) or not Assigned(DSO_load) then Exit;
  
  Result := DSO_new();
  if Result = nil then Exit;
  
  Method := nil;
  if Assigned(DSO_get_default_method) then
    Method := DSO_get_default_method();
    
  if DSO_load(Result, PChar(LibraryPath), Method, 0) = nil then
  begin
    if Assigned(DSO_free) then
      DSO_free(Result);
    Result := nil;
  end;
end;

function DSOGetSymbol(dso: PDSO; const SymbolName: string): Pointer;
begin
  Result := nil;
  if (dso = nil) or not Assigned(DSO_bind_func) then Exit;
  
  Result := DSO_bind_func(dso, PChar(SymbolName));
end;

procedure DSOFreeLibrary(dso: PDSO);
begin
  if (dso <> nil) and Assigned(DSO_free) then
    DSO_free(dso);
end;

function DSOGetFlags(dso: PDSO): TOpenSSLInt;
begin
  Result := 0;
  if (dso <> nil) and Assigned(DSO_ctrl) then
    Result := DSO_ctrl(dso, DSO_CTRL_GET_FLAGS, 0, nil);
end;

function DSOSetFlags(dso: PDSO; Flags: TOpenSSLInt): Boolean;
begin
  Result := False;
  if (dso <> nil) and Assigned(DSO_ctrl) then
    Result := DSO_ctrl(dso, DSO_CTRL_SET_FLAGS, Flags, nil) > 0;
end;

initialization

finalization
  UnloadDSO;

end.