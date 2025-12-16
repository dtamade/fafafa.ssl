{$mode ObjFPC}{$H+}

unit fafafa.ssl.openssl.api.stack;

interface

uses
  fafafa.ssl.base,
  fafafa.ssl.exceptions,
  SysUtils,
  fafafa.ssl.openssl.types;

type
  // Stack comparison function
  Tstack_cmp_func = function(const a, b: Pointer): Integer; cdecl;
  
  // Stack free function
  Tstack_free_func = procedure(ptr: Pointer); cdecl;
  
  // Stack copy function
  Tstack_copy_func = function(ptr: Pointer): Pointer; cdecl;
  
  // OpenSSL stack structure (opaque)
  OPENSSL_STACK = record
    opaque_data: Pointer;
  end;
  POPENSSL_STACK = ^OPENSSL_STACK;
  
  // Typed stack definitions
  PSTACK_OF_X509 = POPENSSL_STACK;
  PSTACK_OF_X509_CRL = POPENSSL_STACK;
  PSTACK_OF_X509_NAME = POPENSSL_STACK;
  PSTACK_OF_X509_EXTENSION = POPENSSL_STACK;
  PSTACK_OF_X509_ATTRIBUTE = POPENSSL_STACK;
  PSTACK_OF_X509_INFO = POPENSSL_STACK;
  PSTACK_OF_GENERAL_NAME = POPENSSL_STACK;
  PSTACK_OF_DIST_POINT = POPENSSL_STACK;
  PSTACK_OF_ASN1_OBJECT = POPENSSL_STACK;
  PSTACK_OF_ASN1_STRING = POPENSSL_STACK;
  PSTACK_OF_ASN1_TYPE = POPENSSL_STACK;
  PSTACK_OF_ASN1_INTEGER = POPENSSL_STACK;
  PSTACK_OF_CONF_VALUE = POPENSSL_STACK;
  PSTACK_OF_OPENSSL_STRING = POPENSSL_STACK;
  PSTACK_OF_OPENSSL_CSTRING = POPENSSL_STACK;
  PSTACK_OF_OPENSSL_BLOCK = POPENSSL_STACK;
  PSTACK_OF_BIO = POPENSSL_STACK;
  PSTACK_OF_SSL_CIPHER = POPENSSL_STACK;
  PSTACK_OF_SSL_COMP = POPENSSL_STACK;
  PSTACK_OF_SCT = POPENSSL_STACK;
  
  // Stack function pointer types
  TOPENSSL_sk_new = function(cmp: Tstack_cmp_func): POPENSSL_STACK; cdecl;
  TOPENSSL_sk_new_null = function: POPENSSL_STACK; cdecl;
  TOPENSSL_sk_new_reserve = function(cmp: Tstack_cmp_func; n: Integer): POPENSSL_STACK; cdecl;
  TOPENSSL_sk_reserve = function(st: POPENSSL_STACK; n: Integer): Integer; cdecl;
  TOPENSSL_sk_free = procedure(st: POPENSSL_STACK); cdecl;
  TOPENSSL_sk_pop_free = procedure(st: POPENSSL_STACK; func: Tstack_free_func); cdecl;
  TOPENSSL_sk_deep_copy = function(st: POPENSSL_STACK; c: Tstack_copy_func; 
                                  f: Tstack_free_func): POPENSSL_STACK; cdecl;
  TOPENSSL_sk_insert = function(sk: POPENSSL_STACK; data: Pointer; where: Integer): Integer; cdecl;
  TOPENSSL_sk_delete = function(st: POPENSSL_STACK; loc: Integer): Pointer; cdecl;
  TOPENSSL_sk_delete_ptr = function(st: POPENSSL_STACK; p: Pointer): Pointer; cdecl;
  TOPENSSL_sk_find = function(st: POPENSSL_STACK; data: Pointer): Integer; cdecl;
  TOPENSSL_sk_find_ex = function(st: POPENSSL_STACK; data: Pointer): Integer; cdecl;
  TOPENSSL_sk_push = function(st: POPENSSL_STACK; data: Pointer): Integer; cdecl;
  TOPENSSL_sk_unshift = function(st: POPENSSL_STACK; data: Pointer): Integer; cdecl;
  TOPENSSL_sk_shift = function(st: POPENSSL_STACK): Pointer; cdecl;
  TOPENSSL_sk_pop = function(st: POPENSSL_STACK): Pointer; cdecl;
  TOPENSSL_sk_zero = procedure(st: POPENSSL_STACK); cdecl;
  TOPENSSL_sk_set_cmp_func = function(sk: POPENSSL_STACK; cmp: Tstack_cmp_func): Tstack_cmp_func; cdecl;
  TOPENSSL_sk_dup = function(st: POPENSSL_STACK): POPENSSL_STACK; cdecl;
  TOPENSSL_sk_sort = procedure(st: POPENSSL_STACK); cdecl;
  TOPENSSL_sk_is_sorted = function(const st: POPENSSL_STACK): Integer; cdecl;
  TOPENSSL_sk_num = function(const st: POPENSSL_STACK): Integer; cdecl;
  TOPENSSL_sk_value = function(const st: POPENSSL_STACK; i: Integer): Pointer; cdecl;
  TOPENSSL_sk_set = function(st: POPENSSL_STACK; i: Integer; data: Pointer): Pointer; cdecl;
  
  // Typed stack macros as functions (for commonly used types)
  Tsk_X509_new_null = function: PSTACK_OF_X509; cdecl;
  Tsk_X509_free = procedure(st: PSTACK_OF_X509); cdecl;
  Tsk_X509_num = function(const st: PSTACK_OF_X509): Integer; cdecl;
  Tsk_X509_value = function(const st: PSTACK_OF_X509; i: Integer): PX509; cdecl;
  Tsk_X509_push = function(st: PSTACK_OF_X509; val: PX509): Integer; cdecl;
  Tsk_X509_pop = function(st: PSTACK_OF_X509): PX509; cdecl;
  Tsk_X509_shift = function(st: PSTACK_OF_X509): PX509; cdecl;
  Tsk_X509_unshift = function(st: PSTACK_OF_X509; val: PX509): Integer; cdecl;
  TX509_free_func = procedure(x: PX509); cdecl;
  Tsk_X509_pop_free = procedure(st: PSTACK_OF_X509; func: TX509_free_func); cdecl;
  
  Tsk_X509_CRL_new_null = function: PSTACK_OF_X509_CRL; cdecl;
  Tsk_X509_CRL_free = procedure(st: PSTACK_OF_X509_CRL); cdecl;
  Tsk_X509_CRL_num = function(const st: PSTACK_OF_X509_CRL): Integer; cdecl;
  Tsk_X509_CRL_value = function(const st: PSTACK_OF_X509_CRL; i: Integer): PX509_CRL; cdecl;
  Tsk_X509_CRL_push = function(st: PSTACK_OF_X509_CRL; val: PX509_CRL): Integer; cdecl;
  
  Tsk_X509_NAME_new_null = function: PSTACK_OF_X509_NAME; cdecl;
  Tsk_X509_NAME_free = procedure(st: PSTACK_OF_X509_NAME); cdecl;
  Tsk_X509_NAME_num = function(const st: PSTACK_OF_X509_NAME): Integer; cdecl;
  Tsk_X509_NAME_value = function(const st: PSTACK_OF_X509_NAME; i: Integer): PX509_NAME; cdecl;
  Tsk_X509_NAME_push = function(st: PSTACK_OF_X509_NAME; val: PX509_NAME): Integer; cdecl;
  
  Tsk_OPENSSL_STRING_new_null = function: PSTACK_OF_OPENSSL_STRING; cdecl;
  Tsk_OPENSSL_STRING_free = procedure(st: PSTACK_OF_OPENSSL_STRING); cdecl;
  Tsk_OPENSSL_STRING_num = function(const st: PSTACK_OF_OPENSSL_STRING): Integer; cdecl;
  Tsk_OPENSSL_STRING_value = function(const st: PSTACK_OF_OPENSSL_STRING; i: Integer): PAnsiChar; cdecl;
  Tsk_OPENSSL_STRING_push = function(st: PSTACK_OF_OPENSSL_STRING; val: PAnsiChar): Integer; cdecl;

var
  // Generic stack functions
  OPENSSL_sk_new: TOPENSSL_sk_new = nil;
  OPENSSL_sk_new_null: TOPENSSL_sk_new_null = nil;
  OPENSSL_sk_new_reserve: TOPENSSL_sk_new_reserve = nil;
  OPENSSL_sk_reserve: TOPENSSL_sk_reserve = nil;
  OPENSSL_sk_free: TOPENSSL_sk_free = nil;
  OPENSSL_sk_pop_free: TOPENSSL_sk_pop_free = nil;
  OPENSSL_sk_deep_copy: TOPENSSL_sk_deep_copy = nil;
  OPENSSL_sk_insert: TOPENSSL_sk_insert = nil;
  OPENSSL_sk_delete: TOPENSSL_sk_delete = nil;
  OPENSSL_sk_delete_ptr: TOPENSSL_sk_delete_ptr = nil;
  OPENSSL_sk_find: TOPENSSL_sk_find = nil;
  OPENSSL_sk_find_ex: TOPENSSL_sk_find_ex = nil;
  OPENSSL_sk_push: TOPENSSL_sk_push = nil;
  OPENSSL_sk_unshift: TOPENSSL_sk_unshift = nil;
  OPENSSL_sk_shift: TOPENSSL_sk_shift = nil;
  OPENSSL_sk_pop: TOPENSSL_sk_pop = nil;
  OPENSSL_sk_zero: TOPENSSL_sk_zero = nil;
  OPENSSL_sk_set_cmp_func: TOPENSSL_sk_set_cmp_func = nil;
  OPENSSL_sk_dup: TOPENSSL_sk_dup = nil;
  OPENSSL_sk_sort: TOPENSSL_sk_sort = nil;
  OPENSSL_sk_is_sorted: TOPENSSL_sk_is_sorted = nil;
  OPENSSL_sk_num: TOPENSSL_sk_num = nil;
  OPENSSL_sk_value: TOPENSSL_sk_value = nil;
  OPENSSL_sk_set: TOPENSSL_sk_set = nil;
  
  // Typed stack functions for X509
  sk_X509_new_null: Tsk_X509_new_null = nil;
  sk_X509_free: Tsk_X509_free = nil;
  sk_X509_num: Tsk_X509_num = nil;
  sk_X509_value: Tsk_X509_value = nil;
  sk_X509_push: Tsk_X509_push = nil;
  sk_X509_pop: Tsk_X509_pop = nil;
  sk_X509_shift: Tsk_X509_shift = nil;
  sk_X509_unshift: Tsk_X509_unshift = nil;
  sk_X509_pop_free: Tsk_X509_pop_free = nil;
  
  // Typed stack functions for X509_CRL
  sk_X509_CRL_new_null: Tsk_X509_CRL_new_null = nil;
  sk_X509_CRL_free: Tsk_X509_CRL_free = nil;
  sk_X509_CRL_num: Tsk_X509_CRL_num = nil;
  sk_X509_CRL_value: Tsk_X509_CRL_value = nil;
  sk_X509_CRL_push: Tsk_X509_CRL_push = nil;
  
  // Typed stack functions for X509_NAME
  sk_X509_NAME_new_null: Tsk_X509_NAME_new_null = nil;
  sk_X509_NAME_free: Tsk_X509_NAME_free = nil;
  sk_X509_NAME_num: Tsk_X509_NAME_num = nil;
  sk_X509_NAME_value: Tsk_X509_NAME_value = nil;
  sk_X509_NAME_push: Tsk_X509_NAME_push = nil;
  
  // Typed stack functions for OPENSSL_STRING
  sk_OPENSSL_STRING_new_null: Tsk_OPENSSL_STRING_new_null = nil;
  sk_OPENSSL_STRING_free: Tsk_OPENSSL_STRING_free = nil;
  sk_OPENSSL_STRING_num: Tsk_OPENSSL_STRING_num = nil;
  sk_OPENSSL_STRING_value: Tsk_OPENSSL_STRING_value = nil;
  sk_OPENSSL_STRING_push: Tsk_OPENSSL_STRING_push = nil;

// Load and unload functions
function LoadStackFunctions: Boolean;
procedure UnloadStackFunctions;
function IsStackLoaded: Boolean;

// High-level helper functions
function CreateStringStack(const Strings: array of string): PSTACK_OF_OPENSSL_STRING;
procedure FreeStringStack(Stack: PSTACK_OF_OPENSSL_STRING);
function GetStringFromStack(Stack: PSTACK_OF_OPENSSL_STRING; Index: Integer): string;
function GetStackCount(Stack: POPENSSL_STACK): Integer;

// Generic stack helpers
function CreateStack: POPENSSL_STACK;
procedure FreeStack(Stack: POPENSSL_STACK);
function PushToStack(Stack: POPENSSL_STACK; Data: Pointer): Boolean;
function PopFromStack(Stack: POPENSSL_STACK): Pointer;
function GetFromStack(Stack: POPENSSL_STACK; Index: Integer): Pointer;

implementation

uses
  {$IFDEF WINDOWS}Windows{$ELSE}dynlibs{$ENDIF};

var
  hCrypto: {$IFDEF WINDOWS}HMODULE{$ELSE}THandle{$ENDIF} = 0;
  StackLoaded: Boolean = False;

function LoadStackFunctions: Boolean;
begin
  Result := False;
  
  // Load crypto library if not already loaded
  if hCrypto = 0 then
  begin
    {$IFDEF WINDOWS}
    hCrypto := LoadLibrary('libcrypto-3-x64.dll');
    if hCrypto = 0 then
      hCrypto := LoadLibrary('libcrypto-1_1-x64.dll');
    if hCrypto = 0 then
      hCrypto := LoadLibrary('libeay32.dll');
    {$ELSE}
    hCrypto := LoadLibrary('libcrypto.so.3');
    if hCrypto = 0 then
      hCrypto := LoadLibrary('libcrypto.so.1.1');
    if hCrypto = 0 then
      hCrypto := LoadLibrary('libcrypto.so');
    {$ENDIF}
  end;
  
  if hCrypto = 0 then
    Exit;
    
  // Load generic stack functions
  OPENSSL_sk_new := TOPENSSL_sk_new(GetProcAddress(hCrypto, 'OPENSSL_sk_new'));
  OPENSSL_sk_new_null := TOPENSSL_sk_new_null(GetProcAddress(hCrypto, 'OPENSSL_sk_new_null'));
  OPENSSL_sk_new_reserve := TOPENSSL_sk_new_reserve(GetProcAddress(hCrypto, 'OPENSSL_sk_new_reserve'));
  OPENSSL_sk_reserve := TOPENSSL_sk_reserve(GetProcAddress(hCrypto, 'OPENSSL_sk_reserve'));
  OPENSSL_sk_free := TOPENSSL_sk_free(GetProcAddress(hCrypto, 'OPENSSL_sk_free'));
  OPENSSL_sk_pop_free := TOPENSSL_sk_pop_free(GetProcAddress(hCrypto, 'OPENSSL_sk_pop_free'));
  OPENSSL_sk_deep_copy := TOPENSSL_sk_deep_copy(GetProcAddress(hCrypto, 'OPENSSL_sk_deep_copy'));
  OPENSSL_sk_insert := TOPENSSL_sk_insert(GetProcAddress(hCrypto, 'OPENSSL_sk_insert'));
  OPENSSL_sk_delete := TOPENSSL_sk_delete(GetProcAddress(hCrypto, 'OPENSSL_sk_delete'));
  OPENSSL_sk_delete_ptr := TOPENSSL_sk_delete_ptr(GetProcAddress(hCrypto, 'OPENSSL_sk_delete_ptr'));
  OPENSSL_sk_find := TOPENSSL_sk_find(GetProcAddress(hCrypto, 'OPENSSL_sk_find'));
  OPENSSL_sk_find_ex := TOPENSSL_sk_find_ex(GetProcAddress(hCrypto, 'OPENSSL_sk_find_ex'));
  OPENSSL_sk_push := TOPENSSL_sk_push(GetProcAddress(hCrypto, 'OPENSSL_sk_push'));
  OPENSSL_sk_unshift := TOPENSSL_sk_unshift(GetProcAddress(hCrypto, 'OPENSSL_sk_unshift'));
  OPENSSL_sk_shift := TOPENSSL_sk_shift(GetProcAddress(hCrypto, 'OPENSSL_sk_shift'));
  OPENSSL_sk_pop := TOPENSSL_sk_pop(GetProcAddress(hCrypto, 'OPENSSL_sk_pop'));
  OPENSSL_sk_zero := TOPENSSL_sk_zero(GetProcAddress(hCrypto, 'OPENSSL_sk_zero'));
  OPENSSL_sk_set_cmp_func := TOPENSSL_sk_set_cmp_func(GetProcAddress(hCrypto, 'OPENSSL_sk_set_cmp_func'));
  OPENSSL_sk_dup := TOPENSSL_sk_dup(GetProcAddress(hCrypto, 'OPENSSL_sk_dup'));
  OPENSSL_sk_sort := TOPENSSL_sk_sort(GetProcAddress(hCrypto, 'OPENSSL_sk_sort'));
  OPENSSL_sk_is_sorted := TOPENSSL_sk_is_sorted(GetProcAddress(hCrypto, 'OPENSSL_sk_is_sorted'));
  OPENSSL_sk_num := TOPENSSL_sk_num(GetProcAddress(hCrypto, 'OPENSSL_sk_num'));
  OPENSSL_sk_value := TOPENSSL_sk_value(GetProcAddress(hCrypto, 'OPENSSL_sk_value'));
  OPENSSL_sk_set := TOPENSSL_sk_set(GetProcAddress(hCrypto, 'OPENSSL_sk_set'));
  
  // Try older sk_* functions for compatibility
  if not Assigned(OPENSSL_sk_new) then
  begin
    OPENSSL_sk_new := TOPENSSL_sk_new(GetProcAddress(hCrypto, 'sk_new'));
    OPENSSL_sk_new_null := TOPENSSL_sk_new_null(GetProcAddress(hCrypto, 'sk_new_null'));
    OPENSSL_sk_free := TOPENSSL_sk_free(GetProcAddress(hCrypto, 'sk_free'));
    OPENSSL_sk_pop_free := TOPENSSL_sk_pop_free(GetProcAddress(hCrypto, 'sk_pop_free'));
    OPENSSL_sk_num := TOPENSSL_sk_num(GetProcAddress(hCrypto, 'sk_num'));
    OPENSSL_sk_value := TOPENSSL_sk_value(GetProcAddress(hCrypto, 'sk_value'));
    OPENSSL_sk_push := TOPENSSL_sk_push(GetProcAddress(hCrypto, 'sk_push'));
    OPENSSL_sk_pop := TOPENSSL_sk_pop(GetProcAddress(hCrypto, 'sk_pop'));
    OPENSSL_sk_shift := TOPENSSL_sk_shift(GetProcAddress(hCrypto, 'sk_shift'));
    OPENSSL_sk_unshift := TOPENSSL_sk_unshift(GetProcAddress(hCrypto, 'sk_unshift'));
    OPENSSL_sk_insert := TOPENSSL_sk_insert(GetProcAddress(hCrypto, 'sk_insert'));
    OPENSSL_sk_delete := TOPENSSL_sk_delete(GetProcAddress(hCrypto, 'sk_delete'));
    OPENSSL_sk_find := TOPENSSL_sk_find(GetProcAddress(hCrypto, 'sk_find'));
    OPENSSL_sk_sort := TOPENSSL_sk_sort(GetProcAddress(hCrypto, 'sk_sort'));
    OPENSSL_sk_dup := TOPENSSL_sk_dup(GetProcAddress(hCrypto, 'sk_dup'));
    OPENSSL_sk_zero := TOPENSSL_sk_zero(GetProcAddress(hCrypto, 'sk_zero'));
    OPENSSL_sk_set := TOPENSSL_sk_set(GetProcAddress(hCrypto, 'sk_set'));
  end;
  
  // Load typed stack functions (these are typically inline/macros in C)
  // They may not exist as symbols, so we'll use the generic functions
  
  StackLoaded := Assigned(OPENSSL_sk_new_null) or Assigned(OPENSSL_sk_new);
  Result := StackLoaded;
end;

procedure UnloadStackFunctions;
begin
  // Clear generic stack functions
  OPENSSL_sk_new := nil;
  OPENSSL_sk_new_null := nil;
  OPENSSL_sk_new_reserve := nil;
  OPENSSL_sk_reserve := nil;
  OPENSSL_sk_free := nil;
  OPENSSL_sk_pop_free := nil;
  OPENSSL_sk_deep_copy := nil;
  OPENSSL_sk_insert := nil;
  OPENSSL_sk_delete := nil;
  OPENSSL_sk_delete_ptr := nil;
  OPENSSL_sk_find := nil;
  OPENSSL_sk_find_ex := nil;
  OPENSSL_sk_push := nil;
  OPENSSL_sk_unshift := nil;
  OPENSSL_sk_shift := nil;
  OPENSSL_sk_pop := nil;
  OPENSSL_sk_zero := nil;
  OPENSSL_sk_set_cmp_func := nil;
  OPENSSL_sk_dup := nil;
  OPENSSL_sk_sort := nil;
  OPENSSL_sk_is_sorted := nil;
  OPENSSL_sk_num := nil;
  OPENSSL_sk_value := nil;
  OPENSSL_sk_set := nil;
  
  // Clear typed stack functions
  sk_X509_new_null := nil;
  sk_X509_free := nil;
  sk_X509_num := nil;
  sk_X509_value := nil;
  sk_X509_push := nil;
  sk_X509_pop := nil;
  sk_X509_shift := nil;
  sk_X509_unshift := nil;
  sk_X509_pop_free := nil;
  
  sk_X509_CRL_new_null := nil;
  sk_X509_CRL_free := nil;
  sk_X509_CRL_num := nil;
  sk_X509_CRL_value := nil;
  sk_X509_CRL_push := nil;
  
  sk_X509_NAME_new_null := nil;
  sk_X509_NAME_free := nil;
  sk_X509_NAME_num := nil;
  sk_X509_NAME_value := nil;
  sk_X509_NAME_push := nil;
  
  sk_OPENSSL_STRING_new_null := nil;
  sk_OPENSSL_STRING_free := nil;
  sk_OPENSSL_STRING_num := nil;
  sk_OPENSSL_STRING_value := nil;
  sk_OPENSSL_STRING_push := nil;
  
  StackLoaded := False;
  
  if hCrypto <> 0 then
  begin
    FreeLibrary(hCrypto);
    hCrypto := 0;
  end;
end;

function IsStackLoaded: Boolean;
begin
  Result := StackLoaded;
end;

// High-level helper function implementations

function CreateStringStack(const Strings: array of string): PSTACK_OF_OPENSSL_STRING;
var
  i: Integer;
  str: PAnsiChar;
begin
  if not Assigned(OPENSSL_sk_new_null) then
    raise ESSLException.Create('Stack functions not loaded');
    
  Result := PSTACK_OF_OPENSSL_STRING(OPENSSL_sk_new_null());
  if Result = nil then
    raise ESSLException.Create('Failed to create string stack');
    
  try
    for i := Low(Strings) to High(Strings) do
    begin
      GetMem(str, Length(Strings[i]) + 1);
      StrPCopy(str, AnsiString(Strings[i]));
      if OPENSSL_sk_push(POPENSSL_STACK(Result), str) = 0 then
        raise ESSLException.Create('Failed to push string to stack');
    end;
  except
    FreeStringStack(Result);
    raise;
  end;
end;

procedure FreeStringStack(Stack: PSTACK_OF_OPENSSL_STRING);
var
  i, count: Integer;
  str: PAnsiChar;
begin
  if Stack = nil then
    Exit;
    
  if Assigned(OPENSSL_sk_num) then
  begin
    count := OPENSSL_sk_num(POPENSSL_STACK(Stack));
    for i := 0 to count - 1 do
    begin
      str := PAnsiChar(OPENSSL_sk_value(POPENSSL_STACK(Stack), i));
      if str <> nil then
        FreeMem(str);
    end;
  end;
  
  if Assigned(OPENSSL_sk_free) then
    OPENSSL_sk_free(POPENSSL_STACK(Stack));
end;

function GetStringFromStack(Stack: PSTACK_OF_OPENSSL_STRING; Index: Integer): string;
var
  str: PAnsiChar;
begin
  Result := '';
  
  if not Assigned(OPENSSL_sk_value) then
    Exit;
    
  str := PAnsiChar(OPENSSL_sk_value(POPENSSL_STACK(Stack), Index));
  if str <> nil then
    Result := string(str);
end;

function GetStackCount(Stack: POPENSSL_STACK): Integer;
begin
  Result := 0;
  
  if (Stack <> nil) and Assigned(OPENSSL_sk_num) then
    Result := OPENSSL_sk_num(Stack);
end;

function CreateStack: POPENSSL_STACK;
begin
  if not Assigned(OPENSSL_sk_new_null) then
    raise ESSLException.Create('Stack functions not loaded');
    
  Result := OPENSSL_sk_new_null();
end;

procedure FreeStack(Stack: POPENSSL_STACK);
begin
  if (Stack <> nil) and Assigned(OPENSSL_sk_free) then
    OPENSSL_sk_free(Stack);
end;

function PushToStack(Stack: POPENSSL_STACK; Data: Pointer): Boolean;
begin
  Result := False;
  
  if (Stack <> nil) and Assigned(OPENSSL_sk_push) then
    Result := OPENSSL_sk_push(Stack, Data) <> 0;
end;

function PopFromStack(Stack: POPENSSL_STACK): Pointer;
begin
  Result := nil;
  
  if (Stack <> nil) and Assigned(OPENSSL_sk_pop) then
    Result := OPENSSL_sk_pop(Stack);
end;

function GetFromStack(Stack: POPENSSL_STACK; Index: Integer): Pointer;
begin
  Result := nil;
  
  if (Stack <> nil) and Assigned(OPENSSL_sk_value) then
    Result := OPENSSL_sk_value(Stack, Index);
end;

initialization
  
finalization
  UnloadStackFunctions;
  
end.