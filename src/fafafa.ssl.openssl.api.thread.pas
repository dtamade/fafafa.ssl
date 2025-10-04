{******************************************************************************}
{                                                                              }
{  fafafa.ssl.openssl.api.thread - OpenSSL Thread Module Pascal Binding           }
{                                                                              }
{  Copyright (c) 2024 fafafa                                                  }
{                                                                              }
{******************************************************************************}
unit fafafa.ssl.openssl.api.thread;

{$MODE OBJFPC}{$H+}

interface

uses
  SysUtils, Classes,
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  {$IFDEF UNIX}cthreads,{$ENDIF}
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api.consts;

type
  { Thread types }
  CRYPTO_RWLOCK = record end;
  PCRYPTO_RWLOCK = ^CRYPTO_RWLOCK;
  
  CRYPTO_THREAD_LOCAL = TOpenSSLULong;
  PCRYPTO_THREAD_LOCAL = ^CRYPTO_THREAD_LOCAL;
  
  CRYPTO_THREAD_ID = TOpenSSLULong;
  PCRYPTO_THREAD_ID = ^CRYPTO_THREAD_ID;
  
  CRYPTO_ONCE = TOpenSSLInt;
  PCRYPTO_ONCE = ^CRYPTO_ONCE;
  
  { Thread callback types }
  TCRYPTO_thread_fn = procedure(arg: Pointer); cdecl;
  TCRYPTO_once_fn = procedure; cdecl;
  TCRYPTO_thread_stop_fn = procedure(arg: Pointer); cdecl;
  
  { Legacy thread types }
  TCRYPTO_lock_fn = procedure(mode: TOpenSSLInt; type_: TOpenSSLInt; const file_: PChar; line: TOpenSSLInt); cdecl;
  TCRYPTO_id_fn = function: TOpenSSLULong; cdecl;
  TCRYPTO_dynlock_create_fn = function(const file_: PChar; line: TOpenSSLInt): Pointer; cdecl;
  TCRYPTO_dynlock_lock_fn = procedure(mode: TOpenSSLInt; l: Pointer; const file_: PChar; line: TOpenSSLInt); cdecl;
  TCRYPTO_dynlock_destroy_fn = procedure(l: Pointer; const file_: PChar; line: TOpenSSLInt); cdecl;
  
  { Function pointers for dynamic loading }
  { Thread functions }
  TCRYPTOThreadSetup = function: TOpenSSLInt; cdecl;
  TCRYPTOThreadCleanup = function: TOpenSSLInt; cdecl;
  TCRYPTOThreadRunOnce = function(once: PCRYPTO_ONCE; init: TCRYPTO_once_fn): TOpenSSLInt; cdecl;
  TCRYPTOThreadGetCurrentID = function: CRYPTO_THREAD_ID; cdecl;
  TCRYPTOThreadCompareID = function(a, b: CRYPTO_THREAD_ID): TOpenSSLInt; cdecl;
  
  { RW Lock functions }
  TCRYPTOThreadRWLockNew = function: PCRYPTO_RWLOCK; cdecl;
  TCRYPTOThreadReadLock = function(lock: PCRYPTO_RWLOCK): TOpenSSLInt; cdecl;
  TCRYPTOThreadWriteLock = function(lock: PCRYPTO_RWLOCK): TOpenSSLInt; cdecl;
  TCRYPTOThreadUnlock = function(lock: PCRYPTO_RWLOCK): TOpenSSLInt; cdecl;
  TCRYPTOThreadRWLockFree = procedure(lock: PCRYPTO_RWLOCK); cdecl;
  
  { Thread local storage }
  TCRYPTOThreadLocalNew = function(key: PCRYPTO_THREAD_LOCAL; cleanup: TCRYPTO_thread_stop_fn): TOpenSSLInt; cdecl;
  TCRYPTOThreadLocalSet = function(key: PCRYPTO_THREAD_LOCAL; val: Pointer): TOpenSSLInt; cdecl;
  TCRYPTOThreadLocalGet = function(key: PCRYPTO_THREAD_LOCAL): Pointer; cdecl;
  TCRYPTOThreadLocalFree = function(key: PCRYPTO_THREAD_LOCAL): TOpenSSLInt; cdecl;
  
  { Atomic operations }
  TCRYPTOAtomicAdd = function(val: POpenSSLInt; amount: TOpenSSLInt; ret: POpenSSLInt; lock: PCRYPTO_RWLOCK): TOpenSSLInt; cdecl;
  TCRYPTOAtomicOr = function(val: PUInt64; op: UInt64; ret: PUInt64; lock: PCRYPTO_RWLOCK): TOpenSSLInt; cdecl;
  TCRYPTOAtomicLoad = function(val: PUInt64; ret: PUInt64; lock: PCRYPTO_RWLOCK): TOpenSSLInt; cdecl;
  
  { Legacy thread functions }
  TCRYPTOSetLockingCallback = procedure(func: TCRYPTO_lock_fn); cdecl;
  TCRYPTOSetIDCallback = procedure(func: TCRYPTO_id_fn); cdecl;
  TCRYPTONumLocks = function: TOpenSSLInt; cdecl;
  TCRYPTOSetDynlockCreateCallback = procedure(func: TCRYPTO_dynlock_create_fn); cdecl;
  TCRYPTOSetDynlockLockCallback = procedure(func: TCRYPTO_dynlock_lock_fn); cdecl;
  TCRYPTOSetDynlockDestroyCallback = procedure(func: TCRYPTO_dynlock_destroy_fn); cdecl;
  TCRYPTOLock = procedure(type_: TOpenSSLInt); cdecl;
  TCRYPTOUnlock = procedure(type_: TOpenSSLInt); cdecl;
  TCRYPTOThreadID = function: TOpenSSLULong; cdecl;

var
  { Function variables }
  { Thread functions }
  CRYPTO_thread_setup: TCRYPTOThreadSetup = nil;
  CRYPTO_thread_cleanup: TCRYPTOThreadCleanup = nil;
  CRYPTO_THREAD_run_once: TCRYPTOThreadRunOnce = nil;
  CRYPTO_THREAD_get_current_id: TCRYPTOThreadGetCurrentID = nil;
  CRYPTO_THREAD_compare_id: TCRYPTOThreadCompareID = nil;
  
  { RW Lock functions }
  CRYPTO_THREAD_lock_new: TCRYPTOThreadRWLockNew = nil;
  CRYPTO_THREAD_read_lock: TCRYPTOThreadReadLock = nil;
  CRYPTO_THREAD_write_lock: TCRYPTOThreadWriteLock = nil;
  CRYPTO_THREAD_unlock: TCRYPTOThreadUnlock = nil;
  CRYPTO_THREAD_lock_free: TCRYPTOThreadRWLockFree = nil;
  
  { Thread local storage }
  CRYPTO_THREAD_local_new: TCRYPTOThreadLocalNew = nil;
  CRYPTO_THREAD_set_local: TCRYPTOThreadLocalSet = nil;
  CRYPTO_THREAD_get_local: TCRYPTOThreadLocalGet = nil;
  CRYPTO_THREAD_cleanup_local: TCRYPTOThreadLocalFree = nil;
  
  { Atomic operations }
  CRYPTO_atomic_add: TCRYPTOAtomicAdd = nil;
  CRYPTO_atomic_or: TCRYPTOAtomicOr = nil;
  CRYPTO_atomic_load: TCRYPTOAtomicLoad = nil;
  
  { Legacy functions }
  CRYPTO_set_locking_callback: TCRYPTOSetLockingCallback = nil;
  CRYPTO_set_id_callback: TCRYPTOSetIDCallback = nil;
  CRYPTO_num_locks: TCRYPTONumLocks = nil;
  CRYPTO_set_dynlock_create_callback: TCRYPTOSetDynlockCreateCallback = nil;
  CRYPTO_set_dynlock_lock_callback: TCRYPTOSetDynlockLockCallback = nil;
  CRYPTO_set_dynlock_destroy_callback: TCRYPTOSetDynlockDestroyCallback = nil;
  CRYPTO_lock: TCRYPTOLock = nil;
  CRYPTO_unlock: TCRYPTOUnlock = nil;
  CRYPTO_thread_id_func: TCRYPTOThreadID = nil;

{ Load/Unload functions }
function LoadThread(const ALibCrypto: THandle): Boolean;
procedure UnloadThread;

{ Helper functions }
function CreateThreadLock: PCRYPTO_RWLOCK;
procedure FreeThreadLock(Lock: PCRYPTO_RWLOCK);
function LockForRead(Lock: PCRYPTO_RWLOCK): Boolean;
function LockForWrite(Lock: PCRYPTO_RWLOCK): Boolean;
function UnlockThread(Lock: PCRYPTO_RWLOCK): Boolean;
function GetCurrentThreadID: CRYPTO_THREAD_ID;
function AtomicIncrement(var Value: TOpenSSLInt; Lock: PCRYPTO_RWLOCK = nil): TOpenSSLInt;
function AtomicDecrement(var Value: TOpenSSLInt; Lock: PCRYPTO_RWLOCK = nil): TOpenSSLInt;

implementation

uses
  fafafa.ssl.openssl.api.utils;

var
  ThreadLoaded: Boolean = False;

function LoadThread(const ALibCrypto: THandle): Boolean;
begin
  Result := False;
  if ThreadLoaded then Exit(True);
  if ALibCrypto = 0 then Exit;

  { Load thread functions }
  CRYPTO_thread_setup := TCRYPTOThreadSetup(GetProcAddress(ALibCrypto, 'CRYPTO_thread_setup'));
  CRYPTO_thread_cleanup := TCRYPTOThreadCleanup(GetProcAddress(ALibCrypto, 'CRYPTO_thread_cleanup'));
  CRYPTO_THREAD_run_once := TCRYPTOThreadRunOnce(GetProcAddress(ALibCrypto, 'CRYPTO_THREAD_run_once'));
  CRYPTO_THREAD_get_current_id := TCRYPTOThreadGetCurrentID(GetProcAddress(ALibCrypto, 'CRYPTO_THREAD_get_current_id'));
  CRYPTO_THREAD_compare_id := TCRYPTOThreadCompareID(GetProcAddress(ALibCrypto, 'CRYPTO_THREAD_compare_id'));
  
  { Load RW Lock functions }
  CRYPTO_THREAD_lock_new := TCRYPTOThreadRWLockNew(GetProcAddress(ALibCrypto, 'CRYPTO_THREAD_lock_new'));
  CRYPTO_THREAD_read_lock := TCRYPTOThreadReadLock(GetProcAddress(ALibCrypto, 'CRYPTO_THREAD_read_lock'));
  CRYPTO_THREAD_write_lock := TCRYPTOThreadWriteLock(GetProcAddress(ALibCrypto, 'CRYPTO_THREAD_write_lock'));
  CRYPTO_THREAD_unlock := TCRYPTOThreadUnlock(GetProcAddress(ALibCrypto, 'CRYPTO_THREAD_unlock'));
  CRYPTO_THREAD_lock_free := TCRYPTOThreadRWLockFree(GetProcAddress(ALibCrypto, 'CRYPTO_THREAD_lock_free'));
  
  { Load thread local storage }
  CRYPTO_THREAD_local_new := TCRYPTOThreadLocalNew(GetProcAddress(ALibCrypto, 'CRYPTO_THREAD_local_new'));
  CRYPTO_THREAD_set_local := TCRYPTOThreadLocalSet(GetProcAddress(ALibCrypto, 'CRYPTO_THREAD_set_local'));
  CRYPTO_THREAD_get_local := TCRYPTOThreadLocalGet(GetProcAddress(ALibCrypto, 'CRYPTO_THREAD_get_local'));
  CRYPTO_THREAD_cleanup_local := TCRYPTOThreadLocalFree(GetProcAddress(ALibCrypto, 'CRYPTO_THREAD_cleanup_local'));
  
  { Load atomic operations }
  CRYPTO_atomic_add := TCRYPTOAtomicAdd(GetProcAddress(ALibCrypto, 'CRYPTO_atomic_add'));
  CRYPTO_atomic_or := TCRYPTOAtomicOr(GetProcAddress(ALibCrypto, 'CRYPTO_atomic_or'));
  CRYPTO_atomic_load := TCRYPTOAtomicLoad(GetProcAddress(ALibCrypto, 'CRYPTO_atomic_load'));
  
  { Load legacy functions }
  CRYPTO_set_locking_callback := TCRYPTOSetLockingCallback(GetProcAddress(ALibCrypto, 'CRYPTO_set_locking_callback'));
  CRYPTO_set_id_callback := TCRYPTOSetIDCallback(GetProcAddress(ALibCrypto, 'CRYPTO_set_id_callback'));
  CRYPTO_num_locks := TCRYPTONumLocks(GetProcAddress(ALibCrypto, 'CRYPTO_num_locks'));
  CRYPTO_set_dynlock_create_callback := TCRYPTOSetDynlockCreateCallback(GetProcAddress(ALibCrypto, 'CRYPTO_set_dynlock_create_callback'));
  CRYPTO_set_dynlock_lock_callback := TCRYPTOSetDynlockLockCallback(GetProcAddress(ALibCrypto, 'CRYPTO_set_dynlock_lock_callback'));
  CRYPTO_set_dynlock_destroy_callback := TCRYPTOSetDynlockDestroyCallback(GetProcAddress(ALibCrypto, 'CRYPTO_set_dynlock_destroy_callback'));
  CRYPTO_lock := TCRYPTOLock(GetProcAddress(ALibCrypto, 'CRYPTO_lock'));
  CRYPTO_unlock := TCRYPTOUnlock(GetProcAddress(ALibCrypto, 'CRYPTO_unlock'));
  CRYPTO_thread_id_func := TCRYPTOThreadID(GetProcAddress(ALibCrypto, 'CRYPTO_thread_id'));

  { Thread functions are optional, so we consider it loaded even if some are missing }
  Result := True;
  ThreadLoaded := Result;
end;

procedure UnloadThread;
begin
  if not ThreadLoaded then Exit;

  CRYPTO_thread_setup := nil;
  CRYPTO_thread_cleanup := nil;
  CRYPTO_THREAD_run_once := nil;
  CRYPTO_THREAD_get_current_id := nil;
  CRYPTO_THREAD_compare_id := nil;
  
  CRYPTO_THREAD_lock_new := nil;
  CRYPTO_THREAD_read_lock := nil;
  CRYPTO_THREAD_write_lock := nil;
  CRYPTO_THREAD_unlock := nil;
  CRYPTO_THREAD_lock_free := nil;
  
  CRYPTO_THREAD_local_new := nil;
  CRYPTO_THREAD_set_local := nil;
  CRYPTO_THREAD_get_local := nil;
  CRYPTO_THREAD_cleanup_local := nil;
  
  CRYPTO_atomic_add := nil;
  CRYPTO_atomic_or := nil;
  CRYPTO_atomic_load := nil;
  
  CRYPTO_set_locking_callback := nil;
  CRYPTO_set_id_callback := nil;
  CRYPTO_num_locks := nil;
  CRYPTO_set_dynlock_create_callback := nil;
  CRYPTO_set_dynlock_lock_callback := nil;
  CRYPTO_set_dynlock_destroy_callback := nil;
  CRYPTO_lock := nil;
  CRYPTO_unlock := nil;
  CRYPTO_thread_id_func := nil;

  ThreadLoaded := False;
end;

{ Helper functions }

function CreateThreadLock: PCRYPTO_RWLOCK;
begin
  Result := nil;
  if Assigned(CRYPTO_THREAD_lock_new) then
    Result := CRYPTO_THREAD_lock_new();
end;

procedure FreeThreadLock(Lock: PCRYPTO_RWLOCK);
begin
  if (Lock <> nil) and Assigned(CRYPTO_THREAD_lock_free) then
    CRYPTO_THREAD_lock_free(Lock);
end;

function LockForRead(Lock: PCRYPTO_RWLOCK): Boolean;
begin
  Result := False;
  if (Lock <> nil) and Assigned(CRYPTO_THREAD_read_lock) then
    Result := CRYPTO_THREAD_read_lock(Lock) = 1;
end;

function LockForWrite(Lock: PCRYPTO_RWLOCK): Boolean;
begin
  Result := False;
  if (Lock <> nil) and Assigned(CRYPTO_THREAD_write_lock) then
    Result := CRYPTO_THREAD_write_lock(Lock) = 1;
end;

function UnlockThread(Lock: PCRYPTO_RWLOCK): Boolean;
begin
  Result := False;
  if (Lock <> nil) and Assigned(CRYPTO_THREAD_unlock) then
    Result := CRYPTO_THREAD_unlock(Lock) = 1;
end;

function GetCurrentThreadID: CRYPTO_THREAD_ID;
begin
  if Assigned(CRYPTO_THREAD_get_current_id) then
    Result := CRYPTO_THREAD_get_current_id()
  else
  {$IFDEF WINDOWS}
    Result := GetCurrentThreadId;
  {$ELSE}
    Result := TOpenSSLULong(GetThreadID);
  {$ENDIF}
end;

function AtomicIncrement(var Value: TOpenSSLInt; Lock: PCRYPTO_RWLOCK): TOpenSSLInt;
begin
  if Assigned(CRYPTO_atomic_add) then
    CRYPTO_atomic_add(@Value, 1, @Result, Lock)
  else
  begin
    if Lock <> nil then
      LockForWrite(Lock);
    Inc(Value);
    Result := Value;
    if Lock <> nil then
      UnlockThread(Lock);
  end;
end;

function AtomicDecrement(var Value: TOpenSSLInt; Lock: PCRYPTO_RWLOCK): TOpenSSLInt;
begin
  if Assigned(CRYPTO_atomic_add) then
    CRYPTO_atomic_add(@Value, -1, @Result, Lock)
  else
  begin
    if Lock <> nil then
      LockForWrite(Lock);
    Dec(Value);
    Result := Value;
    if Lock <> nil then
      UnlockThread(Lock);
  end;
end;

initialization

finalization
  UnloadThread;

end.