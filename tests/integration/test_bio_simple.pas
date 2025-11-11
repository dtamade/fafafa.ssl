program test_bio_simple;

{$mode objfpc}{$H+}

uses
  SysUtils, DynLibs, ctypes,
  fafafa.ssl.openssl.api.types,
  fafafa.ssl.openssl.consts,
  fafafa.ssl.openssl.api.core;

type
  { BIO Basic Function Types }
  TBIO_new = function(const &type: PBIO_METHOD): PBIO; cdecl;
  TBIO_free = function(a: PBIO): Integer; cdecl;
  TBIO_read = function(b: PBIO; data: Pointer; dlen: Integer): Integer; cdecl;
  TBIO_write = function(b: PBIO; const data: Pointer; dlen: Integer): Integer; cdecl;
  TBIO_puts = function(b: PBIO; const buf: PAnsiChar): Integer; cdecl;
  TBIO_gets = function(b: PBIO; buf: PAnsiChar; size: Integer): Integer; cdecl;
  TBIO_ctrl = function(b: PBIO; cmd: Integer; larg: clong; parg: Pointer): clong; cdecl;
  TBIO_push = function(b: PBIO; append: PBIO): PBIO; cdecl;
  TBIO_pop = function(b: PBIO): PBIO; cdecl;
  
  TBIO_s_mem = function: PBIO_METHOD; cdecl;
  TBIO_s_null = function: PBIO_METHOD; cdecl;
  TBIO_new_mem_buf = function(const buf: Pointer; len: Integer): PBIO; cdecl;
  
  TBIO_s_file = function: PBIO_METHOD; cdecl;
  TBIO_new_file = function(const filename: PAnsiChar; const mode: PAnsiChar): PBIO; cdecl;

const
  BIO_CTRL_RESET = 1;
  BIO_CTRL_EOF = 2;
  BIO_CTRL_INFO = 3;
  BIO_CTRL_FLUSH = 11;
  BIO_CTRL_DGRAM_QUERY_MTU = 40;
  BIO_CTRL_WPENDING = 13;
  BIO_CTRL_PENDING = 10;
  BIO_C_SET_BUF_MEM = 114;
  BIO_C_GET_BUF_MEM_PTR = 115;

var
  { Function pointers }
  BIO_new: TBIO_new = nil;
  BIO_free: TBIO_free = nil;
  BIO_read: TBIO_read = nil;
  BIO_write: TBIO_write = nil;
  BIO_puts: TBIO_puts = nil;
  BIO_gets: TBIO_gets = nil;
  BIO_ctrl: TBIO_ctrl = nil;
  BIO_push: TBIO_push = nil;
  BIO_pop: TBIO_pop = nil;
  
  BIO_s_mem: TBIO_s_mem = nil;
  BIO_s_null: TBIO_s_null = nil;
  BIO_new_mem_buf: TBIO_new_mem_buf = nil;
  
  BIO_s_file: TBIO_s_file = nil;
  BIO_new_file: TBIO_new_file = nil;

function LoadBIOFunctions: Boolean;
var
  LibHandle: TLibHandle;
begin
  Result := False;
  LibHandle := GetCryptoLibHandle;
  if LibHandle = 0 then Exit;
  
  // Load BIO core functions
  BIO_new := TBIO_new(GetProcedureAddress(LibHandle, 'BIO_new'));
  BIO_free := TBIO_free(GetProcedureAddress(LibHandle, 'BIO_free'));
  BIO_read := TBIO_read(GetProcedureAddress(LibHandle, 'BIO_read'));
  BIO_write := TBIO_write(GetProcedureAddress(LibHandle, 'BIO_write'));
  BIO_puts := TBIO_puts(GetProcedureAddress(LibHandle, 'BIO_puts'));
  BIO_gets := TBIO_gets(GetProcedureAddress(LibHandle, 'BIO_gets'));
  BIO_ctrl := TBIO_ctrl(GetProcedureAddress(LibHandle, 'BIO_ctrl'));
  BIO_push := TBIO_push(GetProcedureAddress(LibHandle, 'BIO_push'));
  BIO_pop := TBIO_pop(GetProcedureAddress(LibHandle, 'BIO_pop'));
  
  // Load BIO type methods
  BIO_s_mem := TBIO_s_mem(GetProcedureAddress(LibHandle, 'BIO_s_mem'));
  BIO_s_null := TBIO_s_null(GetProcedureAddress(LibHandle, 'BIO_s_null'));
  BIO_new_mem_buf := TBIO_new_mem_buf(GetProcedureAddress(LibHandle, 'BIO_new_mem_buf'));
  
  BIO_s_file := TBIO_s_file(GetProcedureAddress(LibHandle, 'BIO_s_file'));
  BIO_new_file := TBIO_new_file(GetProcedureAddress(LibHandle, 'BIO_new_file'));
  
  Result := Assigned(BIO_new) and Assigned(BIO_s_mem);
end;

// Helper macros as functions
function BIO_flush(b: PBIO): Integer;
begin
  Result := BIO_ctrl(b, BIO_CTRL_FLUSH, 0, nil);
end;

function BIO_reset(b: PBIO): Integer;
begin
  Result := BIO_ctrl(b, BIO_CTRL_RESET, 0, nil);
end;

function BIO_eof(b: PBIO): Integer;
begin
  Result := BIO_ctrl(b, BIO_CTRL_EOF, 0, nil);
end;

function BIO_pending(b: PBIO): Integer;
begin
  Result := BIO_ctrl(b, BIO_CTRL_PENDING, 0, nil);
end;

procedure PrintTestHeader(const TestName: string);
begin
  WriteLn('');
  WriteLn('========================================');
  WriteLn('Test: ', TestName);
  WriteLn('========================================');
end;

procedure PrintTestResult(const TestName: string; Success: Boolean);
begin
  if Success then
    WriteLn('[PASS] ', TestName)
  else
    WriteLn('[FAIL] ', TestName);
end;

// Test 1: Memory BIO Creation and Destruction
function Test_BIO_MemoryBasic: Boolean;
var
  bio: PBIO;
  method: PBIO_METHOD;
begin
  PrintTestHeader('Memory BIO Creation and Destruction');
  Result := False;
  
  Write('Getting memory BIO method... ');
  method := BIO_s_mem();
  if method = nil then
  begin
    WriteLn('FAILED');
    Exit;
  end;
  WriteLn('OK');
  
  Write('Creating memory BIO... ');
  bio := BIO_new(method);
  if bio = nil then
  begin
    WriteLn('FAILED');
    Exit;
  end;
  WriteLn('OK');
  
  Write('Freeing memory BIO... ');
  if BIO_free(bio) <> 1 then
  begin
    WriteLn('FAILED');
    Exit;
  end;
  WriteLn('OK');
  
  Result := True;
  PrintTestResult('Memory BIO Creation and Destruction', Result);
end;

// Test 2: Memory BIO Write and Read
function Test_BIO_MemoryReadWrite: Boolean;
var
  bio: PBIO;
  test_data: AnsiString;
  read_buf: array[0..255] of AnsiChar;
  bytes_written, bytes_read: Integer;
begin
  PrintTestHeader('Memory BIO Write and Read');
  Result := False;
  
  test_data := 'Hello BIO World!';
  
  bio := BIO_new(BIO_s_mem());
  if bio = nil then
  begin
    WriteLn('FAILED to create BIO');
    Exit;
  end;
  
  Write('Writing to memory BIO... ');
  bytes_written := BIO_write(bio, PAnsiChar(test_data), Length(test_data));
  if bytes_written <> Length(test_data) then
  begin
    WriteLn('FAILED (wrote ', bytes_written, ' bytes, expected ', Length(test_data), ')');
    BIO_free(bio);
    Exit;
  end;
  WriteLn('OK (', bytes_written, ' bytes)');
  
  Write('Checking pending data... ');
  bytes_read := BIO_pending(bio);
  if bytes_read <> Length(test_data) then
  begin
    WriteLn('FAILED (pending=', bytes_read, ', expected ', Length(test_data), ')');
    BIO_free(bio);
    Exit;
  end;
  WriteLn('OK (', bytes_read, ' bytes pending)');
  
  Write('Reading from memory BIO... ');
  FillChar(read_buf, SizeOf(read_buf), 0);
  bytes_read := BIO_read(bio, @read_buf[0], SizeOf(read_buf));
  if bytes_read <> Length(test_data) then
  begin
    WriteLn('FAILED (read ', bytes_read, ' bytes, expected ', Length(test_data), ')');
    BIO_free(bio);
    Exit;
  end;
  WriteLn('OK (', bytes_read, ' bytes)');
  WriteLn('Data read: ', read_buf);
  
  BIO_free(bio);
  Result := True;
  PrintTestResult('Memory BIO Write and Read', Result);
end;

// Test 3: Memory BIO puts and gets
function Test_BIO_MemoryPutsGets: Boolean;
var
  bio: PBIO;
  test_line: AnsiString;
  read_buf: array[0..255] of AnsiChar;
  result_code: Integer;
begin
  PrintTestHeader('Memory BIO puts and gets');
  Result := False;
  
  test_line := 'Test Line';
  
  bio := BIO_new(BIO_s_mem());
  if bio = nil then
  begin
    WriteLn('FAILED to create BIO');
    Exit;
  end;
  
  Write('Writing string with BIO_puts... ');
  result_code := BIO_puts(bio, PAnsiChar(test_line));
  if result_code <= 0 then
  begin
    WriteLn('FAILED');
    BIO_free(bio);
    Exit;
  end;
  WriteLn('OK (', result_code, ' bytes)');
  
  Write('Reading string with BIO_gets... ');
  FillChar(read_buf, SizeOf(read_buf), 0);
  result_code := BIO_gets(bio, @read_buf[0], SizeOf(read_buf));
  if result_code <= 0 then
  begin
    WriteLn('FAILED');
    BIO_free(bio);
    Exit;
  end;
  WriteLn('OK (', result_code, ' bytes)');
  WriteLn('Data read: ', read_buf);
  
  BIO_free(bio);
  Result := True;
  PrintTestResult('Memory BIO puts and gets', Result);
end;

// Test 4: Memory BIO Reset
function Test_BIO_MemoryReset: Boolean;
var
  bio: PBIO;
  test_data: AnsiString;
  pending: Integer;
begin
  PrintTestHeader('Memory BIO Reset');
  Result := False;
  
  test_data := 'Test Data';
  
  bio := BIO_new(BIO_s_mem());
  if bio = nil then
  begin
    WriteLn('FAILED to create BIO');
    Exit;
  end;
  
  Write('Writing data... ');
  if BIO_write(bio, PAnsiChar(test_data), Length(test_data)) <> Length(test_data) then
  begin
    WriteLn('FAILED');
    BIO_free(bio);
    Exit;
  end;
  WriteLn('OK');
  
  Write('Checking data pending... ');
  pending := BIO_pending(bio);
  if pending <> Length(test_data) then
  begin
    WriteLn('FAILED (expected ', Length(test_data), ', got ', pending, ')');
    BIO_free(bio);
    Exit;
  end;
  WriteLn('OK (', pending, ' bytes)');
  
  Write('Resetting BIO... ');
  if BIO_reset(bio) <> 1 then
  begin
    WriteLn('FAILED');
    BIO_free(bio);
    Exit;
  end;
  WriteLn('OK');
  
  Write('Checking data after reset... ');
  pending := BIO_pending(bio);
  if pending <> 0 then
  begin
    WriteLn('FAILED (expected 0, got ', pending, ')');
    BIO_free(bio);
    Exit;
  end;
  WriteLn('OK (buffer cleared)');
  
  BIO_free(bio);
  Result := True;
  PrintTestResult('Memory BIO Reset', Result);
end;

// Test 5: Memory BIO from Buffer
function Test_BIO_MemoryFromBuffer: Boolean;
var
  bio: PBIO;
  test_data: AnsiString;
  read_buf: array[0..255] of AnsiChar;
  bytes_read: Integer;
begin
  PrintTestHeader('Memory BIO from Buffer');
  Result := False;
  
  test_data := 'Read-only buffer data';
  
  Write('Creating memory BIO from buffer... ');
  bio := BIO_new_mem_buf(PAnsiChar(test_data), Length(test_data));
  if bio = nil then
  begin
    WriteLn('FAILED');
    Exit;
  end;
  WriteLn('OK');
  
  Write('Reading from buffer BIO... ');
  FillChar(read_buf, SizeOf(read_buf), 0);
  bytes_read := BIO_read(bio, @read_buf[0], SizeOf(read_buf));
  if bytes_read <> Length(test_data) then
  begin
    WriteLn('FAILED (read ', bytes_read, ' bytes, expected ', Length(test_data), ')');
    BIO_free(bio);
    Exit;
  end;
  WriteLn('OK (', bytes_read, ' bytes)');
  WriteLn('Data read: ', read_buf);
  
  Write('Checking EOF... ');
  if BIO_eof(bio) <> 1 then
  begin
    WriteLn('FAILED (expected EOF)');
    BIO_free(bio);
    Exit;
  end;
  WriteLn('OK (EOF reached)');
  
  BIO_free(bio);
  Result := True;
  PrintTestResult('Memory BIO from Buffer', Result);
end;

// Test 6: Null BIO
function Test_BIO_Null: Boolean;
var
  bio: PBIO;
  test_data: AnsiString;
  bytes_written: Integer;
begin
  PrintTestHeader('Null BIO');
  Result := False;
  
  test_data := 'Data to nowhere';
  
  Write('Creating null BIO... ');
  bio := BIO_new(BIO_s_null());
  if bio = nil then
  begin
    WriteLn('FAILED');
    Exit;
  end;
  WriteLn('OK');
  
  Write('Writing to null BIO... ');
  bytes_written := BIO_write(bio, PAnsiChar(test_data), Length(test_data));
  if bytes_written <> Length(test_data) then
  begin
    WriteLn('FAILED (wrote ', bytes_written, ' bytes, expected ', Length(test_data), ')');
    BIO_free(bio);
    Exit;
  end;
  WriteLn('OK (', bytes_written, ' bytes discarded)');
  
  BIO_free(bio);
  Result := True;
  PrintTestResult('Null BIO', Result);
end;

// Test 7: BIO Chain (Push/Pop)
function Test_BIO_Chain: Boolean;
var
  bio1, bio2, result_bio: PBIO;
begin
  PrintTestHeader('BIO Chain (Push/Pop)');
  Result := False;
  
  Write('Creating two memory BIOs... ');
  bio1 := BIO_new(BIO_s_mem());
  bio2 := BIO_new(BIO_s_mem());
  if (bio1 = nil) or (bio2 = nil) then
  begin
    WriteLn('FAILED');
    if bio1 <> nil then BIO_free(bio1);
    if bio2 <> nil then BIO_free(bio2);
    Exit;
  end;
  WriteLn('OK');
  
  Write('Pushing bio2 onto bio1... ');
  result_bio := BIO_push(bio1, bio2);
  if result_bio = nil then
  begin
    WriteLn('FAILED');
    BIO_free(bio1);
    BIO_free(bio2);
    Exit;
  end;
  WriteLn('OK');
  
  Write('Popping from chain... ');
  result_bio := BIO_pop(bio1);
  if result_bio = nil then
  begin
    WriteLn('FAILED');
    BIO_free(bio1);
    Exit;
  end;
  WriteLn('OK');
  
  BIO_free(bio1);
  BIO_free(bio2);
  Result := True;
  PrintTestResult('BIO Chain (Push/Pop)', Result);
end;

var
  PassedTests, TotalTests: Integer;
  
begin
  WriteLn('BIO Module Integration Test');
  WriteLn('============================');
  WriteLn('');
  
  // Initialize OpenSSL Core
  Write('Initializing OpenSSL Core... ');
  try
    LoadOpenSSLCore;
    WriteLn('OK (', GetOpenSSLVersionString, ')');
  except
    on E: Exception do
    begin
      WriteLn('FAILED');
      WriteLn('Error: ', E.Message);
      ExitCode := 1;
      Exit;
    end;
  end;
  
  // Load BIO functions
  Write('Loading BIO functions... ');
  if not LoadBIOFunctions then
  begin
    WriteLn('FAILED');
    WriteLn('Error: Could not load BIO functions');
    UnloadOpenSSLCore;
    ExitCode := 1;
    Exit;
  end;
  WriteLn('OK');
  
  PassedTests := 0;
  TotalTests := 7;
  
  try
    // Run all tests
    if Test_BIO_MemoryBasic then Inc(PassedTests);
    if Test_BIO_MemoryReadWrite then Inc(PassedTests);
    if Test_BIO_MemoryPutsGets then Inc(PassedTests);
    if Test_BIO_MemoryReset then Inc(PassedTests);
    if Test_BIO_MemoryFromBuffer then Inc(PassedTests);
    if Test_BIO_Null then Inc(PassedTests);
    if Test_BIO_Chain then Inc(PassedTests);
    
  except
    on E: Exception do
    begin
      WriteLn('');
      WriteLn('EXCEPTION: ', E.ClassName, ': ', E.Message);
    end;
  end;
  
  // Summary
  WriteLn('');
  WriteLn('========================================');
  WriteLn('Test Summary');
  WriteLn('========================================');
  WriteLn('Total tests: ', TotalTests);
  WriteLn('Passed: ', PassedTests);
  WriteLn('Failed: ', TotalTests - PassedTests);
  WriteLn('Success rate: ', (PassedTests * 100) div TotalTests, '%');
  
  if PassedTests = TotalTests then
  begin
    WriteLn('');
    WriteLn('ALL TESTS PASSED!');
    ExitCode := 0;
  end
  else
  begin
    WriteLn('');
    WriteLn('SOME TESTS FAILED!');
    ExitCode := 1;
  end;
  
  UnloadOpenSSLCore;
end.
