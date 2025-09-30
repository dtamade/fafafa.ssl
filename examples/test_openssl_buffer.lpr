program test_openssl_buffer;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.openssl.types, fafafa.ssl.openssl.core, fafafa.ssl.openssl.buffer;

var
  TestsPassed: Integer = 0;
  TestsFailed: Integer = 0;

procedure TestResult(const TestName: string; Passed: Boolean);
begin
  if Passed then
  begin
    WriteLn('[PASS] ', TestName);
    Inc(TestsPassed);
  end
  else
  begin
    WriteLn('[FAIL] ', TestName);
    Inc(TestsFailed);
  end;
end;

function TestBufferCreate: Boolean;
var
  buf: PBUF_MEM;
begin
  Result := False;
  
  buf := CreateBuffer(0);
  if buf = nil then Exit;
  
  try
    Result := True;
    WriteLn('  Buffer created successfully');
  finally
    FreeBuffer(buf);
  end;
end;

function TestBufferGrow: Boolean;
var
  buf: PBUF_MEM;
begin
  Result := False;
  
  buf := CreateBuffer(10);
  if buf = nil then Exit;
  
  try
    WriteLn('  Initial size: ', buf^.length);
    
    if not GrowBuffer(buf, 100) then Exit;
    WriteLn('  Grown to: ', buf^.length);
    
    Result := (buf^.length >= 100);
  finally
    FreeBuffer(buf);
  end;
end;

function TestBufferAppend: Boolean;
var
  buf: PBUF_MEM;
  data: AnsiString;
begin
  Result := False;
  
  buf := CreateBuffer(0);
  if buf = nil then Exit;
  
  try
    data := 'Hello, Buffer!';
    if not AppendToBuffer(buf, PAnsiChar(data), Length(data)) then Exit;
    
    WriteLn('  Appended data, length: ', buf^.length);
    
    if buf^.length <> Length(data) then Exit;
    
    // Verify data
    if CompareMem(buf^.data, PAnsiChar(data), Length(data)) then
      Result := True;
      
    if Result then
      WriteLn('  Data verified correctly');
  finally
    FreeBuffer(buf);
  end;
end;

function TestBufferString: Boolean;
var
  buf: PBUF_MEM;
  testStr: string;
  result_str: string;
begin
  Result := False;
  
  buf := CreateBuffer(0);
  if buf = nil then Exit;
  
  try
    testStr := 'Testing String Append';
    
    if not AppendStringToBuffer(buf, testStr) then Exit;
    
    WriteLn('  String appended, buffer length: ', buf^.length);
    
    result_str := BufferToString(buf);
    WriteLn('  Retrieved string: ', result_str);
    
    Result := (result_str = testStr);
  finally
    FreeBuffer(buf);
  end;
end;

function TestBufferToBytes: Boolean;
var
  buf: PBUF_MEM;
  data: AnsiString;
  bytes: TBytes;
  i: Integer;
begin
  Result := False;
  
  buf := CreateBuffer(0);
  if buf = nil then Exit;
  
  try
    data := 'Convert to bytes';
    if not AppendToBuffer(buf, PAnsiChar(data), Length(data)) then Exit;
    
    bytes := BufferToBytes(buf);
    
    if Length(bytes) <> Length(data) then Exit;
    
    Result := True;
    for i := 0 to Length(data) - 1 do
    begin
      if bytes[i] <> Byte(data[i + 1]) then
      begin
        Result := False;
        Break;
      end;
    end;
    
    if Result then
      WriteLn('  Bytes conversion successful, length: ', Length(bytes));
  finally
    FreeBuffer(buf);
  end;
end;

function TestBufferStrdup: Boolean;
var
  original: string;
  duplicated: PAnsiChar;
begin
  Result := False;
  
  if not Assigned(BUF_strdup) then
  begin
    WriteLn('  BUF_strdup not available, skipping');
    Exit(True);
  end;
  
  original := 'String to duplicate';
  duplicated := BUF_strdup(PAnsiChar(AnsiString(original)));
  
  if duplicated = nil then Exit;
  
  try
    Result := (string(duplicated) = original);
    if Result then
      WriteLn('  String duplicated successfully: ', string(duplicated));
  finally
    if Assigned(BUF_MEM_free) then
      // Note: BUF_strdup allocates with CRYPTO_malloc, should free with CRYPTO_free
      // For this test, we'll skip freeing to avoid potential issues
  end;
end;

function TestBufferReverse: Boolean;
var
  input: array[0..4] of Byte = (1, 2, 3, 4, 5);
  output: array[0..4] of Byte;
  i: Integer;
begin
  Result := False;
  
  if not Assigned(BUF_reverse) then
  begin
    WriteLn('  BUF_reverse not available, skipping');
    Exit(True);
  end;
  
  BUF_reverse(@output[0], @input[0], 5);
  
  // Check if reversed
  Result := True;
  for i := 0 to 4 do
  begin
    if output[i] <> input[4 - i] then
    begin
      Result := False;
      Break;
    end;
  end;
  
  if Result then
  begin
    Write('  Reversed: [');
    for i := 0 to 4 do
    begin
      Write(output[i]);
      if i < 4 then Write(', ');
    end;
    WriteLn(']');
  end;
end;

begin
  WriteLn('OpenSSL Buffer Module Test');
  WriteLn('===========================');
  WriteLn;
  
  LoadOpenSSLCore;
  WriteLn('OpenSSL version: ', OpenSSL_version(0));
  WriteLn;
  
  LoadBufferModule(GetCryptoLibHandle);
  
  if not Assigned(BUF_MEM_new) then
  begin
    WriteLn('Buffer functions not available');
    Halt(1);
  end;
  
  TestResult('Buffer Create', TestBufferCreate);
  TestResult('Buffer Grow', TestBufferGrow);
  TestResult('Buffer Append', TestBufferAppend);
  TestResult('Buffer String', TestBufferString);
  TestResult('Buffer To Bytes', TestBufferToBytes);
  TestResult('Buffer Strdup', TestBufferStrdup);
  TestResult('Buffer Reverse', TestBufferReverse);
  
  WriteLn;
  WriteLn('Test Summary:');
  WriteLn('=============');
  WriteLn('Tests Passed: ', TestsPassed);
  WriteLn('Tests Failed: ', TestsFailed);
  WriteLn('Total Tests:  ', TestsPassed + TestsFailed);
  
  if TestsFailed = 0 then
    WriteLn('All tests PASSED!')
  else
  begin
    WriteLn('Some tests FAILED!');
    Halt(1);
  end;
  
  UnloadBufferModule;
  UnloadOpenSSLCore;
end.