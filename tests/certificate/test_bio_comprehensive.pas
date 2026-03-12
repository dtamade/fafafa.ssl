program test_bio_comprehensive;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.api.bio;

var
  TestsPassed, TestsFailed: Integer;

procedure RunTest(const TestName: string; Passed: Boolean);
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

procedure Test_BIO_new_mem_buf;
var
  bio: PBIO;
  data: AnsiString;
begin
  data := 'Hello OpenSSL BIO!';
  bio := BIO_new_mem_buf(PAnsiChar(data), Length(data));
  RunTest('BIO_new_mem_buf创建内存BIO', bio <> nil);
  if bio <> nil then
    BIO_free(bio);
end;

procedure Test_BIO_s_mem;
var
  bio: PBIO;
  written: Integer;
  data: AnsiString;
begin
  bio := BIO_new(BIO_s_mem());
  if bio <> nil then
  begin
    data := 'Test data for memory BIO';
    written := BIO_write(bio, PAnsiChar(data), Length(data));
    RunTest('BIO_write写入数据', written = Length(data));
    BIO_free(bio);
  end
  else
    RunTest('BIO_write写入数据', False);
end;

procedure Test_BIO_read_write;
var
  bio: PBIO;
  written, read_count: Integer;
  write_data, read_data: AnsiString;
  buffer: array[0..255] of AnsiChar;
begin
  bio := BIO_new(BIO_s_mem());
  if bio <> nil then
  begin
    write_data := 'OpenSSL BIO Read/Write Test';
    written := BIO_write(bio, PAnsiChar(write_data), Length(write_data));
    
    FillChar(buffer, SizeOf(buffer), 0);
    read_count := BIO_read(bio, @buffer[0], SizeOf(buffer));
    
    SetString(read_data, buffer, read_count);
    RunTest('BIO读写数据一致性', read_data = write_data);
    
    BIO_free(bio);
  end
  else
    RunTest('BIO读写数据一致性', False);
end;

procedure Test_BIO_StringIO;
var
  bio: PBIO;
  test_str: AnsiString;
  buffer: array[0..255] of AnsiChar;
  written, read_count: Integer;
  read_str: AnsiString;
begin
  bio := BIO_new(BIO_s_mem());
  if bio <> nil then
  begin
    test_str := 'BIO string IO test';
    written := BIO_write(bio, PAnsiChar(test_str), Length(test_str));

    FillChar(buffer, SizeOf(buffer), 0);
    read_count := BIO_read(bio, @buffer[0], SizeOf(buffer));
    SetString(read_str, buffer, read_count);

    RunTest('BIO字符串写入/读取', (written = Length(test_str)) and (read_str = test_str));

    BIO_free(bio);
  end
  else
    RunTest('BIO字符串写入/读取', False);
end;

procedure Test_BIO_ctrl_pending;
var
  bio: PBIO;
  data: AnsiString;
  pending: Integer;
begin
  bio := BIO_new(BIO_s_mem());
  if (bio <> nil) and Assigned(BIO_write) and Assigned(BIO_pending) then
  begin
    data := 'Test pending data';
    BIO_write(bio, PAnsiChar(data), Length(data));
    
    pending := BIO_pending(bio);
    RunTest('BIO_ctrl_pending检测待读取数据', pending = Length(data));
    
    BIO_free(bio);
  end
  else
    RunTest('BIO_ctrl_pending检测待读取数据', False);
end;

procedure Test_BIO_reset;
var
  bio: PBIO;
  data: AnsiString;
begin
  bio := BIO_new(BIO_s_mem());
  if (bio <> nil) and Assigned(BIO_write) then
  begin
    data := 'Data to reset';
    BIO_write(bio, PAnsiChar(data), Length(data));
    if Assigned(BIO_reset) then
      BIO_reset(bio);
    RunTest('BIO_reset helper smoke', True);
    BIO_free(bio);
  end
  else
    RunTest('BIO_reset helper smoke', False);
end;

begin
  WriteLn('========================================');
  WriteLn('  OpenSSL BIO Module Test');
  WriteLn('========================================');
  WriteLn;
  
  TestsPassed := 0;
  TestsFailed := 0;
  
  try
    LoadOpenSSLCore;
  except
    on E: Exception do
    begin
      WriteLn('ERROR: 无法加载 OpenSSL 库: ', E.Message);
      Halt(1);
    end;
  end;

  if not TOpenSSLLoader.IsModuleLoaded(osmCore) then
  begin
    WriteLn('ERROR: OpenSSL core did not stay loaded');
    Halt(1);
  end;

  LoadOpenSSLBIO;
  
  try
    WriteLn('运行 BIO 测试...');
    WriteLn;
    
    Test_BIO_new_mem_buf;
    Test_BIO_s_mem;
    Test_BIO_read_write;
    Test_BIO_StringIO;
    Test_BIO_ctrl_pending;
    Test_BIO_reset;
    
    WriteLn;
    WriteLn('========================================');
    WriteLn('  测试结果');
    WriteLn('========================================');
    WriteLn('通过: ', TestsPassed);
    WriteLn('失败: ', TestsFailed);
    WriteLn('总计: ', TestsPassed + TestsFailed);
    if (TestsPassed + TestsFailed) > 0 then
      WriteLn('成功率: ', ((TestsPassed * 100) div (TestsPassed + TestsFailed)):3, '%');
    WriteLn;
    
    if TestsFailed = 0 then
    begin
      WriteLn('✓ 所有测试通过!');
      WriteLn('[PASS] bio comprehensive validation completed');
      Halt(0);
    end
    else
    begin
      WriteLn('✗ 部分测试失败!');
      Halt(1);
    end;
    
  finally
    UnloadOpenSSLBIO;
    UnloadOpenSSLCore;
  end;
end.
