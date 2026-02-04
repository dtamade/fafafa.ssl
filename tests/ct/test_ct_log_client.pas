program test_ct_log_client;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils, Classes,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.ct,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.ct.log;

var
  TotalTests: Integer = 0;
  PassedTests: Integer = 0;
  FailedTests: Integer = 0;

procedure StartTest(const TestName: string);
begin
  Inc(TotalTests);
  Write('[', TotalTests, '] ', TestName, '... ');
end;

procedure PassTest;
begin
  Inc(PassedTests);
  WriteLn('PASS');
end;

procedure FailTest(const Reason: string);
begin
  Inc(FailedTests);
  WriteLn('FAIL: ', Reason);
end;

procedure TestCTLogClientCreation;
var
  Client: TCTLogClient;
begin
  StartTest('Create CT log client');
  try
    Client := TCTLogClient.Create('', False);
    try
      if Client = nil then
        FailTest('Client is nil')
      else
        PassTest;
    finally
      Client.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestCTLogClientWithCacheFile;
var
  Client: TCTLogClient;
  CacheFile: string;
begin
  StartTest('Create CT log client with cache file');
  try
    CacheFile := 'test_ct_cache.json';
    Client := TCTLogClient.Create(CacheFile, False);
    try
      if Client = nil then
        FailTest('Client is nil')
      else if Client.CacheFile <> CacheFile then
        FailTest('Cache file not set correctly')
      else
        PassTest;
    finally
      Client.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestGetLogStoreInitiallyNil;
var
  Client: TCTLogClient;
begin
  StartTest('Get log store initially nil');
  try
    Client := TCTLogClient.Create('', False);
    try
      // 初始状态下，LogStore 应该是 nil（因为没有加载任何日志）
      if Client.GetLogStore <> nil then
        FailTest('LogStore should be nil initially')
      else
        PassTest;
    finally
      Client.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestGetAllLogsEmpty;
var
  Client: TCTLogClient;
  Logs: TCTLogList;
begin
  StartTest('Get all logs returns empty initially');
  try
    Client := TCTLogClient.Create('', False);
    try
      Logs := Client.GetAllLogs;
      
      if Length(Logs) <> 0 then
        FailTest('Logs should be empty initially')
      else
        PassTest;
    finally
      Client.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestGetUsableLogCountZero;
var
  Client: TCTLogClient;
begin
  StartTest('Get usable log count returns zero initially');
  try
    Client := TCTLogClient.Create('', False);
    try
      if Client.GetUsableLogCount <> 0 then
        FailTest('Usable log count should be zero initially')
      else
        PassTest;
    finally
      Client.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestFindLogByIDNotFound;
var
  Client: TCTLogClient;
  LogInfo: TCTLogInfo;
begin
  StartTest('Find log by ID returns empty when not found');
  try
    Client := TCTLogClient.Create('', False);
    try
      LogInfo := Client.FindLogByID('nonexistent-log-id');
      
      if LogInfo.LogID <> '' then
        FailTest('Should return empty LogInfo for nonexistent ID')
      else
        PassTest;
    finally
      Client.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestLoadFromFileNonexistent;
var
  Client: TCTLogClient;
begin
  StartTest('Load from nonexistent file returns false');
  try
    Client := TCTLogClient.Create('', False);
    try
      if Client.LoadFromFile('nonexistent_file.json') then
        FailTest('Should return false for nonexistent file')
      else
        PassTest;
    finally
      Client.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestSaveToFile;
var
  Client: TCTLogClient;
  TempFile: string;
begin
  StartTest('Save to file');
  try
    TempFile := 'test_ct_save.json';
    Client := TCTLogClient.Create('', False);
    try
      // 保存空的日志列表
      if not Client.SaveToFile(TempFile) then
        FailTest('Failed to save to file')
      else if not FileExists(TempFile) then
        FailTest('File was not created')
      else
      begin
        PassTest;
        // 清理测试文件
        DeleteFile(TempFile);
      end;
    finally
      Client.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestLoadFromFileThenSave;
var
  Client: TCTLogClient;
  TempFile: string;
  SaveSuccess, LoadSuccess: Boolean;
begin
  StartTest('Save then load from file');
  try
    TempFile := 'test_ct_roundtrip.json';
    
    // 保存
    Client := TCTLogClient.Create('', False);
    try
      SaveSuccess := Client.SaveToFile(TempFile);
    finally
      Client.Free;
    end;
    
    if not SaveSuccess then
    begin
      FailTest('Failed to save');
      Exit;
    end;
    
    // 加载
    Client := TCTLogClient.Create('', False);
    try
      LoadSuccess := Client.LoadFromFile(TempFile);
      
      if not LoadSuccess then
        FailTest('Failed to load saved file')
      else
        PassTest;
      
      // 清理测试文件
      DeleteFile(TempFile);
    finally
      Client.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestBase64DecodeEmpty;
var
  Result: TBytes;
begin
  StartTest('Base64 decode empty string');
  try
    Result := Base64Decode('');
    
    if Length(Result) <> 0 then
      FailTest('Should return empty array for empty string')
    else
      PassTest;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestBase64DecodeValid;
var
  Result: TBytes;
  Input: string;
begin
  StartTest('Base64 decode valid string');
  try
    // "Hello" in Base64 is "SGVsbG8="
    Input := 'SGVsbG8=';
    Result := Base64Decode(Input);
    
    if Length(Result) = 0 then
      FailTest('Should return non-empty array for valid Base64')
    else if Length(Result) <> 5 then
      FailTest('Decoded length should be 5')
    else
      PassTest;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestAutoUpdateProperty;
var
  Client: TCTLogClient;
begin
  StartTest('AutoUpdate property');
  try
    Client := TCTLogClient.Create('', False);
    try
      if Client.AutoUpdate then
        FailTest('AutoUpdate should be False initially')
      else
      begin
        Client.AutoUpdate := True;
        if not Client.AutoUpdate then
          FailTest('AutoUpdate should be True after setting')
        else
          PassTest;
      end;
    finally
      Client.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure PrintSummary;
begin
  WriteLn;
  WriteLn('=== Test Summary ===');
  WriteLn('Total tests: ', TotalTests);
  WriteLn('Passed: ', PassedTests);
  WriteLn('Failed: ', FailedTests);
  
  if FailedTests = 0 then
    WriteLn('All tests passed!')
  else
    WriteLn('Some tests failed!');
end;

begin
  WriteLn('=== CT Log Client Module Tests ===');
  WriteLn;

  // 加载 OpenSSL 函数
  try
    LoadOpenSSLCore;

    if not TOpenSSLLoader.IsModuleLoaded(osmCore) then
    begin
      WriteLn('ERROR: Failed to load OpenSSL library');
      Halt(1);
    end;

    LoadCTFunctions;
  except
    on E: Exception do
    begin
      WriteLn('ERROR: Failed to initialize OpenSSL: ', E.Message);
      Halt(1);
    end;
  end;
  
  // 运行测试
  TestCTLogClientCreation;
  TestCTLogClientWithCacheFile;
  TestGetLogStoreInitiallyNil;
  TestGetAllLogsEmpty;
  TestGetUsableLogCountZero;
  TestFindLogByIDNotFound;
  TestLoadFromFileNonexistent;
  TestSaveToFile;
  TestLoadFromFileThenSave;
  TestBase64DecodeEmpty;
  TestBase64DecodeValid;
  TestAutoUpdateProperty;
  
  PrintSummary;
  
  // 返回退出码
  if FailedTests > 0 then
    Halt(1)
  else
    Halt(0);
end.
