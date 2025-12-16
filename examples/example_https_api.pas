program example_https_api;

{$mode objfpc}{$H+}

{
  实用HTTPS API示例
  
  演示如何使用 TSimpleHTTPSClient 进行实际API调用
  不依赖JSON库，展示原始响应处理
}

uses
  SysUtils,
  fafafa.ssl.http.simple,
  fafafa.ssl.openssl.api.core;

procedure Example1_SimpleGET;
var
  LResponse: string;
begin
  WriteLn('=== Example 1: Simple GET Request ===');
  WriteLn('Fetching from example.com...');
  WriteLn;
  
  try
    LResponse := TSimpleHTTPSClient.Get('https://www.example.com');
    WriteLn('Response length: ', Length(LResponse), ' bytes');
    WriteLn('First 200 characters:');
    WriteLn(Copy(LResponse, 1, 200));
    WriteLn('...');
    WriteLn('✓ Success');
  except
    on E: Exception do
      WriteLn('✗ Error: ', E.Message);
  end;
  WriteLn;
end;

procedure Example2_APIWithHeaders;
var
  LOptions: THTTPSOptions;
  LResponse: THTTPResponse;
  I: Integer;
begin
  WriteLn('=== Example 2: API Call with Custom Headers ===');
  WriteLn('Calling JSONPlaceholder API...');
  WriteLn;
  
  try
    LOptions := TSimpleHTTPSClient.DefaultOptions;
    LOptions.Headers.Add('Accept: application/json');
    LOptions.Headers.Add('User-Agent: fafafa.ssl Example/1.0');
    
    LResponse := TSimpleHTTPSClient.GetEx(
      'https://jsonplaceholder.typicode.com/posts/1',
      LOptions
    );
    try
      WriteLn('Status: ', LResponse.StatusCode, ' ', LResponse.StatusText);
      WriteLn('Success: ', LResponse.Success);
      WriteLn;
      
      WriteLn('Response Headers:');
      for I := 0 to LResponse.Headers.Count - 1 do
        WriteLn('  ', LResponse.Headers[I]);
      WriteLn;
      
      WriteLn('Response Body (first 500 chars):');
      WriteLn(Copy(LResponse.Body, 1, 500));
      WriteLn('...');
      WriteLn('✓ Success');
      
    finally
      LResponse.Headers.Free;
    end;
  finally
    LOptions.Headers.Free;
  end;
  WriteLn;
end;

procedure Example3_POSTData;
var
  LOptions: THTTPSOptions;
  LResponse: THTTPResponse;
  LData: string;
begin
  WriteLn('=== Example 3: POST Data ===');
  WriteLn('Sending JSON data to API...');
  WriteLn;
  
  try
    LData := '{"title":"Test Post","body":"This is a test","userId":1}';
    
    LOptions := TSimpleHTTPSClient.DefaultOptions;
    LOptions.Headers.Add('Content-Type: application/json');
    
    LResponse := TSimpleHTTPSClient.PostEx(
      'https://jsonplaceholder.typicode.com/posts',
      LData,
      LOptions
    );
    try
      WriteLn('Status: ', LResponse.StatusCode);
      WriteLn('Response:');
      WriteLn(LResponse.Body);
      WriteLn('✓ Success');
    finally
      LResponse.Headers.Free;
    end;
  finally
    LOptions.Headers.Free;
  end;
  WriteLn;
end;

procedure Example4_DownloadFile;
var
  LFilePath: string;
begin
  WriteLn('=== Example 4: Download File ===');
  LFilePath := '/tmp/example_download.html';
  WriteLn('Downloading to: ', LFilePath);
  WriteLn;
  
  try
    if TSimpleHTTPSClient.Download('https://www.example.com', LFilePath) then
    begin
      WriteLn('✓ File downloaded successfully');
      WriteLn('File size: ', FileSize(LFilePath), ' bytes');
      
      if FileExists(LFilePath) then
        DeleteFile(LFilePath);
    end
    else
      WriteLn('✗ Download failed');
  except
    on E: Exception do
      WriteLn('✗ Error: ', E.Message);
  end;
  WriteLn;
end;

procedure Example5_ErrorHandling;
var
  LOptions: THTTPSOptions;
  LResponse: THTTPResponse;
begin
  WriteLn('=== Example 5: Error Handling ===');
  WriteLn('Trying a 404 URL...');
  WriteLn;
  
  try
    LOptions := TSimpleHTTPSClient.DefaultOptions;
    
    LResponse := TSimpleHTTPSClient.GetEx(
      'https://jsonplaceholder.typicode.com/invalid-endpoint',
      LOptions
    );
    try
      WriteLn('Status: ', LResponse.StatusCode, ' ', LResponse.StatusText);
      WriteLn('Success: ', LResponse.Success);
      
      if LResponse.Success then
        WriteLn('✗ Should have reported failure')
      else
        WriteLn('✓ Correctly reported failure');
        
    finally
      LResponse.Headers.Free;
    end;
  finally
    LOptions.Headers.Free;
  end;
  WriteLn;
end;

procedure Example6_HTTPSVerification;
var
  LOptions: THTTPSOptions;
  LResponse: THTTPResponse;
begin
  WriteLn('=== Example 6: HTTPS Certificate Verification ===');
  WriteLn('Testing with proper HTTPS site...');
  WriteLn;
  
  try
    LOptions := TSimpleHTTPSClient.DefaultOptions;
    LOptions.VerifyPeer := True; // 验证证书
    
    LResponse := TSimpleHTTPSClient.GetEx(
      'https://www.google.com',
      LOptions
    );
    try
      WriteLn('Status: ', LResponse.StatusCode);
      WriteLn('Certificate verification: ✓ Passed');
      WriteLn('✓ Success');
    finally
      LResponse.Headers.Free;
    end;
  finally
    LOptions.Headers.Free;
  end;
  WriteLn;
end;

begin
  WriteLn('==========================================');
  WriteLn('  HTTPS API Client Examples');
  WriteLn('  Practical demonstrations');
  WriteLn('==========================================');
  WriteLn;
  
  try
    // 初始化OpenSSL
    LoadOpenSSLCore;
    if not IsOpenSSLCoreLoaded then
    begin
      WriteLn('ERROR: Failed to load OpenSSL');
      Halt(1);
    end;
    WriteLn('OpenSSL: ', GetOpenSSLVersionString);
    WriteLn;
    
    // 运行所有示例
    Example1_SimpleGET;
    Example2_APIWithHeaders;
    Example3_POSTData;
    Example4_DownloadFile;
    Example5_ErrorHandling;
    Example6_HTTPSVerification;
    
    WriteLn('==========================================');
    WriteLn('✓ All examples completed successfully!');
    WriteLn('==========================================');
    WriteLn;
    WriteLn('Key Features Demonstrated:');
    WriteLn('  • Simple GET/POST requests');
    WriteLn('  • Custom headers');
    WriteLn('  • Response parsing');
    WriteLn('  • File downloads');
    WriteLn('  • Error handling');
    WriteLn('  • Certificate verification');
    
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('FATAL ERROR: ', E.Message);
      Halt(1);
    end;
  end;
  
  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
end.
