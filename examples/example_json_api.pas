program example_json_api;

{$mode objfpc}{$H+}

{
  JSON API 示例
  
  演示如何使用 TJSONHTTPClient 调用 REST API
}

uses
  SysUtils, fpjson,
  fafafa.ssl.http.json,
  fafafa.ssl.openssl.api.core;

procedure Example1_GetJSON;
var
  LData: TJSONObject;
begin
  WriteLn('=== Example 1: GET JSON ===');
  WriteLn('Fetching JSON from JSONPlaceholder API...');
  WriteLn;
  
  try
    LData := TJSONHTTPClient.GetJSON('https://jsonplaceholder.typicode.com/posts/1');
    try
      WriteLn('Response:');
      WriteLn('  User ID: ', LData.Get('userId'));
      WriteLn('  ID: ', LData.Get('id'));
      WriteLn('  Title: ', LData.Get('title'));
      WriteLn('  Body: ', Copy(LData.Get('body'), 1, 50), '...');
      WriteLn('✓ Success');
    finally
      LData.Free;
    end;
  except
    on E: Exception do
      WriteLn('✗ Error: ', E.Message);
  end;
  WriteLn;
end;

procedure Example2_PostJSON;
var
  LRequest, LResponse: TJSONObject;
begin
  WriteLn('=== Example 2: POST JSON ===');
  WriteLn('Creating a new post...');
  WriteLn;
  
  try
    LRequest := TJSONObject.Create;
    try
      LRequest.Add('title', 'Test Post from fafafa.ssl');
      LRequest.Add('body', 'This is a test post created using TJSONHTTPClient');
      LRequest.Add('userId', 1);
      
      WriteLn('Sending:');
      WriteLn(LRequest.AsJSON);
      WriteLn;
      
      LResponse := TJSONHTTPClient.PostJSON(
        'https://jsonplaceholder.typicode.com/posts',
        LRequest
      );
      try
        WriteLn('Response:');
        WriteLn('  ID: ', LResponse.Get('id'));
        WriteLn('  Title: ', LResponse.Get('title'));
        WriteLn('✓ Success');
      finally
        LResponse.Free;
      end;
    finally
      LRequest.Free;
    end;
  except
    on E: Exception do
      WriteLn('✗ Error: ', E.Message);
  end;
  WriteLn;
end;

procedure Example3_GetArray;
var
  LData: TJSONArray;
  I: Integer;
  LPost: TJSONObject;
begin
  WriteLn('=== Example 3: GET JSON Array ===');
  WriteLn('Fetching list of posts...');
  WriteLn;
  
  try
    LData := TJSONHTTPClient.GetJSONArray('https://jsonplaceholder.typicode.com/posts?_limit=5');
    try
      WriteLn('Received ', LData.Count, ' posts:');
      for I := 0 to LData.Count - 1 do
      begin
        LPost := LData.Objects[I];
        WriteLn('  [', LPost.Get('id'), '] ', LPost.Get('title'));
      end;
      WriteLn('✓ Success');
    finally
      LData.Free;
    end;
  except
    on E: Exception do
      WriteLn('✗ Error: ', E.Message);
  end;
  WriteLn;
end;

procedure Example4_ErrorHandling;
begin
  WriteLn('=== Example 4: Error Handling ===');
  WriteLn('Trying invalid URL...');
  WriteLn;
  
  try
    TJSONHTTPClient.GetJSON('https://jsonplaceholder.typicode.com/invalid');
    WriteLn('✗ Should have thrown an error');
  except
    on E: EHTTPSClientException do
    begin
      WriteLn('✓ Caught HTTP error correctly:');
      WriteLn('  Status Code: ', E.StatusCode);
      WriteLn('  Message: ', E.Message);
    end;
    on E: Exception do
    begin
      WriteLn('✓ Caught exception:');
      WriteLn('  ', E.Message);
    end;
  end;
  WriteLn;
end;

begin
  WriteLn('==========================================');
  WriteLn('  JSON API Client Examples');
  WriteLn('==========================================');
  WriteLn;
  
  try
    // 初始化OpenSSL
    LoadOpenSSLCore;
    WriteLn('OpenSSL: ', GetOpenSSLVersionString);
    WriteLn;
    
    // 运行示例
    Example1_GetJSON;
    Example2_PostJSON;
    Example3_GetArray;
    Example4_ErrorHandling;
    
    WriteLn('==========================================');
    WriteLn('✓ All examples completed');
    WriteLn('==========================================');
    
  except
    on E: Exception do
    begin
      WriteLn('FATAL ERROR: ', E.Message);
      Halt(1);
    end;
  end;
  
  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
end.
