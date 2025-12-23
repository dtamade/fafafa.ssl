program test_http_json;

{$mode objfpc}{$H+}

{**
 * JSON HTTP 客户端测试
 *
 * 测试 fafafa.ssl.http.json 模块功能：
 * - JSON GET 请求
 * - JSON POST 请求
 * - JSON 数组处理
 * - 错误处理
 *
 * 注意: 测试需要网络连接，使用 httpbin.org 和 jsonplaceholder
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2025-12-23
 *}

uses
  SysUtils, Classes, fpjson, jsonparser,
  fafafa.ssl.base,
  fafafa.ssl.http.simple,
  fafafa.ssl.http.json;

var
  TotalTests, PassedTests, FailedTests, SkippedTests: Integer;
  CurrentSection: string;

procedure BeginSection(const AName: string);
begin
  CurrentSection := AName;
  WriteLn;
  WriteLn('=== ', AName, ' ===');
end;

procedure Check(const AName: string; ASuccess: Boolean; const ADetails: string = '');
begin
  Inc(TotalTests);
  Write('  [', CurrentSection, '] ', AName, ': ');
  if ASuccess then
  begin
    Inc(PassedTests);
    WriteLn('PASS');
  end
  else
  begin
    Inc(FailedTests);
    WriteLn('FAIL');
    if ADetails <> '' then
      WriteLn('    ', ADetails);
  end;
end;

procedure Skip(const AName: string; const AReason: string);
begin
  Inc(TotalTests);
  Inc(SkippedTests);
  WriteLn('  [', CurrentSection, '] ', AName, ': SKIP');
  WriteLn('    ', AReason);
end;

procedure PrintSummary;
begin
  WriteLn;
  WriteLn('========================================');
  WriteLn('JSON HTTP 客户端测试结果');
  WriteLn('========================================');
  WriteLn('总测试数: ', TotalTests);
  WriteLn('通过: ', PassedTests);
  WriteLn('失败: ', FailedTests);
  WriteLn('跳过: ', SkippedTests);
  if TotalTests > 0 then
    WriteLn('通过率: ', (PassedTests * 100) div TotalTests, '%');
  WriteLn('========================================');
end;

// ============================================================================
// 测试 1: JSON GET 请求
// ============================================================================
procedure Test1_GetJSON;
var
  JsonObj: TJSONObject;
begin
  BeginSection('1. JSON GET 请求测试');

  try
    // 使用 jsonplaceholder API
    JsonObj := TJSONHTTPClient.GetJSON('https://jsonplaceholder.typicode.com/posts/1');
    try
      Check('获取 JSON 对象', JsonObj <> nil);
      if JsonObj <> nil then
      begin
        Check('JSON 包含 id 字段', JsonObj.Find('id') <> nil);
        Check('JSON 包含 title 字段', JsonObj.Find('title') <> nil);
        Check('JSON 包含 body 字段', JsonObj.Find('body') <> nil);
        Check('id 值正确', JsonObj.Get('id', 0) = 1);
      end;
    finally
      JsonObj.Free;
    end;
  except
    on E: Exception do
      Skip('JSON GET 请求', '网络错误: ' + E.Message);
  end;
end;

// ============================================================================
// 测试 2: JSON POST 请求
// ============================================================================
procedure Test2_PostJSON;
var
  RequestData, ResponseObj: TJSONObject;
begin
  BeginSection('2. JSON POST 请求测试');

  RequestData := TJSONObject.Create;
  try
    RequestData.Add('title', 'fafafa.ssl test');
    RequestData.Add('body', 'Testing JSON POST');
    RequestData.Add('userId', 1);

    try
      ResponseObj := TJSONHTTPClient.PostJSON(
        'https://jsonplaceholder.typicode.com/posts',
        RequestData);
      try
        Check('POST 返回 JSON 对象', ResponseObj <> nil);
        if ResponseObj <> nil then
        begin
          // jsonplaceholder 返回带有新 id 的对象
          Check('响应包含 id', ResponseObj.Find('id') <> nil);
          Check('响应包含 title', ResponseObj.Find('title') <> nil);
        end;
      finally
        ResponseObj.Free;
      end;
    except
      on E: Exception do
        Skip('JSON POST 请求', '网络错误: ' + E.Message);
    end;
  finally
    RequestData.Free;
  end;
end;

// ============================================================================
// 测试 3: JSON POST 字符串
// ============================================================================
procedure Test3_PostJSONString;
var
  ResponseObj: TJSONObject;
  JsonStr: string;
begin
  BeginSection('3. JSON POST 字符串测试');

  JsonStr := '{"title":"String Test","body":"Testing with string","userId":1}';

  try
    ResponseObj := TJSONHTTPClient.PostJSON(
      'https://jsonplaceholder.typicode.com/posts',
      JsonStr);
    try
      Check('字符串 POST 返回 JSON', ResponseObj <> nil);
      if ResponseObj <> nil then
      begin
        Check('响应包含 title', ResponseObj.Find('title') <> nil);
      end;
    finally
      ResponseObj.Free;
    end;
  except
    on E: Exception do
      Skip('JSON POST 字符串', '网络错误: ' + E.Message);
  end;
end;

// ============================================================================
// 测试 4: JSON PUT 请求
// ============================================================================
procedure Test4_PutJSON;
var
  RequestData, ResponseObj: TJSONObject;
begin
  BeginSection('4. JSON PUT 请求测试');

  RequestData := TJSONObject.Create;
  try
    RequestData.Add('id', 1);
    RequestData.Add('title', 'Updated Title');
    RequestData.Add('body', 'Updated Body');
    RequestData.Add('userId', 1);

    try
      ResponseObj := TJSONHTTPClient.PutJSON(
        'https://jsonplaceholder.typicode.com/posts/1',
        RequestData);
      try
        Check('PUT 返回 JSON 对象', ResponseObj <> nil);
        if ResponseObj <> nil then
        begin
          Check('响应包含更新的数据', ResponseObj.Find('title') <> nil);
        end;
      finally
        ResponseObj.Free;
      end;
    except
      on E: Exception do
        Skip('JSON PUT 请求', '网络错误: ' + E.Message);
    end;
  finally
    RequestData.Free;
  end;
end;

// ============================================================================
// 测试 5: JSON DELETE 请求
// ============================================================================
procedure Test5_DeleteJSON;
var
  ResponseObj: TJSONObject;
begin
  BeginSection('5. JSON DELETE 请求测试');

  try
    ResponseObj := TJSONHTTPClient.DeleteJSON(
      'https://jsonplaceholder.typicode.com/posts/1');
    try
      // jsonplaceholder 返回空对象 {}
      Check('DELETE 返回 JSON 对象', ResponseObj <> nil);
    finally
      ResponseObj.Free;
    end;
  except
    on E: Exception do
      Skip('JSON DELETE 请求', '网络错误: ' + E.Message);
  end;
end;

// ============================================================================
// 测试 6: JSON 数组获取
// ============================================================================
procedure Test6_GetJSONArray;
var
  JsonArray: TJSONArray;
begin
  BeginSection('6. JSON 数组获取测试');

  try
    JsonArray := TJSONHTTPClient.GetJSONArray(
      'https://jsonplaceholder.typicode.com/posts?userId=1');
    try
      Check('获取 JSON 数组', JsonArray <> nil);
      if JsonArray <> nil then
      begin
        Check('数组非空', JsonArray.Count > 0,
              '元素数: ' + IntToStr(JsonArray.Count));
        if JsonArray.Count > 0 then
        begin
          Check('第一个元素是对象', JsonArray.Items[0] is TJSONObject);
        end;
      end;
    finally
      JsonArray.Free;
    end;
  except
    on E: Exception do
      Skip('JSON 数组获取', '网络错误: ' + E.Message);
  end;
end;

// ============================================================================
// 测试 7: 用户列表 API
// ============================================================================
procedure Test7_UserListAPI;
var
  JsonArray: TJSONArray;
  UserObj: TJSONObject;
begin
  BeginSection('7. 用户列表 API 测试');

  try
    JsonArray := TJSONHTTPClient.GetJSONArray(
      'https://jsonplaceholder.typicode.com/users');
    try
      Check('获取用户列表', JsonArray <> nil);
      if JsonArray <> nil then
      begin
        Check('用户列表有 10 个用户', JsonArray.Count = 10);
        if JsonArray.Count > 0 then
        begin
          UserObj := JsonArray.Items[0] as TJSONObject;
          Check('用户有 name 字段', UserObj.Find('name') <> nil);
          Check('用户有 email 字段', UserObj.Find('email') <> nil);
          Check('用户有 address 字段', UserObj.Find('address') <> nil);
        end;
      end;
    finally
      JsonArray.Free;
    end;
  except
    on E: Exception do
      Skip('用户列表 API', '网络错误: ' + E.Message);
  end;
end;

// ============================================================================
// 测试 8: httpbin JSON 端点
// ============================================================================
procedure Test8_HttpbinJSON;
var
  JsonObj: TJSONObject;
begin
  BeginSection('8. httpbin JSON 端点测试');

  try
    JsonObj := TJSONHTTPClient.GetJSON('https://httpbin.org/json');
    try
      Check('httpbin JSON 响应', JsonObj <> nil);
      if JsonObj <> nil then
      begin
        // httpbin 返回一个带有 slideshow 字段的 JSON
        Check('包含 slideshow 字段', JsonObj.Find('slideshow') <> nil);
      end;
    finally
      JsonObj.Free;
    end;
  except
    on E: Exception do
      Skip('httpbin JSON', '网络错误: ' + E.Message);
  end;
end;

// ============================================================================
// 测试 9: 错误处理 - 404
// ============================================================================
procedure Test9_Error404;
var
  JsonObj: TJSONObject;
  GotException: Boolean;
begin
  BeginSection('9. 错误处理测试');

  GotException := False;
  try
    JsonObj := TJSONHTTPClient.GetJSON('https://httpbin.org/status/404');
    // 如果成功到这里，检查返回值
    if JsonObj <> nil then
    begin
      JsonObj.Free;
      Check('404 错误处理', False, '应该抛出异常或返回错误');
    end
    else
      Check('404 返回 nil', True);
  except
    on E: Exception do
    begin
      GotException := True;
      Check('404 抛出异常', True, E.Message);
    end;
  end;

  if not GotException then
    Skip('404 异常测试', '未抛出异常');
end;

// ============================================================================
// 测试 10: 无效 JSON 处理
// ============================================================================
procedure Test10_InvalidJSON;
var
  JsonObj: TJSONObject;
  GotException: Boolean;
begin
  BeginSection('10. 无效 JSON 处理测试');

  GotException := False;
  try
    // httpbin /html 返回 HTML，不是 JSON
    JsonObj := TJSONHTTPClient.GetJSON('https://httpbin.org/html');
    if JsonObj <> nil then
    begin
      JsonObj.Free;
      Check('无效 JSON 处理', False, 'HTML 不应解析为有效 JSON');
    end
    else
      Check('无效 JSON 返回 nil', True);
  except
    on E: Exception do
    begin
      GotException := True;
      Check('无效 JSON 抛出异常', True, E.ClassName + ': ' + E.Message);
    end;
  end;

  if not GotException then
    Skip('无效 JSON 异常', '未抛出异常');
end;

// ============================================================================
// 主程序
// ============================================================================
begin
  TotalTests := 0;
  PassedTests := 0;
  FailedTests := 0;
  SkippedTests := 0;

  WriteLn('========================================');
  WriteLn('fafafa.ssl JSON HTTP 客户端测试');
  WriteLn('========================================');
  WriteLn;
  WriteLn('注意: 测试需要网络连接');
  WriteLn('      使用 jsonplaceholder.typicode.com');
  WriteLn('      和 httpbin.org 作为测试服务器');

  try
    Test1_GetJSON;
    Test2_PostJSON;
    Test3_PostJSONString;
    Test4_PutJSON;
    Test5_DeleteJSON;
    Test6_GetJSONArray;
    Test7_UserListAPI;
    Test8_HttpbinJSON;
    Test9_Error404;
    Test10_InvalidJSON;

    PrintSummary;

    if FailedTests > 0 then
      Halt(1)
    else
      Halt(0);

  except
    on E: Exception do
    begin
      WriteLn('致命错误: ', E.Message);
      Halt(1);
    end;
  end;
end.
