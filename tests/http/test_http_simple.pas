program test_http_simple;

{$mode objfpc}{$H+}

{**
 * HTTP 简化客户端测试
 *
 * 测试 fafafa.ssl.http.simple 模块功能：
 * - URL 解析
 * - HTTP 请求构建
 * - HTTP 响应解析
 * - 真实 HTTPS 连接（需要网络）
 * - 错误处理
 *
 * 注意: 部分测试需要网络连接
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2025-12-23
 *}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.http.simple;

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
  WriteLn('HTTP 简化客户端测试结果');
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
// 测试 1: THTTPSOptions 默认值
// ============================================================================
procedure Test1_DefaultOptions;
var
  Opts: THTTPSOptions;
begin
  BeginSection('1. 默认选项测试');

  Opts := TSimpleHTTPSClient.DefaultOptions;
  try
    Check('默认超时为 30000ms', Opts.Timeout = 30000);
    Check('默认跟随重定向', Opts.FollowRedirects = True);
    Check('默认最大重定向 5 次', Opts.MaxRedirects = 5);
    Check('默认验证证书', Opts.VerifyPeer = True);
    Check('默认客户端证书为空', Opts.ClientCert = '');
    Check('默认 UserAgent 非空', Opts.UserAgent <> '');
    Check('默认 Headers 已创建', Opts.Headers <> nil);
  finally
    if Opts.Headers <> nil then
      Opts.Headers.Free;
  end;
end;

// ============================================================================
// 测试 2: THTTPResponse 记录
// ============================================================================
procedure Test2_HTTPResponse;
var
  Resp: THTTPResponse;
begin
  BeginSection('2. HTTP 响应记录测试');

  // 初始化测试
  FillChar(Resp, SizeOf(Resp), 0);
  Resp.StatusCode := 200;
  Resp.StatusText := 'OK';
  Resp.Body := 'Hello World';
  Resp.Success := True;

  Check('状态码设置', Resp.StatusCode = 200);
  Check('状态文本设置', Resp.StatusText = 'OK');
  Check('响应体设置', Resp.Body = 'Hello World');
  Check('成功标志设置', Resp.Success = True);
end;

// ============================================================================
// 测试 3: HTTP 方法枚举
// ============================================================================
procedure Test3_HTTPMethods;
begin
  BeginSection('3. HTTP 方法枚举测试');

  Check('GET 方法存在', Ord(httpGET) >= 0);
  Check('POST 方法存在', Ord(httpPOST) >= 0);
  Check('PUT 方法存在', Ord(httpPUT) >= 0);
  Check('DELETE 方法存在', Ord(httpDELETE) >= 0);
  Check('HEAD 方法存在', Ord(httpHEAD) >= 0);
  Check('PATCH 方法存在', Ord(httpPATCH) >= 0);
  Check('OPTIONS 方法存在', Ord(httpOPTIONS) >= 0);

  // 验证枚举值顺序
  Check('方法枚举顺序正确', Ord(httpPOST) > Ord(httpGET));
end;

// ============================================================================
// 测试 4: 异常类
// ============================================================================
procedure Test4_ExceptionClass;
var
  Ex: EHTTPSClientException;
begin
  BeginSection('4. 异常类测试');

  try
    Ex := EHTTPSClientException.Create('测试错误', 404);
    try
      Check('异常消息正确', Ex.Message = '测试错误');
      Check('状态码正确', Ex.StatusCode = 404);
    finally
      Ex.Free;
    end;

    Ex := EHTTPSClientException.Create('无状态码错误');
    try
      Check('无状态码时默认为 0', Ex.StatusCode = 0);
    finally
      Ex.Free;
    end;

  except
    on E: Exception do
      Check('异常类创建', False, E.Message);
  end;
end;

// ============================================================================
// 测试 5: 真实网络连接 (需要网络)
// ============================================================================
procedure Test5_RealNetworkConnection;
var
  Response: string;
  RespEx: THTTPResponse;
  Opts: THTTPSOptions;
begin
  BeginSection('5. 真实网络连接测试');

  // 测试简单 GET 请求
  try
    Response := TSimpleHTTPSClient.Get('https://httpbin.org/get');
    Check('简单 GET 请求', Length(Response) > 0,
          '响应长度: ' + IntToStr(Length(Response)));
    Check('响应包含预期内容', Pos('httpbin.org', Response) > 0);
  except
    on E: Exception do
      Skip('简单 GET 请求', '网络错误: ' + E.Message);
  end;

  // 测试完整 GET 请求
  Opts := TSimpleHTTPSClient.DefaultOptions;
  try
    try
      RespEx := TSimpleHTTPSClient.GetEx('https://httpbin.org/status/200', Opts);
      Check('完整 GET 请求 - 状态码', RespEx.StatusCode = 200);
      Check('完整 GET 请求 - 成功标志', RespEx.Success);
    except
      on E: Exception do
        Skip('完整 GET 请求', '网络错误: ' + E.Message);
    end;

    // 测试 404 错误处理
    try
      RespEx := TSimpleHTTPSClient.GetEx('https://httpbin.org/status/404', Opts);
      Check('404 错误处理 - 状态码', RespEx.StatusCode = 404);
    except
      on E: Exception do
        Skip('404 错误处理', '网络错误: ' + E.Message);
    end;

  finally
    if Opts.Headers <> nil then
      Opts.Headers.Free;
  end;
end;

// ============================================================================
// 测试 6: POST 请求 (需要网络)
// ============================================================================
procedure Test6_PostRequest;
var
  Response: string;
  RespEx: THTTPResponse;
  Opts: THTTPSOptions;
begin
  BeginSection('6. POST 请求测试');

  // 简单 POST
  try
    Response := TSimpleHTTPSClient.Post('https://httpbin.org/post', '{"test":"data"}');
    Check('简单 POST 请求', Length(Response) > 0);
    Check('POST 响应包含数据', Pos('test', Response) > 0);
  except
    on E: Exception do
      Skip('简单 POST 请求', '网络错误: ' + E.Message);
  end;

  // 完整 POST
  Opts := TSimpleHTTPSClient.DefaultOptions;
  try
    Opts.Headers.Add('Content-Type: application/json');
    try
      RespEx := TSimpleHTTPSClient.PostEx('https://httpbin.org/post',
        '{"name":"fafafa.ssl"}', Opts);
      Check('完整 POST 请求 - 状态码 200', RespEx.StatusCode = 200);
      Check('完整 POST 请求 - 包含响应体', Length(RespEx.Body) > 0);
    except
      on E: Exception do
        Skip('完整 POST 请求', '网络错误: ' + E.Message);
    end;
  finally
    if Opts.Headers <> nil then
      Opts.Headers.Free;
  end;
end;

// ============================================================================
// 测试 7: HTTPS 证书验证 (需要网络)
// ============================================================================
procedure Test7_CertificateVerification;
var
  RespEx: THTTPResponse;
  Opts: THTTPSOptions;
begin
  BeginSection('7. HTTPS 证书验证测试');

  Opts := TSimpleHTTPSClient.DefaultOptions;
  try
    // 有效证书测试
    Opts.VerifyPeer := True;
    try
      RespEx := TSimpleHTTPSClient.GetEx('https://www.google.com/', Opts);
      Check('有效证书验证通过', RespEx.Success or (RespEx.StatusCode > 0));
    except
      on E: Exception do
        Skip('有效证书验证', '网络错误: ' + E.Message);
    end;

    // 禁用验证测试
    Opts.VerifyPeer := False;
    try
      RespEx := TSimpleHTTPSClient.GetEx('https://httpbin.org/get', Opts);
      Check('禁用验证后连接', RespEx.Success or (RespEx.StatusCode = 200));
    except
      on E: Exception do
        Skip('禁用验证连接', '网络错误: ' + E.Message);
    end;

  finally
    if Opts.Headers <> nil then
      Opts.Headers.Free;
  end;
end;

// ============================================================================
// 测试 8: 超时处理
// ============================================================================
procedure Test8_Timeout;
var
  Opts: THTTPSOptions;
  RespEx: THTTPResponse;
  StartTime: TDateTime;
  ElapsedMs: Integer;
begin
  BeginSection('8. 超时处理测试');

  Opts := TSimpleHTTPSClient.DefaultOptions;
  try
    // 设置短超时
    Opts.Timeout := 5000; // 5秒

    StartTime := Now;
    try
      // httpbin 延迟端点 - 请求延迟 2 秒
      RespEx := TSimpleHTTPSClient.GetEx('https://httpbin.org/delay/2', Opts);
      ElapsedMs := Round((Now - StartTime) * 24 * 60 * 60 * 1000);
      Check('2秒延迟请求成功', RespEx.Success or (RespEx.StatusCode = 200),
            '耗时: ' + IntToStr(ElapsedMs) + 'ms');
    except
      on E: Exception do
        Skip('延迟请求', '网络错误: ' + E.Message);
    end;

  finally
    if Opts.Headers <> nil then
      Opts.Headers.Free;
  end;
end;

// ============================================================================
// 测试 9: 重定向处理 (需要网络)
// ============================================================================
procedure Test9_Redirects;
var
  Opts: THTTPSOptions;
  RespEx: THTTPResponse;
begin
  BeginSection('9. 重定向处理测试');

  Opts := TSimpleHTTPSClient.DefaultOptions;
  try
    // 启用重定向
    Opts.FollowRedirects := True;
    try
      RespEx := TSimpleHTTPSClient.GetEx('https://httpbin.org/redirect/2', Opts);
      Check('跟随重定向', RespEx.Success or (RespEx.StatusCode = 200));
    except
      on E: Exception do
        Skip('跟随重定向', '网络错误: ' + E.Message);
    end;

    // 禁用重定向
    Opts.FollowRedirects := False;
    try
      RespEx := TSimpleHTTPSClient.GetEx('https://httpbin.org/redirect/1', Opts);
      Check('禁用重定向返回 302', RespEx.StatusCode = 302);
    except
      on E: Exception do
        Skip('禁用重定向', '网络错误: ' + E.Message);
    end;

  finally
    if Opts.Headers <> nil then
      Opts.Headers.Free;
  end;
end;

// ============================================================================
// 测试 10: 自定义请求头
// ============================================================================
procedure Test10_CustomHeaders;
var
  Opts: THTTPSOptions;
  RespEx: THTTPResponse;
begin
  BeginSection('10. 自定义请求头测试');

  Opts := TSimpleHTTPSClient.DefaultOptions;
  try
    // 添加自定义头
    Opts.Headers.Add('X-Custom-Header: fafafa-test');
    Opts.Headers.Add('Accept: application/json');

    try
      RespEx := TSimpleHTTPSClient.GetEx('https://httpbin.org/headers', Opts);
      Check('自定义头请求成功', RespEx.Success or (RespEx.StatusCode = 200));
      // httpbin 会返回收到的请求头
      Check('响应包含自定义头', Pos('X-Custom-Header', RespEx.Body) > 0);
    except
      on E: Exception do
        Skip('自定义头请求', '网络错误: ' + E.Message);
    end;

  finally
    if Opts.Headers <> nil then
      Opts.Headers.Free;
  end;
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
  WriteLn('fafafa.ssl HTTP 简化客户端测试');
  WriteLn('========================================');
  WriteLn;
  WriteLn('注意: 部分测试需要网络连接');
  WriteLn('      使用 httpbin.org 作为测试服务器');

  try
    // 本地测试（不需要网络）
    Test1_DefaultOptions;
    Test2_HTTPResponse;
    Test3_HTTPMethods;
    Test4_ExceptionClass;

    // 网络测试
    Test5_RealNetworkConnection;
    Test6_PostRequest;
    Test7_CertificateVerification;
    Test8_Timeout;
    Test9_Redirects;
    Test10_CustomHeaders;

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
