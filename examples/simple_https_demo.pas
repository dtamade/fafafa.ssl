program simple_https_demo;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{ 演示简化HTTPS客户端的使用
  
  展示如何用一行代码实现HTTPS请求
}

uses
  SysUtils, fafafa.ssl.http.simple;

procedure Demo1_SimpleGet;
var
  LResponse: string;
begin
  WriteLn('=== 示例1: 简单GET请求 ===');
  WriteLn;
  
  try
    // 一行代码完成HTTPS GET请求
    LResponse := TSimpleHTTPSClient.Get('https://www.google.com');
    
    WriteLn('请求成功！');
    WriteLn('响应长度: ', Length(LResponse), ' 字节');
    WriteLn('前100字符:');
    WriteLn(Copy(LResponse, 1, 100));
    WriteLn;
  except
    on E: Exception do
      WriteLn('错误: ', E.Message);
  end;
end;

procedure Demo2_SimplePost;
var
  LResponse: string;
  LJSON: string;
begin
  WriteLn('=== 示例2: 简单POST请求 ===');
  WriteLn;
  
  try
    // 构建JSON数据
    LJSON := '{"name":"test","value":"hello"}';
    
    // 一行代码完成HTTPS POST请求
    LResponse := TSimpleHTTPSClient.Post('https://httpbin.org/post', LJSON);
    
    WriteLn('请求成功！');
    WriteLn('响应: ', LResponse);
    WriteLn;
  except
    on E: Exception do
      WriteLn('错误: ', E.Message);
  end;
end;

procedure Demo3_AdvancedGet;
var
  LResponse: THTTPResponse;
  LOptions: THTTPSOptions;
  i: Integer;
begin
  WriteLn('=== 示例3: 高级GET请求（自定义选项）===');
  WriteLn;
  
  try
    // 创建自定义选项
    LOptions := TSimpleHTTPSClient.DefaultOptions;
    LOptions.Timeout := 10000;  // 10秒超时
    LOptions.UserAgent := 'MyApp/1.0';
    
    // 添加自定义请求头
    LOptions.Headers.Add('Accept: application/json');
    LOptions.Headers.Add('X-Custom-Header: CustomValue');
    
    // 执行请求
    LResponse := TSimpleHTTPSClient.GetEx('https://httpbin.org/get', LOptions);
    
    if LResponse.Success then
    begin
      WriteLn('请求成功！');
      WriteLn('状态码: ', LResponse.StatusCode, ' ', LResponse.StatusText);
      WriteLn('响应头:');
      for i := 0 to LResponse.Headers.Count - 1 do
        WriteLn('  ', LResponse.Headers[i]);
      WriteLn;
      WriteLn('响应体:');
      WriteLn(LResponse.Body);
    end
    else
    begin
      WriteLn('请求失败: ', LResponse.ErrorMessage);
    end;
    
    // 清理
    LOptions.Headers.Free;
    LResponse.Headers.Free;
    WriteLn;
  except
    on E: Exception do
      WriteLn('错误: ', E.Message);
  end;
end;

procedure Demo4_DownloadFile;
begin
  WriteLn('=== 示例4: 下载文件 ===');
  WriteLn;
  
  try
    // 下载文件
    if TSimpleHTTPSClient.Download(
      'https://www.google.com/robots.txt', 
      'robots.txt') then
    begin
      WriteLn('文件下载成功: robots.txt');
    end
    else
    begin
      WriteLn('文件下载失败');
    end;
    WriteLn;
  except
    on E: Exception do
      WriteLn('错误: ', E.Message);
  end;
end;

procedure ShowComparison;
begin
  WriteLn('=== 代码量对比 ===');
  WriteLn;
  WriteLn('传统方式（约20行）:');
  WriteLn('  var LContext, LConnection, LRequest, LResponse, ...');
  WriteLn('  LContext := TSSLFactory.CreateContext(...);');
  WriteLn('  LConnection := LContext.CreateConnection;');
  WriteLn('  LConnection.Connect(...);');
  WriteLn('  ... (更多代码)');
  WriteLn;
  WriteLn('简化方式（1行）:');
  WriteLn('  LResponse := TSimpleHTTPSClient.Get(''https://example.com'');');
  WriteLn;
  WriteLn('代码量减少 95%！ 🎉');
  WriteLn;
end;

begin
  WriteLn('fafafa.ssl - 简化HTTPS客户端演示');
  WriteLn('==================================');
  WriteLn;
  
  ShowComparison;
  
  Demo1_SimpleGet;
  Demo2_SimplePost;
  Demo3_AdvancedGet;
  Demo4_DownloadFile;
  
  WriteLn('演示完成！');
  WriteLn;
  WriteLn('提示:');
  WriteLn('  - 简单场景使用 Get/Post 方法');
  WriteLn('  - 复杂场景使用 GetEx/PostEx 方法');
  WriteLn('  - 文件操作使用 Download/Upload 方法');
  WriteLn;
end.


