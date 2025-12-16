program https_client_post;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{ HTTPS POST 示例
  展示如何发送JSON或表单数据并查看响应。 }

uses
  SysUtils, Classes,
  fafafa.ssl.http.simple;

const
  DEFAULT_URL = 'https://httpbin.org/post';
  DEFAULT_BODY = '{"message":"hello from fafafa.ssl"}';
  DEFAULT_CONTENT_TYPE = 'application/json';

procedure PrintHeader;
begin
  WriteLn('============================================');
  WriteLn(' fafafa.ssl - HTTPS 客户端示例 #2 (POST)');
  WriteLn('============================================');
  WriteLn('用法: ./https_client_post [URL] [BODY] [CONTENT-TYPE]');
  WriteLn('缺省 URL: ', DEFAULT_URL);
  WriteLn('缺省内容: ', DEFAULT_BODY);
  WriteLn('缺省类型: ', DEFAULT_CONTENT_TYPE);
  WriteLn;
end;

procedure DumpResponseHeaders(AHeaders: TStringList);
var
  i: Integer;
begin
  if (AHeaders = nil) or (AHeaders.Count = 0) then
    Exit;
  WriteLn('响应头:');
  for i := 0 to AHeaders.Count - 1 do
    WriteLn('  ', AHeaders[i]);
end;

procedure ExecuteRequest(const AURL, ABody, AContentType: string);
var
  LOptions: THTTPSOptions;
  LResponse: THTTPResponse;
begin
  LOptions := TSimpleHTTPSClient.DefaultOptions;
  try
    LOptions.Headers.Add('Content-Type: ' + AContentType);
    LOptions.Headers.Add('Accept: application/json');
    LOptions.Headers.Add('X-Demo-Client: fafafa.ssl');
    LResponse := TSimpleHTTPSClient.PostEx(AURL, ABody, LOptions);
    try
      if LResponse.Success then
      begin
        WriteLn('✓ 请求成功');
        WriteLn('状态: ', LResponse.StatusCode, ' ', LResponse.StatusText);
        DumpResponseHeaders(LResponse.Headers);
        WriteLn;
        WriteLn('响应体:');
        WriteLn(LResponse.Body);
      end
      else
      begin
        WriteLn('✗ 请求失败');
        if LResponse.StatusCode <> 0 then
          WriteLn('HTTP 状态码: ', LResponse.StatusCode);
        WriteLn('错误: ', LResponse.ErrorMessage);
      end;
    finally
      if LResponse.Headers <> nil then
        LResponse.Headers.Free;
    end;
  finally
    LOptions.Headers.Free;
  end;
end;

var
  LURL, LBody, LContentType: string;
begin
  PrintHeader;
  if ParamCount >= 1 then
    LURL := ParamStr(1)
  else
    LURL := DEFAULT_URL;
  if ParamCount >= 2 then
    LBody := ParamStr(2)
  else
    LBody := DEFAULT_BODY;
  if ParamCount >= 3 then
    LContentType := ParamStr(3)
  else
    LContentType := DEFAULT_CONTENT_TYPE;
  ExecuteRequest(LURL, LBody, LContentType);
end.
