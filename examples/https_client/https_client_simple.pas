program https_client_simple;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{ 简单HTTPS GET示例
  使用 TSimpleHTTPSClient 在几行代码内完成一次请求。
}

uses
  SysUtils,
  fafafa.ssl.http.simple;

const
  DEFAULT_URL = 'https://httpbin.org/get';
  PREVIEW_LENGTH = 200;

procedure PrintHeader;
begin
  WriteLn('=============================================');
  WriteLn(' fafafa.ssl - HTTPS 客户端示例 #1 (GET)');
  WriteLn('=============================================');
  WriteLn('用法: ./https_client_simple [URL]');
  WriteLn('缺省 URL: ', DEFAULT_URL);
  WriteLn;
end;

procedure PrintPreview(const ABody: string);
var
  LPreview: string;
begin
  if ABody = '' then
  begin
    WriteLn('响应体为空。');
    Exit;
  end;
  if Length(ABody) > PREVIEW_LENGTH then
    LPreview := Copy(ABody, 1, PREVIEW_LENGTH) + '... (截断)'
  else
    LPreview := ABody;
  WriteLn('内容预览:');
  WriteLn(LPreview);
end;

procedure ExecuteRequest(const AURL: string);
var
  LBody: string;
begin
  WriteLn('目标 URL: ', AURL);
  WriteLn('发送 GET 请求...');
  try
    LBody := TSimpleHTTPSClient.Get(AURL);
    WriteLn('✓ 请求成功');
    WriteLn('响应长度: ', Length(LBody), ' 字节');
    PrintPreview(LBody);
  except
    on E: EHTTPSClientException do
    begin
      WriteLn('✗ HTTP 请求失败 (', E.StatusCode, '): ', E.Message);
      HaltCode := 1;
    end;
    on E: Exception do
    begin
      WriteLn('✗ 未知错误: ', E.Message);
      HaltCode := 1;
    end;
  end;
end;

var
  LURL: string;
begin
  PrintHeader;
  if ParamCount >= 1 then
    LURL := ParamStr(1)
  else
    LURL := DEFAULT_URL;
  ExecuteRequest(LURL);
end.
