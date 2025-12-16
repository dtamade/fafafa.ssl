program https_client_auth;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{ HTTPS 客户端证书示例
  展示如何配置 mTLS (双向 TLS)。 }

uses
  SysUtils,
  fafafa.ssl.http.simple;

procedure PrintHeader;
begin
  WriteLn('=====================================================');
  WriteLn(' fafafa.ssl - HTTPS 客户端示例 #3 (客户端证书)');
  WriteLn('=====================================================');
  WriteLn('用法: ./https_client_auth <URL> <client_cert.pem> <client_key.pem> [ca_bundle.pem]');
  WriteLn('例如: ./https_client_auth https://localhost:8443/secure certs/client.crt certs/client.key ca.pem');
  WriteLn('提示: 可通过 scripts/local_tls_server.sh 启动本地测试服务。');
  WriteLn;
end;

procedure ValidateFile(const APath, ALabel: string);
begin
  if not FileExists(APath) then
  begin
    WriteLn('✗ 找不到 ', ALabel, ': ', APath);
    Halt(1);
  end;
end;

procedure ExecuteRequest(const AURL, ACertFile, AKeyFile, ACAFile: string);
var
  LOptions: THTTPSOptions;
  LResponse: THTTPResponse;
begin
  LOptions := TSimpleHTTPSClient.DefaultOptions;
  try
    LOptions.ClientCert := ACertFile;
    LOptions.ClientKey := AKeyFile;
    if ACAFile <> '' then
      LOptions.CAFile := ACAFile;
    LOptions.VerifyPeer := True;
    LOptions.Headers.Add('Accept: application/json');
    LResponse := TSimpleHTTPSClient.GetEx(AURL, LOptions);
    try
      if LResponse.Success then
      begin
        WriteLn('✓ 握手成功，服务器已接受客户端证书');
        WriteLn('状态码: ', LResponse.StatusCode, ' ', LResponse.StatusText);
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
  LURL, LCertFile, LKeyFile, LCAFile: string;
begin
  PrintHeader;
  if ParamCount < 3 then
  begin
    WriteLn('缺少参数，至少需要 URL、客户端证书和私钥。');
    Halt(1);
  end;
  LURL := ParamStr(1);
  LCertFile := ParamStr(2);
  LKeyFile := ParamStr(3);
  if ParamCount >= 4 then
    LCAFile := ParamStr(4)
  else
    LCAFile := '';
  ValidateFile(LCertFile, '客户端证书');
  ValidateFile(LKeyFile, '客户端私钥');
  if LCAFile <> '' then
    ValidateFile(LCAFile, 'CA 文件');
  ExecuteRequest(LURL, LCertFile, LKeyFile, LCAFile);
end.
