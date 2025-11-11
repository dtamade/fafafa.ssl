program https_client_auth;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{ 生产级HTTPS客户端 - 客户端证书认证示例
  
  功能：
  - 双向TLS认证（mTLS）
  - 客户端证书加载
  - 私钥加载
  - 证书链验证
  
  使用方法：
    ./https_client_auth <URL> <cert.pem> <key.pem> [ca.pem]
  
  示例：
    ./https_client_auth https://example.com client.pem client.key ca.pem
}

uses
  SysUtils, Classes, DateUtils, Math,
  fafafa.ssl, fafafa.ssl.base;

const
  BUFFER_SIZE = 16384;

type
  TAuthConfig = record
    URL: string;
    CertFile: string;
    KeyFile: string;
    CAFile: string;
  end;

var
  GConfig: TAuthConfig;
  GVerbose: Boolean;

procedure Log(const AMsg: string);
begin
  if GVerbose then
    WriteLn('[', FormatDateTime('hh:nn:ss', Now), '] ', AMsg);
end;

function FileExists(const AFileName: string): Boolean;
begin
  Result := SysUtils.FileExists(AFileName);
end;

function LoadFileAsString(const AFileName: string): string;
var
  LStream: TFileStream;
begin
  Result := '';
  if not FileExists(AFileName) then
  begin
    WriteLn('错误: 文件不存在: ', AFileName);
    Exit;
  end;
  
  LStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, LStream.Size);
    if LStream.Size > 0 then
      LStream.Read(Result[1], LStream.Size);
  finally
    LStream.Free;
  end;
end;

function ParseURL(const AURL: string; out AHost, APath: string; out APort: Word): Boolean;
var
  LPos: Integer;
  LTemp: string;
begin
  Result := False;
  APort := 443;
  
  LTemp := AURL;
  if Pos('https://', LowerCase(LTemp)) = 1 then
    Delete(LTemp, 1, 8);
  
  LPos := Pos('/', LTemp);
  if LPos > 0 then
  begin
    AHost := Copy(LTemp, 1, LPos - 1);
    APath := Copy(LTemp, LPos, Length(LTemp));
  end
  else
  begin
    AHost := LTemp;
    APath := '/';
  end;
  
  Result := (AHost <> '');
end;

function BuildHTTPRequest(const AHost, APath: string): RawByteString;
begin
  Result := 'GET ' + APath + ' HTTP/1.1' + #13#10 +
            'Host: ' + AHost + #13#10 +
            'User-Agent: fafafa.ssl-mtls/1.0' + #13#10 +
            'Accept: */*' + #13#10 +
            'Connection: close' + #13#10 +
            #13#10;
end;

function ReadResponse(AConnection: ISSLConnection): string;
var
  LBuffer: array[0..BUFFER_SIZE-1] of Byte;
  LBytesRead: Integer;
  LResponse: TMemoryStream;
begin
  Result := '';
  LResponse := TMemoryStream.Create;
  try
    repeat
      try
        LBytesRead := AConnection.Read(LBuffer[0], BUFFER_SIZE);
        if LBytesRead > 0 then
          LResponse.Write(LBuffer[0], LBytesRead);
      except
        Break;
      end;
    until LBytesRead = 0;
    
    if LResponse.Size > 0 then
    begin
      SetLength(Result, LResponse.Size);
      LResponse.Position := 0;
      LResponse.Read(Result[1], LResponse.Size);
    end;
  finally
    LResponse.Free;
  end;
end;

function DoAuthenticatedRequest(const AConfig: TAuthConfig;
  out AResponse: string): Boolean;
var
  LContext: ISSLContext;
  LConnection: ISSLConnection;
  LHost, APath: string;
  LPort: Word;
  LRequest: RawByteString;
  LStartTime: TDateTime;
  LCertPEM, LKeyPEM, LCAPEM: string;
begin
  Result := False;
  
  // 验证文件存在
  Log('验证证书文件...');
  if not FileExists(AConfig.CertFile) then
  begin
    WriteLn('错误: 证书文件不存在: ', AConfig.CertFile);
    Exit;
  end;
  
  if not FileExists(AConfig.KeyFile) then
  begin
    WriteLn('错误: 私钥文件不存在: ', AConfig.KeyFile);
    Exit;
  end;
  
  // 加载证书和密钥
  Log('加载客户端证书...');
  LCertPEM := LoadFileAsString(AConfig.CertFile);
  if LCertPEM = '' then
  begin
    WriteLn('错误: 无法加载证书文件');
    Exit;
  end;
  
  Log('加载客户端私钥...');
  LKeyPEM := LoadFileAsString(AConfig.KeyFile);
  if LKeyPEM = '' then
  begin
    WriteLn('错误: 无法加载私钥文件');
    Exit;
  end;
  
  // 加载CA证书（如果提供）
  if (AConfig.CAFile <> '') and FileExists(AConfig.CAFile) then
  begin
    Log('加载CA证书...');
    LCAPEM := LoadFileAsString(AConfig.CAFile);
  end;
  
  // 解析URL
  Log('解析URL: ' + AConfig.URL);
  if not ParseURL(AConfig.URL, LHost, APath, LPort) then
  begin
    WriteLn('错误: URL解析失败');
    Exit;
  end;
  
  try
    LStartTime := Now;
    
    // 创建SSL上下文
    Log('创建SSL上下文...');
    LContext := TSSLFactory.CreateContext(sslOpenSSL, sslCtxClient);
    if LContext = nil then
    begin
      WriteLn('错误: 创建SSL上下文失败');
      Exit;
    end;
    
    // 配置证书验证
    Log('配置证书验证...');
    if LCAPEM <> '' then
    begin
      // 使用自定义CA
      LContext.SetVerifyMode([sslVerifyPeer]);
      // 注意：这里需要实际的LoadCAFromPEM方法，示例中简化
      Log('警告: 自定义CA加载功能需要实现');
    end
    else
    begin
      // 使用系统CA
      LContext.SetVerifyMode([sslVerifyPeer]);
    end;
    
    // 加载客户端证书和私钥
    Log('设置客户端证书...');
    try
      LContext.LoadCertificate(AConfig.CertFile);
      LContext.LoadPrivateKey(AConfig.KeyFile);
      Log('客户端证书设置成功');
    except
      on E: Exception do
      begin
        WriteLn('错误: 加载客户端证书失败: ', E.Message);
        Exit;
      end;
    end;
    
    // 创建连接
    Log('创建SSL连接...');
    LConnection := LContext.CreateConnection;
    if LConnection = nil then
    begin
      WriteLn('错误: 创建连接失败');
      Exit;
    end;
    
    // 连接到服务器
    Log(Format('连接到 %s:%d...', [LHost, LPort]));
    LConnection.Connect(LHost, LPort);
    Log('连接成功 - 客户端证书已发送');
    
    // 获取连接信息
    try
      Log(Format('TLS版本: %s', [
        ProtocolVersionToString(LConnection.GetProtocolVersion)]));
      Log(Format('密码套件: %s', [LConnection.GetCipherName]));
      
      // 验证服务器证书
      WriteLn('服务器证书验证: ', 
        Math.IfThen(LConnection.GetVerifyResult = 0, '成功', '失败'));
    except
      Log('警告: 无法获取连接信息');
    end;
    
    // 发送HTTP请求
    LRequest := BuildHTTPRequest(LHost, APath);
    Log('发送HTTP请求...');
    if LConnection.Write(LRequest[1], Length(LRequest)) <> Length(LRequest) then
    begin
      WriteLn('错误: 发送请求失败');
      Exit;
    end;
    
    // 读取响应
    Log('读取响应...');
    AResponse := ReadResponse(LConnection);
    
    Log(Format('请求完成 (耗时 %d ms)', 
      [MilliSecondsBetween(Now, LStartTime)]));
    
    Result := True;
    
  except
    on E: ESSLException do
    begin
      WriteLn('SSL错误: ', E.Message);
      WriteLn('错误码: ', IntToStr(Ord(E.ErrorCode)));
      Exit;
    end;
    on E: Exception do
    begin
      WriteLn('错误: ', E.Message);
      Exit;
    end;
  end;
end;

procedure ShowResponse(const AResponse: string);
var
  LLines: TStringList;
  i: Integer;
begin
  WriteLn;
  WriteLn('=== 响应 ===');
  
  LLines := TStringList.Create;
  try
    LLines.Text := AResponse;
    
    // 显示前20行或所有行
    for i := 0 to Min(LLines.Count - 1, 19) do
      WriteLn(LLines[i]);
      
    if LLines.Count > 20 then
      WriteLn('... (', LLines.Count - 20, ' 行省略)');
  finally
    LLines.Free;
  end;
end;

procedure ShowUsage;
begin
  WriteLn('用法: https_client_auth <URL> <cert.pem> <key.pem> [ca.pem]');
  WriteLn;
  WriteLn('参数:');
  WriteLn('  URL       - 目标URL（必须是HTTPS）');
  WriteLn('  cert.pem  - 客户端证书文件（PEM格式）');
  WriteLn('  key.pem   - 客户端私钥文件（PEM格式）');
  WriteLn('  ca.pem    - CA证书文件（可选，PEM格式）');
  WriteLn;
  WriteLn('示例:');
  WriteLn('  https_client_auth https://example.com client.pem client.key');
  WriteLn('  https_client_auth https://example.com client.pem client.key ca.pem');
  WriteLn;
  WriteLn('说明:');
  WriteLn('  此示例演示如何使用客户端证书进行双向TLS认证。');
  WriteLn('  服务器必须配置为要求或接受客户端证书。');
  WriteLn;
end;

procedure Main;
var
  LResponse: string;
begin
  GVerbose := True;
  
  WriteLn('fafafa.ssl - 客户端证书认证示例');
  WriteLn('==================================');
  WriteLn;
  
  // 解析参数
  if ParamCount < 3 then
  begin
    ShowUsage;
    ExitCode := 1;
    Exit;
  end;
  
  GConfig.URL := ParamStr(1);
  GConfig.CertFile := ParamStr(2);
  GConfig.KeyFile := ParamStr(3);
  GConfig.CAFile := '';
  
  if ParamCount >= 4 then
    GConfig.CAFile := ParamStr(4);
  
  WriteLn('URL: ', GConfig.URL);
  WriteLn('证书: ', GConfig.CertFile);
  WriteLn('私钥: ', GConfig.KeyFile);
  if GConfig.CAFile <> '' then
    WriteLn('CA: ', GConfig.CAFile);
  WriteLn;
  
  try
    if DoAuthenticatedRequest(GConfig, LResponse) then
    begin
      ShowResponse(LResponse);
      WriteLn;
      WriteLn('认证请求成功！');
      ExitCode := 0;
    end
    else
    begin
      WriteLn;
      WriteLn('认证请求失败！');
      ExitCode := 1;
    end;
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('致命错误: ', E.Message);
      ExitCode := 2;
    end;
  end;
end;

begin
  Main;
end.


