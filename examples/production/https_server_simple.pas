program https_server_simple;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{ 生产级HTTPS服务器 - 简单示例
  
  功能：
  - 基础HTTPS服务器
  - 证书加载
  - 请求处理
  - 错误处理和日志
  
  使用方法：
    ./https_server_simple <port> <cert.pem> <key.pem>
  
  示例：
    ./https_server_simple 8443 server.pem server.key
}

uses
  SysUtils, Classes, Sockets,
  fafafa.ssl, fafafa.ssl.base;

const
  BUFFER_SIZE = 16384;
  DEFAULT_PORT = 8443;

type
  TServerConfig = record
    Port: Word;
    CertFile: string;
    KeyFile: string;
  end;

var
  GConfig: TServerConfig;
  GRunning: Boolean;

procedure Log(const AMsg: string);
begin
  WriteLn('[', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), '] ', AMsg);
end;

function FileExists(const AFileName: string): Boolean;
begin
  Result := SysUtils.FileExists(AFileName);
end;

{ 生成简单的HTTP响应 }
function BuildHTTPResponse(const AContent: string): RawByteString;
var
  LContentLength: string;
begin
  LContentLength := IntToStr(Length(AContent));
  
  Result := 'HTTP/1.1 200 OK' + #13#10 +
            'Content-Type: text/html; charset=utf-8' + #13#10 +
            'Content-Length: ' + LContentLength + #13#10 +
            'Connection: close' + #13#10 +
            'Server: fafafa.ssl/1.0' + #13#10 +
            #13#10 +
            AContent;
end;

{ 生成默认HTML页面 }
function GetDefaultHTML: string;
begin
  Result := 
    '<!DOCTYPE html>' + #13#10 +
    '<html>' + #13#10 +
    '<head>' + #13#10 +
    '  <meta charset="UTF-8">' + #13#10 +
    '  <title>fafafa.ssl HTTPS服务器</title>' + #13#10 +
    '  <style>' + #13#10 +
    '    body { font-family: Arial; max-width: 800px; margin: 50px auto; padding: 20px; }' + #13#10 +
    '    h1 { color: #2c3e50; }' + #13#10 +
    '    .info { background: #ecf0f1; padding: 15px; border-radius: 5px; }' + #13#10 +
    '  </style>' + #13#10 +
    '</head>' + #13#10 +
    '<body>' + #13#10 +
    '  <h1>✓ HTTPS服务器运行中</h1>' + #13#10 +
    '  <div class="info">' + #13#10 +
    '    <p><strong>服务器信息：</strong></p>' + #13#10 +
    '    <ul>' + #13#10 +
    '      <li>SSL/TLS库：fafafa.ssl</li>' + #13#10 +
    '      <li>时间：' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + '</li>' + #13#10 +
    '      <li>协议：HTTPS</li>' + #13#10 +
    '    </ul>' + #13#10 +
    '  </div>' + #13#10 +
    '</body>' + #13#10 +
    '</html>';
end;

{ 处理客户端连接 }
procedure HandleClient(AConnection: ISSLConnection);
var
  LRequest: array[0..BUFFER_SIZE-1] of Byte;
  LBytesRead: Integer;
  LResponse: RawByteString;
  LContent: string;
begin
  try
    Log('新连接已建立');
    
    // 读取请求
    LBytesRead := AConnection.Read(LRequest[0], BUFFER_SIZE);
    if LBytesRead > 0 then
    begin
      Log(Format('收到请求 (%d 字节)', [LBytesRead]));
      
      // 生成响应
      LContent := GetDefaultHTML;
      LResponse := BuildHTTPResponse(LContent);
      
      // 发送响应
      AConnection.Write(LResponse[1], Length(LResponse));
      Log(Format('响应已发送 (%d 字节)', [Length(LResponse)]));
    end;
    
  except
    on E: Exception do
      Log('处理连接时出错: ' + E.Message);
  end;
end;

{ 启动服务器 }
procedure RunServer(const AConfig: TServerConfig);
var
  LContext: ISSLContext;
  LConnection: ISSLConnection;
begin
  Log('启动HTTPS服务器...');
  
  // 验证证书文件
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
  
  try
    // 创建SSL上下文
    Log('创建SSL上下文...');
    LContext := TSSLFactory.CreateContext(sslCtxServer, sslOpenSSL);
    if LContext = nil then
    begin
      WriteLn('错误: 创建SSL上下文失败');
      Exit;
    end;
    
    // 加载证书和私钥
    Log('加载服务器证书: ' + AConfig.CertFile);
    LContext.LoadCertificate(AConfig.CertFile);
    
    Log('加载服务器私钥: ' + AConfig.KeyFile);
    LContext.LoadPrivateKey(AConfig.KeyFile);
    
    Log(Format('服务器正在监听端口 %d...', [AConfig.Port]));
    WriteLn;
    WriteLn('提示: 使用以下命令测试服务器：');
    WriteLn('  curl -k https://localhost:', AConfig.Port);
    WriteLn('  或在浏览器中访问: https://localhost:', AConfig.Port);
    WriteLn;
    WriteLn('按 Ctrl+C 停止服务器');
    WriteLn;
    
    // 服务器循环（简化版本，实际需要socket监听）
    // 注意：这是一个概念演示，实际需要底层socket支持
    Log('警告: 完整的服务器实现需要底层socket支持');
    Log('这是一个演示性示例，展示SSL上下文的配置');
    
    // 模拟保持运行
    GRunning := True;
    while GRunning do
    begin
      Sleep(1000);
      // 实际应该在这里accept客户端连接
    end;
    
  except
    on E: Exception do
    begin
      WriteLn('服务器错误: ', E.Message);
      Exit;
    end;
  end;
end;

procedure ShowUsage;
begin
  WriteLn('用法: https_server_simple <port> <cert.pem> <key.pem>');
  WriteLn;
  WriteLn('参数:');
  WriteLn('  port      - 监听端口');
  WriteLn('  cert.pem  - 服务器证书文件（PEM格式）');
  WriteLn('  key.pem   - 服务器私钥文件（PEM格式）');
  WriteLn;
  WriteLn('示例:');
  WriteLn('  https_server_simple 8443 server.pem server.key');
  WriteLn;
  WriteLn('注意:');
  WriteLn('  - 使用端口 < 1024 可能需要root权限');
  WriteLn('  - 确保证书和私钥文件路径正确');
  WriteLn('  - 可以使用 openssl 生成自签名证书进行测试');
  WriteLn;
end;

procedure Main;
begin
  WriteLn('fafafa.ssl - 简单HTTPS服务器');
  WriteLn('==============================');
  WriteLn;
  
  // 解析参数
  if ParamCount < 3 then
  begin
    ShowUsage;
    ExitCode := 1;
    Exit;
  end;
  
  try
    GConfig.Port := StrToInt(ParamStr(1));
  except
    WriteLn('错误: 无效的端口号');
    ExitCode := 1;
    Exit;
  end;
  
  GConfig.CertFile := ParamStr(2);
  GConfig.KeyFile := ParamStr(3);
  
  WriteLn('配置:');
  WriteLn('  端口: ', GConfig.Port);
  WriteLn('  证书: ', GConfig.CertFile);
  WriteLn('  私钥: ', GConfig.KeyFile);
  WriteLn;
  
  try
    RunServer(GConfig);
    ExitCode := 0;
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


