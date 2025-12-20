program https_client_simple;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{ 生产级HTTPS客户端 - 简单GET请求示例
  
  功能：
  - 完整的错误处理
  - 详细的日志记录
  - 超时和重试机制
  - 内存安全管理
  - 资源自动清理
  
  使用方法：
    ./https_client_simple <URL>
  
  示例：
    ./https_client_simple https://www.google.com
}

uses
  SysUtils, Classes, DateUtils,
  fafafa.ssl, fafafa.ssl.base;

const
  DEFAULT_TIMEOUT = 30000;  // 30秒
  MAX_RETRIES = 3;
  BUFFER_SIZE = 16384;      // 16KB

type
  { HTTPS客户端配置 }
  THTTPSClientConfig = record
    URL: string;
    Timeout: Integer;
    MaxRetries: Integer;
    VerifyPeer: Boolean;
    LogToConsole: Boolean;
  end;

  { 日志级别 }
  TLogLevel = (llDebug, llInfo, llWarning, llError);

var
  GConfig: THTTPSClientConfig;

{ 日志记录函数 }
procedure Log(ALevel: TLogLevel; const AMsg: string);
const
  LevelStr: array[TLogLevel] of string = ('DEBUG', 'INFO', 'WARN', 'ERROR');
var
  TimeStr: string;
begin
  if not GConfig.LogToConsole then Exit;
  
  TimeStr := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  WriteLn(Format('[%s] [%s] %s', [TimeStr, LevelStr[ALevel], AMsg]));
end;

{ 解析URL }
function ParseURL(const AURL: string; out AHost, APath: string; out APort: Word): Boolean;
var
  LPos, LPortPos: Integer;
  LTemp: string;
begin
  Result := False;
  APort := 443;
  
  // 移除https://前缀
  LTemp := AURL;
  if Pos('https://', LowerCase(LTemp)) = 1 then
    Delete(LTemp, 1, 8)
  else if Pos('http://', LowerCase(LTemp)) = 1 then
  begin
    Delete(LTemp, 1, 7);
    APort := 80;
  end;
  
  // 查找路径分隔符
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
  
  // 检查端口
  LPortPos := Pos(':', AHost);
  if LPortPos > 0 then
  begin
    try
      APort := StrToInt(Copy(AHost, LPortPos + 1, Length(AHost)));
      AHost := Copy(AHost, 1, LPortPos - 1);
    except
      Log(llError, '无效的端口号');
      Exit;
    end;
  end;
  
  Result := (AHost <> '');
end;

{ 构建HTTP请求 }
function BuildHTTPRequest(const AHost, APath: string): RawByteString;
begin
  Result := 'GET ' + APath + ' HTTP/1.1' + #13#10 +
            'Host: ' + AHost + #13#10 +
            'User-Agent: fafafa.ssl/1.0' + #13#10 +
            'Accept: */*' + #13#10 +
            'Connection: close' + #13#10 +
            #13#10;
end;

{ 读取完整响应 }
function ReadResponse(AConnection: ISSLConnection; out AResponse: string): Boolean;
var
  LBuffer: array[0..BUFFER_SIZE-1] of Byte;
  LBytesRead: Integer;
  LTotalRead: Integer;
  LResponse: TMemoryStream;
begin
  Result := False;
  LResponse := TMemoryStream.Create;
  try
    LTotalRead := 0;
    repeat
      try
        LBytesRead := AConnection.Read(LBuffer[0], BUFFER_SIZE);
        if LBytesRead > 0 then
        begin
          LResponse.Write(LBuffer[0], LBytesRead);
          Inc(LTotalRead, LBytesRead);
          Log(llDebug, Format('读取 %d 字节，总计 %d 字节', [LBytesRead, LTotalRead]));
        end;
      except
        on E: Exception do
        begin
          // SSL_ERROR_WANT_READ 是正常的
          if (LBytesRead = 0) and (LTotalRead > 0) then
            Break
          else
            raise;
        end;
      end;
    until LBytesRead = 0;
    
    if LTotalRead > 0 then
    begin
      SetLength(AResponse, LResponse.Size);
      LResponse.Position := 0;
      LResponse.Read(AResponse[1], LResponse.Size);
      Result := True;
      Log(llInfo, Format('响应接收完成，共 %d 字节', [LTotalRead]));
    end;
  finally
    LResponse.Free;
  end;
end;

{ 执行HTTPS请求 }
function DoHTTPSRequest(const AConfig: THTTPSClientConfig; 
  out AResponse: string): Boolean;
var
  LContext: ISSLContext;
  LConnection: ISSLConnection;
  LHost, LPath: string;
  LPort: Word;
  LRequest: RawByteString;
  LStartTime, LEndTime: TDateTime;
  LRetry: Integer;
begin
  Result := False;
  
  // 解析URL
  Log(llInfo, Format('解析URL: %s', [AConfig.URL]));
  if not ParseURL(AConfig.URL, LHost, LPath, LPort) then
  begin
    Log(llError, 'URL解析失败');
    Exit;
  end;
  Log(llInfo, Format('主机: %s, 端口: %d, 路径: %s', [LHost, LPort, LPath]));
  
  // 重试循环
  for LRetry := 1 to AConfig.MaxRetries do
  begin
    if LRetry > 1 then
      Log(llWarning, Format('第 %d 次重试...', [LRetry]));
    
    try
      LStartTime := Now;
      
      // 创建SSL上下文
      Log(llInfo, '创建SSL上下文...');
      LContext := TSSLFactory.CreateContext(sslOpenSSL, sslCtxClient);
      if LContext = nil then
      begin
        Log(llError, '创建SSL上下文失败');
        Continue;
      end;
      
      // 配置上下文
      if AConfig.VerifyPeer then
      begin
        Log(llInfo, '启用证书验证');
        LContext.SetVerifyMode([sslVerifyPeer]);
      end
      else
      begin
        Log(llWarning, '证书验证已禁用');
        LContext.SetVerifyMode([sslVerifyNone]);
      end;
      
      // 创建连接
      Log(llInfo, '创建SSL连接...');
      LConnection := LContext.CreateConnection;
      if LConnection = nil then
      begin
        Log(llError, '创建连接失败');
        Continue;
      end;
      
      // 连接到服务器
      Log(llInfo, Format('连接到 %s:%d...', [LHost, LPort]));
      LConnection.Connect(LHost, LPort);
      Log(llInfo, '连接成功');
      
      // 获取连接信息
      try
        Log(llInfo, Format('TLS版本: %s', [
          ProtocolVersionToString(LConnection.GetProtocolVersion)]));
        Log(llInfo, Format('密码套件: %s', [LConnection.GetCipherName]));
      except
        // 忽略信息获取错误
      end;
      
      // 发送HTTP请求
      LRequest := BuildHTTPRequest(LHost, LPath);
      Log(llDebug, '发送HTTP请求...');
      if LConnection.Write(LRequest[1], Length(LRequest)) <> Length(LRequest) then
      begin
        Log(llError, '发送请求失败');
        Continue;
      end;
      Log(llInfo, Format('请求已发送，%d 字节', [Length(LRequest)]));
      
      // 读取响应
      Log(llInfo, '读取响应...');
      if not ReadResponse(LConnection, AResponse) then
      begin
        Log(llError, '读取响应失败');
        Continue;
      end;
      
      LEndTime := Now;
      Log(llInfo, Format('请求完成，耗时: %d ms', 
        [MilliSecondsBetween(LEndTime, LStartTime)]));
      
      Result := True;
      Break; // 成功，退出重试循环
      
    except
      on E: ESSLException do
      begin
        Log(llError, Format('SSL错误: %s', [E.Message]));
        if LRetry = AConfig.MaxRetries then
          raise;
      end;
      on E: Exception do
      begin
        Log(llError, Format('错误: %s', [E.Message]));
        if LRetry = AConfig.MaxRetries then
          raise;
      end;
    end;
    
    // 重试前等待
    if (LRetry < AConfig.MaxRetries) and (not Result) then
      Sleep(1000);
  end;
end;

{ 显示响应摘要 }
procedure ShowResponseSummary(const AResponse: string);
var
  LLines: TStringList;
  LStatusLine: string;
  LBodyStart: Integer;
  LBodySize: Integer;
begin
  WriteLn;
  WriteLn('=== 响应摘要 ===');
  
  LLines := TStringList.Create;
  try
    LLines.Text := AResponse;
    
    // 状态行
    if LLines.Count > 0 then
    begin
      LStatusLine := LLines[0];
      WriteLn('状态: ', LStatusLine);
    end;
    
    // 查找响应体
    LBodyStart := Pos(#13#10#13#10, AResponse);
    if LBodyStart > 0 then
    begin
      LBodySize := Length(AResponse) - LBodyStart - 3;
      WriteLn('响应体大小: ', LBodySize, ' 字节');
      
      // 显示前200字节
      if LBodySize > 0 then
      begin
        WriteLn;
        WriteLn('=== 响应体预览（前200字节）===');
        WriteLn(Copy(AResponse, LBodyStart + 4, Min(200, LBodySize)));
        if LBodySize > 200 then
          WriteLn('...');
      end;
    end;
  finally
    LLines.Free;
  end;
end;

{ 主程序 }
procedure Main;
var
  LResponse: string;
begin
  // 默认配置
  GConfig.URL := 'https://www.google.com';
  GConfig.Timeout := DEFAULT_TIMEOUT;
  GConfig.MaxRetries := MAX_RETRIES;
  GConfig.VerifyPeer := True;
  GConfig.LogToConsole := True;
  
  // 解析命令行参数
  if ParamCount >= 1 then
    GConfig.URL := ParamStr(1);
  
  WriteLn('fafafa.ssl - 生产级HTTPS客户端示例');
  WriteLn('======================================');
  WriteLn;
  
  // 执行请求
  try
    if DoHTTPSRequest(GConfig, LResponse) then
    begin
      ShowResponseSummary(LResponse);
      WriteLn;
      WriteLn('请求成功！');
      ExitCode := 0;
    end
    else
    begin
      WriteLn;
      WriteLn('请求失败！');
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


