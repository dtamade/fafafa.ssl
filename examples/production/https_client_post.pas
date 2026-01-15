program https_client_post;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{ 生产级HTTPS客户端 - POST请求示例
  
  功能：
  - POST数据提交
  - JSON/表单数据支持
  - 完整的错误处理
  - 请求/响应日志
  
  使用方法：
    ./https_client_post <URL> <data> [content-type]
  
  示例：
    ./https_client_post https://httpbin.org/post '{"name":"test"}' application/json
    ./https_client_post https://httpbin.org/post 'key1=value1&key2=value2' application/x-www-form-urlencoded
}

uses
  SysUtils, Classes, DateUtils,
  fafafa.ssl, fafafa.ssl.base,
  fafafa.examples.tcp;

const
  DEFAULT_TIMEOUT = 30000;
  BUFFER_SIZE = 16384;

type
  TContentType = (ctJSON, ctFormURLEncoded, ctPlainText);

var
  GURL, GData, GContentType: string;
  GLogEnabled: Boolean;

procedure Log(const AMsg: string);
begin
  if GLogEnabled then
    WriteLn('[', FormatDateTime('hh:nn:ss', Now), '] ', AMsg);
end;

function ContentTypeToString(AType: TContentType): string;
begin
  case AType of
    ctJSON: Result := 'application/json';
    ctFormURLEncoded: Result := 'application/x-www-form-urlencoded';
    ctPlainText: Result := 'text/plain';
  end;
end;

function DetectContentType(const AData: string): TContentType;
begin
  if (Length(AData) > 0) and (AData[1] = '{') then
    Result := ctJSON
  else if Pos('=', AData) > 0 then
    Result := ctFormURLEncoded
  else
    Result := ctPlainText;
end;

function ParseURL(const AURL: string; out AHost, APath: string; out APort: Word): Boolean;
var
  LPos, LPortPos: Integer;
  LTemp: string;
begin
  Result := False;
  APort := 443;

  LTemp := AURL;
  if Pos('https://', LowerCase(LTemp)) = 1 then
    Delete(LTemp, 1, 8)
  else if Pos('http://', LowerCase(LTemp)) = 1 then
  begin
    Delete(LTemp, 1, 7);
    APort := 80;
  end;

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

  LPortPos := Pos(':', AHost);
  if LPortPos > 0 then
  begin
    APort := StrToIntDef(Copy(AHost, LPortPos + 1, Length(AHost)), APort);
    AHost := Copy(AHost, 1, LPortPos - 1);
  end;

  Result := (AHost <> '');
end;

function BuildPOSTRequest(const AHost, APath, AData, AContentType: string): RawByteString;
var
  LContentLength: string;
begin
  LContentLength := IntToStr(Length(AData));
  
  Result := 'POST ' + APath + ' HTTP/1.1' + #13#10 +
            'Host: ' + AHost + #13#10 +
            'User-Agent: fafafa.ssl/1.0' + #13#10 +
            'Accept: */*' + #13#10 +
            'Content-Type: ' + AContentType + #13#10 +
            'Content-Length: ' + LContentLength + #13#10 +
            'Connection: close' + #13#10 +
            #13#10 +
            AData;
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

function DoPOSTRequest(const AURL, AData, AContentType: string; 
  out AResponse: string): Boolean;
var
  LContext: ISSLContext;
  LConnection: ISSLConnection;
  LClientConn: ISSLClientConnection;
  LSocket: TSocketHandle;
  LHost, LPath: string;
  LPort: Word;
  LRequest: RawByteString;
  LStartTime: TDateTime;
begin
  Result := False;
  
  Log('解析URL: ' + AURL);
  if not ParseURL(AURL, LHost, LPath, LPort) then
  begin
    WriteLn('错误: URL解析失败');
    Exit;
  end;
  
  try
    LStartTime := Now;
    
    Log('创建SSL上下文...');
    LContext := TSSLFactory.CreateContext(sslCtxClient, sslOpenSSL);
    if LContext = nil then
      raise Exception.Create('创建SSL上下文失败');

    // 简化示例：默认关闭证书验证（生产环境建议启用并加载系统根证书）
    LContext.SetVerifyMode([sslVerifyNone]);

    Log(Format('连接到 %s:%d...', [LHost, LPort]));
    LSocket := INVALID_SOCKET;
    LConnection := nil;
    try
      LSocket := ConnectTCP(LHost, LPort);

      Log('创建连接...');
      LConnection := LContext.CreateConnection(THandle(LSocket));
      if LConnection = nil then
        raise Exception.Create('创建连接失败');

      LConnection.SetTimeout(DEFAULT_TIMEOUT);

      // per-connection SNI/hostname
      if Supports(LConnection, ISSLClientConnection, LClientConn) then
        LClientConn.SetServerName(LHost);

      if not LConnection.Connect then
        raise Exception.Create('TLS 握手失败');

      Log('连接成功');
    
    // 构建并发送POST请求
    LRequest := BuildPOSTRequest(LHost, LPath, AData, AContentType);
    Log(Format('发送POST请求 (%d 字节)...', [Length(LRequest)]));
    
    if GLogEnabled then
    begin
      WriteLn;
      WriteLn('=== 请求内容 ===');
      WriteLn(LRequest);
      WriteLn('================');
      WriteLn;
    end;
    
    if LConnection.Write(LRequest[1], Length(LRequest)) <> Length(LRequest) then
    begin
      WriteLn('错误: 发送请求失败');
      Exit;
    end;
    
    Log('读取响应...');
    AResponse := ReadResponse(LConnection);

    Log(Format('请求完成 (耗时 %d ms)', 
      [MilliSecondsBetween(Now, LStartTime)]));

    Result := True;
  finally
    if LConnection <> nil then
    begin
      try
        LConnection.Shutdown;
      except
        // ignore
      end;
    end;
    LConnection := nil;
    CloseSocket(LSocket);
  end;

  except
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
  LBodyStart: Integer;
  i: Integer;
begin
  WriteLn;
  WriteLn('=== 响应 ===');
  
  LLines := TStringList.Create;
  try
    LLines.Text := AResponse;
    
    // 显示状态行和头部
    i := 0;
    while (i < LLines.Count) and (Trim(LLines[i]) <> '') do
    begin
      WriteLn(LLines[i]);
      Inc(i);
    end;
    
    // 查找并显示响应体
    LBodyStart := Pos(#13#10#13#10, AResponse);
    if LBodyStart > 0 then
    begin
      WriteLn;
      WriteLn('=== 响应体 ===');
      WriteLn(Copy(AResponse, LBodyStart + 4, Length(AResponse)));
    end;
  finally
    LLines.Free;
  end;
end;

procedure ShowUsage;
begin
  WriteLn('用法: https_client_post <URL> <data> [content-type]');
  WriteLn;
  WriteLn('参数:');
  WriteLn('  URL           - 目标URL（必须是HTTPS）');
  WriteLn('  data          - POST数据');
  WriteLn('  content-type  - 内容类型（可选，自动检测）');
  WriteLn;
  WriteLn('示例:');
  WriteLn('  https_client_post https://httpbin.org/post ''{"name":"test"}''');
  WriteLn('  https_client_post https://httpbin.org/post ''key=value'' application/x-www-form-urlencoded');
  WriteLn;
end;

procedure Main;
var
  LResponse: string;
  LContentType: string;
  NetErr: string;
begin
  GLogEnabled := True;
  
  WriteLn('fafafa.ssl - POST请求示例');
  WriteLn('===========================');
  WriteLn;
  
  // 解析参数
  if ParamCount < 2 then
  begin
    ShowUsage;
    ExitCode := 1;
    Exit;
  end;
  
  GURL := ParamStr(1);
  GData := ParamStr(2);
  
  if ParamCount >= 3 then
    LContentType := ParamStr(3)
  else
    LContentType := ContentTypeToString(DetectContentType(GData));
  
  WriteLn('URL: ', GURL);
  WriteLn('数据长度: ', Length(GData), ' 字节');
  WriteLn('内容类型: ', LContentType);
  WriteLn;
  
  if not InitNetwork(NetErr) then
  begin
    WriteLn('网络初始化失败: ', NetErr);
    ExitCode := 2;
    Exit;
  end;

  try
    if DoPOSTRequest(GURL, GData, LContentType, LResponse) then
    begin
      ShowResponse(LResponse);
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

  CleanupNetwork;
end;

begin
  Main;
end.


