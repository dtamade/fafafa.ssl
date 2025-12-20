program https_client_session;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{ HTTPS 会话复用示例
  对比冷启动请求和复用会话的耗时。 }

uses
  SysUtils, DateUtils,
  fafafa.ssl, fafafa.ssl.base;

const
  DEFAULT_URL = 'https://www.google.com/';
  DEFAULT_REQUESTS = 5;
  BUFFER_SIZE = 16384;

function ParseURL(const AURL: string; out AHost, APath: string; out APort: Word): Boolean;
var
  LTemp: string;
  LPos: Integer;
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
  Result := AHost <> '';
end;

function BuildRequest(const AHost, APath: string): RawByteString;
begin
  Result := 'GET ' + APath + ' HTTP/1.1' + LineEnding +
            'Host: ' + AHost + LineEnding +
            'User-Agent: fafafa.ssl-session-demo/1.0' + LineEnding +
            'Connection: close' + LineEnding +
            LineEnding;
end;

function ReadResponse(AConnection: ISSLConnection): Integer;
var
  LBuffer: array[0..BUFFER_SIZE-1] of Byte;
  LBytes: Integer;
begin
  Result := 0;
  repeat
    try
      LBytes := AConnection.Read(LBuffer[0], BUFFER_SIZE);
    except
      on E: Exception do
      begin
        if Result = 0 then
          raise
        else
          Break;
      end;
    end;
    if LBytes > 0 then
      Inc(Result, LBytes);
  until LBytes = 0;
end;

type
  TRequestResult = record
    DurationMs: Integer;
    SessionReused: Boolean;
  end;

function ExecuteRequest(AContext: ISSLContext; const AHost, APath: string;
  APort: Word; AReuseSession: Boolean; var ACachedSession: ISSLSession): TRequestResult;
var
  LConnection: ISSLConnection;
  LRequest: RawByteString;
  LSession: ISSLSession;
  LStart: TDateTime;
begin
  Result.DurationMs := -1;
  Result.SessionReused := False;
  try
    LConnection := AContext.CreateConnection;
    if LConnection = nil then
      raise Exception.Create('无法创建 SSL 连接');
    if (AReuseSession) and (ACachedSession <> nil) then
    begin
      try
        LConnection.SetSession(ACachedSession);
      except
        on E: Exception do
          WriteLn('  ⚠ 无法设置会话: ', E.Message);
      end;
    end;
    LStart := Now;
    if not LConnection.Connect(AHost, APort) then
      raise Exception.Create('连接失败');
    LRequest := BuildRequest(AHost, APath);
    if LConnection.Write(LRequest[1], Length(LRequest)) <> Length(LRequest) then
      raise Exception.Create('发送请求失败');
    if ReadResponse(LConnection) = 0 then
      raise Exception.Create('未收到任何数据');
    Result.DurationMs := MilliSecondsBetween(Now, LStart);
    try
      Result.SessionReused := LConnection.IsSessionReused;
    except
      Result.SessionReused := False;
    end;
    if ACachedSession = nil then
    begin
      LSession := LConnection.GetSession;
      if LSession <> nil then
        ACachedSession := LSession;
    end;
    LConnection.Shutdown;
  except
    on E: Exception do
      WriteLn('  ✗ 请求失败: ', E.Message);
  end;
end;

procedure RunColdRequests(const AHost, APath: string; APort: Word; ACount: Integer);
var
  i: Integer;
  LContext: ISSLContext;
  LResult: TRequestResult;
  LTotal: Int64;
  LDummySession: ISSLSession;
  LSuccess: Integer;
begin
  WriteLn('--- 冷启动 (每次新建上下文) ---');
  LTotal := 0;
  LSuccess := 0;
  LDummySession := nil;
  for i := 1 to ACount do
  begin
    LContext := TSSLFactory.CreateContext(sslOpenSSL, sslCtxClient);
    if LContext = nil then
    begin
      WriteLn('  ✗ 无法创建 SSL 上下文');
      Continue;
    end;
    LContext.SetVerifyMode([sslVerifyNone]);
    LResult := ExecuteRequest(LContext, AHost, APath, APort, False, LDummySession);
    if LResult.DurationMs >= 0 then
    begin
      Inc(LTotal, LResult.DurationMs);
      Inc(LSuccess);
      WriteLn(Format('  #%d 用时 %d ms', [i, LResult.DurationMs]));
    end;
  end;
  if LSuccess > 0 then
    WriteLn(Format('平均耗时: %.2f ms (成功 %d/%d)', [LTotal / LSuccess, LSuccess, ACount]))
  else
    WriteLn('没有成功的请求，无法计算平均耗时。');
  WriteLn;
end;

procedure RunWarmRequests(const AHost, APath: string; APort: Word; ACount: Integer);
var
  i: Integer;
  LContext: ISSLContext;
  LResult: TRequestResult;
  LTotal: Int64;
  LSession: ISSLSession;
  LReuseCount: Integer;
  LSuccess: Integer;
begin
  WriteLn('--- 会话复用 (共享上下文) ---');
  LContext := TSSLFactory.CreateContext(sslOpenSSL, sslCtxClient);
  if LContext = nil then
  begin
    WriteLn('  ✗ 无法创建 SSL 上下文');
    Exit;
  end;
  LContext.SetVerifyMode([sslVerifyNone]);
  LContext.SetSessionCacheMode(True);
  LContext.SetSessionCacheSize(256);
  LContext.SetSessionTimeout(600);
  LTotal := 0;
  LReuseCount := 0;
  LSuccess := 0;
  LSession := nil;
  for i := 1 to ACount do
  begin
    LResult := ExecuteRequest(LContext, AHost, APath, APort, True, LSession);
    if LResult.DurationMs >= 0 then
    begin
      Inc(LTotal, LResult.DurationMs);
      Inc(LSuccess);
      if LResult.SessionReused then
        Inc(LReuseCount);
      WriteLn(Format('  #%d 用时 %d ms (会话复用: %s)',
        [i, LResult.DurationMs, BoolToStr(LResult.SessionReused, True)]));
    end;
  end;
  if LSuccess > 0 then
    WriteLn(Format('平均耗时: %.2f ms (成功 %d/%d)，复用次数: %d',
      [LTotal / LSuccess, LSuccess, ACount, LReuseCount]))
  else
    WriteLn('没有成功的请求，无法计算平均耗时。');
  WriteLn;
end;

var
  LURL, LHost, LPath: string;
  LPort: Word;
  LCount: Integer;
begin
  WriteLn('====================================================');
  WriteLn(' fafafa.ssl - HTTPS 客户端示例 #4 (会话复用)');
  WriteLn('====================================================');
  if ParamCount >= 1 then
    LURL := ParamStr(1)
  else
    LURL := DEFAULT_URL;
  if ParamCount >= 2 then
    LCount := StrToIntDef(ParamStr(2), DEFAULT_REQUESTS)
  else
    LCount := DEFAULT_REQUESTS;
  if not ParseURL(LURL, LHost, LPath, LPort) then
  begin
    WriteLn('无法解析 URL: ', LURL);
    Halt(1);
  end;
  WriteLn('目标: ', LHost, ':', LPort, LPath);
  WriteLn('请求次数: ', LCount);
  WriteLn('提示: 默认关闭证书验证，若用于生产请加载 CA 并启用校验。');
  WriteLn;
  RunColdRequests(LHost, LPath, LPort, LCount);
  RunWarmRequests(LHost, LPath, LPort, LCount);
end.
