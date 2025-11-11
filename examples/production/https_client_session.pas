program https_client_session;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{ 生产级HTTPS客户端 - 会话复用示例
  
  功能：
  - TLS会话复用
  - 性能对比测试
  - 会话持久化
  - 多次请求优化
  
  使用方法：
    ./https_client_session <URL> [request_count]
  
  示例：
    ./https_client_session https://www.google.com 10
}

uses
  SysUtils, Classes, DateUtils,
  fafafa.ssl, fafafa.ssl.base;

const
  BUFFER_SIZE = 16384;
  DEFAULT_REQUEST_COUNT = 5;

type
  TSessionStats = record
    TotalRequests: Integer;
    SuccessCount: Integer;
    FailCount: Integer;
    TotalTime: Int64;      // 毫秒
    AverageTime: Double;   // 毫秒
    SessionReuseCount: Integer;
  end;

var
  GURL: string;
  GRequestCount: Integer;
  GVerbose: Boolean;

procedure Log(const AMsg: string);
begin
  if GVerbose then
    WriteLn('[', FormatDateTime('hh:nn:ss.zzz', Now), '] ', AMsg);
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
            'User-Agent: fafafa.ssl-session/1.0' + #13#10 +
            'Accept: */*' + #13#10 +
            'Connection: close' + #13#10 +
            #13#10;
end;

function ReadResponse(AConnection: ISSLConnection): Boolean;
var
  LBuffer: array[0..BUFFER_SIZE-1] of Byte;
  LBytesRead: Integer;
  LTotalRead: Integer;
begin
  Result := False;
  LTotalRead := 0;
  
  repeat
    try
      LBytesRead := AConnection.Read(LBuffer[0], BUFFER_SIZE);
      if LBytesRead > 0 then
        Inc(LTotalRead, LBytesRead);
    except
      Break;
    end;
  until LBytesRead = 0;
  
  Result := LTotalRead > 0;
end;

{ 执行单次请求 }
function DoSingleRequest(AContext: ISSLContext; const AHost, APath: string;
  APort: Word; var ASessionReused: Boolean): Integer;
var
  LConnection: ISSLConnection;
  LRequest: RawByteString;
  LStartTime: TDateTime;
begin
  Result := -1;
  ASessionReused := False;
  
  try
    LStartTime := Now;
    
    // 创建连接
    LConnection := AContext.CreateConnection;
    if LConnection = nil then
      Exit;
    
    // 连接
    LConnection.Connect(AHost, APort);
    
    // 检查会话是否被复用
    try
      ASessionReused := LConnection.IsSessionReused;
    except
      // 某些实现可能不支持
    end;
    
    // 发送请求
    LRequest := BuildHTTPRequest(AHost, APath);
    if LConnection.Write(LRequest[1], Length(LRequest)) <> Length(LRequest) then
      Exit;
    
    // 读取响应
    if not ReadResponse(LConnection) then
      Exit;
    
    Result := MilliSecondsBetween(Now, LStartTime);
    
  except
    Result := -1;
  end;
end;

{ 测试：不使用会话复用 }
procedure TestWithoutSessionReuse(const AHost, APath: string; APort: Word;
  ACount: Integer; out AStats: TSessionStats);
var
  i: Integer;
  LContext: ISSLContext;
  LTime: Integer;
  LSessionReused: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试：不使用会话复用 ===');
  
  FillChar(AStats, SizeOf(AStats), 0);
  AStats.TotalRequests := ACount;
  
  for i := 1 to ACount do
  begin
    Write(Format('请求 %d/%d ... ', [i, ACount]));
    
    // 每次创建新的上下文
    LContext := TSSLFactory.CreateContext(sslOpenSSL, sslCtxClient);
    LContext.SetVerifyMode([sslVerifyNone]);
    
    LTime := DoSingleRequest(LContext, AHost, APath, APort, LSessionReused);
    
    if LTime >= 0 then
    begin
      Inc(AStats.SuccessCount);
      Inc(AStats.TotalTime, LTime);
      WriteLn(Format('成功 (%d ms)', [LTime]));
    end
    else
    begin
      Inc(AStats.FailCount);
      WriteLn('失败');
    end;
  end;
  
  if AStats.SuccessCount > 0 then
    AStats.AverageTime := AStats.TotalTime / AStats.SuccessCount;
end;

{ 测试：使用会话复用 }
procedure TestWithSessionReuse(const AHost, APath: string; APort: Word;
  ACount: Integer; out AStats: TSessionStats);
var
  i: Integer;
  LContext: ISSLContext;
  LTime: Integer;
  LSessionReused: Boolean;
begin
  WriteLn;
  WriteLn('=== 测试：使用会话复用 ===');
  
  FillChar(AStats, SizeOf(AStats), 0);
  AStats.TotalRequests := ACount;
  
  // 创建一个共享的上下文
  LContext := TSSLFactory.CreateContext(sslOpenSSL, sslCtxClient);
  LContext.SetVerifyMode([sslVerifyNone]);
  
  for i := 1 to ACount do
  begin
    Write(Format('请求 %d/%d ... ', [i, ACount]));
    
    LTime := DoSingleRequest(LContext, AHost, APath, APort, LSessionReused);
    
    if LTime >= 0 then
    begin
      Inc(AStats.SuccessCount);
      Inc(AStats.TotalTime, LTime);
      if LSessionReused then
      begin
        Inc(AStats.SessionReuseCount);
        WriteLn(Format('成功 (%d ms) [会话复用]', [LTime]));
      end
      else
        WriteLn(Format('成功 (%d ms) [新会话]', [LTime]));
    end
    else
    begin
      Inc(AStats.FailCount);
      WriteLn('失败');
    end;
  end;
  
  if AStats.SuccessCount > 0 then
    AStats.AverageTime := AStats.TotalTime / AStats.SuccessCount;
end;

procedure ShowStats(const ANoReuseStats, AReuseStats: TSessionStats);
var
  LSpeedup: Double;
begin
  WriteLn;
  WriteLn('=== 性能对比 ===');
  WriteLn;
  
  WriteLn('不使用会话复用:');
  WriteLn('  总请求: ', ANoReuseStats.TotalRequests);
  WriteLn('  成功: ', ANoReuseStats.SuccessCount);
  WriteLn('  失败: ', ANoReuseStats.FailCount);
  WriteLn('  总时间: ', ANoReuseStats.TotalTime, ' ms');
  WriteLn('  平均时间: ', Format('%.2f', [ANoReuseStats.AverageTime]), ' ms');
  WriteLn;
  
  WriteLn('使用会话复用:');
  WriteLn('  总请求: ', AReuseStats.TotalRequests);
  WriteLn('  成功: ', AReuseStats.SuccessCount);
  WriteLn('  失败: ', AReuseStats.FailCount);
  WriteLn('  会话复用次数: ', AReuseStats.SessionReuseCount);
  WriteLn('  总时间: ', AReuseStats.TotalTime, ' ms');
  WriteLn('  平均时间: ', Format('%.2f', [AReuseStats.AverageTime]), ' ms');
  WriteLn;
  
  if (ANoReuseStats.AverageTime > 0) and (AReuseStats.AverageTime > 0) then
  begin
    LSpeedup := ANoReuseStats.AverageTime / AReuseStats.AverageTime;
    WriteLn('性能提升:');
    WriteLn('  加速比: ', Format('%.2fx', [LSpeedup]));
    WriteLn('  时间节省: ', Format('%.2f%%', 
      [(1 - AReuseStats.AverageTime / ANoReuseStats.AverageTime) * 100]));
  end;
end;

procedure ShowUsage;
begin
  WriteLn('用法: https_client_session <URL> [request_count]');
  WriteLn;
  WriteLn('参数:');
  WriteLn('  URL            - 目标URL（必须是HTTPS）');
  WriteLn('  request_count  - 请求次数（默认5次）');
  WriteLn;
  WriteLn('示例:');
  WriteLn('  https_client_session https://www.google.com');
  WriteLn('  https_client_session https://www.google.com 10');
  WriteLn;
  WriteLn('说明:');
  WriteLn('  此示例对比使用和不使用会话复用时的性能差异。');
  WriteLn('  会话复用可以显著减少TLS握手时间。');
  WriteLn;
end;

procedure Main;
var
  LHost, LPath: string;
  LPort: Word;
  LNoReuseStats, LReuseStats: TSessionStats;
begin
  GVerbose := False; // 简化输出
  
  WriteLn('fafafa.ssl - 会话复用示例');
  WriteLn('===========================');
  
  // 解析参数
  if ParamCount < 1 then
  begin
    WriteLn;
    ShowUsage;
    ExitCode := 1;
    Exit;
  end;
  
  GURL := ParamStr(1);
  GRequestCount := DEFAULT_REQUEST_COUNT;
  
  if ParamCount >= 2 then
  begin
    try
      GRequestCount := StrToInt(ParamStr(2));
      if GRequestCount < 1 then
        GRequestCount := DEFAULT_REQUEST_COUNT;
    except
      WriteLn('警告: 无效的请求次数，使用默认值');
    end;
  end;
  
  WriteLn('URL: ', GURL);
  WriteLn('请求次数: ', GRequestCount);
  
  // 解析URL
  if not ParseURL(GURL, LHost, LPath, LPort) then
  begin
    WriteLn('错误: URL解析失败');
    ExitCode := 1;
    Exit;
  end;
  
  try
    // 测试不使用会话复用
    TestWithoutSessionReuse(LHost, LPath, LPort, GRequestCount, LNoReuseStats);
    
    // 短暂延迟
    Sleep(1000);
    
    // 测试使用会话复用
    TestWithSessionReuse(LHost, LPath, LPort, GRequestCount, LReuseStats);
    
    // 显示统计
    ShowStats(LNoReuseStats, LReuseStats);
    
    ExitCode := 0;
    
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('错误: ', E.Message);
      ExitCode := 2;
    end;
  end;
end;

begin
  Main;
end.


