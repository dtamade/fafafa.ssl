program session_resumption_example;

{$mode objfpc}{$H+}

{**
 * TLS 会话复用示例
 *
 * 演示如何通过 `ISSLSessionResumption` owner path
 * 保存 / 注入 session candidate，并观察当前运行里是否真的命中恢复路径。
 *
 * 注意：
 * - 具体耗时和是否命中恢复路径取决于 backend、目标主机、网络路径、
 *   TLS 版本以及服务器是否真的发放并接受可恢复的 session / ticket。
 * - `SetSession(...)` 表示配置候选 session，不等于已经观测到 resumed handshake。
 *
 * 编译：
 *   fpc -Fusrc -Fusrc/openssl -o"examples/bin/session_resumption_example" \
 *       examples/session_resumption_example.pas
 *
 * 运行：
 *   ./examples/bin/session_resumption_example www.example.com 443 10
 *}

uses
  SysUtils, Classes,
  fafafa.ssl,
  fafafa.ssl.context.builder,
  fafafa.examples.tcp;

type
  { 连接统计 }
  TConnectionStats = record
    ConnectionNumber: Integer;
    StartTime: QWord;
    EndTime: QWord;
    Duration: Int64;
    SessionReused: Boolean;
  end;

var
  { 全局变量 }
  GHost: string;
  GPort: Word;
  GConnectionCount: Integer;
  GStats: array of TConnectionStats;
  GSavedSession: ISSLSession;  // 保存的会话，用于后续连接复用

{ ============================================================================ }
{ 辅助函数                                                                     }
{ ============================================================================ }

function GetTickCount64MS: QWord;
begin
  Result := GetTickCount64;
end;

procedure PrintUsage;
begin
  WriteLn('用法: ', ExtractFileName(ParamStr(0)), ' <host> <port> <connections>');
  WriteLn;
  WriteLn('参数:');
  WriteLn('  host         目标主机 (例如: www.example.com)');
  WriteLn('  port         目标端口 (例如: 443)');
  WriteLn('  connections  连接次数 (例如: 10)');
  WriteLn;
  WriteLn('示例:');
  WriteLn('  ', ExtractFileName(ParamStr(0)), ' www.example.com 443 10');
  WriteLn('  ', ExtractFileName(ParamStr(0)), ' localhost 44330 20');
end;

procedure ParseCommandLine;
begin
  if ParamCount < 3 then
  begin
    PrintUsage;
    Halt(1);
  end;

  GHost := ParamStr(1);
  GPort := StrToIntDef(ParamStr(2), 443);
  GConnectionCount := StrToIntDef(ParamStr(3), 10);

  if GConnectionCount <= 0 then
  begin
    WriteLn('错误: 连接次数必须大于 0');
    Halt(1);
  end;
end;

{ ============================================================================ }
{ 连接测试函数                                                                 }
{ ============================================================================ }

function TestConnection(AContext: ISSLContext; AConnectionNum: Integer): Boolean;
var
  Sock: TSocketHandle;
  Conn: ISSLConnection;
  Resumption: ISSLSessionResumption;
  TLS: TSSLStream;
  NetErr: string;
  StartTime, EndTime: QWord;
  StatIdx: Integer;
  HttpBuf: TBytes;
begin
  Result := False;
  Sock := INVALID_SOCKET;
  Conn := nil;
  TLS := nil;

  StatIdx := AConnectionNum - 1;
  GStats[StatIdx].ConnectionNumber := AConnectionNum;
  GStats[StatIdx].StartTime := GetTickCount64MS;

  try
    // 初始化网络
    if not InitNetwork(NetErr) then
    begin
      WriteLn('  [', AConnectionNum, '] 网络初始化失败: ', NetErr);
      Exit;
    end;

    // TCP 连接
    StartTime := GetTickCount64MS;
    Sock := ConnectTCP(GHost, GPort);
    if Sock = INVALID_SOCKET then
    begin
      WriteLn('  [', AConnectionNum, '] TCP 连接失败');
      Exit;
    end;

    // 创建 SSL 连接对象（但不立即握手）
    Conn := AContext.CreateConnection(THandle(Sock));
    if Conn = nil then
    begin
      WriteLn('  [', AConnectionNum, '] 创建 SSL 连接失败');
      Exit;
    end;

    // 如果有保存的会话，在握手前恢复它（第 2+ 次连接）
    // 关键：必须在握手之前调用 SetSession
    if (AConnectionNum > 1) and (GSavedSession <> nil) then
    begin
      WriteLn('  → 尝试恢复会话候选 (ID: ', Copy(GSavedSession.GetID, 1, 16), '...)');
      if Supports(Conn, ISSLSessionResumption, Resumption) then
      begin
        Resumption.SetSession(GSavedSession);
        WriteLn('  → ISSLSessionResumption.SetSession() 调用完成');
      end
      else
        WriteLn('  ⚠ 当前连接未暴露 ISSLSessionResumption，跳过 session candidate 注入');
    end
    else if (AConnectionNum > 1) and (GSavedSession = nil) then
    begin
      WriteLn('  ⚠ 警告: 第 ', AConnectionNum, ' 次连接，但没有保存的会话');
    end;

    // 设置 SNI（服务器名称指示）
    (Conn as ISSLClientConnection).SetServerName(GHost);

    // 执行 TLS 握手
    if not Conn.Connect then
    begin
      WriteLn('  [', AConnectionNum, '] TLS 握手失败');
      Exit;
    end;

    EndTime := GetTickCount64MS;

    // 包装为 TSSLStream
    TLS := TSSLStream.Create(Conn);

    if (TLS <> nil) and (TLS.Connection <> nil) then
    begin
      // 关键：TLS 1.3 会话票据在握手后异步发送
      // 需要发送 HTTP 请求并读取响应以触发票据接收
      if AConnectionNum = 1 then
      begin
        WriteLn('  → 发送 HTTP 请求以触发 TLS 1.3 票据接收...');
        try
          // 发送 HTTP 请求
          HttpBuf := TEncoding.UTF8.GetBytes('GET / HTTP/1.0'#13#10#13#10);
          TLS.Write(HttpBuf[0], Length(HttpBuf));
          // 读取一些数据以触发票据接收（不需要读取全部响应）
          SetLength(HttpBuf, 100);
          TLS.Read(HttpBuf[0], 100);
        except
          // 忽略读取错误，我们只是为了触发票据接收
        end;
      end;

      GStats[StatIdx].EndTime := GetTickCount64MS;
      GStats[StatIdx].Duration := GStats[StatIdx].EndTime - StartTime;

      // 通过 session owner path 读取当前握手是否真的命中恢复路径
      GStats[StatIdx].SessionReused := Supports(TLS.Connection, ISSLSessionResumption,
        Resumption) and Resumption.IsSessionReused;

      // 第一次连接成功后，保存会话供后续连接使用
      if (AConnectionNum = 1) and (GSavedSession = nil) then
      begin
        GSavedSession := nil;
        if Supports(Conn, ISSLSessionResumption, Resumption) then
          GSavedSession := Resumption.GetSession;
        if GSavedSession <> nil then
        begin
          WriteLn('  → 会话已保存');
          WriteLn('     会话 ID: ', GSavedSession.GetID);
          WriteLn('     协议版本: ', Ord(GSavedSession.GetProtocolVersion));
          WriteLn('     密码套件: ', GSavedSession.GetCipherName);
          WriteLn('     是否可复用: ', GSavedSession.IsResumable);
        end
        else
          WriteLn('  ⚠ 警告: ISSLSessionResumption.GetSession() 返回 nil');
      end;

      Result := True;

      Write('  [', AConnectionNum, '] 连接成功 - ', GStats[StatIdx].Duration, 'ms');
      if GStats[StatIdx].SessionReused then
        WriteLn(' (观测到恢复路径命中)')
      else if AConnectionNum = 1 then
        WriteLn(' (首次握手)')
      else
        WriteLn(' (未观测到恢复路径命中)');
    end
    else
    begin
      WriteLn('  [', AConnectionNum, '] TLS 握手失败');
    end;

  except
    on E: Exception do
    begin
      WriteLn('  [', AConnectionNum, '] 异常: ', E.Message);
    end;
  end;

  // 清理
  if TLS <> nil then
    TLS.Free;
  if Sock <> INVALID_SOCKET then
    CloseSocket(Sock);
end;

{ ============================================================================ }
{ 统计分析                                                                     }
{ ============================================================================ }

procedure PrintStatistics;
var
  I: Integer;
  TotalDuration: Int64;
  FirstHandshakeDuration: Int64;
  ResumedHandshakeCount: Integer;
  ResumedHandshakeTotalDuration: Int64;
  WarmMissCount: Integer;
  WarmMissTotalDuration: Int64;
  AvgResumedDuration: Double;
  AvgWarmMissDuration: Double;
  Improvement: Double;
begin
  WriteLn;
  WriteLn('================================================================');
  WriteLn('性能统计');
  WriteLn('================================================================');
  WriteLn;

  TotalDuration := 0;
  FirstHandshakeDuration := 0;
  ResumedHandshakeCount := 0;
  ResumedHandshakeTotalDuration := 0;
  WarmMissCount := 0;
  WarmMissTotalDuration := 0;

  for I := 0 to High(GStats) do
  begin
    if GStats[I].Duration > 0 then
    begin
      TotalDuration := TotalDuration + GStats[I].Duration;

      if I = 0 then
        FirstHandshakeDuration := GStats[I].Duration
      else if GStats[I].SessionReused then
      begin
        Inc(ResumedHandshakeCount);
        ResumedHandshakeTotalDuration := ResumedHandshakeTotalDuration + GStats[I].Duration;
      end
      else
      begin
        Inc(WarmMissCount);
        WarmMissTotalDuration := WarmMissTotalDuration + GStats[I].Duration;
      end;
    end;
  end;

  WriteLn('总连接数: ', Length(GStats));
  WriteLn('总耗时: ', TotalDuration, 'ms');
  WriteLn;

  if FirstHandshakeDuration > 0 then
  begin
    WriteLn('首次握手:');
    WriteLn('  耗时: ', FirstHandshakeDuration, 'ms');
    WriteLn;
  end;

  if ResumedHandshakeCount > 0 then
  begin
    AvgResumedDuration := ResumedHandshakeTotalDuration / ResumedHandshakeCount;
    WriteLn('观测到恢复路径命中 (', ResumedHandshakeCount, ' 次):');
    WriteLn('  平均耗时: ', AvgResumedDuration:0:2, 'ms');
    WriteLn('  总耗时: ', ResumedHandshakeTotalDuration, 'ms');
    WriteLn;

    if FirstHandshakeDuration > 0 then
    begin
      Improvement := FirstHandshakeDuration / AvgResumedDuration;
      WriteLn('性能提升:');
      WriteLn('  ', Improvement:0:1, 'x 倍');
      WriteLn('  节省时间: ', FirstHandshakeDuration - Round(AvgResumedDuration), 'ms');
    end;
  end;

  if WarmMissCount > 0 then
  begin
    AvgWarmMissDuration := WarmMissTotalDuration / WarmMissCount;
    WriteLn('未观测到恢复路径命中 (', WarmMissCount, ' 次):');
    WriteLn('  平均耗时: ', AvgWarmMissDuration:0:2, 'ms');
    WriteLn('  总耗时: ', WarmMissTotalDuration, 'ms');
    WriteLn;
  end;

  WriteLn;
  WriteLn('================================================================');
end;

{ ============================================================================ }
{ 主程序                                                                       }
{ ============================================================================ }

var
  Context: ISSLContext;
  I: Integer;
  SuccessCount: Integer;

begin
  WriteLn('================================================================');
  WriteLn('TLS 会话复用示例');
  WriteLn('================================================================');
  WriteLn;

  // 解析命令行参数
  ParseCommandLine;

  WriteLn('配置:');
  WriteLn('  目标: ', GHost, ':', GPort);
  WriteLn('  连接次数: ', GConnectionCount);
  WriteLn;

  // 初始化统计数组
  SetLength(GStats, GConnectionCount);

  // 创建支持会话复用的 SSL 上下文
  WriteLn('创建 SSL 上下文 (启用会话复用)...');

  // 对于 localhost，禁用证书验证（用于测试）
  if (GHost = 'localhost') or (GHost = '127.0.0.1') then
  begin
    WriteLn('检测到 localhost，禁用证书验证');

    // 根据端口选择 TLS 版本：44330=TLS 1.2, 44331=TLS 1.3
    if GPort = 44331 then
    begin
      WriteLn('使用 TLS 1.3（是否命中恢复路径仍取决于运行时 truth）');
      Context := TSSLContextBuilder.Create
        .WithTLS13                   // 仅使用 TLS 1.3
        .WithVerifyNone              // 禁用证书验证（仅用于 localhost 测试）
        .WithSessionCache(True)      // 启用会话缓存 ⭐
        .WithSessionTimeout(300)     // 会话超时 5 分钟 ⭐
        .BuildClient;
    end
    else
    begin
      WriteLn('使用 TLS 1.2（是否命中恢复路径仍取决于运行时 truth）');
      Context := TSSLContextBuilder.Create
        .WithTLS12                   // 仅使用 TLS 1.2
        .WithVerifyNone              // 禁用证书验证（仅用于 localhost 测试）
        .WithSessionCache(True)      // 启用会话缓存 ⭐
        .WithSessionTimeout(300)     // 会话超时 5 分钟 ⭐
        .BuildClient;
    end;
  end
  else
  begin
    WriteLn('使用 TLS 1.2+1.3（自动协商）');
    Context := TSSLContextBuilder.Create
      .WithTLS12And13              // 支持 TLS 1.2 和 1.3
      .WithVerifyPeer              // 验证服务器证书
      .WithSystemRoots             // 使用系统根证书
      .WithSessionCache(True)      // 启用会话缓存 ⭐
      .WithSessionTimeout(300)     // 会话超时 5 分钟 ⭐
      .BuildClient;
  end;

  WriteLn('SSL 上下文创建成功');
  WriteLn;

  // 执行多次连接测试
  WriteLn('开始连接测试...');
  WriteLn('================================================================');

  SuccessCount := 0;
  for I := 1 to GConnectionCount do
  begin
    if TestConnection(Context, I) then
      Inc(SuccessCount);

    // 短暂延迟，避免过快连接
    Sleep(100);
  end;

  WriteLn('================================================================');
  WriteLn;
  WriteLn('连接测试完成: ', SuccessCount, '/', GConnectionCount, ' 成功');

  // 打印统计信息
  if SuccessCount > 0 then
    PrintStatistics;

  WriteLn;
  WriteLn('提示: 首次连接建立 session candidate 后，后续连接可以尝试恢复。');
  WriteLn('      是否真的命中恢复路径，请以 IsSessionReused 的运行输出为准。');
end.
