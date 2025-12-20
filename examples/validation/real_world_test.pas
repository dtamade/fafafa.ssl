program real_world_test;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{ fafafa.ssl 真实网站连接测试
  
  功能：
  - 连接真实HTTPS网站
  - 测试TLS握手
  - 验证证书
  - 测试数据传输
  - 生成详细报告
  
  使用方法：
    ./real_world_test [sites_file]
  
  示例：
    ./real_world_test
    ./real_world_test custom_sites.txt
}

uses
  SysUtils, Classes, DateUtils, Math, StrUtils,
  fafafa.ssl, fafafa.ssl.base;

const
  DEFAULT_SITES_FILE = 'test_sites.txt';
  BUFFER_SIZE = 8192;
  CONNECT_TIMEOUT = 10000;  // 10秒

type
  { 测试结果 }
  TTestResult = record
    URL: string;
    Description: string;
    Success: Boolean;
    ConnectTime: Integer;  // 毫秒
    TLSVersion: string;
    CipherSuite: string;
    CertValid: Boolean;
    DataReceived: Integer;
    ErrorMsg: string;
  end;

  { 测试统计 }
  TTestStats = record
    TotalTests: Integer;
    SuccessCount: Integer;
    FailCount: Integer;
    TotalTime: Int64;
    AverageTime: Double;
  end;

var
  GResults: array of TTestResult;
  GStats: TTestStats;

procedure Log(const AMsg: string);
begin
  WriteLn('[', FormatDateTime('hh:nn:ss', Now), '] ', AMsg);
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

function TestSingleSite(const AURL, ADescription: string): TTestResult;
var
  LContext: ISSLContext;
  LConnection: ISSLConnection;
  LHost, LPath: string;
  LPort: Word;
  LRequest: RawByteString;
  LBuffer: array[0..BUFFER_SIZE-1] of Byte;
  LBytesRead: Integer;
  LStartTime: TDateTime;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.URL := AURL;
  Result.Description := ADescription;
  Result.Success := False;
  
  Write('测试 ', AURL, ' ... ');
  
  try
    // 解析URL
    if not ParseURL(AURL, LHost, LPath, LPort) then
    begin
      Result.ErrorMsg := 'URL解析失败';
      WriteLn('失败 (URL解析失败)');
      Exit;
    end;
    
    LStartTime := Now;
    
    // 创建SSL上下文
    LContext := TSSLFactory.CreateContext(sslOpenSSL, sslCtxClient);
    if LContext = nil then
    begin
      Result.ErrorMsg := '创建SSL上下文失败';
      WriteLn('失败 (上下文创建失败)');
      Exit;
    end;
    
    // 配置证书验证
    LContext.SetVerifyMode([sslVerifyPeer]);
    
    // 创建连接
    LConnection := LContext.CreateConnection;
    if LConnection = nil then
    begin
      Result.ErrorMsg := '创建连接失败';
      WriteLn('失败 (连接创建失败)');
      Exit;
    end;
    
    // 连接到服务器
    LConnection.Connect(LHost, LPort);
    
    // 记录连接时间
    Result.ConnectTime := MilliSecondsBetween(Now, LStartTime);
    
    // 获取TLS信息
    try
      Result.TLSVersion := ProtocolVersionToString(LConnection.GetProtocolVersion);
      Result.CipherSuite := LConnection.GetCipherName;
      Result.CertValid := (LConnection.GetVerifyResult = 0);
    except
      Result.TLSVersion := '未知';
      Result.CipherSuite := '未知';
      Result.CertValid := False;
    end;
    
    // 发送简单的HTTP请求
    LRequest := 'GET ' + LPath + ' HTTP/1.1' + #13#10 +
                'Host: ' + LHost + #13#10 +
                'User-Agent: fafafa.ssl-test/1.0' + #13#10 +
                'Accept: */*' + #13#10 +
                'Connection: close' + #13#10 +
                #13#10;
    
    if LConnection.Write(LRequest[1], Length(LRequest)) <> Length(LRequest) then
    begin
      Result.ErrorMsg := '发送请求失败';
      WriteLn('失败 (发送请求失败)');
      Exit;
    end;
    
    // 读取响应
    Result.DataReceived := 0;
    repeat
      try
        LBytesRead := LConnection.Read(LBuffer[0], BUFFER_SIZE);
        if LBytesRead > 0 then
          Inc(Result.DataReceived, LBytesRead);
      except
        Break;
      end;
    until LBytesRead = 0;
    
    if Result.DataReceived > 0 then
    begin
      Result.Success := True;
      WriteLn(Format('成功 (%s, %s, %d ms)', 
        [Result.TLSVersion, Result.CipherSuite, Result.ConnectTime]));
    end
    else
    begin
      Result.ErrorMsg := '未收到数据';
      WriteLn('失败 (未收到数据)');
    end;
    
  except
    on E: ESSLException do
    begin
      Result.ErrorMsg := 'SSL错误: ' + E.Message;
      WriteLn('失败 (', Result.ErrorMsg, ')');
    end;
    on E: Exception do
    begin
      Result.ErrorMsg := '错误: ' + E.Message;
      WriteLn('失败 (', Result.ErrorMsg, ')');
    end;
  end;
end;

function LoadTestSites(const AFileName: string; out ASites: TStringList): Boolean;
var
  LFile: TextFile;
  LLine: string;
begin
  Result := False;
  ASites := TStringList.Create;
  
  if not FileExists(AFileName) then
  begin
    WriteLn('错误: 测试网站列表文件不存在: ', AFileName);
    Exit;
  end;
  
  try
    AssignFile(LFile, AFileName);
    Reset(LFile);
    try
      while not Eof(LFile) do
      begin
        ReadLn(LFile, LLine);
        LLine := Trim(LLine);
        // 跳过空行和注释
        if (LLine <> '') and (LLine[1] <> '#') then
          ASites.Add(LLine);
      end;
    finally
      CloseFile(LFile);
    end;
    Result := ASites.Count > 0;
  except
    on E: Exception do
    begin
      WriteLn('错误: 读取文件失败: ', E.Message);
      ASites.Free;
      ASites := nil;
    end;
  end;
end;

procedure RunTests(const ASitesFile: string);
var
  LSites: TStringList;
  i: Integer;
  LLine, LURL, LDesc: string;
  LPos: Integer;
  LStartTime: TDateTime;
begin
  WriteLn('加载测试网站列表: ', ASitesFile);
  
  if not LoadTestSites(ASitesFile, LSites) then
  begin
    WriteLn('无法加载测试网站');
    Exit;
  end;
  
  try
    WriteLn(Format('找到 %d 个测试网站', [LSites.Count]));
    WriteLn;
    
    // 初始化统计
    FillChar(GStats, SizeOf(GStats), 0);
    GStats.TotalTests := LSites.Count;
    SetLength(GResults, LSites.Count);
    
    LStartTime := Now;
    
    // 执行测试
    for i := 0 to LSites.Count - 1 do
    begin
      LLine := LSites[i];
      
      // 解析URL和描述
      LPos := Pos(' ', LLine);
      if LPos > 0 then
      begin
        LURL := Copy(LLine, 1, LPos - 1);
        LDesc := Trim(Copy(LLine, LPos + 1, Length(LLine)));
      end
      else
      begin
        LURL := LLine;
        LDesc := '';
      end;
      
      // 执行测试
      GResults[i] := TestSingleSite(LURL, LDesc);
      
      // 更新统计
      if GResults[i].Success then
      begin
        Inc(GStats.SuccessCount);
        Inc(GStats.TotalTime, GResults[i].ConnectTime);
      end
      else
        Inc(GStats.FailCount);
    end;
    
    // 计算平均时间
    if GStats.SuccessCount > 0 then
      GStats.AverageTime := GStats.TotalTime / GStats.SuccessCount;
    
  finally
    LSites.Free;
  end;
end;

procedure GenerateReport;
var
  i: Integer;
  LSuccessRate: Double;
begin
  WriteLn;
  WriteLn('='.PadRight(60, '='));
  WriteLn('测试报告 - ', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
  WriteLn('='.PadRight(60, '='));
  WriteLn;
  
  // 统计摘要
  WriteLn('统计摘要:');
  WriteLn('  总测试数: ', GStats.TotalTests);
  WriteLn('  成功: ', GStats.SuccessCount);
  WriteLn('  失败: ', GStats.FailCount);
  
  if GStats.TotalTests > 0 then
  begin
    LSuccessRate := (GStats.SuccessCount / GStats.TotalTests) * 100;
    WriteLn('  成功率: ', Format('%.2f%%', [LSuccessRate]));
  end;
  
  if GStats.SuccessCount > 0 then
    WriteLn('  平均连接时间: ', Format('%.2f ms', [GStats.AverageTime]));
  
  WriteLn;
  WriteLn('详细结果:');
  WriteLn('-'.PadRight(60, '-'));
  
  for i := 0 to Length(GResults) - 1 do
  begin
    if GResults[i].Success then
    begin
      WriteLn(Format('[✓] %s', [GResults[i].URL]));
      if GResults[i].Description <> '' then
        WriteLn(Format('    描述: %s', [GResults[i].Description]));
      WriteLn(Format('    TLS: %s, 密码套件: %s', 
        [GResults[i].TLSVersion, GResults[i].CipherSuite]));
      WriteLn(Format('    连接时间: %d ms, 数据: %d 字节', 
        [GResults[i].ConnectTime, GResults[i].DataReceived]));
      WriteLn(Format('    证书验证: %s', 
        [IfThen(GResults[i].CertValid, '通过', '失败')]));
    end
    else
    begin
      WriteLn(Format('[✗] %s', [GResults[i].URL]));
      if GResults[i].Description <> '' then
        WriteLn(Format('    描述: %s', [GResults[i].Description]));
      WriteLn(Format('    错误: %s', [GResults[i].ErrorMsg]));
    end;
    WriteLn;
  end;
  
  WriteLn('='.PadRight(60, '='));
  
  // 评估
  WriteLn;
  WriteLn('评估结果:');
  if GStats.SuccessCount = GStats.TotalTests then
    WriteLn('  ✓ 优秀！所有测试通过')
  else if GStats.SuccessCount >= GStats.TotalTests * 0.9 then
    WriteLn('  ✓ 良好！大部分测试通过')
  else if GStats.SuccessCount >= GStats.TotalTests * 0.7 then
    WriteLn('  ⚠ 一般，部分测试失败')
  else
    WriteLn('  ✗ 需要改进，多个测试失败');
end;

procedure Main;
var
  LSitesFile: string;
begin
  WriteLn('fafafa.ssl - 真实网站连接测试');
  WriteLn('=================================');
  WriteLn;
  
  // 解析参数
  if ParamCount >= 1 then
    LSitesFile := ParamStr(1)
  else
    LSitesFile := DEFAULT_SITES_FILE;
  
  try
    RunTests(LSitesFile);
    GenerateReport;
    
    // 返回码基于成功率
    if GStats.SuccessCount >= GStats.TotalTests * 0.95 then
      ExitCode := 0  // >= 95%成功
    else if GStats.SuccessCount >= GStats.TotalTests * 0.8 then
      ExitCode := 1  // 80-95%成功
    else
      ExitCode := 2; // < 80%成功
      
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('致命错误: ', E.Message);
      ExitCode := 3;
    end;
  end;
end;

begin
  Main;
end.


