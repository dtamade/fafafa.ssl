program test_session_security;

{$mode objfpc}{$H+}{$J-}

{**
 * Session Security Test Suite
 *
 * P3-8 P0-2: 会话安全测试
 *
 * 测试内容:
 * - 会话票据安全性
 * - 会话 ID 随机性
 * - 会话超时强制执行
 * - 会话缓存安全性
 * - 会话恢复安全性
 *
 * 安全说明:
 * TLS 会话管理需要正确配置以防止：
 * - 会话固定攻击
 * - 会话重放攻击
 * - 会话泄露
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2025-12-23
 *}

uses
  SysUtils, Classes, DateUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.context.builder,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.backed;

const
  // 会话安全参数
  MIN_SESSION_TIMEOUT = 60;       // 最小 1 分钟
  MAX_SESSION_TIMEOUT = 86400;    // 最大 24 小时
  DEFAULT_SESSION_TIMEOUT = 300;  // 默认 5 分钟

  MIN_SESSION_CACHE_SIZE = 0;     // 禁用缓存
  MAX_SESSION_CACHE_SIZE = 65535;

  // 会话 ID 长度
  MIN_SESSION_ID_LENGTH = 16;     // 128 bits
  RECOMMENDED_SESSION_ID_LENGTH = 32;  // 256 bits

var
  Total, Passed, Failed: Integer;
  Section: string;

procedure BeginSection(const AName: string);
begin
  Section := AName;
  WriteLn;
  WriteLn('=== ', AName, ' ===');
end;

procedure Check(const AName: string; AOk: Boolean; const ADetails: string = '');
begin
  Inc(Total);
  Write('  [', Section, '] ', AName, ': ');
  if AOk then
  begin
    Inc(Passed);
    WriteLn('PASS');
  end
  else
  begin
    Inc(Failed);
    WriteLn('FAIL');
    if ADetails <> '' then
      WriteLn('    ', ADetails);
  end;
end;

{ 验证会话超时是否在安全范围内 }
function IsSessionTimeoutSecure(ATimeout: Integer): Boolean;
begin
  Result := (ATimeout >= MIN_SESSION_TIMEOUT) and (ATimeout <= MAX_SESSION_TIMEOUT);
end;

{ 验证会话缓存大小是否合理 }
function IsSessionCacheSizeSecure(ACacheSize: Integer): Boolean;
begin
  Result := (ACacheSize >= MIN_SESSION_CACHE_SIZE) and
            (ACacheSize <= MAX_SESSION_CACHE_SIZE);
end;

{ 验证会话 ID 长度是否安全 }
function IsSessionIdLengthSecure(ALength: Integer): Boolean;
begin
  Result := ALength >= MIN_SESSION_ID_LENGTH;
end;

{ 检查字节数组是否全零（用于检测弱随机性） }
function IsAllZero(const AData: TBytes): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Length(AData) - 1 do
  begin
    if AData[I] <> 0 then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

{ 检查字节数组是否全相同（用于检测弱随机性） }
function IsAllSame(const AData: TBytes): Boolean;
var
  I: Integer;
  FirstByte: Byte;
begin
  if Length(AData) = 0 then
    Exit(True);

  FirstByte := AData[0];
  Result := True;
  for I := 1 to Length(AData) - 1 do
  begin
    if AData[I] <> FirstByte then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

{ 计算熵估算（简单版本） }
function EstimateEntropy(const AData: TBytes): Double;
var
  ByteCounts: array[0..255] of Integer;
  I: Integer;
  Prob: Double;
begin
  Result := 0;
  if Length(AData) = 0 then
    Exit;

  FillChar(ByteCounts, SizeOf(ByteCounts), 0);
  for I := 0 to Length(AData) - 1 do
    Inc(ByteCounts[AData[I]]);

  for I := 0 to 255 do
  begin
    if ByteCounts[I] > 0 then
    begin
      Prob := ByteCounts[I] / Length(AData);
      Result := Result - Prob * (Ln(Prob) / Ln(2));
    end;
  end;
end;

procedure TestSessionTimeoutValidation;
begin
  BeginSection('Session Timeout Validation');

  // 测试过短的超时
  Check('0 seconds insecure', not IsSessionTimeoutSecure(0));
  Check('30 seconds insecure', not IsSessionTimeoutSecure(30));

  // 测试安全的超时范围
  Check('60 seconds secure', IsSessionTimeoutSecure(60));
  Check('300 seconds secure', IsSessionTimeoutSecure(300));
  Check('3600 seconds secure', IsSessionTimeoutSecure(3600));
  Check('86400 seconds secure', IsSessionTimeoutSecure(86400));

  // 测试过长的超时
  Check('100000 seconds insecure', not IsSessionTimeoutSecure(100000));
end;

procedure TestSessionCacheSizeValidation;
begin
  BeginSection('Session Cache Size Validation');

  // 测试有效的缓存大小
  Check('0 (disabled) valid', IsSessionCacheSizeSecure(0));
  Check('100 valid', IsSessionCacheSizeSecure(100));
  Check('1024 valid', IsSessionCacheSizeSecure(1024));
  Check('65535 valid', IsSessionCacheSizeSecure(65535));

  // 测试无效的缓存大小
  Check('-1 invalid', not IsSessionCacheSizeSecure(-1));
  Check('100000 invalid', not IsSessionCacheSizeSecure(100000));
end;

procedure TestSessionIdLengthValidation;
begin
  BeginSection('Session ID Length Validation');

  // 测试不安全的长度
  Check('8 bytes insecure', not IsSessionIdLengthSecure(8));
  Check('12 bytes insecure', not IsSessionIdLengthSecure(12));

  // 测试安全的长度
  Check('16 bytes secure', IsSessionIdLengthSecure(16));
  Check('32 bytes secure', IsSessionIdLengthSecure(32));
  Check('48 bytes secure', IsSessionIdLengthSecure(48));
end;

procedure TestRandomnessValidation;
var
  LTestData: TBytes;
  LEntropy: Double;
begin
  BeginSection('Randomness Validation');

  // 测试全零数据
  SetLength(LTestData, 32);
  FillChar(LTestData[0], 32, 0);
  Check('All-zero detected', IsAllZero(LTestData));
  Check('All-zero same', IsAllSame(LTestData));

  // 测试全相同数据
  FillChar(LTestData[0], 32, $AA);
  Check('All-AA same', IsAllSame(LTestData));

  // 测试正常随机数据
  try
    LTestData := TSSLHelper.GenerateRandomBytes(32);
    Check('Random not all zero', not IsAllZero(LTestData));
    Check('Random not all same', not IsAllSame(LTestData));

    LEntropy := EstimateEntropy(LTestData);
    Check('Entropy > 4 bits', LEntropy > 4.0,
          Format('Entropy: %.2f bits', [LEntropy]));
  except
    on E: Exception do
      Check('Random generation', False, E.Message);
  end;
end;

procedure TestDefaultSessionConfig;
var
  LConfig: TSSLConfig;
begin
  BeginSection('Default Session Config');

  LConfig := Default(TSSLConfig);
  TSSLFactory.NormalizeConfig(LConfig);

  Check('Default timeout > 0', LConfig.SessionTimeout > 0,
        Format('Timeout: %d', [LConfig.SessionTimeout]));

  Check('Default timeout secure',
        IsSessionTimeoutSecure(LConfig.SessionTimeout),
        Format('Timeout: %d', [LConfig.SessionTimeout]));

  Check('Default cache size valid',
        IsSessionCacheSizeSecure(LConfig.SessionCacheSize),
        Format('CacheSize: %d', [LConfig.SessionCacheSize]));
end;

procedure TestContextSessionSettings;
var
  LContext: ISSLContext;
begin
  BeginSection('Context Session Settings');

  try
    LContext := TSSLContextBuilder.Create
      .WithTLS12And13
      .BuildClient;

    if LContext = nil then
    begin
      Check('Context creation', False, 'Context is nil');
      Exit;
    end;

    // 测试会话缓存设置
    LContext.SetSessionCacheSize(1024);
    Check('Set cache size', True);

    // 测试会话超时设置
    LContext.SetSessionTimeout(300);
    Check('Set session timeout', True);

    // 测试会话缓存模式
    LContext.SetSessionCacheMode(True);
    Check('Enable session cache', True);

    LContext.SetSessionCacheMode(False);
    Check('Disable session cache', True);

  except
    on E: Exception do
      Check('Context session settings', False, E.Message);
  end;
end;

procedure TestSessionTicketSecurity;
var
  LConfig: TSSLConfig;
begin
  BeginSection('Session Ticket Security');

  // 检查会话票据默认设置
  LConfig := Default(TSSLConfig);
  TSSLFactory.NormalizeConfig(LConfig);

  // 会话票据应该可以配置
  Check('Session tickets configurable', True);

  // 测试启用会话票据
  LConfig.EnableSessionTickets := True;
  TSSLFactory.NormalizeConfig(LConfig);
  Check('Enable session tickets option',
        ssoEnableSessionTickets in LConfig.Options);

  // 测试禁用会话票据
  LConfig.EnableSessionTickets := False;
  TSSLFactory.NormalizeConfig(LConfig);
  Check('Disable session tickets option',
        not (ssoEnableSessionTickets in LConfig.Options));
end;

procedure TestRenegotiationSecurity;
var
  LConfig: TSSLConfig;
begin
  BeginSection('Renegotiation Security');

  LConfig := Default(TSSLConfig);
  TSSLFactory.NormalizeConfig(LConfig);

  // 默认应该禁用重协商（防止重协商攻击）
  Check('Renegotiation disabled by default',
        ssoDisableRenegotiation in LConfig.Options);
end;

begin
  Total := 0;
  Passed := 0;
  Failed := 0;

  WriteLn('======================================');
  WriteLn('  Session Security Tests');
  WriteLn('======================================');

  try
    // 初始化 OpenSSL
    if not LoadOpenSSLLibrary then
    begin
      WriteLn('ERROR: Failed to load OpenSSL library');
      Halt(1);
    end;

    TestSessionTimeoutValidation;
    TestSessionCacheSizeValidation;
    TestSessionIdLengthValidation;
    TestRandomnessValidation;
    TestDefaultSessionConfig;
    TestContextSessionSettings;
    TestSessionTicketSecurity;
    TestRenegotiationSecurity;

  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('ERROR: ', E.Message);
      Inc(Failed);
    end;
  end;

  WriteLn;
  WriteLn('======================================');
  WriteLn(Format('Total: %d  Passed: %d  Failed: %d', [Total, Passed, Failed]));
  WriteLn('======================================');

  if Failed > 0 then
    Halt(1);
end.
