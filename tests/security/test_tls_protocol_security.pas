program test_tls_protocol_security;

{$mode objfpc}{$H+}{$J-}

{**
 * TLS Protocol Security Test Suite
 *
 * P0-3: 安全测试扩展 - TLS 协议安全
 *
 * 测试内容:
 * - 协议版本限制（禁用 SSLv2/SSLv3/TLS 1.0/1.1）
 * - 密码套件强度验证
 * - 安全默认配置
 * - 降级攻击防护
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2025-12-23
 *}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.openssl.backed;

var
  Total, Passed, Failed: Integer;
  Section: string;

procedure BeginSection(const aName: string);
begin
  Section := aName;
  WriteLn;
  WriteLn('=== ', aName, ' ===');
end;

procedure Check(const aName: string; ok: Boolean; const details: string = '');
begin
  Inc(Total);
  Write('  [', Section, '] ', aName, ': ');
  if ok then
  begin
    Inc(Passed);
    WriteLn('PASS');
  end
  else
  begin
    Inc(Failed);
    WriteLn('FAIL');
    if details <> '' then
      WriteLn('    ', details);
  end;
end;

{ 配置安全默认值（通过接口方法） }
procedure ConfigureSecureDefaults(Ctx: ISSLContext);
begin
  // 仅 TLS 1.2 和 1.3
  Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);

  // 安全选项
  Ctx.SetOptions([
    ssoEnableSessionCache,
    ssoEnableSessionTickets,
    ssoDisableCompression,
    ssoDisableRenegotiation,
    ssoNoSSLv2,
    ssoNoSSLv3,
    ssoNoTLSv1,
    ssoNoTLSv1_1,
    ssoCipherServerPreference,
    ssoSingleECDHUse
  ]);

  // 强密码套件
  Ctx.SetCipherList('ECDHE+AESGCM:ECDHE+CHACHA20:ECDHE+AES256:DHE+AESGCM:DHE+AES256:!ANULL:!MD5:!DSS:!RC4:!3DES');
  Ctx.SetCipherSuites('TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256:TLS_AES_128_GCM_SHA256');

  // 启用验证
  Ctx.SetVerifyMode([sslVerifyPeer]);
  Ctx.SetVerifyDepth(4);

  // 会话配置
  Ctx.SetSessionCacheMode(True);
  Ctx.SetSessionTimeout(3600);
end;

{ 测试安全默认值配置 }
procedure TestSecureDefaults;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  Options: TSSLOptions;
  Versions: TSSLProtocolVersions;
begin
  BeginSection('安全默认配置');

  try
    Lib := TOpenSSLLibrary.Create;
    if not Lib.Initialize then
    begin
      Check('初始化 OpenSSL', False);
      Exit;
    end;
    Check('初始化 OpenSSL', True);

    Ctx := Lib.CreateContext(sslCtxClient);
    Check('创建上下文', Ctx <> nil);

    if Ctx = nil then Exit;

    // 应用安全默认配置
    ConfigureSecureDefaults(Ctx);
    Check('配置安全默认值', True);

    // 验证协议版本
    Versions := Ctx.GetProtocolVersions;
    Check('TLS 1.2 已启用', sslProtocolTLS12 in Versions);
    Check('TLS 1.3 已启用', sslProtocolTLS13 in Versions);
    Check('TLS 1.0 已禁用', not (sslProtocolTLS10 in Versions));
    Check('TLS 1.1 已禁用', not (sslProtocolTLS11 in Versions));

    // 验证安全选项
    Options := Ctx.GetOptions;
    Check('SSLv2 已禁用', ssoNoSSLv2 in Options);
    Check('SSLv3 已禁用', ssoNoSSLv3 in Options);
    Check('TLS 1.0 选项已禁用', ssoNoTLSv1 in Options);
    Check('TLS 1.1 选项已禁用', ssoNoTLSv1_1 in Options);
    Check('压缩已禁用', ssoDisableCompression in Options);
    Check('重协商已禁用', ssoDisableRenegotiation in Options);

  except
    on E: Exception do
      Check('安全默认配置', False, E.Message);
  end;
end;

{ 检查密码套件列表是否包含弱密码（排除以!开头的排除模式）}
function ContainsWeakCipherName(const ACipherList, AWeakName: string): Boolean;
var
  UpperList, UpperWeak: string;
  Parts: TStringArray;
  Part: string;
begin
  Result := False;
  UpperList := UpperCase(ACipherList);
  UpperWeak := UpperCase(AWeakName);

  // 按 ':' 分割密码套件列表
  Parts := UpperList.Split([':']);
  for Part in Parts do
  begin
    // 跳过以 '!' 开头的排除模式
    if (Length(Part) > 0) and (Part[1] = '!') then
      Continue;
    // 检查这个密码套件是否包含弱名称
    if Pos(UpperWeak, Part) > 0 then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

{ 测试密码套件强度 }
procedure TestCipherStrength;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  CipherList: string;
begin
  BeginSection('密码套件强度');

  try
    Lib := TOpenSSLLibrary.Create;
    if not Lib.Initialize then
    begin
      Check('初始化 OpenSSL', False);
      Exit;
    end;

    Ctx := Lib.CreateContext(sslCtxClient);
    if Ctx = nil then
    begin
      Check('创建上下文', False);
      Exit;
    end;

    ConfigureSecureDefaults(Ctx);

    // 获取配置的密码套件
    CipherList := Ctx.GetCipherList;
    Check('密码套件已配置', CipherList <> '');

    // 验证不包含弱密码（排除模式如 !ANULL, !RC4 是好的，不计入）
    Check('不含 NULL 密码', not ContainsWeakCipherName(CipherList, 'NULL'));
    Check('不含 EXPORT 密码', not ContainsWeakCipherName(CipherList, 'EXPORT'));
    Check('不含 DES 密码', not ContainsWeakCipherName(CipherList, 'DES'));
    Check('不含 RC4 密码', not ContainsWeakCipherName(CipherList, 'RC4'));
    Check('不含 MD5 密码', not ContainsWeakCipherName(CipherList, 'MD5'));

    // 验证优先使用强密码
    Check('优先 ECDHE', Pos('ECDHE', UpperCase(CipherList)) > 0);
    Check('支持 AES-GCM', Pos('GCM', UpperCase(CipherList)) > 0);

  except
    on E: Exception do
      Check('密码套件强度', False, E.Message);
  end;
end;

{ 测试协议版本限制 }
procedure TestProtocolVersionRestriction;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
begin
  BeginSection('协议版本限制');

  try
    Lib := TOpenSSLLibrary.Create;
    if not Lib.Initialize then
    begin
      Check('初始化 OpenSSL', False);
      Exit;
    end;

    Ctx := Lib.CreateContext(sslCtxClient);
    if Ctx = nil then
    begin
      Check('创建上下文', False);
      Exit;
    end;

    // 测试仅 TLS 1.3
    Ctx.SetProtocolVersions([sslProtocolTLS13]);
    Check('设置仅 TLS 1.3', sslProtocolTLS13 in Ctx.GetProtocolVersions);
    Check('TLS 1.2 已排除', not (sslProtocolTLS12 in Ctx.GetProtocolVersions));

    // 测试 TLS 1.2 和 1.3
    Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    Check('设置 TLS 1.2+1.3',
          (sslProtocolTLS12 in Ctx.GetProtocolVersions) and
          (sslProtocolTLS13 in Ctx.GetProtocolVersions));

    // 测试设置空（应至少有一个版本）
    Ctx.SetProtocolVersions([]);
    // 验证设置空后的行为（不应崩溃）
    Check('空协议版本集不崩溃', True);

  except
    on E: Exception do
      Check('协议版本限制', False, E.Message);
  end;
end;

{ 测试验证深度 }
procedure TestVerifyDepth;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
begin
  BeginSection('证书验证深度');

  try
    Lib := TOpenSSLLibrary.Create;
    if not Lib.Initialize then
    begin
      Check('初始化 OpenSSL', False);
      Exit;
    end;

    Ctx := Lib.CreateContext(sslCtxClient);
    if Ctx = nil then
    begin
      Check('创建上下文', False);
      Exit;
    end;

    // 测试默认验证深度
    Check('默认验证深度 > 0', Ctx.GetVerifyDepth > 0);

    // 测试设置验证深度
    Ctx.SetVerifyDepth(5);
    Check('设置验证深度为 5', Ctx.GetVerifyDepth = 5);

    // 测试设置极限值
    Ctx.SetVerifyDepth(1);
    Check('设置最小验证深度', Ctx.GetVerifyDepth = 1);

    Ctx.SetVerifyDepth(100);
    Check('设置大验证深度', Ctx.GetVerifyDepth = 100);

  except
    on E: Exception do
      Check('证书验证深度', False, E.Message);
  end;
end;

{ 测试验证模式 }
procedure TestVerifyModes;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  Mode: TSSLVerifyModes;
begin
  BeginSection('证书验证模式');

  try
    Lib := TOpenSSLLibrary.Create;
    if not Lib.Initialize then
    begin
      Check('初始化 OpenSSL', False);
      Exit;
    end;

    Ctx := Lib.CreateContext(sslCtxClient);
    if Ctx = nil then
    begin
      Check('创建上下文', False);
      Exit;
    end;

    // 测试设置验证对端
    Ctx.SetVerifyMode([sslVerifyPeer]);
    Mode := Ctx.GetVerifyMode;
    Check('设置 VerifyPeer', sslVerifyPeer in Mode);

    // 测试设置无验证（不推荐，但应该能工作）
    Ctx.SetVerifyMode([]);
    Mode := Ctx.GetVerifyMode;
    Check('设置无验证模式', Mode = []);

    // 测试设置严格验证
    Ctx.SetVerifyMode([sslVerifyPeer, sslVerifyFailIfNoPeerCert]);
    Mode := Ctx.GetVerifyMode;
    Check('设置严格验证',
          (sslVerifyPeer in Mode) and
          (sslVerifyFailIfNoPeerCert in Mode));

  except
    on E: Exception do
      Check('证书验证模式', False, E.Message);
  end;
end;

{ 测试会话配置 }
procedure TestSessionSecurity;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
begin
  BeginSection('会话安全');

  try
    Lib := TOpenSSLLibrary.Create;
    if not Lib.Initialize then
    begin
      Check('初始化 OpenSSL', False);
      Exit;
    end;

    Ctx := Lib.CreateContext(sslCtxClient);
    if Ctx = nil then
    begin
      Check('创建上下文', False);
      Exit;
    end;

    ConfigureSecureDefaults(Ctx);

    // 测试会话超时
    Check('会话超时 > 0', Ctx.GetSessionTimeout > 0);

    Ctx.SetSessionTimeout(3600);
    Check('设置会话超时', Ctx.GetSessionTimeout = 3600);

    // 测试会话缓存
    Ctx.SetSessionCacheMode(True);
    Check('启用会话缓存', Ctx.GetSessionCacheMode);

    Ctx.SetSessionCacheMode(False);
    Check('禁用会话缓存', not Ctx.GetSessionCacheMode);

    // 测试会话缓存大小
    Ctx.SetSessionCacheSize(1000);
    Check('设置会话缓存大小', Ctx.GetSessionCacheSize = 1000);

  except
    on E: Exception do
      Check('会话安全', False, E.Message);
  end;
end;

procedure PrintSummary;
begin
  WriteLn;
  WriteLn('================================================================');
  WriteLn('TLS 协议安全测试摘要');
  WriteLn('================================================================');
  WriteLn('总计: ', Total);
  WriteLn('通过: ', Passed);
  WriteLn('失败: ', Failed);
  WriteLn;

  if Failed = 0 then
    WriteLn('结果: 全部测试通过')
  else
    WriteLn('结果: 存在失败的测试');

  WriteLn('================================================================');
end;

begin
  Total := 0;
  Passed := 0;
  Failed := 0;

  WriteLn('================================================================');
  WriteLn('TLS 协议安全测试套件');
  WriteLn('================================================================');
  WriteLn;
  WriteLn('说明:');
  WriteLn('  验证 TLS 协议配置的安全性，包括版本限制、密码强度等。');
  WriteLn;

  try
    TestSecureDefaults;
    TestCipherStrength;
    TestProtocolVersionRestriction;
    TestVerifyDepth;
    TestVerifyModes;
    TestSessionSecurity;

    PrintSummary;

    if Failed > 0 then
      Halt(1);
  except
    on E: Exception do
    begin
      WriteLn('CRITICAL ERROR: ', E.Message);
      Halt(2);
    end;
  end;
end.
