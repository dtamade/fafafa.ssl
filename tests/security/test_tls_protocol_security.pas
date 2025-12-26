program test_tls_protocol_security;

{$mode objfpc}{$H+}{$J-}

{**
 * TLS Protocol Security Test Suite
 *
 * P0-3: 安全测试扩展 - TLS 协议安全 (已迁移到 TSimpleTestRunner)
 *
 * 测试内容:
 * - 协议版本限制（禁用 SSLv2/SSLv3/TLS 1.0/1.1）
 * - 密码套件强度验证
 * - 安全默认配置
 * - 降级攻击防护
 *
 * @author fafafa.ssl team
 * @version 1.1.0 - P1-2.3 迁移到统一测试框架
 * @since 2025-12-23
 *}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.openssl.backed,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.api.core,
  test_openssl_base;

var
  Runner: TSimpleTestRunner;

{ 配置安全默认值（通过接口方法） }
procedure ConfigureSecureDefaults(Ctx: ISSLContext);
begin
  Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
  Ctx.SetOptions([
    ssoEnableSessionCache, ssoEnableSessionTickets, ssoDisableCompression,
    ssoDisableRenegotiation, ssoNoSSLv2, ssoNoSSLv3, ssoNoTLSv1, ssoNoTLSv1_1,
    ssoCipherServerPreference, ssoSingleECDHUse
  ]);
  Ctx.SetCipherList('ECDHE+AESGCM:ECDHE+CHACHA20:ECDHE+AES256:DHE+AESGCM:DHE+AES256:!ANULL:!MD5:!DSS:!RC4:!3DES');
  Ctx.SetCipherSuites('TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256:TLS_AES_128_GCM_SHA256');
  Ctx.SetVerifyMode([sslVerifyPeer]);
  Ctx.SetVerifyDepth(4);
  Ctx.SetSessionCacheMode(True);
  Ctx.SetSessionTimeout(3600);
end;

function ContainsWeakCipherName(const ACipherList, AWeakName: string): Boolean;
var
  UpperList, UpperWeak: string;
  Parts: TStringArray;
  Part: string;
begin
  Result := False;
  UpperList := UpperCase(ACipherList);
  UpperWeak := UpperCase(AWeakName);
  Parts := UpperList.Split([':']);
  for Part in Parts do
  begin
    if (Length(Part) > 0) and (Part[1] = '!') then Continue;
    if Pos(UpperWeak, Part) > 0 then Exit(True);
  end;
end;

procedure TestSecureDefaults;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  Options: TSSLOptions;
  Versions: TSSLProtocolVersions;
begin
  WriteLn;
  WriteLn('=== 安全默认配置 ===');

  try
    Lib := TOpenSSLLibrary.Create;
    if not Lib.Initialize then begin Runner.Check('初始化 OpenSSL', False); Exit; end;
    Runner.Check('初始化 OpenSSL', True);

    Ctx := Lib.CreateContext(sslCtxClient);
    Runner.Check('创建上下文', Ctx <> nil);
    if Ctx = nil then Exit;

    ConfigureSecureDefaults(Ctx);
    Runner.Check('配置安全默认值', True);

    Versions := Ctx.GetProtocolVersions;
    Runner.Check('TLS 1.2 已启用', sslProtocolTLS12 in Versions);
    Runner.Check('TLS 1.3 已启用', sslProtocolTLS13 in Versions);
    Runner.Check('TLS 1.0 已禁用', not (sslProtocolTLS10 in Versions));
    Runner.Check('TLS 1.1 已禁用', not (sslProtocolTLS11 in Versions));

    Options := Ctx.GetOptions;
    Runner.Check('SSLv2 已禁用', ssoNoSSLv2 in Options);
    Runner.Check('SSLv3 已禁用', ssoNoSSLv3 in Options);
    Runner.Check('TLS 1.0 选项已禁用', ssoNoTLSv1 in Options);
    Runner.Check('TLS 1.1 选项已禁用', ssoNoTLSv1_1 in Options);
    Runner.Check('压缩已禁用', ssoDisableCompression in Options);
    Runner.Check('重协商已禁用', ssoDisableRenegotiation in Options);
  except
    on E: Exception do Runner.Check('安全默认配置', False, E.Message);
  end;
end;

procedure TestCipherStrength;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  CipherList: string;
begin
  WriteLn;
  WriteLn('=== 密码套件强度 ===');

  try
    Lib := TOpenSSLLibrary.Create;
    if not Lib.Initialize then begin Runner.Check('初始化 OpenSSL', False); Exit; end;

    Ctx := Lib.CreateContext(sslCtxClient);
    if Ctx = nil then begin Runner.Check('创建上下文', False); Exit; end;

    ConfigureSecureDefaults(Ctx);
    CipherList := Ctx.GetCipherList;
    Runner.Check('密码套件已配置', CipherList <> '');

    Runner.Check('不含 NULL 密码', not ContainsWeakCipherName(CipherList, 'NULL'));
    Runner.Check('不含 EXPORT 密码', not ContainsWeakCipherName(CipherList, 'EXPORT'));
    Runner.Check('不含 DES 密码', not ContainsWeakCipherName(CipherList, 'DES'));
    Runner.Check('不含 RC4 密码', not ContainsWeakCipherName(CipherList, 'RC4'));
    Runner.Check('不含 MD5 密码', not ContainsWeakCipherName(CipherList, 'MD5'));

    Runner.Check('优先 ECDHE', Pos('ECDHE', UpperCase(CipherList)) > 0);
    Runner.Check('支持 AES-GCM', Pos('GCM', UpperCase(CipherList)) > 0);
  except
    on E: Exception do Runner.Check('密码套件强度', False, E.Message);
  end;
end;

procedure TestProtocolVersionRestriction;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
begin
  WriteLn;
  WriteLn('=== 协议版本限制 ===');

  try
    Lib := TOpenSSLLibrary.Create;
    if not Lib.Initialize then begin Runner.Check('初始化 OpenSSL', False); Exit; end;

    Ctx := Lib.CreateContext(sslCtxClient);
    if Ctx = nil then begin Runner.Check('创建上下文', False); Exit; end;

    Ctx.SetProtocolVersions([sslProtocolTLS13]);
    Runner.Check('设置仅 TLS 1.3', sslProtocolTLS13 in Ctx.GetProtocolVersions);
    Runner.Check('TLS 1.2 已排除', not (sslProtocolTLS12 in Ctx.GetProtocolVersions));

    Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    Runner.Check('设置 TLS 1.2+1.3',
          (sslProtocolTLS12 in Ctx.GetProtocolVersions) and
          (sslProtocolTLS13 in Ctx.GetProtocolVersions));

    Ctx.SetProtocolVersions([]);
    Runner.Check('空协议版本集不崩溃', True);
  except
    on E: Exception do Runner.Check('协议版本限制', False, E.Message);
  end;
end;

procedure TestVerifyDepth;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
begin
  WriteLn;
  WriteLn('=== 证书验证深度 ===');

  try
    Lib := TOpenSSLLibrary.Create;
    if not Lib.Initialize then begin Runner.Check('初始化 OpenSSL', False); Exit; end;

    Ctx := Lib.CreateContext(sslCtxClient);
    if Ctx = nil then begin Runner.Check('创建上下文', False); Exit; end;

    Runner.Check('默认验证深度 > 0', Ctx.GetVerifyDepth > 0);
    Ctx.SetVerifyDepth(5);
    Runner.Check('设置验证深度为 5', Ctx.GetVerifyDepth = 5);
    Ctx.SetVerifyDepth(1);
    Runner.Check('设置最小验证深度', Ctx.GetVerifyDepth = 1);
    Ctx.SetVerifyDepth(100);
    Runner.Check('设置大验证深度', Ctx.GetVerifyDepth = 100);
  except
    on E: Exception do Runner.Check('证书验证深度', False, E.Message);
  end;
end;

procedure TestVerifyModes;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  Mode: TSSLVerifyModes;
begin
  WriteLn;
  WriteLn('=== 证书验证模式 ===');

  try
    Lib := TOpenSSLLibrary.Create;
    if not Lib.Initialize then begin Runner.Check('初始化 OpenSSL', False); Exit; end;

    Ctx := Lib.CreateContext(sslCtxClient);
    if Ctx = nil then begin Runner.Check('创建上下文', False); Exit; end;

    Ctx.SetVerifyMode([sslVerifyPeer]);
    Mode := Ctx.GetVerifyMode;
    Runner.Check('设置 VerifyPeer', sslVerifyPeer in Mode);

    Ctx.SetVerifyMode([]);
    Mode := Ctx.GetVerifyMode;
    Runner.Check('设置无验证模式', Mode = []);

    Ctx.SetVerifyMode([sslVerifyPeer, sslVerifyFailIfNoPeerCert]);
    Mode := Ctx.GetVerifyMode;
    Runner.Check('设置严格验证', (sslVerifyPeer in Mode) and (sslVerifyFailIfNoPeerCert in Mode));
  except
    on E: Exception do Runner.Check('证书验证模式', False, E.Message);
  end;
end;

procedure TestSessionSecurity;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
begin
  WriteLn;
  WriteLn('=== 会话安全 ===');

  try
    Lib := TOpenSSLLibrary.Create;
    if not Lib.Initialize then begin Runner.Check('初始化 OpenSSL', False); Exit; end;

    Ctx := Lib.CreateContext(sslCtxClient);
    if Ctx = nil then begin Runner.Check('创建上下文', False); Exit; end;

    ConfigureSecureDefaults(Ctx);

    Runner.Check('会话超时 > 0', Ctx.GetSessionTimeout > 0);
    Ctx.SetSessionTimeout(3600);
    Runner.Check('设置会话超时', Ctx.GetSessionTimeout = 3600);

    Ctx.SetSessionCacheMode(True);
    Runner.Check('启用会话缓存', Ctx.GetSessionCacheMode);
    Ctx.SetSessionCacheMode(False);
    Runner.Check('禁用会话缓存', not Ctx.GetSessionCacheMode);

    Ctx.SetSessionCacheSize(1000);
    Runner.Check('设置会话缓存大小', Ctx.GetSessionCacheSize = 1000);
  except
    on E: Exception do Runner.Check('会话安全', False, E.Message);
  end;
end;

begin
  WriteLn('================================================================');
  WriteLn('TLS 协议安全测试套件');
  WriteLn('================================================================');

  Runner := TSimpleTestRunner.Create;
  try
    Runner.RequireModules([osmCore, osmSSL]);
    if not Runner.Initialize then
    begin
      WriteLn('ERROR: Failed to initialize test environment');
      Halt(1);
    end;

    WriteLn('OpenSSL Version: ', GetOpenSSLVersionString);

    TestSecureDefaults;
    TestCipherStrength;
    TestProtocolVersionRestriction;
    TestVerifyDepth;
    TestVerifyModes;
    TestSessionSecurity;

    Runner.PrintSummary;
    Halt(Runner.FailCount);
  finally
    Runner.Free;
  end;
end.
