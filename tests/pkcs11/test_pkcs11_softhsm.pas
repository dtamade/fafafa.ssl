{******************************************************************************}
{                                                                              }
{  fafafa.ssl - PKCS#11 SoftHSM 测试                                          }
{                                                                              }
{  Purpose: 测试 PKCS#11 功能，包括：                                          }
{    - 令牌初始化                                                              }
{    - 密钥生成                                                                }
{    - PIN 回调                                                                }
{    - 私钥加载                                                                }
{    - 签名操作                                                                }
{                                                                              }
{  Requirements:                                                               }
{    - SoftHSM2 已安装 (softhsm2-util, libsofthsm2.so)                        }
{    - OpenSSL 1.1.1+ 或 3.x                                                   }
{                                                                              }
{******************************************************************************}

program test_pkcs11_softhsm;

{$mode objfpc}{$H+}{$J-}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, Process,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.rsa,
  fafafa.ssl.openssl.api.bn,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.asn1,
  fafafa.ssl.openssl.api.pem,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.err,
  fafafa.ssl.pkcs11.types,
  fafafa.ssl.pkcs11.uri,
  fafafa.ssl.pkcs11.backend,
  fafafa.ssl.pkcs11.api,
  fafafa.ssl.context.builder,
  fafafa.ssl.base;

const
  // SoftHSM2 模块路径（Linux）
  SOFTHSM_MODULE_PATH = '/usr/lib/softhsm/libsofthsm2.so';
  // 测试令牌配置
  TEST_TOKEN_LABEL = 'TestToken';
  TEST_USER_PIN = '1234';
  TEST_SO_PIN = '12345678';
  TEST_KEY_LABEL = 'TestKey';
  TEST_SLOT = 0;

var
  TestsPassed: Integer = 0;
  TestsFailed: Integer = 0;
  TestsSkipped: Integer = 0;
  SoftHSMAvailable: Boolean = False;
  OpenSSLInitialized: Boolean = False;

procedure Pass(const TestName: string);
begin
  Inc(TestsPassed);
  WriteLn('[PASS] ', TestName);
end;

procedure Fail(const TestName, Reason: string);
begin
  Inc(TestsFailed);
  WriteLn('[FAIL] ', TestName, ': ', Reason);
end;

procedure Skip(const TestName, Reason: string);
begin
  Inc(TestsSkipped);
  WriteLn('[SKIP] ', TestName, ': ', Reason);
end;

procedure TestSection(const Name: string);
begin
  WriteLn;
  WriteLn(StringOfChar('=', 70));
  WriteLn(' ', Name);
  WriteLn(StringOfChar('=', 70));
end;

{ 运行外部命令 }
function RunCommand(const Cmd: string; const Args: array of string;
  out StdOut, StdErr: string): Integer;
var
  AProcess: TProcess;
  OutList, ErrList: TStringList;
  I: Integer;
begin
  Result := -1;
  StdOut := '';
  StdErr := '';

  AProcess := TProcess.Create(nil);
  OutList := TStringList.Create;
  ErrList := TStringList.Create;
  try
    AProcess.Executable := Cmd;
    for I := 0 to High(Args) do
      AProcess.Parameters.Add(Args[I]);

    AProcess.Options := [poWaitOnExit, poUsePipes, poStderrToOutPut];

    try
      AProcess.Execute;

      OutList.LoadFromStream(AProcess.Output);
      StdOut := OutList.Text;

      // StdErr 可能在 Output 流中（由于 poStderrToOutPut）
      StdErr := '';

      Result := AProcess.ExitCode;
    except
      on E: Exception do
      begin
        StdErr := E.Message;
        Result := -1;
      end;
    end;
  finally
    ErrList.Free;
    OutList.Free;
    AProcess.Free;
  end;
end;

{ 检查 SoftHSM2 是否可用 }
function CheckSoftHSMAvailable: Boolean;
var
  StdOut, StdErr: string;
begin
  Result := False;

  // 检查 SoftHSM2 模块是否存在
  if not FileExists(SOFTHSM_MODULE_PATH) then
  begin
    WriteLn('SoftHSM2 模块未找到: ', SOFTHSM_MODULE_PATH);
    Exit;
  end;

  // 检查 softhsm2-util 是否可用
  if RunCommand('softhsm2-util', ['--version'], StdOut, StdErr) <> 0 then
  begin
    WriteLn('softhsm2-util 不可用');
    Exit;
  end;

  WriteLn('SoftHSM2 版本: ', Trim(StdOut));
  Result := True;
end;

{ 初始化或重置 SoftHSM2 令牌 }
function InitializeSoftHSMToken: Boolean;
var
  StdOut, StdErr: string;
  ExitCode: Integer;
begin
  Result := False;

  WriteLn('正在初始化 SoftHSM2 令牌...');

  // 首先尝试删除现有令牌
  ExitCode := RunCommand('softhsm2-util', ['--delete-token', '--token', TEST_TOKEN_LABEL],
    StdOut, StdErr);
  // 忽略删除错误（令牌可能不存在）

  // 初始化新令牌
  ExitCode := RunCommand('softhsm2-util', [
    '--init-token',
    '--slot', IntToStr(TEST_SLOT),
    '--label', TEST_TOKEN_LABEL,
    '--pin', TEST_USER_PIN,
    '--so-pin', TEST_SO_PIN
  ], StdOut, StdErr);

  if ExitCode <> 0 then
  begin
    WriteLn('初始化令牌失败: ', StdErr);
    // 尝试使用 --free 选项
    ExitCode := RunCommand('softhsm2-util', [
      '--init-token',
      '--free',
      '--label', TEST_TOKEN_LABEL,
      '--pin', TEST_USER_PIN,
      '--so-pin', TEST_SO_PIN
    ], StdOut, StdErr);

    if ExitCode <> 0 then
    begin
      WriteLn('初始化令牌失败 (使用 --free): ', StdErr);
      Exit;
    end;
  end;

  WriteLn('令牌初始化成功');
  WriteLn(Trim(StdOut));
  Result := True;
end;

{ 在令牌中生成 RSA 密钥对 }
function GenerateKeyInToken: Boolean;
var
  StdOut, StdErr: string;
  ExitCode: Integer;
begin
  Result := False;

  WriteLn('正在在令牌中生成 RSA 密钥对...');

  // 使用 pkcs11-tool 生成密钥（如果可用）
  ExitCode := RunCommand('pkcs11-tool', [
    '--module', SOFTHSM_MODULE_PATH,
    '--token-label', TEST_TOKEN_LABEL,
    '--login',
    '--pin', TEST_USER_PIN,
    '--keypairgen',
    '--key-type', 'rsa:2048',
    '--id', '01',
    '--label', TEST_KEY_LABEL
  ], StdOut, StdErr);

  if ExitCode <> 0 then
  begin
    WriteLn('使用 pkcs11-tool 生成密钥失败: ', StdErr);
    WriteLn('尝试使用 softhsm2-util...');

    // 尝试使用 OpenSSL + PKCS#11 引擎生成密钥
    // 这是备选方案，但通常 pkcs11-tool 更可靠
    WriteLn('pkcs11-tool 不可用，跳过密钥生成');
    WriteLn('请手动运行以下命令生成测试密钥:');
    WriteLn('  pkcs11-tool --module ', SOFTHSM_MODULE_PATH, ' --token-label ', TEST_TOKEN_LABEL);
    WriteLn('              --login --pin ', TEST_USER_PIN, ' --keypairgen');
    WriteLn('              --key-type rsa:2048 --id 01 --label ', TEST_KEY_LABEL);
    Exit;
  end;

  WriteLn('密钥生成成功');
  WriteLn(Trim(StdOut));
  Result := True;
end;

{ 测试 PKCS#11 URI 解析 }
procedure TestPKCS11URIParsing;
var
  URI: TPKCS11URI;
  URIStr: string;
begin
  TestSection('测试 PKCS#11 URI 解析');

  // 测试 1: 解析带有所有属性的 URI
  try
    URIStr := 'pkcs11:token=' + TEST_TOKEN_LABEL + ';object=' + TEST_KEY_LABEL +
              '?module-path=' + SOFTHSM_MODULE_PATH + '&pin-value=' + TEST_USER_PIN;
    URI := TPKCS11URIParser.Parse(URIStr);

    if (URI.Token = TEST_TOKEN_LABEL) and
       (URI.ObjectLabel = TEST_KEY_LABEL) and
       (URI.ModulePath = SOFTHSM_MODULE_PATH) and
       (URI.PINValue = TEST_USER_PIN) then
      Pass('解析完整 PKCS#11 URI')
    else
      Fail('解析完整 PKCS#11 URI', '属性值不匹配');
  except
    on E: Exception do
      Fail('解析完整 PKCS#11 URI', E.Message);
  end;

  // 测试 2: 生成 URI
  try
    FillChar(URI, SizeOf(URI), 0);
    URI.Token := TEST_TOKEN_LABEL;
    URI.ObjectLabel := TEST_KEY_LABEL;
    URI.ModulePath := SOFTHSM_MODULE_PATH;

    URIStr := TPKCS11URIParser.Generate(URI);

    if (Pos('token=' + TEST_TOKEN_LABEL, URIStr) > 0) and
       (Pos('object=' + TEST_KEY_LABEL, URIStr) > 0) then
      Pass('生成 PKCS#11 URI')
    else
      Fail('生成 PKCS#11 URI', '生成的 URI 缺少必要属性');
  except
    on E: Exception do
      Fail('生成 PKCS#11 URI', E.Message);
  end;

  // 测试 3: 验证 URI
  try
    URIStr := 'pkcs11:token=' + TEST_TOKEN_LABEL;
    if TPKCS11URIParser.IsPKCS11URI(URIStr) then
      Pass('验证 PKCS#11 URI 格式')
    else
      Fail('验证 PKCS#11 URI 格式', '有效 URI 被拒绝');

    if not TPKCS11URIParser.IsPKCS11URI('file:///path/to/key.pem') then
      Pass('拒绝非 PKCS#11 URI')
    else
      Fail('拒绝非 PKCS#11 URI', '非 PKCS#11 URI 被接受');
  except
    on E: Exception do
      Fail('验证 PKCS#11 URI 格式', E.Message);
  end;
end;

{ 测试 PKCS#11 配置 }
procedure TestPKCS11Config;
var
  Config: TPKCS11Config;
  URI: TPKCS11URI;
begin
  TestSection('测试 PKCS#11 配置');

  // 测试 1: 默认配置
  try
    Config := TPKCS11ConfigDefault;
    if (Config.SlotID = -1) and Config.ReadOnly and Config.LoginRequired then
      Pass('默认配置值正确')
    else
      Fail('默认配置值正确', '默认值不正确');
  except
    on E: Exception do
      Fail('默认配置值正确', E.Message);
  end;

  // 测试 2: 从 URI 创建配置
  try
    FillChar(URI, SizeOf(URI), 0);
    URI.Token := TEST_TOKEN_LABEL;
    URI.ObjectLabel := TEST_KEY_LABEL;
    URI.ModulePath := SOFTHSM_MODULE_PATH;
    URI.PINValue := TEST_USER_PIN;

    Config := TPKCS11ConfigFromURI(URI);

    if (Config.TokenLabel = TEST_TOKEN_LABEL) and
       (Config.KeyLabel = TEST_KEY_LABEL) and
       (Config.ModulePath = SOFTHSM_MODULE_PATH) and
       (Config.PINMethod = pmValue) and
       (Config.PINValue = TEST_USER_PIN) then
      Pass('从 URI 创建配置')
    else
      Fail('从 URI 创建配置', '配置值不匹配');
  except
    on E: Exception do
      Fail('从 URI 创建配置', E.Message);
  end;

  // 测试 3: 配置验证
  try
    Config := TPKCS11ConfigDefault;
    Config.ModulePath := SOFTHSM_MODULE_PATH;
    Config.TokenLabel := TEST_TOKEN_LABEL;
    Config.KeyLabel := TEST_KEY_LABEL;

    if Config.IsValid then
      Pass('有效配置通过验证')
    else
      Fail('有效配置通过验证', 'IsValid 返回 False');

    Config.ModulePath := '';  // 无效配置
    if not Config.IsValid then
      Pass('无效配置被拒绝')
    else
      Fail('无效配置被拒绝', 'IsValid 应返回 False');
  except
    on E: Exception do
      Fail('配置验证', E.Message);
  end;
end;

{ PIN 回调函数 - 用于测试 }
type
  TTestPINCallbackHelper = class
    class function PINCallback(const ATokenLabel: string; out APIN: string): Boolean;
  end;

class function TTestPINCallbackHelper.PINCallback(const ATokenLabel: string; out APIN: string): Boolean;
begin
  WriteLn('  [PIN回调] 令牌: ', ATokenLabel);
  APIN := TEST_USER_PIN;
  Result := True;
end;

{ 测试 PKCS#11 后端 }
procedure TestPKCS11Backend;
var
  Backend: IPKCS11Backend;
begin
  TestSection('测试 PKCS#11 后端');

  // 测试 1: 检查后端可用性
  try
    if TPKCS11BackendFactory.IsBackendAvailable(btAuto) then
    begin
      Pass('PKCS#11 后端可用');

      // 测试 2: 创建后端实例
      try
        Backend := TPKCS11BackendFactory.CreateBackend(btAuto);
        if Backend <> nil then
        begin
          Pass('创建 PKCS#11 后端实例');
          WriteLn('  后端名称: ', Backend.GetName);
          WriteLn('  后端版本: ', Backend.GetVersion);
        end
        else
          Fail('创建 PKCS#11 后端实例', '返回 nil');
      except
        on E: Exception do
          Fail('创建 PKCS#11 后端实例', E.Message);
      end;
    end
    else
      Skip('PKCS#11 后端', 'OpenSSL PKCS#11 支持不可用');
  except
    on E: Exception do
      Fail('检查后端可用性', E.Message);
  end;
end;

{ 测试从 PKCS#11 加载私钥 }
procedure TestLoadPrivateKey;
var
  Config: TPKCS11Config;
  Backend: IPKCS11Backend;
  PKey: PEVP_PKEY;
begin
  TestSection('测试从 PKCS#11 加载私钥');

  if not SoftHSMAvailable then
  begin
    Skip('加载私钥', 'SoftHSM2 不可用');
    Exit;
  end;

  // 配置
  Config := TPKCS11ConfigDefault;
  Config.ModulePath := SOFTHSM_MODULE_PATH;
  Config.TokenLabel := TEST_TOKEN_LABEL;
  Config.KeyLabel := TEST_KEY_LABEL;
  Config.PINMethod := pmValue;
  Config.PINValue := TEST_USER_PIN;

  // 测试 1: 使用直接 PIN 加载私钥
  try
    if not TPKCS11BackendFactory.IsBackendAvailable(btAuto) then
    begin
      Skip('加载私钥 (直接 PIN)', 'PKCS#11 后端不可用');
      Exit;
    end;

    Backend := TPKCS11BackendFactory.CreateBackend(btAuto);

    try
      PKey := Backend.LoadPrivateKey(Config);
      if PKey <> nil then
      begin
        Pass('使用直接 PIN 加载私钥');
        EVP_PKEY_free(PKey);
      end
      else
        Fail('使用直接 PIN 加载私钥', '返回 nil');
    except
      on E: EPKCS11Exception do
        Fail('使用直接 PIN 加载私钥', Format('PKCS#11 错误: %s', [E.Message]));
      on E: Exception do
        Fail('使用直接 PIN 加载私钥', E.Message);
    end;
  except
    on E: Exception do
      Fail('创建后端', E.Message);
  end;

  // 测试 2: 使用 PIN 回调加载私钥
  try
    Config.PINMethod := pmCallback;
    Config.PINValue := '';
    Config.PINCallback := @TTestPINCallbackHelper.PINCallback;

    Backend := TPKCS11BackendFactory.CreateBackend(btAuto);

    try
      PKey := Backend.LoadPrivateKey(Config);
      if PKey <> nil then
      begin
        Pass('使用 PIN 回调加载私钥');
        EVP_PKEY_free(PKey);
      end
      else
        Fail('使用 PIN 回调加载私钥', '返回 nil');
    except
      on E: EPKCS11Exception do
        Fail('使用 PIN 回调加载私钥', Format('PKCS#11 错误: %s', [E.Message]));
      on E: Exception do
        Fail('使用 PIN 回调加载私钥', E.Message);
    end;
  except
    on E: Exception do
      Fail('PIN 回调测试', E.Message);
  end;
end;

{ 测试 SSL Context Builder 的 PKCS#11 支持 }
procedure TestContextBuilderPKCS11;
var
  Builder: ISSLContextBuilder;
  URIStr: string;
begin
  TestSection('测试 SSL Context Builder PKCS#11 支持');

  if not SoftHSMAvailable then
  begin
    Skip('Context Builder PKCS#11', 'SoftHSM2 不可用');
    Exit;
  end;

  // 构建 PKCS#11 URI
  URIStr := 'pkcs11:token=' + TEST_TOKEN_LABEL + ';object=' + TEST_KEY_LABEL +
            '?module-path=' + SOFTHSM_MODULE_PATH;

  // 测试 1: 配置 PKCS#11 URI
  try
    Builder := TSSLContextBuilder.Create;
    Builder := Builder.UsePKCS11(URIStr)
                      .WithPKCS11PIN(TEST_USER_PIN)
                      .WithTLS12And13
                      .WithVerifyPeer;
    Pass('配置 PKCS#11 URI');
  except
    on E: Exception do
      Fail('配置 PKCS#11 URI', E.Message);
  end;

  // 注意：完整的 BuildClient/BuildServer 测试需要有效的证书
  // 这里只测试配置部分
  WriteLn('  注意: 完整的客户端/服务器构建测试需要配套的证书');
end;

{ 测试签名操作 }
procedure TestSignOperation;
var
  Config: TPKCS11Config;
  Backend: IPKCS11Backend;
  PKey: PEVP_PKEY;
  Data: AnsiString;
  Signature: TBytes;
  MD_ctx: PEVP_MD_CTX;
  SigLen: NativeUInt;
begin
  TestSection('测试签名操作');

  if not SoftHSMAvailable then
  begin
    Skip('签名操作', 'SoftHSM2 不可用');
    Exit;
  end;

  if not OpenSSLInitialized then
  begin
    Skip('签名操作', 'OpenSSL 未初始化');
    Exit;
  end;

  // 配置
  Config := TPKCS11ConfigDefault;
  Config.ModulePath := SOFTHSM_MODULE_PATH;
  Config.TokenLabel := TEST_TOKEN_LABEL;
  Config.KeyLabel := TEST_KEY_LABEL;
  Config.PINMethod := pmValue;
  Config.PINValue := TEST_USER_PIN;

  try
    if not TPKCS11BackendFactory.IsBackendAvailable(btAuto) then
    begin
      Skip('签名操作', 'PKCS#11 后端不可用');
      Exit;
    end;

    Backend := TPKCS11BackendFactory.CreateBackend(btAuto);
    PKey := Backend.LoadPrivateKey(Config);

    if PKey = nil then
    begin
      Fail('签名操作 - 加载密钥', '无法加载私钥');
      Exit;
    end;

    try
      // 准备要签名的数据
      Data := 'Hello, PKCS#11 World!';

      // 创建签名上下文
      MD_ctx := EVP_MD_CTX_new();
      if MD_ctx = nil then
      begin
        Fail('签名操作', '无法创建 MD 上下文');
        Exit;
      end;

      try
        // 初始化签名操作
        if EVP_DigestSignInit(MD_ctx, nil, EVP_sha256(), nil, PKey) <> 1 then
        begin
          Fail('签名操作', 'EVP_DigestSignInit 失败');
          Exit;
        end;

        // 更新数据
        if EVP_DigestSignUpdate(MD_ctx, PAnsiChar(Data), Length(Data)) <> 1 then
        begin
          Fail('签名操作', 'EVP_DigestSignUpdate 失败');
          Exit;
        end;

        // 获取签名长度
        SigLen := 0;
        if EVP_DigestSignFinal(MD_ctx, nil, SigLen) <> 1 then
        begin
          Fail('签名操作', 'EVP_DigestSignFinal (获取长度) 失败');
          Exit;
        end;

        // 分配签名缓冲区
        SetLength(Signature, SigLen);

        // 执行签名
        if EVP_DigestSignFinal(MD_ctx, @Signature[0], SigLen) <> 1 then
        begin
          Fail('签名操作', 'EVP_DigestSignFinal 失败');
          Exit;
        end;

        SetLength(Signature, SigLen);  // 调整为实际长度

        if Length(Signature) > 0 then
        begin
          Pass('使用 PKCS#11 密钥签名');
          WriteLn('  签名长度: ', Length(Signature), ' 字节');
        end
        else
          Fail('签名操作', '签名为空');

      finally
        EVP_MD_CTX_free(MD_ctx);
      end;
    finally
      EVP_PKEY_free(PKey);
    end;
  except
    on E: Exception do
      Fail('签名操作', E.Message);
  end;
end;

{ 清理 SoftHSM2 测试令牌 }
procedure CleanupSoftHSMToken;
var
  StdOut, StdErr: string;
begin
  WriteLn;
  WriteLn('正在清理测试令牌...');

  RunCommand('softhsm2-util', ['--delete-token', '--token', TEST_TOKEN_LABEL],
    StdOut, StdErr);

  WriteLn('清理完成');
end;

{ 主程序 }
begin
  WriteLn('fafafa.ssl PKCS#11 SoftHSM 测试');
  WriteLn('================================');
  WriteLn;

  // 初始化 OpenSSL
  WriteLn('正在初始化 OpenSSL...');
  try
    try
      LoadOpenSSLCore;
    except
      on E: Exception do
      begin
        Skip('OpenSSL initialization', E.Message);
        Exit;
      end;
    end;

    if TOpenSSLLoader.IsModuleLoaded(osmCore) then
    begin
      OpenSSLInitialized := True;
      WriteLn('OpenSSL 初始化成功');
      WriteLn('OpenSSL 版本: ', GetOpenSSLVersionString);
    end
    else
    begin
      WriteLn('警告: OpenSSL 初始化失败');
      WriteLn('部分测试将被跳过');
    end;
  except
    on E: Exception do
    begin
      WriteLn('OpenSSL 初始化错误: ', E.Message);
    end;
  end;

  // 检查 SoftHSM2 可用性
  WriteLn;
  WriteLn('正在检查 SoftHSM2...');
  SoftHSMAvailable := CheckSoftHSMAvailable;

  if not SoftHSMAvailable then
  begin
    WriteLn;
    WriteLn('SoftHSM2 不可用，将跳过需要 SoftHSM2 的测试');
    WriteLn('要安装 SoftHSM2:');
    WriteLn('  Ubuntu/Debian: sudo apt-get install softhsm2');
    WriteLn('  Fedora/RHEL:   sudo dnf install softhsm');
  end
  else
  begin
    // 初始化测试令牌
    if not InitializeSoftHSMToken then
    begin
      WriteLn('警告: 无法初始化测试令牌');
      SoftHSMAvailable := False;
    end
    else
    begin
      // 生成测试密钥
      if not GenerateKeyInToken then
      begin
        WriteLn('警告: 无法生成测试密钥');
        // 继续测试，某些测试可能仍然有效
      end;
    end;
  end;

  // 运行测试
  TestPKCS11URIParsing;
  TestPKCS11Config;
  TestPKCS11Backend;
  TestLoadPrivateKey;
  TestContextBuilderPKCS11;
  TestSignOperation;

  // 清理
  if SoftHSMAvailable then
    CleanupSoftHSMToken;

  // 输出测试结果摘要
  WriteLn;
  WriteLn(StringOfChar('=', 70));
  WriteLn(' 测试结果摘要');
  WriteLn(StringOfChar('=', 70));
  WriteLn('通过: ', TestsPassed);
  WriteLn('失败: ', TestsFailed);
  WriteLn('跳过: ', TestsSkipped);
  WriteLn(StringOfChar('-', 70));

  if TestsFailed = 0 then
  begin
    WriteLn('所有测试通过!');
    ExitCode := 0;
  end
  else
  begin
    WriteLn('有 ', TestsFailed, ' 个测试失败');
    ExitCode := 1;
  end;
end.
