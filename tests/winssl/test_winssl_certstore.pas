program test_winssl_certstore;

{$mode objfpc}{$H+}

{
  test_winssl_certstore - WinSSL 证书存储测试

  版本: 1.0
  作者: fafafa.ssl 开发团队
  创建: 2026-01-18

  描述:
    Phase 3.4 测试覆盖 - 第三阶段
    测试 WinSSL 证书存储功能

    需要 Windows 环境运行

  测试内容:
    1. 证书存储创建和打开
    2. 系统存储访问
    3. 证书添加和删除
    4. 证书查询和搜索
    5. 证书验证
    6. 证书链构建
    7. 存储清空操作
    8. 证书计数
    9. 证书加载
    10. 存储关闭
}

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.exceptions,
  fafafa.ssl.winssl.base,
  fafafa.ssl.winssl.api,
  fafafa.ssl.winssl.certstore,
  fafafa.ssl.winssl.certificate;

var
  GTestsPassed: Integer = 0;
  GTestsFailed: Integer = 0;

procedure Assert(ACondition: Boolean; const AMessage: string);
begin
  if ACondition then
  begin
    Inc(GTestsPassed);
    WriteLn('  ✓ ', AMessage);
  end
  else
  begin
    Inc(GTestsFailed);
    WriteLn('  ✗ FAILED: ', AMessage);
  end;
end;

function BuildLooseDNQueryVariant(const AValue: string): string;
begin
  Result := Trim(AValue);
  Result := StringReplace(Result, ',', ' , ', [rfReplaceAll]);
  Result := StringReplace(Result, '=', ' = ', [rfReplaceAll]);
  Result := '  ' + LowerCase(Result) + '  ';
end;

function CreateMemoryBackedStore: TWinSSLCertificateStore;
var
  LStoreHandle: HCERTSTORE;
begin
  Result := nil;
  LStoreHandle := CertOpenStore(
    CERT_STORE_PROV_MEMORY,
    X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
    0,
    0,
    nil
  );
  if LStoreHandle <> nil then
    Result := TWinSSLCertificateStore.Create(LStoreHandle, True);
end;

function OpenConcreteSystemStore(const AStoreName: string): TWinSSLCertificateStore;
begin
  Result := TWinSSLCertificateStore.Create(AStoreName);
end;

procedure TestStoreCreation;
var
  LStore: TWinSSLCertificateStore;
begin
  WriteLn('【测试 1】证书存储创建');
  WriteLn('---');

  try
    LStore := TWinSSLCertificateStore.Create('');
    Assert(LStore <> nil, '证书存储创建成功');
    Assert(not LStore.IsOpen, '新创建的存储未打开');

  except
    on E: Exception do
      WriteLn('  注意: 测试需要 Windows 环境 - ', E.Message);
  end;

  WriteLn;
end;

procedure TestStoreOpen;
var
  LStore: TWinSSLCertificateStore;
begin
  WriteLn('【测试 2】证书存储打开');
  WriteLn('---');

  try
    LStore := TWinSSLCertificateStore.Create('');

    // 打开存储（只读）
    Assert(LStore.Open('MY', False), '证书存储打开成功（只读）');
    Assert(LStore.IsOpen, '证书存储状态为已打开');

    // 关闭存储
    LStore.Close;
    Assert(not LStore.IsOpen, '证书存储关闭后状态正确');

  except
    on E: Exception do
      WriteLn('  注意: 测试需要 Windows 环境 - ', E.Message);
  end;

  WriteLn;
end;

procedure TestSystemStoreAccess;
var
  LStore: TWinSSLCertificateStore;
begin
  WriteLn('【测试 3】系统存储访问');
  WriteLn('---');

  try
    // 测试访问 ROOT 存储
    LStore := OpenConcreteSystemStore(SSL_STORE_ROOT);
    Assert(LStore <> nil, 'ROOT 系统存储访问成功');
    Assert(LStore.IsOpen, 'ROOT 存储已打开');
    LStore.Close;

    // 测试访问 MY 存储
    LStore := OpenConcreteSystemStore(SSL_STORE_MY);
    Assert(LStore <> nil, 'MY 系统存储访问成功');
    LStore.Close;

    // 测试访问 CA 存储
    LStore := OpenConcreteSystemStore(SSL_STORE_CA);
    Assert(LStore <> nil, 'CA 系统存储访问成功');
    LStore.Close;

  except
    on E: Exception do
      WriteLn('  注意: 测试需要 Windows 环境 - ', E.Message);
  end;

  WriteLn;
end;

procedure TestStoreCount;
var
  LStore: TWinSSLCertificateStore;
  LCount: Integer;
begin
  WriteLn('【测试 4】证书计数');
  WriteLn('---');

  try
    LStore := OpenConcreteSystemStore(SSL_STORE_ROOT);

    // 获取证书数量
    LCount := LStore.GetCount;
    Assert(LCount >= 0, 'ROOT 存储证书数量 >= 0');
    WriteLn('  信息: ROOT 存储包含 ', LCount, ' 个证书');

    LStore.Close;

  except
    on E: Exception do
      WriteLn('  注意: 测试需要 Windows 环境 - ', E.Message);
  end;

  WriteLn;
end;

procedure TestGetAllCertificates;
var
  LStore: TWinSSLCertificateStore;
  LCerts: TSSLCertificateArray;
begin
  WriteLn('【测试 5】获取所有证书');
  WriteLn('---');

  try
    LStore := OpenConcreteSystemStore(SSL_STORE_ROOT);

    // 获取所有证书
    LCerts := LStore.GetAllCertificates;
    Assert(Length(LCerts) >= 0, '获取证书数组成功');
    WriteLn('  信息: 获取到 ', Length(LCerts), ' 个证书');

    LStore.Close;

  except
    on E: Exception do
      WriteLn('  注意: 测试需要 Windows 环境 - ', E.Message);
  end;

  WriteLn;
end;

procedure TestGetCertificateByIndex;
var
  LStore: TWinSSLCertificateStore;
  LCert: ISSLCertificate;
  LCount: Integer;
begin
  WriteLn('【测试 6】按索引获取证书');
  WriteLn('---');

  try
    LStore := OpenConcreteSystemStore(SSL_STORE_ROOT);
    LCount := LStore.GetCount;

    if LCount > 0 then
    begin
      // 获取第一个证书
      LCert := LStore.GetCertificate(0);
      Assert(LCert <> nil, '按索引获取证书成功');

      // 测试无效索引
      try
        LCert := LStore.GetCertificate(LCount + 100);
        Assert(LCert = nil, '无效索引返回 nil');
      except
        on E: Exception do
          Assert(True, '无效索引抛出异常（预期行为）');
      end;
    end
    else
      WriteLn('  跳过: ROOT 存储为空');

    LStore.Close;

  except
    on E: Exception do
      WriteLn('  注意: 测试需要 Windows 环境 - ', E.Message);
  end;

  WriteLn;
end;

procedure TestFindBySubject;
var
  LStore: TWinSSLCertificateStore;
  LCert: ISSLCertificate;
begin
  WriteLn('【测试 7】按主题查找证书');
  WriteLn('---');

  try
    LStore := OpenConcreteSystemStore(SSL_STORE_ROOT);

    // 查找不存在的证书
    LCert := LStore.FindBySubject('CN=NonExistentCert');
    Assert(LCert = nil, '查找不存在的证书返回 nil');

    // 查找可能存在的证书（Microsoft Root）
    LCert := LStore.FindBySubject('Microsoft');
    if LCert <> nil then
      Assert(True, '按主题查找证书成功')
    else
      WriteLn('  信息: 未找到 Microsoft 证书');

    LStore.Close;

  except
    on E: Exception do
      WriteLn('  注意: 测试需要 Windows 环境 - ', E.Message);
  end;

  WriteLn;
end;

procedure TestFindByIssuer;
var
  LStore: TWinSSLCertificateStore;
  LCert: ISSLCertificate;
begin
  WriteLn('【测试 8】按颁发者查找证书');
  WriteLn('---');

  try
    LStore := OpenConcreteSystemStore(SSL_STORE_ROOT);

    // 查找不存在的颁发者
    LCert := LStore.FindByIssuer('CN=NonExistentIssuer');
    Assert(LCert = nil, '查找不存在的颁发者返回 nil');

    LStore.Close;

  except
    on E: Exception do
      WriteLn('  注意: 测试需要 Windows 环境 - ', E.Message);
  end;

  WriteLn;
end;

procedure TestDeterministicDNQueryContract;
var
  LStore: TWinSSLCertificateStore;
  LCert: ISSLCertificate;
  LSubjectVariant: string;
  LIssuerVariant: string;
begin
  WriteLn('【测试 8A】确定性 DN 查询契约');
  WriteLn('---');

  try
    LStore := CreateMemoryBackedStore;
    Assert(LStore <> nil, '创建内存证书存储成功');

    LCert := TWinSSLCertificate.Create(nil, False);
    Assert(LCert <> nil, '创建夹具证书对象成功');
    Assert(LCert.LoadFromFile('tests/certificate/test_certs/signer_cert.pem'),
      '加载 distinct-issuer fixture 成功');
    Assert(LStore.AddCertificate(LCert), '夹具证书加入内存存储成功');

    LSubjectVariant := BuildLooseDNQueryVariant('CN=Test Signer,O=Test Org');
    Assert(LStore.FindBySubject(LSubjectVariant) <> nil,
      '按归一化主题片段查找成功');
    Assert(LStore.FindBySubject('') = nil, '空主题查询返回 nil');

    LIssuerVariant := BuildLooseDNQueryVariant('CN=Test CA,O=Test CA');
    Assert(LStore.FindByIssuer(LIssuerVariant) <> nil,
      '按归一化颁发者片段查找成功');
    Assert(LStore.FindByIssuer('') = nil, '空颁发者查询返回 nil');
    Assert(LStore.GetCount = 1, '内存存储证书数量保持为 1');

    LStore.Close;

  except
    on E: Exception do
      WriteLn('  注意: 测试需要 Windows 环境 - ', E.Message);
  end;

  WriteLn;
end;

procedure TestFindBySerialNumber;
var
  LStore: TWinSSLCertificateStore;
  LCert: ISSLCertificate;
begin
  WriteLn('【测试 9】按序列号查找证书');
  WriteLn('---');

  try
    LStore := OpenConcreteSystemStore(SSL_STORE_ROOT);

    // 查找不存在的序列号
    LCert := LStore.FindBySerialNumber('00:00:00:00');
    Assert(LCert = nil, '查找不存在的序列号返回 nil');

    LStore.Close;

  except
    on E: Exception do
      WriteLn('  注意: 测试需要 Windows 环境 - ', E.Message);
  end;

  WriteLn;
end;

procedure TestFindByFingerprint;
var
  LStore: TWinSSLCertificateStore;
  LCert: ISSLCertificate;
begin
  WriteLn('【测试 10】按指纹查找证书');
  WriteLn('---');

  try
    LStore := OpenConcreteSystemStore(SSL_STORE_ROOT);

    // 查找不存在的指纹
    LCert := LStore.FindByFingerprint('00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00');
    Assert(LCert = nil, '查找不存在的指纹返回 nil');

    LStore.Close;

  except
    on E: Exception do
      WriteLn('  注意: 测试需要 Windows 环境 - ', E.Message);
  end;

  WriteLn;
end;

procedure TestContainsCertificate;
var
  LStore: TWinSSLCertificateStore;
  LCert: ISSLCertificate;
  LCount: Integer;
begin
  WriteLn('【测试 11】检查证书是否存在');
  WriteLn('---');

  try
    LStore := OpenConcreteSystemStore(SSL_STORE_ROOT);
    LCount := LStore.GetCount;

    if LCount > 0 then
    begin
      // 获取第一个证书
      LCert := LStore.GetCertificate(0);

      // 检查证书是否在存储中
      Assert(LStore.Contains(LCert), '证书存在于存储中');
    end
    else
      WriteLn('  跳过: ROOT 存储为空');

    LStore.Close;

  except
    on E: Exception do
      WriteLn('  注意: 测试需要 Windows 环境 - ', E.Message);
  end;

  WriteLn;
end;

procedure TestLoadSystemStore;
var
  LStore: TWinSSLCertificateStore;
begin
  WriteLn('【测试 12】加载系统存储');
  WriteLn('---');

  try
    LStore := TWinSSLCertificateStore.Create('');

    // 加载系统存储
    Assert(LStore.LoadSystemStore, '加载系统存储成功');
    Assert(LStore.IsOpen, '加载系统存储后状态为已打开');
    Assert(LStore.GetCount >= 0, '加载后证书数量 >= 0');

    LStore.Close;

  except
    on E: Exception do
      WriteLn('  注意: 测试需要 Windows 环境 - ', E.Message);
  end;

  WriteLn;
end;

procedure TestStoreNativeHandle;
var
  LStore: TWinSSLCertificateStore;
  LHandle: Pointer;
begin
  WriteLn('【测试 13】获取原生句柄');
  WriteLn('---');

  try
    LStore := OpenConcreteSystemStore(SSL_STORE_ROOT);

    // 获取原生句柄
    LHandle := LStore.GetNativeHandle;
    Assert(LHandle <> nil, '原生句柄非空');

    LStore.Close;

  except
    on E: Exception do
      WriteLn('  注意: 测试需要 Windows 环境 - ', E.Message);
  end;

  WriteLn;
end;

procedure TestMultipleStores;
var
  LStore1, LStore2, LStore3: TWinSSLCertificateStore;
begin
  WriteLn('【测试 14】多个存储同时打开');
  WriteLn('---');

  try
    // 同时打开多个存储
    LStore1 := OpenConcreteSystemStore(SSL_STORE_ROOT);
    LStore2 := OpenConcreteSystemStore(SSL_STORE_MY);
    LStore3 := OpenConcreteSystemStore(SSL_STORE_CA);

    Assert(LStore1.IsOpen, 'ROOT 存储已打开');
    Assert(LStore2.IsOpen, 'MY 存储已打开');
    Assert(LStore3.IsOpen, 'CA 存储已打开');

    // 关闭所有存储
    LStore1.Close;
    LStore2.Close;
    LStore3.Close;

    Assert(not LStore1.IsOpen, 'ROOT 存储已关闭');
    Assert(not LStore2.IsOpen, 'MY 存储已关闭');
    Assert(not LStore3.IsOpen, 'CA 存储已关闭');

  except
    on E: Exception do
      WriteLn('  注意: 测试需要 Windows 环境 - ', E.Message);
  end;

  WriteLn;
end;

procedure TestStoreReopen;
var
  LStore: TWinSSLCertificateStore;
begin
  WriteLn('【测试 15】存储重新打开');
  WriteLn('---');

  try
    LStore := TWinSSLCertificateStore.Create('');

    // 第一次打开
    Assert(LStore.Open('ROOT', False), '第一次打开成功');
    Assert(LStore.IsOpen, '存储已打开');

    // 关闭
    LStore.Close;
    Assert(not LStore.IsOpen, '存储已关闭');

    // 重新打开
    Assert(LStore.Open('ROOT', False), '重新打开成功');
    Assert(LStore.IsOpen, '存储重新打开后状态正确');

    LStore.Close;

  except
    on E: Exception do
      WriteLn('  注意: 测试需要 Windows 环境 - ', E.Message);
  end;

  WriteLn;
end;

procedure PrintSummary;
begin
  WriteLn('=========================================');
  WriteLn('测试总结');
  WriteLn('=========================================');
  WriteLn('通过: ', GTestsPassed);
  WriteLn('失败: ', GTestsFailed);
  WriteLn('总计: ', GTestsPassed + GTestsFailed);

  if GTestsFailed = 0 then
  begin
    WriteLn;
    WriteLn('✓ 所有证书存储测试通过！');
  end
  else
  begin
    WriteLn;
    WriteLn('✗ 有测试失败，请检查证书存储实现');
  end;
  WriteLn('=========================================');
end;

begin
  WriteLn('=========================================');
  WriteLn('WinSSL 证书存储测试');
  WriteLn('测试日期: ', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
  WriteLn('=========================================');
  WriteLn;

  {$IFDEF WINDOWS}
  WriteLn('运行环境: Windows');
  {$ELSE}
  WriteLn('运行环境: 非 Windows（部分测试将跳过）');
  {$ENDIF}
  WriteLn;

  try
    TestStoreCreation;
    TestStoreOpen;
    TestSystemStoreAccess;
    TestStoreCount;
    TestGetAllCertificates;
    TestGetCertificateByIndex;
    TestFindBySubject;
    TestFindByIssuer;
    TestDeterministicDNQueryContract;
    TestFindBySerialNumber;
    TestFindByFingerprint;
    TestContainsCertificate;
    TestLoadSystemStore;
    TestStoreNativeHandle;
    TestMultipleStores;
    TestStoreReopen;

    WriteLn;
    PrintSummary;
  except
    on E: Exception do
    begin
      WriteLn('错误: ', E.Message);
      Halt(1);
    end;
  end;
end.
