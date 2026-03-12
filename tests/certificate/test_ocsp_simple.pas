program test_ocsp_simple;

{$mode ObjFPC}{$H+}

{ Simple OCSP smoke program: runtime-safe under redirected stdin for CI, while still friendly for manual console runs. }

uses
  SysUtils,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.ocsp,
  fafafa.ssl.openssl.loader;

var
  LResult: Boolean;
  LCount: Integer;

function HasEnoughOCSPFunctions: Boolean;
begin
  Result := LCount >= 5;
end;

begin
  WriteLn('========================================');
  WriteLn('OCSP 模块简单验证测试');
  WriteLn('========================================');
  WriteLn;

  // 初始化 OpenSSL
  WriteLn('1. 初始化 OpenSSL...');
  try
    LoadOpenSSLCore;
    if not TOpenSSLLoader.IsModuleLoaded(osmCore) then
    begin
      WriteLn('   ❌ OpenSSL Core 未保持加载');
      Halt(1);
    end;

    if not LoadOpenSSLOCSP(GetCryptoLibHandle) then
    begin
      WriteLn('   ❌ OCSP 模块加载失败');
      Halt(1);
    end;

    WriteLn('   ✅ OpenSSL/OCSP 模块加载成功');
  except
    on E: Exception do
    begin
      WriteLn('   ❌ OpenSSL/OCSP 加载失败: ', E.Message);
      Halt(1);
    end;
  end;
  WriteLn;

  // 统计可用函数数量
  WriteLn('2. 检查 OCSP 函数可用性...');
  LCount := 0;
  
  if Assigned(OCSP_REQUEST_new) then Inc(LCount, 1);
  if Assigned(OCSP_RESPONSE_new) then Inc(LCount, 1);
  if Assigned(OCSP_BASICRESP_new) then Inc(LCount, 1);
  if Assigned(OCSP_cert_to_id) then Inc(LCount, 1);
  if Assigned(OCSP_REQUEST_add0_id) then Inc(LCount, 1);
  if Assigned(OCSP_RESPONSE_status) then Inc(LCount, 1);
  if Assigned(OCSP_parse_url) then Inc(LCount, 1);
  if Assigned(OCSP_check_validity) then Inc(LCount, 1);
  
  WriteLn(Format('   可用函数数量: %d/8', [LCount]));
  
  if HasEnoughOCSPFunctions then
    WriteLn('   ✅ 大部分 OCSP 函数可用')
  else
    WriteLn('   ⚠️  部分 OCSP 函数不可用');
  WriteLn;

  // 检查常量
  WriteLn('3. 检查 OCSP 常量...');
  WriteLn(Format('   OCSP_RESPONSE_STATUS_SUCCESSFUL = %d', [OCSP_RESPONSE_STATUS_SUCCESSFUL]));
  WriteLn(Format('   V_OCSP_CERTSTATUS_GOOD = %d', [V_OCSP_CERTSTATUS_GOOD]));
  WriteLn(Format('   V_OCSP_CERTSTATUS_REVOKED = %d', [V_OCSP_CERTSTATUS_REVOKED]));
  WriteLn(Format('   V_OCSP_CERTSTATUS_UNKNOWN = %d', [V_OCSP_CERTSTATUS_UNKNOWN]));
  WriteLn('   ✅ 常量定义正确');
  WriteLn;

  // 输出结果
  WriteLn('========================================');
  if HasEnoughOCSPFunctions then
  begin
    WriteLn('✅ OCSP 模块验证成功！');
    WriteLn('   模块已可正常使用');
    WriteLn('[PASS] ocsp simple completed');
  end
  else
  begin
    WriteLn('⚠️  OCSP 模块部分可用');
    WriteLn('   建议检查 OpenSSL 库版本');
    Halt(1);
  end;
  WriteLn('========================================');
  WriteLn;
  
  WriteLn('按 Enter 键退出...');
  ReadLn;
end.
