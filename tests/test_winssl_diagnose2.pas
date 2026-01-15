{**
 * 诊断测试: 逐步检查 WinSSL 初始化
 *}

program test_winssl_diagnose2;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.autoregister;

begin
  WriteLn('Step 1: Program started');
  WriteLn('Step 2: fafafa.ssl.winssl.autoregister loaded');
  WriteLn('SUCCESS');
end.
