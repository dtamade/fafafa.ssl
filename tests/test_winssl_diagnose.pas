{**
 * 诊断测试 - 逐步添加导入以找出崩溃原因
 *}

program test_winssl_diagnose;

{$mode ObjFPC}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils,
  fafafa.ssl.base,       // Step 1: 基础类型
  fafafa.ssl.factory,    // Step 2: 工厂类
  fafafa.ssl.winssl.base,  // Step 3a: WinSSL 基础
  fafafa.ssl.winssl.api,   // Step 3b: WinSSL API
  fafafa.ssl.winssl.utils, // Step 3c: WinSSL Utils
  fafafa.ssl.winssl.certificate, // Step 4: WinSSL Certificate
  fafafa.ssl.winssl.connection;  // Step 5: WinSSL Connection

begin
  WriteLn('Step 1: fafafa.ssl.base loaded OK');
  WriteLn('Step 2: fafafa.ssl.factory loaded OK');
  WriteLn('Step 3: fafafa.ssl.winssl.base/api/utils loaded OK');
  WriteLn('Step 4: fafafa.ssl.winssl.certificate loaded OK');
  WriteLn('Step 5: fafafa.ssl.winssl.connection loaded OK');
  WriteLn('SUCCESS');
  Halt(0);
end.
