{**
 * 诊断测试 Step 3a: 只导入 fafafa.ssl.winssl.base
 *}

program test_winssl_step3a;

{$mode ObjFPC}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.winssl.base;

begin
  WriteLn('Step 3a: Program started with fafafa.ssl.winssl.base');
  WriteLn('SUCCESS');
  Halt(0);
end.
