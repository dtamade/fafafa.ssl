{**
 * 诊断测试 Step 3: 导入 fafafa.ssl.winssl.lib
 *}

program test_winssl_step3;

{$mode ObjFPC}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.lib;

begin
  WriteLn('Step 3: Program started with fafafa.ssl.winssl.lib');
  WriteLn('SUCCESS');
  Halt(0);
end.
