{**
 * 诊断测试 Step 3b: 导入 fafafa.ssl.winssl.api
 *}

program test_winssl_step3b;

{$mode ObjFPC}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.winssl.base,
  fafafa.ssl.winssl.api;

begin
  WriteLn('Step 3b: Program started with fafafa.ssl.winssl.api');
  WriteLn('SUCCESS');
  Halt(0);
end.
