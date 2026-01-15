{**
 * 测试导入 winssl.context 和 factory
 *}

program test_winssl_context_import;

{$mode ObjFPC}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.context;

begin
  WriteLn('All modules loaded successfully');
  WriteLn('SUCCESS');
  Halt(0);
end.
