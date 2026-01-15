{**
 * 诊断测试: 只导入 WinSSL lib 模块
 *}

program test_winssl_import_only;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.winssl.lib;

begin
  WriteLn('SUCCESS: fafafa.ssl.winssl.lib imported');
end.
