{**
 * 诊断测试: 只导入 WinSSL context 模块
 *}

program test_context_import;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.winssl.context;

begin
  WriteLn('SUCCESS: fafafa.ssl.winssl.context imported');
end.
