{**
 * 诊断测试: 同时导入 factory 和所有 WinSSL 模块
 *}

program test_factory_all;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.context,
  fafafa.ssl.winssl.connection,
  fafafa.ssl.winssl.certificate,
  fafafa.ssl.winssl.certstore;

begin
  WriteLn('SUCCESS: factory and all WinSSL modules imported');
end.
