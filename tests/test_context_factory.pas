{**
 * 诊断测试: 同时导入 context 和 factory
 *}

program test_context_factory;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.context,
  fafafa.ssl.winssl.certificate,
  fafafa.ssl.winssl.certstore;

begin
  WriteLn('SUCCESS: context and factory imported');
end.
