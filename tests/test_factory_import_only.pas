{**
 * 诊断测试: 只导入 factory 模块
 *}

program test_factory_import_only;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.factory;

begin
  WriteLn('SUCCESS: fafafa.ssl.factory imported');
end.
