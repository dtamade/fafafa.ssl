{**
 * 诊断测试: 只导入 certificate
 *}

program test_certificate_only;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.winssl.certificate;

begin
  WriteLn('SUCCESS: certificate imported');
end.
