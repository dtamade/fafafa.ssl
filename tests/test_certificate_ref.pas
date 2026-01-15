{**
 * 诊断测试: 引用 TWinSSLCertificate
 *}

program test_certificate_ref;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.winssl.certificate;

var
  LClass: TClass;
begin
  WriteLn('Step 1: Modules imported');
  Flush(Output);
  
  WriteLn('Step 2: Getting TWinSSLCertificate class reference...');
  Flush(Output);
  
  LClass := TWinSSLCertificate;
  
  WriteLn('Step 3: Class reference obtained: ', LClass.ClassName);
  Flush(Output);
  
  WriteLn('SUCCESS');
end.
