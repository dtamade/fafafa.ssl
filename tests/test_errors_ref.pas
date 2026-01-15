{**
 * 诊断测试: 引用 fafafa.ssl.errors
 *}

program test_errors_ref;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.errors;

begin
  WriteLn('Step 1: Modules imported');
  Flush(Output);
  
  WriteLn('Step 2: Calling RaiseInvalidParameter...');
  Flush(Output);
  
  try
    RaiseInvalidParameter('test');
  except
    on E: Exception do
      WriteLn('Step 3: Exception caught: ', E.Message);
  end;
  Flush(Output);
  
  WriteLn('SUCCESS');
end.
