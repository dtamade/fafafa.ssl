{**
 * 诊断测试: 引用 TSecurityLog
 *}

program test_logging_ref;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.logging;

begin
  WriteLn('Step 1: Modules imported');
  Flush(Output);
  
  WriteLn('Step 2: Calling TSecurityLog.Info...');
  Flush(Output);
  
  TSecurityLog.Info('Test', 'Hello');
  
  WriteLn('Step 3: TSecurityLog.Info completed');
  Flush(Output);
  
  WriteLn('SUCCESS');
end.
