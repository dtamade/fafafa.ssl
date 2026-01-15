{**
 * 诊断测试: 创建 TWinSSLContext 实例
 *}

program test_create_context;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.winssl.context;

var
  LCtx: ISSLContext;
begin
  WriteLn('Step 1: Modules imported');
  Flush(Output);
  
  WriteLn('Step 2: Creating TWinSSLContext...');
  Flush(Output);
  
  try
    LCtx := TWinSSLContext.Create(nil, sslCtxClient);
    WriteLn('Step 3: TWinSSLContext created');
  except
    on E: Exception do
      WriteLn('Step 3: Create FAILED: ', E.Message);
  end;
  Flush(Output);
  
  WriteLn('SUCCESS');
end.
