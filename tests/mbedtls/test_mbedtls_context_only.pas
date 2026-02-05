program test_mbedtls_context_only;

{$mode ObjFPC}{$H+}

{
  最小化测试:只测试 Context 的创建和释放
}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.mbedtls.lib,
  fafafa.ssl.mbedtls.context;

var
  LLib1, LLib2: TMbedTLSLibrary;
  LCtx: TMbedTLSContext;

begin
  WriteLn('Test 1: Create and Free Library only');
  LLib1 := TMbedTLSLibrary.Create;
  LLib1.Initialize;
  WriteLn('  Library initialized');
  LLib1.Finalize;
  WriteLn('  Library finalized');
  LLib1.Free;
  WriteLn('  Library freed');
  WriteLn('  ✅ Test 1 OK');
  WriteLn;

  WriteLn('Test 2: Create Context, Free before Finalize');
  LLib2 := TMbedTLSLibrary.Create;
  LLib2.Initialize;
  WriteLn('  Library initialized');

  LCtx := TMbedTLSContext.Create(LLib2, sslCtxClient);
  WriteLn('  Context created');

  WriteLn('  Freeing context...');
  LCtx.Free;
  WriteLn('  ✅ Context freed');

  LLib2.Finalize;
  WriteLn('  Library finalized');
  WriteLn('  About to free library...');
  LLib2.Free;
  WriteLn('  Library freed');
  WriteLn('  ✅ Test 2 OK');
  WriteLn;

  WriteLn('About to exit program...');
  WriteLn('🎉 All tests passed!');
end.
