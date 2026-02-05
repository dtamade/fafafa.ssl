{
  测试统一原生句柄辅助单元

  验证 fafafa.ssl.native_handle 的所有功能
}

program test_native_handle_unified;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.native_handle,
  fafafa.ssl.exceptions;

type
  PSSL_CTX = Pointer;  // OpenSSL 类型（简化）
  PSSL = Pointer;

procedure Test(const AName: string; AResult: Boolean);
begin
  if AResult then
    WriteLn('[PASS] ', AName)
  else
    WriteLn('[FAIL] ', AName);
end;

procedure TestBasicFunctions;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  Handle: Pointer;
  BackendType: TSSLLibraryType;
begin
  WriteLn('=== 测试基础函数 ===');

  // 创建 OpenSSL 上下文
  try
    Lib := TSSLFactory.GetLibrary(sslOpenSSL);
  except
    WriteLn('[SKIP] Factory not available');
    Exit;
  end;
  if not Lib.Initialize then
  begin
    WriteLn('[SKIP] OpenSSL not available');
    Exit;
  end;

  Ctx := Lib.CreateContext(sslCtxClient);

  // 测试 IsNativeHandleAvailable
  Test('IsNativeHandleAvailable', IsNativeHandleAvailable(Ctx));

  // 测试 GetBackendType
  BackendType := GetBackendType(Ctx);
  Test('GetBackendType returns sslOpenSSL',
       BackendType = sslOpenSSL);

  // 测试 GetNativeHandle
  try
    Handle := GetNativeHandle(Ctx);
    Test('GetNativeHandle returns non-nil', Handle <> nil);
  except
    on E: Exception do
      WriteLn('[FAIL] GetNativeHandle exception: ', E.Message);
  end;

  // 测试 TryGetNativeHandle
  if TryGetNativeHandle(Ctx, Handle) then
    Test('TryGetNativeHandle success', Handle <> nil)
  else
    WriteLn('[FAIL] TryGetNativeHandle failed');

  // 测试 IsNativeHandleValid
  Test('IsNativeHandleValid', IsNativeHandleValid(Ctx));

  WriteLn;
end;

procedure TestSafeFunction;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  Handle: Pointer;
begin
  WriteLn('=== 测试安全函数 ===');

  Lib := TSSLFactory.GetLibrary(sslOpenSSL);
  if not Lib.Initialize then
  begin
    WriteLn('[SKIP] OpenSSL not available');
    Exit;
  end;

  Ctx := Lib.CreateContext(sslCtxClient);

  // 测试 GetNativeHandleSafe 带上下文
  try
    Handle := GetNativeHandleSafe(Ctx, 'TestSafeFunction');
    Test('GetNativeHandleSafe with context', Handle <> nil);
  except
    on E: Exception do
      WriteLn('[FAIL] GetNativeHandleSafe exception: ', E.Message);
  end;

  // 测试 GetNativeHandleSafe 无上下文
  try
    Handle := GetNativeHandleSafe(Ctx);
    Test('GetNativeHandleSafe without context', Handle <> nil);
  except
    on E: Exception do
      WriteLn('[FAIL] GetNativeHandleSafe exception: ', E.Message);
  end;

  WriteLn;
end;

procedure TestGenericFunctions;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  SSL_CTX: PSSL_CTX;
begin
  WriteLn('=== 测试泛型函数 ===');

  Lib := TSSLFactory.GetLibrary(sslOpenSSL);
  if not Lib.Initialize then
  begin
    WriteLn('[SKIP] OpenSSL not available');
    Exit;
  end;

  Ctx := Lib.CreateContext(sslCtxClient);

  // 测试 GetNativeHandleAs
  try
    SSL_CTX := specialize GetNativeHandleAs<PSSL_CTX>(Ctx);
    Test('GetNativeHandleAs<PSSL_CTX>', SSL_CTX <> nil);
  except
    on E: Exception do
      WriteLn('[FAIL] GetNativeHandleAs exception: ', E.Message);
  end;

  // 测试 GetNativeHandleAsSafe
  try
    SSL_CTX := specialize GetNativeHandleAsSafe<PSSL_CTX>(Ctx, 'TestGenericFunctions');
    Test('GetNativeHandleAsSafe<PSSL_CTX>', SSL_CTX <> nil);
  except
    on E: Exception do
      WriteLn('[FAIL] GetNativeHandleAsSafe exception: ', E.Message);
  end;

  // 测试 TryGetNativeHandleAs
  if specialize TryGetNativeHandleAs<PSSL_CTX>(Ctx, SSL_CTX) then
    Test('TryGetNativeHandleAs<PSSL_CTX>', SSL_CTX <> nil)
  else
    WriteLn('[FAIL] TryGetNativeHandleAs failed');

  WriteLn;
end;

procedure TestErrorMessages;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  DummyIntf: IInterface;
  Handle: Pointer;
begin
  WriteLn('=== 测试错误消息 ===');

  // 创建一个不支持 ISSLNativeHandleAccess 的对象
  // （这里使用一个简单的接口作为示例）
  DummyIntf := nil;  // nil 接口

  // 测试错误消息 - GetNativeHandle
  try
    Handle := GetNativeHandle(DummyIntf);
    WriteLn('[FAIL] Should throw exception for nil interface');
  except
    on E: ESSLException do
    begin
      Test('GetNativeHandle throws exception for unsupported object', True);
      WriteLn('  Error message preview: ',
              Copy(E.Message, 1, 50), '...');
    end;
    on E: Exception do
      WriteLn('[FAIL] Unexpected exception: ', E.ClassName);
  end;

  // 测试错误消息 - GetNativeHandleSafe 带上下文
  try
    Handle := GetNativeHandleSafe(DummyIntf, 'TestErrorMessages.Line123');
    WriteLn('[FAIL] Should throw exception');
  except
    on E: ESSLException do
    begin
      Test('GetNativeHandleSafe throws exception with context', True);
      Test('Error message contains context',
           Pos('TestErrorMessages.Line123', E.Message) > 0);
      WriteLn('  Full error message:');
      WriteLn('  ', E.Message);
    end;
  end;

  WriteLn;
end;

procedure TestUsageExample;
var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  SSL_CTX: PSSL_CTX;
begin
  WriteLn('=== 使用示例 ===');

  Lib := TSSLFactory.GetLibrary(sslOpenSSL);
  if not Lib.Initialize then
  begin
    WriteLn('[SKIP] OpenSSL not available');
    Exit;
  end;

  Ctx := Lib.CreateContext(sslCtxClient);

  // 示例1: 简洁方式
  SSL_CTX := PSSL_CTX(GetNativeHandle(Ctx));
  WriteLn('[示例1] 简洁方式: Handle = ', PtrUInt(SSL_CTX));

  // 示例2: 类型安全方式
  SSL_CTX := specialize GetNativeHandleAs<PSSL_CTX>(Ctx);
  WriteLn('[示例2] 类型安全方式: Handle = ', PtrUInt(SSL_CTX));

  // 示例3: 最安全方式
  SSL_CTX := specialize GetNativeHandleAsSafe<PSSL_CTX>(Ctx, 'TestUsageExample');
  WriteLn('[示例3] 最安全方式: Handle = ', PtrUInt(SSL_CTX));

  // 示例4: 检查可用性
  if IsNativeHandleAvailable(Ctx) then
  begin
    WriteLn('[示例4] Native handle is available');
    WriteLn('  Backend type: ', GetBackendType(Ctx));
  end;

  WriteLn;
end;

begin
  WriteLn('fafafa.ssl - 统一原生句柄辅助单元测试');
  WriteLn('==========================================');
  WriteLn;

  try
    TestBasicFunctions;
    TestSafeFunction;
    TestGenericFunctions;
    TestErrorMessages;
    TestUsageExample;

    WriteLn('==========================================');
    WriteLn('测试完成！');
  except
    on E: Exception do
    begin
      WriteLn('测试失败: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
