program example_result_type;

{$mode objfpc}{$H+}

{**
 * Result 类型使用示例
 *
 * 展示如何使用 Rust 风格的 Result<T, E> 类型进行优雅的错误处理
 *
 * 演示内容:
 * - 基本的 Ok/Err 创建
 * - Unwrap/UnwrapOr/Expect 方法
 * - IsOkAnd/Inspect 方法
 * - Try* 方法模式
 * - 实际加密操作中的使用
 *}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.crypto.utils,
  fafafa.ssl.exceptions;

type
  { Helper class for Example 3 callbacks }
  TExampleHelper = class
  public
    function IsNonEmpty(const AData: TBytes): Boolean;
    procedure LogData(const AData: TBytes);
  end;

function TExampleHelper.IsNonEmpty(const AData: TBytes): Boolean;
begin
  Result := Length(AData) > 0;
end;

procedure TExampleHelper.LogData(const AData: TBytes);
begin
  WriteLn('  Inspecting data, length: ', Length(AData));
end;

procedure Example1_BasicUsage;
var
  LResult: TSSLDataResult;
  LData: TBytes;
begin
  WriteLn('=== Example 1: 基本用法 ===');

  // 创建成功结果
  SetLength(LData, 3);
  LData[0] := 1; LData[1] := 2; LData[2] := 3;
  LResult := TSSLDataResult.Ok(LData);

  WriteLn('IsOk: ', LResult.IsOk);
  WriteLn('IsErr: ', LResult.IsErr);

  // 创建错误结果
  LResult := TSSLDataResult.Err(sslErrInvalidParam, 'Invalid parameter');
  WriteLn('Error Code: ', Ord(LResult.ErrorCode));
  WriteLn('Error Message: ', LResult.ErrorMessage);
  WriteLn;
end;

procedure Example2_UnwrapMethods;
var
  LResult: TSSLDataResult;
  LData, LDefault: TBytes;
begin
  WriteLn('=== Example 2: Unwrap 方法 ===');

  // Unwrap - 成功时返回值
  SetLength(LData, 2);
  LData[0] := 42; LData[1] := 99;
  LResult := TSSLDataResult.Ok(LData);
  LData := LResult.Unwrap;
  WriteLn('Unwrap 成功: ', LData[0], ', ', LData[1]);

  // UnwrapOr - 失败时返回默认值
  LResult := TSSLDataResult.Err(sslErrGeneral, 'Some error');
  SetLength(LDefault, 1);
  LDefault[0] := 0;
  LData := LResult.UnwrapOr(LDefault);
  WriteLn('UnwrapOr 返回默认值: ', LData[0]);

  // Expect - 失败时抛出自定义异常
  try
    LResult := TSSLDataResult.Err(sslErrMemory, 'Out of memory');
    LData := LResult.Expect('Expected valid data');
  except
    on E: Exception do
      WriteLn('Expect 异常: ', E.Message);
  end;
  WriteLn;
end;

procedure Example3_IsOkAndInspect;
var
  LResult: TSSLDataResult;
  LData: TBytes;
  LHelper: TExampleHelper;
begin
  WriteLn('=== Example 3: IsOkAnd 和 Inspect ===');

  LHelper := TExampleHelper.Create;
  try
    // IsOkAnd - 检查是否成功且满足条件
    SetLength(LData, 5);
    LResult := TSSLDataResult.Ok(LData);
    WriteLn('IsOkAnd(非空): ', LResult.IsOkAnd(@LHelper.IsNonEmpty));

    SetLength(LData, 0);
    LResult := TSSLDataResult.Ok(LData);
    WriteLn('IsOkAnd(空数组): ', LResult.IsOkAnd(@LHelper.IsNonEmpty));

    // Inspect - 检查但不消耗值
    SetLength(LData, 10);
    LResult := TSSLDataResult.Ok(LData);
    LResult := LResult.Inspect(@LHelper.LogData);
    WriteLn('Inspect 后仍可使用: Length = ', Length(LResult.Data));
  finally
    LHelper.Free;
  end;
  WriteLn;
end;

procedure Example4_TryPattern;
var
  LData, LKey, LIV, LCiphertext: TBytes;
begin
  WriteLn('=== Example 4: Try* 模式 ===');

  // 生成测试数据
  LData := TCryptoUtils.SecureRandom(32);
  LKey := TCryptoUtils.GenerateKey(256);
  LIV := TCryptoUtils.GenerateIV(16);

  // Try 模式 - 不抛异常
  if TCryptoUtils.TryAES_CBC_Encrypt(LData, LKey, LIV, LCiphertext) then
  begin
    WriteLn('✓ AES-CBC 加密成功, 长度: ', Length(LCiphertext));

    // 解密验证
    if TCryptoUtils.TryAES_CBC_Decrypt(LCiphertext, LKey, LIV, LData) then
      WriteLn('✓ AES-CBC 解密成功, 长度: ', Length(LData))
    else
      WriteLn('✗ AES-CBC 解密失败');
  end
  else
    WriteLn('✗ AES-CBC 加密失败');

  WriteLn;
end;

procedure Example5_ErrorHandlingComparison;
var
  LData, LKey, LIV: TBytes;
  LHash: TBytes;
begin
  WriteLn('=== Example 5: 错误处理对比 ===');

  LData := TCryptoUtils.SecureRandom(16);

  // 方式 1: 异常处理（传统）
  WriteLn('方式 1: 异常处理');
  try
    LHash := TCryptoUtils.SHA256(LData);
    WriteLn('  ✓ SHA256 成功, 长度: ', Length(LHash));
  except
    on E: Exception do
      WriteLn('  ✗ 错误: ', E.Message);
  end;

  // 方式 2: Try 方法（Rust 风格）
  WriteLn('方式 2: Try 方法');
  if TCryptoUtils.TrySHA256(LData, LHash) then
    WriteLn('  ✓ SHA256 成功, 长度: ', Length(LHash))
  else
    WriteLn('  ✗ SHA256 失败');

  // 方式 3: 错误密钥测试
  WriteLn('方式 3: 错误密钥（应该失败）');
  SetLength(LKey, 10);  // 错误的密钥长度
  SetLength(LIV, 16);

  if not TCryptoUtils.TryAES_CBC_Encrypt(LData, LKey, LIV, LHash) then
    WriteLn('  ✓ 正确检测到错误（密钥长度无效）');

  WriteLn;
end;

procedure Example6_StringResult;
var
  LResult: TSSLStringResult;
  LValue: string;
begin
  WriteLn('=== Example 6: TSSLStringResult ===');

  // 成功结果
  LResult := TSSLStringResult.Ok('Hello, World!');
  WriteLn('IsOk: ', LResult.IsOk);
  WriteLn('Value: ', LResult.Unwrap);

  // 错误结果
  LResult := TSSLStringResult.Err(sslErrGeneral, 'Something went wrong');
  LValue := LResult.UnwrapOr('Default Value');
  WriteLn('UnwrapOr: ', LValue);

  // UnwrapErr
  WriteLn('UnwrapErr: ', Ord(LResult.UnwrapErr));
  WriteLn;
end;

procedure Example7_OperationResult;
var
  LResult: TSSLOperationResult;
begin
  WriteLn('=== Example 7: TSSLOperationResult ===');

  // 成功操作
  LResult := TSSLOperationResult.Ok;
  WriteLn('操作成功: ', LResult.IsOk);

  // 失败操作
  LResult := TSSLOperationResult.Err(sslErrTimeout, 'Connection timeout');
  WriteLn('操作失败: ', LResult.IsErr);
  WriteLn('错误信息: ', LResult.ErrorMessage);

  // Expect
  try
    LResult.Expect('Operation must succeed');
  except
    on E: Exception do
      WriteLn('捕获异常: ', E.Message);
  end;

  WriteLn;
end;

begin
  WriteLn('╔════════════════════════════════════════════════════════════╗');
  WriteLn('║   fafafa.ssl Result 类型示例 (Rust 风格错误处理)          ║');
  WriteLn('╚════════════════════════════════════════════════════════════╝');
  WriteLn;

  try
    Example1_BasicUsage;
    Example2_UnwrapMethods;
    Example3_IsOkAndInspect;
    Example4_TryPattern;
    Example5_ErrorHandlingComparison;
    Example6_StringResult;
    Example7_OperationResult;

    WriteLn('╔════════════════════════════════════════════════════════════╗');
    WriteLn('║   所有示例执行完成！                                        ║');
    WriteLn('╚════════════════════════════════════════════════════════════╝');
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('致命错误: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;

  {$IFDEF UNIX}
  WriteLn;
  WriteLn('按回车键退出...');
  ReadLn;
  {$ENDIF}
end.
