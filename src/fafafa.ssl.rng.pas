{**
 * Unit: fafafa.ssl.rng
 * Purpose: 平台原生安全随机数生成器 - 零 OpenSSL 依赖
 *
 * 此单元提供跨平台的安全随机数生成，不依赖 OpenSSL。
 * - Windows: 使用 BCryptGenRandom (Vista+)
 * - Unix/Linux: 使用 /dev/urandom
 * - macOS: 使用 /dev/urandom
 *
 * 设计目标:
 * - 为 WinSSL 后端提供真正的"零依赖"RNG
 * - 线程安全
 * - 加密安全级别的随机数
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-10
 *}

unit fafafa.ssl.rng;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  SysUtils;

type
  {**
   * 平台原生安全随机数生成器
   *
   * 提供不依赖 OpenSSL 的加密安全随机数生成。
   * 所有方法都是类方法（静态），无需创建实例。
   *
   * 线程安全性: 所有方法都是线程安全的
   *}
  TNativeRandom = class
  public
    {**
     * 生成安全随机字节
     *
     * @param ALength 随机字节数
     * @return 随机字节数组
     * @raises Exception 随机数生成失败
     *}
    class function GetBytes(ALength: Integer): TBytes; static;

    {**
     * 生成安全随机字节（Try 版本，不抛异常）
     *
     * @param ALength 随机字节数
     * @param AResult 输出随机字节数组
     * @return 成功返回 True
     *}
    class function TryGetBytes(ALength: Integer; out AResult: TBytes): Boolean; static;

    {**
     * 填充缓冲区为随机字节
     *
     * @param ABuffer 缓冲区指针
     * @param ASize 缓冲区大小
     * @raises Exception 随机数生成失败
     *}
    class procedure FillBuffer(ABuffer: PByte; ASize: Integer); static;

    {**
     * 填充缓冲区为随机字节（Try 版本）
     *
     * @param ABuffer 缓冲区指针
     * @param ASize 缓冲区大小
     * @return 成功返回 True
     *}
    class function TryFillBuffer(ABuffer: PByte; ASize: Integer): Boolean; static;

    {**
     * 生成随机 32 位无符号整数
     *}
    class function GetUInt32: Cardinal; static;

    {**
     * 生成随机 64 位无符号整数
     *}
    class function GetUInt64: QWord; static;

    {**
     * 生成指定范围内的随机整数 [0, AMax)
     *
     * @param AMax 上限（不包含）
     * @return 随机整数
     *}
    class function GetRange(AMax: Cardinal): Cardinal; static;
  end;

implementation

{$IFDEF WINDOWS}
uses
  Windows;

const
  BCRYPT_USE_SYSTEM_PREFERRED_RNG = $00000002;

// BCryptGenRandom - Windows Vista+ 安全随机数生成器
function BCryptGenRandom(
  hAlgorithm: THandle;
  pbBuffer: PByte;
  cbBuffer: ULONG;
  dwFlags: ULONG
): LONG; stdcall; external 'bcrypt.dll' name 'BCryptGenRandom';
{$ENDIF}

{ TNativeRandom }

class procedure TNativeRandom.FillBuffer(ABuffer: PByte; ASize: Integer);
{$IFDEF UNIX}
var
  LFile: File;
  LBytesRead: Integer;
{$ENDIF}
{$IFDEF WINDOWS}
var
  LStatus: LONG;
{$ENDIF}
begin
  if (ABuffer = nil) or (ASize <= 0) then
    raise Exception.Create('Invalid buffer or size for random generation');

  {$IFDEF UNIX}
  LBytesRead := 0;
  AssignFile(LFile, '/dev/urandom');
  try
    Reset(LFile, 1);
    try
      BlockRead(LFile, ABuffer^, ASize, LBytesRead);
      if LBytesRead <> ASize then
        raise Exception.CreateFmt(
          'Insufficient random bytes from /dev/urandom: got %d, expected %d',
          [LBytesRead, ASize]);
    finally
      CloseFile(LFile);
    end;
  except
    on E: Exception do
      raise Exception.Create('Native random source failed: ' + E.Message);
  end;
  {$ENDIF}

  {$IFDEF WINDOWS}
  LStatus := BCryptGenRandom(0, ABuffer, ULONG(ASize), BCRYPT_USE_SYSTEM_PREFERRED_RNG);
  if LStatus <> 0 then
    raise Exception.CreateFmt(
      'BCryptGenRandom failed with NTSTATUS: 0x%x', [LStatus]);
  {$ENDIF}
end;

class function TNativeRandom.TryFillBuffer(ABuffer: PByte; ASize: Integer): Boolean;
begin
  try
    FillBuffer(ABuffer, ASize);
    Result := True;
  except
    Result := False;
  end;
end;

class function TNativeRandom.GetBytes(ALength: Integer): TBytes;
begin
  if ALength <= 0 then
    raise Exception.Create('Random length must be positive');

  SetLength(Result, ALength);
  FillBuffer(@Result[0], ALength);
end;

class function TNativeRandom.TryGetBytes(ALength: Integer; out AResult: TBytes): Boolean;
begin
  try
    AResult := GetBytes(ALength);
    Result := True;
  except
    SetLength(AResult, 0);
    Result := False;
  end;
end;

class function TNativeRandom.GetUInt32: Cardinal;
var
  LBytes: TBytes;
begin
  LBytes := GetBytes(4);
  Result := PCardinal(@LBytes[0])^;
end;

class function TNativeRandom.GetUInt64: QWord;
var
  LBytes: TBytes;
begin
  LBytes := GetBytes(8);
  Result := PQWord(@LBytes[0])^;
end;

class function TNativeRandom.GetRange(AMax: Cardinal): Cardinal;
var
  LThreshold: Cardinal;
begin
  if AMax = 0 then
    raise Exception.Create('Range maximum must be positive');

  if AMax = 1 then
    Exit(0);

  // 使用拒绝采样避免模偏差
  // threshold = (2^32 - AMax) mod AMax
  LThreshold := (High(Cardinal) - AMax + 1) mod AMax;

  repeat
    Result := GetUInt32;
  until Result >= LThreshold;

  Result := Result mod AMax;
end;

end.
