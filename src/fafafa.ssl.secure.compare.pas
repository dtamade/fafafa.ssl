{**
 * Unit: fafafa.ssl.secure.compare
 * Purpose: 常量时间比较操作（防时序攻击）
 *
 * 此模块提供不依赖任何 SSL 后端的安全比较功能。
 * 用于 WinSSL 等不需要 OpenSSL 的场景。
 *
 * 特性:
 * - 常量时间字符串比较（防止时序攻击）
 * - 常量时间字节数组比较
 * - 无外部依赖
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-06
 *}

unit fafafa.ssl.secure.compare;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  SysUtils;

{**
 * 常量时间字节数组比较
 *
 * 无论比较结果如何，执行时间都相同，防止时序攻击。
 *
 * @param A 第一个字节数组
 * @param B 第二个字节数组
 * @return True 如果两个数组相等，False 否则
 *}
function SecureCompare(const A, B: TBytes): Boolean;

{**
 * 常量时间字符串比较
 *
 * 无论比较结果如何，执行时间都相同，防止时序攻击。
 *
 * @param A 第一个字符串
 * @param B 第二个字符串
 * @return True 如果两个字符串相等，False 否则
 *}
function SecureCompareStrings(const A, B: string): Boolean;

{**
 * 常量时间内存比较
 *
 * @param P1 第一个内存块指针
 * @param P2 第二个内存块指针
 * @param Size 比较的字节数
 * @return True 如果两个内存块相等，False 否则
 *}
function SecureCompareMemory(P1, P2: Pointer; Size: Integer): Boolean;

implementation

function SecureCompareMemory(P1, P2: Pointer; Size: Integer): Boolean;
var
  I: Integer;
  Diff: Byte;
  B1, B2: PByte;
begin
  if Size <= 0 then
  begin
    Result := True;
    Exit;
  end;

  if (P1 = nil) or (P2 = nil) then
  begin
    Result := (P1 = P2);
    Exit;
  end;

  Diff := 0;
  B1 := PByte(P1);
  B2 := PByte(P2);

  // 常量时间比较：始终遍历所有字节
  for I := 0 to Size - 1 do
    Diff := Diff or (B1[I] xor B2[I]);

  Result := (Diff = 0);
end;

function SecureCompare(const A, B: TBytes): Boolean;
var
  I: Integer;
  Diff: Byte;
  LenA, LenB, MaxLen: Integer;
begin
  LenA := Length(A);
  LenB := Length(B);

  // 长度不同时仍然执行完整比较以保持常量时间
  if LenA > LenB then
    MaxLen := LenA
  else
    MaxLen := LenB;

  if MaxLen = 0 then
  begin
    Result := True;
    Exit;
  end;

  Diff := 0;

  // 长度差异也计入
  if LenA <> LenB then
    Diff := 1;

  // 常量时间比较
  for I := 0 to MaxLen - 1 do
  begin
    if (I < LenA) and (I < LenB) then
      Diff := Diff or (A[I] xor B[I])
    else
      Diff := Diff or 1;  // 长度不同
  end;

  Result := (Diff = 0);
end;

function SecureCompareStrings(const A, B: string): Boolean;
var
  I: Integer;
  Diff: Byte;
  LenA, LenB, MaxLen: Integer;
begin
  LenA := Length(A);
  LenB := Length(B);

  // 长度不同时仍然执行完整比较以保持常量时间
  if LenA > LenB then
    MaxLen := LenA
  else
    MaxLen := LenB;

  if MaxLen = 0 then
  begin
    Result := True;
    Exit;
  end;

  Diff := 0;

  // 长度差异也计入
  if LenA <> LenB then
    Diff := 1;

  // 常量时间比较
  for I := 1 to MaxLen do
  begin
    if (I <= LenA) and (I <= LenB) then
      Diff := Diff or (Ord(A[I]) xor Ord(B[I]))
    else
      Diff := Diff or 1;  // 长度不同
  end;

  Result := (Diff = 0);
end;

end.
