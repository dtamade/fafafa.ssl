{**
 * Unit: fafafa.ssl.base64
 * Purpose: 纯 Pascal 实现的 Base64 编码/解码
 *
 * 此模块提供不依赖任何 SSL 后端的 Base64 编码功能。
 * 用于 WinSSL 等不需要 OpenSSL 的场景。
 *
 * 注意: 如果需要高性能 Base64 操作，请使用 fafafa.ssl.encoding
 * （需要 OpenSSL）。此模块优先考虑独立性而非性能。
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-06
 *}

unit fafafa.ssl.base64;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  SysUtils, Classes;

type
  {**
   * 纯 Pascal Base64 编码工具类
   *
   * 所有方法都是类方法（静态），无需创建实例。
   * 线程安全性: 所有方法都是线程安全的（无共享状态）
   *}
  TBase64Utils = class
  public
    {**
     * Base64 编码（TBytes 输入）
     *
     * @param AInput 输入字节数组
     * @return Base64 编码字符串（无换行符）
     *}
    class function Encode(const AInput: TBytes): string; overload; static;

    {**
     * Base64 编码（string 输入）
     * 字符串使用 UTF-8 编码
     *}
    class function Encode(const AInput: string): string; overload; static;

    {**
     * Base64 解码
     *
     * @param AInput Base64 编码字符串
     * @return 解码后的字节数组
     * @raises Exception 解码失败（非法字符或格式错误）
     *}
    class function Decode(const AInput: string): TBytes; static;

    {**
     * Base64 解码为字符串
     * 使用 UTF-8 解码
     *}
    class function DecodeString(const AInput: string): string; static;

    {**
     * Base64 编码（Try 模式）
     * 不抛异常，失败返回 False
     *}
    class function TryEncode(const AInput: TBytes; out AResult: string): Boolean; static;

    {**
     * Base64 解码（Try 模式）
     *}
    class function TryDecode(const AInput: string; out AResult: TBytes): Boolean; static;
  end;

implementation

const
  // Base64 编码表
  Base64Chars: array[0..63] of Char =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

  // Base64 解码表 (ASCII 值 -> 6位值, 255 = 无效)
  Base64Decode: array[0..127] of Byte = (
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,  62, 255, 255, 255,  63,
     52,  53,  54,  55,  56,  57,  58,  59,  60,  61, 255, 255, 255, 255, 255, 255,
    255,   0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,
     15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25, 255, 255, 255, 255, 255,
    255,  26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,
     41,  42,  43,  44,  45,  46,  47,  48,  49,  50,  51, 255, 255, 255, 255, 255
  );

{ TBase64Utils }

class function TBase64Utils.Encode(const AInput: TBytes): string;
var
  I, LLen, LOutLen: Integer;
  LTriple: Cardinal;
  POut: PChar;
begin
  Result := '';
  LLen := Length(AInput);
  if LLen = 0 then
    Exit;

  // 计算输出长度: 每3字节输入 -> 4字符输出，向上取整
  LOutLen := ((LLen + 2) div 3) * 4;
  SetLength(Result, LOutLen);
  POut := PChar(Result);

  I := 0;
  while I < LLen do
  begin
    // 读取最多3个字节
    LTriple := AInput[I] shl 16;
    if I + 1 < LLen then
      LTriple := LTriple or (AInput[I + 1] shl 8);
    if I + 2 < LLen then
      LTriple := LTriple or AInput[I + 2];

    // 输出4个Base64字符
    POut^ := Base64Chars[(LTriple shr 18) and $3F];
    Inc(POut);
    POut^ := Base64Chars[(LTriple shr 12) and $3F];
    Inc(POut);

    if I + 1 < LLen then
      POut^ := Base64Chars[(LTriple shr 6) and $3F]
    else
      POut^ := '=';
    Inc(POut);

    if I + 2 < LLen then
      POut^ := Base64Chars[LTriple and $3F]
    else
      POut^ := '=';
    Inc(POut);

    Inc(I, 3);
  end;
end;

class function TBase64Utils.Encode(const AInput: string): string;
var
  LBytes: TBytes;
begin
  LBytes := TEncoding.UTF8.GetBytes(UnicodeString(AInput));
  Result := Encode(LBytes);
end;

class function TBase64Utils.Decode(const AInput: string): TBytes;
var
  I, J, LLen, LOutLen, LPadding: Integer;
  LQuad: Cardinal;
  LChar: Char;
  LValue: Byte;
  LClean: string;
begin
  Result := nil;
  SetLength(Result, 0);

  if AInput = '' then
    Exit;

  // 移除空白字符
  LClean := '';
  for I := 1 to Length(AInput) do
  begin
    LChar := AInput[I];
    if not (LChar in [#9, #10, #13, ' ']) then
      LClean := LClean + LChar;
  end;

  LLen := Length(LClean);
  if LLen = 0 then
    Exit;

  // 检查长度是否为4的倍数
  if (LLen mod 4) <> 0 then
    raise Exception.Create('Invalid Base64 string length');

  // 计算填充
  LPadding := 0;
  if (LLen >= 1) and (LClean[LLen] = '=') then
    Inc(LPadding);
  if (LLen >= 2) and (LClean[LLen - 1] = '=') then
    Inc(LPadding);

  // 计算输出长度
  LOutLen := (LLen div 4) * 3 - LPadding;
  SetLength(Result, LOutLen);

  J := 0;
  I := 1;
  while I <= LLen do
  begin
    LQuad := 0;

    // 读取4个Base64字符
    LChar := LClean[I];
    if LChar = '=' then
      LValue := 0
    else if (Ord(LChar) > 127) or (Base64Decode[Ord(LChar)] = 255) then
      raise Exception.CreateFmt('Invalid Base64 character: %s', [LChar])
    else
      LValue := Base64Decode[Ord(LChar)];
    LQuad := LValue shl 18;
    Inc(I);

    LChar := LClean[I];
    if LChar = '=' then
      LValue := 0
    else if (Ord(LChar) > 127) or (Base64Decode[Ord(LChar)] = 255) then
      raise Exception.CreateFmt('Invalid Base64 character: %s', [LChar])
    else
      LValue := Base64Decode[Ord(LChar)];
    LQuad := LQuad or (LValue shl 12);
    Inc(I);

    LChar := LClean[I];
    if LChar = '=' then
      LValue := 0
    else if (Ord(LChar) > 127) or (Base64Decode[Ord(LChar)] = 255) then
      raise Exception.CreateFmt('Invalid Base64 character: %s', [LChar])
    else
      LValue := Base64Decode[Ord(LChar)];
    LQuad := LQuad or (LValue shl 6);
    Inc(I);

    LChar := LClean[I];
    if LChar = '=' then
      LValue := 0
    else if (Ord(LChar) > 127) or (Base64Decode[Ord(LChar)] = 255) then
      raise Exception.CreateFmt('Invalid Base64 character: %s', [LChar])
    else
      LValue := Base64Decode[Ord(LChar)];
    LQuad := LQuad or LValue;
    Inc(I);

    // 输出最多3个字节
    if J < LOutLen then
    begin
      Result[J] := (LQuad shr 16) and $FF;
      Inc(J);
    end;
    if J < LOutLen then
    begin
      Result[J] := (LQuad shr 8) and $FF;
      Inc(J);
    end;
    if J < LOutLen then
    begin
      Result[J] := LQuad and $FF;
      Inc(J);
    end;
  end;
end;

class function TBase64Utils.DecodeString(const AInput: string): string;
begin
  Result := string(TEncoding.UTF8.GetString(Decode(AInput)));
end;

class function TBase64Utils.TryEncode(const AInput: TBytes; out AResult: string): Boolean;
begin
  try
    AResult := Encode(AInput);
    Result := True;
  except
    AResult := '';
    Result := False;
  end;
end;

class function TBase64Utils.TryDecode(const AInput: string; out AResult: TBytes): Boolean;
begin
  try
    AResult := Decode(AInput);
    Result := True;
  except
    SetLength(AResult, 0);
    Result := False;
  end;
end;

end.
