program password_hash_improved;

{$mode objfpc}{$H+}

{**
 * 密码哈希工具 - 改进版
 * 
 * 使用改进的TCryptoUtils进行密码哈希
 * 特性：
 * - 统一异常处理
 * - 详细错误消息
 * - 参数验证
 *}

uses
  SysUtils,
  fafafa.ssl.crypto.utils,
  fafafa.ssl.exceptions;

{** 计算密码SHA-256哈希 *}
function HashPassword(const APassword: string): string;
var
  LHash: TBytes;
begin
  if Length(APassword) = 0 then
    raise ESSLInvalidArgument.Create('Password cannot be empty');
    
  LHash := TCryptoUtils.SHA256(APassword);
  Result := LowerCase(TCryptoUtils.BytesToHex(LHash));
end;

{** 验证密码哈希 *}
function VerifyPassword(const APassword, AHash: string): Boolean;
begin
  Result := HashPassword(APassword) = LowerCase(AHash);
end;

procedure ShowUsage;
begin
  WriteLn('密码哈希工具 v2.0 - fafafa.ssl');
  WriteLn;
  WriteLn('用法:');
  WriteLn('  password_hash hash <密码>           - 计算密码哈希');
  WriteLn('  password_hash verify <密码> <哈希>  - 验证密码');
  WriteLn;
  WriteLn('示例:');
  WriteLn('  password_hash hash mypassword123');
  WriteLn('  password_hash verify mypassword123 abc123...');
  WriteLn;
  WriteLn('改进:');
  WriteLn('  ✓ 统一异常处理');
  WriteLn('  ✓ 参数验证');
  WriteLn('  ✓ 详细错误消息');
end;

var
  LMode, LPassword, LHash: string;

begin
  WriteLn('==========================================');
  WriteLn('  密码哈希工具 v2.0');
  WriteLn('==========================================');
  WriteLn;
  
  if ParamCount < 2 then
  begin
    ShowUsage;
    Halt(1);
  end;
  
  try
    LMode := LowerCase(ParamStr(1));
    LPassword := ParamStr(2);
    
    if LMode = 'hash' then
    begin
      LHash := HashPassword(LPassword);
      WriteLn('密码: ', LPassword);
      WriteLn('SHA-256: ', LHash);
      WriteLn;
      WriteLn('✓ 请保存此哈希值用于验证');
    end
    else if LMode = 'verify' then
    begin
      if ParamCount < 3 then
        raise ESSLInvalidArgument.Create('Missing hash argument');
      
      LHash := ParamStr(3);
      
      WriteLn('密码: ', LPassword);
      WriteLn('哈希: ', LHash);
      WriteLn;
      
      if VerifyPassword(LPassword, LHash) then
        WriteLn('✓ 密码匹配!')
      else
        WriteLn('✗ 密码不匹配');
    end
    else
      raise ESSLInvalidArgument.CreateFmt('Invalid mode: %s', [LMode]);
    
    WriteLn;
    WriteLn('==========================================');
    
  except
    on E: ESSLException do
    begin
      WriteLn('错误: ', E.Message);
      if E.ErrorCode <> 0 then
        WriteLn('错误码: 0x', IntToHex(E.ErrorCode, 8));
      Halt(1);
    end;
    on E: Exception do
    begin
      WriteLn('错误: ', E.Message);
      Halt(1);
    end;
  end;
end.
