program password_hash;

{$mode objfpc}{$H+}

{
  密码哈希工具
  用于安全存储密码的哈希计算
}

uses
  SysUtils,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.evp;

function HashPassword(const APassword: string): string;
var
  LCtx: PEVP_MD_CTX;
  LHash: array[0..31] of Byte;
  LLen: Cardinal;
  I: Integer;
begin
  LCtx := EVP_MD_CTX_new();
  try
    EVP_DigestInit_ex(LCtx, EVP_sha256(), nil);
    EVP_DigestUpdate(LCtx, PAnsiChar(APassword), Length(APassword));
    EVP_DigestFinal_ex(LCtx, @LHash[0], LLen);
    
    Result := '';
    for I := 0 to 31 do
      Result := Result + LowerCase(IntToHex(LHash[I], 2));
  finally
    EVP_MD_CTX_free(LCtx);
  end;
end;

function VerifyPassword(const APassword, AHash: string): Boolean;
begin
  Result := HashPassword(APassword) = LowerCase(AHash);
end;

procedure ShowUsage;
begin
  WriteLn('密码哈希工具 - fafafa.ssl');
  WriteLn;
  WriteLn('用法:');
  WriteLn('  password_hash hash <密码>           - 计算密码哈希');
  WriteLn('  password_hash verify <密码> <哈希>  - 验证密码');
  WriteLn;
  WriteLn('示例:');
  WriteLn('  password_hash hash mypassword123');
  WriteLn('  password_hash verify mypassword123 abc123...');
end;

var
  LMode, LPassword, LHash: string;

begin
  WriteLn('==========================================');
  WriteLn('  密码哈希工具');
  WriteLn('==========================================');
  WriteLn;
  
  if ParamCount < 2 then
  begin
    ShowUsage;
    Halt(1);
  end;
  
  try
    // 初始化OpenSSL
    LoadOpenSSLCore();
    LoadEVP(GetCryptoLibHandle);
    
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
      begin
        WriteLn('错误: 缺少哈希参数');
        ShowUsage;
        Halt(1);
      end;
      
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
    begin
      WriteLn('错误: 无效的模式: ', LMode);
      ShowUsage;
      Halt(1);
    end;
    
    WriteLn;
    WriteLn('==========================================');
    
  except
    on E: Exception do
    begin
      WriteLn('错误: ', E.Message);
      Halt(1);
    end;
  end;
end.
