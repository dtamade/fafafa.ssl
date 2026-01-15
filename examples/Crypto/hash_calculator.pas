program hash_calculator;

{$mode objfpc}{$H+}

{
  文件哈希计算工具
  支持 SHA-256, SHA-512, MD5
}

uses
  SysUtils, Classes,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.evp;

const
  BUFFER_SIZE = 8192;

function GetDigest(const AAlgorithm: string): PEVP_MD;
begin
  case LowerCase(AAlgorithm) of
    'sha256', 'sha-256': Result := EVP_sha256();
    'sha512', 'sha-512': Result := EVP_sha512();
    'sha1', 'sha-1': Result := EVP_sha1();
    'md5': Result := EVP_md5();
    else
      Result := nil;
  end;
end;

function CalculateFileHash(const AFileName, AAlgorithm: string): string;
var
  LFile: TFileStream;
  LBuffer: array[0..BUFFER_SIZE-1] of Byte;
  LBytesRead: Integer;
  LCtx: PEVP_MD_CTX;
  LDigest: PEVP_MD;
  LHash: array[0..63] of Byte;  // 最大64字节（SHA-512）
  LHashLen: Cardinal;
  I: Integer;
begin
  Result := '';
  
  // 获取摘要算法
  LDigest := GetDigest(AAlgorithm);
  if LDigest = nil then
    raise Exception.Create('不支持的算法: ' + AAlgorithm);
  
  // 打开文件
  LFile := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LCtx := EVP_MD_CTX_new();
    if LCtx = nil then
      raise Exception.Create('无法创建摘要上下文');
      
    try
      if EVP_DigestInit_ex(LCtx, LDigest, nil) <> 1 then
        raise Exception.Create('摘要初始化失败');
      
      // 读取并处理文件
      repeat
        LBytesRead := LFile.Read(LBuffer, BUFFER_SIZE);
        if LBytesRead > 0 then
          if EVP_DigestUpdate(LCtx, @LBuffer[0], LBytesRead) <> 1 then
            raise Exception.Create('摘要更新失败');
      until LBytesRead = 0;
      
      // 获取最终哈希
      if EVP_DigestFinal_ex(LCtx, @LHash[0], LHashLen) <> 1 then
        raise Exception.Create('获取摘要失败');
      
      // 转换为十六进制字符串
      for I := 0 to LHashLen - 1 do
        Result := Result + LowerCase(IntToHex(LHash[I], 2));
        
    finally
      EVP_MD_CTX_free(LCtx);
    end;
  finally
    LFile.Free;
  end;
end;

procedure ProcessFile(const AFileName, AAlgorithm: string);
var
  LHash: string;
  LFileInfo: TSearchRec;
begin
  if FindFirst(AFileName, faAnyFile, LFileInfo) = 0 then
  begin
    try
      Write(UpperCase(AAlgorithm), ' (', AFileName, ') = ');
      LHash := CalculateFileHash(AFileName, AAlgorithm);
      WriteLn(LHash);
      WriteLn('  文件大小: ', LFileInfo.Size, ' 字节');
    except
      on E: Exception do
        WriteLn('错误: ', E.Message);
    end;
    FindClose(LFileInfo);
  end
  else
    WriteLn('错误: 文件不存在: ', AFileName);
end;

procedure ShowUsage;
begin
  WriteLn('文件哈希计算工具 - fafafa.ssl');
  WriteLn;
  WriteLn('用法:');
  WriteLn('  hash_calculator [算法] <文件1> [文件2] ...');
  WriteLn;
  WriteLn('算法 (默认: sha256):');
  WriteLn('  sha256, sha-256   - SHA-256 (默认)');
  WriteLn('  sha512, sha-512   - SHA-512');
  WriteLn('  sha1, sha-1       - SHA-1');
  WriteLn('  md5               - MD5');
  WriteLn;
  WriteLn('示例:');
  WriteLn('  hash_calculator file.txt');
  WriteLn('  hash_calculator sha512 *.pas');
  WriteLn('  hash_calculator md5 document.pdf');
end;

var
  LAlgorithm: string;
  LFirstFile: Integer;
  I: Integer;
  LSR: TSearchRec;

begin
  WriteLn('==========================================');
  WriteLn('  文件哈希计算工具');
  WriteLn('==========================================');
  WriteLn;
  
  // 检查参数
  if ParamCount = 0 then
  begin
    ShowUsage;
    Halt(1);
  end;
  
  // 初始化OpenSSL
  try
    LoadOpenSSLCore();
    if not IsOpenSSLCoreLoaded then
    begin
      WriteLn('错误: 无法加载OpenSSL');
      Halt(1);
    end;
    LoadEVP(GetCryptoLibHandle);
  except
    on E: Exception do
    begin
      WriteLn('错误: ', E.Message);
      Halt(1);
    end;
  end;
  
  // 解析参数
  if (ParamCount >= 2) and (GetDigest(ParamStr(1)) <> nil) then
  begin
    LAlgorithm := ParamStr(1);
    LFirstFile := 2;
  end
  else
  begin
    LAlgorithm := 'sha256';
    LFirstFile := 1;
  end;
  
  // 处理每个文件
  for I := LFirstFile to ParamCount do
  begin
    if FindFirst(ParamStr(I), faAnyFile, LSR) = 0 then
    begin
      repeat
        if (LSR.Attr and faDirectory) = 0 then  // 不是目录
        begin
          ProcessFile(LSR.Name, LAlgorithm);
          WriteLn;
        end;
      until FindNext(LSR) <> 0;
      FindClose(LSR);
    end
    else
      ProcessFile(ParamStr(I), LAlgorithm);
  end;
  
  WriteLn('==========================================');
  WriteLn('✓ 完成');
  WriteLn('==========================================');
end.
