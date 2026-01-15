program test_ssh;

{$mode objfpc}{$H+}

{
  SSH 密钥解析测试程序
  
  测试内容:
  - RSA 公钥解析
  - Ed25519 公钥解析
  - ECDSA 公钥解析
  - 公钥指纹计算
  - 私钥解析 (OpenSSH 格式)
  - 私钥解析 (PEM 格式)
}

uses
  SysUtils, Classes, fafafa.ssl.ssh, fafafa.ssl.openssl.loader;

var
  GTestsPassed: Integer = 0;
  GTestsFailed: Integer = 0;

procedure PrintHeader(const ATitle: string);
begin
  WriteLn;
  WriteLn(StringOfChar('=', 60));
  WriteLn('  ', ATitle);
  WriteLn(StringOfChar('=', 60));
end;

procedure PrintResult(const ATestName: string; APassed: Boolean; const AMessage: string = '');
begin
  if APassed then
  begin
    WriteLn('[PASS] ', ATestName);
    Inc(GTestsPassed);
  end
  else
  begin
    WriteLn('[FAIL] ', ATestName);
    if AMessage <> '' then
      WriteLn('       Error: ', AMessage);
    Inc(GTestsFailed);
  end;
end;

{ ==================== 测试数据 ==================== }

const
  // RSA 公钥测试数据 (2048 位)
  TEST_RSA_PUBKEY = 'ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDLHd7r' +
    'Gy8GTBZ7a3EHnqJ8QmqrT3A7x9z5y4xvYvE7f8Nz5y4xvYvE7f8Nz5y4xvYvE7f8N' +
    'z5y4xvYvE7f8Nz5y4xvYvE7f8Nz5y4xvYvE7f8Nz5y4xvYvE7f8Nz5y4xvYvE7f8N' +
    'z5y4xvYvE7f8Nz5y4xvYvE7f8Nz5y4xvYvE7f8Nz5y4xvYvE7f8Nz5y4xvYvE7f8N' +
    'z5y4xvYvE7f8Nz5y4xvYvE7f8Nz5y4xvYvE7f8Nz5y4xvYvE7f8Nz5y4xvYvE7f8N' +
    'z5y4xvYvE7f8Nz5y4xvYvE7f8Nz5y4xvYvE7f8Nz5y4xvYvE7f8Nz5y4xvYvE7f8N' +
    'z5y4xvYvE7f8Nz5y4xvYvE7f8N test@example.com';

  // Ed25519 公钥测试数据
  TEST_ED25519_PUBKEY = 'ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl test@example.com';

  // ECDSA P-256 公钥测试数据
  TEST_ECDSA_P256_PUBKEY = 'ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBEmKSENjQEezOmxkZMy7opKgwFB9nkt5YRrYMjNuG5N87uRgg6CLrbo5wAdT/y6v0mKV0U2w0WZ2YB/++Tpockg= test@example.com';

{ ==================== 公钥解析测试 ==================== }

procedure TestRSAPublicKeyParsing;
var
  LKey: ISSHPublicKey;
  LResult: TSSHPublicKeyResult;
begin
  PrintHeader('RSA Public Key Parsing');
  
  // 测试 1: 使用 Result 类型解析
  LResult := TSSHKeyManager.ParsePublicKeyResult(TEST_RSA_PUBKEY);
  PrintResult('ParsePublicKeyResult returns success', LResult.IsOk, LResult.ErrorMessage);
  
  if LResult.IsOk then
  begin
    LKey := LResult.Key;
    PrintResult('Key type is RSA', LKey.KeyType = sshKeyRSA);
    PrintResult('Key data is not empty', Length(LKey.KeyData) > 0);
    PrintResult('Comment is extracted', LKey.Comment = 'test@example.com');
    PrintResult('Key is valid', LKey.IsValid);
    PrintResult('Fingerprint SHA256 starts with SHA256:', Pos('SHA256:', LKey.GetFingerprintSHA256) = 1);
    PrintResult('Fingerprint MD5 starts with MD5:', Pos('MD5:', LKey.GetFingerprintMD5) = 1);
  end;
  
  // 测试 2: 使用 TryParse
  if TSSHKeyManager.TryParsePublicKey(TEST_RSA_PUBKEY, LKey) then
    PrintResult('TryParsePublicKey succeeds', True)
  else
    PrintResult('TryParsePublicKey succeeds', False, 'TryParsePublicKey returned false');
end;

procedure TestEd25519PublicKeyParsing;
var
  LKey: ISSHPublicKey;
  LResult: TSSHPublicKeyResult;
begin
  PrintHeader('Ed25519 Public Key Parsing');
  
  LResult := TSSHKeyManager.ParsePublicKeyResult(TEST_ED25519_PUBKEY);
  PrintResult('ParsePublicKeyResult returns success', LResult.IsOk, LResult.ErrorMessage);
  
  if LResult.IsOk then
  begin
    LKey := LResult.Key;
    PrintResult('Key type is Ed25519', LKey.KeyType = sshKeyEd25519);
    PrintResult('Key data is not empty', Length(LKey.KeyData) > 0);
    PrintResult('Comment is extracted', LKey.Comment = 'test@example.com');
    PrintResult('Key is valid', LKey.IsValid);
    PrintResult('Key size is 256 bits', LKey.GetKeySize = 256);
  end;
end;

procedure TestECDSAPublicKeyParsing;
var
  LKey: ISSHPublicKey;
  LResult: TSSHPublicKeyResult;
begin
  PrintHeader('ECDSA P-256 Public Key Parsing');
  
  LResult := TSSHKeyManager.ParsePublicKeyResult(TEST_ECDSA_P256_PUBKEY);
  PrintResult('ParsePublicKeyResult returns success', LResult.IsOk, LResult.ErrorMessage);
  
  if LResult.IsOk then
  begin
    LKey := LResult.Key;
    PrintResult('Key type is ECDSA P-256', LKey.KeyType = sshKeyECDSA_P256);
    PrintResult('Key data is not empty', Length(LKey.KeyData) > 0);
    PrintResult('Comment is extracted', LKey.Comment = 'test@example.com');
    PrintResult('Key is valid', LKey.IsValid);
    PrintResult('Key size is 256 bits', LKey.GetKeySize = 256);
  end;
end;

{ ==================== 错误处理测试 ==================== }

procedure TestInvalidKeyHandling;
var
  LKey: ISSHPublicKey;
  LResult: TSSHPublicKeyResult;
begin
  PrintHeader('Invalid Key Handling');
  
  // 测试空字符串
  LResult := TSSHKeyManager.ParsePublicKeyResult('');
  PrintResult('Empty string returns error', LResult.IsErr);
  
  // 测试无效格式
  LResult := TSSHKeyManager.ParsePublicKeyResult('invalid-key-data');
  PrintResult('Invalid format returns error', LResult.IsErr);
  
  // 测试不支持的密钥类型
  LResult := TSSHKeyManager.ParsePublicKeyResult('ssh-dss AAAAB3NzaC1kc3MAAACBAP test');
  PrintResult('Unsupported key type returns error', LResult.IsErr);
  
  // 测试 TryParse 不抛异常
  if not TSSHKeyManager.TryParsePublicKey('invalid', LKey) then
    PrintResult('TryParsePublicKey returns false for invalid input', True)
  else
    PrintResult('TryParsePublicKey returns false for invalid input', False);
end;

{ ==================== 辅助函数测试 ==================== }

procedure TestHelperFunctions;
begin
  PrintHeader('Helper Functions');
  
  // 测试 SSHKeyTypeToString
  PrintResult('SSHKeyTypeToString(sshKeyRSA) = ssh-rsa', 
    SSHKeyTypeToString(sshKeyRSA) = 'ssh-rsa');
  PrintResult('SSHKeyTypeToString(sshKeyEd25519) = ssh-ed25519', 
    SSHKeyTypeToString(sshKeyEd25519) = 'ssh-ed25519');
  PrintResult('SSHKeyTypeToString(sshKeyECDSA_P256) = ecdsa-sha2-nistp256', 
    SSHKeyTypeToString(sshKeyECDSA_P256) = 'ecdsa-sha2-nistp256');
  
  // 测试 StringToSSHKeyType
  PrintResult('StringToSSHKeyType(ssh-rsa) = sshKeyRSA', 
    StringToSSHKeyType('ssh-rsa') = sshKeyRSA);
  PrintResult('StringToSSHKeyType(ssh-ed25519) = sshKeyEd25519', 
    StringToSSHKeyType('ssh-ed25519') = sshKeyEd25519);
  PrintResult('StringToSSHKeyType(unknown) = sshKeyUnknown', 
    StringToSSHKeyType('unknown') = sshKeyUnknown);
  
  // 测试 SSHKeyErrorToString
  PrintResult('SSHKeyErrorToString(sshKeyErrNone) is not empty', 
    SSHKeyErrorToString(sshKeyErrNone) <> '');
  PrintResult('SSHKeyErrorToString(sshKeyErrInvalidFormat) is not empty', 
    SSHKeyErrorToString(sshKeyErrInvalidFormat) <> '');
end;

{ ==================== Result 类型测试 ==================== }

procedure TestResultTypes;
var
  LKeyResult: TSSHKeyResult;
  LPubKeyResult: TSSHPublicKeyResult;
begin
  PrintHeader('Result Types');
  
  // 测试 TSSHKeyResult
  LKeyResult := TSSHKeyResult.Ok;
  PrintResult('TSSHKeyResult.Ok.IsOk = True', LKeyResult.IsOk);
  PrintResult('TSSHKeyResult.Ok.IsErr = False', not LKeyResult.IsErr);
  
  LKeyResult := TSSHKeyResult.Err(sshKeyErrInvalidFormat, 'Test error');
  PrintResult('TSSHKeyResult.Err.IsOk = False', not LKeyResult.IsOk);
  PrintResult('TSSHKeyResult.Err.IsErr = True', LKeyResult.IsErr);
  PrintResult('TSSHKeyResult.Err.ErrorCode is correct', 
    LKeyResult.ErrorCode = sshKeyErrInvalidFormat);
  PrintResult('TSSHKeyResult.Err.ErrorMessage is correct', 
    LKeyResult.ErrorMessage = 'Test error');
  
  // 测试 TSSHPublicKeyResult
  LPubKeyResult := TSSHPublicKeyResult.Err(sshKeyErrDecodingFailed, 'Decode error');
  PrintResult('TSSHPublicKeyResult.Err works correctly', 
    LPubKeyResult.IsErr and (LPubKeyResult.Key = nil));
end;

{ ==================== 公钥导出测试 ==================== }

procedure TestPublicKeyExport;
var
  LKey: ISSHPublicKey;
  LExported: string;
begin
  PrintHeader('Public Key Export');
  
  // 解析 Ed25519 公钥
  LKey := TSSHKeyManager.ParsePublicKey(TEST_ED25519_PUBKEY);
  if LKey <> nil then
  begin
    // 导出为 OpenSSH 格式
    LExported := LKey.ToOpenSSHFormat;
    PrintResult('ToOpenSSHFormat returns non-empty string', LExported <> '');
    PrintResult('Exported key starts with key type', 
      Pos('ssh-ed25519', LExported) = 1);
    PrintResult('Exported key contains comment', 
      Pos('test@example.com', LExported) > 0);
    
    // 验证导出的密钥可以重新解析
    try
      LKey := TSSHKeyManager.ParsePublicKey(LExported);
      PrintResult('Exported key can be re-parsed', LKey <> nil);
    except
      on E: Exception do
        PrintResult('Exported key can be re-parsed', False, E.Message);
    end;
  end
  else
    PrintResult('Failed to parse test key', False);
end;

{ ==================== 弱密钥检测测试 ==================== }

procedure TestWeakKeyDetection;
var
  LKey: ISSHPublicKey;
begin
  PrintHeader('Weak Key Detection');
  
  // Ed25519 不应该被检测为弱密钥
  LKey := TSSHKeyManager.ParsePublicKey(TEST_ED25519_PUBKEY);
  if LKey <> nil then
    PrintResult('Ed25519 key is not weak', not TSSHKeyManager.IsWeakKey(LKey))
  else
    PrintResult('Ed25519 key is not weak', False, 'Failed to parse key');
  
  // ECDSA P-256 不应该被检测为弱密钥
  LKey := TSSHKeyManager.ParsePublicKey(TEST_ECDSA_P256_PUBKEY);
  if LKey <> nil then
    PrintResult('ECDSA P-256 key is not weak', not TSSHKeyManager.IsWeakKey(LKey))
  else
    PrintResult('ECDSA P-256 key is not weak', False, 'Failed to parse key');
  
  // nil 密钥应该被检测为弱密钥
  PrintResult('nil key is detected as weak', TSSHKeyManager.IsWeakKey(nil));
end;

{ ==================== 密钥生成测试 ==================== }

function IsOpenSSLAvailable: Boolean;
var
  LHandle: THandle;
begin
  LHandle := TOpenSSLLoader.GetLibraryHandle(osslLibCrypto);
  Result := LHandle <> 0;
end;

procedure TestRSAKeyGeneration;
var
  LKeyPair: ISSHKeyPair;
  LPubKey: ISSHPublicKey;
  LPrivKey: ISSHPrivateKey;
  LExported: string;
begin
  PrintHeader('RSA Key Generation');
  
  // 检查 OpenSSL 是否可用
  if not IsOpenSSLAvailable then
  begin
    WriteLn('[SKIP] OpenSSL not available - skipping RSA key generation tests');
    Exit;
  end;
  
  try
    // 生成 2048 位 RSA 密钥对
    LKeyPair := TSSHKeyManager.GenerateRSAKeyPair(2048, 'test@rsa2048');
    PrintResult('GenerateRSAKeyPair(2048) succeeds', LKeyPair <> nil);
    
    if LKeyPair <> nil then
    begin
      LPubKey := LKeyPair.PublicKey;
      LPrivKey := LKeyPair.PrivateKey;
      
      PrintResult('Public key is not nil', LPubKey <> nil);
      PrintResult('Private key is not nil', LPrivKey <> nil);
      
      if LPubKey <> nil then
      begin
        PrintResult('Public key type is RSA', LPubKey.KeyType = sshKeyRSA);
        PrintResult('Public key is valid', LPubKey.IsValid);
        PrintResult('Public key comment is correct', LPubKey.Comment = 'test@rsa2048');
        
        // 测试导出
        LExported := LPubKey.ToOpenSSHFormat;
        PrintResult('Public key can be exported', LExported <> '');
        PrintResult('Exported key starts with ssh-rsa', Pos('ssh-rsa', LExported) = 1);
      end;
      
      if LPrivKey <> nil then
      begin
        PrintResult('Private key type is RSA', LPrivKey.KeyType = sshKeyRSA);
        PrintResult('Private key is valid', LPrivKey.IsValid);
        PrintResult('Private key is not encrypted', not LPrivKey.IsEncrypted);
      end;
      
      // 测试密钥对匹配
      PrintResult('Key pair is valid', LKeyPair.IsValidPair);
    end;
  except
    on E: Exception do
      PrintResult('RSA key generation', False, E.Message);
  end;
end;

procedure TestEd25519KeyGeneration;
var
  LKeyPair: ISSHKeyPair;
  LPubKey: ISSHPublicKey;
  LPrivKey: ISSHPrivateKey;
  LExported: string;
begin
  PrintHeader('Ed25519 Key Generation');
  
  // 检查 OpenSSL 是否可用
  if not IsOpenSSLAvailable then
  begin
    WriteLn('[SKIP] OpenSSL not available - skipping Ed25519 key generation tests');
    Exit;
  end;
  
  try
    // 生成 Ed25519 密钥对
    LKeyPair := TSSHKeyManager.GenerateEd25519KeyPair('test@ed25519');
    PrintResult('GenerateEd25519KeyPair succeeds', LKeyPair <> nil);
    
    if LKeyPair <> nil then
    begin
      LPubKey := LKeyPair.PublicKey;
      LPrivKey := LKeyPair.PrivateKey;
      
      PrintResult('Public key is not nil', LPubKey <> nil);
      PrintResult('Private key is not nil', LPrivKey <> nil);
      
      if LPubKey <> nil then
      begin
        PrintResult('Public key type is Ed25519', LPubKey.KeyType = sshKeyEd25519);
        PrintResult('Public key is valid', LPubKey.IsValid);
        PrintResult('Public key size is 256 bits', LPubKey.GetKeySize = 256);
        PrintResult('Public key comment is correct', LPubKey.Comment = 'test@ed25519');
        
        // 测试导出
        LExported := LPubKey.ToOpenSSHFormat;
        PrintResult('Public key can be exported', LExported <> '');
        PrintResult('Exported key starts with ssh-ed25519', Pos('ssh-ed25519', LExported) = 1);
      end;
      
      if LPrivKey <> nil then
      begin
        PrintResult('Private key type is Ed25519', LPrivKey.KeyType = sshKeyEd25519);
        PrintResult('Private key is valid', LPrivKey.IsValid);
        PrintResult('Private key is not encrypted', not LPrivKey.IsEncrypted);
      end;
      
      // 测试密钥对匹配
      PrintResult('Key pair is valid', LKeyPair.IsValidPair);
    end;
  except
    on E: Exception do
      PrintResult('Ed25519 key generation', False, E.Message);
  end;
end;

procedure TestECDSAKeyGeneration;
var
  LKeyPair: ISSHKeyPair;
  LPubKey: ISSHPublicKey;
  LPrivKey: ISSHPrivateKey;
  LExported: string;
begin
  PrintHeader('ECDSA Key Generation');
  
  // 检查 OpenSSL 是否可用
  if not IsOpenSSLAvailable then
  begin
    WriteLn('[SKIP] OpenSSL not available - skipping ECDSA key generation tests');
    Exit;
  end;
  
  // 测试 P-256 曲线
  try
    LKeyPair := TSSHKeyManager.GenerateECDSAKeyPair(sshKeyECDSA_P256, 'test@p256');
    PrintResult('GenerateECDSAKeyPair(P-256) succeeds', LKeyPair <> nil);
    
    if LKeyPair <> nil then
    begin
      LPubKey := LKeyPair.PublicKey;
      PrintResult('P-256 public key type is correct', LPubKey.KeyType = sshKeyECDSA_P256);
      PrintResult('P-256 public key is valid', LPubKey.IsValid);
      PrintResult('P-256 public key size is 256 bits', LPubKey.GetKeySize = 256);
      
      LExported := LPubKey.ToOpenSSHFormat;
      PrintResult('P-256 key can be exported', LExported <> '');
      PrintResult('P-256 exported key starts correctly', Pos('ecdsa-sha2-nistp256', LExported) = 1);
    end;
  except
    on E: Exception do
      PrintResult('ECDSA P-256 key generation', False, E.Message);
  end;
  
  // 测试 P-384 曲线
  try
    LKeyPair := TSSHKeyManager.GenerateECDSAKeyPair(sshKeyECDSA_P384, 'test@p384');
    PrintResult('GenerateECDSAKeyPair(P-384) succeeds', LKeyPair <> nil);
    
    if LKeyPair <> nil then
    begin
      LPubKey := LKeyPair.PublicKey;
      PrintResult('P-384 public key type is correct', LPubKey.KeyType = sshKeyECDSA_P384);
      PrintResult('P-384 public key is valid', LPubKey.IsValid);
      PrintResult('P-384 public key size is 384 bits', LPubKey.GetKeySize = 384);
    end;
  except
    on E: Exception do
      PrintResult('ECDSA P-384 key generation', False, E.Message);
  end;
  
  // 测试 P-521 曲线
  try
    LKeyPair := TSSHKeyManager.GenerateECDSAKeyPair(sshKeyECDSA_P521, 'test@p521');
    PrintResult('GenerateECDSAKeyPair(P-521) succeeds', LKeyPair <> nil);
    
    if LKeyPair <> nil then
    begin
      LPubKey := LKeyPair.PublicKey;
      PrintResult('P-521 public key type is correct', LPubKey.KeyType = sshKeyECDSA_P521);
      PrintResult('P-521 public key is valid', LPubKey.IsValid);
      PrintResult('P-521 public key size is 521 bits', LPubKey.GetKeySize = 521);
    end;
  except
    on E: Exception do
      PrintResult('ECDSA P-521 key generation', False, E.Message);
  end;
end;

{ ==================== 私钥导出测试 ==================== }

procedure TestPrivateKeyExport;
var
  LKeyPair: ISSHKeyPair;
  LPrivKey: ISSHPrivateKey;
  LExported: string;
  LParsedKey: ISSHPrivateKey;
begin
  PrintHeader('Private Key Export');
  
  // 检查 OpenSSL 是否可用
  if not IsOpenSSLAvailable then
  begin
    WriteLn('[SKIP] OpenSSL not available - skipping private key export tests');
    Exit;
  end;
  
  // 测试 Ed25519 私钥导出 (未加密)
  try
    LKeyPair := TSSHKeyManager.GenerateEd25519KeyPair('test@export');
    PrintResult('Generate Ed25519 key pair', LKeyPair <> nil);
    
    if LKeyPair <> nil then
    begin
      LPrivKey := LKeyPair.PrivateKey;
      
      // 导出未加密私钥
      LExported := LPrivKey.ToOpenSSHFormat('');
      PrintResult('Export unencrypted private key', LExported <> '');
      PrintResult('Exported key has correct header', Pos('-----BEGIN OPENSSH PRIVATE KEY-----', LExported) > 0);
      PrintResult('Exported key has correct footer', Pos('-----END OPENSSH PRIVATE KEY-----', LExported) > 0);
      
      // 尝试重新解析导出的私钥
      if TSSHKeyManager.TryParsePrivateKey(LExported, '', LParsedKey) then
      begin
        PrintResult('Re-parse exported key succeeds', True);
        PrintResult('Re-parsed key type matches', LParsedKey.KeyType = sshKeyEd25519);
        PrintResult('Re-parsed key is valid', LParsedKey.IsValid);
      end
      else
        PrintResult('Re-parse exported key succeeds', False, 'Failed to re-parse exported key');
    end;
  except
    on E: Exception do
      PrintResult('Ed25519 private key export (unencrypted)', False, E.Message);
  end;
  
  // 测试 Ed25519 私钥导出 (加密)
  try
    LKeyPair := TSSHKeyManager.GenerateEd25519KeyPair('test@encrypted');
    PrintResult('Generate Ed25519 key pair for encryption test', LKeyPair <> nil);
    
    if LKeyPair <> nil then
    begin
      LPrivKey := LKeyPair.PrivateKey;
      
      // 导出加密私钥
      LExported := LPrivKey.ToOpenSSHFormat('testpassword123');
      PrintResult('Export encrypted private key', LExported <> '');
      PrintResult('Encrypted key has correct header', Pos('-----BEGIN OPENSSH PRIVATE KEY-----', LExported) > 0);
      
      // 尝试用正确密码解析
      if TSSHKeyManager.TryParsePrivateKey(LExported, 'testpassword123', LParsedKey) then
      begin
        PrintResult('Parse encrypted key with correct password', True);
        PrintResult('Decrypted key type matches', LParsedKey.KeyType = sshKeyEd25519);
      end
      else
        PrintResult('Parse encrypted key with correct password', False, 'Failed to parse with correct password');
      
      // 尝试用错误密码解析 (应该失败)
      if not TSSHKeyManager.TryParsePrivateKey(LExported, 'wrongpassword', LParsedKey) then
        PrintResult('Parse encrypted key with wrong password fails', True)
      else
        PrintResult('Parse encrypted key with wrong password fails', False, 'Should have failed with wrong password');
    end;
  except
    on E: Exception do
      PrintResult('Ed25519 private key export (encrypted)', False, E.Message);
  end;
  
  // 测试 RSA 私钥导出
  try
    LKeyPair := TSSHKeyManager.GenerateRSAKeyPair(2048, 'test@rsa-export');
    PrintResult('Generate RSA key pair for export', LKeyPair <> nil);
    
    if LKeyPair <> nil then
    begin
      LPrivKey := LKeyPair.PrivateKey;
      
      // 导出未加密私钥
      LExported := LPrivKey.ToOpenSSHFormat('');
      PrintResult('Export RSA private key', LExported <> '');
      PrintResult('RSA exported key has correct header', Pos('-----BEGIN OPENSSH PRIVATE KEY-----', LExported) > 0);
    end;
  except
    on E: Exception do
      PrintResult('RSA private key export', False, E.Message);
  end;
end;

{ ==================== 主程序 ==================== }

begin
  WriteLn('SSH Key Parsing Test Suite');
  WriteLn('==========================');
  WriteLn;
  
  try
    // 公钥解析测试
    TestRSAPublicKeyParsing;
    TestEd25519PublicKeyParsing;
    TestECDSAPublicKeyParsing;
    
    // 错误处理测试
    TestInvalidKeyHandling;
    
    // 辅助函数测试
    TestHelperFunctions;
    
    // Result 类型测试
    TestResultTypes;
    
    // 公钥导出测试
    TestPublicKeyExport;
    
    // 弱密钥检测测试
    TestWeakKeyDetection;
    
    // 密钥生成测试
    TestRSAKeyGeneration;
    TestEd25519KeyGeneration;
    TestECDSAKeyGeneration;
    
    // 私钥导出测试
    TestPrivateKeyExport;
    
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('FATAL ERROR: ', E.Message);
      Inc(GTestsFailed);
    end;
  end;
  
  // 打印总结
  WriteLn;
  WriteLn(StringOfChar('=', 60));
  WriteLn('  Test Summary');
  WriteLn(StringOfChar('=', 60));
  WriteLn('  Passed: ', GTestsPassed);
  WriteLn('  Failed: ', GTestsFailed);
  WriteLn('  Total:  ', GTestsPassed + GTestsFailed);
  WriteLn;
  
  if GTestsFailed > 0 then
  begin
    WriteLn('SOME TESTS FAILED!');
    Halt(1);
  end
  else
    WriteLn('ALL TESTS PASSED!');
end.
