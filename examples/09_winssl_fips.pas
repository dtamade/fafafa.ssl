program winssl_fips;

{$mode objfpc}{$H+}

{ ============================================================================
  示例 9: WinSSL FIPS 模式（Windows 专用）
  
  功能：演示如何检测和使用 Windows FIPS 140-2 合规模式
  用途：学习 FIPS 的概念和在 Windows 上的配置
  
  什么是 FIPS？
    FIPS 140-2 是美国联邦信息处理标准，规定了密码模块的安全要求。
    许多政府和金融机构要求使用 FIPS 认证的加密模块。
  
  WinSSL 优势：
    - 使用 Windows 内置的 Schannel
    - 自动遵循系统 FIPS 策略
    - 无需部署额外的加密库
    - 支持 Windows 证书存储
  
  注意：此示例仅在 Windows 上有效
  
  编译：fpc -Fusrc -Fusrc/winssl 09_winssl_fips.pas
  运行：09_winssl_fips（仅限 Windows）
  ============================================================================ }

uses
  SysUtils
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF};

{ 解释 FIPS 140-2 }
procedure ExplainFIPS;
begin
  WriteLn('================================================================================');
  WriteLn('  示例 9: WinSSL FIPS 模式');
  WriteLn('  理解 FIPS 140-2 和 Windows Schannel');
  WriteLn('================================================================================');
  WriteLn;
  
  WriteLn('[1/4] 什么是 FIPS 140-2？');
  WriteLn;
  WriteLn('  FIPS 140-2 = Federal Information Processing Standard 140-2');
  WriteLn('  联邦信息处理标准 140-2');
  WriteLn;
  WriteLn('  目的：');
  WriteLn('    • 定义密码模块的安全要求');
  WriteLn('    • 确保加密实现的正确性');
  WriteLn('    • 保护敏感信息');
  WriteLn;
  WriteLn('  认证级别：');
  WriteLn('    Level 1: 基础安全要求');
  WriteLn('    Level 2: 物理篡改检测');
  WriteLn('    Level 3: 篡改响应机制');
  WriteLn('    Level 4: 最高安全级别');
  WriteLn;
  WriteLn('  谁需要 FIPS？');
  WriteLn('    • 美国联邦政府机构（强制）');
  WriteLn('    • 金融机构');
  WriteLn('    • 医疗健康组织');
  WriteLn('    • 国防承包商');
  WriteLn('    • 合规性要求的企业');
  WriteLn;
end;

{ 解释 Windows Schannel }
procedure ExplainSchannel;
begin
  WriteLn('[2/4] Windows Schannel 和 FIPS');
  WriteLn;
  WriteLn('  什么是 Schannel？');
  WriteLn('    Schannel = Secure Channel');
  WriteLn('    Windows 内置的 SSL/TLS 实现');
  WriteLn;
  WriteLn('  Schannel 架构：');
  WriteLn('  ┌──────────────────────────────┐');
  WriteLn('  │   应用程序 (fafafa.ssl)      │');
  WriteLn('  └────────────┬─────────────────┘');
  WriteLn('               │');
  WriteLn('  ┌────────────▼─────────────────┐');
  WriteLn('  │   Schannel (SSPI)            │  ← Windows SSL/TLS');
  WriteLn('  └────────────┬─────────────────┘');
  WriteLn('               │');
  WriteLn('  ┌────────────▼─────────────────┐');
  WriteLn('  │   CNG (Cryptography Next Gen)│  ← FIPS 认证');
  WriteLn('  └──────────────────────────────┘');
  WriteLn;
  WriteLn('  优势：');
  WriteLn('    ✓ Windows 内置，无需部署');
  WriteLn('    ✓ 自动更新（通过 Windows Update）');
  WriteLn('    ✓ FIPS 140-2 认证');
  WriteLn('    ✓ 与 Windows 证书存储集成');
  WriteLn('    ✓ 支持企业策略（GPO）');
  WriteLn;
  WriteLn('  vs OpenSSL：');
  WriteLn('    OpenSSL:');
  WriteLn('      • 跨平台');
  WriteLn('      • 需要部署 DLL');
  WriteLn('      • 手动更新');
  WriteLn('      • FIPS 需要特殊编译');
  WriteLn;
  WriteLn('    WinSSL/Schannel:');
  WriteLn('      • Windows 专用');
  WriteLn('      • 零依赖部署');
  WriteLn('      • 自动更新');
  WriteLn('      • FIPS 内置支持');
  WriteLn;
end;

{ 检测 FIPS 模式 }
{$IFDEF WINDOWS}
function IsFIPSEnabled: Boolean;
var
  LReg: HKEY;
  LValue, LValueSize, LType: DWORD;
begin
  Result := False;
  
  // 检查注册表键：HKLM\System\CurrentControlSet\Control\Lsa\FipsAlgorithmPolicy
  if RegOpenKeyEx(HKEY_LOCAL_MACHINE,
                  'SYSTEM\CurrentControlSet\Control\Lsa\FipsAlgorithmPolicy',
                  0,
                  KEY_READ,
                  LReg) = ERROR_SUCCESS then
  begin
    LValueSize := SizeOf(DWORD);
    if RegQueryValueEx(LReg, 'Enabled', nil, @LType, @LValue, @LValueSize) = ERROR_SUCCESS then
    begin
      Result := (LValue <> 0);
    end;
    RegCloseKey(LReg);
  end;
end;
{$ENDIF}

procedure CheckFIPSStatus;
begin
  WriteLn('[3/4] 检测 FIPS 模式状态');
  WriteLn;
  
  {$IFDEF WINDOWS}
  WriteLn('  正在检查 Windows FIPS 模式...');
  WriteLn;
  
  if IsFIPSEnabled then
  begin
    WriteLn('  ✓ FIPS 模式：已启用');
    WriteLn;
    WriteLn('  当前系统配置为 FIPS 合规模式');
    WriteLn('  所有加密操作将使用 FIPS 认证的算法');
    WriteLn;
    WriteLn('  允许的算法：');
    WriteLn('    • AES (128, 192, 256)');
    WriteLn('    • 3DES');
    WriteLn('    • SHA-1, SHA-256, SHA-384, SHA-512');
    WriteLn('    • RSA (1024, 2048, 3072, 4096)');
    WriteLn('    • ECDSA (P-256, P-384, P-521)');
    WriteLn;
    WriteLn('  禁用的算法：');
    WriteLn('    • MD5');
    WriteLn('    • RC4');
    WriteLn('    • DES');
    WriteLn('    • 非认证的自定义算法');
  end
  else
  begin
    WriteLn('  ℹ FIPS 模式：未启用');
    WriteLn;
    WriteLn('  当前系统未启用 FIPS 模式');
    WriteLn('  所有算法可用（包括非 FIPS 算法）');
  end;
  
  WriteLn;
  WriteLn('  注册表位置：');
  WriteLn('    HKLM\SYSTEM\CurrentControlSet\Control\Lsa\FipsAlgorithmPolicy');
  WriteLn('    值名称：Enabled');
  WriteLn('    0 = 禁用，1 = 启用');
  WriteLn;
  
  {$ELSE}
  WriteLn('  ⚠️  此示例仅在 Windows 上有效');
  WriteLn;
  WriteLn('  当前平台：Linux/Unix');
  WriteLn('  WinSSL/Schannel 不可用');
  WriteLn;
  WriteLn('  在 Windows 上运行此示例将检测实际的 FIPS 状态。');
  {$ENDIF}
  
  WriteLn;
end;

{ 说明如何启用 FIPS }
procedure ExplainHowToEnable;
begin
  WriteLn('[4/4] 如何启用/禁用 FIPS 模式');
  WriteLn;
  
  WriteLn('  方法 1: 本地安全策略（推荐）');
  WriteLn('  ────────────────────────────────────────');
  WriteLn('  1. 打开"本地安全策略"（secpol.msc）');
  WriteLn('  2. 导航到：本地策略 → 安全选项');
  WriteLn('  3. 找到："系统加密：使用 FIPS 兼容算法..."');
  WriteLn('  4. 设置为"已启用"或"已禁用"');
  WriteLn('  5. 重启计算机');
  WriteLn;
  
  WriteLn('  方法 2: 组策略（企业域环境）');
  WriteLn('  ────────────────────────────────────────');
  WriteLn('  1. 打开"组策略管理器"（gpmc.msc）');
  WriteLn('  2. 创建或编辑 GPO');
  WriteLn('  3. 导航到同样的安全选项');
  WriteLn('  4. 应用策略到目标计算机');
  WriteLn('  5. 运行 gpupdate /force');
  WriteLn;
  
  WriteLn('  方法 3: 注册表（高级用户）');
  WriteLn('  ────────────────────────────────────────');
  WriteLn('  路径：');
  WriteLn('    HKLM\SYSTEM\CurrentControlSet\Control\Lsa\FipsAlgorithmPolicy');
  WriteLn('  值：');
  WriteLn('    Enabled (DWORD): 0 = 禁用, 1 = 启用');
  WriteLn;
  WriteLn('  PowerShell 命令：');
  WriteLn('    # 启用 FIPS');
  WriteLn('    Set-ItemProperty -Path "HKLM:\SYSTEM\CurrentControlSet\Control\Lsa\FipsAlgorithmPolicy" `');
  WriteLn('                     -Name "Enabled" -Value 1');
  WriteLn;
  WriteLn('    # 禁用 FIPS');
  WriteLn('    Set-ItemProperty -Path "HKLM:\SYSTEM\CurrentControlSet\Control\Lsa\FipsAlgorithmPolicy" `');
  WriteLn('                     -Name "Enabled" -Value 0');
  WriteLn;
  
  WriteLn('  ⚠️  重要提示：');
  WriteLn('    • 启用 FIPS 后需要重启');
  WriteLn('    • 某些应用程序可能不兼容');
  WriteLn('    • 测试所有关键应用程序');
  WriteLn('    • 准备回退计划');
  WriteLn;
end;

{ 说明 fafafa.ssl 中的使用 }
procedure ExplainUsage;
begin
  WriteLn('================================================================================');
  WriteLn('  ✓ 示例执行完成！');
  WriteLn('================================================================================');
  WriteLn;
  
  WriteLn('💡 在 fafafa.ssl 中使用 WinSSL + FIPS：');
  WriteLn;
  WriteLn('  uses');
  WriteLn('    fafafa.ssl.factory,');
  WriteLn('    fafafa.ssl.base;');
  WriteLn;
  WriteLn('  var');
  WriteLn('    LLib: ISSLLibrary;');
  WriteLn('    LContext: ISSLContext;');
  WriteLn('  begin');
  WriteLn('    // 使用 WinSSL 后端（自动支持 FIPS）');
  WriteLn('    LLib := TSSLFactory.GetLibraryInstance(sslWinSSL);');
  WriteLn('    LLib.Initialize;');
  WriteLn;
  WriteLn('    // 创建上下文');
  WriteLn('    LContext := LLib.CreateContext(sslCtxClient);');
  WriteLn;
  WriteLn('    // 如果系统启用了 FIPS，Schannel 会自动：');
  WriteLn('    // • 仅使用 FIPS 认证的算法');
  WriteLn('    // • 拒绝非 FIPS 算法');
  WriteLn('    // • 确保合规性');
  WriteLn('  end;');
  WriteLn;
  
  WriteLn('🔒 FIPS 合规性检查清单：');
  WriteLn('  □ 确认系统启用了 FIPS 模式');
  WriteLn('  □ 使用 WinSSL 后端（而非 OpenSSL）');
  WriteLn('  □ 仅使用 FIPS 认证的算法');
  WriteLn('  □ 测试所有加密操作');
  WriteLn('  □ 记录合规性证明');
  WriteLn('  □ 定期审计配置');
  WriteLn;
  
  WriteLn('📚 FIPS 认证的算法：');
  WriteLn;
  WriteLn('  加密算法：');
  WriteLn('    ✓ AES-128, AES-192, AES-256');
  WriteLn('    ✓ 3DES (Triple DES)');
  WriteLn('    ✗ RC4, DES, Blowfish');
  WriteLn;
  WriteLn('  哈希算法：');
  WriteLn('    ✓ SHA-1 (仅用于签名验证)');
  WriteLn('    ✓ SHA-256, SHA-384, SHA-512');
  WriteLn('    ✗ MD5, MD4');
  WriteLn;
  WriteLn('  非对称加密：');
  WriteLn('    ✓ RSA (1024-4096 位)');
  WriteLn('    ✓ ECDSA (P-256, P-384, P-521)');
  WriteLn('    ✗ DSA < 2048 位');
  WriteLn;
  WriteLn('⚠️  常见问题：');
  WriteLn;
  WriteLn('  Q: 启用 FIPS 会影响性能吗？');
  WriteLn('  A: 有轻微影响，但在现代硬件上可忽略不计。');
  WriteLn('     AES-NI 等硬件加速会减少影响。');
  WriteLn;
  WriteLn('  Q: 所有 Windows 版本都支持 FIPS 吗？');
  WriteLn('  A: Windows Server 2008+ 和 Windows Vista+ 都支持。');
  WriteLn('     但只有特定版本经过正式认证。');
  WriteLn;
  WriteLn('  Q: 启用 FIPS 后无法连接某些网站？');
  WriteLn('  A: 该网站可能使用了非 FIPS 算法（如 MD5）。');
  WriteLn('     需要网站升级到 FIPS 兼容算法。');
  WriteLn;
  WriteLn('  Q: 如何验证我的应用程序是 FIPS 合规的？');
  WriteLn('  A: 1. 在启用 FIPS 的系统上测试');
  WriteLn('     2. 确认所有加密操作成功');
  WriteLn('     3. 使用网络分析工具验证算法');
  WriteLn('     4. 记录测试结果作为合规证明');
  WriteLn;
  
  WriteLn('🔗 相关资源：');
  WriteLn('  - NIST FIPS 140-2: https://csrc.nist.gov/publications/detail/fips/140/2/final');
  WriteLn('  - Windows FIPS 文档: https://learn.microsoft.com/windows/security/');
  WriteLn('  - Schannel 文档: https://learn.microsoft.com/windows/win32/secauthn/');
  WriteLn;
  
  WriteLn('📊 WinSSL vs OpenSSL FIPS：');
  WriteLn;
  WriteLn('  特性              WinSSL            OpenSSL FIPS');
  WriteLn('  ─────────────────────────────────────────────────');
  WriteLn('  部署              零依赖            需要特殊编译');
  WriteLn('  更新              自动              手动');
  WriteLn('  认证              内置              需要购买模块');
  WriteLn('  配置              系统策略          代码配置');
  WriteLn('  跨平台            仅 Windows        所有平台');
  WriteLn('  证书存储          Windows 集成      文件系统');
  WriteLn('  企业管理          GPO 支持          手动');
  WriteLn;
end;

begin
  try
    ExplainFIPS;
    ExplainSchannel;
    CheckFIPSStatus;
    ExplainHowToEnable;
    ExplainUsage;
    
    ExitCode := 0;
    
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('================================================================================');
      WriteLn('  ✗ 错误: ', E.Message);
      WriteLn('================================================================================');
      WriteLn;
      ExitCode := 1;
    end;
  end;
end.

