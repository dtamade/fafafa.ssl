program cert_renewal;

{$mode objfpc}{$H+}

{ ============================================================================
  示例 10: 证书自动更新服务
  
  功能：演示证书过期监控和自动更新流程
  用途：学习生产环境中的证书生命周期管理
  
  为什么需要证书自动更新？
    - 证书有有效期（通常 90 天）
    - 手动更新容易出错和遗忘
    - 过期证书导致服务中断
    - 自动化提高可靠性
  
  常见解决方案：
    - Let's Encrypt + ACME 协议
    - cert-manager (Kubernetes)
    - AWS Certificate Manager
    - Azure Key Vault
  
  编译：fpc -Fusrc -Fusrc/openssl 10_cert_renewal.pas
  运行：10_cert_renewal
  ============================================================================ }

uses
  SysUtils;

{ 辅助函数：增加天数 }
function IncDay(const ADate: TDateTime; ADays: Integer): TDateTime;
begin
  Result := ADate + ADays;
end;

{ 辅助函数：格式化日期 }
function FormatDate(const ADate: TDateTime): string;
var
  Year, Month, Day: Word;
begin
  DecodeDate(ADate, Year, Month, Day);
  Result := Format('%.4d-%.2d-%.2d', [Year, Month, Day]);
end;

type
  { 证书信息 }
  TCertificateInfo = record
    Name: string;
    Subject: string;
    Issuer: string;
    NotBefore: TDateTime;
    NotAfter: TDateTime;
    DaysUntilExpiry: Integer;
  end;

{ 解释证书生命周期 }
procedure ExplainCertificateLifecycle;
begin
  WriteLn('================================================================================');
  WriteLn('  示例 10: 证书自动更新服务');
  WriteLn('  理解证书生命周期管理');
  WriteLn('================================================================================');
  WriteLn;
  
  WriteLn('[1/5] 证书生命周期');
  WriteLn;
  WriteLn('  典型的证书生命周期（90 天为例）：');
  WriteLn;
  WriteLn('  Day 0-60: 正常使用期');
  WriteLn('  ┌─────────────────────────────────────────────────┐');
  WriteLn('  │ ✓ 证书有效                                       │');
  WriteLn('  │ ✓ 服务正常运行                                   │');
  WriteLn('  │ • 无需操作                                       │');
  WriteLn('  └─────────────────────────────────────────────────┘');
  WriteLn;
  WriteLn('  Day 61-75: 更新准备期');
  WriteLn('  ┌─────────────────────────────────────────────────┐');
  WriteLn('  │ ⚠️  距离过期 < 30 天                              │');
  WriteLn('  │ • 开始监控告警                                   │');
  WriteLn('  │ • 准备更新流程                                   │');
  WriteLn('  │ • 检查更新依赖                                   │');
  WriteLn('  └─────────────────────────────────────────────────┘');
  WriteLn;
  WriteLn('  Day 76-85: 更新窗口期');
  WriteLn('  ┌─────────────────────────────────────────────────┐');
  WriteLn('  │ ⚠️  距离过期 < 15 天                              │');
  WriteLn('  │ → 执行自动更新                                   │');
  WriteLn('  │ → 验证新证书                                     │');
  WriteLn('  │ → 部署到服务器                                   │');
  WriteLn('  │ → 重启服务（如需要）                             │');
  WriteLn('  └─────────────────────────────────────────────────┘');
  WriteLn;
  WriteLn('  Day 86-90: 紧急更新期');
  WriteLn('  ┌─────────────────────────────────────────────────┐');
  WriteLn('  │ 🚨 距离过期 < 5 天                                │');
  WriteLn('  │ → 紧急人工介入                                   │');
  WriteLn('  │ → 加急更新流程                                   │');
  WriteLn('  │ → 准备降级方案                                   │');
  WriteLn('  └─────────────────────────────────────────────────┘');
  WriteLn;
  WriteLn('  Day 90+: 证书过期');
  WriteLn('  ┌─────────────────────────────────────────────────┐');
  WriteLn('  │ ✗ 证书已过期                                     │');
  WriteLn('  │ ✗ 服务中断                                       │');
  WriteLn('  │ ✗ 客户端拒绝连接                                 │');
  WriteLn('  │ → 需要紧急更新和服务重启                         │');
  WriteLn('  └─────────────────────────────────────────────────┘');
  WriteLn;
end;

{ 演示证书检查 }
procedure DemonstrateCertificateCheck;
var
  LCerts: array[1..3] of TCertificateInfo;
  i: Integer;
  LNow: TDateTime;
begin
  WriteLn('[2/5] 证书过期检查');
  WriteLn;
  
  LNow := Now;
  
  // 模拟不同状态的证书
  LCerts[1].Name := 'www.example.com';
  LCerts[1].Subject := 'CN=www.example.com';
  LCerts[1].Issuer := 'CN=Let''s Encrypt Authority X3';
  LCerts[1].NotBefore := IncDay(LNow, -60);
  LCerts[1].NotAfter := IncDay(LNow, 30);
  LCerts[1].DaysUntilExpiry := 30;
  
  LCerts[2].Name := 'api.example.com';
  LCerts[2].Subject := 'CN=api.example.com';
  LCerts[2].Issuer := 'CN=Let''s Encrypt Authority X3';
  LCerts[2].NotBefore := IncDay(LNow, -80);
  LCerts[2].NotAfter := IncDay(LNow, 10);
  LCerts[2].DaysUntilExpiry := 10;
  
  LCerts[3].Name := 'old.example.com';
  LCerts[3].Subject := 'CN=old.example.com';
  LCerts[3].Issuer := 'CN=Let''s Encrypt Authority X3';
  LCerts[3].NotBefore := IncDay(LNow, -93);
  LCerts[3].NotAfter := IncDay(LNow, -3);
  LCerts[3].DaysUntilExpiry := -3;
  
  WriteLn('  扫描证书状态...');
  WriteLn;
  
  for i := 1 to 3 do
  begin
    WriteLn('  证书 ', i, ': ', LCerts[i].Name);
    WriteLn('    主题：', LCerts[i].Subject);
    WriteLn('    颁发者：', LCerts[i].Issuer);
    WriteLn('    有效期：', FormatDate(LCerts[i].NotBefore), 
             ' 至 ', FormatDate(LCerts[i].NotAfter));
    
    if LCerts[i].DaysUntilExpiry < 0 then
    begin
      WriteLn('    状态：✗ 已过期 ', Abs(LCerts[i].DaysUntilExpiry), ' 天');
      WriteLn('    操作：🚨 立即更新！服务可能中断');
    end
    else if LCerts[i].DaysUntilExpiry < 7 then
    begin
      WriteLn('    状态：🚨 ', LCerts[i].DaysUntilExpiry, ' 天后过期');
      WriteLn('    操作：紧急更新，人工介入');
    end
    else if LCerts[i].DaysUntilExpiry < 15 then
    begin
      WriteLn('    状态：⚠️  ', LCerts[i].DaysUntilExpiry, ' 天后过期');
      WriteLn('    操作：触发自动更新流程');
    end
    else if LCerts[i].DaysUntilExpiry < 30 then
    begin
      WriteLn('    状态：ℹ️  ', LCerts[i].DaysUntilExpiry, ' 天后过期');
      WriteLn('    操作：开始监控，准备更新');
    end
    else
    begin
      WriteLn('    状态：✓ 正常（', LCerts[i].DaysUntilExpiry, ' 天有效期）');
      WriteLn('    操作：无需操作');
    end;
    
    WriteLn;
  end;
end;

{ 说明自动更新流程 }
procedure ExplainRenewalProcess;
begin
  WriteLn('[3/5] 证书自动更新流程');
  WriteLn;
  
  WriteLn('  ACME 协议自动更新（Let''s Encrypt）：');
  WriteLn('  ═══════════════════════════════════════════════════════');
  WriteLn;
  WriteLn('  步骤 1: 监控和检测');
  WriteLn('    ┌─────────────────────┐');
  WriteLn('    │ 定时任务（每天）     │');
  WriteLn('    │ • 检查证书过期时间   │');
  WriteLn('    │ • 距离 < 30 天？     │');
  WriteLn('    └──────────┬──────────┘');
  WriteLn('               │ 是');
  WriteLn('               ▼');
  WriteLn;
  WriteLn('  步骤 2: 触发更新');
  WriteLn('    ┌─────────────────────┐');
  WriteLn('    │ ACME 客户端         │');
  WriteLn('    │ • 向 CA 请求新证书  │');
  WriteLn('    │ • 使用账户密钥认证  │');
  WriteLn('    └──────────┬──────────┘');
  WriteLn('               │');
  WriteLn('               ▼');
  WriteLn;
  WriteLn('  步骤 3: 域名验证');
  WriteLn('    ┌─────────────────────┐');
  WriteLn('    │ 验证域名所有权       │');
  WriteLn('    │ • HTTP-01: 网站文件 │');
  WriteLn('    │ • DNS-01: TXT 记录  │');
  WriteLn('    │ • TLS-ALPN-01       │');
  WriteLn('    └──────────┬──────────┘');
  WriteLn('               │');
  WriteLn('               ▼');
  WriteLn;
  WriteLn('  步骤 4: 证书颁发');
  WriteLn('    ┌─────────────────────┐');
  WriteLn('    │ 下载新证书           │');
  WriteLn('    │ • 证书文件          │');
  WriteLn('    │ • 证书链            │');
  WriteLn('    │ • 保存到磁盘        │');
  WriteLn('    └──────────┬──────────┘');
  WriteLn('               │');
  WriteLn('               ▼');
  WriteLn;
  WriteLn('  步骤 5: 部署和验证');
  WriteLn('    ┌─────────────────────┐');
  WriteLn('    │ 部署新证书           │');
  WriteLn('    │ • 备份旧证书        │');
  WriteLn('    │ • 复制新证书        │');
  WriteLn('    │ • 验证证书格式      │');
  WriteLn('    │ • 检查证书链        │');
  WriteLn('    └──────────┬──────────┘');
  WriteLn('               │');
  WriteLn('               ▼');
  WriteLn;
  WriteLn('  步骤 6: 服务重启');
  WriteLn('    ┌─────────────────────┐');
  WriteLn('    │ 重新加载证书         │');
  WriteLn('    │ • 优雅重启服务      │');
  WriteLn('    │ • 或热重载证书      │');
  WriteLn('    └──────────┬──────────┘');
  WriteLn('               │');
  WriteLn('               ▼');
  WriteLn;
  WriteLn('  步骤 7: 验证和通知');
  WriteLn('    ┌─────────────────────┐');
  WriteLn('    │ 验证新证书有效       │');
  WriteLn('    │ • TLS 握手测试      │');
  WriteLn('    │ • 发送成功通知      │');
  WriteLn('    │ • 记录更新日志      │');
  WriteLn('    └─────────────────────┘');
  WriteLn;
end;

{ 说明实现方案 }
procedure ExplainImplementations;
begin
  WriteLn('[4/5] 实现方案对比');
  WriteLn;
  
  WriteLn('  方案 1: Let''s Encrypt + Certbot');
  WriteLn('  ──────────────────────────────────────');
  WriteLn('  优势：');
  WriteLn('    ✓ 免费');
  WriteLn('    ✓ 自动化');
  WriteLn('    ✓ 90 天有效期');
  WriteLn('    ✓ 广泛支持');
  WriteLn;
  WriteLn('  配置：');
  WriteLn('    # 安装 certbot');
  WriteLn('    apt-get install certbot');
  WriteLn;
  WriteLn('    # 自动更新（每天检查）');
  WriteLn('    certbot renew --deploy-hook "systemctl reload nginx"');
  WriteLn;
  WriteLn('    # 添加到 crontab');
  WriteLn('    0 0,12 * * * certbot renew --quiet');
  WriteLn;
  
  WriteLn('  方案 2: cert-manager (Kubernetes)');
  WriteLn('  ──────────────────────────────────────');
  WriteLn('  优势：');
  WriteLn('    ✓ K8s 原生');
  WriteLn('    ✓ 声明式配置');
  WriteLn('    ✓ 多种 Issuer');
  WriteLn('    ✓ 自动化程度高');
  WriteLn;
  WriteLn('  配置：');
  WriteLn('    apiVersion: cert-manager.io/v1');
  WriteLn('    kind: Certificate');
  WriteLn('    metadata:');
  WriteLn('      name: example-com');
  WriteLn('    spec:');
  WriteLn('      secretName: example-com-tls');
  WriteLn('      duration: 2160h  # 90d');
  WriteLn('      renewBefore: 360h  # 15d');
  WriteLn('      issuerRef:');
  WriteLn('        name: letsencrypt-prod');
  WriteLn('      dnsNames:');
  WriteLn('      - example.com');
  WriteLn;
  
  WriteLn('  方案 3: 云服务商托管');
  WriteLn('  ──────────────────────────────────────');
  WriteLn('  AWS Certificate Manager:');
  WriteLn('    ✓ 全托管');
  WriteLn('    ✓ 自动更新');
  WriteLn('    ✓ 与 AWS 服务集成');
  WriteLn('    ✓ 免费（用于 AWS 服务）');
  WriteLn;
  WriteLn('  Azure Key Vault:');
  WriteLn('    ✓ 集中管理');
  WriteLn('    ✓ 自动轮换');
  WriteLn('    ✓ 访问控制');
  WriteLn('    ✓ 审计日志');
  WriteLn;
  
  WriteLn('  方案 4: 自建 ACME 客户端');
  WriteLn('  ──────────────────────────────────────');
  WriteLn('  适用于：');
  WriteLn('    • 特殊环境要求');
  WriteLn('    • 自定义更新逻辑');
  WriteLn('    • 与现有系统集成');
  WriteLn;
  WriteLn('  使用 fafafa.ssl 实现：');
  WriteLn('    1. 定时检查证书（System.Threading.Timer）');
  WriteLn('    2. 调用 ACME API 请求新证书');
  WriteLn('    3. 验证域名所有权');
  WriteLn('    4. 下载并部署新证书');
  WriteLn('    5. 重新加载 SSL 上下文');
  WriteLn;
end;

{ 最佳实践和总结 }
procedure ShowBestPractices;
begin
  WriteLn('[5/5] 最佳实践');
  WriteLn;
  
  WriteLn('  ✓ 监控和告警：');
  WriteLn('    • 30 天前：开始告警');
  WriteLn('    • 15 天前：触发自动更新');
  WriteLn('    • 7 天前：人工干预');
  WriteLn('    • 使用 Prometheus + AlertManager');
  WriteLn;
  
  WriteLn('  ✓ 更新策略：');
  WriteLn('    • 在低峰期更新');
  WriteLn('    • 先更新测试环境');
  WriteLn('    • 保留旧证书备份');
  WriteLn('    • 准备回滚方案');
  WriteLn;
  
  WriteLn('  ✓ 验证流程：');
  WriteLn('    • 检查证书有效期');
  WriteLn('    • 验证域名匹配');
  WriteLn('    • 测试 TLS 握手');
  WriteLn('    • 监控告警状态');
  WriteLn;
  
  WriteLn('  ✓ 日志和审计：');
  WriteLn('    • 记录所有更新操作');
  WriteLn('    • 保存证书变更历史');
  WriteLn('    • 设置失败通知');
  WriteLn('    • 定期审查日志');
  WriteLn;
  
  WriteLn('  ✓ 安全考虑：');
  WriteLn('    • 保护私钥安全（权限 400）');
  WriteLn('    • 使用 HSM 存储关键密钥');
  WriteLn('    • 定期轮换证书（即使未过期）');
  WriteLn('    • 实施吊销机制');
  WriteLn;
  
  WriteLn('================================================================================');
  WriteLn('  ✓ 示例执行完成！');
  WriteLn('================================================================================');
  WriteLn;
  
  WriteLn('💡 关键要点：');
  WriteLn('  1. 证书会过期，自动更新是必需的');
  WriteLn('  2. 提前 15-30 天开始更新流程');
  WriteLn('  3. 使用成熟的自动化工具（Let''s Encrypt）');
  WriteLn('  4. 监控证书状态，设置多级告警');
  WriteLn('  5. 测试更新流程，准备回滚方案');
  WriteLn;
  
  WriteLn('🔗 相关工具：');
  WriteLn('  - Certbot: https://certbot.eff.org/');
  WriteLn('  - cert-manager: https://cert-manager.io/');
  WriteLn('  - ACME.sh: https://acme.sh/');
  WriteLn('  - Caddy: 内置自动 HTTPS');
  WriteLn('  - Traefik: 自动证书管理');
  WriteLn;
  
  WriteLn('📚 相关示例：');
  WriteLn('  - 示例 02: 证书生成');
  WriteLn('  - 示例 07: 证书链验证');
  WriteLn('  - 示例 08: 双向 TLS 认证');
  WriteLn;
  
  WriteLn('⚠️  生产环境检查清单：');
  WriteLn('  □ 启用自动更新（certbot/cert-manager）');
  WriteLn('  □ 配置监控告警（30/15/7 天）');
  WriteLn('  □ 测试更新流程（staging 环境）');
  WriteLn('  □ 配置通知（邮件/Slack/PagerDuty）');
  WriteLn('  □ 备份策略（旧证书保留）');
  WriteLn('  □ 回滚方案（快速恢复）');
  WriteLn('  □ 文档记录（更新流程）');
  WriteLn('  □ 定期演练（每季度）');
  WriteLn;
end;

begin
  try
    ExplainCertificateLifecycle;
    DemonstrateCertificateCheck;
    ExplainRenewalProcess;
    ExplainImplementations;
    ShowBestPractices;
    
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

