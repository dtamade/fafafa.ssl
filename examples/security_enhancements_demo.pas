program SecurityEnhancementsDemo;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl,
  fafafa.ssl.context.builder,
  fafafa.ssl.cert.pinning,
  fafafa.ssl.cert.rotation;

{**
 * Security Enhancements Demo
 *
 * Demonstrates the new security features:
 * 1. Certificate Pinning (Public Key Pinning)
 * 2. Certificate Rotation with Hot Reload
 *
 * Usage:
 *   ./security_enhancements_demo
 *}

procedure DemoCertificatePinning;
var
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  Socket: THandle;
begin
  WriteLn('=== Certificate Pinning Demo ===');
  WriteLn;

  // Create SSL context
  Ctx := TSSLContextBuilder.Create
    .WithTLS12And13
    .WithVerifyPeer
    .WithSystemRoots
    .BuildClient;

  // Add certificate pins (example pins - replace with real ones)
  WriteLn('Adding certificate pins...');
  
  // Primary pin: Current certificate public key
  Ctx.AddCertificatePinBase64(
    'YLh1dUR9y6Kja30RrAn7JKnbQG/uEtLMkBgFF2Fuihg=',
    Ord(ptPublicKey),
    'Primary - Current Certificate',
    False
  );

  // Backup pin: Intermediate CA public key
  Ctx.AddCertificatePinBase64(
    'sRHdihwgkaib1P1gxX8HFszlD+7/gTfNvuAybgLPNis=',
    Ord(ptPublicKey),
    'Backup - Intermediate CA',
    True
  );

  // Enable certificate pinning
  Ctx.SetCertificatePinningEnabled(True);
  WriteLn('Certificate pinning enabled');
  WriteLn;

  WriteLn('Pinning configuration:');
  WriteLn('  - 2 pins configured (primary + backup)');
  WriteLn('  - Using public key pinning (SPKI)');
  WriteLn('  - SHA-256 hashing');
  WriteLn;

  // In a real application, you would now connect to a server
  // and the certificate pinning validation would occur automatically
  // during the TLS handshake
  
  WriteLn('Note: In production, connect to server and pinning validation');
  WriteLn('      will occur automatically during TLS handshake.');
  WriteLn;
end;

procedure RotationEventHandler(AEventType: TRotationEventType;
  const AMessage: string; const ACertPath: string);
begin
  case AEventType of
    retCertificateExpiring:
      WriteLn('[WARNING] ', AMessage);
    retCertificateExpired:
      WriteLn('[ERROR] ', AMessage);
    retFileChanged:
      WriteLn('[INFO] ', AMessage);
    retReloadSuccess:
      WriteLn('[SUCCESS] ', AMessage);
    retReloadFailed:
      WriteLn('[ERROR] ', AMessage);
  end;
end;

type
  TDemoRotationEventHandler = class
  public
    procedure HandleRotationEvent(AEventType: TRotationEventType;
      const AMessage: string; const ACertPath: string);
  end;

procedure TDemoRotationEventHandler.HandleRotationEvent(AEventType: TRotationEventType;
  const AMessage: string; const ACertPath: string);
begin
  RotationEventHandler(AEventType, AMessage, ACertPath);
end;

procedure DemoCertificateRotation;
var
  Ctx: ISSLContext;
  RotationMgr: TCertificateRotationManager;
  EventHandler: TDemoRotationEventHandler;
  Config: TRotationConfig;
  DaysRemaining: Integer;
begin
  WriteLn('=== Certificate Rotation Demo ===');
  WriteLn;

  // Create SSL context
  Ctx := TSSLContextBuilder.Create
    .WithTLS12And13
    .WithVerifyPeer
    .BuildServer;

  // Load initial certificate
  WriteLn('Loading initial certificate...');
  // In production, replace with actual certificate paths
  // Ctx.LoadCertificate('server.crt');
  // Ctx.LoadPrivateKey('server.key');
  WriteLn('(Skipped - no certificate files in demo)');
  WriteLn;

  // Create rotation manager
  RotationMgr := TCertificateRotationManager.Create(Ctx);
  EventHandler := TDemoRotationEventHandler.Create;
  try
    // Set up rotation event handler
    RotationMgr.OnRotationEvent := @EventHandler.HandleRotationEvent;

    // Configure rotation
    Config.CertificatePath := 'server.crt';
    Config.PrivateKeyPath := 'server.key';
    Config.PrivateKeyPassword := '';
    Config.ExpiryWarningDays := 30;        // Warn 30 days before expiry
    Config.CheckIntervalSeconds := 3600;   // Check every hour
    Config.AutoReloadOnChange := True;     // Auto-reload on file change
    Config.AutoReloadOnExpiry := False;    // Don't auto-reload on expiry

    WriteLn('Rotation configuration:');
    WriteLn('  - Certificate: ', Config.CertificatePath);
    WriteLn('  - Private Key: ', Config.PrivateKeyPath);
    WriteLn('  - Expiry warning: ', Config.ExpiryWarningDays, ' days');
    WriteLn('  - Check interval: ', Config.CheckIntervalSeconds, ' seconds');
    WriteLn('  - Auto-reload on change: ', Config.AutoReloadOnChange);
    WriteLn;

    // Start rotation monitoring
    WriteLn('Starting certificate rotation monitoring...');
    // In production, uncomment this:
    // if RotationMgr.Start(Config) then
    //   WriteLn('Rotation monitoring started successfully')
    // else
    //   WriteLn('Failed to start rotation monitoring');
    WriteLn('(Skipped - no certificate files in demo)');
    WriteLn;

    WriteLn('Features:');
    WriteLn('  - Monitors certificate files for changes');
    WriteLn('  - Automatically reloads on file modification');
    WriteLn('  - Checks certificate expiry daily');
    WriteLn('  - Triggers warnings before expiry');
    WriteLn('  - Zero-downtime certificate updates');
    WriteLn;

    // Check expiry status
    WriteLn('Checking certificate expiry...');
    if RotationMgr.CheckExpiry(DaysRemaining) then
      WriteLn('Certificate valid - ', DaysRemaining, ' days remaining')
    else
      WriteLn('Certificate check failed or expired');
    WriteLn;

    // Manual reload example
    WriteLn('Manual reload example:');
    WriteLn('  Call RotationMgr.ManualReload to force certificate reload');
    WriteLn;

    // Stop monitoring
    WriteLn('Stopping rotation monitoring...');
    RotationMgr.Stop;
    WriteLn('Rotation monitoring stopped');
    WriteLn;
  finally
    EventHandler.Free;
    RotationMgr.Free;
  end;
end;

procedure ShowSecurityBestPractices;
begin
  WriteLn('=== Security Best Practices ===');
  WriteLn;
  
  WriteLn('Certificate Pinning:');
  WriteLn('  1. Use public key pinning (not certificate pinning)');
  WriteLn('  2. Always include minimum 2 pins (primary + backup)');
  WriteLn('  3. Pin intermediate CA as backup');
  WriteLn('  4. Use SHA-256 for hashing');
  WriteLn('  5. Store pins in compiled code (not config files)');
  WriteLn('  6. Plan pin rotation with overlap period');
  WriteLn;

  WriteLn('Certificate Rotation:');
  WriteLn('  1. Monitor certificate expiry (30+ days warning)');
  WriteLn('  2. Enable automatic reload on file changes');
  WriteLn('  3. Test rotation in staging before production');
  WriteLn('  4. Keep backup certificates ready');
  WriteLn('  5. Monitor rotation events and failures');
  WriteLn('  6. Use short-lived certificates (90 days max)');
  WriteLn;

  WriteLn('General Security:');
  WriteLn('  1. Always use TLS 1.2 or higher');
  WriteLn('  2. Enable certificate verification');
  WriteLn('  3. Use strong cipher suites');
  WriteLn('  4. Keep OpenSSL/WinSSL updated');
  WriteLn('  5. Monitor security logs');
  WriteLn('  6. Implement defense in depth');
  WriteLn;
end;

begin
  try
    WriteLn('fafafa.ssl Security Enhancements Demo');
    WriteLn('======================================');
    WriteLn;

    // Demo 1: Certificate Pinning
    DemoCertificatePinning;
    WriteLn;

    // Demo 2: Certificate Rotation
    DemoCertificateRotation;
    WriteLn;

    // Show best practices
    ShowSecurityBestPractices;

    WriteLn('Demo completed successfully!');
    WriteLn;
    WriteLn('For production use:');
    WriteLn('  1. Replace example pins with real certificate hashes');
    WriteLn('  2. Provide actual certificate file paths');
    WriteLn('  3. Configure rotation parameters for your environment');
    WriteLn('  4. Implement proper error handling and logging');
    WriteLn('  5. Test thoroughly in staging environment');
    WriteLn;

  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
