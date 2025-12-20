program demo_facade_api;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.cert,
  fafafa.ssl.cert.builder;

var
  LCert: ICertificate;
begin
  WriteLn('====================================');
  WriteLn('  Facade API Demo - Simplest Usage');
  WriteLn('====================================');
  WriteLn;
  
  try
    // Scenario 1: One-liner certificate generation
    WriteLn('[1] One-liner: Create self-signed cert');
    TCertificate.CreateSelfSigned('localhost')
      .SaveToFiles('localhost.crt', 'localhost.key');
    WriteLn('✓ Done! localhost.crt created');
    WriteLn;
    
    // Scenario 2: Server certificate with SANs
    WriteLn('[2] Server cert with multiple SANs');
    TCertificate.CreateServerCert('api.example.com', [
      'api.example.com',
      '*.api.example.com',
      'www.example.com'
    ]).SaveToFiles('server.crt', 'server.key');
    WriteLn('✓ Done! server.crt with 3 SANs');
    WriteLn;
    
    // Scenario 3: Load and inspect
    WriteLn('[3] Load certificate and inspect');
    LCert := TCertificate.LoadFromFile('localhost.crt');
    WriteLn('  Subject: ', LCert.Subject);
    WriteLn('  Expired: ', LCert.IsExpired);
    WriteLn;
    
    // Scenario 4: Format conversion
    WriteLn('[4] PEM to DER conversion');
    WriteLn('  DER size: ', Length(TCertificate.ConvertPEMToDER(LCert.ToPEM)), ' bytes');
    WriteLn;
    
    WriteLn('====================================');
    WriteLn('✓ ALL SCENARIOS COMPLETE');
    WriteLn('====================================');
    WriteLn;
    WriteLn('Usage is now DEAD SIMPLE:');
    WriteLn('  TCertificate.CreateSelfSigned(''myapp.com'')');
    WriteLn('    .SaveToFiles(''cert.crt'', ''key.pem'');');
    WriteLn;
    
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.Message);
      Halt(1);
    end;
  end;
end.
