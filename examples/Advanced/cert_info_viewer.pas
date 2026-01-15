program cert_info_viewer;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.cert.utils;

procedure PrintBanner;
begin
  WriteLn;
  WriteLn('=====================================');
  WriteLn('  Certificate Information Viewer');
  WriteLn('  fafafa.ssl toolkit');
  WriteLn('=====================================');
  WriteLn;
end;

procedure PrintUsage;
begin
  WriteLn('Usage: cert_info_viewer <certificate.pem>');
  WriteLn;
  WriteLn('Displays detailed information about an X.509 certificate.');
  WriteLn;
end;

procedure DisplayCertInfo(const AFileName: string);
var
  LCertPEM: string;
  LInfo: TCertInfo;
  i: Integer;
begin
  WriteLn('Reading certificate: ', AFileName);
  
  // Load certificate
  try
    LCertPEM := TCertificateUtils.LoadFromFile(AFileName);
  except
    on E: Exception do
    begin
      WriteLn('ERROR: Failed to load certificate: ', E.Message);
      Halt(1);
    end;
  end;
  
  // Get certificate info
  LInfo := TCertificateUtils.GetInfo(LCertPEM);
  
  // Display information
  WriteLn;
  WriteLn('---- Certificate Details ----');
  WriteLn;
  WriteLn('Subject    : ', LInfo.Subject);
  WriteLn('Issuer     : ', LInfo.Issuer);
  WriteLn('Version    : X509v', LInfo.Version);
  WriteLn('Serial     : ', LInfo.SerialNumber);
  WriteLn;
  
  WriteLn('Valid From : ', DateTimeToStr(LInfo.NotBefore));
  WriteLn('Valid Until: ', DateTimeToStr(LInfo.NotAfter));
  WriteLn;
  
  if LInfo.NotAfter < Now then
    WriteLn('Status     : ⚠ EXPIRED')
  else if LInfo.NotBefore > Now then
    WriteLn('Status     : ⚠ NOT YET VALID')
  else
    WriteLn('Status     : ✓ VALID');
  WriteLn;
  
  if LInfo.PublicKeyType <> '' then
  begin
    WriteLn('Public Key : ', LInfo.PublicKeyType);
    if LInfo.PublicKeyBits > 0 then
      WriteLn('Key Bits   : ', LInfo.PublicKeyBits);
  end;
  
  if LInfo.SignatureAlgorithm <> '' then
    WriteLn('Signature  : ', LInfo.SignatureAlgorithm);
  
  WriteLn;
  WriteLn('Is CA      : ', LInfo.IsCA);
  
  if LInfo.KeyUsage <> '' then
    WriteLn('Key Usage  : ', LInfo.KeyUsage);
  
  if Assigned(LInfo.SubjectAltNames) and (LInfo.SubjectAltNames.Count > 0) then
  begin
    WriteLn;
    WriteLn('Subject Alternative Names:');
    for i := 0 to LInfo.SubjectAltNames.Count - 1 do
      WriteLn('  - ', LInfo.SubjectAltNames[i]);
  end;
  
  // Calculate fingerprint
  WriteLn;
  WriteLn('Fingerprint (SHA256):');
  WriteLn('  ', TCertificateUtils.GetFingerprint(LCertPEM));
  
  WriteLn;
  WriteLn('---- End of Certificate ----');
  WriteLn;
  
  if Assigned(LInfo.SubjectAltNames) then
    LInfo.SubjectAltNames.Free;
end;

begin
  PrintBanner;
  
  if ParamCount < 1 then
  begin
    PrintUsage;
    Halt(1);
  end;
  
  DisplayCertInfo(ParamStr(1));
end.
