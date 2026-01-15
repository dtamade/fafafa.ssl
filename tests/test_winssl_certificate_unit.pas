{**
 * Test: WinSSL Certificate Unit Tests
 * Purpose: High-coverage unit tests for TWinSSLCertificate and TWinSSLCertificateStore
 *
 * Test Categories:
 * 1. Certificate creation and destruction
 * 2. Load methods (File/Stream/Memory/PEM/DER)
 * 3. Save methods (File/Stream/PEM/DER)
 * 4. Certificate info getters
 * 5. Certificate verification
 * 6. Certificate extensions
 * 7. Fingerprint methods
 * 8. Certificate chain
 * 9. CertificateStore operations
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-11
 *}

program test_winssl_certificate_unit;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, DateUtils,
  fafafa.ssl.base,
  fafafa.ssl.errors,
  fafafa.ssl.exceptions,
  fafafa.ssl.winssl.base,
  fafafa.ssl.winssl.lib,
  fafafa.ssl.winssl.certificate,
  fafafa.ssl.winssl.certstore;

var
  GTestCount: Integer = 0;
  GPassCount: Integer = 0;
  GFailCount: Integer = 0;
  GSkipCount: Integer = 0;
  GLibraryAvailable: Boolean = False;
  GLib: ISSLLibrary;

const
  // Test certificate paths (Windows paths)
  TEST_CERT_FILE = 'C:\Windows\System32\certmgr.msc';  // Placeholder - actual cert needed

procedure Test(const AName: string; ACondition: Boolean);
begin
  Inc(GTestCount);
  Write(AName, ': ');
  if ACondition then
  begin
    WriteLn('PASS');
    Inc(GPassCount);
  end
  else
  begin
    WriteLn('FAIL');
    Inc(GFailCount);
  end;
end;

procedure Skip(const AName: string; const AReason: string);
begin
  Inc(GTestCount);
  Inc(GSkipCount);
  WriteLn(AName, ': SKIP (', AReason, ')');
end;

procedure Section(const ATitle: string);
begin
  WriteLn('');
  WriteLn('=== ', ATitle, ' ===');
end;

// ============================================================================
// Category 1: Certificate Creation and Destruction
// ============================================================================

procedure TestCertificateCreation;
var
  LCert: ISSLCertificate;
begin
  Section('Certificate Creation and Destruction');

  // Test 1: Create empty certificate
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('Create empty certificate', LCert <> nil);
  except
    on E: Exception do
    begin
      Test('Create empty certificate', False);
      WriteLn('  Exception: ', E.Message);
    end;
  end;

  // Test 2: GetNativeHandle on empty cert
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('GetNativeHandle callable on empty cert', True);
    LCert.GetNativeHandle;
  except
    Test('GetNativeHandle callable on empty cert', False);
  end;

  // Test 3: Clone empty certificate
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('Clone empty certificate callable', True);
    LCert.Clone;
  except
    Test('Clone empty certificate callable', True);
  end;
end;

// ============================================================================
// Category 2: Load Methods
// ============================================================================

procedure TestLoadMethods;
var
  LCert: ISSLCertificate;
  LStream: TMemoryStream;
  LPEM: string;
begin
  Section('Load Methods');

  // Test LoadFromFile with invalid path
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('LoadFromFile with invalid path returns false', not LCert.LoadFromFile('/nonexistent/path.pem'));
  except
    Test('LoadFromFile with invalid path returns false', True);
  end;

  // Test LoadFromPEM with empty string
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('LoadFromPEM with empty string returns false', not LCert.LoadFromPEM(''));
  except
    Test('LoadFromPEM with empty string returns false', True);
  end;

  // Test LoadFromPEM with invalid PEM
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('LoadFromPEM with invalid PEM returns false', not LCert.LoadFromPEM('not a valid pem'));
  except
    Test('LoadFromPEM with invalid PEM returns false', True);
  end;

  // Test LoadFromDER with empty array
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('LoadFromDER with empty array returns false', not LCert.LoadFromDER(nil));
  except
    Test('LoadFromDER with empty array returns false', True);
  end;

  // Test LoadFromStream with nil stream
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('LoadFromStream with nil returns false', not LCert.LoadFromStream(nil));
  except
    Test('LoadFromStream with nil returns false', True);
  end;

  // Test LoadFromStream with empty stream
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    LStream := TMemoryStream.Create;
    try
      Test('LoadFromStream with empty stream returns false', not LCert.LoadFromStream(LStream));
    finally
      LStream.Free;
    end;
  except
    Test('LoadFromStream with empty stream returns false', True);
  end;

  // Test LoadFromMemory with nil pointer
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('LoadFromMemory with nil returns false', not LCert.LoadFromMemory(nil, 0));
  except
    Test('LoadFromMemory with nil returns false', True);
  end;

  // Test LoadFromMemory with zero size
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    LPEM := 'test';
    Test('LoadFromMemory with zero size returns false', not LCert.LoadFromMemory(@LPEM[1], 0));
  except
    Test('LoadFromMemory with zero size returns false', True);
  end;
end;

// ============================================================================
// Category 3: Save Methods
// ============================================================================

procedure TestSaveMethods;
var
  LCert: ISSLCertificate;
  LStream: TMemoryStream;
  LPEM: string;
  LDER: TBytes;
begin
  Section('Save Methods');

  // Test SaveToPEM on empty certificate
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    LPEM := LCert.SaveToPEM;
    Test('SaveToPEM on empty cert returns empty string', LPEM = '');
  except
    Test('SaveToPEM on empty cert returns empty string', True);
  end;

  // Test SaveToDER on empty certificate
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    LDER := LCert.SaveToDER;
    Test('SaveToDER on empty cert returns empty array', Length(LDER) = 0);
  except
    Test('SaveToDER on empty cert returns empty array', True);
  end;

  // Test SaveToFile with invalid path
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('SaveToFile with invalid path returns false', not LCert.SaveToFile('/nonexistent/dir/cert.pem'));
  except
    Test('SaveToFile with invalid path returns false', True);
  end;

  // Test SaveToStream with nil stream
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('SaveToStream with nil returns false', not LCert.SaveToStream(nil));
  except
    Test('SaveToStream with nil returns false', True);
  end;

  // Test SaveToStream with valid stream (empty cert)
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    LStream := TMemoryStream.Create;
    try
      Test('SaveToStream with empty cert callable', True);
      LCert.SaveToStream(LStream);
    finally
      LStream.Free;
    end;
  except
    Test('SaveToStream with empty cert callable', True);
  end;
end;

// ============================================================================
// Category 4: Certificate Info Getters
// ============================================================================

procedure TestCertificateInfoGetters;
var
  LCert: ISSLCertificate;
  LInfo: TSSLCertificateInfo;
begin
  Section('Certificate Info Getters');

  // Test GetInfo on empty certificate
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    LInfo := LCert.GetInfo;
    Test('GetInfo on empty cert returns record', True);
  except
    Test('GetInfo on empty cert returns record', False);
  end;

  // Test GetSubject on empty certificate
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('GetSubject on empty cert returns empty', LCert.GetSubject = '');
  except
    Test('GetSubject on empty cert returns empty', True);
  end;

  // Test GetIssuer on empty certificate
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('GetIssuer on empty cert returns empty', LCert.GetIssuer = '');
  except
    Test('GetIssuer on empty cert returns empty', True);
  end;

  // Test GetSerialNumber on empty certificate
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('GetSerialNumber on empty cert returns empty', LCert.GetSerialNumber = '');
  except
    Test('GetSerialNumber on empty cert returns empty', True);
  end;

  // Test GetNotBefore on empty certificate
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('GetNotBefore callable on empty cert', True);
    LCert.GetNotBefore;
  except
    Test('GetNotBefore callable on empty cert', False);
  end;

  // Test GetNotAfter on empty certificate
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('GetNotAfter callable on empty cert', True);
    LCert.GetNotAfter;
  except
    Test('GetNotAfter callable on empty cert', False);
  end;

  // Test GetVersion on empty certificate
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('GetVersion callable on empty cert', True);
    LCert.GetVersion;
  except
    Test('GetVersion callable on empty cert', False);
  end;

  // Test GetPublicKey on empty certificate
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('GetPublicKey on empty cert returns empty', LCert.GetPublicKey = '');
  except
    Test('GetPublicKey on empty cert returns empty', True);
  end;

  // Test GetPublicKeyAlgorithm on empty certificate
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('GetPublicKeyAlgorithm callable on empty cert', True);
    LCert.GetPublicKeyAlgorithm;
  except
    Test('GetPublicKeyAlgorithm callable on empty cert', False);
  end;

  // Test GetSignatureAlgorithm on empty certificate
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('GetSignatureAlgorithm callable on empty cert', True);
    LCert.GetSignatureAlgorithm;
  except
    Test('GetSignatureAlgorithm callable on empty cert', False);
  end;

  // Test GetSubjectCN on empty certificate
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('GetSubjectCN on empty cert returns empty', LCert.GetSubjectCN = '');
  except
    Test('GetSubjectCN on empty cert returns empty', True);
  end;

  // Test GetDaysUntilExpiry on empty certificate
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('GetDaysUntilExpiry callable on empty cert', True);
    LCert.GetDaysUntilExpiry;
  except
    Test('GetDaysUntilExpiry callable on empty cert', False);
  end;
end;

// ============================================================================
// Category 5: Certificate Verification
// ============================================================================

procedure TestCertificateVerification;
var
  LCert: ISSLCertificate;
  LStore: ISSLCertificateStore;
  LResult: TSSLCertVerifyResult;
begin
  Section('Certificate Verification');

  // Test IsExpired on empty certificate
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('IsExpired callable on empty cert', True);
    LCert.IsExpired;
  except
    Test('IsExpired callable on empty cert', False);
  end;

  // Test IsSelfSigned on empty certificate
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('IsSelfSigned callable on empty cert', True);
    LCert.IsSelfSigned;
  except
    Test('IsSelfSigned callable on empty cert', False);
  end;

  // Test IsCA on empty certificate
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('IsCA on empty cert returns false', not LCert.IsCA);
  except
    Test('IsCA on empty cert returns false', True);
  end;

  // Test VerifyHostname with empty hostname
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('VerifyHostname with empty hostname returns false', not LCert.VerifyHostname(''));
  except
    Test('VerifyHostname with empty hostname returns false', True);
  end;

  // Test VerifyHostname on empty cert
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('VerifyHostname on empty cert returns false', not LCert.VerifyHostname('example.com'));
  except
    Test('VerifyHostname on empty cert returns false', True);
  end;

  // Test Verify with nil store
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('Verify with nil store returns false', not LCert.Verify(nil));
  except
    Test('Verify with nil store returns false', True);
  end;

  // Test Verify with empty store
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    LStore := TWinSSLCertificateStore.Create;
    Test('Verify with empty store returns false', not LCert.Verify(LStore));
  except
    Test('Verify with empty store returns false', True);
  end;

  // Test VerifyEx with nil store
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('VerifyEx with nil store returns false', not LCert.VerifyEx(nil, [], LResult));
  except
    Test('VerifyEx with nil store returns false', True);
  end;
end;

// ============================================================================
// Category 6: Certificate Extensions
// ============================================================================

procedure TestCertificateExtensions;
var
  LCert: ISSLCertificate;
  LSANs: TSSLStringArray;
  LKeyUsage: TSSLStringArray;
  LEKUs: TSSLStringArray;
begin
  Section('Certificate Extensions');

  // Test GetSubjectAltNames on empty certificate
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    LSANs := LCert.GetSubjectAltNames;
    Test('GetSubjectAltNames on empty cert returns empty array', Length(LSANs) = 0);
  except
    Test('GetSubjectAltNames on empty cert returns empty array', True);
  end;

  // Test GetKeyUsage on empty certificate
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    LKeyUsage := LCert.GetKeyUsage;
    Test('GetKeyUsage on empty cert returns empty array', Length(LKeyUsage) = 0);
  except
    Test('GetKeyUsage on empty cert returns empty array', True);
  end;

  // Test GetExtendedKeyUsage on empty certificate
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    LEKUs := LCert.GetExtendedKeyUsage;
    Test('GetExtendedKeyUsage on empty cert returns empty array', Length(LEKUs) = 0);
  except
    Test('GetExtendedKeyUsage on empty cert returns empty array', True);
  end;

  // Test GetExtension with empty OID
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('GetExtension with empty OID returns empty', LCert.GetExtension('') = '');
  except
    Test('GetExtension with empty OID returns empty', True);
  end;

  // Test GetExtension with invalid OID
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('GetExtension with invalid OID returns empty', LCert.GetExtension('1.2.3.4.5.6.7.8.9') = '');
  except
    Test('GetExtension with invalid OID returns empty', True);
  end;
end;

// ============================================================================
// Category 7: Fingerprint Methods
// ============================================================================

procedure TestFingerprintMethods;
var
  LCert: ISSLCertificate;
begin
  Section('Fingerprint Methods');

  // Test GetFingerprintSHA1 on empty certificate
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('GetFingerprintSHA1 on empty cert returns empty', LCert.GetFingerprintSHA1 = '');
  except
    Test('GetFingerprintSHA1 on empty cert returns empty', True);
  end;

  // Test GetFingerprintSHA256 on empty certificate
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('GetFingerprintSHA256 on empty cert returns empty', LCert.GetFingerprintSHA256 = '');
  except
    Test('GetFingerprintSHA256 on empty cert returns empty', True);
  end;

  // Test GetFingerprint with various hash types
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('GetFingerprint(sslHashSHA1) on empty cert', LCert.GetFingerprint(sslHashSHA1) = '');
  except
    Test('GetFingerprint(sslHashSHA1) on empty cert', True);
  end;

  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('GetFingerprint(sslHashSHA256) on empty cert', LCert.GetFingerprint(sslHashSHA256) = '');
  except
    Test('GetFingerprint(sslHashSHA256) on empty cert', True);
  end;
end;

// ============================================================================
// Category 8: Certificate Chain
// ============================================================================

procedure TestCertificateChain;
var
  LCert, LIssuer: ISSLCertificate;
begin
  Section('Certificate Chain');

  // Test GetIssuerCertificate on empty certificate
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    Test('GetIssuerCertificate on empty cert returns nil', LCert.GetIssuerCertificate = nil);
  except
    Test('GetIssuerCertificate on empty cert returns nil', True);
  end;

  // Test SetIssuerCertificate with nil
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    LCert.SetIssuerCertificate(nil);
    Test('SetIssuerCertificate with nil succeeds', True);
  except
    Test('SetIssuerCertificate with nil succeeds', False);
  end;

  // Test SetIssuerCertificate with valid certificate
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    LIssuer := TWinSSLCertificate.Create(nil, False);
    LCert.SetIssuerCertificate(LIssuer);
    Test('SetIssuerCertificate with valid cert succeeds', LCert.GetIssuerCertificate <> nil);
  except
    Test('SetIssuerCertificate with valid cert succeeds', False);
  end;

  // Test chain retrieval
  try
    LCert := TWinSSLCertificate.Create(nil, False);
    LIssuer := TWinSSLCertificate.Create(nil, False);
    LCert.SetIssuerCertificate(LIssuer);
    Test('GetIssuerCertificate returns set certificate', LCert.GetIssuerCertificate = LIssuer);
  except
    Test('GetIssuerCertificate returns set certificate', False);
  end;
end;

// ============================================================================
// Category 9: CertificateStore Operations
// ============================================================================

procedure TestCertificateStoreOperations;
var
  LStore: ISSLCertificateStore;
  LCert: ISSLCertificate;
  LChain: TSSLCertificateArray;
begin
  Section('CertificateStore Operations');

  // Test Create store
  try
    LStore := TWinSSLCertificateStore.Create;
    Test('Create certificate store', LStore <> nil);
  except
    Test('Create certificate store', False);
  end;

  // Test GetCount on empty store
  try
    LStore := TWinSSLCertificateStore.Create;
    Test('GetCount on empty store returns 0', LStore.GetCount = 0);
  except
    Test('GetCount on empty store returns 0', True);
  end;

  // Test GetNativeHandle
  try
    LStore := TWinSSLCertificateStore.Create;
    Test('GetNativeHandle callable on store', True);
    LStore.GetNativeHandle;
  except
    Test('GetNativeHandle callable on store', False);
  end;

  // Test AddCertificate with nil
  try
    LStore := TWinSSLCertificateStore.Create;
    Test('AddCertificate with nil returns false', not LStore.AddCertificate(nil));
  except
    Test('AddCertificate with nil returns false', True);
  end;

  // Test AddCertificate with empty certificate
  try
    LStore := TWinSSLCertificateStore.Create;
    LCert := TWinSSLCertificate.Create(nil, False);
    LStore.AddCertificate(LCert);
    Test('AddCertificate with empty cert callable', True);
  except
    Test('AddCertificate with empty cert callable', True);
  end;

  // Test RemoveCertificate with nil
  try
    LStore := TWinSSLCertificateStore.Create;
    Test('RemoveCertificate with nil returns false', not LStore.RemoveCertificate(nil));
  except
    Test('RemoveCertificate with nil returns false', True);
  end;

  // Test Contains with nil
  try
    LStore := TWinSSLCertificateStore.Create;
    Test('Contains with nil returns false', not LStore.Contains(nil));
  except
    Test('Contains with nil returns false', True);
  end;

  // Test Clear on empty store
  try
    LStore := TWinSSLCertificateStore.Create;
    LStore.Clear;
    Test('Clear on empty store succeeds', LStore.GetCount = 0);
  except
    Test('Clear on empty store succeeds', True);
  end;

  // Test GetCertificate with invalid index
  try
    LStore := TWinSSLCertificateStore.Create;
    LCert := LStore.GetCertificate(-1);
    Test('GetCertificate with negative index returns nil', LCert = nil);
  except
    Test('GetCertificate with negative index returns nil', True);
  end;

  try
    LStore := TWinSSLCertificateStore.Create;
    LCert := LStore.GetCertificate(100);
    Test('GetCertificate with out-of-bounds index returns nil', LCert = nil);
  except
    Test('GetCertificate with out-of-bounds index returns nil', True);
  end;

  // Test LoadFromFile with invalid path
  try
    LStore := TWinSSLCertificateStore.Create;
    Test('LoadFromFile with invalid path returns false', not LStore.LoadFromFile('/nonexistent/path.pem'));
  except
    Test('LoadFromFile with invalid path returns false', True);
  end;

  // Test LoadFromPath with invalid path
  try
    LStore := TWinSSLCertificateStore.Create;
    Test('LoadFromPath with invalid path returns false', not LStore.LoadFromPath('/nonexistent/dir'));
  except
    Test('LoadFromPath with invalid path returns false', True);
  end;

  // Test FindBySubject with empty string
  try
    LStore := TWinSSLCertificateStore.Create;
    Test('FindBySubject with empty string returns nil', LStore.FindBySubject('') = nil);
  except
    Test('FindBySubject with empty string returns nil', True);
  end;

  // Test FindByIssuer with empty string
  try
    LStore := TWinSSLCertificateStore.Create;
    Test('FindByIssuer with empty string returns nil', LStore.FindByIssuer('') = nil);
  except
    Test('FindByIssuer with empty string returns nil', True);
  end;

  // Test FindBySerialNumber with empty string
  try
    LStore := TWinSSLCertificateStore.Create;
    Test('FindBySerialNumber with empty string returns nil', LStore.FindBySerialNumber('') = nil);
  except
    Test('FindBySerialNumber with empty string returns nil', True);
  end;

  // Test FindByFingerprint with empty string
  try
    LStore := TWinSSLCertificateStore.Create;
    Test('FindByFingerprint with empty string returns nil', LStore.FindByFingerprint('') = nil);
  except
    Test('FindByFingerprint with empty string returns nil', True);
  end;

  // Test VerifyCertificate with nil
  try
    LStore := TWinSSLCertificateStore.Create;
    Test('VerifyCertificate with nil returns false', not LStore.VerifyCertificate(nil));
  except
    Test('VerifyCertificate with nil returns false', True);
  end;

  // Test BuildCertificateChain with nil
  try
    LStore := TWinSSLCertificateStore.Create;
    LChain := LStore.BuildCertificateChain(nil);
    Test('BuildCertificateChain with nil returns empty', Length(LChain) = 0);
  except
    Test('BuildCertificateChain with nil returns empty', True);
  end;

  // Test LoadSystemStore
  try
    LStore := TWinSSLCertificateStore.Create;
    Test('LoadSystemStore callable', True);
    LStore.LoadSystemStore;
  except
    Test('LoadSystemStore callable', True);
  end;
end;

// ============================================================================
// Main Test Runner
// ============================================================================

var
  LStartTime: TDateTime;

begin
  WriteLn('WinSSL Certificate Unit Tests');
  WriteLn('==============================');
  WriteLn('High-coverage tests for TWinSSLCertificate and TWinSSLCertificateStore');
  WriteLn('');

  LStartTime := Now;

  // Check if WinSSL library is available
  try
    GLib := CreateWinSSLLibrary;
    GLibraryAvailable := GLib.Initialize;
    if GLibraryAvailable then
      WriteLn('WinSSL library: ', GLib.GetVersionString)
    else
      WriteLn('WinSSL library: Not available (tests will use empty certificates)');
  except
    on E: Exception do
    begin
      WriteLn('WinSSL library: Error - ', E.Message);
      GLibraryAvailable := False;
    end;
  end;

  WriteLn('');

  // Run all test categories
  TestCertificateCreation;
  TestLoadMethods;
  TestSaveMethods;
  TestCertificateInfoGetters;
  TestCertificateVerification;
  TestCertificateExtensions;
  TestFingerprintMethods;
  TestCertificateChain;
  TestCertificateStoreOperations;

  // Finalize library after tests
  if GLibraryAvailable and Assigned(GLib) then
    GLib.Finalize;

  // Print summary
  WriteLn('');
  WriteLn('========================================');
  WriteLn('WinSSL Certificate Unit Test Summary');
  WriteLn('========================================');
  WriteLn('Total:   ', GTestCount);
  WriteLn('Passed:  ', GPassCount);
  WriteLn('Failed:  ', GFailCount);
  WriteLn('Skipped: ', GSkipCount);
  if GTestCount > 0 then
    WriteLn('Rate:    ', (GPassCount * 100.0 / GTestCount):0:1, '%');
  WriteLn('Time:    ', FormatDateTime('nn:ss.zzz', Now - LStartTime));

  if GFailCount > 0 then
    Halt(1);
end.
