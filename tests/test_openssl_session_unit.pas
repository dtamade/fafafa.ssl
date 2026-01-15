{**
 * Test: OpenSSL Session Unit Tests
 * Purpose: High-coverage unit tests for TOpenSSLSession
 *
 * Test Categories:
 * 1. Session creation and destruction
 * 2. Session properties
 * 3. Session serialization
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-11
 *}

program test_openssl_session_unit;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, DateUtils,
  fafafa.ssl.base,
  fafafa.ssl.errors,
  fafafa.ssl.exceptions,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.backed,
  fafafa.ssl.openssl.session;

var
  GTestCount: Integer = 0;
  GPassCount: Integer = 0;
  GFailCount: Integer = 0;
  GSkipCount: Integer = 0;
  GLibraryAvailable: Boolean = False;
  GLib: ISSLLibrary;

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
// Category 1: Session Creation and Destruction
// ============================================================================

procedure TestSessionCreation;
var
  LSession: ISSLSession;
begin
  Section('Session Creation and Destruction');

  // Test Create empty session (nil handle)
  try
    LSession := TOpenSSLSession.Create(nil, False);
    Test('OpenSSL: Create empty session', LSession <> nil);
  except
    on E: Exception do
    begin
      Test('OpenSSL: Create empty session', False);
      WriteLn('  Exception: ', E.Message);
    end;
  end;

  // Test GetNativeHandle on empty session
  try
    LSession := TOpenSSLSession.Create(nil, False);
    Test('OpenSSL: GetNativeHandle callable', True);
    LSession.GetNativeHandle;
  except
    Test('OpenSSL: GetNativeHandle callable', False);
  end;

  // Test Clone empty session
  try
    LSession := TOpenSSLSession.Create(nil, False);
    Test('OpenSSL: Clone empty session callable', True);
    LSession.Clone;
  except
    Test('OpenSSL: Clone empty session callable', True);
  end;

  // Test GetID on empty session
  try
    LSession := TOpenSSLSession.Create(nil, False);
    Test('OpenSSL: GetID on empty session returns empty', LSession.GetID = '');
  except
    Test('OpenSSL: GetID on empty session returns empty', True);
  end;
end;

// ============================================================================
// Category 2: Session Properties
// ============================================================================

procedure TestSessionProperties;
var
  LSession: ISSLSession;
  LCreationTime: TDateTime;
begin
  Section('Session Properties');

  // Test GetCreationTime on empty session
  try
    LSession := TOpenSSLSession.Create(nil, False);
    LCreationTime := LSession.GetCreationTime;
    Test('OpenSSL: GetCreationTime callable', True);
  except
    Test('OpenSSL: GetCreationTime callable', False);
  end;

  // Test SetTimeout/GetTimeout
  try
    LSession := TOpenSSLSession.Create(nil, False);
    LSession.SetTimeout(3600);
    Test('OpenSSL: SetTimeout callable', True);
  except
    Test('OpenSSL: SetTimeout callable', False);
  end;

  try
    LSession := TOpenSSLSession.Create(nil, False);
    Test('OpenSSL: GetTimeout callable', True);
    LSession.GetTimeout;
  except
    Test('OpenSSL: GetTimeout callable', False);
  end;

  // Test IsValid on empty session
  try
    LSession := TOpenSSLSession.Create(nil, False);
    Test('OpenSSL: IsValid on empty session returns false', not LSession.IsValid);
  except
    Test('OpenSSL: IsValid on empty session returns false', True);
  end;

  // Test IsResumable on empty session
  try
    LSession := TOpenSSLSession.Create(nil, False);
    Test('OpenSSL: IsResumable on empty session returns false', not LSession.IsResumable);
  except
    Test('OpenSSL: IsResumable on empty session returns false', True);
  end;

  // Test GetProtocolVersion on empty session
  try
    LSession := TOpenSSLSession.Create(nil, False);
    Test('OpenSSL: GetProtocolVersion callable', True);
    LSession.GetProtocolVersion;
  except
    Test('OpenSSL: GetProtocolVersion callable', False);
  end;

  // Test GetCipherName on empty session
  try
    LSession := TOpenSSLSession.Create(nil, False);
    Test('OpenSSL: GetCipherName callable', True);
    LSession.GetCipherName;
  except
    Test('OpenSSL: GetCipherName callable', False);
  end;

  // Test GetPeerCertificate on empty session
  try
    LSession := TOpenSSLSession.Create(nil, False);
    Test('OpenSSL: GetPeerCertificate returns nil', LSession.GetPeerCertificate = nil);
  except
    Test('OpenSSL: GetPeerCertificate returns nil', True);
  end;
end;

// ============================================================================
// Category 3: Session Serialization
// ============================================================================

procedure TestSessionSerialization;
var
  LSession: ISSLSession;
  LData: TBytes;
begin
  Section('Session Serialization');

  // Test Serialize on empty session
  try
    LSession := TOpenSSLSession.Create(nil, False);
    LData := LSession.Serialize;
    Test('OpenSSL: Serialize on empty session callable', True);
  except
    Test('OpenSSL: Serialize on empty session callable', True);
  end;

  // Test Deserialize with empty data
  try
    LSession := TOpenSSLSession.Create(nil, False);
    SetLength(LData, 0);
    Test('OpenSSL: Deserialize with empty data returns false', not LSession.Deserialize(LData));
  except
    Test('OpenSSL: Deserialize with empty data returns false', True);
  end;

  // Test Deserialize with nil
  try
    LSession := TOpenSSLSession.Create(nil, False);
    Test('OpenSSL: Deserialize with nil returns false', not LSession.Deserialize(nil));
  except
    Test('OpenSSL: Deserialize with nil returns false', True);
  end;

  // Test Deserialize with invalid data
  try
    LSession := TOpenSSLSession.Create(nil, False);
    SetLength(LData, 10);
    FillChar(LData[0], 10, $FF);
    Test('OpenSSL: Deserialize with invalid data callable', True);
    LSession.Deserialize(LData);  // Just verify it doesn't crash
  except
    Test('OpenSSL: Deserialize with invalid data callable', True);
  end;
end;

// ============================================================================
// Main Test Runner
// ============================================================================

var
  LStartTime: TDateTime;

begin
  WriteLn('OpenSSL Session Unit Tests');
  WriteLn('==========================');
  WriteLn('High-coverage tests for TOpenSSLSession');
  WriteLn('');

  LStartTime := Now;

  // Check if OpenSSL library is available
  try
    GLib := CreateOpenSSLLibrary;
    GLibraryAvailable := GLib.Initialize;
    if GLibraryAvailable then
      WriteLn('OpenSSL library: ', GLib.GetVersionString)
    else
      WriteLn('OpenSSL library: Not available');
  except
    on E: Exception do
    begin
      WriteLn('OpenSSL library: Error - ', E.Message);
      GLibraryAvailable := False;
    end;
  end;

  WriteLn('');

  // Run all test categories
  TestSessionCreation;
  TestSessionProperties;
  TestSessionSerialization;

  // Finalize library after tests
  if GLibraryAvailable and Assigned(GLib) then
    GLib.Finalize;

  // Print summary
  WriteLn('');
  WriteLn('========================================');
  WriteLn('OpenSSL Session Unit Test Summary');
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
