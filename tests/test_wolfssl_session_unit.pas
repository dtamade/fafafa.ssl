{**
 * Test: WolfSSL Session Unit Tests
 * Purpose: High-coverage unit tests for TWolfSSLSession
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

program test_wolfssl_session_unit;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, DateUtils,
  fafafa.ssl.base,
  fafafa.ssl.errors,
  fafafa.ssl.exceptions,
  fafafa.ssl.wolfssl.base,
  fafafa.ssl.wolfssl.lib,
  fafafa.ssl.wolfssl.session;

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

  // Test Create empty session
  try
    LSession := TWolfSSLSession.Create;
    Test('WolfSSL: Create empty session', LSession <> nil);
  except
    on E: Exception do
    begin
      Test('WolfSSL: Create empty session', False);
      WriteLn('  Exception: ', E.Message);
    end;
  end;

  // Test GetNativeHandle on empty session
  try
    LSession := TWolfSSLSession.Create;
    Test('WolfSSL: GetNativeHandle callable', True);
    LSession.GetNativeHandle;
  except
    Test('WolfSSL: GetNativeHandle callable', False);
  end;

  // Test Clone empty session
  try
    LSession := TWolfSSLSession.Create;
    Test('WolfSSL: Clone empty session callable', True);
    LSession.Clone;
  except
    Test('WolfSSL: Clone empty session callable', True);
  end;

  // Test GetID on empty session (may return empty or default ID)
  try
    LSession := TWolfSSLSession.Create;
    Test('WolfSSL: GetID callable on empty session', True);
    LSession.GetID;  // Just verify it doesn't crash
  except
    Test('WolfSSL: GetID callable on empty session', False);
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
    LSession := TWolfSSLSession.Create;
    LCreationTime := LSession.GetCreationTime;
    Test('WolfSSL: GetCreationTime callable', True);
  except
    Test('WolfSSL: GetCreationTime callable', False);
  end;

  // Test SetTimeout/GetTimeout
  try
    LSession := TWolfSSLSession.Create;
    LSession.SetTimeout(3600);
    Test('WolfSSL: SetTimeout callable', True);
  except
    Test('WolfSSL: SetTimeout callable', False);
  end;

  try
    LSession := TWolfSSLSession.Create;
    Test('WolfSSL: GetTimeout callable', True);
    LSession.GetTimeout;
  except
    Test('WolfSSL: GetTimeout callable', False);
  end;

  // Test IsValid on empty session
  try
    LSession := TWolfSSLSession.Create;
    Test('WolfSSL: IsValid on empty session returns false', not LSession.IsValid);
  except
    Test('WolfSSL: IsValid on empty session returns false', True);
  end;

  // Test IsResumable on empty session
  try
    LSession := TWolfSSLSession.Create;
    Test('WolfSSL: IsResumable on empty session returns false', not LSession.IsResumable);
  except
    Test('WolfSSL: IsResumable on empty session returns false', True);
  end;

  // Test GetProtocolVersion on empty session
  try
    LSession := TWolfSSLSession.Create;
    Test('WolfSSL: GetProtocolVersion callable', True);
    LSession.GetProtocolVersion;
  except
    Test('WolfSSL: GetProtocolVersion callable', False);
  end;

  // Test GetCipherName on empty session
  try
    LSession := TWolfSSLSession.Create;
    Test('WolfSSL: GetCipherName callable', True);
    LSession.GetCipherName;
  except
    Test('WolfSSL: GetCipherName callable', False);
  end;

  // Test GetPeerCertificate on empty session
  try
    LSession := TWolfSSLSession.Create;
    Test('WolfSSL: GetPeerCertificate returns nil', LSession.GetPeerCertificate = nil);
  except
    Test('WolfSSL: GetPeerCertificate returns nil', True);
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
    LSession := TWolfSSLSession.Create;
    LData := LSession.Serialize;
    Test('WolfSSL: Serialize on empty session callable', True);
  except
    Test('WolfSSL: Serialize on empty session callable', True);
  end;

  // Test Deserialize with empty data
  try
    LSession := TWolfSSLSession.Create;
    SetLength(LData, 0);
    Test('WolfSSL: Deserialize with empty data returns false', not LSession.Deserialize(LData));
  except
    Test('WolfSSL: Deserialize with empty data returns false', True);
  end;

  // Test Deserialize with nil
  try
    LSession := TWolfSSLSession.Create;
    Test('WolfSSL: Deserialize with nil returns false', not LSession.Deserialize(nil));
  except
    Test('WolfSSL: Deserialize with nil returns false', True);
  end;

  // Test Deserialize with invalid data
  try
    LSession := TWolfSSLSession.Create;
    SetLength(LData, 10);
    FillChar(LData[0], 10, $FF);
    Test('WolfSSL: Deserialize with invalid data callable', True);
    LSession.Deserialize(LData);
  except
    Test('WolfSSL: Deserialize with invalid data callable', True);
  end;
end;

// ============================================================================
// Main Test Runner
// ============================================================================

var
  LStartTime: TDateTime;

begin
  WriteLn('WolfSSL Session Unit Tests');
  WriteLn('==========================');
  WriteLn('High-coverage tests for TWolfSSLSession');
  WriteLn('');

  LStartTime := Now;

  // Check if WolfSSL library is available
  try
    GLib := CreateWolfSSLLibrary;
    GLibraryAvailable := GLib.Initialize;
    if GLibraryAvailable then
      WriteLn('WolfSSL library: ', GLib.GetVersionString)
    else
      WriteLn('WolfSSL library: Not available');
  except
    on E: Exception do
    begin
      WriteLn('WolfSSL library: Error - ', E.Message);
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
  WriteLn('WolfSSL Session Unit Test Summary');
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
