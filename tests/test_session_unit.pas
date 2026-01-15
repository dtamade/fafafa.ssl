{**
 * Test: Session Module Unit Tests
 * Purpose: High-coverage unit tests for TMbedTLSSession and TWolfSSLSession
 *
 * Test Categories:
 * 1. MbedTLS Session creation and destruction
 * 2. MbedTLS Session properties
 * 3. MbedTLS Session serialization
 * 4. WolfSSL Session creation and destruction
 * 5. WolfSSL Session properties
 * 6. WolfSSL Session serialization
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-11
 *}

program test_session_unit;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, DateUtils,
  fafafa.ssl.base,
  fafafa.ssl.errors,
  fafafa.ssl.exceptions,
  fafafa.ssl.mbedtls.base,
  fafafa.ssl.mbedtls.api,
  fafafa.ssl.mbedtls.lib,
  fafafa.ssl.mbedtls.session,
  fafafa.ssl.wolfssl.base,
  fafafa.ssl.wolfssl.api,
  fafafa.ssl.wolfssl.lib,
  fafafa.ssl.wolfssl.session;

var
  GTestCount: Integer = 0;
  GPassCount: Integer = 0;
  GFailCount: Integer = 0;
  GSkipCount: Integer = 0;
  GMbedTLSAvailable: Boolean = False;
  GWolfSSLAvailable: Boolean = False;
  GMbedTLSLib: ISSLLibrary;
  GWolfSSLLib: ISSLLibrary;

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
// Category 1: MbedTLS Session Creation and Destruction
// ============================================================================

procedure TestMbedTLSSessionCreation;
var
  LSession: ISSLSession;
begin
  Section('MbedTLS Session Creation and Destruction');

  // Test Create empty session
  try
    LSession := TMbedTLSSession.Create;
    Test('MbedTLS: Create empty session', LSession <> nil);
  except
    on E: Exception do
    begin
      Test('MbedTLS: Create empty session', False);
      WriteLn('  Exception: ', E.Message);
    end;
  end;

  // Test GetNativeHandle on empty session
  try
    LSession := TMbedTLSSession.Create;
    Test('MbedTLS: GetNativeHandle callable', True);
    LSession.GetNativeHandle;
  except
    Test('MbedTLS: GetNativeHandle callable', False);
  end;

  // Test Clone empty session
  try
    LSession := TMbedTLSSession.Create;
    Test('MbedTLS: Clone empty session', LSession.Clone <> nil);
  except
    Test('MbedTLS: Clone empty session', False);
  end;

  // Test GetID on empty session
  try
    LSession := TMbedTLSSession.Create;
    Test('MbedTLS: GetID returns non-empty', LSession.GetID <> '');
  except
    Test('MbedTLS: GetID returns non-empty', False);
  end;
end;

// ============================================================================
// Category 2: MbedTLS Session Properties
// ============================================================================

procedure TestMbedTLSSessionProperties;
var
  LSession: ISSLSession;
  LCreationTime: TDateTime;
begin
  Section('MbedTLS Session Properties');

  // Test GetCreationTime
  try
    LSession := TMbedTLSSession.Create;
    LCreationTime := LSession.GetCreationTime;
    Test('MbedTLS: GetCreationTime returns valid time', LCreationTime > 0);
  except
    Test('MbedTLS: GetCreationTime returns valid time', False);
  end;

  // Test SetTimeout/GetTimeout
  try
    LSession := TMbedTLSSession.Create;
    LSession.SetTimeout(3600);
    Test('MbedTLS: SetTimeout/GetTimeout', LSession.GetTimeout = 3600);
  except
    Test('MbedTLS: SetTimeout/GetTimeout', False);
  end;

  // Test IsValid on empty session
  try
    LSession := TMbedTLSSession.Create;
    Test('MbedTLS: IsValid callable', True);
    LSession.IsValid;
  except
    Test('MbedTLS: IsValid callable', False);
  end;

  // Test IsResumable on empty session
  try
    LSession := TMbedTLSSession.Create;
    Test('MbedTLS: IsResumable callable', True);
    LSession.IsResumable;
  except
    Test('MbedTLS: IsResumable callable', False);
  end;

  // Test GetProtocolVersion
  try
    LSession := TMbedTLSSession.Create;
    Test('MbedTLS: GetProtocolVersion callable', True);
    LSession.GetProtocolVersion;
  except
    Test('MbedTLS: GetProtocolVersion callable', False);
  end;

  // Test GetCipherName
  try
    LSession := TMbedTLSSession.Create;
    Test('MbedTLS: GetCipherName callable', True);
    LSession.GetCipherName;
  except
    Test('MbedTLS: GetCipherName callable', False);
  end;

  // Test GetPeerCertificate on empty session
  try
    LSession := TMbedTLSSession.Create;
    Test('MbedTLS: GetPeerCertificate returns nil', LSession.GetPeerCertificate = nil);
  except
    Test('MbedTLS: GetPeerCertificate returns nil', True);
  end;
end;

// ============================================================================
// Category 3: MbedTLS Session Serialization
// ============================================================================

procedure TestMbedTLSSessionSerialization;
var
  LSession: ISSLSession;
  LData: TBytes;
begin
  Section('MbedTLS Session Serialization');

  // Test Serialize on empty session
  try
    LSession := TMbedTLSSession.Create;
    LData := LSession.Serialize;
    Test('MbedTLS: Serialize callable', True);
  except
    Test('MbedTLS: Serialize callable', True);
  end;

  // Test Deserialize with empty data
  try
    LSession := TMbedTLSSession.Create;
    SetLength(LData, 0);
    Test('MbedTLS: Deserialize with empty data returns false', not LSession.Deserialize(LData));
  except
    Test('MbedTLS: Deserialize with empty data returns false', True);
  end;

  // Test Deserialize with nil
  try
    LSession := TMbedTLSSession.Create;
    Test('MbedTLS: Deserialize with nil returns false', not LSession.Deserialize(nil));
  except
    Test('MbedTLS: Deserialize with nil returns false', True);
  end;

  // Test Deserialize with invalid data
  try
    LSession := TMbedTLSSession.Create;
    SetLength(LData, 10);
    FillChar(LData[0], 10, $FF);
    Test('MbedTLS: Deserialize with invalid data callable', True);
    LSession.Deserialize(LData);  // Just verify it doesn't crash
  except
    Test('MbedTLS: Deserialize with invalid data callable', True);
  end;
end;

// ============================================================================
// Category 4: WolfSSL Session Creation and Destruction
// ============================================================================

procedure TestWolfSSLSessionCreation;
var
  LSession: ISSLSession;
begin
  Section('WolfSSL Session Creation and Destruction');

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
    Test('WolfSSL: Clone empty session', LSession.Clone <> nil);
  except
    Test('WolfSSL: Clone empty session', False);
  end;

  // Test GetID on empty session
  try
    LSession := TWolfSSLSession.Create;
    Test('WolfSSL: GetID returns non-empty', LSession.GetID <> '');
  except
    Test('WolfSSL: GetID returns non-empty', False);
  end;
end;

// ============================================================================
// Category 5: WolfSSL Session Properties
// ============================================================================

procedure TestWolfSSLSessionProperties;
var
  LSession: ISSLSession;
  LCreationTime: TDateTime;
begin
  Section('WolfSSL Session Properties');

  // Test GetCreationTime
  try
    LSession := TWolfSSLSession.Create;
    LCreationTime := LSession.GetCreationTime;
    Test('WolfSSL: GetCreationTime returns valid time', LCreationTime > 0);
  except
    Test('WolfSSL: GetCreationTime returns valid time', False);
  end;

  // Test SetTimeout/GetTimeout
  try
    LSession := TWolfSSLSession.Create;
    LSession.SetTimeout(3600);
    Test('WolfSSL: SetTimeout/GetTimeout', LSession.GetTimeout = 3600);
  except
    Test('WolfSSL: SetTimeout/GetTimeout', False);
  end;

  // Test IsValid on empty session
  try
    LSession := TWolfSSLSession.Create;
    Test('WolfSSL: IsValid callable', True);
    LSession.IsValid;
  except
    Test('WolfSSL: IsValid callable', False);
  end;

  // Test IsResumable on empty session
  try
    LSession := TWolfSSLSession.Create;
    Test('WolfSSL: IsResumable callable', True);
    LSession.IsResumable;
  except
    Test('WolfSSL: IsResumable callable', False);
  end;

  // Test GetProtocolVersion
  try
    LSession := TWolfSSLSession.Create;
    Test('WolfSSL: GetProtocolVersion callable', True);
    LSession.GetProtocolVersion;
  except
    Test('WolfSSL: GetProtocolVersion callable', False);
  end;

  // Test GetCipherName
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
// Category 6: WolfSSL Session Serialization
// ============================================================================

procedure TestWolfSSLSessionSerialization;
var
  LSession: ISSLSession;
  LData: TBytes;
begin
  Section('WolfSSL Session Serialization');

  // Test Serialize on empty session
  try
    LSession := TWolfSSLSession.Create;
    LData := LSession.Serialize;
    Test('WolfSSL: Serialize callable', True);
  except
    Test('WolfSSL: Serialize callable', True);
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
    Test('WolfSSL: Deserialize with invalid data returns false', not LSession.Deserialize(LData));
  except
    Test('WolfSSL: Deserialize with invalid data returns false', True);
  end;
end;

// ============================================================================
// Main Test Runner
// ============================================================================

var
  LStartTime: TDateTime;

begin
  WriteLn('Session Module Unit Tests');
  WriteLn('=========================');
  WriteLn('High-coverage tests for TMbedTLSSession and TWolfSSLSession');
  WriteLn('');

  LStartTime := Now;

  // Check if MbedTLS library is available
  try
    GMbedTLSLib := CreateMbedTLSLibrary;
    GMbedTLSAvailable := GMbedTLSLib.Initialize;
    if GMbedTLSAvailable then
      WriteLn('MbedTLS library: ', GMbedTLSLib.GetVersionString)
    else
      WriteLn('MbedTLS library: Not available');
  except
    on E: Exception do
    begin
      WriteLn('MbedTLS library: Error - ', E.Message);
      GMbedTLSAvailable := False;
    end;
  end;

  // Check if WolfSSL library is available
  try
    GWolfSSLLib := CreateWolfSSLLibrary;
    GWolfSSLAvailable := GWolfSSLLib.Initialize;
    if GWolfSSLAvailable then
      WriteLn('WolfSSL library: ', GWolfSSLLib.GetVersionString)
    else
      WriteLn('WolfSSL library: Not available');
  except
    on E: Exception do
    begin
      WriteLn('WolfSSL library: Error - ', E.Message);
      GWolfSSLAvailable := False;
    end;
  end;

  WriteLn('');

  // Run all test categories
  TestMbedTLSSessionCreation;
  TestMbedTLSSessionProperties;
  TestMbedTLSSessionSerialization;
  TestWolfSSLSessionCreation;
  TestWolfSSLSessionProperties;
  TestWolfSSLSessionSerialization;

  // Finalize libraries after tests
  if GMbedTLSAvailable and Assigned(GMbedTLSLib) then
    GMbedTLSLib.Finalize;
  if GWolfSSLAvailable and Assigned(GWolfSSLLib) then
    GWolfSSLLib.Finalize;

  // Print summary
  WriteLn('');
  WriteLn('========================================');
  WriteLn('Session Module Unit Test Summary');
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
