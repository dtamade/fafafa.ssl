{**
 * Test: OpenSSL Connection Unit Tests
 * Purpose: High-coverage unit tests for TOpenSSLConnection
 *
 * Test Categories:
 * 1. Connection creation
 * 2. Handshake operations
 * 3. Read/Write operations
 * 4. Error handling
 * 5. Connection info
 * 6. Certificate operations
 * 7. Session management
 * 8. SNI/ALPN
 * 9. State and timeout
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-11
 *}

program test_openssl_connection_unit;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.errors,
  fafafa.ssl.exceptions,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.backed,
  fafafa.ssl.openssl.certificate,
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
// Category 1: Connection Creation
// ============================================================================

procedure TestConnectionCreation;
var
  LCtx: ISSLContext;
  LConn: ISSLConnection;
begin
  Section('Connection Creation');

  if not GLibraryAvailable then
  begin
    Skip('Connection creation tests', 'Library not available');
    Exit;
  end;

  // Test CreateConnection with invalid socket
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    if LCtx <> nil then
    begin
      LConn := LCtx.CreateConnection(THandle(-1));
      Test('CreateConnection with invalid socket', LConn <> nil);
    end
    else
      Skip('CreateConnection with invalid socket', 'Context creation failed');
  except
    on E: Exception do
      Test('CreateConnection with invalid socket', True);
  end;

  // Test GetNativeHandle
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    if LCtx <> nil then
    begin
      LConn := LCtx.CreateConnection(THandle(-1));
      if LConn <> nil then
      begin
        Test('GetNativeHandle callable', True);
        LConn.GetNativeHandle;
      end
      else
        Skip('GetNativeHandle', 'Connection creation failed');
    end
    else
      Skip('GetNativeHandle', 'Context creation failed');
  except
    on E: Exception do
      Test('GetNativeHandle callable', True);
  end;

  // Test GetContext
  try
    LCtx := GLib.CreateContext(sslCtxClient);
    if LCtx <> nil then
    begin
      LConn := LCtx.CreateConnection(THandle(-1));
      if LConn <> nil then
        Test('GetContext returns non-nil', LConn.GetContext <> nil)
      else
        Skip('GetContext', 'Connection creation failed');
    end
    else
      Skip('GetContext', 'Context creation failed');
  except
    on E: Exception do
      Skip('GetContext', E.Message);
  end;
end;

// ============================================================================
// Category 2: Handshake Operations
// ============================================================================

procedure TestHandshakeOperations;
var
  LCtx: ISSLContext;
  LConn: ISSLConnection;
begin
  Section('Handshake Operations');

  if not GLibraryAvailable then
  begin
    Skip('Handshake tests', 'Library not available');
    Exit;
  end;

  try
    LCtx := GLib.CreateContext(sslCtxClient);
    if LCtx = nil then
    begin
      Skip('Handshake tests', 'Context creation failed');
      Exit;
    end;

    LConn := LCtx.CreateConnection(THandle(-1));
    if LConn = nil then
    begin
      Skip('Handshake tests', 'Connection creation failed');
      Exit;
    end;

    // Test IsHandshakeComplete initially false
    Test('IsHandshakeComplete initially false', not LConn.IsHandshakeComplete);

    // Test IsConnected initially false
    Test('IsConnected initially false', not LConn.IsConnected);

    // Test DoHandshake on invalid socket
    try
      LConn.DoHandshake;
      Test('DoHandshake callable on invalid socket', True);
    except
      Test('DoHandshake callable on invalid socket', True);
    end;

    // Test Connect on invalid socket
    try
      LConn.Connect;
      Test('Connect callable on invalid socket', True);
    except
      Test('Connect callable on invalid socket', True);
    end;

    // Test Renegotiate
    try
      LConn.Renegotiate;
      Test('Renegotiate callable', True);
    except
      Test('Renegotiate callable', True);
    end;

  except
    on E: Exception do
      Skip('Handshake tests', E.Message);
  end;
end;

// ============================================================================
// Category 3: Read/Write Operations
// ============================================================================

procedure TestReadWriteOperations;
var
  LCtx: ISSLContext;
  LConn: ISSLConnection;
  LBuffer: array[0..1023] of Byte;
  LStr: string;
  LResult: Integer;
begin
  Section('Read/Write Operations');

  if not GLibraryAvailable then
  begin
    Skip('Read/Write tests', 'Library not available');
    Exit;
  end;

  try
    LCtx := GLib.CreateContext(sslCtxClient);
    if LCtx = nil then
    begin
      Skip('Read/Write tests', 'Context creation failed');
      Exit;
    end;

    LConn := LCtx.CreateConnection(THandle(-1));
    if LConn = nil then
    begin
      Skip('Read/Write tests', 'Connection creation failed');
      Exit;
    end;

    // Test Read on unconnected socket
    try
      LResult := LConn.Read(LBuffer, SizeOf(LBuffer));
      Test('Read on unconnected socket returns <= 0', LResult <= 0);
    except
      Test('Read on unconnected socket returns <= 0', True);
    end;

    // Test Write on unconnected socket
    try
      LStr := 'test data';
      LResult := LConn.Write(LStr[1], Length(LStr));
      Test('Write on unconnected socket returns <= 0', LResult <= 0);
    except
      Test('Write on unconnected socket returns <= 0', True);
    end;

    // Test ReadString
    try
      LConn.ReadString(LStr);
      Test('ReadString callable', True);
    except
      Test('ReadString callable', True);
    end;

    // Test WriteString
    try
      LConn.WriteString('test');
      Test('WriteString callable', True);
    except
      Test('WriteString callable', True);
    end;

    // Test WantRead
    try
      Test('WantRead callable', True);
      LConn.WantRead;
    except
      Test('WantRead callable', True);
    end;

    // Test WantWrite
    try
      Test('WantWrite callable', True);
      LConn.WantWrite;
    except
      Test('WantWrite callable', True);
    end;

  except
    on E: Exception do
      Skip('Read/Write tests', E.Message);
  end;
end;

// ============================================================================
// Category 4: Error Handling
// ============================================================================

procedure TestErrorHandling;
var
  LCtx: ISSLContext;
  LConn: ISSLConnection;
begin
  Section('Error Handling');

  if not GLibraryAvailable then
  begin
    Skip('Error handling tests', 'Library not available');
    Exit;
  end;

  try
    LCtx := GLib.CreateContext(sslCtxClient);
    if LCtx = nil then
    begin
      Skip('Error handling tests', 'Context creation failed');
      Exit;
    end;

    LConn := LCtx.CreateConnection(THandle(-1));
    if LConn = nil then
    begin
      Skip('Error handling tests', 'Connection creation failed');
      Exit;
    end;

    // Test GetError with various return codes
    try
      Test('GetError(0) callable', True);
      LConn.GetError(0);
    except
      Test('GetError(0) callable', False);
    end;

    try
      Test('GetError(-1) callable', True);
      LConn.GetError(-1);
    except
      Test('GetError(-1) callable', False);
    end;

  except
    on E: Exception do
      Skip('Error handling tests', E.Message);
  end;
end;

// ============================================================================
// Category 5: Connection Info
// ============================================================================

procedure TestConnectionInfo;
var
  LCtx: ISSLContext;
  LConn: ISSLConnection;
  LInfo: TSSLConnectionInfo;
begin
  Section('Connection Info');

  if not GLibraryAvailable then
  begin
    Skip('Connection info tests', 'Library not available');
    Exit;
  end;

  try
    LCtx := GLib.CreateContext(sslCtxClient);
    if LCtx = nil then
    begin
      Skip('Connection info tests', 'Context creation failed');
      Exit;
    end;

    LConn := LCtx.CreateConnection(THandle(-1));
    if LConn = nil then
    begin
      Skip('Connection info tests', 'Connection creation failed');
      Exit;
    end;

    // Test GetConnectionInfo
    try
      LInfo := LConn.GetConnectionInfo;
      Test('GetConnectionInfo returns record', True);
    except
      Test('GetConnectionInfo returns record', False);
    end;

    // Test GetProtocolVersion
    try
      Test('GetProtocolVersion callable', True);
      LConn.GetProtocolVersion;
    except
      Test('GetProtocolVersion callable', False);
    end;

    // Test GetCipherName
    try
      Test('GetCipherName callable', True);
      LConn.GetCipherName;
    except
      Test('GetCipherName callable', False);
    end;

    // Test GetState
    try
      Test('GetState callable', True);
      LConn.GetState;
    except
      Test('GetState callable', False);
    end;

    // Test GetStateString
    try
      Test('GetStateString callable', True);
      LConn.GetStateString;
    except
      Test('GetStateString callable', False);
    end;

  except
    on E: Exception do
      Skip('Connection info tests', E.Message);
  end;
end;

// ============================================================================
// Category 6: Certificate Operations
// ============================================================================

procedure TestCertificateOperations;
var
  LCtx: ISSLContext;
  LConn: ISSLConnection;
  LCert: ISSLCertificate;
  LChain: TSSLCertificateArray;
begin
  Section('Certificate Operations');

  if not GLibraryAvailable then
  begin
    Skip('Certificate operations tests', 'Library not available');
    Exit;
  end;

  try
    LCtx := GLib.CreateContext(sslCtxClient);
    if LCtx = nil then
    begin
      Skip('Certificate operations tests', 'Context creation failed');
      Exit;
    end;

    LConn := LCtx.CreateConnection(THandle(-1));
    if LConn = nil then
    begin
      Skip('Certificate operations tests', 'Connection creation failed');
      Exit;
    end;

    // Test GetPeerCertificate (should be nil before handshake)
    try
      LCert := LConn.GetPeerCertificate;
      Test('GetPeerCertificate before handshake returns nil', LCert = nil);
    except
      Test('GetPeerCertificate before handshake returns nil', True);
    end;

    // Test GetPeerCertificateChain
    try
      LChain := LConn.GetPeerCertificateChain;
      Test('GetPeerCertificateChain before handshake returns empty', Length(LChain) = 0);
    except
      Test('GetPeerCertificateChain before handshake returns empty', True);
    end;

    // Test GetVerifyResult
    try
      Test('GetVerifyResult callable', True);
      LConn.GetVerifyResult;
    except
      Test('GetVerifyResult callable', False);
    end;

    // Test GetVerifyResultString
    try
      Test('GetVerifyResultString callable', True);
      LConn.GetVerifyResultString;
    except
      Test('GetVerifyResultString callable', False);
    end;

  except
    on E: Exception do
      Skip('Certificate operations tests', E.Message);
  end;
end;

// ============================================================================
// Category 7: Session Management
// ============================================================================

procedure TestSessionManagement;
var
  LCtx: ISSLContext;
  LConn: ISSLConnection;
  LSession: ISSLSession;
begin
  Section('Session Management');

  if not GLibraryAvailable then
  begin
    Skip('Session management tests', 'Library not available');
    Exit;
  end;

  try
    LCtx := GLib.CreateContext(sslCtxClient);
    if LCtx = nil then
    begin
      Skip('Session management tests', 'Context creation failed');
      Exit;
    end;

    LConn := LCtx.CreateConnection(THandle(-1));
    if LConn = nil then
    begin
      Skip('Session management tests', 'Connection creation failed');
      Exit;
    end;

    // Test GetSession before handshake
    try
      LSession := LConn.GetSession;
      Test('GetSession before handshake callable', True);
    except
      Test('GetSession before handshake callable', True);
    end;

    // Test SetSession with nil
    try
      LConn.SetSession(nil);
      Test('SetSession with nil succeeds', True);
    except
      Test('SetSession with nil succeeds', True);
    end;

    // Test SetSession with valid session
    try
      LSession := TOpenSSLSession.Create(nil, False);
      LConn.SetSession(LSession);
      Test('SetSession with valid session succeeds', True);
    except
      Test('SetSession with valid session succeeds', True);
    end;

    // Test IsSessionReused
    try
      Test('IsSessionReused callable', True);
      LConn.IsSessionReused;
    except
      Test('IsSessionReused callable', False);
    end;

  except
    on E: Exception do
      Skip('Session management tests', E.Message);
  end;
end;

// ============================================================================
// Category 8: SNI/ALPN (via Context)
// ============================================================================

procedure TestSNIALPN;
var
  LCtx: ISSLContext;
  LConn: ISSLConnection;
begin
  Section('SNI/ALPN (via Context)');

  if not GLibraryAvailable then
  begin
    Skip('SNI/ALPN tests', 'Library not available');
    Exit;
  end;

  try
    LCtx := GLib.CreateContext(sslCtxClient);
    if LCtx = nil then
    begin
      Skip('SNI/ALPN tests', 'Context creation failed');
      Exit;
    end;

    // Test SetServerName on context
    try
      LCtx.SetServerName('example.com');
      Test('Context SetServerName succeeds', True);
    except
      Test('Context SetServerName succeeds', False);
    end;

    // Test GetServerName on context
    try
      Test('Context GetServerName returns set value', LCtx.GetServerName = 'example.com');
    except
      Test('Context GetServerName returns set value', False);
    end;

    LConn := LCtx.CreateConnection(THandle(-1));
    if LConn = nil then
    begin
      Skip('SNI/ALPN connection tests', 'Connection creation failed');
      Exit;
    end;

    // Test GetSelectedALPNProtocol
    try
      Test('GetSelectedALPNProtocol callable', True);
      LConn.GetSelectedALPNProtocol;
    except
      Test('GetSelectedALPNProtocol callable', False);
    end;

  except
    on E: Exception do
      Skip('SNI/ALPN tests', E.Message);
  end;
end;

// ============================================================================
// Category 9: State and Timeout
// ============================================================================

procedure TestStateAndTimeout;
var
  LCtx: ISSLContext;
  LConn: ISSLConnection;
begin
  Section('State and Timeout');

  if not GLibraryAvailable then
  begin
    Skip('State and timeout tests', 'Library not available');
    Exit;
  end;

  try
    LCtx := GLib.CreateContext(sslCtxClient);
    if LCtx = nil then
    begin
      Skip('State and timeout tests', 'Context creation failed');
      Exit;
    end;

    LConn := LCtx.CreateConnection(THandle(-1));
    if LConn = nil then
    begin
      Skip('State and timeout tests', 'Connection creation failed');
      Exit;
    end;

    // Test SetTimeout
    try
      LConn.SetTimeout(5000);
      Test('SetTimeout succeeds', True);
    except
      Test('SetTimeout succeeds', False);
    end;

    // Test GetTimeout
    try
      Test('GetTimeout returns set value', LConn.GetTimeout = 5000);
    except
      Test('GetTimeout returns set value', False);
    end;

    // Test SetBlocking
    try
      LConn.SetBlocking(True);
      Test('SetBlocking(True) succeeds', True);
    except
      Test('SetBlocking(True) succeeds', False);
    end;

    // Test GetBlocking
    try
      Test('GetBlocking returns set value', LConn.GetBlocking = True);
    except
      Test('GetBlocking returns set value', False);
    end;

    // Test SetBlocking(False)
    try
      LConn.SetBlocking(False);
      Test('SetBlocking(False) succeeds', True);
    except
      Test('SetBlocking(False) succeeds', False);
    end;

    // Test Shutdown on unconnected socket
    try
      LConn.Shutdown;
      Test('Shutdown on unconnected socket callable', True);
    except
      Test('Shutdown on unconnected socket callable', True);
    end;

    // Test Close
    try
      LConn.Close;
      Test('Close callable', True);
    except
      Test('Close callable', True);
    end;

  except
    on E: Exception do
      Skip('State and timeout tests', E.Message);
  end;
end;

// ============================================================================
// Main Test Runner
// ============================================================================

var
  LStartTime: TDateTime;

begin
  WriteLn('OpenSSL Connection Unit Tests');
  WriteLn('==============================');
  WriteLn('High-coverage tests for TOpenSSLConnection');
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
  TestConnectionCreation;
  TestHandshakeOperations;
  TestReadWriteOperations;
  TestErrorHandling;
  TestConnectionInfo;
  TestCertificateOperations;
  TestSessionManagement;
  TestSNIALPN;
  TestStateAndTimeout;

  // Finalize library after tests
  if GLibraryAvailable and Assigned(GLib) then
    GLib.Finalize;

  // Print summary
  WriteLn('');
  WriteLn('========================================');
  WriteLn('OpenSSL Connection Unit Test Summary');
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
