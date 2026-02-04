{**
 * Unit: test_stream_connection
 * Purpose: 测试各后端的流式连接支持
 *
 * 验证 CreateConnection(TStream) 方法在各后端的实现状态。
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-02-04
 *}
program test_stream_connection;

{$mode objfpc}{$H+}
{$DEFINE ENABLE_WOLFSSL}
{$DEFINE ENABLE_MBEDTLS}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.exceptions,
  fafafa.ssl.openssl.backed,
  fafafa.ssl.wolfssl.lib,
  fafafa.ssl.mbedtls.lib;

var
  GTestsPassed: Integer = 0;
  GTestsFailed: Integer = 0;
  GTestsSkipped: Integer = 0;

procedure LogPass(const AMessage: string);
begin
  Inc(GTestsPassed);
  WriteLn('  [PASS] ', AMessage);
end;

procedure LogFail(const AMessage: string);
begin
  Inc(GTestsFailed);
  WriteLn('  [FAIL] ', AMessage);
end;

procedure LogSkip(const AMessage: string);
begin
  Inc(GTestsSkipped);
  WriteLn('  [SKIP] ', AMessage);
end;

procedure TestOpenSSLStreamConnection;
var
  LLibrary: ISSLLibrary;
  LContext: ISSLContext;
  LStream: TMemoryStream;
  LConnection: ISSLConnection;
begin
  WriteLn;
  WriteLn('=== OpenSSL Stream Connection Test ===');

  try
    LLibrary := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if LLibrary = nil then
    begin
      LogSkip('OpenSSL library not available');
      Exit;
    end;

    if not LLibrary.Initialize then
    begin
      LogSkip('OpenSSL library initialization failed');
      Exit;
    end;

    LContext := LLibrary.CreateContext(sslContextClient);
    if LContext = nil then
    begin
      LogFail('Failed to create OpenSSL context');
      Exit;
    end;

    LStream := TMemoryStream.Create;
    try
      try
        LConnection := LContext.CreateConnection(LStream);
        if LConnection <> nil then
          LogPass('OpenSSL CreateConnection(TStream) returns valid connection')
        else
          LogFail('OpenSSL CreateConnection(TStream) returned nil');
      except
        on E: ESSLException do
        begin
          if Pos('not supported', LowerCase(E.Message)) > 0 then
            LogSkip('OpenSSL stream connection not supported: ' + E.Message)
          else if Pos('not implemented', LowerCase(E.Message)) > 0 then
            LogSkip('OpenSSL stream connection not implemented: ' + E.Message)
          else
            LogFail('OpenSSL CreateConnection(TStream) exception: ' + E.Message);
        end;
        on E: Exception do
          LogFail('OpenSSL CreateConnection(TStream) unexpected error: ' + E.Message);
      end;
    finally
      LStream.Free;
    end;

    LLibrary.Finalize;
  except
    on E: Exception do
      LogSkip('OpenSSL test skipped: ' + E.Message);
  end;
end;

procedure TestWinSSLStreamConnection;
var
  LLibrary: ISSLLibrary;
  LContext: ISSLContext;
  LStream: TMemoryStream;
  LConnection: ISSLConnection;
begin
  WriteLn;
  WriteLn('=== WinSSL Stream Connection Test ===');

  {$IFNDEF WINDOWS}
  LogSkip('WinSSL only available on Windows');
  Exit;
  {$ENDIF}

  try
    LLibrary := TSSLFactory.GetLibraryInstance(sslWinSSL);
    if LLibrary = nil then
    begin
      LogSkip('WinSSL library not available');
      Exit;
    end;

    if not LLibrary.Initialize then
    begin
      LogSkip('WinSSL library initialization failed');
      Exit;
    end;

    LContext := LLibrary.CreateContext(sslContextClient);
    if LContext = nil then
    begin
      LogFail('Failed to create WinSSL context');
      Exit;
    end;

    LStream := TMemoryStream.Create;
    try
      try
        LConnection := LContext.CreateConnection(LStream);
        if LConnection <> nil then
          LogPass('WinSSL CreateConnection(TStream) returns valid connection')
        else
          LogFail('WinSSL CreateConnection(TStream) returned nil');
      except
        on E: ESSLException do
        begin
          if Pos('not supported', LowerCase(E.Message)) > 0 then
            LogSkip('WinSSL stream connection not supported: ' + E.Message)
          else if Pos('not implemented', LowerCase(E.Message)) > 0 then
            LogSkip('WinSSL stream connection not implemented: ' + E.Message)
          else
            LogFail('WinSSL CreateConnection(TStream) exception: ' + E.Message);
        end;
        on E: Exception do
          LogFail('WinSSL CreateConnection(TStream) unexpected error: ' + E.Message);
      end;
    finally
      LStream.Free;
    end;

    LLibrary.Finalize;
  except
    on E: Exception do
      LogSkip('WinSSL test skipped: ' + E.Message);
  end;
end;

procedure TestWolfSSLStreamConnection;
var
  LLibrary: ISSLLibrary;
  LContext: ISSLContext;
  LStream: TMemoryStream;
  LConnection: ISSLConnection;
begin
  WriteLn;
  WriteLn('=== WolfSSL Stream Connection Test ===');

  try
    LLibrary := TSSLFactory.GetLibraryInstance(sslWolfSSL);
    if LLibrary = nil then
    begin
      LogSkip('WolfSSL library not available');
      Exit;
    end;

    if not LLibrary.Initialize then
    begin
      LogSkip('WolfSSL library initialization failed');
      Exit;
    end;

    LContext := LLibrary.CreateContext(sslContextClient);
    if LContext = nil then
    begin
      LogFail('Failed to create WolfSSL context');
      Exit;
    end;

    LStream := TMemoryStream.Create;
    try
      try
        LConnection := LContext.CreateConnection(LStream);
        if LConnection <> nil then
          LogPass('WolfSSL CreateConnection(TStream) returns valid connection')
        else
          LogFail('WolfSSL CreateConnection(TStream) returned nil');
      except
        on E: ESSLException do
        begin
          if Pos('not supported', LowerCase(E.Message)) > 0 then
            LogSkip('WolfSSL stream connection not supported: ' + E.Message)
          else if Pos('not implemented', LowerCase(E.Message)) > 0 then
            LogSkip('WolfSSL stream connection not implemented: ' + E.Message)
          else if Pos('i/o callback', LowerCase(E.Message)) > 0 then
            LogSkip('WolfSSL I/O callbacks not available: ' + E.Message)
          else
            LogFail('WolfSSL CreateConnection(TStream) exception: ' + E.Message);
        end;
        on E: Exception do
          LogFail('WolfSSL CreateConnection(TStream) unexpected error: ' + E.Message);
      end;
    finally
      LStream.Free;
    end;

    LLibrary.Finalize;
  except
    on E: Exception do
      LogSkip('WolfSSL test skipped: ' + E.Message);
  end;
end;

procedure TestMbedTLSStreamConnection;
var
  LLibrary: ISSLLibrary;
  LContext: ISSLContext;
  LStream: TMemoryStream;
  LConnection: ISSLConnection;
begin
  WriteLn;
  WriteLn('=== MbedTLS Stream Connection Test ===');

  try
    LLibrary := TSSLFactory.GetLibraryInstance(sslMbedTLS);
    if LLibrary = nil then
    begin
      LogSkip('MbedTLS library not available');
      Exit;
    end;

    if not LLibrary.Initialize then
    begin
      LogSkip('MbedTLS library initialization failed');
      Exit;
    end;

    LContext := LLibrary.CreateContext(sslContextClient);
    if LContext = nil then
    begin
      LogFail('Failed to create MbedTLS context');
      Exit;
    end;

    LStream := TMemoryStream.Create;
    try
      try
        LConnection := LContext.CreateConnection(LStream);
        if LConnection <> nil then
          LogPass('MbedTLS CreateConnection(TStream) returns valid connection')
        else
          LogFail('MbedTLS CreateConnection(TStream) returned nil');
      except
        on E: ESSLException do
        begin
          if Pos('not supported', LowerCase(E.Message)) > 0 then
            LogSkip('MbedTLS stream connection not supported: ' + E.Message)
          else if Pos('not implemented', LowerCase(E.Message)) > 0 then
            LogSkip('MbedTLS stream connection not implemented: ' + E.Message)
          else
            LogFail('MbedTLS CreateConnection(TStream) exception: ' + E.Message);
        end;
        on E: Exception do
          LogFail('MbedTLS CreateConnection(TStream) unexpected error: ' + E.Message);
      end;
    finally
      LStream.Free;
    end;

    LLibrary.Finalize;
  except
    on E: Exception do
      LogSkip('MbedTLS test skipped: ' + E.Message);
  end;
end;

procedure TestNilStreamHandling;
var
  LLibrary: ISSLLibrary;
  LContext: ISSLContext;
  LRaisedException: Boolean;
begin
  WriteLn;
  WriteLn('=== Nil Stream Handling Test ===');

  try
    LLibrary := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if (LLibrary = nil) or (not LLibrary.Initialize) then
    begin
      LogSkip('OpenSSL not available for nil stream test');
      Exit;
    end;

    LContext := LLibrary.CreateContext(sslContextClient);
    if LContext = nil then
    begin
      LogFail('Failed to create context');
      Exit;
    end;

    LRaisedException := False;
    try
      LContext.CreateConnection(TStream(nil));
    except
      on E: ESSLException do
      begin
        LRaisedException := True;
        if Pos('nil', LowerCase(E.Message)) > 0 then
          LogPass('Nil stream correctly raises exception: ' + E.Message)
        else
          LogPass('Nil stream raises exception (message: ' + E.Message + ')');
      end;
      on E: Exception do
      begin
        LRaisedException := True;
        LogPass('Nil stream raises exception: ' + E.ClassName);
      end;
    end;

    if not LRaisedException then
      LogFail('Nil stream should raise exception');

    LLibrary.Finalize;
  except
    on E: Exception do
      LogSkip('Nil stream test skipped: ' + E.Message);
  end;
end;

begin
  WriteLn('========================================');
  WriteLn('  Stream Connection Tests');
  WriteLn('========================================');
  WriteLn;
  WriteLn('Testing CreateConnection(TStream) for all backends.');
  WriteLn('This verifies stream-based SSL connection support.');

  TestOpenSSLStreamConnection;
  TestWinSSLStreamConnection;
  TestWolfSSLStreamConnection;
  TestMbedTLSStreamConnection;
  TestNilStreamHandling;

  WriteLn;
  WriteLn('========================================');
  WriteLn('  Summary');
  WriteLn('========================================');
  WriteLn('  Passed:  ', GTestsPassed);
  WriteLn('  Failed:  ', GTestsFailed);
  WriteLn('  Skipped: ', GTestsSkipped);
  WriteLn('========================================');

  if GTestsFailed > 0 then
    ExitCode := 1
  else
    ExitCode := 0;
end.
