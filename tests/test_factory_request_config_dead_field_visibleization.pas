program test_factory_request_config_dead_field_visibleization;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.exceptions,
  fafafa.ssl.freepascal.lib;

procedure Require(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
  begin
    WriteLn('[FAIL] ', AMessage);
    Halt(1);
  end;
end;

procedure InitBaseConfig(out AConfig: TSSLConfig);
begin
  FillChar(AConfig, SizeOf(AConfig), 0);
  AConfig.LibraryType := sslFreePascal;
  AConfig.ContextType := sslCtxClient;
  AConfig.ProtocolVersions := [sslProtocolTLS13];
  AConfig.PreferredVersion := sslProtocolTLS13;
  AConfig.VerifyMode := [sslVerifyNone];
  AConfig.CipherSuites := 'TLS_AES_256_GCM_SHA384';
end;

procedure ExpectRequestConfigError(const AName: string; const AConfig: TSSLConfig; const AExpectedText: string);
var
  LRaised: Boolean;
  LContext: ISSLContext;
begin
  LRaised := False;
  LContext := nil;
  try
    LContext := TSSLFactory.CreateContext(AConfig);
  except
    on E: ESSLConfigurationException do
    begin
      LRaised := True;
      Require(Pos(AExpectedText, E.Message) > 0,
        AName + ' should mention ' + AExpectedText + ', actual=' + E.Message);
    end;
  end;

  Require(LRaised, AName + ' should raise ESSLConfigurationException');
  Require(LContext = nil, AName + ' should not return a context');
end;

procedure TestHandshakeTimeoutRejected;
var
  LConfig: TSSLConfig;
begin
  InitBaseConfig(LConfig);
  LConfig.HandshakeTimeout := 100;
  ExpectRequestConfigError('request HandshakeTimeout', LConfig, 'HandshakeTimeout');
end;

procedure TestBufferSizeRejected;
var
  LConfig: TSSLConfig;
begin
  InitBaseConfig(LConfig);
  LConfig.BufferSize := 8192;
  ExpectRequestConfigError('request BufferSize', LConfig, 'BufferSize');
end;

procedure TestDefaultDeadFieldsStillAccepted;
var
  LConfig: TSSLConfig;
  LContext: ISSLContext;
begin
  InitBaseConfig(LConfig);
  LConfig.BufferSize := SSL_DEFAULT_BUFFER_SIZE;
  LConfig.HandshakeTimeout := SSL_DEFAULT_HANDSHAKE_TIMEOUT;

  LContext := TSSLFactory.CreateContext(LConfig);
  Require(LContext <> nil, 'default dead-field values should remain accepted for compatibility');
end;

begin
  WriteLn('fafafa.ssl - factory request config dead-field visibleization');
  TestHandshakeTimeoutRejected;
  TestBufferSizeRejected;
  TestDefaultDeadFieldsStillAccepted;
  WriteLn('[PASS] factory request config dead-field visibleization');
end.
