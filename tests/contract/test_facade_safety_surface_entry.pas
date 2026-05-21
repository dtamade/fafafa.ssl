program test_facade_safety_surface_entry;

{$mode ObjFPC}{$H+}{$J-}

uses
  SysUtils,
  fafafa.ssl;

var
  LVersion: TSSLVersion;
  LKeyType: TKeyType;
  LCertFormat: TCertificateFormat;
  LKeySize: TKeySize;
  LTimeout: TTimeoutDuration;
  LBuffer: TBufferSize;
begin
  LVersion := StringToSSLVersion('TLS 1.3');
  if LVersion <> sslv_TLS13 then
    Halt(1);
  if SSLVersionToString(sslv_TLS12) <> 'TLS 1.2' then
    Halt(2);

  LKeyType := kt_RSA;
  if KeyTypeToString(LKeyType) <> 'RSA' then
    Halt(3);

  LCertFormat := cf_PKCS12;
  if CertificateFormatToString(LCertFormat) <> 'PKCS12' then
    Halt(4);

  LKeySize := TKeySize.Bits(256);
  if LKeySize.ToBytes <> 32 then
    Halt(5);

  LTimeout := TTimeoutDuration.Seconds(5);
  if LTimeout.ToMilliseconds <> 5000 then
    Halt(6);

  LBuffer := TBufferSize.KB(8);
  if LBuffer.ToBytes <> 8192 then
    Halt(7);
end.
