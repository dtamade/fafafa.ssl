program test_facade_optional_owner_surface_entry;

{$mode objfpc}{$H+}

uses
  fafafa.ssl;

type
  TFacadeOptionalOwnerSurfacePack = record
    Connection: ISSLConnection;
    ConnectionInfo: ISSLConnectionInfo;
    Diagnostics: ISSLDiagnostics;
    SessionResumption: ISSLSessionResumption;
    CertificateVerification: ISSLCertificateVerification;
    OCSPStapling: ISSLOCSPStapling;
    CertificateTransparency: ISSLCertificateTransparency;
    CertificateTransparencyValidation: ISSLCertificateTransparencyValidation;
    CertificateChain: TSSLCertificateArray;
    HealthStatus: TSSLHealthStatus;
    PerformanceMetrics: TSSLPerformanceMetrics;
    DiagnosticInfo: TSSLDiagnosticInfo;
  end;

procedure AcceptFacadeOptionalOwnerSurfacePack(
  const APack: TFacadeOptionalOwnerSurfacePack
);
begin
  if APack.Connection <> nil then
    Halt(1);
end;

var
  LPack: TFacadeOptionalOwnerSurfacePack;

begin
  AcceptFacadeOptionalOwnerSurfacePack(LPack);
end.
