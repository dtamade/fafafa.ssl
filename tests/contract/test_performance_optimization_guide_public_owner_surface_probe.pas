program test_performance_optimization_guide_public_owner_surface_probe;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl;

var
  Conn1, Conn2: ISSLConnection;
  Session: ISSLSession;
  Resumption1, Resumption2: ISSLSessionResumption;
  Stream: TSSLStream;
  Diag: ISSLDiagnostics;
  Perf: TSSLPerformanceMetrics;
begin
  Conn1 := nil;
  Conn2 := nil;
  Session := nil;
  Resumption1 := nil;
  Resumption2 := nil;
  Stream := nil;
  Diag := nil;
  Perf.HandshakeTime := 0;

  if Assigned(Conn1) and Assigned(Conn2) and Assigned(Stream) then
  begin
    if Supports(Conn1, ISSLSessionResumption, Resumption1) then
      Session := Resumption1.GetSession;

    if Supports(Conn2, ISSLSessionResumption, Resumption2) and Assigned(Session) then
      Resumption2.SetSession(Session);

    if Supports(Stream.Connection, ISSLDiagnostics, Diag) then
      Perf := Diag.GetPerformanceMetrics;
  end;

  if Perf.HandshakeTime = -1 then
    WriteLn('unreachable');
end.
