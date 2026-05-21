program test_context_builder_session_timeout_safety_entry;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.context.builder,
  fafafa.ssl.exceptions,
  fafafa.ssl.freepascal.lib;

var
  Ctx: ISSLContext;
  Raised: Boolean;
begin
  Ctx := TSSLContextBuilder.Create
    .WithBackend(sslFreePascal)
    .WithSessionTimeout(TTimeoutDuration.Minutes(2))
    .BuildClient;
  if Ctx.GetSessionTimeout <> 120 then
    Halt(1);

  Ctx := TSSLContextBuilder.Create
    .WithBackend(sslFreePascal)
    .WithSessionTimeout(90)
    .BuildClient;
  if Ctx.GetSessionTimeout <> 90 then
    Halt(2);

  Raised := False;
  try
    TSSLContextBuilder.Create
      .WithSessionTimeout(TTimeoutDuration.Milliseconds(1500));
  except
    on E: ESSLInvalidArgument do
      Raised := True;
  end;
  if not Raised then
    Halt(3);

  Raised := False;
  try
    TSSLContextBuilder.Create
      .WithSessionTimeout(TTimeoutDuration.Infinite);
  except
    on E: ESSLInvalidArgument do
      Raised := True;
  end;
  if not Raised then
    Halt(4);
end.
