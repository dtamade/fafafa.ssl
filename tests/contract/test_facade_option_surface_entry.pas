program test_facade_option_surface_entry;

{$mode ObjFPC}{$H+}

uses
  fafafa.ssl;

var
  LOption: TSSLOption;
  LOptions: TSSLOptions;
  LConfig: TSSLConfig;
begin
  LOption := ssoEnableSNI;
  LOptions := [LOption, ssoEnableALPN, ssoDisableCompression];
  LConfig := Default(TSSLConfig);
  LConfig.Options := LOptions;

  if not (ssoEnableSNI in LConfig.Options) then
    Halt(1);
  if not (ssoEnableALPN in LOptions) then
    Halt(2);
  if ssoEnableSessionTickets in LOptions then
    Halt(3);
end.
