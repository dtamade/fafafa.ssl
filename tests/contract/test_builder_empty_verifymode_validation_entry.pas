program test_builder_empty_verifymode_validation_entry;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.context.builder,
  fafafa.ssl.base;

function WarningsContain(const AResult: TBuildValidationResult; const AFragment: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to AResult.WarningCount - 1 do
    if Pos(AFragment, AResult.Warnings[I]) > 0 then
      Exit(True);
end;

var
  Builder: ISSLContextBuilder;
  ResultInfo: TBuildValidationResult;
  Ctx: ISSLContext;
begin
  Builder := TSSLContextBuilder.Create
    .ImportFromJSON('{"verify_modes": []}');
  ResultInfo := Builder.ValidateClient;
  if not ResultInfo.HasWarnings then
    Halt(1);
  if not WarningsContain(ResultInfo, 'Certificate verification is disabled') then
    Halt(2);
  Ctx := Builder.BuildClient;
  if Ctx.GetVerifyMode <> [] then
    Halt(3);

  Builder := TSSLContextBuilder.Create
    .ImportFromINI('verify_modes=' + LineEnding);
  ResultInfo := Builder.ValidateClient;
  if not ResultInfo.HasWarnings then
    Halt(4);
  if not WarningsContain(ResultInfo, 'Certificate verification is disabled') then
    Halt(5);
  Ctx := Builder.BuildClient;
  if Ctx.GetVerifyMode <> [] then
    Halt(6);
end.
