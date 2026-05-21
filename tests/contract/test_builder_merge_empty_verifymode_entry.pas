program test_builder_merge_empty_verifymode_entry;

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

procedure AssertMergedEmptyVerifyModes(const ASource: ISSLContextBuilder; ABaseExitCode: Integer);
var
  Target: ISSLContextBuilder;
  Validation: TBuildValidationResult;
  Ctx: ISSLContext;
begin
  Target := TSSLContextBuilder.Create.WithVerifyPeer;
  Target.Merge(ASource);

  Validation := Target.ValidateClient;
  if not Validation.HasWarnings then
    Halt(ABaseExitCode + 1);
  if not WarningsContain(Validation, 'Certificate verification is disabled') then
    Halt(ABaseExitCode + 2);

  Ctx := Target.BuildClient;
  if Ctx.GetVerifyMode <> [] then
    Halt(ABaseExitCode + 3);
end;

begin
  AssertMergedEmptyVerifyModes(
    TSSLContextBuilder.Create.ImportFromJSON('{"verify_modes": []}'),
    0
  );

  AssertMergedEmptyVerifyModes(
    TSSLContextBuilder.Create.ImportFromINI('verify_modes=' + LineEnding),
    10
  );
end.
