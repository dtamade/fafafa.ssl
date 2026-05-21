program test_facade_builder_diagnostic_supporting_types_entry;

{$mode ObjFPC}{$H+}

uses
  fafafa.ssl,
  fafafa.ssl.context.builder;

var
  LBuilder: ISSLContextBuilder;
  LValidation: TBuildValidationResult;
  LDiagnostic: TSSLDiagnosticInfo;
  LError: TSSLErrorRecord;
begin
  LBuilder := TSSLContextBuilder.Create;
  LValidation := LBuilder.Validate;

  LError.ErrorCode := sslErrNone;
  LError.ErrorMessage := '';
  LError.Timestamp := 0;

  LDiagnostic := Default(TSSLDiagnosticInfo);
  SetLength(LDiagnostic.ErrorHistory, 1);
  LDiagnostic.ErrorHistory[0] := LError;

  if LValidation.HasErrors then
    Halt(1);
  if LDiagnostic.ErrorHistory[0].ErrorCode <> sslErrNone then
    Halt(2);
end.
