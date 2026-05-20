program test_facade_certificate_supporting_types_entry;

{$mode ObjFPC}{$H+}

uses
  fafafa.ssl;

var
  LValues: TSSLStringArray;
  LVerifyResult: TSSLCertVerifyResult;
  LFlags: TSSLCertVerifyFlags;
begin
  LValues := nil;
  LFlags := [sslCertVerifyIgnoreExpiry];

  LVerifyResult.Success := sslCertVerifyIgnoreExpiry in LFlags;
  LVerifyResult.ErrorCode := 0;
  LVerifyResult.ErrorMessage := '';
  LVerifyResult.ChainStatus := 0;
  LVerifyResult.RevocationStatus := 0;
  LVerifyResult.DetailedInfo := '';

  if Length(LValues) <> 0 then
    Halt(1);
  if not LVerifyResult.Success then
    Halt(2);
  if LVerifyResult.ErrorMessage <> '' then
    Halt(3);
end.
