program test_facade_zerocopy_supporting_type_entry;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.encoding,
  fafafa.ssl.crypto.utils;

var
  LView: TBytesView;
  LHash: TBytes;
  LBase64: string;
begin
  LView := TBytesView.Empty;

  if False then
  begin
    LBase64 := TEncodingUtils.Base64EncodeView(LView);
    LHash := TCryptoUtils.SHA256View(LView);
    if (Length(LBase64) + Length(LHash)) = -1 then
      Halt(9);
  end;

  if not LView.IsEmpty then
    Halt(1);
  if LView.Length <> 0 then
    Halt(2);
end.
