{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.des`.
  New code should prefer `fafafa.ssl.openssl.api.des`.
}

unit fafafa.ssl.openssl.des;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.des;

implementation

end.
