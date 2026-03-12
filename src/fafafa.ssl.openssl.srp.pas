{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.srp`.
  New code should prefer `fafafa.ssl.openssl.api.srp`.
}

unit fafafa.ssl.openssl.srp;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.srp;

implementation

end.
