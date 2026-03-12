{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.lhash`.
  New code should prefer `fafafa.ssl.openssl.api.lhash`.
}

unit fafafa.ssl.openssl.lhash;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.lhash;

implementation

end.
