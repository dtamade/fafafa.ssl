{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.chacha`.
  New code should prefer `fafafa.ssl.openssl.api.chacha`.
}

unit fafafa.ssl.openssl.chacha;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.chacha;

implementation

end.
