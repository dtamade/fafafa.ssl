{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.bn`.
  New code should prefer `fafafa.ssl.openssl.api.bn`.
}

unit fafafa.ssl.openssl.bn;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.bn;

implementation

end.
