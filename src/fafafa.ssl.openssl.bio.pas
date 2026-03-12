{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.bio`.
  New code should prefer `fafafa.ssl.openssl.api.bio`.
}

unit fafafa.ssl.openssl.bio;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.bio;

implementation

end.
