{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.evp`.
  New code should prefer `fafafa.ssl.openssl.api.evp`.
}

unit fafafa.ssl.openssl.evp;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.evp;

implementation

end.
