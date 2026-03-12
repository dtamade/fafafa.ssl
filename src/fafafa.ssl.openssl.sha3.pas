{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.sha3`.
  New code should prefer `fafafa.ssl.openssl.api.sha3.evp`.
}

unit fafafa.ssl.openssl.sha3;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.sha3.evp;

implementation

end.
