{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.buffer`.
  New code should prefer `fafafa.ssl.openssl.api.buffer`.
}

unit fafafa.ssl.openssl.buffer;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.buffer;

implementation

end.
