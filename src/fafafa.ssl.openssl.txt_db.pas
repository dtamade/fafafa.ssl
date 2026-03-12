{
  Compatibility shim for legacy OpenSSL unit name `fafafa.ssl.openssl.txt_db`.
  New code should prefer `fafafa.ssl.openssl.api.txt_db`.
}

unit fafafa.ssl.openssl.txt_db;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.openssl.api.txt_db;

implementation

end.
