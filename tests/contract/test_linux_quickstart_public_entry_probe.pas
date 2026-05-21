program test_linux_quickstart_public_entry_probe;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl;

var
  Lib: ISSLLibrary;
begin
  Lib := TSSLFactory.GetLibraryInstance(sslAutoDetect);
  WriteLn('Detected: ', LibraryTypeToString(Lib.GetLibraryType));
end.
