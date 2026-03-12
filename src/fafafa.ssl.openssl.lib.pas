{
  fafafa.ssl.openssl.lib - OpenSSL 库管理兼容层

  描述:
    为 OpenSSL 后端提供更一致的规范单元名，统一到其它后端使用的
    `*.lib` 命名模式。当前实现委托给历史兼容单元
    `fafafa.ssl.openssl.backed`，以避免破坏现有调用方。
}

unit fafafa.ssl.openssl.lib;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.base,
  fafafa.ssl.openssl.backed;

type
  TOpenSSLLibraryPaths = fafafa.ssl.openssl.backed.TOpenSSLLibraryPaths;
  TOpenSSLLibrary = fafafa.ssl.openssl.backed.TOpenSSLLibrary;

procedure SetCustomLibraryPaths(const ACryptoPath, ASSLPath: string);
function GetCustomLibraryPaths: TOpenSSLLibraryPaths;
function IsUsingCustomPaths: Boolean;
procedure ClearCustomLibraryPaths;
function CreateOpenSSLLibrary: ISSLLibrary;
procedure RegisterOpenSSLBackend;
procedure UnregisterOpenSSLBackend;

implementation

procedure SetCustomLibraryPaths(const ACryptoPath, ASSLPath: string);
begin
  fafafa.ssl.openssl.backed.SetCustomLibraryPaths(ACryptoPath, ASSLPath);
end;

function GetCustomLibraryPaths: TOpenSSLLibraryPaths;
begin
  Result := fafafa.ssl.openssl.backed.GetCustomLibraryPaths;
end;

function IsUsingCustomPaths: Boolean;
begin
  Result := fafafa.ssl.openssl.backed.IsUsingCustomPaths;
end;

procedure ClearCustomLibraryPaths;
begin
  fafafa.ssl.openssl.backed.ClearCustomLibraryPaths;
end;

function CreateOpenSSLLibrary: ISSLLibrary;
begin
  Result := fafafa.ssl.openssl.backed.CreateOpenSSLLibrary;
end;

procedure RegisterOpenSSLBackend;
begin
  fafafa.ssl.openssl.backed.RegisterOpenSSLBackend;
end;

procedure UnregisterOpenSSLBackend;
begin
  fafafa.ssl.openssl.backed.UnregisterOpenSSLBackend;
end;

end.
