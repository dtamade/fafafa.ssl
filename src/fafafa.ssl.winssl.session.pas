{**
 * Unit: fafafa.ssl.winssl.session
 * Purpose: WinSSL session compatibility shim
 *
 * 真实的 WinSSL session 实现已经收敛到
 * `fafafa.ssl.winssl.connection.TWinSSLSession`。
 * 本单元只保留兼容入口，避免外部代码直接引用
 * `fafafa.ssl.winssl.session` 时断裂。
 *}

unit fafafa.ssl.winssl.session;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.winssl.base,
  fafafa.ssl.winssl.connection;

type
  { Compatibility shim: canonical implementation lives in winssl.connection. }
  TWinSSLSession = class(fafafa.ssl.winssl.connection.TWinSSLSession)
  public
    constructor CreateFromData(const AData: TBytes);
    constructor CreateFromConnection(AContext: PCtxtHandle;
      AProtocol: TSSLProtocolVersion; const ACipher: string);
  end;

implementation

constructor TWinSSLSession.CreateFromData(const AData: TBytes);
begin
  inherited Create;
  Deserialize(AData);
end;

constructor TWinSSLSession.CreateFromConnection(AContext: PCtxtHandle;
  AProtocol: TSSLProtocolVersion; const ACipher: string);
var
  LSessionID: string;
begin
  inherited Create;

  if AContext = nil then
    Exit;

  // Keep the compatibility shim aligned with the current canonical conservative truth:
  // no direct risky Schannel session-info probe here; callers only get a fallback session id.
  LSessionID := Format('winssl-session-%p', [Pointer(AContext)]);
  SetSessionMetadata(LSessionID, AProtocol, ACipher, False);
end;

end.
