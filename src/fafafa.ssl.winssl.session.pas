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
  fafafa.ssl.winssl.api,
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

const
  HexDigits: array[0..15] of Char = '0123456789ABCDEF';

constructor TWinSSLSession.CreateFromData(const AData: TBytes);
begin
  inherited Create;
  Deserialize(AData);
end;

constructor TWinSSLSession.CreateFromConnection(AContext: PCtxtHandle;
  AProtocol: TSSLProtocolVersion; const ACipher: string);
var
  LSecStatus: SECURITY_STATUS;
  LSessionInfo: SecPkgContext_SessionInfo;
  LSessionID: string;
  I: Integer;
begin
  inherited Create;

  if AContext = nil then
    Exit;

  LSessionID := '';
  if Assigned(QueryContextAttributesW) then
  begin
    FillChar(LSessionInfo, SizeOf(LSessionInfo), 0);
    LSecStatus := QueryContextAttributesW(AContext,
      SECPKG_ATTR_SESSION_INFO, @LSessionInfo);

    if (LSecStatus = SEC_E_OK) and (LSessionInfo.cbSessionId > 0) then
    begin
      SetLength(LSessionID, LSessionInfo.cbSessionId * 2);
      for I := 0 to LSessionInfo.cbSessionId - 1 do
      begin
        LSessionID[I * 2 + 1] := HexDigits[(LSessionInfo.rgbSessionId[I] shr 4) and $0F];
        LSessionID[I * 2 + 2] := HexDigits[LSessionInfo.rgbSessionId[I] and $0F];
      end;
    end;
  end;

  if LSessionID = '' then
    LSessionID := IntToHex(Int64(Now * 86400000), 16);

  SetSessionMetadata(LSessionID, AProtocol, ACipher, False);
end;

end.
