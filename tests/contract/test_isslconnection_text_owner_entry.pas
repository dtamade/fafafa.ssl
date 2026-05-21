program test_isslconnection_text_owner_entry;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.connection.base;

type
  TMockTextConnection = class(TBaseSSLConnection)
  protected
    function DoRead(var ABuffer; ACount: Integer): Integer; override;
    function DoWrite(const ABuffer; ACount: Integer): Integer; override;
    function DoConnect: Boolean; override;
    function DoAccept: Boolean; override;
    function DoHandshakeInternal: TSSLHandshakeState; override;
    function DoShutdown: Boolean; override;
    procedure DoClose; override;
    function DoRenegotiate: Boolean; override;
    function DoGetError(ARet: Integer): TSSLErrorCode; override;
    function DoWantRead: Boolean; override;
    function DoWantWrite: Boolean; override;
    function DoGetProtocolVersion: TSSLProtocolVersion; override;
    function DoGetCipherName: string; override;
    function DoGetPeerCertificate: ISSLCertificate; override;
    function DoGetPeerCertificateChain: TSSLCertificateArray; override;
    function DoGetVerifyResult: Integer; override;
    function DoGetVerifyResultString: string; override;
    function DoGetSession: ISSLSession; override;
    procedure DoSetSession(ASession: ISSLSession); override;
    function DoIsSessionReused: Boolean; override;
    function DoGetSelectedALPNProtocol: string; override;
    function DoGetState: string; override;
    function DoGetNativeHandle: Pointer; override;
  end;

procedure AssertTrue(ACondition: Boolean; ACode: Integer);
begin
  if not ACondition then
    Halt(ACode);
end;

procedure AssertFalse(ACondition: Boolean; ACode: Integer);
begin
  if ACondition then
    Halt(ACode);
end;

procedure AssertEquals(const AExpected, AActual: string; ACode: Integer);
begin
  if AExpected <> AActual then
    Halt(ACode);
end;

function TMockTextConnection.DoRead(var ABuffer; ACount: Integer): Integer;
begin
  Result := 0;
end;

function TMockTextConnection.DoWrite(const ABuffer; ACount: Integer): Integer;
begin
  Result := ACount;
end;

function TMockTextConnection.DoConnect: Boolean;
begin
  Result := True;
end;

function TMockTextConnection.DoAccept: Boolean;
begin
  Result := True;
end;

function TMockTextConnection.DoHandshakeInternal: TSSLHandshakeState;
begin
  Result := sslHsCompleted;
end;

function TMockTextConnection.DoShutdown: Boolean;
begin
  Result := True;
end;

procedure TMockTextConnection.DoClose;
begin
end;

function TMockTextConnection.DoRenegotiate: Boolean;
begin
  Result := False;
end;

function TMockTextConnection.DoGetError(ARet: Integer): TSSLErrorCode;
begin
  Result := sslErrNone;
end;

function TMockTextConnection.DoWantRead: Boolean;
begin
  Result := False;
end;

function TMockTextConnection.DoWantWrite: Boolean;
begin
  Result := False;
end;

function TMockTextConnection.DoGetProtocolVersion: TSSLProtocolVersion;
begin
  Result := sslProtocolTLS13;
end;

function TMockTextConnection.DoGetCipherName: string;
begin
  Result := 'MOCK-CIPHER';
end;

function TMockTextConnection.DoGetPeerCertificate: ISSLCertificate;
begin
  Result := nil;
end;

function TMockTextConnection.DoGetPeerCertificateChain: TSSLCertificateArray;
begin
  Result := nil;
end;

function TMockTextConnection.DoGetVerifyResult: Integer;
begin
  Result := 0;
end;

function TMockTextConnection.DoGetVerifyResultString: string;
begin
  Result := 'OK';
end;

function TMockTextConnection.DoGetSession: ISSLSession;
begin
  Result := nil;
end;

procedure TMockTextConnection.DoSetSession(ASession: ISSLSession);
begin
end;

function TMockTextConnection.DoIsSessionReused: Boolean;
begin
  Result := False;
end;

function TMockTextConnection.DoGetSelectedALPNProtocol: string;
begin
  Result := '';
end;

function TMockTextConnection.DoGetState: string;
begin
  Result := 'mock';
end;

function TMockTextConnection.DoGetNativeHandle: Pointer;
begin
  Result := nil;
end;

var
  LConn: ISSLConnection;
  LTextIO: ISSLConnectionTextIO;
  LText: string;

begin
  LConn := TMockTextConnection.Create(nil);

  AssertTrue(Supports(LConn, ISSLConnectionTextIO, LTextIO), 11);
  AssertTrue(LConn.WriteString('') = LTextIO.WriteString(''), 12);
  AssertTrue(LConn.WriteString('hello') = LTextIO.WriteString('hello'), 13);

  LText := 'seed';
  AssertFalse(LTextIO.ReadString(LText), 14);
  AssertEquals('', LText, 15);

  LText := 'seed';
  AssertFalse(LConn.ReadString(LText), 16);
  AssertEquals('', LText, 17);
end.
