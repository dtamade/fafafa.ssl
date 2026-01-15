{
  Test unit to isolate the crash
}

unit fafafa.ssl.winssl.lib.test;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  Windows, SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.winssl.base,
  fafafa.ssl.winssl.api,
  fafafa.ssl.winssl.utils;

type
  TTestWinSSLLibrary = class(TInterfacedObject, ISSLLibrary)
  public
    function Initialize: Boolean;
    procedure Finalize;
    function IsInitialized: Boolean;
    function GetLibraryType: TSSLLibraryType;
    function GetVersionString: string;
    function GetVersionNumber: Cardinal;
    function GetCompileFlags: string;
    function IsProtocolSupported(AProtocol: TSSLProtocolVersion): Boolean;
    function IsCipherSupported(const ACipherName: string): Boolean;
    function IsFeatureSupported(AFeature: TSSLFeature): Boolean;
    function GetCapabilities: TSSLBackendCapabilities;
    procedure SetDefaultConfig(const AConfig: TSSLConfig);
    function GetDefaultConfig: TSSLConfig;
    function GetLastError: Integer;
    function GetLastErrorString: string;
    procedure ClearError;
    function GetStatistics: TSSLStatistics;
    procedure ResetStatistics;
    procedure SetLogCallback(ACallback: TSSLLogCallback);
    procedure Log(ALevel: TSSLLogLevel; const AMessage: string);
    function CreateContext(AType: TSSLContextType): ISSLContext;
    function CreateCertificate: ISSLCertificate;
    function CreateCertificateStore: ISSLCertificateStore;
  end;

procedure RegisterTestWinSSLBackend;

implementation

uses
  fafafa.ssl.winssl.context,
  fafafa.ssl.winssl.certificate,
  fafafa.ssl.winssl.certstore,
  fafafa.ssl.factory;

function TTestWinSSLLibrary.Initialize: Boolean;
begin
  Result := True;
end;

procedure TTestWinSSLLibrary.Finalize;
begin
end;

function TTestWinSSLLibrary.IsInitialized: Boolean;
begin
  Result := True;
end;

function TTestWinSSLLibrary.GetLibraryType: TSSLLibraryType;
begin
  Result := sslWinSSL;
end;

function TTestWinSSLLibrary.GetVersionString: string;
begin
  Result := 'Test';
end;

function TTestWinSSLLibrary.GetVersionNumber: Cardinal;
begin
  Result := 0;
end;

function TTestWinSSLLibrary.GetCompileFlags: string;
begin
  Result := '';
end;

function TTestWinSSLLibrary.IsProtocolSupported(AProtocol: TSSLProtocolVersion): Boolean;
begin
  Result := False;
end;

function TTestWinSSLLibrary.IsCipherSupported(const ACipherName: string): Boolean;
begin
  Result := False;
end;

function TTestWinSSLLibrary.IsFeatureSupported(AFeature: TSSLFeature): Boolean;
begin
  Result := False;
end;

function TTestWinSSLLibrary.GetCapabilities: TSSLBackendCapabilities;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

procedure TTestWinSSLLibrary.SetDefaultConfig(const AConfig: TSSLConfig);
begin
end;

function TTestWinSSLLibrary.GetDefaultConfig: TSSLConfig;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

function TTestWinSSLLibrary.GetLastError: Integer;
begin
  Result := 0;
end;

function TTestWinSSLLibrary.GetLastErrorString: string;
begin
  Result := '';
end;

procedure TTestWinSSLLibrary.ClearError;
begin
end;

function TTestWinSSLLibrary.GetStatistics: TSSLStatistics;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

procedure TTestWinSSLLibrary.ResetStatistics;
begin
end;

procedure TTestWinSSLLibrary.SetLogCallback(ACallback: TSSLLogCallback);
begin
end;

procedure TTestWinSSLLibrary.Log(ALevel: TSSLLogLevel; const AMessage: string);
begin
end;

function TTestWinSSLLibrary.CreateContext(AType: TSSLContextType): ISSLContext;
begin
  Result := nil;
end;

function TTestWinSSLLibrary.CreateCertificate: ISSLCertificate;
begin
  Result := nil;
end;

function TTestWinSSLLibrary.CreateCertificateStore: ISSLCertificateStore;
begin
  Result := nil;
end;

procedure RegisterTestWinSSLBackend;
begin
  TSSLFactory.RegisterLibrary(sslWinSSL, TTestWinSSLLibrary, 'Test WinSSL', 200);
end;

end.
