{
  Minimal test version of winssl.lib
}

unit fafafa.ssl.winssl.lib.test2;

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
  TWinSSLLibraryTest = class(TInterfacedObject, ISSLLibrary)
  private
    FInitialized: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
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

implementation

uses
  fafafa.ssl.winssl.context,
  fafafa.ssl.winssl.certificate,
  fafafa.ssl.winssl.certstore;

constructor TWinSSLLibraryTest.Create;
begin
  inherited Create;
  FInitialized := False;
end;

destructor TWinSSLLibraryTest.Destroy;
begin
  inherited Destroy;
end;

function TWinSSLLibraryTest.Initialize: Boolean;
begin
  FInitialized := True;
  Result := True;
end;

procedure TWinSSLLibraryTest.Finalize;
begin
  FInitialized := False;
end;

function TWinSSLLibraryTest.IsInitialized: Boolean;
begin
  Result := FInitialized;
end;

function TWinSSLLibraryTest.GetLibraryType: TSSLLibraryType;
begin
  Result := sslWinSSL;
end;

function TWinSSLLibraryTest.GetVersionString: string;
begin
  Result := 'Test';
end;

function TWinSSLLibraryTest.GetVersionNumber: Cardinal;
begin
  Result := 0;
end;

function TWinSSLLibraryTest.GetCompileFlags: string;
begin
  Result := '';
end;

function TWinSSLLibraryTest.IsProtocolSupported(AProtocol: TSSLProtocolVersion): Boolean;
begin
  Result := False;
end;

function TWinSSLLibraryTest.IsCipherSupported(const ACipherName: string): Boolean;
begin
  Result := False;
end;

function TWinSSLLibraryTest.IsFeatureSupported(AFeature: TSSLFeature): Boolean;
begin
  Result := False;
end;

function TWinSSLLibraryTest.GetCapabilities: TSSLBackendCapabilities;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

procedure TWinSSLLibraryTest.SetDefaultConfig(const AConfig: TSSLConfig);
begin
end;

function TWinSSLLibraryTest.GetDefaultConfig: TSSLConfig;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

function TWinSSLLibraryTest.GetLastError: Integer;
begin
  Result := 0;
end;

function TWinSSLLibraryTest.GetLastErrorString: string;
begin
  Result := '';
end;

procedure TWinSSLLibraryTest.ClearError;
begin
end;

function TWinSSLLibraryTest.GetStatistics: TSSLStatistics;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

procedure TWinSSLLibraryTest.ResetStatistics;
begin
end;

procedure TWinSSLLibraryTest.SetLogCallback(ACallback: TSSLLogCallback);
begin
end;

procedure TWinSSLLibraryTest.Log(ALevel: TSSLLogLevel; const AMessage: string);
begin
end;

function TWinSSLLibraryTest.CreateContext(AType: TSSLContextType): ISSLContext;
begin
  Result := nil;
end;

function TWinSSLLibraryTest.CreateCertificate: ISSLCertificate;
begin
  Result := nil;
end;

function TWinSSLLibraryTest.CreateCertificateStore: ISSLCertificateStore;
begin
  Result := nil;
end;

end.
