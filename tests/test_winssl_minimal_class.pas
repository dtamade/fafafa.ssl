{**
 * Test minimal WinSSL class definition
 *}
program test_winssl_minimal_class;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.context,
  fafafa.ssl.winssl.certificate,
  fafafa.ssl.winssl.certstore;

type
  TMinimalWinSSLLibrary = class(TInterfacedObject, ISSLLibrary)
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

function TMinimalWinSSLLibrary.Initialize: Boolean;
begin
  Result := True;
end;

procedure TMinimalWinSSLLibrary.Finalize;
begin
end;

function TMinimalWinSSLLibrary.IsInitialized: Boolean;
begin
  Result := True;
end;

function TMinimalWinSSLLibrary.GetLibraryType: TSSLLibraryType;
begin
  Result := sslWinSSL;
end;

function TMinimalWinSSLLibrary.GetVersionString: string;
begin
  Result := 'Test';
end;

function TMinimalWinSSLLibrary.GetVersionNumber: Cardinal;
begin
  Result := 0;
end;

function TMinimalWinSSLLibrary.GetCompileFlags: string;
begin
  Result := '';
end;

function TMinimalWinSSLLibrary.IsProtocolSupported(AProtocol: TSSLProtocolVersion): Boolean;
begin
  Result := False;
end;

function TMinimalWinSSLLibrary.IsCipherSupported(const ACipherName: string): Boolean;
begin
  Result := False;
end;

function TMinimalWinSSLLibrary.IsFeatureSupported(AFeature: TSSLFeature): Boolean;
begin
  Result := False;
end;

function TMinimalWinSSLLibrary.GetCapabilities: TSSLBackendCapabilities;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

procedure TMinimalWinSSLLibrary.SetDefaultConfig(const AConfig: TSSLConfig);
begin
end;

function TMinimalWinSSLLibrary.GetDefaultConfig: TSSLConfig;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

function TMinimalWinSSLLibrary.GetLastError: Integer;
begin
  Result := 0;
end;

function TMinimalWinSSLLibrary.GetLastErrorString: string;
begin
  Result := '';
end;

procedure TMinimalWinSSLLibrary.ClearError;
begin
end;

function TMinimalWinSSLLibrary.GetStatistics: TSSLStatistics;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

procedure TMinimalWinSSLLibrary.ResetStatistics;
begin
end;

procedure TMinimalWinSSLLibrary.SetLogCallback(ACallback: TSSLLogCallback);
begin
end;

procedure TMinimalWinSSLLibrary.Log(ALevel: TSSLLogLevel; const AMessage: string);
begin
end;

function TMinimalWinSSLLibrary.CreateContext(AType: TSSLContextType): ISSLContext;
begin
  Result := nil;
end;

function TMinimalWinSSLLibrary.CreateCertificate: ISSLCertificate;
begin
  Result := nil;
end;

function TMinimalWinSSLLibrary.CreateCertificateStore: ISSLCertificateStore;
begin
  Result := nil;
end;

begin
  WriteLn('Step 1: Program started');
  WriteLn('Step 2: About to register minimal WinSSL backend...');
  TSSLFactory.RegisterLibrary(sslWinSSL, TMinimalWinSSLLibrary, 'Minimal WinSSL', 200);
  WriteLn('Step 3: Minimal WinSSL backend registered successfully');
  WriteLn('Step 4: About to exit normally');
  Halt(0);
end.
