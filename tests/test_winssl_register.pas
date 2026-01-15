{**
 * 测试手动注册 WinSSL 后端
 *}

program test_winssl_register;

{$mode ObjFPC}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.winssl.base,
  fafafa.ssl.winssl.api,
  fafafa.ssl.winssl.utils,
  fafafa.ssl.winssl.context,
  fafafa.ssl.winssl.certificate,
  fafafa.ssl.winssl.certstore;

type
  { 复制 TWinSSLLibrary 的定义 }
  TWinSSLLibrary = class(TInterfacedObject, ISSLLibrary)
  private
    FInitialized: Boolean;
    FDefaultConfig: TSSLConfig;
    FStatistics: TSSLStatistics;
    FLastError: Integer;
    FLastErrorString: string;
    FLogCallback: TSSLLogCallback;
    FLogLevel: TSSLLogLevel;
    FWindowsVersion: record
      Major: DWORD;
      Minor: DWORD;
      Build: DWORD;
      IsServer: Boolean;
    end;
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

constructor TWinSSLLibrary.Create;
begin
  inherited Create;
  FInitialized := False;
end;

destructor TWinSSLLibrary.Destroy;
begin
  inherited Destroy;
end;

function TWinSSLLibrary.Initialize: Boolean;
begin
  FInitialized := True;
  Result := True;
end;

procedure TWinSSLLibrary.Finalize;
begin
  FInitialized := False;
end;

function TWinSSLLibrary.IsInitialized: Boolean;
begin
  Result := FInitialized;
end;

function TWinSSLLibrary.GetLibraryType: TSSLLibraryType;
begin
  Result := sslWinSSL;
end;

function TWinSSLLibrary.GetVersionString: string;
begin
  Result := 'Test WinSSL';
end;

function TWinSSLLibrary.GetVersionNumber: Cardinal;
begin
  Result := 0;
end;

function TWinSSLLibrary.GetCompileFlags: string;
begin
  Result := '';
end;

function TWinSSLLibrary.IsProtocolSupported(AProtocol: TSSLProtocolVersion): Boolean;
begin
  Result := False;
end;

function TWinSSLLibrary.IsCipherSupported(const ACipherName: string): Boolean;
begin
  Result := False;
end;

function TWinSSLLibrary.IsFeatureSupported(AFeature: TSSLFeature): Boolean;
begin
  Result := False;
end;

function TWinSSLLibrary.GetCapabilities: TSSLBackendCapabilities;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

procedure TWinSSLLibrary.SetDefaultConfig(const AConfig: TSSLConfig);
begin
  FDefaultConfig := AConfig;
end;

function TWinSSLLibrary.GetDefaultConfig: TSSLConfig;
begin
  Result := FDefaultConfig;
end;

function TWinSSLLibrary.GetLastError: Integer;
begin
  Result := FLastError;
end;

function TWinSSLLibrary.GetLastErrorString: string;
begin
  Result := FLastErrorString;
end;

procedure TWinSSLLibrary.ClearError;
begin
  FLastError := 0;
  FLastErrorString := '';
end;

function TWinSSLLibrary.GetStatistics: TSSLStatistics;
begin
  Result := FStatistics;
end;

procedure TWinSSLLibrary.ResetStatistics;
begin
  FillChar(FStatistics, SizeOf(FStatistics), 0);
end;

procedure TWinSSLLibrary.SetLogCallback(ACallback: TSSLLogCallback);
begin
  FLogCallback := ACallback;
end;

procedure TWinSSLLibrary.Log(ALevel: TSSLLogLevel; const AMessage: string);
begin
end;

function TWinSSLLibrary.CreateContext(AType: TSSLContextType): ISSLContext;
begin
  Result := nil;
end;

function TWinSSLLibrary.CreateCertificate: ISSLCertificate;
begin
  Result := nil;
end;

function TWinSSLLibrary.CreateCertificateStore: ISSLCertificateStore;
begin
  Result := nil;
end;

begin
  WriteLn('Step 1: Starting test...');
  try
    WriteLn('Step 2: Registering WinSSL backend...');
    TSSLFactory.RegisterLibrary(sslWinSSL, TWinSSLLibrary, 'Test WinSSL', 200);
    WriteLn('Step 3: Registration successful');
    WriteLn('SUCCESS');
    Halt(0);
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
