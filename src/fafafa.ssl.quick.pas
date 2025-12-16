{
  fafafa.ssl.quick - Quick SSL/TLS Convenience API
  
  Provides simple one-liner methods for common SSL/TLS operations.
  HTTP operations delegate to TSimpleHTTPSClient for robust socket handling.
  
  Usage:
    // One-liner HTTPS GET
    Response := TSSLQuick.Get('https://example.com/api');
    
    // Generate self-signed certificate
    KeyPair := TSSLQuick.GenerateSelfSigned('localhost');
}

unit fafafa.ssl.quick;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.cert.builder,
  fafafa.ssl.context.builder,
  {$IFDEF WINDOWS}
  WinSock2,
  {$ELSE}
  fafafa.ssl.sockets,
  {$ENDIF}
  fafafa.ssl.factory;

type
  { Quick certificate info }
  TQuickCertInfo = record
    Subject: string;
    Issuer: string;
    ValidFrom: TDateTime;
    ValidUntil: TDateTime;
    IsValid: Boolean;
    IsExpired: Boolean;
    DaysUntilExpiry: Integer;
    Fingerprint: string;
    ErrorMessage: string;
  end;

  { TSSLQuick - Simple one-liner SSL/TLS operations }
  TSSLQuick = class
  public
    { ==================== Quick HTTP (Delegates to TSimpleHTTPSClient) ==================== }
    
    {**
     * Perform an HTTPS GET request.
     * @param AURL Full URL (must start with https://)
     * @return Response body as string
     *}
    class function Get(const AURL: string): string; static;
    
    {**
     * Perform an HTTPS POST request.
     * @param AURL Full URL
     * @param ABody Request body
     * @return Response body as string
     *}
    class function Post(const AURL, ABody: string): string; static;
    
    {**
     * Try HTTP GET, returning success/failure without exception.
     *}
    class function TryGet(const AURL: string; out AResponse: string): Boolean; static;
    
    {**
     * Try HTTP POST, returning success/failure without exception.
     *}
    class function TryPost(const AURL, ABody: string; out AResponse: string): Boolean; static;
    
    { ==================== Quick Connection ==================== }
    
    {**
     * Connect to a secure server (one-liner).
     * @param AHost Hostname
     * @param APort Port (default 443)
     * @return Established SSL connection
     *}
    class function Connect(const AHost: string; APort: Word = 443): ISSLConnection; static;
    
    {**
     * Connect with strict peer verification.
     *}
    class function ConnectWithVerify(const AHost: string; APort: Word = 443): ISSLConnection; static;
    
    {**
     * Check if server certificate is valid.
     *}
    class function IsCertificateValid(const AHost: string; APort: Word = 443): Boolean; static;
    
    {**
     * Get server certificate.
     *}
    class function GetServerCertificate(const AHost: string; APort: Word = 443): ISSLCertificate; static;
    
    {**
     * Get certificate information as a quick summary.
     * @param AHost Server hostname
     * @param APort Server port (default 443)
     * @return TQuickCertInfo containing certificate details
     *}
    class function GetCertificateInfo(const AHost: string; APort: Word = 443): TQuickCertInfo; static;
    
    { Internal helper }
    class function CreateSocket(const AHost: string; APort: Word): THandle; static;
    
    { ==================== Quick Certificate Generation ==================== }
    
    {**
     * Generate a self-signed certificate for testing.
     * @param ACommonName Certificate common name (e.g., 'localhost')
     * @param AValidDays Validity period in days
     * @return Key pair containing certificate and private key
     *}
    class function GenerateSelfSigned(const ACommonName: string; 
      AValidDays: Integer = 365): IKeyPairWithCertificate; static;
    
    {**
     * Generate a self-signed server certificate with SANs.
     *}
    class function GenerateServerCert(const ACommonName: string;
      ADomains: array of string; AValidDays: Integer = 365): IKeyPairWithCertificate; static;
      
    {**
     * Generate a self-signed CA certificate.
     *}
    class function GenerateCACert(const ACommonName, AOrgName: string;
      AValidDays: Integer = 3650): IKeyPairWithCertificate; static;
    
    {**
     * Generate certificate and private key files at specified paths.
     *}
    class function GenerateCertFiles(const ACommonName, ACertPath, AKeyPath: string;
      AValidDays: Integer = 365): Boolean; static;
  end;

implementation

uses
  fafafa.ssl.http.simple;

{ TSSLQuick }

class function TSSLQuick.Get(const AURL: string): string;
begin
  Result := TSimpleHTTPSClient.Get(AURL);
end;

class function TSSLQuick.Post(const AURL, ABody: string): string;
begin
  Result := TSimpleHTTPSClient.Post(AURL, ABody);
end;

class function TSSLQuick.TryGet(const AURL: string; out AResponse: string): Boolean;
begin
  try
    AResponse := TSimpleHTTPSClient.Get(AURL);
    Result := True;
  except
    AResponse := '';
    Result := False;
  end;
end;

class function TSSLQuick.TryPost(const AURL, ABody: string; out AResponse: string): Boolean;
begin
  try
    AResponse := TSimpleHTTPSClient.Post(AURL, ABody);
    Result := True;
  except
    AResponse := '';
    Result := False;
  end;
end;



class function TSSLQuick.CreateSocket(const AHost: string; APort: Word): THandle;
var
  {$IFDEF WINDOWS}
  LWSAData: TWSAData;
  {$ENDIF}
  LAddr: TSockAddrIn;
  LHostEnt: PHostEnt;
  LSocket: THandle;
begin
  Result := THandle(INVALID_SOCKET);
  
  {$IFDEF WINDOWS}
  if WSAStartup($0202, LWSAData) <> 0 then Exit;
  
  LSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if LSocket = THandle(INVALID_SOCKET) then
  begin
    WSACleanup;
    Exit;
  end;
  
  FillChar(LAddr, SizeOf(LAddr), 0);
  LAddr.sin_family := AF_INET;
  LAddr.sin_port := htons(APort);
  LAddr.sin_addr.S_addr := inet_addr(PAnsiChar(AnsiString(AHost)));
  
  if LAddr.sin_addr.S_addr = INADDR_NONE then
  begin
    LHostEnt := gethostbyname(PAnsiChar(AnsiString(AHost)));
    if LHostEnt = nil then
    begin
      closesocket(LSocket);
      WSACleanup;
      Exit;
    end;
    LAddr.sin_addr := PInAddr(LHostEnt^.h_addr_list^)^;
  end;
  
  if WinSock2.connect(LSocket, @LAddr, SizeOf(LAddr)) = SOCKET_ERROR then
  begin
    closesocket(LSocket);
    WSACleanup;
    Exit;
  end;
  {$ELSE}
  LSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if LSocket = INVALID_SOCKET then Exit;
  
  FillChar(LAddr, SizeOf(LAddr), 0);
  LAddr.sin_family := AF_INET;
  LAddr.sin_port := htons(APort);
  LAddr.sin_addr.s_addr := inet_addr(PAnsiChar(AnsiString(AHost)));
  
  if LAddr.sin_addr.s_addr = INADDR_NONE then
  begin
    LHostEnt := gethostbyname(PAnsiChar(AnsiString(AHost)));
    if LHostEnt = nil then
    begin
      close(LSocket);
      Exit;
    end;
    LAddr.sin_addr := PInAddr(LHostEnt^.h_addr_list^)^;
  end;
  
  if fafafa.ssl.sockets.connect(LSocket, @LAddr, SizeOf(LAddr)) = SOCKET_ERROR then
  begin
    close(LSocket);
    Exit;
  end;
  {$ENDIF}
  
  Result := LSocket;
end;

class function TSSLQuick.Connect(const AHost: string; APort: Word): ISSLConnection;
var
  LSocket: THandle;
  LContext: ISSLContext;
begin
  LSocket := CreateSocket(AHost, APort);
  if LSocket = THandle(INVALID_SOCKET) then
    raise ESSLException.Create('Failed to create socket connection');
    
  try
    LContext := TSSLContextBuilder.Create
      .WithTLS12And13
      .WithSNI(AHost)
      .WithVerifyNone // Default to no verify for quick connect (convenience)
      .BuildClient;
      
    Result := LContext.CreateConnection(LSocket);
    if not Result.Connect then
      raise ESSLException.Create('SSL handshake failed');
  except
    {$IFDEF WINDOWS}
    closesocket(LSocket);
    WSACleanup; // Note: This might be problematic if other sockets use WSA
    {$ELSE}
    close(LSocket);
    {$ENDIF}
    raise;
  end;
end;

class function TSSLQuick.ConnectWithVerify(const AHost: string; APort: Word): ISSLConnection;
var
  LSocket: THandle;
  LContext: ISSLContext;
begin
  LSocket := CreateSocket(AHost, APort);
  if LSocket = THandle(INVALID_SOCKET) then
    raise ESSLException.Create('Failed to create socket connection');
    
  try
    LContext := TSSLContextBuilder.CreateWithSafeDefaults
      .WithSNI(AHost)
      .WithSystemRoots
      .BuildClient;
      
    Result := LContext.CreateConnection(LSocket);
    if not Result.Connect then
      raise ESSLException.Create('SSL handshake failed');
  except
    {$IFDEF WINDOWS}
    closesocket(LSocket);
    {$ELSE}
    close(LSocket);
    {$ENDIF}
    raise;
  end;
end;

class function TSSLQuick.IsCertificateValid(const AHost: string; APort: Word): Boolean;
var
  LConn: ISSLConnection;
begin
  Result := False;
  try
    LConn := ConnectWithVerify(AHost, APort);
    Result := True; // If handshake succeeds with verify, cert is valid
    LConn.Close;
  except
    Result := False;
  end;
end;

class function TSSLQuick.GetServerCertificate(const AHost: string; APort: Word): ISSLCertificate;
var
  LConn: ISSLConnection;
begin
  // Use Connect (no verify) to get cert even if invalid
  LConn := Connect(AHost, APort);
  try
    Result := LConn.GetPeerCertificate;
  finally
    LConn.Close;
  end;
end;

class function TSSLQuick.GetCertificateInfo(const AHost: string; APort: Word): TQuickCertInfo;
var
  LCert: ISSLCertificate;
  LNow: TDateTime;
begin
  // Initialize result
  FillChar(Result, SizeOf(Result), 0);
  Result.IsValid := False;
  Result.IsExpired := True;
  Result.DaysUntilExpiry := -1;
  
  try
    LCert := GetServerCertificate(AHost, APort);
    if LCert <> nil then
    begin
      Result.Subject := LCert.GetSubject;
      Result.Issuer := LCert.GetIssuer;
      Result.ValidFrom := LCert.GetNotBefore;
      Result.ValidUntil := LCert.GetNotAfter;
      Result.Fingerprint := LCert.GetFingerprint(sslHashSHA256);
      
      LNow := Now;
      Result.IsExpired := LNow > Result.ValidUntil;
      Result.IsValid := (LNow >= Result.ValidFrom) and (LNow <= Result.ValidUntil);
      Result.DaysUntilExpiry := Trunc(Result.ValidUntil - LNow);
    end
    else
      Result.ErrorMessage := 'No certificate returned';
  except
    on E: Exception do
    begin
      Result.IsValid := False;
      Result.ErrorMessage := E.Message;
    end;
  end;
end;

class function TSSLQuick.GenerateSelfSigned(const ACommonName: string;
  AValidDays: Integer): IKeyPairWithCertificate;
begin
  Result := TCertificateBuilder.Create
    .WithCommonName(ACommonName)
    .ValidFor(AValidDays)
    .WithRSAKey(2048)
    .SelfSigned;
end;

class function TSSLQuick.GenerateServerCert(const ACommonName: string;
  ADomains: array of string; AValidDays: Integer): IKeyPairWithCertificate;
var
  LBuilder: ICertificateBuilder;
  I: Integer;
begin
  LBuilder := TCertificateBuilder.Create
    .WithCommonName(ACommonName)
    .ValidFor(AValidDays)
    .WithRSAKey(2048)
    .AsServerCert;
  
  for I := Low(ADomains) to High(ADomains) do
    LBuilder := LBuilder.AddSubjectAltName('DNS:' + ADomains[I]);
  
  Result := LBuilder.SelfSigned;
end;

class function TSSLQuick.GenerateCACert(const ACommonName, AOrgName: string;
  AValidDays: Integer): IKeyPairWithCertificate;
begin
  Result := TCertificateBuilder.Create
    .WithCommonName(ACommonName)
    .WithOrganization(AOrgName)
    .ValidFor(AValidDays)
    .WithRSAKey(4096)
    .AsCA
    .SelfSigned;
end;

class function TSSLQuick.GenerateCertFiles(const ACommonName, ACertPath, AKeyPath: string;
  AValidDays: Integer): Boolean;
var
  LKeyPair: IKeyPairWithCertificate;
  LCertFile, LKeyFile: TFileStream;
  LCertPEM, LKeyPEM: string;
begin
  Result := False;
  try
    LKeyPair := GenerateSelfSigned(ACommonName, AValidDays);
    LCertPEM := LKeyPair.GetCertificate.ToPEM;
    LKeyPEM := LKeyPair.GetPrivateKey.ToPEM;
    
    // Write certificate file
    LCertFile := TFileStream.Create(ACertPath, fmCreate);
    try
      LCertFile.Write(LCertPEM[1], Length(LCertPEM));
    finally
      LCertFile.Free;
    end;
    
    // Write private key file
    LKeyFile := TFileStream.Create(AKeyPath, fmCreate);
    try
      LKeyFile.Write(LKeyPEM[1], Length(LKeyPEM));
    finally
      LKeyFile.Free;
    end;
    
    Result := True;
  except
    // Failed to generate or write files
  end;
end;

end.
