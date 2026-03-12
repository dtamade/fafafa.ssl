{
  fafafa.ssl.cert.advanced - Advanced Certificate Features

  Enterprise-grade certificate management:
  - OCSP (Online Certificate Status Protocol) client
  - CRL (Certificate Revocation List) manager
  - PKCS#12 (.p12/.pfx) import/export
}

unit fafafa.ssl.cert.advanced;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{ 禁用函数结果未初始化警告 - SetLength 已经初始化 TBytes }
{$WARN 5093 off}  // Function result variable of managed type does not seem initialized

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.cert.builder,
  fafafa.ssl.openssl.cert.builder,
  fafafa.ssl.errors,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.api.ocsp,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.pem;

type
  { OCSP Status }
  TOCSPStatus = (
    ocspGood,           // Certificate is valid
    ocspRevoked,        // Certificate has been revoked
    ocspUnknown,        // Status unknown
    ocspError           // Error checking status
  );

  { OCSP Response }
  TOCSPResponse = record
    Status: TOCSPStatus;
    RevokedAt: TDateTime;
    Reason: string;
    NextUpdate: TDateTime;
    ErrorMessage: string;
  end;

  { Test seam for deterministic OCSP status mapping checks }
  TOCSPStatusResolver = function(ACert: PX509; AIssuer: PX509;
    const AOCSPUrl: string; ATimeout: Integer): Integer;

  { Test seam for deterministic CRL expiry boundary checks }
  TCRLNowProvider = function: TDateTime;

  {**
   * IOCSPClient - OCSP Client interface
   * @stable 1.0
   * @locked 2025-12-24
   * @breaking-change-policy Requires major version bump
   *}
  IOCSPClient = interface
    ['{F6071819-1011-4345-6789-012345678901}']
    
    function CheckCertificate(const ACert: ICertificate; const AIssuer: ICertificate): TOCSPResponse;
    procedure SetResponderURL(const AURL: string);
    procedure SetTimeout(ASeconds: Integer);
  end;

  {**
   * ICRLManager - CRL Manager interface
   * @stable 1.0
   * @locked 2025-12-24
   * @breaking-change-policy Requires major version bump
   *}
  ICRLManager = interface
    ['{07182019-2021-4567-8901-234567890123}']
    
    procedure LoadFromURL(const AURL: string);
    procedure LoadFromFile(const AFile: string);
    procedure LoadFromPEM(const APEM: string);
    
    function IsRevoked(const ACert: ICertificate): Boolean;
    function GetRevokedDate(const ACert: ICertificate): TDateTime;
    function GetRevocationReason(const ACert: ICertificate): string;
    
    procedure Refresh;
    function IsExpired: Boolean;
    function GetNextUpdate: TDateTime;
  end;

  { PKCS#12 Options }
  TPKCS12Options = record
    FriendlyName: string;
    Password: string;
    Iterations: Integer;  // Key derivation iterations
    IncludeChain: Boolean; // Include certificate chain
  end;

  { PKCS#12 Manager }
  TPKCS12Manager = class
  public
    { Export certificate + key to PKCS#12 }
    class function CreatePKCS12(
      const ACert: ICertificate;
      const AKey: IPrivateKey;
      const AOptions: TPKCS12Options
    ): TBytes; static;
    
    class function CreatePKCS12ToFile(
      const ACert: ICertificate;
      const AKey: IPrivateKey;
      const AFile: string;
      const AOptions: TPKCS12Options
    ): Boolean; static;
    
    { Import from PKCS#12 }
    class function LoadFromPKCS12(
      const APKCS12: TBytes;
      const APassword: string;
      out ACert: ICertificate;
      out AKey: IPrivateKey
    ): Boolean; static;
    
    class function LoadFromPKCS12File(
      const AFile: string;
      const APassword: string;
      out ACert: ICertificate;
      out AKey: IPrivateKey
    ): Boolean; static;
  end;

{ Helper functions }
function DefaultPKCS12Options: TPKCS12Options;

{ Factory functions }
function CreateOCSPClient: IOCSPClient;
function CreateCRLManager: ICRLManager;

var
  { Keep nil in production: OCSP checks use OpenSSL CheckCertificateStatus }
  OCSPStatusResolverHook: TOCSPStatusResolver = nil;
  { Keep nil in production: IsExpired uses System.Now }
  CRLNowProviderHook: TCRLNowProvider = nil;



implementation

uses
  fafafa.ssl.cert.builder.impl,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.api.asn1,
  fafafa.ssl.openssl.api.pkcs12,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.evp;

{ Helper functions }

function ASN1TimeToDateTime(const ATime: PASN1_TIME): TDateTime;
var
  LTm: TM;
  LRawTime: AnsiString;
  LData: PByte;
  LLen: Integer;
  LYear, LMonth, LDay: Integer;
  LHour, LMinute, LSecond: Integer;
begin
  Result := 0;

  if ATime = nil then
    Exit;

  if Assigned(ASN1_STRING_get0_data) and Assigned(ASN1_STRING_length) then
  begin
    LLen := ASN1_STRING_length(ASN1_STRING(ATime));
    LData := ASN1_STRING_get0_data(ASN1_STRING(ATime));
    if (LLen > 0) and Assigned(LData) then
    begin
      SetString(LRawTime, PAnsiChar(LData), LLen);
      if (LLen >= 13) and (LRawTime[LLen] = 'Z') then
      begin
        if LLen = 13 then
        begin
          // UTCTime: YYMMDDHHMMSSZ
          LYear := StrToIntDef(Copy(string(LRawTime), 1, 2), -1);
          if LYear >= 0 then
          begin
            if LYear < 50 then
              LYear := 2000 + LYear
            else
              LYear := 1900 + LYear;

            LMonth := StrToIntDef(Copy(string(LRawTime), 3, 2), -1);
            LDay := StrToIntDef(Copy(string(LRawTime), 5, 2), -1);
            LHour := StrToIntDef(Copy(string(LRawTime), 7, 2), -1);
            LMinute := StrToIntDef(Copy(string(LRawTime), 9, 2), -1);
            LSecond := StrToIntDef(Copy(string(LRawTime), 11, 2), -1);

            if (LMonth > 0) and (LDay > 0) and
              (LHour >= 0) and (LMinute >= 0) and (LSecond >= 0) then
            begin
              try
                Result := EncodeDate(LYear, LMonth, LDay) +
                          EncodeTime(LHour, LMinute, LSecond, 0);
                Exit;
              except
                Result := 0;
              end;
            end;
          end;
        end
        else if LLen = 15 then
        begin
          // GeneralizedTime: YYYYMMDDHHMMSSZ
          LYear := StrToIntDef(Copy(string(LRawTime), 1, 4), -1);
          LMonth := StrToIntDef(Copy(string(LRawTime), 5, 2), -1);
          LDay := StrToIntDef(Copy(string(LRawTime), 7, 2), -1);
          LHour := StrToIntDef(Copy(string(LRawTime), 9, 2), -1);
          LMinute := StrToIntDef(Copy(string(LRawTime), 11, 2), -1);
          LSecond := StrToIntDef(Copy(string(LRawTime), 13, 2), -1);

          if (LYear > 0) and (LMonth > 0) and (LDay > 0) and
            (LHour >= 0) and (LMinute >= 0) and (LSecond >= 0) then
          begin
            try
              Result := EncodeDate(LYear, LMonth, LDay) +
                        EncodeTime(LHour, LMinute, LSecond, 0);
              Exit;
            except
              Result := 0;
            end;
          end;
        end;
      end;
    end;
  end;

  // Fallback: legacy conversion path
  if Assigned(ASN1_TIME_to_tm) then
  begin
    try
      if ASN1_TIME_to_tm(ATime, @LTm) = 1 then
        Result := EncodeDate(LTm.tm_year + 1900, LTm.tm_mon + 1, LTm.tm_mday) +
                  EncodeTime(LTm.tm_hour, LTm.tm_min, LTm.tm_sec, 0);
    except
      Result := 0;
    end;
  end;
end;

function RevocationReasonCodeToString(const AReasonCode: Integer): string;
begin
  case AReasonCode of
    0: Result := 'Unspecified';
    1: Result := 'KeyCompromise';
    2: Result := 'CACompromise';
    3: Result := 'AffiliationChanged';
    4: Result := 'Superseded';
    5: Result := 'CessationOfOperation';
    6: Result := 'CertificateHold';
    8: Result := 'RemoveFromCRL';
    9: Result := 'PrivilegeWithdrawn';
    10: Result := 'AACompromise';
  else
    Result := 'Unknown(' + IntToStr(AReasonCode) + ')';
  end;
end;

function DefaultPKCS12Options: TPKCS12Options;
begin
  Result.FriendlyName := '';
  Result.Password := '';
  Result.Iterations := 2048;
  Result.IncludeChain := False;
end;

{ Internal implementations }

type
  TOCSPClient = class(TInterfacedObject, IOCSPClient)
  private
    FResponderURL: string;
    FTimeout: Integer;
  public
    constructor Create;
    function CheckCertificate(const ACert: ICertificate; const AIssuer: ICertificate): TOCSPResponse;
    procedure SetResponderURL(const AURL: string);
    procedure SetTimeout(ASeconds: Integer);
  end;

{ TOCSPClient }

constructor TOCSPClient.Create;
begin
  inherited Create;
  FTimeout := 10; // Default timeout 10 seconds
end;

procedure TOCSPClient.SetResponderURL(const AURL: string);
begin
  FResponderURL := AURL;
end;

procedure TOCSPClient.SetTimeout(ASeconds: Integer);
begin
  FTimeout := ASeconds;
end;

function TOCSPClient.CheckCertificate(const ACert: ICertificate; const AIssuer: ICertificate): TOCSPResponse;
var
  LCertEx, LIssuerEx: fafafa.ssl.openssl.cert.builder.ICertificateEx;
  LX509, LIssuerX509: PX509;
  LStatus: Integer;
  LURL: string;
begin
  // Initialize result
  Result.Status := ocspUnknown;
  Result.ErrorMessage := '';
  Result.Reason := '';
  Result.RevokedAt := 0;
  Result.NextUpdate := 0;
  
  // Get extended interfaces
  if not Assigned(ACert) then
    RaiseUnsupported('Certificate handle access');

  if not Assigned(AIssuer) then
    RaiseUnsupported('Issuer certificate handle access');

  if not Supports(ACert, fafafa.ssl.openssl.cert.builder.ICertificateEx, LCertEx) then
    RaiseUnsupported('Certificate handle access');
    
  if not Supports(AIssuer, fafafa.ssl.openssl.cert.builder.ICertificateEx, LIssuerEx) then
    RaiseUnsupported('Issuer certificate handle access');
    
  LX509 := LCertEx.X509Handle;
  LIssuerX509 := LIssuerEx.X509Handle;

  if not Assigned(LX509) then
    RaiseUnsupported('Certificate handle access');

  if not Assigned(LIssuerX509) then
    RaiseUnsupported('Issuer certificate handle access');
  
  // Determine Responder URL
  LURL := FResponderURL;
  // Future Enhancement: Extract URL from AIA extension if FResponderURL is empty
  // Requires: X509_get_ext_d2i with NID_info_access parsing
  
  if LURL = '' then
  begin
    Result.Status := ocspError;
    Result.ErrorMessage := 'No OCSP responder URL provided';
    Exit;
  end;

  if (not Assigned(OCSPStatusResolverHook)) and
    TOpenSSLLoader.IsModuleLoaded(osmOCSP) and
    (not CheckCertificateStatusDependenciesAvailable) then
    RaiseUnsupported('OpenSSL API CheckCertificateStatus');
  
  try
    if Assigned(OCSPStatusResolverHook) then
      LStatus := OCSPStatusResolverHook(LX509, LIssuerX509, LURL, FTimeout)
    else
      // Call OpenSSL OCSP helper
      LStatus := CheckCertificateStatus(LX509, LIssuerX509, LURL, FTimeout);
    
    case LStatus of
      V_OCSP_CERTSTATUS_GOOD:
        Result.Status := ocspGood;
      V_OCSP_CERTSTATUS_REVOKED:
        begin
          Result.Status := ocspRevoked;
          // Revocation status is known, but revocation timestamp is unavailable
          // in this path. Keep explicit unknown semantic (0).
        end;
      V_OCSP_CERTSTATUS_UNKNOWN:
        Result.Status := ocspUnknown;
      else
        Result.Status := ocspError;
        Result.ErrorMessage := 'OCSP check failed with status: ' + IntToStr(LStatus);
    end;
  except
    on E: Exception do
    begin
      Result.Status := ocspError;
      Result.ErrorMessage := E.Message;
    end;
  end;
end;




type
  TCRLManagerImpl = class(TInterfacedObject, ICRLManager)
  private
    FCRL: PX509_CRL;
    FNextUpdate: TDateTime;
    procedure ParseCRL(const APEM: string);
    function TryGetRevokedEntry(const ACert: ICertificate; out ARevoked: Pointer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromURL(const AURL: string);
    procedure LoadFromFile(const AFile: string);
    procedure LoadFromPEM(const APEM: string);
    
    function IsRevoked(const ACert: ICertificate): Boolean;
    function GetRevokedDate(const ACert: ICertificate): TDateTime;
    function GetRevocationReason(const ACert: ICertificate): string;
    
    procedure Refresh;
    function IsExpired: Boolean;
    function GetNextUpdate: TDateTime;
  end;


{ TCRLManagerImpl }

procedure TCRLManagerImpl.LoadFromURL(const AURL: string);
begin
  // Out of Scope: Download CRL from URL requires HTTP client
  // Recommendation: Use external HTTP library (Synapse, Indy, fphttpclient) to download,
  // then call LoadFromPEM with the downloaded CRL data
  RaiseUnsupported('CRL URL loading - use external HTTP library');
end;

constructor TCRLManagerImpl.Create;
begin
  inherited Create;
  FCRL := nil;
  FNextUpdate := 0;
end;

destructor TCRLManagerImpl.Destroy;
begin
  if Assigned(FCRL) and Assigned(X509_CRL_free) then
    X509_CRL_free(FCRL);
  inherited;
end;

procedure TCRLManagerImpl.ParseCRL(const APEM: string);
var
  LBio: PBIO;
begin
  // Clear metadata first so parse failures cannot leak previous CRL state.
  FNextUpdate := 0;

  // Free existing CRL
  if Assigned(FCRL) then
  begin
    if not Assigned(X509_CRL_free) then
      RaiseUnsupported('OpenSSL API X509_CRL_free');
    X509_CRL_free(FCRL);
    FCRL := nil;
  end;

  if Trim(APEM) = '' then
    RaiseInvalidData('CRL PEM (empty)');

  if not Assigned(BIO_new_mem_buf) then
    RaiseUnsupported('OpenSSL API BIO_new_mem_buf');
  if not Assigned(PEM_read_bio_X509_CRL) then
    RaiseUnsupported('OpenSSL API PEM_read_bio_X509_CRL');
  if not Assigned(BIO_free) then
    RaiseUnsupported('OpenSSL API BIO_free');
  
  // Parse PEM
  LBio := BIO_new_mem_buf(PAnsiChar(APEM), Length(APEM));
  if not Assigned(LBio) then
    RaiseMemoryError('BIO creation');
    
  try
    FCRL := PEM_read_bio_X509_CRL(LBio, nil, nil, nil);
    if not Assigned(FCRL) then
      RaiseParseError('CRL data');
      
    // Extract next update time
    if Assigned(X509_CRL_get0_nextUpdate) then
      FNextUpdate := ASN1TimeToDateTime(X509_CRL_get0_nextUpdate(FCRL))
    else
      FNextUpdate := 0;
  finally
    BIO_free(LBio);
  end;
end;

procedure TCRLManagerImpl.LoadFromFile(const AFile: string);
var
  LStream: TFileStream;
  LPEM: string;
begin
  if not FileExists(AFile) then
    RaiseLoadError(AFile);

  LStream := TFileStream.Create(AFile, fmOpenRead);
  try
    if LStream.Size <= 0 then
      RaiseInvalidData('CRL file (empty)');

    SetLength(LPEM, LStream.Size);
    if LStream.Read(LPEM[1], Length(LPEM)) <> Length(LPEM) then
      RaiseLoadError(AFile);

    ParseCRL(LPEM);
  finally
    LStream.Free;
  end;
end;

procedure TCRLManagerImpl.LoadFromPEM(const APEM: string);
begin
  ParseCRL(APEM);
end;

function TCRLManagerImpl.TryGetRevokedEntry(const ACert: ICertificate; out ARevoked: Pointer): Boolean;
var
  LCertEx: fafafa.ssl.openssl.cert.builder.ICertificateEx;
  LX509: PX509;
begin
  Result := False;
  ARevoked := nil;

  if not Assigned(FCRL) then
    RaiseInvalidData('CRL (no CRL loaded)');

  if not Assigned(ACert) then
    RaiseUnsupported('Certificate handle access');

  if not Supports(ACert, fafafa.ssl.openssl.cert.builder.ICertificateEx, LCertEx) then
    RaiseUnsupported('Certificate handle access');

  if not Assigned(X509_CRL_get0_by_cert) then
    RaiseUnsupported('OpenSSL API X509_CRL_get0_by_cert');

  LX509 := LCertEx.X509Handle;
  if not Assigned(LX509) then
    RaiseUnsupported('Certificate handle access');

  Result := X509_CRL_get0_by_cert(FCRL, @ARevoked, LX509) = 1;
end;

function TCRLManagerImpl.IsRevoked(const ACert: ICertificate): Boolean;
var
  LRevoked: Pointer;
begin
  Result := TryGetRevokedEntry(ACert, LRevoked);
end;

function TCRLManagerImpl.GetRevokedDate(const ACert: ICertificate): TDateTime;
var
  LRevoked: Pointer;
  LRevocationDate: PASN1_TIME;
begin
  Result := 0;

  if not TryGetRevokedEntry(ACert, LRevoked) then
    Exit;

  if Assigned(X509_REVOKED_get0_revocationDate) then
  begin
    LRevocationDate := X509_REVOKED_get0_revocationDate(LRevoked);
    if Assigned(LRevocationDate) then
      Result := ASN1TimeToDateTime(LRevocationDate);
  end;
end;

function TCRLManagerImpl.GetRevocationReason(const ACert: ICertificate): string;
const
  NID_crl_reason = 141;
var
  LRevoked: Pointer;
  LReasonValue: Pointer;
  LReasonCode: Integer;
  LReasonCode64: Int64;
  LReasonLen: Integer;
  LReasonData: PByte;
begin
  Result := '';

  if not TryGetRevokedEntry(ACert, LRevoked) then
    Exit;

  if not Assigned(X509_REVOKED_get_ext_d2i) then
    Exit;

  Result := 'Unspecified';

  LReasonValue := X509_REVOKED_get_ext_d2i(LRevoked, NID_crl_reason, nil, nil);
  if Assigned(LReasonValue) then
  begin
    try
      LReasonCode := -1;
      if Assigned(ASN1_INTEGER_get_int64) then
      begin
        if ASN1_INTEGER_get_int64(@LReasonCode64, PASN1_INTEGER(LReasonValue)) = 1 then
          LReasonCode := LReasonCode64;
      end;
      if (LReasonCode < 0) and Assigned(ASN1_INTEGER_get) then
        LReasonCode := ASN1_INTEGER_get(PASN1_INTEGER(LReasonValue));
      if (LReasonCode < 0) and Assigned(ASN1_STRING_length) and Assigned(ASN1_STRING_get0_data) then
      begin
        LReasonLen := ASN1_STRING_length(ASN1_STRING(LReasonValue));
        LReasonData := ASN1_STRING_get0_data(ASN1_STRING(LReasonValue));
        if (LReasonLen = 1) and Assigned(LReasonData) then
          LReasonCode := LReasonData^;
      end;
      if LReasonCode < 0 then
        Result := ''
      else
        Result := RevocationReasonCodeToString(LReasonCode);
    finally
      if Assigned(ASN1_STRING_free) then
        ASN1_STRING_free(ASN1_STRING(LReasonValue));
    end;
  end;
end;

procedure TCRLManagerImpl.Refresh;
begin
  // Out of Scope: Re-download CRL requires HTTP client (see LoadFromURL)
  // Users should manually re-download and call LoadFromPEM
end;

function TCRLManagerImpl.IsExpired: Boolean;
var
  LNow: TDateTime;
begin
  if Assigned(CRLNowProviderHook) then
    LNow := CRLNowProviderHook()
  else
    LNow := Now;

  Result := LNow >= FNextUpdate;
end;

function TCRLManagerImpl.GetNextUpdate: TDateTime;
begin
  Result := FNextUpdate;
end;

{ TPKCS12Manager }

class function TPKCS12Manager.CreatePKCS12(
  const ACert: ICertificate;
  const AKey: IPrivateKey;
  const AOptions: TPKCS12Options
): TBytes;
var
  LP12: fafafa.ssl.openssl.api.pkcs12.PPKCS12;
  LCertEx: fafafa.ssl.openssl.cert.builder.ICertificateEx;
  LKeyEx: fafafa.ssl.openssl.cert.builder.IPrivateKeyEx;
  LX509: PX509;
  LEVP: PEVP_PKEY;
  LBio: PBIO;
  LPassBytes, LNameBytes: TBytes;
  LDataPtr: PAnsiChar;
  LDataLen: Integer;
begin
  SetLength(Result, 0);
  
  // Get extended interfaces with handle access
  if not Supports(ACert, fafafa.ssl.openssl.cert.builder.ICertificateEx, LCertEx) then
    RaiseUnsupported('Certificate handle access');
      
  if not Supports(AKey, fafafa.ssl.openssl.cert.builder.IPrivateKeyEx, LKeyEx) then
    RaiseUnsupported('Private key handle access');
  
  // Get OpenSSL handles
  LX509 := LCertEx.X509Handle;
  LEVP := LKeyEx.EVP_PKEYHandle;
  
  // Prepare password and name
  if AOptions.Password <> '' then
    LPassBytes := TEncoding.UTF8.GetBytes(UnicodeString(AOptions.Password));
  if AOptions.FriendlyName <> '' then
    LNameBytes := TEncoding.UTF8.GetBytes(UnicodeString(AOptions.FriendlyName));
  
  // Create PKCS#12 structure
  LP12 := fafafa.ssl.openssl.api.pkcs12.PKCS12_create(
    PAnsiChar(LPassBytes),
    PAnsiChar(LNameBytes),
    LEVP,
    LX509,
    nil,  // CA certs
    0, 0,  // Use default NIDs
    AOptions.Iterations,
    AOptions.Iterations,
    0
  );
  
  if not Assigned(LP12) then
    RaiseMemoryError('PKCS#12 structure creation');
  
  try
    // Write to memory BIO
    LBio := BIO_new(BIO_s_mem());
    if not Assigned(LBio) then
      RaiseMemoryError('BIO creation');
      
    try
      if fafafa.ssl.openssl.api.pkcs12.i2d_PKCS12_bio(LBio, LP12) <> 1 then
        RaiseSSLError('Failed to write PKCS#12 to BIO', sslErrIO);
      
      // Get data from BIO
      LDataLen := BIO_get_mem_data(LBio, @LDataPtr);
      if (LDataLen > 0) and Assigned(LDataPtr) then
      begin
        SetLength(Result, LDataLen);
        Move(LDataPtr^, Result[0], LDataLen);
      end;
    finally
      BIO_free(LBio);
    end;
  finally
    fafafa.ssl.openssl.api.pkcs12.PKCS12_free(LP12);
  end;
end;

class function TPKCS12Manager.CreatePKCS12ToFile(
  const ACert: ICertificate;
  const AKey: IPrivateKey;
  const AFile: string;
  const AOptions: TPKCS12Options
): Boolean;
var
  LP12: TBytes;
  LStream: TFileStream;
begin
  LP12 := CreatePKCS12(ACert, AKey, AOptions);
  
  LStream := TFileStream.Create(AFile, fmCreate);
  try
    LStream.Write(LP12[0], Length(LP12));
    Result := True;
  finally
    LStream.Free;
  end;
end;

class function TPKCS12Manager.LoadFromPKCS12(
  const APKCS12: TBytes;
  const APassword: string;
  out ACert: ICertificate;
  out AKey: IPrivateKey
): Boolean;
var
  LP12: fafafa.ssl.openssl.api.pkcs12.PPKCS12;
  LBio: PBIO;
  LCertPtr: PX509;
  LKeyPtr: PEVP_PKEY;
  LCAStack: Pointer;  // PSTACK_OF_X509
  LPassBytes: TBytes;
begin
  Result := False;
  ACert := nil;
  AKey := nil;
  
  if Length(APKCS12) = 0 then Exit;
  
  // Create BIO from bytes
  LBio := BIO_new_mem_buf(@APKCS12[0], Length(APKCS12));
  if not Assigned(LBio) then Exit;
  
  try
    // Read PKCS#12 from BIO
    LP12 := nil;
    LP12 := fafafa.ssl.openssl.api.pkcs12.d2i_PKCS12_bio(LBio, LP12);
    if not Assigned(LP12) then Exit;
    
    try
      LPassBytes := TEncoding.UTF8.GetBytes(UnicodeString(APassword));
      LCertPtr := nil;
      LKeyPtr := nil;
      LCAStack := nil;
      
      // Parse PKCS#12
      if fafafa.ssl.openssl.api.pkcs12.PKCS12_parse(
        LP12, PAnsiChar(LPassBytes),
        LKeyPtr, LCertPtr, LCAStack) = 1 then
      begin
        if Assigned(LCertPtr) and Assigned(LKeyPtr) then
        begin
          ACert := TCertificateImpl.CreateFromHandle(LCertPtr, True);
          AKey := TPrivateKeyImpl.CreateFromHandle(LKeyPtr, True);
          Result := True;
        end;
      end;
    finally
      fafafa.ssl.openssl.api.pkcs12.PKCS12_free(LP12);
    end;
  finally
    BIO_free(LBio);
  end;
end;

class function TPKCS12Manager.LoadFromPKCS12File(
  const AFile: string;
  const APassword: string;
  out ACert: ICertificate;
  out AKey: IPrivateKey
): Boolean;
var
  LStream: TFileStream;
  LData: TBytes;
begin
  LStream := TFileStream.Create(AFile, fmOpenRead);
  try
    SetLength(LData, LStream.Size);
    LStream.Read(LData[0], LStream.Size);
    Result := LoadFromPKCS12(LData, APassword, ACert, AKey);
  finally
    LStream.Free;
  end;
end;

{ Factory functions }

function CreateOCSPClient: IOCSPClient;
begin
  Result := TOCSPClient.Create;
end;

function CreateCRLManager: ICRLManager;
begin
  Result := TCRLManagerImpl.Create;
end;

end.
