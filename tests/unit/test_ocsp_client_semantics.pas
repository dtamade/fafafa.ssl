program test_ocsp_client_semantics;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.cert,
  fafafa.ssl.cert.builder,
  fafafa.ssl.openssl.cert.builder,
  fafafa.ssl.cert.advanced,
  fafafa.ssl,
  fafafa.ssl.openssl.api.ocsp,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.api.x509;

var
  TestsPassed: Integer = 0;
  TestsFailed: Integer = 0;
  GForcedStatus: Integer = V_OCSP_CERTSTATUS_ERROR;
  GVerifyResponseStatusCalls: Integer = 0;

procedure Pass(const AName: string);
begin
  Inc(TestsPassed);
  WriteLn('[PASS] ', AName);
end;

procedure Fail(const AName, ADetail: string);
begin
  Inc(TestsFailed);
  WriteLn('[FAIL] ', AName, ': ', ADetail);
end;

procedure Check(const AName: string; ACondition: Boolean; const ADetail: string = '');
begin
  if ACondition then
    Pass(AName)
  else
    Fail(AName, ADetail);
end;

function ForcedOCSPStatus(ACert: PX509; AIssuer: PX509;
  const AOCSPUrl: string; ATimeout: Integer): Integer;
begin
  Result := GForcedStatus;
end;

function StrictHandleOCSPStatus(ACert: PX509; AIssuer: PX509;
  const AOCSPUrl: string; ATimeout: Integer): Integer;
begin
  if not Assigned(ACert) then
    raise Exception.Create('nil leaf x509 passed to resolver');
  if not Assigned(AIssuer) then
    raise Exception.Create('nil issuer x509 passed to resolver');
  Result := V_OCSP_CERTSTATUS_GOOD;
end;

function DummyOCSPResponseStatus(resp: POCSP_RESPONSE): Integer; cdecl;
begin
  Result := 0;
end;

function CountingOCSPResponseStatus(resp: POCSP_RESPONSE): Integer; cdecl;
begin
  Inc(GVerifyResponseStatusCalls);
  Result := OCSP_RESPONSE_STATUS_SUCCESSFUL;
end;

function DummyOCSPResponseGet1Basic(resp: POCSP_RESPONSE): POCSP_BASICRESP; cdecl;
begin
  Result := nil;
end;

function DummyOCSPBasicRespVerify(bs: POCSP_BASICRESP; certs: PSTACK_OF_X509;
  st: PX509_STORE; flags: Cardinal): Integer; cdecl;
begin
  Result := 1;
end;

function DummyOCSPCertToID(const dgst: PEVP_MD; const subject: PX509;
  const issuer: PX509): POCSP_CERTID; cdecl;
begin
  Result := nil;
end;

function DummyOCSPRespFindStatus(bs: POCSP_BASICRESP; id: POCSP_CERTID; status: PInteger;
  reason: PInteger; revtime: PPASN1_GENERALIZEDTIME; thisupd: PPASN1_GENERALIZEDTIME;
  nextupd: PPASN1_GENERALIZEDTIME): Integer; cdecl;
begin
  Result := 0;
end;

function DummyOCSPCheckValidity(thisupd: Pointer;
  nextupd: Pointer; sec: Integer; maxsec: Integer): Integer; cdecl;
begin
  Result := 1;
end;

function DummyOCSPRequestNew: POCSP_REQUEST; cdecl;
begin
  Result := nil;
end;

procedure DummyOCSPRequestFree(a: POCSP_REQUEST); cdecl;
begin
end;

procedure DummyOCSPResponseFree(a: POCSP_RESPONSE); cdecl;
begin
end;

procedure DummyOCSPBasicRespFree(a: POCSP_BASICRESP); cdecl;
begin
end;

procedure DummyOCSPCertIDFree(a: POCSP_CERTID); cdecl;
begin
end;

procedure InstallDeterministicDependencyBaseline;
begin
  OCSP_RESPONSE_status := TOCSP_RESPONSE_status(@DummyOCSPResponseStatus);
  OCSP_RESPONSE_get1_basic := TOCSP_RESPONSE_get1_basic(@DummyOCSPResponseGet1Basic);
  OCSP_BASICRESP_verify := TOCSP_BASICRESP_verify(@DummyOCSPBasicRespVerify);
  OCSP_cert_to_id := TOCSP_cert_to_id(@DummyOCSPCertToID);
  OCSP_resp_find_status := TOCSP_resp_find_status(@DummyOCSPRespFindStatus);
  OCSP_check_validity := TOCSP_check_validity(@DummyOCSPCheckValidity);
  OCSP_REQUEST_new := TOCSP_REQUEST_new(@DummyOCSPRequestNew);
  OCSP_REQUEST_free := TOCSP_REQUEST_free(@DummyOCSPRequestFree);
  OCSP_RESPONSE_free := TOCSP_RESPONSE_free(@DummyOCSPResponseFree);
  OCSP_BASICRESP_free := TOCSP_BASICRESP_free(@DummyOCSPBasicRespFree);
  OCSP_CERTID_free := TOCSP_CERTID_free(@DummyOCSPCertIDFree);
end;

procedure RunCheckCertificateStatusDependencyContractScenario;
var
  LSavedResponseStatus: TOCSP_RESPONSE_status;
  LSavedResponseGet1Basic: TOCSP_RESPONSE_get1_basic;
  LSavedBasicRespVerify: TOCSP_BASICRESP_verify;
  LSavedCertToId: TOCSP_cert_to_id;
  LSavedRespFindStatus: TOCSP_resp_find_status;
  LSavedCheckValidity: TOCSP_check_validity;
  LSavedRequestNew: TOCSP_REQUEST_new;
  LSavedRequestFree: TOCSP_REQUEST_free;
  LSavedResponseFree: TOCSP_RESPONSE_free;
  LSavedBasicRespFree: TOCSP_BASICRESP_free;
  LSavedCertIDFree: TOCSP_CERTID_free;
begin
  LSavedResponseStatus := OCSP_RESPONSE_status;
  LSavedResponseGet1Basic := OCSP_RESPONSE_get1_basic;
  LSavedBasicRespVerify := OCSP_BASICRESP_verify;
  LSavedCertToId := OCSP_cert_to_id;
  LSavedRespFindStatus := OCSP_resp_find_status;
  LSavedCheckValidity := OCSP_check_validity;
  LSavedRequestNew := OCSP_REQUEST_new;
  LSavedRequestFree := OCSP_REQUEST_free;
  LSavedResponseFree := OCSP_RESPONSE_free;
  LSavedBasicRespFree := OCSP_BASICRESP_free;
  LSavedCertIDFree := OCSP_CERTID_free;
  try
    // Install deterministic non-nil baseline so each missing symbol assertion is isolated.
    InstallDeterministicDependencyBaseline;

    Check('checkcert-deps baseline available',
      CheckCertificateStatusDependenciesAvailable,
      'expected helper baseline to report dependencies available');

    OCSP_RESPONSE_status := nil;
    Check('checkcert-deps response_status missing',
      not CheckCertificateStatusDependenciesAvailable,
      'response_status missing must report unavailable');
    OCSP_RESPONSE_status := TOCSP_RESPONSE_status(@DummyOCSPResponseStatus);

    OCSP_RESPONSE_get1_basic := nil;
    Check('checkcert-deps response_get1_basic missing',
      not CheckCertificateStatusDependenciesAvailable,
      'response_get1_basic missing must report unavailable');
    OCSP_RESPONSE_get1_basic := TOCSP_RESPONSE_get1_basic(@DummyOCSPResponseGet1Basic);

    OCSP_BASICRESP_verify := nil;
    Check('checkcert-deps basicresp_verify missing',
      not CheckCertificateStatusDependenciesAvailable,
      'basicresp_verify missing must report unavailable');
    OCSP_BASICRESP_verify := TOCSP_BASICRESP_verify(@DummyOCSPBasicRespVerify);

    OCSP_cert_to_id := nil;
    Check('checkcert-deps cert_to_id missing',
      not CheckCertificateStatusDependenciesAvailable,
      'cert_to_id missing must report unavailable');
    OCSP_cert_to_id := TOCSP_cert_to_id(@DummyOCSPCertToID);

    OCSP_resp_find_status := nil;
    Check('checkcert-deps resp_find_status missing',
      not CheckCertificateStatusDependenciesAvailable,
      'resp_find_status missing must report unavailable');
    OCSP_resp_find_status := TOCSP_resp_find_status(@DummyOCSPRespFindStatus);

    OCSP_check_validity := nil;
    Check('checkcert-deps check_validity missing',
      not CheckCertificateStatusDependenciesAvailable,
      'check_validity missing must report unavailable');
    OCSP_check_validity := TOCSP_check_validity(@DummyOCSPCheckValidity);

    OCSP_REQUEST_new := nil;
    Check('checkcert-deps request_new missing',
      not CheckCertificateStatusDependenciesAvailable,
      'request_new missing must report unavailable');
    OCSP_REQUEST_new := TOCSP_REQUEST_new(@DummyOCSPRequestNew);

    OCSP_REQUEST_free := nil;
    Check('checkcert-deps request_free missing',
      not CheckCertificateStatusDependenciesAvailable,
      'request_free missing must report unavailable');
    OCSP_REQUEST_free := TOCSP_REQUEST_free(@DummyOCSPRequestFree);

    OCSP_RESPONSE_free := nil;
    Check('checkcert-deps response_free missing',
      not CheckCertificateStatusDependenciesAvailable,
      'response_free missing must report unavailable');
    OCSP_RESPONSE_free := TOCSP_RESPONSE_free(@DummyOCSPResponseFree);

    OCSP_BASICRESP_free := nil;
    Check('checkcert-deps basicresp_free missing',
      not CheckCertificateStatusDependenciesAvailable,
      'basicresp_free missing must report unavailable');
    OCSP_BASICRESP_free := TOCSP_BASICRESP_free(@DummyOCSPBasicRespFree);

    OCSP_CERTID_free := nil;
    Check('checkcert-deps certid_free missing',
      not CheckCertificateStatusDependenciesAvailable,
      'certid_free missing must report unavailable');
    OCSP_CERTID_free := TOCSP_CERTID_free(@DummyOCSPCertIDFree);
  finally
    OCSP_CERTID_free := LSavedCertIDFree;
    OCSP_BASICRESP_free := LSavedBasicRespFree;
    OCSP_RESPONSE_free := LSavedResponseFree;
    OCSP_REQUEST_free := LSavedRequestFree;
    OCSP_REQUEST_new := LSavedRequestNew;
    OCSP_check_validity := LSavedCheckValidity;
    OCSP_resp_find_status := LSavedRespFindStatus;
    OCSP_cert_to_id := LSavedCertToId;
    OCSP_BASICRESP_verify := LSavedBasicRespVerify;
    OCSP_RESPONSE_get1_basic := LSavedResponseGet1Basic;
    OCSP_RESPONSE_status := LSavedResponseStatus;
  end;
end;

procedure RunVerifyOCSPResponseDependencyPreflightScenario;
var
  LLoaded: Boolean;
  LSavedResponseStatus: TOCSP_RESPONSE_status;
  LSavedResponseGet1Basic: TOCSP_RESPONSE_get1_basic;
  LSavedBasicRespVerify: TOCSP_BASICRESP_verify;
  LSavedBasicRespFree: TOCSP_BASICRESP_free;
  LResult: Boolean;
begin
  LLoaded := LoadOpenSSLOCSP(GetCryptoLibHandle);
  Check('verifyocsp-preflight load ocsp module', LLoaded,
    'failed to load OCSP module');
  if not LLoaded then
    Exit;

  LSavedResponseStatus := OCSP_RESPONSE_status;
  LSavedResponseGet1Basic := OCSP_RESPONSE_get1_basic;
  LSavedBasicRespVerify := OCSP_BASICRESP_verify;
  LSavedBasicRespFree := OCSP_BASICRESP_free;
  try
    InstallDeterministicDependencyBaseline;
    OCSP_RESPONSE_status := TOCSP_RESPONSE_status(@CountingOCSPResponseStatus);
    OCSP_RESPONSE_get1_basic := TOCSP_RESPONSE_get1_basic(@DummyOCSPResponseGet1Basic);
    OCSP_BASICRESP_verify := TOCSP_BASICRESP_verify(@DummyOCSPBasicRespVerify);
    OCSP_BASICRESP_free := nil;

    GVerifyResponseStatusCalls := 0;
    LResult := VerifyOCSPResponse(POCSP_RESPONSE(PtrUInt(1)), nil, nil, nil, nil);

    Check('verifyocsp-preflight missing-basicresp_free returns false',
      not LResult,
      'expected VerifyOCSPResponse to fail when basicresp_free is missing');
    Check('verifyocsp-preflight missing-basicresp_free blocks response_status call',
      GVerifyResponseStatusCalls = 0,
      Format('expected response_status calls=0 got=%d', [GVerifyResponseStatusCalls]));
  finally
    OCSP_BASICRESP_free := LSavedBasicRespFree;
    OCSP_BASICRESP_verify := LSavedBasicRespVerify;
    OCSP_RESPONSE_get1_basic := LSavedResponseGet1Basic;
    OCSP_RESPONSE_status := LSavedResponseStatus;
  end;
end;

type
  TNilX509Certificate = class(TInterfacedObject, fafafa.ssl.openssl.cert.builder.ICertificateEx)
  public
    function GetSubject: string;
    function GetIssuer: string;
    function GetSerialNumber: string;
    function GetNotBefore: TDateTime;
    function GetNotAfter: TDateTime;
    function GetSubjectAltNames: TStringArray;
    function IsCA: Boolean;
    function IsValidAt(ATime: TDateTime): Boolean;
    function IsExpired: Boolean;
    function ToPEM: string;
    function ToDER: TBytes;
    procedure SaveToFile(const AFile: string);
    function GetX509Handle: Pointer;
  end;

function TNilX509Certificate.GetSubject: string;
begin
  Result := '';
end;

function TNilX509Certificate.GetIssuer: string;
begin
  Result := '';
end;

function TNilX509Certificate.GetSerialNumber: string;
begin
  Result := '';
end;

function TNilX509Certificate.GetNotBefore: TDateTime;
begin
  Result := 0;
end;

function TNilX509Certificate.GetNotAfter: TDateTime;
begin
  Result := 0;
end;

function TNilX509Certificate.GetSubjectAltNames: TStringArray;
begin
  SetLength(Result, 0);
end;

function TNilX509Certificate.IsCA: Boolean;
begin
  Result := False;
end;

function TNilX509Certificate.IsValidAt(ATime: TDateTime): Boolean;
begin
  Result := False;
end;

function TNilX509Certificate.IsExpired: Boolean;
begin
  Result := True;
end;

function TNilX509Certificate.ToPEM: string;
begin
  Result := '';
end;

function TNilX509Certificate.ToDER: TBytes;
begin
  SetLength(Result, 0);
end;

procedure TNilX509Certificate.SaveToFile(const AFile: string);
begin
  // No-op test double.
end;

function TNilX509Certificate.GetX509Handle: Pointer;
begin
  Result := nil;
end;

procedure RunStatusScenario(const AName: string; AForcedStatus: Integer;
  AExpectedStatus: TOCSPStatus; AExpectErrorMessage: Boolean);
var
  LOCSP: IOCSPClient;
  LCert, LIssuer: ICertificate;
  LResp: TOCSPResponse;
begin
  LOCSP := CreateOCSPClient;
  Check(AName + ' create ocsp client', LOCSP <> nil, 'CreateOCSPClient returned nil');
  if LOCSP = nil then
    Exit;

  LOCSP.SetResponderURL('http://ocsp.example.test');
  LOCSP.SetTimeout(3);

  GForcedStatus := AForcedStatus;
  LCert := TCertificate.CreateSelfSigned('leaf-' + AName + '.example').Certificate;
  LIssuer := TCertificate.CreateSelfSigned('issuer-' + AName + '.example').Certificate;

  LResp := LOCSP.CheckCertificate(LCert, LIssuer);

  Check(AName + ' status mapping', LResp.Status = AExpectedStatus,
    Format('expected=%d got=%d', [Ord(AExpectedStatus), Ord(LResp.Status)]));

  if AExpectedStatus = ocspRevoked then
    Check(AName + ' revokedAt unknown semantics', LResp.RevokedAt = 0,
      Format('expected 0 got %s', [FloatToStr(LResp.RevokedAt)]));

  if AExpectErrorMessage then
    Check(AName + ' error message', LResp.ErrorMessage <> '', 'expected non-empty error message')
  else
    Check(AName + ' no error message', LResp.ErrorMessage = '',
      Format('expected empty error message, got "%s"', [LResp.ErrorMessage]));
end;

procedure RunNilHandleFailClosedScenario;
var
  LOCSP: IOCSPClient;
  LNilLeaf: fafafa.ssl.openssl.cert.builder.ICertificateEx;
  LIssuer: ICertificate;
  LRaised: Boolean;
begin
  LOCSP := CreateOCSPClient;
  Check('nil-handle create ocsp client', LOCSP <> nil, 'CreateOCSPClient returned nil');
  if LOCSP = nil then
    Exit;

  LOCSP.SetResponderURL('http://ocsp.example.test');
  LOCSP.SetTimeout(3);

  LNilLeaf := TNilX509Certificate.Create;
  LIssuer := TCertificate.CreateSelfSigned('issuer-nil-handle.example').Certificate;

  LRaised := False;
  try
    LOCSP.CheckCertificate(ICertificate(LNilLeaf), LIssuer);
  except
    on E: Exception do
    begin
      LRaised := True;
      Check('nil-handle ocsp returns controlled certificate-access error',
        Pos('Certificate handle access', E.Message) > 0,
        'unexpected message: ' + E.Message);
    end;
  end;
  Check('nil-handle ocsp must fail closed', LRaised,
    'expected controlled exception for nil X509 handle');
end;

procedure RunNilIssuerFailClosedScenario;
var
  LOCSP: IOCSPClient;
  LLeaf: ICertificate;
  LRaised: Boolean;
begin
  LOCSP := CreateOCSPClient;
  Check('nil-issuer create ocsp client', LOCSP <> nil, 'CreateOCSPClient returned nil');
  if LOCSP = nil then
    Exit;

  LOCSP.SetResponderURL('http://ocsp.example.test');
  LOCSP.SetTimeout(3);

  LLeaf := TCertificate.CreateSelfSigned('leaf-nil-issuer.example').Certificate;

  LRaised := False;
  try
    LOCSP.CheckCertificate(LLeaf, nil);
  except
    on E: Exception do
    begin
      LRaised := True;
      Check('nil-issuer ocsp returns controlled issuer-access error',
        Pos('Issuer certificate handle access', E.Message) > 0,
        'unexpected message: ' + E.Message);
    end;
  end;
  Check('nil-issuer ocsp must fail closed', LRaised,
    'expected controlled exception for nil issuer');
end;

procedure RunNilIssuerHandleFailClosedScenario;
var
  LOCSP: IOCSPClient;
  LLeaf: ICertificate;
  LNilIssuer: fafafa.ssl.openssl.cert.builder.ICertificateEx;
  LRaised: Boolean;
begin
  LOCSP := CreateOCSPClient;
  Check('nil-issuer-handle create ocsp client', LOCSP <> nil, 'CreateOCSPClient returned nil');
  if LOCSP = nil then
    Exit;

  LOCSP.SetResponderURL('http://ocsp.example.test');
  LOCSP.SetTimeout(3);

  LLeaf := TCertificate.CreateSelfSigned('leaf-nil-issuer-handle.example').Certificate;
  LNilIssuer := TNilX509Certificate.Create;

  LRaised := False;
  try
    LOCSP.CheckCertificate(LLeaf, ICertificate(LNilIssuer));
  except
    on E: Exception do
    begin
      LRaised := True;
      Check('nil-issuer-handle ocsp returns controlled issuer-access error',
        Pos('Issuer certificate handle access', E.Message) > 0,
        'unexpected message: ' + E.Message);
    end;
  end;
  Check('nil-issuer-handle ocsp must fail closed', LRaised,
    'expected controlled exception for nil issuer X509 handle');
end;

procedure RunMissingCheckCertificateStatusAPIFailClosedScenario;
var
  LOCSP: IOCSPClient;
  LLeaf, LIssuer: ICertificate;
  LRaised: Boolean;
  LLoaded: Boolean;
  LSavedResolver: TOCSPStatusResolver;
  LSavedResponseStatus: TOCSP_RESPONSE_status;
  LSavedResponseGet1Basic: TOCSP_RESPONSE_get1_basic;
  LSavedBasicRespVerify: TOCSP_BASICRESP_verify;
  LSavedCertToId: TOCSP_cert_to_id;
  LSavedRespFindStatus: TOCSP_resp_find_status;
  LSavedCheckValidity: TOCSP_check_validity;
  LSavedRequestNew: TOCSP_REQUEST_new;
  LSavedRequestFree: TOCSP_REQUEST_free;
  LSavedResponseFree: TOCSP_RESPONSE_free;
  LSavedBasicRespFree: TOCSP_BASICRESP_free;
  LSavedCertIDFree: TOCSP_CERTID_free;

  procedure ExpectFailClosedForMissingAPI(const ACaseName: string);
  begin
    LRaised := False;
    try
      LOCSP.CheckCertificate(LLeaf, LIssuer);
    except
      on E: Exception do
      begin
        LRaised := True;
        Check(ACaseName + ' returns controlled unsupported error',
          Pos('OpenSSL API CheckCertificateStatus', E.Message) > 0,
          'unexpected message: ' + E.Message);
      end;
    end;
    Check(ACaseName + ' must fail closed', LRaised,
      'expected controlled unsupported exception when OCSP status API is missing');
  end;
begin
  LOCSP := CreateOCSPClient;
  Check('missing-checkcertificatestatus-api create ocsp client', LOCSP <> nil, 'CreateOCSPClient returned nil');
  if LOCSP = nil then
    Exit;

  LOCSP.SetResponderURL('http://ocsp.example.test');
  LOCSP.SetTimeout(3);

  LLeaf := TCertificate.CreateSelfSigned('leaf-missing-checkcertstatus-api.example').Certificate;
  LIssuer := TCertificate.CreateSelfSigned('issuer-missing-checkcertstatus-api.example').Certificate;

  LLoaded := LoadOpenSSLOCSP(GetCryptoLibHandle);
  Check('missing-checkcertificatestatus-api load ocsp module', LLoaded,
    'failed to load OCSP module');
  if not LLoaded then
    Exit;

  LSavedResolver := OCSPStatusResolverHook;
  LSavedResponseStatus := OCSP_RESPONSE_status;
  LSavedResponseGet1Basic := OCSP_RESPONSE_get1_basic;
  LSavedBasicRespVerify := OCSP_BASICRESP_verify;
  LSavedCertToId := OCSP_cert_to_id;
  LSavedRespFindStatus := OCSP_resp_find_status;
  LSavedCheckValidity := OCSP_check_validity;
  LSavedRequestNew := OCSP_REQUEST_new;
  LSavedRequestFree := OCSP_REQUEST_free;
  LSavedResponseFree := OCSP_RESPONSE_free;
  LSavedBasicRespFree := OCSP_BASICRESP_free;
  LSavedCertIDFree := OCSP_CERTID_free;
  OCSPStatusResolverHook := nil;
  try
    InstallDeterministicDependencyBaseline;
    Check('missing-checkcertificatestatus-api deterministic baseline available',
      CheckCertificateStatusDependenciesAvailable,
      'expected all CheckCertificateStatus dependencies assigned before missing-symbol subcases');

    OCSP_RESPONSE_status := nil;
    ExpectFailClosedForMissingAPI('missing-checkcertificatestatus-api response_status');
    OCSP_RESPONSE_status := TOCSP_RESPONSE_status(@DummyOCSPResponseStatus);

    OCSP_RESPONSE_get1_basic := nil;
    ExpectFailClosedForMissingAPI('missing-checkcertificatestatus-api response_get1_basic');
    OCSP_RESPONSE_get1_basic := TOCSP_RESPONSE_get1_basic(@DummyOCSPResponseGet1Basic);

    OCSP_BASICRESP_verify := nil;
    ExpectFailClosedForMissingAPI('missing-checkcertificatestatus-api basicresp_verify');
    OCSP_BASICRESP_verify := TOCSP_BASICRESP_verify(@DummyOCSPBasicRespVerify);

    OCSP_cert_to_id := nil;
    ExpectFailClosedForMissingAPI('missing-checkcertificatestatus-api cert_to_id');
    OCSP_cert_to_id := TOCSP_cert_to_id(@DummyOCSPCertToID);

    OCSP_resp_find_status := nil;
    ExpectFailClosedForMissingAPI('missing-checkcertificatestatus-api resp_find_status');
    OCSP_resp_find_status := TOCSP_resp_find_status(@DummyOCSPRespFindStatus);

    OCSP_check_validity := nil;
    ExpectFailClosedForMissingAPI('missing-checkcertificatestatus-api check_validity');
    OCSP_check_validity := TOCSP_check_validity(@DummyOCSPCheckValidity);

    OCSP_REQUEST_new := nil;
    ExpectFailClosedForMissingAPI('missing-checkcertificatestatus-api request_new');
    OCSP_REQUEST_new := TOCSP_REQUEST_new(@DummyOCSPRequestNew);

    OCSP_REQUEST_free := nil;
    ExpectFailClosedForMissingAPI('missing-checkcertificatestatus-api request_free');
    OCSP_REQUEST_free := TOCSP_REQUEST_free(@DummyOCSPRequestFree);

    OCSP_RESPONSE_free := nil;
    ExpectFailClosedForMissingAPI('missing-checkcertificatestatus-api response_free');
    OCSP_RESPONSE_free := TOCSP_RESPONSE_free(@DummyOCSPResponseFree);

    OCSP_BASICRESP_free := nil;
    ExpectFailClosedForMissingAPI('missing-checkcertificatestatus-api basicresp_free');
    OCSP_BASICRESP_free := TOCSP_BASICRESP_free(@DummyOCSPBasicRespFree);

    OCSP_CERTID_free := nil;
    ExpectFailClosedForMissingAPI('missing-checkcertificatestatus-api certid_free');
    OCSP_CERTID_free := TOCSP_CERTID_free(@DummyOCSPCertIDFree);
  finally
    OCSP_CERTID_free := LSavedCertIDFree;
    OCSP_BASICRESP_free := LSavedBasicRespFree;
    OCSP_RESPONSE_free := LSavedResponseFree;
    OCSP_REQUEST_free := LSavedRequestFree;
    OCSP_REQUEST_new := LSavedRequestNew;
    OCSP_check_validity := LSavedCheckValidity;
    OCSP_resp_find_status := LSavedRespFindStatus;
    OCSP_cert_to_id := LSavedCertToId;
    OCSP_BASICRESP_verify := LSavedBasicRespVerify;
    OCSP_RESPONSE_get1_basic := LSavedResponseGet1Basic;
    OCSP_RESPONSE_status := LSavedResponseStatus;
    OCSPStatusResolverHook := LSavedResolver;
  end;
end;

procedure RunOCSPModuleLoadedStateBoundaryScenario;
var
  LOCSP: IOCSPClient;
  LLeaf, LIssuer: ICertificate;
  LRaised: Boolean;
  LLoaded: Boolean;
  LSavedResolver: TOCSPStatusResolver;
  LSavedResponseStatus: TOCSP_RESPONSE_status;
  LSavedResponseGet1Basic: TOCSP_RESPONSE_get1_basic;
  LSavedBasicRespVerify: TOCSP_BASICRESP_verify;
  LSavedCertToId: TOCSP_cert_to_id;
  LSavedRespFindStatus: TOCSP_resp_find_status;
  LSavedCheckValidity: TOCSP_check_validity;
  LSavedRequestNew: TOCSP_REQUEST_new;
  LSavedRequestFree: TOCSP_REQUEST_free;
  LSavedResponseFree: TOCSP_RESPONSE_free;
  LSavedBasicRespFree: TOCSP_BASICRESP_free;
  LSavedCertIDFree: TOCSP_CERTID_free;
begin
  LOCSP := CreateOCSPClient;
  Check('ocsp-module-state create ocsp client', LOCSP <> nil, 'CreateOCSPClient returned nil');
  if LOCSP = nil then
    Exit;

  LOCSP.SetResponderURL('http://ocsp.example.test');
  LOCSP.SetTimeout(3);

  LLeaf := TCertificate.CreateSelfSigned('leaf-ocsp-module-state.example').Certificate;
  LIssuer := TCertificate.CreateSelfSigned('issuer-ocsp-module-state.example').Certificate;

  LLoaded := LoadOpenSSLOCSP(GetCryptoLibHandle);
  Check('ocsp-module-state load ocsp module', LLoaded, 'failed to load OCSP module');
  if not LLoaded then
    Exit;

  Check('ocsp-module-state module loaded baseline',
    TOpenSSLLoader.IsModuleLoaded(osmOCSP),
    'expected OCSP module loaded flag to be true after load');

  LSavedResolver := OCSPStatusResolverHook;
  LSavedResponseStatus := OCSP_RESPONSE_status;
  LSavedResponseGet1Basic := OCSP_RESPONSE_get1_basic;
  LSavedBasicRespVerify := OCSP_BASICRESP_verify;
  LSavedCertToId := OCSP_cert_to_id;
  LSavedRespFindStatus := OCSP_resp_find_status;
  LSavedCheckValidity := OCSP_check_validity;
  LSavedRequestNew := OCSP_REQUEST_new;
  LSavedRequestFree := OCSP_REQUEST_free;
  LSavedResponseFree := OCSP_RESPONSE_free;
  LSavedBasicRespFree := OCSP_BASICRESP_free;
  LSavedCertIDFree := OCSP_CERTID_free;
  OCSPStatusResolverHook := nil;
  try
    InstallDeterministicDependencyBaseline;
    Check('ocsp-module-state deps available deterministic baseline',
      CheckCertificateStatusDependenciesAvailable,
      'expected dependencies available after deterministic baseline install');

    OCSP_check_validity := nil;
    Check('ocsp-module-state module still loaded when dep missing',
      TOpenSSLLoader.IsModuleLoaded(osmOCSP),
      'module loaded flag should remain true even when one helper dependency is missing');
    Check('ocsp-module-state deps unavailable when dep missing',
      not CheckCertificateStatusDependenciesAvailable,
      'helper dependency availability should be false when one dependency is nil');

    LRaised := False;
    try
      LOCSP.CheckCertificate(LLeaf, LIssuer);
    except
      on E: Exception do
      begin
        LRaised := True;
        Check('ocsp-module-state missing-dep returns controlled unsupported',
          Pos('OpenSSL API CheckCertificateStatus', E.Message) > 0,
          'unexpected message: ' + E.Message);
      end;
    end;
    Check('ocsp-module-state missing-dep must fail closed', LRaised,
      'expected controlled unsupported exception for loaded-module missing dependency');
  finally
    OCSP_CERTID_free := LSavedCertIDFree;
    OCSP_BASICRESP_free := LSavedBasicRespFree;
    OCSP_RESPONSE_free := LSavedResponseFree;
    OCSP_REQUEST_free := LSavedRequestFree;
    OCSP_REQUEST_new := LSavedRequestNew;
    OCSP_check_validity := LSavedCheckValidity;
    OCSP_resp_find_status := LSavedRespFindStatus;
    OCSP_cert_to_id := LSavedCertToId;
    OCSP_BASICRESP_verify := LSavedBasicRespVerify;
    OCSP_RESPONSE_get1_basic := LSavedResponseGet1Basic;
    OCSP_RESPONSE_status := LSavedResponseStatus;
    OCSPStatusResolverHook := LSavedResolver;
  end;
end;

procedure RunOCSPModuleUnloadedSemanticsScenario;
var
  LOCSP: IOCSPClient;
  LLeaf, LIssuer: ICertificate;
  LResp: TOCSPResponse;
  LRaised: Boolean;
  LLoaded: Boolean;
  LReloaded: Boolean;
  LSavedResolver: TOCSPStatusResolver;
begin
  LOCSP := CreateOCSPClient;
  Check('ocsp-module-unloaded create ocsp client', LOCSP <> nil, 'CreateOCSPClient returned nil');
  if LOCSP = nil then
    Exit;

  LOCSP.SetResponderURL('http://ocsp.example.test');
  LOCSP.SetTimeout(3);

  LLeaf := TCertificate.CreateSelfSigned('leaf-ocsp-module-unloaded.example').Certificate;
  LIssuer := TCertificate.CreateSelfSigned('issuer-ocsp-module-unloaded.example').Certificate;

  LLoaded := LoadOpenSSLOCSP(GetCryptoLibHandle);
  Check('ocsp-module-unloaded preload ocsp module', LLoaded, 'failed to load OCSP module');
  if not LLoaded then
    Exit;

  LSavedResolver := OCSPStatusResolverHook;
  OCSPStatusResolverHook := nil;
  UnloadOpenSSLOCSP;
  Check('ocsp-module-unloaded module is unloaded',
    not TOpenSSLLoader.IsModuleLoaded(osmOCSP),
    'expected OCSP module to be unloaded');
  try
    LRaised := False;
    try
      LResp := LOCSP.CheckCertificate(LLeaf, LIssuer);
    except
      on E: Exception do
      begin
        LRaised := True;
        Fail('ocsp-module-unloaded no exception contract', E.Message);
      end;
    end;

    Check('ocsp-module-unloaded must not raise', not LRaised,
      'expected ocspError result when module is unloaded');
    Check('ocsp-module-unloaded returns ocspError', LResp.Status = ocspError,
      Format('expected=%d got=%d', [Ord(ocspError), Ord(LResp.Status)]));
    Check('ocsp-module-unloaded error semantic',
      Pos('OCSP check failed with status:', LResp.ErrorMessage) > 0,
      'unexpected message: ' + LResp.ErrorMessage);
  finally
    LReloaded := LoadOpenSSLOCSP(GetCryptoLibHandle);
    Check('ocsp-module-unloaded module reload', LReloaded, 'failed to reload OCSP module');
    OCSPStatusResolverHook := LSavedResolver;
  end;
end;

var
  LLib: ISSLLibrary;
  LSavedResolver: TOCSPStatusResolver;
begin
  WriteLn('====================================');
  WriteLn('  OCSP Client Semantics Test');
  WriteLn('====================================');
  WriteLn;

  try
    LLib := TSSLFactory.GetLibraryInstance(sslOpenSSL);
    if (LLib = nil) or (not LLib.Initialize) then
    begin
      Fail('openssl initialize', 'failed to initialize openssl backend');
      Halt(1);
    end;

    LSavedResolver := OCSPStatusResolverHook;
    OCSPStatusResolverHook := @ForcedOCSPStatus;
    try
      RunCheckCertificateStatusDependencyContractScenario;
      RunVerifyOCSPResponseDependencyPreflightScenario;
      RunStatusScenario('good', V_OCSP_CERTSTATUS_GOOD, ocspGood, False);
      RunStatusScenario('revoked', V_OCSP_CERTSTATUS_REVOKED, ocspRevoked, False);
      RunStatusScenario('unknown', V_OCSP_CERTSTATUS_UNKNOWN, ocspUnknown, False);
      RunStatusScenario('error', V_OCSP_CERTSTATUS_ERROR, ocspError, True);

      OCSPStatusResolverHook := @StrictHandleOCSPStatus;
      RunNilHandleFailClosedScenario;
      RunNilIssuerFailClosedScenario;
      RunNilIssuerHandleFailClosedScenario;
      RunMissingCheckCertificateStatusAPIFailClosedScenario;
      RunOCSPModuleLoadedStateBoundaryScenario;
      RunOCSPModuleUnloadedSemanticsScenario;
    finally
      OCSPStatusResolverHook := LSavedResolver;
    end;
  except
    on E: Exception do
      Fail('unexpected exception', E.Message);
  end;

  WriteLn;
  WriteLn('====================================');
  WriteLn(Format('Results: %d passed, %d failed', [TestsPassed, TestsFailed]));
  WriteLn('====================================');

  if TestsFailed > 0 then
    Halt(1);
end.
