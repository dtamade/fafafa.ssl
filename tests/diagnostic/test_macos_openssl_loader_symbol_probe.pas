program test_macos_openssl_loader_symbol_probe;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  Classes,
  Dynlibs,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.pem,
  fafafa.ssl.openssl.api.pkcs12,
  fafafa.ssl.openssl.api.cms,
  fafafa.ssl.openssl.api.ocsp,
  fafafa.ssl.openssl.api.ts,
  fafafa.ssl.openssl.api.ct,
  fafafa.ssl.openssl.api.store;

function JsonEscape(const S: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    case S[I] of
      '"': Result := Result + '\"';
      '\': Result := Result + '\\';
      '/': Result := Result + '\/';
      #8: Result := Result + '\b';
      #9: Result := Result + '\t';
      #10: Result := Result + '\n';
      #12: Result := Result + '\f';
      #13: Result := Result + '\r';
    else
      if Ord(S[I]) < 32 then
        Result := Result + '\u' + IntToHex(Ord(S[I]), 4)
      else
        Result := Result + S[I];
    end;
  end;
end;

function JsonBool(AValue: Boolean): string;
begin
  if AValue then
    Result := 'true'
  else
    Result := 'false';
end;

function HandleToHex(AHandle: TLibHandle): string;
begin
  Result := '0x' + IntToHex(PtrUInt(AHandle), SizeOf(PtrUInt) * 2);
end;

function DirectSymbolAvailable(AHandle: TLibHandle; const ASymbol: string): Boolean;
begin
  Result := (AHandle <> NilHandle) and Assigned(TOpenSSLLoader.GetFunction(AHandle, ASymbol));
end;

procedure AppendJSONPair(ALines: TStrings; const AIndent, AKey, AValue: string;
  AComma: Boolean = True);
begin
  if AComma then
    ALines.Add(AIndent + '"' + AKey + '": ' + AValue + ',')
  else
    ALines.Add(AIndent + '"' + AKey + '": ' + AValue);
end;

procedure SaveOutput(const APath, AContent: string);
var
  LOutput: TStringList;
begin
  if APath = '' then
  begin
    Write(AContent);
    Exit;
  end;

  ForceDirectories(ExtractFileDir(APath));
  LOutput := TStringList.Create;
  try
    LOutput.Text := AContent;
    LOutput.SaveToFile(APath);
  finally
    LOutput.Free;
  end;
end;

var
  LLines: TStringList;
  LOutputPath: string;
  LStatus: string;
  LCryptoHandle: TLibHandle;
  LSSLHandle: TLibHandle;
  LVersionInfo: TOpenSSLVersionInfo;
  LCoreLoaded: Boolean;
  LEVPLoaded: Boolean;
  LPEMLoaded: Boolean;
  LCMSLoaded: Boolean;
  LOCSPLoaded: Boolean;
  LEVPLoadedCount: Integer;
  LPEMLoadedCount: Integer;
  LPKCS12LoadedCount: Integer;
  LCMSLoadedCount: Integer;
  LOCSPLoadedCount: Integer;
  LEVPMissingRequired: string;
  LPEMMissingRequired: string;
  LPKCS12MissingRequired: string;
  LCMSMissingRequired: string;
  LOCSPMissingRequired: string;
begin
  LOutputPath := '';
  if ParamCount >= 1 then
    LOutputPath := ExpandFileName(ParamStr(1));

  LCryptoHandle := TOpenSSLLoader.GetLibraryHandle(osslLibCrypto);
  LSSLHandle := TOpenSSLLoader.GetLibraryHandle(osslLibSSL);
  if LCryptoHandle <> NilHandle then
    LStatus := 'ok'
  else
    LStatus := 'error';
  LVersionInfo := TOpenSSLLoader.GetVersionInfo;

  LCoreLoaded := False;
  LEVPLoaded := False;
  LPEMLoaded := False;
  LCMSLoaded := False;
  LOCSPLoaded := False;
  LEVPLoadedCount := 0;
  LPEMLoadedCount := 0;
  LPKCS12LoadedCount := 0;
  LCMSLoadedCount := 0;
  LOCSPLoadedCount := 0;
  LEVPMissingRequired := '';
  LPEMMissingRequired := '';
  LPKCS12MissingRequired := '';
  LCMSMissingRequired := '';
  LOCSPMissingRequired := '';

  try
    LoadOpenSSLCore;
    LCoreLoaded := TOpenSSLLoader.IsModuleLoaded(osmCore);

    if LCoreLoaded then
    begin
      LoadOpenSSLBIO;
      LoadOpenSSLX509;
      LEVPLoaded := LoadEVP(LCryptoHandle);
      LEVPLoadedCount := TOpenSSLLoader.GetLastLoadFunctionsLoadedCount;
      LEVPMissingRequired := TOpenSSLLoader.GetLastLoadFunctionsMissingRequired;
      LPEMLoaded := LoadOpenSSLPEM(LCryptoHandle);
      LPEMLoadedCount := TOpenSSLLoader.GetLastLoadFunctionsLoadedCount;
      LPEMMissingRequired := TOpenSSLLoader.GetLastLoadFunctionsMissingRequired;
      LoadPKCS12Module(LCryptoHandle);
      LPKCS12LoadedCount := TOpenSSLLoader.GetLastLoadFunctionsLoadedCount;
      LPKCS12MissingRequired := TOpenSSLLoader.GetLastLoadFunctionsMissingRequired;
      LCMSLoaded := LoadOpenSSLCMS(LCryptoHandle);
      LCMSLoadedCount := TOpenSSLLoader.GetLastLoadFunctionsLoadedCount;
      LCMSMissingRequired := TOpenSSLLoader.GetLastLoadFunctionsMissingRequired;
      LOCSPLoaded := LoadOpenSSLOCSP(LCryptoHandle);
      LOCSPLoadedCount := TOpenSSLLoader.GetLastLoadFunctionsLoadedCount;
      LOCSPMissingRequired := TOpenSSLLoader.GetLastLoadFunctionsMissingRequired;
      LoadTSFunctions;
      LoadCTFunctions;
      LoadSTOREFunctions;
    end;

      LLines := TStringList.Create;
    try
      LLines.Add('{');
      AppendJSONPair(LLines, '  ', 'status', '"' + LStatus + '"');
      AppendJSONPair(LLines, '  ', 'crypto_handle_loaded', JsonBool(LCryptoHandle <> NilHandle));
      AppendJSONPair(LLines, '  ', 'ssl_handle_loaded', JsonBool(LSSLHandle <> NilHandle));
      AppendJSONPair(LLines, '  ', 'crypto_handle', '"' + HandleToHex(LCryptoHandle) + '"');
      AppendJSONPair(LLines, '  ', 'ssl_handle', '"' + HandleToHex(LSSLHandle) + '"');
      AppendJSONPair(LLines, '  ', 'loader_version_string',
        '"' + JsonEscape(LVersionInfo.VersionString) + '"');
      AppendJSONPair(LLines, '  ', 'loader_is_openssl3', JsonBool(LVersionInfo.IsOpenSSL3));
      AppendJSONPair(LLines, '  ', 'api_version_string',
        '"' + JsonEscape(GetOpenSSLVersionString) + '"');

      LLines.Add('  "direct_symbols": {');
      AppendJSONPair(LLines, '    ', 'OpenSSL_version_num',
        JsonBool(DirectSymbolAvailable(LCryptoHandle, 'OpenSSL_version_num')));
      AppendJSONPair(LLines, '    ', 'EVP_MD_CTX_new',
        JsonBool(DirectSymbolAvailable(LCryptoHandle, 'EVP_MD_CTX_new')));
      AppendJSONPair(LLines, '    ', 'EVP_CIPHER_CTX_new',
        JsonBool(DirectSymbolAvailable(LCryptoHandle, 'EVP_CIPHER_CTX_new')));
      AppendJSONPair(LLines, '    ', 'PEM_read_bio_X509',
        JsonBool(DirectSymbolAvailable(LCryptoHandle, 'PEM_read_bio_X509')));
      AppendJSONPair(LLines, '    ', 'PEM_write_bio_X509',
        JsonBool(DirectSymbolAvailable(LCryptoHandle, 'PEM_write_bio_X509')));
      AppendJSONPair(LLines, '    ', 'PKCS12_new',
        JsonBool(DirectSymbolAvailable(LCryptoHandle, 'PKCS12_new')));
      AppendJSONPair(LLines, '    ', 'PKCS12_parse',
        JsonBool(DirectSymbolAvailable(LCryptoHandle, 'PKCS12_parse')));
      AppendJSONPair(LLines, '    ', 'CMS_sign',
        JsonBool(DirectSymbolAvailable(LCryptoHandle, 'CMS_sign')));
      AppendJSONPair(LLines, '    ', 'CMS_verify',
        JsonBool(DirectSymbolAvailable(LCryptoHandle, 'CMS_verify')));
      AppendJSONPair(LLines, '    ', 'OCSP_REQUEST_new',
        JsonBool(DirectSymbolAvailable(LCryptoHandle, 'OCSP_REQUEST_new')));
      AppendJSONPair(LLines, '    ', 'OCSP_RESPONSE_new',
        JsonBool(DirectSymbolAvailable(LCryptoHandle, 'OCSP_RESPONSE_new')));
      AppendJSONPair(LLines, '    ', 'TS_REQ_new',
        JsonBool(DirectSymbolAvailable(LCryptoHandle, 'TS_REQ_new')));
      AppendJSONPair(LLines, '    ', 'CTLOG_STORE_new',
        JsonBool(DirectSymbolAvailable(LCryptoHandle, 'CTLOG_STORE_new')));
      AppendJSONPair(LLines, '    ', 'OSSL_STORE_open',
        JsonBool(DirectSymbolAvailable(LCryptoHandle, 'OSSL_STORE_open')), False);
      LLines.Add('  },');

      LLines.Add('  "module_results": {');
      LLines.Add('    "core": {');
      AppendJSONPair(LLines, '      ', 'module_loaded', JsonBool(LCoreLoaded));
      AppendJSONPair(LLines, '      ', 'get_crypto_procaddress_openssl_version_num',
        JsonBool(Assigned(GetCryptoProcAddress('OpenSSL_version_num'))), False);
      LLines.Add('    },');

      LLines.Add('    "evp": {');
      AppendJSONPair(LLines, '      ', 'load_result', JsonBool(LEVPLoaded));
      AppendJSONPair(LLines, '      ', 'module_loaded', JsonBool(TOpenSSLLoader.IsModuleLoaded(osmEVP)));
      AppendJSONPair(LLines, '      ', 'load_functions_loaded_count', IntToStr(LEVPLoadedCount));
      AppendJSONPair(LLines, '      ', 'missing_required_bindings',
        '"' + JsonEscape(LEVPMissingRequired) + '"');
      AppendJSONPair(LLines, '      ', 'evp_md_ctx_new_assigned', JsonBool(Assigned(EVP_MD_CTX_new)));
      AppendJSONPair(LLines, '      ', 'evp_cipher_ctx_new_assigned',
        JsonBool(Assigned(EVP_CIPHER_CTX_new)), False);
      LLines.Add('    },');

      LLines.Add('    "pem": {');
      AppendJSONPair(LLines, '      ', 'load_result', JsonBool(LPEMLoaded));
      AppendJSONPair(LLines, '      ', 'module_loaded', JsonBool(TOpenSSLLoader.IsModuleLoaded(osmPEM)));
      AppendJSONPair(LLines, '      ', 'load_functions_loaded_count', IntToStr(LPEMLoadedCount));
      AppendJSONPair(LLines, '      ', 'missing_required_bindings',
        '"' + JsonEscape(LPEMMissingRequired) + '"');
      AppendJSONPair(LLines, '      ', 'pem_read_bio_x509_assigned', JsonBool(Assigned(PEM_read_bio_X509)));
      AppendJSONPair(LLines, '      ', 'pem_write_bio_x509_assigned',
        JsonBool(Assigned(PEM_write_bio_X509)), False);
      LLines.Add('    },');

      LLines.Add('    "pkcs12": {');
      AppendJSONPair(LLines, '      ', 'module_loaded', JsonBool(TOpenSSLLoader.IsModuleLoaded(osmPKCS12)));
      AppendJSONPair(LLines, '      ', 'load_functions_loaded_count', IntToStr(LPKCS12LoadedCount));
      AppendJSONPair(LLines, '      ', 'missing_required_bindings',
        '"' + JsonEscape(LPKCS12MissingRequired) + '"');
      AppendJSONPair(LLines, '      ', 'pkcs12_new_assigned', JsonBool(Assigned(PKCS12_new)));
      AppendJSONPair(LLines, '      ', 'pkcs12_create_assigned', JsonBool(Assigned(PKCS12_create)));
      AppendJSONPair(LLines, '      ', 'pkcs12_parse_assigned', JsonBool(Assigned(PKCS12_parse)), False);
      LLines.Add('    },');

      LLines.Add('    "cms": {');
      AppendJSONPair(LLines, '      ', 'load_result', JsonBool(LCMSLoaded));
      AppendJSONPair(LLines, '      ', 'module_loaded', JsonBool(TOpenSSLLoader.IsModuleLoaded(osmCMS)));
      AppendJSONPair(LLines, '      ', 'load_functions_loaded_count', IntToStr(LCMSLoadedCount));
      AppendJSONPair(LLines, '      ', 'missing_required_bindings',
        '"' + JsonEscape(LCMSMissingRequired) + '"');
      AppendJSONPair(LLines, '      ', 'cms_sign_assigned', JsonBool(Assigned(CMS_sign)));
      AppendJSONPair(LLines, '      ', 'cms_verify_assigned', JsonBool(Assigned(CMS_verify)), False);
      LLines.Add('    },');

      LLines.Add('    "ocsp": {');
      AppendJSONPair(LLines, '      ', 'load_result', JsonBool(LOCSPLoaded));
      AppendJSONPair(LLines, '      ', 'module_loaded', JsonBool(TOpenSSLLoader.IsModuleLoaded(osmOCSP)));
      AppendJSONPair(LLines, '      ', 'load_functions_loaded_count', IntToStr(LOCSPLoadedCount));
      AppendJSONPair(LLines, '      ', 'missing_required_bindings',
        '"' + JsonEscape(LOCSPMissingRequired) + '"');
      AppendJSONPair(LLines, '      ', 'ocsp_request_new_assigned', JsonBool(Assigned(OCSP_REQUEST_new)));
      AppendJSONPair(LLines, '      ', 'ocsp_response_new_assigned',
        JsonBool(Assigned(OCSP_RESPONSE_new)), False);
      LLines.Add('    },');

      LLines.Add('    "ts": {');
      AppendJSONPair(LLines, '      ', 'ts_req_new_assigned', JsonBool(Assigned(TS_REQ_new)));
      AppendJSONPair(LLines, '      ', 'ts_resp_new_assigned', JsonBool(Assigned(TS_RESP_new)), False);
      LLines.Add('    },');

      LLines.Add('    "ct": {');
      AppendJSONPair(LLines, '      ', 'ctlog_store_new_assigned', JsonBool(Assigned(CTLOG_STORE_new)));
      AppendJSONPair(LLines, '      ', 'ssl_ctx_enable_ct_assigned',
        JsonBool(Assigned(SSL_CTX_enable_ct)), False);
      LLines.Add('    },');

      LLines.Add('    "store": {');
      AppendJSONPair(LLines, '      ', 'ossl_store_open_assigned', JsonBool(Assigned(OSSL_STORE_open)));
      AppendJSONPair(LLines, '      ', 'ossl_store_info_get0_cert_assigned',
        JsonBool(Assigned(OSSL_STORE_INFO_get0_CERT)), False);
      LLines.Add('    }');
      LLines.Add('  }');
      LLines.Add('}');

      SaveOutput(LOutputPath, LLines.Text);
    finally
      LLines.Free;
    end;
  except
    on E: Exception do
    begin
      SaveOutput(LOutputPath,
        '{' + LineEnding +
        '  "status": "fatal",' + LineEnding +
        '  "error_class": "' + JsonEscape(E.ClassName) + '",' + LineEnding +
        '  "error_message": "' + JsonEscape(E.Message) + '"' + LineEnding +
        '}' + LineEnding);
      Halt(1);
    end;
  end;
end.
