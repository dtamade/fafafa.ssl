{
  能力矩阵序列化单元

  提供 JSON 和 XML 格式的序列化/反序列化支持
}

unit fafafa.ssl.capability.serializer;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base;

{ JSON 序列化 }
function CapabilitiesToJSON(const ACaps: TSSLBackendCapabilities;
                            const APretty: Boolean = True): string;
function JSONToCapabilities(const AJSON: string): TSSLBackendCapabilities;

{ XML 序列化 }
function CapabilitiesToXML(const ACaps: TSSLBackendCapabilities;
                          const APretty: Boolean = True): string;
function XMLToCapabilities(const AXML: string): TSSLBackendCapabilities;

{ 文件操作 }
procedure SaveCapabilitiesToFile(const ACaps: TSSLBackendCapabilities;
                                 const AFileName: string;
                                 const AFormat: string = 'json');  // 'json' or 'xml'
function LoadCapabilitiesFromFile(const AFileName: string): TSSLBackendCapabilities;

implementation

uses
  StrUtils;

{ ============================================================================ }
{ JSON 序列化 }
{ ============================================================================ }

function BoolToJSONStr(AValue: Boolean): string;
begin
  if AValue then
    Result := 'true'
  else
    Result := 'false';
end;

function JSONStrToBool(const AValue: string): Boolean;
begin
  Result := SameText(AValue, 'true');
end;

function EscapeJSON(const S: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    case S[I] of
      '"': Result := Result + '\"';
      '\': Result := Result + '\\';
      #8: Result := Result + '\b';
      #9: Result := Result + '\t';
      #10: Result := Result + '\n';
      #12: Result := Result + '\f';
      #13: Result := Result + '\r';
      else
        Result := Result + S[I];
    end;
  end;
end;

function SetToJSONArray(const ASet; const ANames: array of string): string;
var
  I: Integer;
  First: Boolean;
  Value: Integer;
begin
  Result := '[';
  First := True;
  Value := Integer(ASet);

  for I := Low(ANames) to High(ANames) do
  begin
    if (Value and (1 shl I)) <> 0 then
    begin
      if not First then
        Result := Result + ', ';
      Result := Result + '"' + ANames[I] + '"';
      First := False;
    end;
  end;

  Result := Result + ']';
end;

function EncodeCipherSet(const ASet: TSSLCipherSupport): string;
var
  LCipher: TSSLCipher;
begin
  Result := '';
  for LCipher := Low(TSSLCipher) to High(TSSLCipher) do
  begin
    if LCipher in ASet then
    begin
      if Result <> '' then
        Result := Result + ';';
      Result := Result + IntToStr(Ord(LCipher));
    end;
  end;
end;

function DecodeCipherSet(const AValue: string): TSSLCipherSupport;
var
  LParts: TStringList;
  I: Integer;
  LOrdinal: Integer;
begin
  Result := [];
  if Trim(AValue) = '' then
    Exit;

  LParts := TStringList.Create;
  try
    LParts.StrictDelimiter := True;
    LParts.Delimiter := ';';
    LParts.DelimitedText := AValue;
    for I := 0 to LParts.Count - 1 do
    begin
      LOrdinal := StrToIntDef(Trim(LParts[I]), -1);
      if (LOrdinal >= Ord(Low(TSSLCipher))) and
         (LOrdinal <= Ord(High(TSSLCipher))) then
        Include(Result, TSSLCipher(LOrdinal));
    end;
  finally
    LParts.Free;
  end;
end;

function EncodeHashSet(const ASet: TSSLHashSupport): string;
var
  LHash: TSSLHash;
begin
  Result := '';
  for LHash := Low(TSSLHash) to High(TSSLHash) do
  begin
    if LHash in ASet then
    begin
      if Result <> '' then
        Result := Result + ';';
      Result := Result + IntToStr(Ord(LHash));
    end;
  end;
end;

function DecodeHashSet(const AValue: string): TSSLHashSupport;
var
  LParts: TStringList;
  I: Integer;
  LOrdinal: Integer;
begin
  Result := [];
  if Trim(AValue) = '' then
    Exit;

  LParts := TStringList.Create;
  try
    LParts.StrictDelimiter := True;
    LParts.Delimiter := ';';
    LParts.DelimitedText := AValue;
    for I := 0 to LParts.Count - 1 do
    begin
      LOrdinal := StrToIntDef(Trim(LParts[I]), -1);
      if (LOrdinal >= Ord(Low(TSSLHash))) and
         (LOrdinal <= Ord(High(TSSLHash))) then
        Include(Result, TSSLHash(LOrdinal));
    end;
  finally
    LParts.Free;
  end;
end;

function EncodeKeyExchangeSet(const ASet: TSSLKeyExchangeSupport): string;
var
  LKex: TSSLKeyExchange;
begin
  Result := '';
  for LKex := Low(TSSLKeyExchange) to High(TSSLKeyExchange) do
  begin
    if LKex in ASet then
    begin
      if Result <> '' then
        Result := Result + ';';
      Result := Result + IntToStr(Ord(LKex));
    end;
  end;
end;

function DecodeKeyExchangeSet(const AValue: string): TSSLKeyExchangeSupport;
var
  LParts: TStringList;
  I: Integer;
  LOrdinal: Integer;
begin
  Result := [];
  if Trim(AValue) = '' then
    Exit;

  LParts := TStringList.Create;
  try
    LParts.StrictDelimiter := True;
    LParts.Delimiter := ';';
    LParts.DelimitedText := AValue;
    for I := 0 to LParts.Count - 1 do
    begin
      LOrdinal := StrToIntDef(Trim(LParts[I]), -1);
      if (LOrdinal >= Ord(Low(TSSLKeyExchange))) and
         (LOrdinal <= Ord(High(TSSLKeyExchange))) then
        Include(Result, TSSLKeyExchange(LOrdinal));
    end;
  finally
    LParts.Free;
  end;
end;

{ 6018 抑制范围含嵌套函数 FeatureSupportLevelToStr 等 — FPC 限制，函数级指令无法缩小到单个 case }
{$WARN 6018 OFF}
function CapabilitiesToJSON(const ACaps: TSSLBackendCapabilities;
                            const APretty: Boolean = True): string;
var
  Indent: string;
  NL: string;

  function AddField(const AName, AValue: string; ALast: Boolean = False): string;
  begin
    Result := Indent + '"' + AName + '": ' + AValue;
    if not ALast then
      Result := Result + ',';
    Result := Result + NL;
  end;

  function FeatureSupportLevelToStr(ALevel: TSSLFeatureSupportLevel): string;
  begin
    case ALevel of
      sslSupportNone: Result := '"none"';
      sslSupportExperimental: Result := '"experimental"';
      sslSupportStable: Result := '"stable"';
      sslSupportDeprecated: Result := '"deprecated"';
      else Result := '"unknown"';
    end;
  end;

begin
  if APretty then
  begin
    Indent := '  ';
    NL := LineEnding;
  end
  else
  begin
    Indent := '';
    NL := '';
  end;

  Result := '{' + NL;

  // v1.1.0 字段
  Result := Result + AddField('supportsTLS13', BoolToJSONStr(ACaps.SupportsTLS13));
  Result := Result + AddField('supportsALPN', BoolToJSONStr(ACaps.SupportsALPN));
  Result := Result + AddField('supportsSNI', BoolToJSONStr(ACaps.SupportsSNI));
  Result := Result + AddField('supportsOCSPStapling', BoolToJSONStr(ACaps.SupportsOCSPStapling));
  Result := Result + AddField('supportsCertificateTransparency', BoolToJSONStr(ACaps.SupportsCertificateTransparency));
  Result := Result + AddField('supportsSessionTickets', BoolToJSONStr(ACaps.SupportsSessionTickets));
  Result := Result + AddField('supportsECDHE', BoolToJSONStr(ACaps.SupportsECDHE));
  Result := Result + AddField('supportsChaChaPoly', BoolToJSONStr(ACaps.SupportsChaChaPoly));
  Result := Result + AddField('supportsPEMPrivateKey', BoolToJSONStr(ACaps.SupportsPEMPrivateKey));
  Result := Result + AddField('minTLSVersion', IntToStr(Ord(ACaps.MinTLSVersion)));
  Result := Result + AddField('maxTLSVersion', IntToStr(Ord(ACaps.MaxTLSVersion)));

  // v1.2.0 字段
  Result := Result + AddField('backendType', IntToStr(Ord(ACaps.BackendType)));
  Result := Result + AddField('backendImplType', IntToStr(Ord(ACaps.BackendImplType)));
  Result := Result + AddField('backendVersion', '"' + EscapeJSON(ACaps.BackendVersion) + '"');
  Result := Result + AddField('supportsDTLS', BoolToJSONStr(ACaps.SupportsDTLS));

  // 功能支持级别
  Result := Result + AddField('sniSupport', FeatureSupportLevelToStr(ACaps.SNISupport));
  Result := Result + AddField('alpnSupport', FeatureSupportLevelToStr(ACaps.ALPNSupport));
  Result := Result + AddField('ocspStaplingSupport', FeatureSupportLevelToStr(ACaps.OCSPStaplingSupport));
  Result := Result + AddField('certTransparencySupport', FeatureSupportLevelToStr(ACaps.CertTransparencySupport));
  Result := Result + AddField('sessionTicketsSupport', FeatureSupportLevelToStr(ACaps.SessionTicketsSupport));
  Result := Result + AddField('sessionCacheSupport', FeatureSupportLevelToStr(ACaps.SessionCacheSupport));
  Result := Result + AddField('zeroRTTSupport', FeatureSupportLevelToStr(ACaps.ZeroRTTSupport));
  Result := Result + AddField('earlyDataSupport', FeatureSupportLevelToStr(ACaps.EarlyDataSupport));
  Result := Result + AddField('renegotiationSupport', FeatureSupportLevelToStr(ACaps.RenegotiationSupport));
  Result := Result + AddField('postHandshakeAuthSupport', FeatureSupportLevelToStr(ACaps.PostHandshakeAuthSupport));

  // 算法支持
  Result := Result + AddField('supportedCiphers', '"' + EncodeCipherSet(ACaps.SupportedCiphers) + '"');
  Result := Result + AddField('supportedHashes', '"' + EncodeHashSet(ACaps.SupportedHashes) + '"');
  Result := Result + AddField('supportedKeyExchanges', '"' + EncodeKeyExchangeSet(ACaps.SupportedKeyExchanges) + '"');

  // 性能特性
  Result := Result + AddField('hasHardwareAcceleration', BoolToJSONStr(ACaps.HasHardwareAcceleration));
  Result := Result + AddField('hasSIMDOptimization', BoolToJSONStr(ACaps.HasSIMDOptimization));
  Result := Result + AddField('hasAssemblyOptimization', BoolToJSONStr(ACaps.HasAssemblyOptimization));

  // 平台特性
  Result := Result + AddField('requiresExternalLibrary', BoolToJSONStr(ACaps.RequiresExternalLibrary));
  Result := Result + AddField('supportsSystemCertStore', BoolToJSONStr(ACaps.SupportsSystemCertStore));
  Result := Result + AddField('supportsPKCS11', BoolToJSONStr(ACaps.SupportsPKCS11));
  Result := Result + AddField('supportsTPM', BoolToJSONStr(ACaps.SupportsTPM));

  // 安全特性
  Result := Result + AddField('hasConstantTimeOperations', BoolToJSONStr(ACaps.HasConstantTimeOperations));
  Result := Result + AddField('supportsFIPSMode', BoolToJSONStr(ACaps.SupportsFIPSMode));
  Result := Result + AddField('hasSecureMemoryWipe', BoolToJSONStr(ACaps.HasSecureMemoryWipe));

  // 证书和密钥支持
  Result := Result + AddField('supportsDERPrivateKey', BoolToJSONStr(ACaps.SupportsDERPrivateKey));
  Result := Result + AddField('supportsPKCS8PrivateKey', BoolToJSONStr(ACaps.SupportsPKCS8PrivateKey));
  Result := Result + AddField('supportsPKCS12', BoolToJSONStr(ACaps.SupportsPKCS12));
  Result := Result + AddField('supportsPasswordProtectedKeys', BoolToJSONStr(ACaps.SupportsPasswordProtectedKeys));

  // 扩展性
  Result := Result + AddField('supportsCustomCipherSuites', BoolToJSONStr(ACaps.SupportsCustomCipherSuites));
  Result := Result + AddField('supportsCallbacks', BoolToJSONStr(ACaps.SupportsCallbacks));

  // 兼容性
  Result := Result + AddField('compatibilityLevel', IntToStr(ACaps.CompatibilityLevel));
  Result := Result + AddField('knownIssues', '"' + EscapeJSON(ACaps.KnownIssues) + '"', True);

  Result := Result + '}';
end;
{$WARN 6018 ON}

function JSONToCapabilities(const AJSON: string): TSSLBackendCapabilities;
var
  LValue: string;
  LIsString: Boolean;

  function IntToProtocolVersion(AInt: Integer): TSSLProtocolVersion;
  begin
    if (AInt >= Ord(Low(TSSLProtocolVersion))) and
       (AInt <= Ord(High(TSSLProtocolVersion))) then
      Result := TSSLProtocolVersion(AInt)
    else
      Result := sslProtocolUnknown;
  end;

  function IntToLibraryType(AInt: Integer): TSSLLibraryType;
  begin
    if (AInt >= Ord(Low(TSSLLibraryType))) and
       (AInt <= Ord(High(TSSLLibraryType))) then
      Result := TSSLLibraryType(AInt)
    else
      Result := sslAutoDetect;
  end;

  function IntToBackendImplType(AInt: Integer): TSSLBackendImplType;
  begin
    if (AInt >= Ord(Low(TSSLBackendImplType))) and
       (AInt <= Ord(High(TSSLBackendImplType))) then
      Result := TSSLBackendImplType(AInt)
    else
      Result := sslImplNative;
  end;

  function StrToFeatureSupportLevel(const AValue: string): TSSLFeatureSupportLevel;
  begin
    if SameText(AValue, 'none') then
      Result := sslSupportNone
    else if SameText(AValue, 'experimental') then
      Result := sslSupportExperimental
    else if SameText(AValue, 'stable') then
      Result := sslSupportStable
    else if SameText(AValue, 'deprecated') then
      Result := sslSupportDeprecated
    else
      Result := sslSupportNone;
  end;

  function JSONUnescape(const S: string): string;
  var
    I: Integer;
  begin
    Result := '';
    I := 1;
    while I <= Length(S) do
    begin
      if (S[I] = '\') and (I < Length(S)) then
      begin
        Inc(I);
        case S[I] of
          '"': Result := Result + '"';
          '\': Result := Result + '\';
          '/': Result := Result + '/';
          'b': Result := Result + #8;
          't': Result := Result + #9;
          'n': Result := Result + #10;
          'f': Result := Result + #12;
          'r': Result := Result + #13;
        else
          Result := Result + S[I];
        end;
      end
      else
        Result := Result + S[I];
      Inc(I);
    end;
  end;

  function ExtractJSONValue(const AName: string; out AOutValue: string;
    out AOutIsString: Boolean): Boolean;
  var
    LKey: string;
    LKeyPos: Integer;
    LColonPos: Integer;
    LPos: Integer;
    LStart: Integer;
    LEscaped: Boolean;
  begin
    Result := False;
    AOutValue := '';
    AOutIsString := False;

    LKey := '"' + AName + '"';
    LKeyPos := Pos(LKey, AJSON);
    if LKeyPos <= 0 then
      Exit;

    LColonPos := PosEx(':', AJSON, LKeyPos + Length(LKey));
    if LColonPos <= 0 then
      Exit;

    LPos := LColonPos + 1;
    while (LPos <= Length(AJSON)) and (AJSON[LPos] in [' ', #9, #10, #13]) do
      Inc(LPos);

    if LPos > Length(AJSON) then
      Exit;

    if AJSON[LPos] = '"' then
    begin
      AOutIsString := True;
      Inc(LPos);
      LStart := LPos;
      LEscaped := False;
      while LPos <= Length(AJSON) do
      begin
        if LEscaped then
          LEscaped := False
        else if AJSON[LPos] = '\\' then
          LEscaped := True
        else if AJSON[LPos] = '"' then
          Break;
        Inc(LPos);
      end;

      if (LPos > Length(AJSON)) or (AJSON[LPos] <> '"') then
        Exit;

      AOutValue := JSONUnescape(Copy(AJSON, LStart, LPos - LStart));
      Result := True;
      Exit;
    end;

    LStart := LPos;
    while (LPos <= Length(AJSON)) and not (AJSON[LPos] in [',', '}', #10, #13]) do
      Inc(LPos);

    AOutValue := Trim(Copy(AJSON, LStart, LPos - LStart));
    Result := AOutValue <> '';
  end;

begin
  FillChar(Result, SizeOf(Result), 0);

  // v1.1.0 字段
  if ExtractJSONValue('supportsTLS13', LValue, LIsString) then
    Result.SupportsTLS13 := JSONStrToBool(LValue);
  if ExtractJSONValue('supportsALPN', LValue, LIsString) then
    Result.SupportsALPN := JSONStrToBool(LValue);
  if ExtractJSONValue('supportsSNI', LValue, LIsString) then
    Result.SupportsSNI := JSONStrToBool(LValue);
  if ExtractJSONValue('supportsOCSPStapling', LValue, LIsString) then
    Result.SupportsOCSPStapling := JSONStrToBool(LValue);
  if ExtractJSONValue('supportsCertificateTransparency', LValue, LIsString) then
    Result.SupportsCertificateTransparency := JSONStrToBool(LValue);
  if ExtractJSONValue('supportsSessionTickets', LValue, LIsString) then
    Result.SupportsSessionTickets := JSONStrToBool(LValue);
  if ExtractJSONValue('supportsECDHE', LValue, LIsString) then
    Result.SupportsECDHE := JSONStrToBool(LValue);
  if ExtractJSONValue('supportsChaChaPoly', LValue, LIsString) then
    Result.SupportsChaChaPoly := JSONStrToBool(LValue);
  if ExtractJSONValue('supportsPEMPrivateKey', LValue, LIsString) then
    Result.SupportsPEMPrivateKey := JSONStrToBool(LValue);
  if ExtractJSONValue('minTLSVersion', LValue, LIsString) then
    Result.MinTLSVersion := IntToProtocolVersion(StrToIntDef(LValue, Ord(sslProtocolUnknown)));
  if ExtractJSONValue('maxTLSVersion', LValue, LIsString) then
    Result.MaxTLSVersion := IntToProtocolVersion(StrToIntDef(LValue, Ord(sslProtocolUnknown)));

  // v1.2.0 字段
  if ExtractJSONValue('backendType', LValue, LIsString) then
    Result.BackendType := IntToLibraryType(StrToIntDef(LValue, Ord(sslAutoDetect)));
  if ExtractJSONValue('backendImplType', LValue, LIsString) then
    Result.BackendImplType := IntToBackendImplType(StrToIntDef(LValue, Ord(sslImplNative)));
  if ExtractJSONValue('backendVersion', LValue, LIsString) then
    Result.BackendVersion := LValue;
  if ExtractJSONValue('supportsDTLS', LValue, LIsString) then
    Result.SupportsDTLS := JSONStrToBool(LValue);

  // 功能支持级别
  if ExtractJSONValue('sniSupport', LValue, LIsString) then
    Result.SNISupport := StrToFeatureSupportLevel(LValue);
  if ExtractJSONValue('alpnSupport', LValue, LIsString) then
    Result.ALPNSupport := StrToFeatureSupportLevel(LValue);
  if ExtractJSONValue('ocspStaplingSupport', LValue, LIsString) then
    Result.OCSPStaplingSupport := StrToFeatureSupportLevel(LValue);
  if ExtractJSONValue('certTransparencySupport', LValue, LIsString) then
    Result.CertTransparencySupport := StrToFeatureSupportLevel(LValue);
  if ExtractJSONValue('sessionTicketsSupport', LValue, LIsString) then
    Result.SessionTicketsSupport := StrToFeatureSupportLevel(LValue);
  if ExtractJSONValue('sessionCacheSupport', LValue, LIsString) then
    Result.SessionCacheSupport := StrToFeatureSupportLevel(LValue);
  if ExtractJSONValue('zeroRTTSupport', LValue, LIsString) then
    Result.ZeroRTTSupport := StrToFeatureSupportLevel(LValue);
  if ExtractJSONValue('earlyDataSupport', LValue, LIsString) then
    Result.EarlyDataSupport := StrToFeatureSupportLevel(LValue);
  if ExtractJSONValue('renegotiationSupport', LValue, LIsString) then
    Result.RenegotiationSupport := StrToFeatureSupportLevel(LValue);
  if ExtractJSONValue('postHandshakeAuthSupport', LValue, LIsString) then
    Result.PostHandshakeAuthSupport := StrToFeatureSupportLevel(LValue);

  // 算法支持
  if ExtractJSONValue('supportedCiphers', LValue, LIsString) then
    Result.SupportedCiphers := DecodeCipherSet(LValue);
  if ExtractJSONValue('supportedHashes', LValue, LIsString) then
    Result.SupportedHashes := DecodeHashSet(LValue);
  if ExtractJSONValue('supportedKeyExchanges', LValue, LIsString) then
    Result.SupportedKeyExchanges := DecodeKeyExchangeSet(LValue);

  // 性能特性
  if ExtractJSONValue('hasHardwareAcceleration', LValue, LIsString) then
    Result.HasHardwareAcceleration := JSONStrToBool(LValue);
  if ExtractJSONValue('hasSIMDOptimization', LValue, LIsString) then
    Result.HasSIMDOptimization := JSONStrToBool(LValue);
  if ExtractJSONValue('hasAssemblyOptimization', LValue, LIsString) then
    Result.HasAssemblyOptimization := JSONStrToBool(LValue);

  // 平台特性
  if ExtractJSONValue('requiresExternalLibrary', LValue, LIsString) then
    Result.RequiresExternalLibrary := JSONStrToBool(LValue);
  if ExtractJSONValue('supportsSystemCertStore', LValue, LIsString) then
    Result.SupportsSystemCertStore := JSONStrToBool(LValue);
  if ExtractJSONValue('supportsPKCS11', LValue, LIsString) then
    Result.SupportsPKCS11 := JSONStrToBool(LValue);
  if ExtractJSONValue('supportsTPM', LValue, LIsString) then
    Result.SupportsTPM := JSONStrToBool(LValue);

  // 安全特性
  if ExtractJSONValue('hasConstantTimeOperations', LValue, LIsString) then
    Result.HasConstantTimeOperations := JSONStrToBool(LValue);
  if ExtractJSONValue('supportsFIPSMode', LValue, LIsString) then
    Result.SupportsFIPSMode := JSONStrToBool(LValue);
  if ExtractJSONValue('hasSecureMemoryWipe', LValue, LIsString) then
    Result.HasSecureMemoryWipe := JSONStrToBool(LValue);

  // 证书和密钥支持
  if ExtractJSONValue('supportsDERPrivateKey', LValue, LIsString) then
    Result.SupportsDERPrivateKey := JSONStrToBool(LValue);
  if ExtractJSONValue('supportsPKCS8PrivateKey', LValue, LIsString) then
    Result.SupportsPKCS8PrivateKey := JSONStrToBool(LValue);
  if ExtractJSONValue('supportsPKCS12', LValue, LIsString) then
    Result.SupportsPKCS12 := JSONStrToBool(LValue);
  if ExtractJSONValue('supportsPasswordProtectedKeys', LValue, LIsString) then
    Result.SupportsPasswordProtectedKeys := JSONStrToBool(LValue);

  // 扩展性
  if ExtractJSONValue('supportsCustomCipherSuites', LValue, LIsString) then
    Result.SupportsCustomCipherSuites := JSONStrToBool(LValue);
  if ExtractJSONValue('supportsCallbacks', LValue, LIsString) then
    Result.SupportsCallbacks := JSONStrToBool(LValue);

  // 兼容性
  if ExtractJSONValue('compatibilityLevel', LValue, LIsString) then
    Result.CompatibilityLevel := StrToIntDef(LValue, 0);
  if ExtractJSONValue('knownIssues', LValue, LIsString) then
    Result.KnownIssues := LValue;
end;

{ ============================================================================ }
{ XML 序列化 }
{ ============================================================================ }

{ 6018 抑制范围含嵌套函数 — 同 CapabilitiesToJSON }
{$WARN 6018 OFF}
function CapabilitiesToXML(const ACaps: TSSLBackendCapabilities;
                          const APretty: Boolean = True): string;
var
  Indent: string;
  NL: string;

  function XMLEscape(const S: string): string;
  var
    I: Integer;
  begin
    Result := '';
    for I := 1 to Length(S) do
    begin
      case S[I] of
        '<': Result := Result + '&lt;';
        '>': Result := Result + '&gt;';
        '&': Result := Result + '&amp;';
        '"': Result := Result + '&quot;';
        '''': Result := Result + '&apos;';
        else
          Result := Result + S[I];
      end;
    end;
  end;

  function AddElement(const AName, AValue: string; const AIndent: Integer = 1): string;
  var
    I: Integer;
  begin
    Result := '';
    for I := 1 to AIndent do
      Result := Result + Indent;
    Result := Result + '<' + AName + '>' + AValue + '</' + AName + '>' + NL;
  end;

  function FeatureSupportLevelToStr(ALevel: TSSLFeatureSupportLevel): string;
  begin
    case ALevel of
      sslSupportNone: Result := 'none';
      sslSupportExperimental: Result := 'experimental';
      sslSupportStable: Result := 'stable';
      sslSupportDeprecated: Result := 'deprecated';
      else Result := 'unknown';
    end;
  end;

begin
  if APretty then
  begin
    Indent := '  ';
    NL := LineEnding;
  end
  else
  begin
    Indent := '';
    NL := '';
  end;

  Result := '<?xml version="1.0" encoding="UTF-8"?>' + NL;
  Result := Result + '<SSLBackendCapabilities>' + NL;

  // v1.1.0 字段
  Result := Result + AddElement('supportsTLS13', BoolToStr(ACaps.SupportsTLS13, True));
  Result := Result + AddElement('supportsALPN', BoolToStr(ACaps.SupportsALPN, True));
  Result := Result + AddElement('supportsSNI', BoolToStr(ACaps.SupportsSNI, True));
  Result := Result + AddElement('supportsOCSPStapling', BoolToStr(ACaps.SupportsOCSPStapling, True));
  Result := Result + AddElement('supportsCertificateTransparency', BoolToStr(ACaps.SupportsCertificateTransparency, True));
  Result := Result + AddElement('supportsSessionTickets', BoolToStr(ACaps.SupportsSessionTickets, True));
  Result := Result + AddElement('supportsECDHE', BoolToStr(ACaps.SupportsECDHE, True));
  Result := Result + AddElement('supportsChaChaPoly', BoolToStr(ACaps.SupportsChaChaPoly, True));
  Result := Result + AddElement('supportsPEMPrivateKey', BoolToStr(ACaps.SupportsPEMPrivateKey, True));
  Result := Result + AddElement('minTLSVersion', IntToStr(Ord(ACaps.MinTLSVersion)));
  Result := Result + AddElement('maxTLSVersion', IntToStr(Ord(ACaps.MaxTLSVersion)));

  // v1.2.0 字段
  Result := Result + AddElement('backendType', IntToStr(Ord(ACaps.BackendType)));
  Result := Result + AddElement('backendImplType', IntToStr(Ord(ACaps.BackendImplType)));
  Result := Result + AddElement('backendVersion', XMLEscape(ACaps.BackendVersion));
  Result := Result + AddElement('supportsDTLS', BoolToStr(ACaps.SupportsDTLS, True));

  // 功能支持级别
  Result := Result + AddElement('sniSupport', FeatureSupportLevelToStr(ACaps.SNISupport));
  Result := Result + AddElement('alpnSupport', FeatureSupportLevelToStr(ACaps.ALPNSupport));
  Result := Result + AddElement('ocspStaplingSupport', FeatureSupportLevelToStr(ACaps.OCSPStaplingSupport));
  Result := Result + AddElement('certTransparencySupport', FeatureSupportLevelToStr(ACaps.CertTransparencySupport));
  Result := Result + AddElement('sessionTicketsSupport', FeatureSupportLevelToStr(ACaps.SessionTicketsSupport));
  Result := Result + AddElement('sessionCacheSupport', FeatureSupportLevelToStr(ACaps.SessionCacheSupport));
  Result := Result + AddElement('zeroRTTSupport', FeatureSupportLevelToStr(ACaps.ZeroRTTSupport));
  Result := Result + AddElement('earlyDataSupport', FeatureSupportLevelToStr(ACaps.EarlyDataSupport));
  Result := Result + AddElement('renegotiationSupport', FeatureSupportLevelToStr(ACaps.RenegotiationSupport));
  Result := Result + AddElement('postHandshakeAuthSupport', FeatureSupportLevelToStr(ACaps.PostHandshakeAuthSupport));

  // 算法支持
  Result := Result + AddElement('supportedCiphers', EncodeCipherSet(ACaps.SupportedCiphers));
  Result := Result + AddElement('supportedHashes', EncodeHashSet(ACaps.SupportedHashes));
  Result := Result + AddElement('supportedKeyExchanges', EncodeKeyExchangeSet(ACaps.SupportedKeyExchanges));

  // 性能特性
  Result := Result + AddElement('hasHardwareAcceleration', BoolToStr(ACaps.HasHardwareAcceleration, True));
  Result := Result + AddElement('hasSIMDOptimization', BoolToStr(ACaps.HasSIMDOptimization, True));
  Result := Result + AddElement('hasAssemblyOptimization', BoolToStr(ACaps.HasAssemblyOptimization, True));

  // 平台特性
  Result := Result + AddElement('requiresExternalLibrary', BoolToStr(ACaps.RequiresExternalLibrary, True));
  Result := Result + AddElement('supportsSystemCertStore', BoolToStr(ACaps.SupportsSystemCertStore, True));
  Result := Result + AddElement('supportsPKCS11', BoolToStr(ACaps.SupportsPKCS11, True));
  Result := Result + AddElement('supportsTPM', BoolToStr(ACaps.SupportsTPM, True));

  // 安全特性
  Result := Result + AddElement('hasConstantTimeOperations', BoolToStr(ACaps.HasConstantTimeOperations, True));
  Result := Result + AddElement('supportsFIPSMode', BoolToStr(ACaps.SupportsFIPSMode, True));
  Result := Result + AddElement('hasSecureMemoryWipe', BoolToStr(ACaps.HasSecureMemoryWipe, True));

  // 证书和密钥支持
  Result := Result + AddElement('supportsDERPrivateKey', BoolToStr(ACaps.SupportsDERPrivateKey, True));
  Result := Result + AddElement('supportsPKCS8PrivateKey', BoolToStr(ACaps.SupportsPKCS8PrivateKey, True));
  Result := Result + AddElement('supportsPKCS12', BoolToStr(ACaps.SupportsPKCS12, True));
  Result := Result + AddElement('supportsPasswordProtectedKeys', BoolToStr(ACaps.SupportsPasswordProtectedKeys, True));

  // 扩展性
  Result := Result + AddElement('supportsCustomCipherSuites', BoolToStr(ACaps.SupportsCustomCipherSuites, True));
  Result := Result + AddElement('supportsCallbacks', BoolToStr(ACaps.SupportsCallbacks, True));

  // 兼容性
  Result := Result + AddElement('compatibilityLevel', IntToStr(ACaps.CompatibilityLevel));
  Result := Result + AddElement('knownIssues', XMLEscape(ACaps.KnownIssues));

  Result := Result + '</SSLBackendCapabilities>';
end;
{$WARN 6018 ON}

function XMLToCapabilities(const AXML: string): TSSLBackendCapabilities;
var
  LValue: string;

  function IntToProtocolVersion(AInt: Integer): TSSLProtocolVersion;
  begin
    if (AInt >= Ord(Low(TSSLProtocolVersion))) and
       (AInt <= Ord(High(TSSLProtocolVersion))) then
      Result := TSSLProtocolVersion(AInt)
    else
      Result := sslProtocolUnknown;
  end;

  function IntToLibraryType(AInt: Integer): TSSLLibraryType;
  begin
    if (AInt >= Ord(Low(TSSLLibraryType))) and
       (AInt <= Ord(High(TSSLLibraryType))) then
      Result := TSSLLibraryType(AInt)
    else
      Result := sslAutoDetect;
  end;

  function IntToBackendImplType(AInt: Integer): TSSLBackendImplType;
  begin
    if (AInt >= Ord(Low(TSSLBackendImplType))) and
       (AInt <= Ord(High(TSSLBackendImplType))) then
      Result := TSSLBackendImplType(AInt)
    else
      Result := sslImplNative;
  end;

  function StrToFeatureSupportLevel(const AValue: string): TSSLFeatureSupportLevel;
  begin
    if SameText(AValue, 'none') then
      Result := sslSupportNone
    else if SameText(AValue, 'experimental') then
      Result := sslSupportExperimental
    else if SameText(AValue, 'stable') then
      Result := sslSupportStable
    else if SameText(AValue, 'deprecated') then
      Result := sslSupportDeprecated
    else
      Result := sslSupportNone;
  end;

  function XMLUnescape(const S: string): string;
  begin
    Result := S;
    Result := StringReplace(Result, '&lt;', '<', [rfReplaceAll]);
    Result := StringReplace(Result, '&gt;', '>', [rfReplaceAll]);
    Result := StringReplace(Result, '&quot;', '"', [rfReplaceAll]);
    Result := StringReplace(Result, '&apos;', '''', [rfReplaceAll]);
    Result := StringReplace(Result, '&amp;', '&', [rfReplaceAll]);
  end;

  function ExtractXMLValue(const AName: string; out AOutValue: string): Boolean;
  var
    LOpenTag: string;
    LCloseTag: string;
    LOpenPos: Integer;
    LValueStart: Integer;
    LClosePos: Integer;
  begin
    Result := False;
    AOutValue := '';

    LOpenTag := '<' + AName + '>';
    LCloseTag := '</' + AName + '>';

    LOpenPos := Pos(LOpenTag, AXML);
    if LOpenPos <= 0 then
      Exit;

    LValueStart := LOpenPos + Length(LOpenTag);
    LClosePos := PosEx(LCloseTag, AXML, LValueStart);
    if LClosePos <= 0 then
      Exit;

    AOutValue := XMLUnescape(Copy(AXML, LValueStart, LClosePos - LValueStart));
    Result := True;
  end;

begin
  FillChar(Result, SizeOf(Result), 0);

  // v1.1.0 字段
  if ExtractXMLValue('supportsTLS13', LValue) then
    Result.SupportsTLS13 := JSONStrToBool(Trim(LValue));
  if ExtractXMLValue('supportsALPN', LValue) then
    Result.SupportsALPN := JSONStrToBool(Trim(LValue));
  if ExtractXMLValue('supportsSNI', LValue) then
    Result.SupportsSNI := JSONStrToBool(Trim(LValue));
  if ExtractXMLValue('supportsOCSPStapling', LValue) then
    Result.SupportsOCSPStapling := JSONStrToBool(Trim(LValue));
  if ExtractXMLValue('supportsCertificateTransparency', LValue) then
    Result.SupportsCertificateTransparency := JSONStrToBool(Trim(LValue));
  if ExtractXMLValue('supportsSessionTickets', LValue) then
    Result.SupportsSessionTickets := JSONStrToBool(Trim(LValue));
  if ExtractXMLValue('supportsECDHE', LValue) then
    Result.SupportsECDHE := JSONStrToBool(Trim(LValue));
  if ExtractXMLValue('supportsChaChaPoly', LValue) then
    Result.SupportsChaChaPoly := JSONStrToBool(Trim(LValue));
  if ExtractXMLValue('supportsPEMPrivateKey', LValue) then
    Result.SupportsPEMPrivateKey := JSONStrToBool(Trim(LValue));
  if ExtractXMLValue('minTLSVersion', LValue) then
    Result.MinTLSVersion := IntToProtocolVersion(StrToIntDef(Trim(LValue), Ord(sslProtocolUnknown)));
  if ExtractXMLValue('maxTLSVersion', LValue) then
    Result.MaxTLSVersion := IntToProtocolVersion(StrToIntDef(Trim(LValue), Ord(sslProtocolUnknown)));

  // v1.2.0 字段
  if ExtractXMLValue('backendType', LValue) then
    Result.BackendType := IntToLibraryType(StrToIntDef(Trim(LValue), Ord(sslAutoDetect)));
  if ExtractXMLValue('backendImplType', LValue) then
    Result.BackendImplType := IntToBackendImplType(StrToIntDef(Trim(LValue), Ord(sslImplNative)));
  if ExtractXMLValue('backendVersion', LValue) then
    Result.BackendVersion := LValue;
  if ExtractXMLValue('supportsDTLS', LValue) then
    Result.SupportsDTLS := JSONStrToBool(Trim(LValue));

  // 功能支持级别
  if ExtractXMLValue('sniSupport', LValue) then
    Result.SNISupport := StrToFeatureSupportLevel(Trim(LValue));
  if ExtractXMLValue('alpnSupport', LValue) then
    Result.ALPNSupport := StrToFeatureSupportLevel(Trim(LValue));
  if ExtractXMLValue('ocspStaplingSupport', LValue) then
    Result.OCSPStaplingSupport := StrToFeatureSupportLevel(Trim(LValue));
  if ExtractXMLValue('certTransparencySupport', LValue) then
    Result.CertTransparencySupport := StrToFeatureSupportLevel(Trim(LValue));
  if ExtractXMLValue('sessionTicketsSupport', LValue) then
    Result.SessionTicketsSupport := StrToFeatureSupportLevel(Trim(LValue));
  if ExtractXMLValue('sessionCacheSupport', LValue) then
    Result.SessionCacheSupport := StrToFeatureSupportLevel(Trim(LValue));
  if ExtractXMLValue('zeroRTTSupport', LValue) then
    Result.ZeroRTTSupport := StrToFeatureSupportLevel(Trim(LValue));
  if ExtractXMLValue('earlyDataSupport', LValue) then
    Result.EarlyDataSupport := StrToFeatureSupportLevel(Trim(LValue));
  if ExtractXMLValue('renegotiationSupport', LValue) then
    Result.RenegotiationSupport := StrToFeatureSupportLevel(Trim(LValue));
  if ExtractXMLValue('postHandshakeAuthSupport', LValue) then
    Result.PostHandshakeAuthSupport := StrToFeatureSupportLevel(Trim(LValue));

  // 算法支持
  if ExtractXMLValue('supportedCiphers', LValue) then
    Result.SupportedCiphers := DecodeCipherSet(Trim(LValue));
  if ExtractXMLValue('supportedHashes', LValue) then
    Result.SupportedHashes := DecodeHashSet(Trim(LValue));
  if ExtractXMLValue('supportedKeyExchanges', LValue) then
    Result.SupportedKeyExchanges := DecodeKeyExchangeSet(Trim(LValue));

  // 性能特性
  if ExtractXMLValue('hasHardwareAcceleration', LValue) then
    Result.HasHardwareAcceleration := JSONStrToBool(Trim(LValue));
  if ExtractXMLValue('hasSIMDOptimization', LValue) then
    Result.HasSIMDOptimization := JSONStrToBool(Trim(LValue));
  if ExtractXMLValue('hasAssemblyOptimization', LValue) then
    Result.HasAssemblyOptimization := JSONStrToBool(Trim(LValue));

  // 平台特性
  if ExtractXMLValue('requiresExternalLibrary', LValue) then
    Result.RequiresExternalLibrary := JSONStrToBool(Trim(LValue));
  if ExtractXMLValue('supportsSystemCertStore', LValue) then
    Result.SupportsSystemCertStore := JSONStrToBool(Trim(LValue));
  if ExtractXMLValue('supportsPKCS11', LValue) then
    Result.SupportsPKCS11 := JSONStrToBool(Trim(LValue));
  if ExtractXMLValue('supportsTPM', LValue) then
    Result.SupportsTPM := JSONStrToBool(Trim(LValue));

  // 安全特性
  if ExtractXMLValue('hasConstantTimeOperations', LValue) then
    Result.HasConstantTimeOperations := JSONStrToBool(Trim(LValue));
  if ExtractXMLValue('supportsFIPSMode', LValue) then
    Result.SupportsFIPSMode := JSONStrToBool(Trim(LValue));
  if ExtractXMLValue('hasSecureMemoryWipe', LValue) then
    Result.HasSecureMemoryWipe := JSONStrToBool(Trim(LValue));

  // 证书和密钥支持
  if ExtractXMLValue('supportsDERPrivateKey', LValue) then
    Result.SupportsDERPrivateKey := JSONStrToBool(Trim(LValue));
  if ExtractXMLValue('supportsPKCS8PrivateKey', LValue) then
    Result.SupportsPKCS8PrivateKey := JSONStrToBool(Trim(LValue));
  if ExtractXMLValue('supportsPKCS12', LValue) then
    Result.SupportsPKCS12 := JSONStrToBool(Trim(LValue));
  if ExtractXMLValue('supportsPasswordProtectedKeys', LValue) then
    Result.SupportsPasswordProtectedKeys := JSONStrToBool(Trim(LValue));

  // 扩展性
  if ExtractXMLValue('supportsCustomCipherSuites', LValue) then
    Result.SupportsCustomCipherSuites := JSONStrToBool(Trim(LValue));
  if ExtractXMLValue('supportsCallbacks', LValue) then
    Result.SupportsCallbacks := JSONStrToBool(Trim(LValue));

  // 兼容性
  if ExtractXMLValue('compatibilityLevel', LValue) then
    Result.CompatibilityLevel := StrToIntDef(Trim(LValue), 0);
  if ExtractXMLValue('knownIssues', LValue) then
    Result.KnownIssues := LValue;
end;

{ ============================================================================ }
{ 文件操作 }
{ ============================================================================ }

procedure SaveCapabilitiesToFile(const ACaps: TSSLBackendCapabilities;
                                 const AFileName: string;
                                 const AFormat: string = 'json');
var
  SL: TStringList;
  Content: string;
begin
  if SameText(AFormat, 'json') then
    Content := CapabilitiesToJSON(ACaps, True)
  else if SameText(AFormat, 'xml') then
    Content := CapabilitiesToXML(ACaps, True)
  else
    raise Exception.CreateFmt('Unsupported format: %s', [AFormat]);

  SL := TStringList.Create;
  try
    SL.Text := Content;
    SL.SaveToFile(AFileName);
  finally
    SL.Free;
  end;
end;

function LoadCapabilitiesFromFile(const AFileName: string): TSSLBackendCapabilities;
var
  SL: TStringList;
  Content: string;
  Ext: string;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(AFileName);
    Content := SL.Text;

    Ext := LowerCase(ExtractFileExt(AFileName));
    if Ext = '.json' then
      Result := JSONToCapabilities(Content)
    else if Ext = '.xml' then
      Result := XMLToCapabilities(Content)
    else
      raise Exception.CreateFmt('Unknown file extension: %s', [Ext]);
  finally
    SL.Free;
  end;
end;

end.
