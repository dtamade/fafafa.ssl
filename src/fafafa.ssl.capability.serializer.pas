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

  // 兼容性
  Result := Result + AddField('compatibilityLevel', IntToStr(ACaps.CompatibilityLevel));
  Result := Result + AddField('knownIssues', '"' + EscapeJSON(ACaps.KnownIssues) + '"', True);

  Result := Result + '}';
end;

function JSONToCapabilities(const AJSON: string): TSSLBackendCapabilities;
begin
  // 简化实现：实际应使用 JSON 解析库
  // 此处仅作示例，生产环境建议使用 fpjson 或其他库
  FillChar(Result, SizeOf(Result), 0);
  raise Exception.Create('JSON deserialization not implemented yet. Please use fpjson library.');
end;

{ ============================================================================ }
{ XML 序列化 }
{ ============================================================================ }

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

  // 兼容性
  Result := Result + AddElement('compatibilityLevel', IntToStr(ACaps.CompatibilityLevel));
  Result := Result + AddElement('knownIssues', XMLEscape(ACaps.KnownIssues));

  Result := Result + '</SSLBackendCapabilities>';
end;

function XMLToCapabilities(const AXML: string): TSSLBackendCapabilities;
begin
  // 简化实现：实际应使用 XML 解析库
  FillChar(Result, SizeOf(Result), 0);
  raise Exception.Create('XML deserialization not implemented yet. Please use DOM or SAX parser.');
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
