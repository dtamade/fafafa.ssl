unit fafafa.ssl.ocsp;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

{
  OCSP (Online Certificate Status Protocol) 纯 Pascal 实现

  基于 RFC 6960 实现 OCSP 请求/响应处理。
  使用 fafafa.ssl.asn1 和 fafafa.ssl.x509 模块。

  OCSP 用于在线查询证书吊销状态，比 CRL 更高效。

  @author fafafa.ssl team
  @version 1.0.0
}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.asn1, fafafa.ssl.x509, fafafa.ssl.crypto.hash;

type
  // ========================================================================
  // OCSP 状态
  // ========================================================================
  TOCSPCertStatus = (
    ocspGood,           // 证书有效
    ocspRevoked,        // 证书已吊销
    ocspUnknown         // 状态未知
  );

  // ========================================================================
  // OCSP 响应状态
  // ========================================================================
  TOCSPResponseStatus = (
    ocsprsSuccessful,        // 响应成功
    ocsprsMalformedRequest,  // 请求格式错误
    ocsprsInternalError,     // 内部错误
    ocsprsTryLater,          // 稍后重试
    ocsprsSignatureRequired, // 需要签名
    ocsprsUnauthorized       // 未授权
  );

  // ========================================================================
  // 吊销原因
  // ========================================================================
  TOCSPRevokeReason = (
    ocsprrUnspecified,         // 未指定
    ocsprrKeyCompromise,       // 密钥泄露
    ocsprrCACompromise,        // CA 泄露
    ocsprrAffiliationChanged,  // 从属关系变更
    ocsprrSuperseded,          // 被取代
    ocsprrCessationOfOperation,// 停止运营
    ocsprrCertificateHold,     // 证书暂停
    ocsprrRemoveFromCRL,       // 从 CRL 移除
    ocsprrPrivilegeWithdrawn,  // 权限撤回
    ocsprrAACompromise         // AA 泄露
  );

  // ========================================================================
  // OCSP 证书 ID
  // ========================================================================
  TOCSPCertID = record
    HashAlgorithm: string;      // 哈希算法 OID
    IssuerNameHash: TBytes;     // 颁发者名称哈希
    IssuerKeyHash: TBytes;      // 颁发者公钥哈希
    SerialNumber: TBytes;       // 证书序列号

    function Encode: TBytes;
    class function Decode(ANode: TASN1Node): TOCSPCertID; static;
    class function Create(ACert, AIssuerCert: TX509Certificate;
      AHashAlg: THashAlgorithm = haSHA1): TOCSPCertID; static;
  end;

  // ========================================================================
  // OCSP 单个响应
  // ========================================================================
  TOCSPSingleResponse = record
    CertID: TOCSPCertID;
    CertStatus: TOCSPCertStatus;
    ThisUpdate: TDateTime;
    NextUpdate: TDateTime;
    HasNextUpdate: Boolean;
    RevokedTime: TDateTime;
    RevokeReason: TOCSPRevokeReason;
  end;

  TOCSPSingleResponseArray = array of TOCSPSingleResponse;

  // ========================================================================
  // OCSP 请求
  // ========================================================================
  TOCSPRequest = class
  private
    FCertIDs: array of TOCSPCertID;
    FNonce: TBytes;
    FUseNonce: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddCertificate(ACert, AIssuerCert: TX509Certificate);
    procedure AddCertID(const ACertID: TOCSPCertID);
    procedure GenerateNonce;

    function Encode: TBytes;

    property UseNonce: Boolean read FUseNonce write FUseNonce;
    property Nonce: TBytes read FNonce;
  end;

  // ========================================================================
  // OCSP 响应
  // ========================================================================
  TOCSPResponse = class
  private
    FResponseStatus: TOCSPResponseStatus;
    FResponses: TOCSPSingleResponseArray;
    FProducedAt: TDateTime;
    FResponderID: string;
    FNonce: TBytes;
    FRawResponse: TBytes;

    procedure ParseBasicOCSPResponse(ANode: TASN1Node);
    procedure ParseResponseData(ANode: TASN1Node);
    procedure ParseSingleResponse(ANode: TASN1Node; var AResp: TOCSPSingleResponse);
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromDER(const AData: TBytes);

    function GetCertStatus(const ACertID: TOCSPCertID): TOCSPCertStatus;
    function FindResponse(const ACertID: TOCSPCertID): Integer;

    property ResponseStatus: TOCSPResponseStatus read FResponseStatus;
    property Responses: TOCSPSingleResponseArray read FResponses;
    property ProducedAt: TDateTime read FProducedAt;
    property ResponderID: string read FResponderID;
    property Nonce: TBytes read FNonce;
  end;

// ========================================================================
// 辅助函数
// ========================================================================

// 从证书中提取 OCSP URL
function GetOCSPURLFromCertificate(ACert: TX509Certificate): string;

// 比较两个 CertID 是否相同
function CompareCertID(const A, B: TOCSPCertID): Boolean;

// OCSP 状态转字符串
function OCSPStatusToString(AStatus: TOCSPCertStatus): string;
function OCSPResponseStatusToString(AStatus: TOCSPResponseStatus): string;

implementation

// ========================================================================
// OID 常量
// ========================================================================
const
  OID_OCSP_BASIC = '1.3.6.1.5.5.7.48.1.1';  // id-pkix-ocsp-basic
  OID_OCSP_NONCE = '1.3.6.1.5.5.7.48.1.2';  // id-pkix-ocsp-nonce
  OID_AIA = '1.3.6.1.5.5.7.1.1';            // Authority Info Access
  OID_OCSP = '1.3.6.1.5.5.7.48.1';          // OCSP access method

// ========================================================================
// TOCSPCertID
// ========================================================================

function TOCSPCertID.Encode: TBytes;
var
  Writer: TASN1Writer;
  AlgOID, AlgParams: TBytes;
begin
  Writer := TASN1Writer.Create;
  try
    // CertID ::= SEQUENCE {
    //   hashAlgorithm       AlgorithmIdentifier,
    //   issuerNameHash      OCTET STRING,
    //   issuerKeyHash       OCTET STRING,
    //   serialNumber        CertificateSerialNumber }

    // 简化实现：直接构建 DER
    // AlgorithmIdentifier
    AlgOID := EncodeOID(HashAlgorithm);

    // 构建完整结构
    // TODO: 完整实现

    Result := Writer.GetData;
  finally
    Writer.Free;
  end;
end;

class function TOCSPCertID.Decode(ANode: TASN1Node): TOCSPCertID;
var
  AlgNode: TASN1Node;
begin
  FillChar(Result, SizeOf(Result), 0);

  if not ANode.IsSequence then
    Exit;
  if ANode.ChildCount < 4 then
    Exit;

  // hashAlgorithm
  AlgNode := ANode.GetChild(0);
  if AlgNode.IsSequence and (AlgNode.ChildCount >= 1) then
    Result.HashAlgorithm := AlgNode.GetChild(0).AsOID;

  // issuerNameHash
  if ANode.GetChild(1).IsOctetString then
    Result.IssuerNameHash := ANode.GetChild(1).AsOctetString;

  // issuerKeyHash
  if ANode.GetChild(2).IsOctetString then
    Result.IssuerKeyHash := ANode.GetChild(2).AsOctetString;

  // serialNumber
  Result.SerialNumber := ANode.GetChild(3).AsBigInteger;
end;

class function TOCSPCertID.Create(ACert, AIssuerCert: TX509Certificate;
  AHashAlg: THashAlgorithm): TOCSPCertID;
var
  IssuerNameDER: TBytes;
  IssuerKeyDER: TBytes;
begin
  FillChar(Result, SizeOf(Result), 0);

  // 设置哈希算法 OID
  case AHashAlg of
    haSHA1:   Result.HashAlgorithm := '1.3.14.3.2.26';
    haSHA256: Result.HashAlgorithm := '2.16.840.1.101.3.4.2.1';
    haSHA384: Result.HashAlgorithm := '2.16.840.1.101.3.4.2.2';
    haSHA512: Result.HashAlgorithm := '2.16.840.1.101.3.4.2.3';
  else
    Result.HashAlgorithm := '1.3.14.3.2.26'; // 默认 SHA-1
  end;

  // 计算颁发者名称哈希
  // 注意：需要使用 DER 编码的颁发者名称
  // 这里简化处理，使用证书中的原始数据
  // TODO: 从 AIssuerCert 提取 Subject 的 DER 编码

  // 计算颁发者公钥哈希
  // 需要使用 SubjectPublicKeyInfo 中的公钥位串
  IssuerKeyDER := AIssuerCert.PublicKeyInfo.PublicKey;

  case AHashAlg of
    haSHA1:
    begin
      Result.IssuerNameHash := SHA1(TEncoding.UTF8.GetBytes(AIssuerCert.Subject.ToString));
      Result.IssuerKeyHash := SHA1(IssuerKeyDER);
    end;
    haSHA256:
    begin
      Result.IssuerNameHash := SHA256(TEncoding.UTF8.GetBytes(AIssuerCert.Subject.ToString));
      Result.IssuerKeyHash := SHA256(IssuerKeyDER);
    end;
  else
    Result.IssuerNameHash := SHA1(TEncoding.UTF8.GetBytes(AIssuerCert.Subject.ToString));
    Result.IssuerKeyHash := SHA1(IssuerKeyDER);
  end;

  // 复制序列号
  Result.SerialNumber := Copy(ACert.SerialNumber, 0, Length(ACert.SerialNumber));
end;

// ========================================================================
// TOCSPRequest
// ========================================================================

constructor TOCSPRequest.Create;
begin
  inherited Create;
  SetLength(FCertIDs, 0);
  SetLength(FNonce, 0);
  FUseNonce := True;
end;

destructor TOCSPRequest.Destroy;
begin
  inherited Destroy;
end;

procedure TOCSPRequest.AddCertificate(ACert, AIssuerCert: TX509Certificate);
begin
  AddCertID(TOCSPCertID.Create(ACert, AIssuerCert));
end;

procedure TOCSPRequest.AddCertID(const ACertID: TOCSPCertID);
begin
  SetLength(FCertIDs, Length(FCertIDs) + 1);
  FCertIDs[High(FCertIDs)] := ACertID;
end;

procedure TOCSPRequest.GenerateNonce;
var
  I: Integer;
begin
  SetLength(FNonce, 16);
  for I := 0 to 15 do
    FNonce[I] := Random(256);
end;

function TOCSPRequest.Encode: TBytes;
var
  Writer: TASN1Writer;
  I: Integer;
begin
  // OCSPRequest ::= SEQUENCE {
  //   tbsRequest     TBSRequest,
  //   optionalSignature [0] EXPLICIT Signature OPTIONAL }
  //
  // TBSRequest ::= SEQUENCE {
  //   version            [0] EXPLICIT Version DEFAULT v1,
  //   requestorName      [1] EXPLICIT GeneralName OPTIONAL,
  //   requestList        SEQUENCE OF Request,
  //   requestExtensions  [2] EXPLICIT Extensions OPTIONAL }
  //
  // Request ::= SEQUENCE {
  //   reqCert     CertID,
  //   singleRequestExtensions [0] EXPLICIT Extensions OPTIONAL }

  Writer := TASN1Writer.Create;
  try
    // 简化实现：只包含基本请求
    // 完整实现需要手动构建 DER 结构

    // 生成 nonce
    if FUseNonce and (Length(FNonce) = 0) then
      GenerateNonce;

    // TODO: 完整的 DER 编码
    // 这里返回空数据，实际使用时需要完整实现

    Result := Writer.GetData;
  finally
    Writer.Free;
  end;
end;

// ========================================================================
// TOCSPResponse
// ========================================================================

constructor TOCSPResponse.Create;
begin
  inherited Create;
  FResponseStatus := ocsprsInternalError;
  SetLength(FResponses, 0);
  FProducedAt := 0;
  FResponderID := '';
  SetLength(FNonce, 0);
end;

destructor TOCSPResponse.Destroy;
begin
  inherited Destroy;
end;

procedure TOCSPResponse.LoadFromDER(const AData: TBytes);
var
  Reader: TASN1Reader;
  Root, StatusNode, BytesNode, ResponseNode: TASN1Node;
  ResponseBytes: TBytes;
  ResponseOID: string;
begin
  FRawResponse := Copy(AData, 0, Length(AData));

  Reader := TASN1Reader.Create(AData);
  try
    Root := Reader.Parse;
    if Root = nil then
      Exit;

    try
      // OCSPResponse ::= SEQUENCE {
      //   responseStatus   OCSPResponseStatus,
      //   responseBytes    [0] EXPLICIT ResponseBytes OPTIONAL }

      if not Root.IsSequence then
        Exit;

      // responseStatus
      if Root.ChildCount >= 1 then
      begin
        StatusNode := Root.GetChild(0);
        case StatusNode.AsInteger of
          0: FResponseStatus := ocsprsSuccessful;
          1: FResponseStatus := ocsprsMalformedRequest;
          2: FResponseStatus := ocsprsInternalError;
          3: FResponseStatus := ocsprsTryLater;
          5: FResponseStatus := ocsprsSignatureRequired;
          6: FResponseStatus := ocsprsUnauthorized;
        end;
      end;

      // responseBytes (可选)
      if (Root.ChildCount >= 2) and Root.GetChild(1).IsContextTag(0) then
      begin
        BytesNode := Root.GetChild(1);
        if BytesNode.ChildCount >= 1 then
        begin
          ResponseNode := BytesNode.GetChild(0);
          if ResponseNode.IsSequence and (ResponseNode.ChildCount >= 2) then
          begin
            // ResponseBytes ::= SEQUENCE {
            //   responseType   OBJECT IDENTIFIER,
            //   response       OCTET STRING }
            ResponseOID := ResponseNode.GetChild(0).AsOID;
            if ResponseOID = OID_OCSP_BASIC then
            begin
              ResponseBytes := ResponseNode.GetChild(1).AsOctetString;
              // 解析 BasicOCSPResponse
              ParseBasicOCSPResponse(ResponseNode.GetChild(1));
            end;
          end;
        end;
      end;
    finally
      Root.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TOCSPResponse.ParseBasicOCSPResponse(ANode: TASN1Node);
var
  Reader: TASN1Reader;
  BasicRoot, TBSNode: TASN1Node;
  BasicData: TBytes;
begin
  // 如果是 OCTET STRING，需要再解析一次
  if ANode.IsOctetString then
    BasicData := ANode.AsOctetString
  else
    Exit;

  Reader := TASN1Reader.Create(BasicData);
  try
    BasicRoot := Reader.Parse;
    if BasicRoot = nil then
      Exit;

    try
      // BasicOCSPResponse ::= SEQUENCE {
      //   tbsResponseData   ResponseData,
      //   signatureAlgorithm AlgorithmIdentifier,
      //   signature          BIT STRING,
      //   certs              [0] EXPLICIT SEQUENCE OF Certificate OPTIONAL }

      if not BasicRoot.IsSequence then
        Exit;

      if BasicRoot.ChildCount >= 1 then
      begin
        TBSNode := BasicRoot.GetChild(0);
        ParseResponseData(TBSNode);
      end;
    finally
      BasicRoot.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TOCSPResponse.ParseResponseData(ANode: TASN1Node);
var
  I, Index: Integer;
  Child, ResponsesNode, SingleNode: TASN1Node;
  SingleResp: TOCSPSingleResponse;
begin
  // ResponseData ::= SEQUENCE {
  //   version            [0] EXPLICIT Version DEFAULT v1,
  //   responderID        ResponderID,
  //   producedAt         GeneralizedTime,
  //   responses          SEQUENCE OF SingleResponse,
  //   responseExtensions [1] EXPLICIT Extensions OPTIONAL }

  if not ANode.IsSequence then
    Exit;

  Index := 0;

  // version (可选)
  if (ANode.ChildCount > Index) and ANode.GetChild(Index).IsContextTag(0) then
    Inc(Index);

  // responderID
  if ANode.ChildCount > Index then
  begin
    Child := ANode.GetChild(Index);
    if Child.IsContextTag(1) then
    begin
      // byName
      if Child.ChildCount > 0 then
        FResponderID := 'byName'; // TODO: 解析名称
    end
    else if Child.IsContextTag(2) then
    begin
      // byKey
      FResponderID := 'byKey';
    end;
    Inc(Index);
  end;

  // producedAt
  if ANode.ChildCount > Index then
  begin
    FProducedAt := ANode.GetChild(Index).AsDateTime;
    Inc(Index);
  end;

  // responses
  if ANode.ChildCount > Index then
  begin
    ResponsesNode := ANode.GetChild(Index);
    if ResponsesNode.IsSequence then
    begin
      SetLength(FResponses, ResponsesNode.ChildCount);
      for I := 0 to ResponsesNode.ChildCount - 1 do
      begin
        FillChar(FResponses[I], SizeOf(TOCSPSingleResponse), 0);
        ParseSingleResponse(ResponsesNode.GetChild(I), FResponses[I]);
      end;
    end;
    Inc(Index);
  end;

  // responseExtensions (可选)
  if (ANode.ChildCount > Index) and ANode.GetChild(Index).IsContextTag(1) then
  begin
    // 解析扩展 (如 nonce)
    Child := ANode.GetChild(Index);
    // TODO: 解析 nonce 扩展
  end;
end;

procedure TOCSPResponse.ParseSingleResponse(ANode: TASN1Node; var AResp: TOCSPSingleResponse);
var
  Index: Integer;
  CertStatusNode, RevokedNode: TASN1Node;
begin
  // SingleResponse ::= SEQUENCE {
  //   certID           CertID,
  //   certStatus       CertStatus,
  //   thisUpdate       GeneralizedTime,
  //   nextUpdate       [0] EXPLICIT GeneralizedTime OPTIONAL,
  //   singleExtensions [1] EXPLICIT Extensions OPTIONAL }

  if not ANode.IsSequence then
    Exit;

  Index := 0;

  // certID
  if ANode.ChildCount > Index then
  begin
    AResp.CertID := TOCSPCertID.Decode(ANode.GetChild(Index));
    Inc(Index);
  end;

  // certStatus
  if ANode.ChildCount > Index then
  begin
    CertStatusNode := ANode.GetChild(Index);

    // CertStatus ::= CHOICE {
    //   good        [0] IMPLICIT NULL,
    //   revoked     [1] IMPLICIT RevokedInfo,
    //   unknown     [2] IMPLICIT UnknownInfo }

    if CertStatusNode.IsContextTag(0) then
      AResp.CertStatus := ocspGood
    else if CertStatusNode.IsContextTag(1) then
    begin
      AResp.CertStatus := ocspRevoked;
      // RevokedInfo ::= SEQUENCE {
      //   revocationTime     GeneralizedTime,
      //   revocationReason   [0] EXPLICIT CRLReason OPTIONAL }
      if CertStatusNode.ChildCount >= 1 then
        AResp.RevokedTime := CertStatusNode.GetChild(0).AsDateTime;
      if CertStatusNode.ChildCount >= 2 then
        AResp.RevokeReason := TOCSPRevokeReason(CertStatusNode.GetChild(1).AsInteger);
    end
    else if CertStatusNode.IsContextTag(2) then
      AResp.CertStatus := ocspUnknown;

    Inc(Index);
  end;

  // thisUpdate
  if ANode.ChildCount > Index then
  begin
    AResp.ThisUpdate := ANode.GetChild(Index).AsDateTime;
    Inc(Index);
  end;

  // nextUpdate (可选)
  AResp.HasNextUpdate := False;
  if (ANode.ChildCount > Index) and ANode.GetChild(Index).IsContextTag(0) then
  begin
    if ANode.GetChild(Index).ChildCount > 0 then
    begin
      AResp.NextUpdate := ANode.GetChild(Index).GetChild(0).AsDateTime;
      AResp.HasNextUpdate := True;
    end;
    Inc(Index);
  end;
end;

function TOCSPResponse.GetCertStatus(const ACertID: TOCSPCertID): TOCSPCertStatus;
var
  Idx: Integer;
begin
  Idx := FindResponse(ACertID);
  if Idx >= 0 then
    Result := FResponses[Idx].CertStatus
  else
    Result := ocspUnknown;
end;

function TOCSPResponse.FindResponse(const ACertID: TOCSPCertID): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FResponses) do
  begin
    if CompareCertID(FResponses[I].CertID, ACertID) then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

// ========================================================================
// 辅助函数实现
// ========================================================================

function GetOCSPURLFromCertificate(ACert: TX509Certificate): string;
var
  I, J: Integer;
  Ext: TX509Extension;
  Reader: TASN1Reader;
  Node, SeqNode, AccessNode: TASN1Node;
  AccessMethod, AccessLocation: string;
begin
  Result := '';

  // 查找 Authority Information Access 扩展
  for I := 0 to High(ACert.Extensions) do
  begin
    Ext := ACert.Extensions[I];
    if Ext.OID = OID_AIA then
    begin
      // 解析 AIA 扩展
      if Length(Ext.Value) = 0 then
        Continue;

      Reader := TASN1Reader.Create(Ext.Value);
      try
        Node := Reader.Parse;
        if Node = nil then
          Continue;

        try
          // AuthorityInfoAccessSyntax ::= SEQUENCE SIZE (1..MAX) OF AccessDescription
          // AccessDescription ::= SEQUENCE {
          //   accessMethod    OBJECT IDENTIFIER,
          //   accessLocation  GeneralName }
          if not Node.IsSequence then
            Continue;

          for J := 0 to Node.ChildCount - 1 do
          begin
            AccessNode := Node.GetChild(J);
            if not AccessNode.IsSequence then
              Continue;
            if AccessNode.ChildCount < 2 then
              Continue;

            AccessMethod := AccessNode.GetChild(0).AsOID;
            if AccessMethod = OID_OCSP then
            begin
              // accessLocation 是 GeneralName
              // uniformResourceIdentifier [6] IA5String
              if AccessNode.GetChild(1).IsContextTag(6) then
              begin
                Result := AccessNode.GetChild(1).AsString;
                Exit;
              end;
            end;
          end;
        finally
          Node.Free;
        end;
      finally
        Reader.Free;
      end;
    end;
  end;
end;

function CompareCertID(const A, B: TOCSPCertID): Boolean;
var
  I: Integer;
begin
  Result := False;

  // 比较哈希算法
  if A.HashAlgorithm <> B.HashAlgorithm then
    Exit;

  // 比较颁发者名称哈希
  if Length(A.IssuerNameHash) <> Length(B.IssuerNameHash) then
    Exit;
  for I := 0 to High(A.IssuerNameHash) do
    if A.IssuerNameHash[I] <> B.IssuerNameHash[I] then
      Exit;

  // 比较颁发者公钥哈希
  if Length(A.IssuerKeyHash) <> Length(B.IssuerKeyHash) then
    Exit;
  for I := 0 to High(A.IssuerKeyHash) do
    if A.IssuerKeyHash[I] <> B.IssuerKeyHash[I] then
      Exit;

  // 比较序列号
  if Length(A.SerialNumber) <> Length(B.SerialNumber) then
    Exit;
  for I := 0 to High(A.SerialNumber) do
    if A.SerialNumber[I] <> B.SerialNumber[I] then
      Exit;

  Result := True;
end;

function OCSPStatusToString(AStatus: TOCSPCertStatus): string;
begin
  case AStatus of
    ocspGood:    Result := 'Good';
    ocspRevoked: Result := 'Revoked';
    ocspUnknown: Result := 'Unknown';
  else
    Result := 'Invalid';
  end;
end;

function OCSPResponseStatusToString(AStatus: TOCSPResponseStatus): string;
begin
  case AStatus of
    ocsprsSuccessful:        Result := 'Successful';
    ocsprsMalformedRequest:  Result := 'Malformed Request';
    ocsprsInternalError:     Result := 'Internal Error';
    ocsprsTryLater:          Result := 'Try Later';
    ocsprsSignatureRequired: Result := 'Signature Required';
    ocsprsUnauthorized:      Result := 'Unauthorized';
  else
    Result := 'Unknown';
  end;
end;

end.
