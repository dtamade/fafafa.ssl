{
  Test Audit Crypto - 加密功能审计器

  检查加密相关函数的测试质量
}
unit test_audit_crypto;

{$mode objfpc}{$H+}{$J-}

interface

uses
  SysUtils, Classes, test_audit_types;

type
  { 加密功能审计器 }
  TCryptoAuditor = class
  private
    FVerbose: Boolean;
    FFunctions: TFunctionInfoArray;
    FTestCases: TTestCaseInfoArray;
    
    function IsCryptoFunction(const AFunc: TFunctionInfo): Boolean;
    function GetCryptoCategory(const AFuncName: string): string;
    function HasKATTest(const AFuncName: string): Boolean;
    function HasRoundTripTest(const AFuncName: string): Boolean;
    function HasVectorTest(const AFuncName: string): Boolean;
  public
    constructor Create;
    
    procedure SetData(const AFunctions: TFunctionInfoArray; const ATestCases: TTestCaseInfoArray);
    function CheckKnownAnswerTests: TCryptoTestGapArray;
    function CheckRoundTripTests: TCryptoTestGapArray;
    function AuditAllCryptoFunctions: TCryptoTestGapArray;
    function GetCryptoScore: Integer;
    
    property Verbose: Boolean read FVerbose write FVerbose;
  end;

implementation

constructor TCryptoAuditor.Create;
begin
  inherited Create;
  FVerbose := False;
end;

procedure TCryptoAuditor.SetData(const AFunctions: TFunctionInfoArray; const ATestCases: TTestCaseInfoArray);
begin
  FFunctions := AFunctions;
  FTestCases := ATestCases;
end;

function TCryptoAuditor.IsCryptoFunction(const AFunc: TFunctionInfo): Boolean;
var
  LowerName, LowerUnit: string;
begin
  LowerName := LowerCase(AFunc.Name);
  LowerUnit := LowerCase(AFunc.UnitName);
  
  Result := (Pos('encrypt', LowerName) > 0) or
            (Pos('decrypt', LowerName) > 0) or
            (Pos('hash', LowerName) > 0) or
            (Pos('sign', LowerName) > 0) or
            (Pos('verify', LowerName) > 0) or
            (Pos('cipher', LowerName) > 0) or
            (Pos('aes', LowerName) > 0) or
            (Pos('rsa', LowerName) > 0) or
            (Pos('sha', LowerName) > 0) or
            (Pos('md5', LowerName) > 0) or
            (Pos('hmac', LowerName) > 0) or
            (Pos('pbkdf', LowerName) > 0) or
            (Pos('key', LowerName) > 0) or
            (Pos('crypto', LowerUnit) > 0) or
            (Pos('cipher', LowerUnit) > 0) or
            (Pos('hash', LowerUnit) > 0);
end;

function TCryptoAuditor.GetCryptoCategory(const AFuncName: string): string;
var
  LowerName: string;
begin
  LowerName := LowerCase(AFuncName);
  
  if (Pos('aes', LowerName) > 0) or (Pos('encrypt', LowerName) > 0) or (Pos('decrypt', LowerName) > 0) then
    Result := 'Symmetric Encryption'
  else if (Pos('rsa', LowerName) > 0) or (Pos('ecdsa', LowerName) > 0) then
    Result := 'Asymmetric Encryption'
  else if (Pos('sha', LowerName) > 0) or (Pos('md5', LowerName) > 0) or (Pos('hash', LowerName) > 0) then
    Result := 'Hash Function'
  else if (Pos('sign', LowerName) > 0) or (Pos('verify', LowerName) > 0) then
    Result := 'Digital Signature'
  else if (Pos('hmac', LowerName) > 0) then
    Result := 'MAC'
  else if (Pos('key', LowerName) > 0) then
    Result := 'Key Management'
  else
    Result := 'Cryptographic';
end;

function TCryptoAuditor.HasKATTest(const AFuncName: string): Boolean;
var
  I: Integer;
  LowerFunc, LowerTest: string;
begin
  Result := False;
  LowerFunc := LowerCase(AFuncName);
  
  for I := 0 to High(FTestCases) do
  begin
    LowerTest := LowerCase(FTestCases[I].Name);
    if (Pos(LowerFunc, LowerTest) > 0) and
       ((Pos('kat', LowerTest) > 0) or
        (Pos('vector', LowerTest) > 0) or
        (Pos('known', LowerTest) > 0) or
        (Pos('nist', LowerTest) > 0) or
        (Pos('rfc', LowerTest) > 0)) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TCryptoAuditor.HasRoundTripTest(const AFuncName: string): Boolean;
var
  I: Integer;
  LowerFunc, LowerTest: string;
begin
  Result := False;
  LowerFunc := LowerCase(AFuncName);
  
  for I := 0 to High(FTestCases) do
  begin
    LowerTest := LowerCase(FTestCases[I].Name);
    if (Pos(LowerFunc, LowerTest) > 0) and
       ((Pos('roundtrip', LowerTest) > 0) or
        (Pos('round_trip', LowerTest) > 0) or
        (Pos('encryptdecrypt', LowerTest) > 0) or
        (Pos('signverify', LowerTest) > 0)) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TCryptoAuditor.HasVectorTest(const AFuncName: string): Boolean;
var
  I: Integer;
  LowerFunc, LowerTest: string;
begin
  Result := False;
  LowerFunc := LowerCase(AFuncName);
  
  for I := 0 to High(FTestCases) do
  begin
    LowerTest := LowerCase(FTestCases[I].Name);
    if (Pos(LowerFunc, LowerTest) > 0) and
       ((Pos('vector', LowerTest) > 0) or
        (Pos('testvec', LowerTest) > 0)) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TCryptoAuditor.CheckKnownAnswerTests: TCryptoTestGapArray;
var
  I, GapCount: Integer;
begin
  SetLength(Result, 0);
  GapCount := 0;
  
  for I := 0 to High(FFunctions) do
  begin
    if IsCryptoFunction(FFunctions[I]) and not HasKATTest(FFunctions[I].Name) then
    begin
      SetLength(Result, GapCount + 1);
      Result[GapCount].FunctionName := FFunctions[I].Name;
      Result[GapCount].UnitName := FFunctions[I].UnitName;
      Result[GapCount].GapType := cgtMissingKAT;
      Result[GapCount].StandardReference := 'NIST/RFC';
      Result[GapCount].Recommendation := Format('Add KAT test with standard vectors for %s', [FFunctions[I].Name]);
      Inc(GapCount);
    end;
  end;
end;

function TCryptoAuditor.CheckRoundTripTests: TCryptoTestGapArray;
var
  I, GapCount: Integer;
  LowerName: string;
begin
  SetLength(Result, 0);
  GapCount := 0;
  
  for I := 0 to High(FFunctions) do
  begin
    LowerName := LowerCase(FFunctions[I].Name);
    if IsCryptoFunction(FFunctions[I]) and
       ((Pos('encrypt', LowerName) > 0) or (Pos('sign', LowerName) > 0)) and
       not HasRoundTripTest(FFunctions[I].Name) then
    begin
      SetLength(Result, GapCount + 1);
      Result[GapCount].FunctionName := FFunctions[I].Name;
      Result[GapCount].UnitName := FFunctions[I].UnitName;
      Result[GapCount].GapType := cgtMissingRoundTrip;
      Result[GapCount].StandardReference := '';
      Result[GapCount].Recommendation := Format('Add round-trip test for %s', [FFunctions[I].Name]);
      Inc(GapCount);
    end;
  end;
end;

function TCryptoAuditor.AuditAllCryptoFunctions: TCryptoTestGapArray;
var
  KATGaps, RoundTripGaps: TCryptoTestGapArray;
  I, TotalCount: Integer;
begin
  SetLength(Result, 0);
  TotalCount := 0;
  
  KATGaps := CheckKnownAnswerTests;
  RoundTripGaps := CheckRoundTripTests;
  
  for I := 0 to High(KATGaps) do
  begin
    SetLength(Result, TotalCount + 1);
    Result[TotalCount] := KATGaps[I];
    Inc(TotalCount);
  end;
  
  for I := 0 to High(RoundTripGaps) do
  begin
    SetLength(Result, TotalCount + 1);
    Result[TotalCount] := RoundTripGaps[I];
    Inc(TotalCount);
  end;
  
  if FVerbose then
    WriteLn(Format('  Found %d crypto test gaps', [TotalCount]));
end;

function TCryptoAuditor.GetCryptoScore: Integer;
var
  CryptoFuncs, GapsCount, I: Integer;
begin
  CryptoFuncs := 0;
  for I := 0 to High(FFunctions) do
    if IsCryptoFunction(FFunctions[I]) then
      Inc(CryptoFuncs);
  
  GapsCount := Length(AuditAllCryptoFunctions);
  
  if CryptoFuncs = 0 then
    Result := 100
  else
    Result := Round(100 - (GapsCount / CryptoFuncs * 50));
  
  if Result < 0 then Result := 0;
  if Result > 100 then Result := 100;
end;

end.
