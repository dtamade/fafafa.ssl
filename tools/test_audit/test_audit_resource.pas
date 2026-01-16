{
  Test Audit Resource Management - 资源管理审计器

  检查资源管理相关的测试覆盖
}
unit test_audit_resource;

{$mode objfpc}{$H+}{$J-}

interface

uses
  SysUtils, Classes, test_audit_types;

type
  { 资源管理审计器 }
  TResourceAuditor = class
  private
    FVerbose: Boolean;
    FFunctions: TFunctionInfoArray;
    FTestCases: TTestCaseInfoArray;
    
    function IsResourceFunction(const AFunc: TFunctionInfo): Boolean;
    function GetResourceType(const AFuncName: string): string;
    function HasLeakTest(const AFuncName: string): Boolean;
    function HasCleanupTest(const AFuncName: string): Boolean;
  public
    constructor Create;
    
    procedure SetData(const AFunctions: TFunctionInfoArray; const ATestCases: TTestCaseInfoArray);
    function AuditAllFunctions: TResourceTestGapArray;
    function GetResourceScore: Integer;
    
    property Verbose: Boolean read FVerbose write FVerbose;
  end;

implementation

constructor TResourceAuditor.Create;
begin
  inherited Create;
  FVerbose := False;
end;

procedure TResourceAuditor.SetData(const AFunctions: TFunctionInfoArray; const ATestCases: TTestCaseInfoArray);
begin
  FFunctions := AFunctions;
  FTestCases := ATestCases;
end;

function TResourceAuditor.IsResourceFunction(const AFunc: TFunctionInfo): Boolean;
var
  LowerName: string;
begin
  LowerName := LowerCase(AFunc.Name);
  
  Result := (Pos('create', LowerName) > 0) or
            (Pos('destroy', LowerName) > 0) or
            (Pos('free', LowerName) > 0) or
            (Pos('alloc', LowerName) > 0) or
            (Pos('release', LowerName) > 0) or
            (Pos('open', LowerName) > 0) or
            (Pos('close', LowerName) > 0) or
            (Pos('init', LowerName) > 0) or
            (Pos('finalize', LowerName) > 0) or
            (Pos('dispose', LowerName) > 0) or
            (Pos('handle', LowerName) > 0) or
            (Pos('context', LowerName) > 0) or
            AFunc.IsConstructor or
            AFunc.IsDestructor;
end;

function TResourceAuditor.GetResourceType(const AFuncName: string): string;
var
  LowerName: string;
begin
  LowerName := LowerCase(AFuncName);
  
  if (Pos('memory', LowerName) > 0) or (Pos('alloc', LowerName) > 0) then
    Result := 'Memory'
  else if (Pos('file', LowerName) > 0) or (Pos('stream', LowerName) > 0) then
    Result := 'File/Stream'
  else if (Pos('handle', LowerName) > 0) then
    Result := 'Handle'
  else if (Pos('context', LowerName) > 0) or (Pos('ctx', LowerName) > 0) then
    Result := 'Context'
  else if (Pos('socket', LowerName) > 0) or (Pos('connection', LowerName) > 0) then
    Result := 'Network'
  else
    Result := 'Resource';
end;

function TResourceAuditor.HasLeakTest(const AFuncName: string): Boolean;
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
       ((Pos('leak', LowerTest) > 0) or
        (Pos('memory', LowerTest) > 0)) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TResourceAuditor.HasCleanupTest(const AFuncName: string): Boolean;
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
       ((Pos('cleanup', LowerTest) > 0) or
        (Pos('destroy', LowerTest) > 0) or
        (Pos('free', LowerTest) > 0) or
        (Pos('release', LowerTest) > 0)) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TResourceAuditor.AuditAllFunctions: TResourceTestGapArray;
var
  I, GapCount: Integer;
  LowerName: string;
begin
  SetLength(Result, 0);
  GapCount := 0;
  
  for I := 0 to High(FFunctions) do
  begin
    if IsResourceFunction(FFunctions[I]) then
    begin
      LowerName := LowerCase(FFunctions[I].Name);
      
      // Check for leak tests on allocation functions
      if ((Pos('create', LowerName) > 0) or (Pos('alloc', LowerName) > 0) or
          (Pos('init', LowerName) > 0) or FFunctions[I].IsConstructor) and
         not HasLeakTest(FFunctions[I].Name) then
      begin
        SetLength(Result, GapCount + 1);
        Result[GapCount].FunctionName := FFunctions[I].Name;
        Result[GapCount].UnitName := FFunctions[I].UnitName;
        Result[GapCount].GapType := rgtMissingLeakTest;
        Result[GapCount].ResourceType := GetResourceType(FFunctions[I].Name);
        Result[GapCount].Recommendation := Format('Add memory leak test for %s', [FFunctions[I].Name]);
        Inc(GapCount);
      end;
      
      // Check for cleanup tests on deallocation functions
      if ((Pos('destroy', LowerName) > 0) or (Pos('free', LowerName) > 0) or
          (Pos('finalize', LowerName) > 0) or FFunctions[I].IsDestructor) and
         not HasCleanupTest(FFunctions[I].Name) then
      begin
        SetLength(Result, GapCount + 1);
        Result[GapCount].FunctionName := FFunctions[I].Name;
        Result[GapCount].UnitName := FFunctions[I].UnitName;
        Result[GapCount].GapType := rgtMissingCleanupTest;
        Result[GapCount].ResourceType := GetResourceType(FFunctions[I].Name);
        Result[GapCount].Recommendation := Format('Add cleanup verification test for %s', [FFunctions[I].Name]);
        Inc(GapCount);
      end;
    end;
  end;
  
  if FVerbose then
    WriteLn(Format('  Found %d resource management gaps', [GapCount]));
end;

function TResourceAuditor.GetResourceScore: Integer;
var
  ResourceFuncs, GapsCount, I: Integer;
begin
  ResourceFuncs := 0;
  for I := 0 to High(FFunctions) do
    if IsResourceFunction(FFunctions[I]) then
      Inc(ResourceFuncs);
  
  GapsCount := Length(AuditAllFunctions);
  
  if ResourceFuncs = 0 then
    Result := 100
  else
    Result := Round(100 - (GapsCount / ResourceFuncs * 50));
  
  if Result < 0 then Result := 0;
  if Result > 100 then Result := 100;
end;

end.
