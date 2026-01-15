{
  Test Audit Test Scanner - 测试扫描器

  解析测试文件，提取测试用例信息并映射到源码函数
}
unit test_audit_test_scanner;

{$mode objfpc}{$H+}{$J-}

interface

uses
  SysUtils, Classes, Math, test_audit_types;

type
  { 测试到函数的映射 }
  TTestFunctionMap = record
    FunctionName: string;
    UnitName: string;
    TestCases: TStringArray;
    TestCount: Integer;
  end;
  TTestFunctionMapArray = array of TTestFunctionMap;

  { 测试扫描器 }
  TTestScanner = class
  private
    FVerbose: Boolean;
    FCurrentFile: string;
    
    function ParseLine(const ALine: string): string;
    function IsTestProcedure(const ALine: string; out ATestName: string): Boolean;
    function IsUsesClause(const ALine: string): Boolean;
    function ExtractUsesUnits(const AContent: string): TStringArray;
    function ExtractCalledFunctions(const AContent: string): TStringArray;
    function CountAssertions(const AContent: string): Integer;
    function HasSetupTeardown(const AContent: string; out AHasSetup, AHasTeardown: Boolean): Boolean;
    function DetermineTestType(const ATestName, AFileName: string): TTestType;
    function ExtractTestedFunction(const ATestName: string): string;
  public
    constructor Create;
    
    function ScanTestDirectory(const APath: string): TTestCaseInfoArray;
    function ScanTestFile(const AFilePath: string): TTestCaseInfoArray;
    function MapTestsToFunctions(const ATests: TTestCaseInfoArray;
      const AFunctions: TFunctionInfoArray): TTestFunctionMapArray;
    
    property Verbose: Boolean read FVerbose write FVerbose;
  end;

implementation

constructor TTestScanner.Create;
begin
  inherited Create;
  FVerbose := False;
end;

function TTestScanner.ParseLine(const ALine: string): string;
var
  CommentPos: Integer;
begin
  Result := ALine;
  
  // 移除 // 注释
  CommentPos := Pos('//', Result);
  if CommentPos > 0 then
    Result := Copy(Result, 1, CommentPos - 1);
    
  Result := Trim(Result);
end;

function TTestScanner.IsTestProcedure(const ALine: string; out ATestName: string): Boolean;
var
  Line: string;
  P: Integer;
begin
  Result := False;
  ATestName := '';
  Line := LowerCase(Trim(ALine));
  
  // 检查是否是测试过程
  if Pos('procedure ', Line) <> 1 then
    Exit;
    
  Line := Trim(Copy(ALine, 11, MaxInt));
  
  // 提取过程名
  P := Pos('(', Line);
  if P > 0 then
    ATestName := Trim(Copy(Line, 1, P - 1))
  else
  begin
    P := Pos(';', Line);
    if P > 0 then
      ATestName := Trim(Copy(Line, 1, P - 1))
    else
      ATestName := Trim(Line);
  end;
  
  // 检查是否是测试命名约定
  Result := (Pos('test', LowerCase(ATestName)) > 0) or
            (Pos('Test', ATestName) > 0);
end;

function TTestScanner.IsUsesClause(const ALine: string): Boolean;
var
  Line: string;
begin
  Line := LowerCase(Trim(ALine));
  Result := (Pos('uses', Line) = 1);
end;

function TTestScanner.ExtractUsesUnits(const AContent: string): TStringArray;
var
  StartPos, EndPos: Integer;
  UsesSection: string;
  Units: TStringList;
  I: Integer;
begin
  SetLength(Result, 0);
  
  StartPos := Pos('uses', LowerCase(AContent));
  if StartPos = 0 then
    Exit;
    
  EndPos := Pos(';', Copy(AContent, StartPos, MaxInt));
  if EndPos = 0 then
    Exit;
    
  UsesSection := Copy(AContent, StartPos + 4, EndPos - 5);
  
  Units := TStringList.Create;
  try
    Units.Delimiter := ',';
    Units.StrictDelimiter := True;
    Units.DelimitedText := UsesSection;
    
    SetLength(Result, Units.Count);
    for I := 0 to Units.Count - 1 do
      Result[I] := Trim(Units[I]);
  finally
    Units.Free;
  end;
end;

function TTestScanner.ExtractCalledFunctions(const AContent: string): TStringArray;
var
  I, P: Integer;
  Line: string;
  FuncName: string;
  Functions: TStringList;
  Lines: TStringList;
begin
  SetLength(Result, 0);
  Functions := TStringList.Create;
  Functions.Sorted := True;
  Functions.Duplicates := dupIgnore;
  
  Lines := TStringList.Create;
  try
    Lines.Text := AContent;
    
    for I := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[I]);
      
      // 查找函数调用模式: FunctionName( 或 Object.FunctionName(
      P := Pos('(', Line);
      while P > 0 do
      begin
        // 向前查找函数名
        FuncName := '';
        while (P > 1) and (Line[P-1] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '.']) do
        begin
          Dec(P);
          FuncName := Line[P] + FuncName;
        end;
        
        if FuncName <> '' then
        begin
          // 移除对象前缀
          if Pos('.', FuncName) > 0 then
            FuncName := Copy(FuncName, Pos('.', FuncName) + 1, MaxInt);
          Functions.Add(FuncName);
        end;
        
        // 继续查找下一个
        Line := Copy(Line, Pos('(', Line) + 1, MaxInt);
        P := Pos('(', Line);
      end;
    end;
    
    SetLength(Result, Functions.Count);
    for I := 0 to Functions.Count - 1 do
      Result[I] := Functions[I];
  finally
    Lines.Free;
    Functions.Free;
  end;
end;

function TTestScanner.CountAssertions(const AContent: string): Integer;
var
  LowerContent: string;
  P: Integer;
begin
  Result := 0;
  LowerContent := LowerCase(AContent);
  
  // 计算各种断言调用
  P := 1;
  while P <= Length(LowerContent) do
  begin
    if Pos('assert', Copy(LowerContent, P, 10)) = 1 then
    begin
      Inc(Result);
      Inc(P, 6);
    end
    else if Pos('check', Copy(LowerContent, P, 10)) = 1 then
    begin
      Inc(Result);
      Inc(P, 5);
    end
    else if Pos('expect', Copy(LowerContent, P, 10)) = 1 then
    begin
      Inc(Result);
      Inc(P, 6);
    end
    else if Pos('printresult', Copy(LowerContent, P, 15)) = 1 then
    begin
      Inc(Result);
      Inc(P, 11);
    end
    else
      Inc(P);
  end;
end;


function TTestScanner.HasSetupTeardown(const AContent: string; out AHasSetup, AHasTeardown: Boolean): Boolean;
var
  LowerContent: string;
begin
  LowerContent := LowerCase(AContent);
  
  AHasSetup := (Pos('setup', LowerContent) > 0) or
               (Pos('beforetest', LowerContent) > 0) or
               (Pos('initialize', LowerContent) > 0);
               
  AHasTeardown := (Pos('teardown', LowerContent) > 0) or
                  (Pos('aftertest', LowerContent) > 0) or
                  (Pos('cleanup', LowerContent) > 0) or
                  (Pos('finalize', LowerContent) > 0);
                  
  Result := AHasSetup or AHasTeardown;
end;

function TTestScanner.DetermineTestType(const ATestName, AFileName: string): TTestType;
var
  LowerName, LowerFile: string;
begin
  LowerName := LowerCase(ATestName);
  LowerFile := LowerCase(AFileName);
  
  // 基于文件路径判断
  if Pos('integration', LowerFile) > 0 then
    Result := ttIntegration
  else if Pos('performance', LowerFile) > 0 then
    Result := ttPerformance
  else if Pos('security', LowerFile) > 0 then
    Result := ttSecurity
  else if Pos('stress', LowerFile) > 0 then
    Result := ttStress
  else if Pos('fuzz', LowerFile) > 0 then
    Result := ttFuzz
  else if Pos('property', LowerFile) > 0 then
    Result := ttProperty
  // 基于测试名称判断
  else if Pos('property', LowerName) > 0 then
    Result := ttProperty
  else if Pos('boundary', LowerName) > 0 then
    Result := ttBoundary
  else if Pos('error', LowerName) > 0 then
    Result := ttError
  else if Pos('stress', LowerName) > 0 then
    Result := ttStress
  else if Pos('perf', LowerName) > 0 then
    Result := ttPerformance
  else if Pos('integration', LowerName) > 0 then
    Result := ttIntegration
  else
    Result := ttUnit;
end;

function TTestScanner.ExtractTestedFunction(const ATestName: string): string;
var
  Name: string;
  P: Integer;
begin
  Name := ATestName;
  
  // 移除 Test 前缀
  if Pos('Test', Name) = 1 then
    Name := Copy(Name, 5, MaxInt)
  else if Pos('test_', LowerCase(Name)) = 1 then
    Name := Copy(Name, 6, MaxInt)
  else if Pos('test', LowerCase(Name)) = 1 then
    Name := Copy(Name, 5, MaxInt);
    
  // 移除常见后缀
  P := Pos('_', Name);
  if P > 0 then
  begin
    // 检查是否是描述性后缀
    if (Pos('_empty', LowerCase(Name)) > 0) or
       (Pos('_null', LowerCase(Name)) > 0) or
       (Pos('_invalid', LowerCase(Name)) > 0) or
       (Pos('_valid', LowerCase(Name)) > 0) or
       (Pos('_error', LowerCase(Name)) > 0) or
       (Pos('_success', LowerCase(Name)) > 0) then
      Name := Copy(Name, 1, P - 1);
  end;
  
  Result := Name;
end;

function TTestScanner.ScanTestFile(const AFilePath: string): TTestCaseInfoArray;
var
  Lines: TStringList;
  Content: string;
  I, LineNum: Integer;
  Line, TestName: string;
  TestInfo: TTestCaseInfo;
  HasSetup, HasTeardown: Boolean;
begin
  SetLength(Result, 0);
  
  if not FileExists(AFilePath) then
    Exit;
    
  FCurrentFile := AFilePath;
  
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(AFilePath);
    Content := Lines.Text;
    
    // 提取 uses 子句
    TestInfo.UsesClause := ExtractUsesUnits(Content);
    
    // 检查 setup/teardown
    HasSetupTeardown(Content, HasSetup, HasTeardown);
    
    for I := 0 to Lines.Count - 1 do
    begin
      LineNum := I + 1;
      Line := ParseLine(Lines[I]);
      
      if Line = '' then
        Continue;
        
      // 检查测试过程
      if IsTestProcedure(Line, TestName) then
      begin
        TestInfo.Name := TestName;
        TestInfo.TestFile := AFilePath;
        TestInfo.TestedFunction := ExtractTestedFunction(TestName);
        TestInfo.TestType := DetermineTestType(TestName, AFilePath);
        TestInfo.HasSetup := HasSetup;
        TestInfo.HasTeardown := HasTeardown;
        TestInfo.LineNumber := LineNum;
        
        // 提取测试体内容来计算断言数
        // 简化处理：使用整个文件内容
        TestInfo.AssertionCount := CountAssertions(Content) div Max(1, Length(Result) + 1);
        TestInfo.CalledFunctions := ExtractCalledFunctions(Content);
        
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := TestInfo;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

function TTestScanner.ScanTestDirectory(const APath: string): TTestCaseInfoArray;
var
  SearchRec: TSearchRec;
  FilePath: string;
  FileTests: TTestCaseInfoArray;
  I: Integer;
  
  procedure ScanSubDir(const ASubPath: string);
  var
    SubSearchRec: TSearchRec;
    SubFilePath: string;
    SubTests: TTestCaseInfoArray;
    J: Integer;
  begin
    // 扫描子目录中的 .pas 文件
    if FindFirst(IncludeTrailingPathDelimiter(ASubPath) + '*.pas', faAnyFile, SubSearchRec) = 0 then
    begin
      try
        repeat
          if (SubSearchRec.Attr and faDirectory) = 0 then
          begin
            SubFilePath := IncludeTrailingPathDelimiter(ASubPath) + SubSearchRec.Name;
            
            if FVerbose then
              WriteLn('Scanning test: ', SubFilePath);
              
            SubTests := ScanTestFile(SubFilePath);
            
            for J := 0 to High(SubTests) do
            begin
              SetLength(Result, Length(Result) + 1);
              Result[High(Result)] := SubTests[J];
            end;
          end;
        until FindNext(SubSearchRec) <> 0;
      finally
        FindClose(SubSearchRec);
      end;
    end;
    
    // 递归扫描子目录
    if FindFirst(IncludeTrailingPathDelimiter(ASubPath) + '*', faDirectory, SubSearchRec) = 0 then
    begin
      try
        repeat
          if ((SubSearchRec.Attr and faDirectory) <> 0) and
             (SubSearchRec.Name <> '.') and (SubSearchRec.Name <> '..') then
          begin
            ScanSubDir(IncludeTrailingPathDelimiter(ASubPath) + SubSearchRec.Name);
          end;
        until FindNext(SubSearchRec) <> 0;
      finally
        FindClose(SubSearchRec);
      end;
    end;
  end;
  
begin
  SetLength(Result, 0);
  
  if not DirectoryExists(APath) then
    Exit;
    
  // 扫描根目录
  if FindFirst(IncludeTrailingPathDelimiter(APath) + '*.pas', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        if (SearchRec.Attr and faDirectory) = 0 then
        begin
          FilePath := IncludeTrailingPathDelimiter(APath) + SearchRec.Name;
          
          if FVerbose then
            WriteLn('Scanning test: ', FilePath);
            
          FileTests := ScanTestFile(FilePath);
          
          for I := 0 to High(FileTests) do
          begin
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)] := FileTests[I];
          end;
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
  
  // 扫描子目录
  if FindFirst(IncludeTrailingPathDelimiter(APath) + '*', faDirectory, SearchRec) = 0 then
  begin
    try
      repeat
        if ((SearchRec.Attr and faDirectory) <> 0) and
           (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          ScanSubDir(IncludeTrailingPathDelimiter(APath) + SearchRec.Name);
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
end;

function TTestScanner.MapTestsToFunctions(const ATests: TTestCaseInfoArray;
  const AFunctions: TFunctionInfoArray): TTestFunctionMapArray;
var
  I, J, K: Integer;
  Found: Boolean;
  MapEntry: TTestFunctionMap;
begin
  SetLength(Result, 0);
  
  // 为每个函数创建映射
  for I := 0 to High(AFunctions) do
  begin
    MapEntry.FunctionName := AFunctions[I].Name;
    MapEntry.UnitName := AFunctions[I].UnitName;
    SetLength(MapEntry.TestCases, 0);
    MapEntry.TestCount := 0;
    
    // 查找测试此函数的测试用例
    for J := 0 to High(ATests) do
    begin
      // 检查测试名称是否匹配
      if (LowerCase(ATests[J].TestedFunction) = LowerCase(AFunctions[I].Name)) or
         (Pos(LowerCase(AFunctions[I].Name), LowerCase(ATests[J].Name)) > 0) then
      begin
        SetLength(MapEntry.TestCases, Length(MapEntry.TestCases) + 1);
        MapEntry.TestCases[High(MapEntry.TestCases)] := ATests[J].Name;
        Inc(MapEntry.TestCount);
      end
      else
      begin
        // 检查调用的函数列表
        for K := 0 to High(ATests[J].CalledFunctions) do
        begin
          if LowerCase(ATests[J].CalledFunctions[K]) = LowerCase(AFunctions[I].Name) then
          begin
            SetLength(MapEntry.TestCases, Length(MapEntry.TestCases) + 1);
            MapEntry.TestCases[High(MapEntry.TestCases)] := ATests[J].Name;
            Inc(MapEntry.TestCount);
            Break;
          end;
        end;
      end;
    end;
    
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := MapEntry;
  end;
end;

end.
