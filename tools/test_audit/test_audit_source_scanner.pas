{
  Test Audit Source Scanner - 源码扫描器

  解析 Pascal 源码文件，提取公共函数和方法信息
}
unit test_audit_source_scanner;

{$mode objfpc}{$H+}{$J-}

interface

uses
  SysUtils, Classes, test_audit_types;

type
  { 源码扫描器 }
  TSourceScanner = class
  private
    FVerbose: Boolean;
    FCurrentUnit: string;
    FCurrentFile: string;
    FCurrentClass: string;
    FInInterface: Boolean;
    FInImplementation: Boolean;
    FInClass: Boolean;
    FCurrentVisibility: TVisibility;
    
    function ParseLine(const ALine: string): string;
    function IsUnitDeclaration(const ALine: string; out AUnitName: string): Boolean;
    function IsInterfaceSection(const ALine: string): Boolean;
    function IsImplementationSection(const ALine: string): Boolean;
    function IsClassDeclaration(const ALine: string; out AClassName: string): Boolean;
    function IsEndOfClass(const ALine: string): Boolean;
    function IsVisibilityKeyword(const ALine: string; out AVisibility: TVisibility): Boolean;
    function IsFunctionDeclaration(const ALine: string; out AFuncInfo: TFunctionInfo): Boolean;
    function IsProcedureDeclaration(const ALine: string; out AFuncInfo: TFunctionInfo): Boolean;
    function IsConstructorDeclaration(const ALine: string; out AFuncInfo: TFunctionInfo): Boolean;
    function IsDestructorDeclaration(const ALine: string; out AFuncInfo: TFunctionInfo): Boolean;
    function ParseParameters(const AParamStr: string): TParamInfoArray;
    function ClassifyParamType(const ATypeName: string): TParamCategory;
    function ExtractBetween(const AStr, AStart, AEnd: string): string;
    function TrimComment(const ALine: string): string;
  public
    constructor Create;
    
    function ScanDirectory(const APath: string): TFunctionInfoArray;
    function ScanUnit(const AFilePath: string): TFunctionInfoArray;
    function ExtractPublicAPI(const AFilePath: string): TFunctionInfoArray;
    
    property Verbose: Boolean read FVerbose write FVerbose;
  end;

implementation

constructor TSourceScanner.Create;
begin
  inherited Create;
  FVerbose := False;
end;

function TSourceScanner.TrimComment(const ALine: string): string;
var
  CommentPos: Integer;
begin
  Result := ALine;
  
  // 移除 // 注释
  CommentPos := Pos('//', Result);
  if CommentPos > 0 then
    Result := Copy(Result, 1, CommentPos - 1);
    
  // 移除 { } 注释 (简单处理，不处理跨行)
  CommentPos := Pos('{', Result);
  if CommentPos > 0 then
  begin
    if Pos('}', Result) > CommentPos then
      Result := Copy(Result, 1, CommentPos - 1) + 
                Copy(Result, Pos('}', Result) + 1, MaxInt)
    else
      Result := Copy(Result, 1, CommentPos - 1);
  end;
  
  // 移除 (* *) 注释 (简单处理)
  CommentPos := Pos('(*', Result);
  if CommentPos > 0 then
  begin
    if Pos('*)', Result) > CommentPos then
      Result := Copy(Result, 1, CommentPos - 1) + 
                Copy(Result, Pos('*)', Result) + 2, MaxInt)
    else
      Result := Copy(Result, 1, CommentPos - 1);
  end;
  
  Result := Trim(Result);
end;

function TSourceScanner.ParseLine(const ALine: string): string;
begin
  Result := TrimComment(ALine);
end;

function TSourceScanner.IsUnitDeclaration(const ALine: string; out AUnitName: string): Boolean;
var
  Line: string;
  P: Integer;
begin
  Result := False;
  AUnitName := '';
  Line := LowerCase(Trim(ALine));
  
  if Pos('unit ', Line) = 1 then
  begin
    Line := Trim(Copy(ALine, 6, MaxInt));
    P := Pos(';', Line);
    if P > 0 then
      AUnitName := Trim(Copy(Line, 1, P - 1))
    else
      AUnitName := Trim(Line);
    Result := AUnitName <> '';
  end;
end;

function TSourceScanner.IsInterfaceSection(const ALine: string): Boolean;
var
  Line: string;
begin
  Line := LowerCase(Trim(ALine));
  Result := (Line = 'interface') or (Pos('interface', Line) = 1);
end;

function TSourceScanner.IsImplementationSection(const ALine: string): Boolean;
var
  Line: string;
begin
  Line := LowerCase(Trim(ALine));
  Result := (Line = 'implementation') or (Pos('implementation', Line) = 1);
end;

function TSourceScanner.IsClassDeclaration(const ALine: string; out AClassName: string): Boolean;
var
  Line: string;
  P, EqPos: Integer;
begin
  Result := False;
  AClassName := '';
  Line := Trim(ALine);
  
  // 查找 = class 或 = class(
  EqPos := Pos('=', Line);
  if EqPos > 0 then
  begin
    P := Pos('class', LowerCase(Copy(Line, EqPos, MaxInt)));
    if P > 0 then
    begin
      AClassName := Trim(Copy(Line, 1, EqPos - 1));
      Result := AClassName <> '';
    end;
  end;
end;

function TSourceScanner.IsEndOfClass(const ALine: string): Boolean;
var
  Line: string;
begin
  Line := LowerCase(Trim(ALine));
  Result := (Line = 'end;') and FInClass;
end;

function TSourceScanner.IsVisibilityKeyword(const ALine: string; out AVisibility: TVisibility): Boolean;
var
  Line: string;
begin
  Result := False;
  Line := LowerCase(Trim(ALine));
  
  if Line = 'public' then
  begin
    AVisibility := vsPublic;
    Result := True;
  end
  else if Line = 'private' then
  begin
    AVisibility := vsPrivate;
    Result := True;
  end
  else if Line = 'protected' then
  begin
    AVisibility := vsProtected;
    Result := True;
  end
  else if Line = 'published' then
  begin
    AVisibility := vsPublished;
    Result := True;
  end;
end;


function TSourceScanner.ExtractBetween(const AStr, AStart, AEnd: string): string;
var
  StartPos, EndPos: Integer;
begin
  Result := '';
  StartPos := Pos(AStart, AStr);
  if StartPos > 0 then
  begin
    EndPos := Pos(AEnd, Copy(AStr, StartPos + Length(AStart), MaxInt));
    if EndPos > 0 then
      Result := Copy(AStr, StartPos + Length(AStart), EndPos - 1);
  end;
end;

function TSourceScanner.ClassifyParamType(const ATypeName: string): TParamCategory;
var
  LowerType: string;
begin
  LowerType := LowerCase(Trim(ATypeName));
  
  // 数值类型
  if (LowerType = 'integer') or (LowerType = 'int64') or (LowerType = 'cardinal') or
     (LowerType = 'longint') or (LowerType = 'shortint') or (LowerType = 'smallint') or
     (LowerType = 'byte') or (LowerType = 'word') or (LowerType = 'longword') or
     (LowerType = 'qword') or (LowerType = 'nativeint') or (LowerType = 'nativeuint') or
     (LowerType = 'single') or (LowerType = 'double') or (LowerType = 'extended') or
     (LowerType = 'real') or (LowerType = 'currency') or (LowerType = 'comp') or
     (LowerType = 'boolean') or (LowerType = 'bytebool') or (LowerType = 'wordbool') or
     (LowerType = 'longbool') or (LowerType = 'char') or (LowerType = 'ansichar') or
     (LowerType = 'widechar') then
    Result := pcNumeric
  // 字符串类型
  else if (LowerType = 'string') or (LowerType = 'ansistring') or (LowerType = 'widestring') or
          (LowerType = 'unicodestring') or (LowerType = 'shortstring') or
          (LowerType = 'pchar') or (LowerType = 'pansichar') or (LowerType = 'pwidechar') then
    Result := pcString
  // 数组类型
  else if (Pos('array', LowerType) > 0) or (LowerType = 'tbytes') or
          (Pos('array of', LowerType) > 0) then
    Result := pcArray
  // 指针类型
  else if (LowerType = 'pointer') or (LowerType[1] = 'p') or (Pos('^', LowerType) > 0) then
    Result := pcPointer
  // 接口类型
  else if (LowerType[1] = 'i') and (Length(LowerType) > 1) and (LowerType[2] in ['A'..'Z']) then
    Result := pcInterface
  // 类类型
  else if (LowerType[1] = 't') and (Length(LowerType) > 1) and (LowerType[2] in ['A'..'Z']) then
    Result := pcClass
  else
    Result := pcOther;
end;

function TSourceScanner.ParseParameters(const AParamStr: string): TParamInfoArray;
var
  Params: TStringList;
  I, J: Integer;
  ParamPart, ParamName, ParamType: string;
  Names: TStringList;
  Info: TParamInfo;
  IsVar, IsConst, IsOut: Boolean;
  ColonPos: Integer;
begin
  SetLength(Result, 0);
  if Trim(AParamStr) = '' then
    Exit;
    
  Params := TStringList.Create;
  Names := TStringList.Create;
  try
    Params.Delimiter := ';';
    Params.StrictDelimiter := True;
    Params.DelimitedText := AParamStr;
    
    for I := 0 to Params.Count - 1 do
    begin
      ParamPart := Trim(Params[I]);
      if ParamPart = '' then
        Continue;
        
      // 检查修饰符
      IsVar := False;
      IsConst := False;
      IsOut := False;
      
      if Pos('var ', LowerCase(ParamPart)) = 1 then
      begin
        IsVar := True;
        ParamPart := Trim(Copy(ParamPart, 5, MaxInt));
      end
      else if Pos('const ', LowerCase(ParamPart)) = 1 then
      begin
        IsConst := True;
        ParamPart := Trim(Copy(ParamPart, 7, MaxInt));
      end
      else if Pos('out ', LowerCase(ParamPart)) = 1 then
      begin
        IsOut := True;
        ParamPart := Trim(Copy(ParamPart, 5, MaxInt));
      end;
      
      // 分离名称和类型
      ColonPos := Pos(':', ParamPart);
      if ColonPos > 0 then
      begin
        ParamName := Trim(Copy(ParamPart, 1, ColonPos - 1));
        ParamType := Trim(Copy(ParamPart, ColonPos + 1, MaxInt));
        
        // 移除默认值
        if Pos('=', ParamType) > 0 then
          ParamType := Trim(Copy(ParamType, 1, Pos('=', ParamType) - 1));
        
        // 处理多个同类型参数 (a, b: Integer)
        Names.Clear;
        Names.Delimiter := ',';
        Names.StrictDelimiter := True;
        Names.DelimitedText := ParamName;
        
        for J := 0 to Names.Count - 1 do
        begin
          Info.Name := Trim(Names[J]);
          Info.TypeName := ParamType;
          Info.Category := ClassifyParamType(ParamType);
          Info.IsVar := IsVar;
          Info.IsConst := IsConst;
          Info.IsOut := IsOut;
          Info.HasDefault := Pos('=', Params[I]) > 0;
          
          SetLength(Result, Length(Result) + 1);
          Result[High(Result)] := Info;
        end;
      end;
    end;
  finally
    Params.Free;
    Names.Free;
  end;
end;

function TSourceScanner.IsFunctionDeclaration(const ALine: string; out AFuncInfo: TFunctionInfo): Boolean;
var
  Line: string;
  P, ParenStart, ParenEnd, ColonPos: Integer;
  FuncName, ParamStr, ReturnType: string;
begin
  Result := False;
  Line := Trim(ALine);
  
  // 检查是否以 function 开头
  if Pos('function ', LowerCase(Line)) <> 1 then
    Exit;
    
  Line := Trim(Copy(Line, 10, MaxInt));
  
  // 提取函数名
  ParenStart := Pos('(', Line);
  ColonPos := Pos(':', Line);
  
  if ParenStart > 0 then
  begin
    FuncName := Trim(Copy(Line, 1, ParenStart - 1));
    ParenEnd := Pos(')', Line);
    if ParenEnd > ParenStart then
    begin
      ParamStr := Copy(Line, ParenStart + 1, ParenEnd - ParenStart - 1);
      ColonPos := Pos(':', Copy(Line, ParenEnd, MaxInt));
      if ColonPos > 0 then
        ReturnType := Trim(Copy(Line, ParenEnd + ColonPos, MaxInt))
      else
        ReturnType := '';
    end;
  end
  else if ColonPos > 0 then
  begin
    FuncName := Trim(Copy(Line, 1, ColonPos - 1));
    ParamStr := '';
    ReturnType := Trim(Copy(Line, ColonPos + 1, MaxInt));
  end
  else
    Exit;
    
  // 移除返回类型中的分号
  P := Pos(';', ReturnType);
  if P > 0 then
    ReturnType := Trim(Copy(ReturnType, 1, P - 1));
    
  // 填充函数信息
  AFuncInfo.Name := FuncName;
  AFuncInfo.UnitName := FCurrentUnit;
  AFuncInfo.FileName := FCurrentFile;
  AFuncInfo.Visibility := FCurrentVisibility;
  AFuncInfo.Parameters := ParseParameters(ParamStr);
  AFuncInfo.ReturnType := ReturnType;
  AFuncInfo.IsMethod := FInClass;
  AFuncInfo.IsConstructor := False;
  AFuncInfo.IsDestructor := False;
  AFuncInfo.ClassName := FCurrentClass;
  AFuncInfo.HasErrorReturn := (Pos('result', LowerCase(ReturnType)) > 0) or
                              (Pos('boolean', LowerCase(ReturnType)) > 0) or
                              (Pos('error', LowerCase(ReturnType)) > 0);
  AFuncInfo.IsOverload := Pos('overload', LowerCase(ALine)) > 0;
  
  Result := True;
end;


function TSourceScanner.IsProcedureDeclaration(const ALine: string; out AFuncInfo: TFunctionInfo): Boolean;
var
  Line: string;
  P, ParenStart, ParenEnd: Integer;
  ProcName, ParamStr: string;
begin
  Result := False;
  Line := Trim(ALine);
  
  // 检查是否以 procedure 开头
  if Pos('procedure ', LowerCase(Line)) <> 1 then
    Exit;
    
  Line := Trim(Copy(Line, 11, MaxInt));
  
  // 提取过程名
  ParenStart := Pos('(', Line);
  
  if ParenStart > 0 then
  begin
    ProcName := Trim(Copy(Line, 1, ParenStart - 1));
    ParenEnd := Pos(')', Line);
    if ParenEnd > ParenStart then
      ParamStr := Copy(Line, ParenStart + 1, ParenEnd - ParenStart - 1)
    else
      ParamStr := '';
  end
  else
  begin
    P := Pos(';', Line);
    if P > 0 then
      ProcName := Trim(Copy(Line, 1, P - 1))
    else
      ProcName := Trim(Line);
    ParamStr := '';
  end;
  
  // 填充函数信息
  AFuncInfo.Name := ProcName;
  AFuncInfo.UnitName := FCurrentUnit;
  AFuncInfo.FileName := FCurrentFile;
  AFuncInfo.Visibility := FCurrentVisibility;
  AFuncInfo.Parameters := ParseParameters(ParamStr);
  AFuncInfo.ReturnType := '';
  AFuncInfo.IsMethod := FInClass;
  AFuncInfo.IsConstructor := False;
  AFuncInfo.IsDestructor := False;
  AFuncInfo.ClassName := FCurrentClass;
  AFuncInfo.HasErrorReturn := False;
  AFuncInfo.IsOverload := Pos('overload', LowerCase(ALine)) > 0;
  
  Result := True;
end;

function TSourceScanner.IsConstructorDeclaration(const ALine: string; out AFuncInfo: TFunctionInfo): Boolean;
var
  Line: string;
  P, ParenStart, ParenEnd: Integer;
  CtorName, ParamStr: string;
begin
  Result := False;
  Line := Trim(ALine);
  
  if Pos('constructor ', LowerCase(Line)) <> 1 then
    Exit;
    
  Line := Trim(Copy(Line, 13, MaxInt));
  
  ParenStart := Pos('(', Line);
  if ParenStart > 0 then
  begin
    CtorName := Trim(Copy(Line, 1, ParenStart - 1));
    ParenEnd := Pos(')', Line);
    if ParenEnd > ParenStart then
      ParamStr := Copy(Line, ParenStart + 1, ParenEnd - ParenStart - 1)
    else
      ParamStr := '';
  end
  else
  begin
    P := Pos(';', Line);
    if P > 0 then
      CtorName := Trim(Copy(Line, 1, P - 1))
    else
      CtorName := Trim(Line);
    ParamStr := '';
  end;
  
  AFuncInfo.Name := CtorName;
  AFuncInfo.UnitName := FCurrentUnit;
  AFuncInfo.FileName := FCurrentFile;
  AFuncInfo.Visibility := FCurrentVisibility;
  AFuncInfo.Parameters := ParseParameters(ParamStr);
  AFuncInfo.ReturnType := '';
  AFuncInfo.IsMethod := True;
  AFuncInfo.IsConstructor := True;
  AFuncInfo.IsDestructor := False;
  AFuncInfo.ClassName := FCurrentClass;
  AFuncInfo.HasErrorReturn := False;
  AFuncInfo.IsOverload := Pos('overload', LowerCase(ALine)) > 0;
  
  Result := True;
end;

function TSourceScanner.IsDestructorDeclaration(const ALine: string; out AFuncInfo: TFunctionInfo): Boolean;
var
  Line: string;
  P: Integer;
  DtorName: string;
begin
  Result := False;
  Line := Trim(ALine);
  
  if Pos('destructor ', LowerCase(Line)) <> 1 then
    Exit;
    
  Line := Trim(Copy(Line, 12, MaxInt));
  
  P := Pos(';', Line);
  if P > 0 then
    DtorName := Trim(Copy(Line, 1, P - 1))
  else
    DtorName := Trim(Line);
    
  // 移除参数部分（析构函数通常没有参数）
  P := Pos('(', DtorName);
  if P > 0 then
    DtorName := Trim(Copy(DtorName, 1, P - 1));
  
  AFuncInfo.Name := DtorName;
  AFuncInfo.UnitName := FCurrentUnit;
  AFuncInfo.FileName := FCurrentFile;
  AFuncInfo.Visibility := FCurrentVisibility;
  SetLength(AFuncInfo.Parameters, 0);
  AFuncInfo.ReturnType := '';
  AFuncInfo.IsMethod := True;
  AFuncInfo.IsConstructor := False;
  AFuncInfo.IsDestructor := True;
  AFuncInfo.ClassName := FCurrentClass;
  AFuncInfo.HasErrorReturn := False;
  AFuncInfo.IsOverload := False;
  
  Result := True;
end;

function TSourceScanner.ScanUnit(const AFilePath: string): TFunctionInfoArray;
var
  Lines: TStringList;
  I, LineNum: Integer;
  Line: string;
  FuncInfo: TFunctionInfo;
  LUnitName, LClassName: string;
  LVisibility: TVisibility;
begin
  SetLength(Result, 0);
  
  if not FileExists(AFilePath) then
    Exit;
    
  FCurrentFile := AFilePath;
  FCurrentUnit := '';
  FCurrentClass := '';
  FInInterface := False;
  FInImplementation := False;
  FInClass := False;
  FCurrentVisibility := vsPublic;
  
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(AFilePath);
    
    for I := 0 to Lines.Count - 1 do
    begin
      LineNum := I + 1;
      Line := ParseLine(Lines[I]);
      
      if Line = '' then
        Continue;
        
      // 检查 unit 声明
      if IsUnitDeclaration(Line, LUnitName) then
      begin
        FCurrentUnit := LUnitName;
        Continue;
      end;
      
      // 检查 interface 部分
      if IsInterfaceSection(Line) then
      begin
        FInInterface := True;
        FInImplementation := False;
        Continue;
      end;
      
      // 检查 implementation 部分
      if IsImplementationSection(Line) then
      begin
        FInInterface := False;
        FInImplementation := True;
        Continue;
      end;
      
      // 只处理 interface 部分的声明
      if not FInInterface then
        Continue;
        
      // 检查类声明
      if IsClassDeclaration(Line, LClassName) then
      begin
        FInClass := True;
        FCurrentClass := LClassName;
        FCurrentVisibility := vsPublic;  // 默认 public
        Continue;
      end;
      
      // 检查类结束
      if IsEndOfClass(Line) then
      begin
        FInClass := False;
        FCurrentClass := '';
        FCurrentVisibility := vsPublic;
        Continue;
      end;
      
      // 检查可见性关键字
      if IsVisibilityKeyword(Line, LVisibility) then
      begin
        FCurrentVisibility := LVisibility;
        Continue;
      end;
      
      // 检查函数声明
      if IsFunctionDeclaration(Line, FuncInfo) then
      begin
        FuncInfo.LineNumber := LineNum;
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := FuncInfo;
        Continue;
      end;
      
      // 检查过程声明
      if IsProcedureDeclaration(Line, FuncInfo) then
      begin
        FuncInfo.LineNumber := LineNum;
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := FuncInfo;
        Continue;
      end;
      
      // 检查构造函数
      if IsConstructorDeclaration(Line, FuncInfo) then
      begin
        FuncInfo.LineNumber := LineNum;
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := FuncInfo;
        Continue;
      end;
      
      // 检查析构函数
      if IsDestructorDeclaration(Line, FuncInfo) then
      begin
        FuncInfo.LineNumber := LineNum;
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := FuncInfo;
        Continue;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

function TSourceScanner.ScanDirectory(const APath: string): TFunctionInfoArray;
var
  SearchRec: TSearchRec;
  FilePath: string;
  UnitFunctions: TFunctionInfoArray;
  I: Integer;
begin
  SetLength(Result, 0);
  
  if not DirectoryExists(APath) then
    Exit;
    
  if FindFirst(IncludeTrailingPathDelimiter(APath) + '*.pas', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        if (SearchRec.Attr and faDirectory) = 0 then
        begin
          FilePath := IncludeTrailingPathDelimiter(APath) + SearchRec.Name;
          
          if FVerbose then
            WriteLn('Scanning: ', FilePath);
            
          UnitFunctions := ScanUnit(FilePath);
          
          for I := 0 to High(UnitFunctions) do
          begin
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)] := UnitFunctions[I];
          end;
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
end;

function TSourceScanner.ExtractPublicAPI(const AFilePath: string): TFunctionInfoArray;
var
  AllFunctions: TFunctionInfoArray;
  I: Integer;
begin
  SetLength(Result, 0);
  AllFunctions := ScanUnit(AFilePath);
  
  for I := 0 to High(AllFunctions) do
  begin
    if AllFunctions[I].Visibility = vsPublic then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := AllFunctions[I];
    end;
  end;
end;

end.
