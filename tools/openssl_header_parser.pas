program OpenSSLHeaderParser;

{$mode objfpc}{$H+}
{$I-}

uses
  SysUtils, Classes, RegExpr, Types, StrUtils, fgl;

type
  TSymbolType = (stConstant, stType, stFunction, stStructure, stEnumeration, stMacro);
  
  TSymbol = record
    Name: String;
    SymType: TSymbolType;
    Declaration: String;
    SourceFile: String;
    LineNumber: Integer;
    Dependencies: TStringList;
  end;
  
  TSymbolMap = specialize TFPGMap<String, TSymbol>;
  
  { TOpenSSLHeaderParser }
  TOpenSSLHeaderParser = class
  private
    FSourcePath: String;
    FOutputPath: String;
    FSymbols: TSymbolMap;
    FProcessedFiles: TStringList;
    FCurrentFile: String;
    
    // Regular expressions for parsing
    FRegExDefine: TRegExpr;
    FRegExTypedef: TRegExpr;
    FRegExStruct: TRegExpr;
    FRegExFunction: TRegExpr;
    FRegExEnum: TRegExpr;
    
    procedure InitRegEx;
    procedure ParseFile(const FileName: String);
    procedure ExtractDefines(const Content: String);
    procedure ExtractTypedefs(const Content: String);
    procedure ExtractStructs(const Content: String);
    procedure ExtractFunctions(const Content: String);
    procedure ExtractEnums(const Content: String);
    function ConvertCTypeToPascal(const CType: String): String;
    function ConvertCFunctionToPascal(const CFunc: String): String;
    function SanitizePascalIdentifier(const Name: String): String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseOpenSSLHeaders(const SourcePath: String);
    procedure GeneratePascalUnits(const OutputPath: String);
  end;
  
  { TModuleOrganizer }
  TModuleOrganizer = class
  private
    FModules: TStringList;
    procedure CreateModule(const ModuleName: String; Symbols: TStringList);
  public
    constructor Create;
    destructor Destroy; override;
    procedure OrganizeSymbols(Symbols: TSymbolMap);
    procedure GenerateModules(const OutputPath: String);
  end;

{ TOpenSSLHeaderParser }

constructor TOpenSSLHeaderParser.Create;
begin
  inherited;
  FSymbols := TSymbolMap.Create;
  FProcessedFiles := TStringList.Create;
  FProcessedFiles.Sorted := True;
  FProcessedFiles.Duplicates := dupIgnore;
  InitRegEx;
end;

destructor TOpenSSLHeaderParser.Destroy;
var
  I: Integer;
begin
  FRegExDefine.Free;
  FRegExTypedef.Free;
  FRegExStruct.Free;
  FRegExFunction.Free;
  FRegExEnum.Free;
  
  for I := 0 to FSymbols.Count - 1 do
    if Assigned(FSymbols.Data[I].Dependencies) then
      FSymbols.Data[I].Dependencies.Free;
      
  FSymbols.Free;
  FProcessedFiles.Free;
  inherited;
end;

procedure TOpenSSLHeaderParser.InitRegEx;
begin
  // Initialize regular expressions for different C constructs
  FRegExDefine := TRegExpr.Create;
  FRegExDefine.Expression := '#define\s+(\w+)\s+(.+)';
  
  FRegExTypedef := TRegExpr.Create;
  FRegExTypedef.Expression := 'typedef\s+(.+?)\s+(\w+);';
  
  FRegExStruct := TRegExpr.Create;
  FRegExStruct.Expression := 'struct\s+(\w+)\s*\{([^}]+)\}';
  
  FRegExFunction := TRegExpr.Create;
  FRegExFunction.Expression := '(\w+[\s\*]+)\s*(\w+)\s*\(([^)]*)\)';
  
  FRegExEnum := TRegExpr.Create;
  FRegExEnum.Expression := 'enum\s+(\w+)?\s*\{([^}]+)\}';
end;

procedure TOpenSSLHeaderParser.ParseFile(const FileName: String);
var
  FileContent: TStringList;
  Content: String;
begin
  if FProcessedFiles.IndexOf(FileName) >= 0 then
    Exit;
    
  FProcessedFiles.Add(FileName);
  FCurrentFile := FileName;
  
  if not FileExists(FileName) then
  begin
    WriteLn('Warning: File not found: ', FileName);
    Exit;
  end;
  
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FileName);
    Content := FileContent.Text;
    
    // Remove C-style comments
    Content := StringReplace(Content, '/*', '{', [rfReplaceAll]);
    Content := StringReplace(Content, '*/', '}', [rfReplaceAll]);
    
    // Parse different elements
    ExtractDefines(Content);
    ExtractTypedefs(Content);
    ExtractStructs(Content);
    ExtractFunctions(Content);
    ExtractEnums(Content);
  finally
    FileContent.Free;
  end;
end;

procedure TOpenSSLHeaderParser.ExtractDefines(const Content: String);
var
  Symbol: TSymbol;
begin
  if FRegExDefine.Exec(Content) then
  begin
    repeat
      Symbol.Name := FRegExDefine.Match[1];
      Symbol.SymType := stConstant;
      Symbol.Declaration := FRegExDefine.Match[2];
      Symbol.SourceFile := FCurrentFile;
      Symbol.Dependencies := nil;
      
      if not FSymbols.Find(Symbol.Name, Symbol) then
        FSymbols.Add(Symbol.Name, Symbol);
    until not FRegExDefine.ExecNext;
  end;
end;

procedure TOpenSSLHeaderParser.ExtractTypedefs(const Content: String);
var
  Symbol: TSymbol;
begin
  if FRegExTypedef.Exec(Content) then
  begin
    repeat
      Symbol.Name := FRegExTypedef.Match[2];
      Symbol.SymType := stType;
      Symbol.Declaration := FRegExTypedef.Match[1];
      Symbol.SourceFile := FCurrentFile;
      Symbol.Dependencies := nil;
      
      if not FSymbols.Find(Symbol.Name, Symbol) then
        FSymbols.Add(Symbol.Name, Symbol);
    until not FRegExTypedef.ExecNext;
  end;
end;

procedure TOpenSSLHeaderParser.ExtractStructs(const Content: String);
var
  Symbol: TSymbol;
begin
  if FRegExStruct.Exec(Content) then
  begin
    repeat
      Symbol.Name := FRegExStruct.Match[1];
      Symbol.SymType := stStructure;
      Symbol.Declaration := FRegExStruct.Match[2];
      Symbol.SourceFile := FCurrentFile;
      Symbol.Dependencies := nil;
      
      if not FSymbols.Find(Symbol.Name, Symbol) then
        FSymbols.Add(Symbol.Name, Symbol);
    until not FRegExStruct.ExecNext;
  end;
end;

procedure TOpenSSLHeaderParser.ExtractFunctions(const Content: String);
var
  Symbol: TSymbol;
begin
  if FRegExFunction.Exec(Content) then
  begin
    repeat
      Symbol.Name := FRegExFunction.Match[2];
      Symbol.SymType := stFunction;
      Symbol.Declaration := FRegExFunction.Match[0];
      Symbol.SourceFile := FCurrentFile;
      Symbol.Dependencies := nil;
      
      // Skip certain patterns
      if (Pos('typedef', Symbol.Declaration) = 0) and
        (Pos('#define', Symbol.Declaration) = 0) then
      begin
        if not FSymbols.Find(Symbol.Name, Symbol) then
          FSymbols.Add(Symbol.Name, Symbol);
      end;
    until not FRegExFunction.ExecNext;
  end;
end;

procedure TOpenSSLHeaderParser.ExtractEnums(const Content: String);
var
  Symbol: TSymbol;
begin
  if FRegExEnum.Exec(Content) then
  begin
    repeat
      if FRegExEnum.Match[1] <> '' then
        Symbol.Name := FRegExEnum.Match[1]
      else
        Symbol.Name := 'anonymous_enum_' + IntToStr(Random(10000));
        
      Symbol.SymType := stEnumeration;
      Symbol.Declaration := FRegExEnum.Match[2];
      Symbol.SourceFile := FCurrentFile;
      Symbol.Dependencies := nil;
      
      if not FSymbols.Find(Symbol.Name, Symbol) then
        FSymbols.Add(Symbol.Name, Symbol);
    until not FRegExEnum.ExecNext;
  end;
end;

function TOpenSSLHeaderParser.ConvertCTypeToPascal(const CType: String): String;
var
  S: String;
begin
  S := Trim(CType);
  
  // Basic type conversions
  S := StringReplace(S, 'unsigned char', 'Byte', [rfReplaceAll]);
  S := StringReplace(S, 'signed char', 'ShortInt', [rfReplaceAll]);
  S := StringReplace(S, 'unsigned short', 'Word', [rfReplaceAll]);
  S := StringReplace(S, 'unsigned int', 'Cardinal', [rfReplaceAll]);
  S := StringReplace(S, 'unsigned long long', 'UInt64', [rfReplaceAll]);
  S := StringReplace(S, 'unsigned long', 'LongWord', [rfReplaceAll]);
  S := StringReplace(S, 'long long', 'Int64', [rfReplaceAll]);
  S := StringReplace(S, 'long', 'LongInt', [rfReplaceAll]);
  S := StringReplace(S, 'short', 'SmallInt', [rfReplaceAll]);
  S := StringReplace(S, 'char', 'AnsiChar', [rfReplaceAll]);
  S := StringReplace(S, 'void', 'Pointer', [rfReplaceAll]);
  S := StringReplace(S, 'int', 'Integer', [rfReplaceAll]);
  S := StringReplace(S, 'float', 'Single', [rfReplaceAll]);
  S := StringReplace(S, 'double', 'Double', [rfReplaceAll]);
  S := StringReplace(S, 'size_t', 'NativeUInt', [rfReplaceAll]);
  S := StringReplace(S, 'ssize_t', 'NativeInt', [rfReplaceAll]);
  
  // Handle pointers
  S := StringReplace(S, '*', '^', [rfReplaceAll]);
  
  Result := S;
end;

function TOpenSSLHeaderParser.ConvertCFunctionToPascal(const CFunc: String): String;
var
  RetType, FuncName, Params: String;
  I: Integer;
begin
  // Basic conversion - this is simplified
  Result := CFunc;
  
  // Convert return type
  if Pos('void ', Result) = 1 then
    Result := 'procedure' + Copy(Result, 5, MaxInt)
  else
    Result := 'function' + Result;
    
  // Convert parameter separators
  Result := StringReplace(Result, ',', ';', [rfReplaceAll]);
  
  // Convert types
  Result := ConvertCTypeToPascal(Result);
end;

function TOpenSSLHeaderParser.SanitizePascalIdentifier(const Name: String): String;
const
  ReservedWords: array[0..64] of string = (
    'and', 'array', 'as', 'asm', 'begin', 'case', 'class', 'const',
    'constructor', 'destructor', 'div', 'do', 'downto', 'else', 'end',
    'except', 'exports', 'file', 'finalization', 'finally', 'for', 'function',
    'goto', 'if', 'implementation', 'in', 'inherited', 'initialization',
    'inline', 'interface', 'is', 'label', 'library', 'mod', 'nil', 'not',
    'object', 'of', 'or', 'packed', 'procedure', 'program', 'property',
    'raise', 'record', 'repeat', 'resourcestring', 'set', 'shl', 'shr',
    'string', 'then', 'threadvar', 'to', 'try', 'type', 'unit', 'until',
    'uses', 'var', 'while', 'with', 'xor'
  );
var
  I: Integer;
begin
  Result := Name;
  
  // Check if it's a reserved word
  for I := 0 to High(ReservedWords) do
    if LowerCase(Result) = ReservedWords[I] then
    begin
      Result := '&' + Result;
      Break;
    end;
end;

procedure TOpenSSLHeaderParser.ParseOpenSSLHeaders(const SourcePath: String);
var
  SearchRec: TSearchRec;
  HeaderFiles: TStringList;
  I: Integer;
begin
  FSourcePath := IncludeTrailingPathDelimiter(SourcePath);
  HeaderFiles := TStringList.Create;
  try
    // Find all header files
    if FindFirst(FSourcePath + '*.h', faAnyFile, SearchRec) = 0 then
    begin
      repeat
        if (SearchRec.Attr and faDirectory) = 0 then
          HeaderFiles.Add(FSourcePath + SearchRec.Name);
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;
    
    // Parse each header file
    for I := 0 to HeaderFiles.Count - 1 do
    begin
      WriteLn('Parsing: ', ExtractFileName(HeaderFiles[I]));
      ParseFile(HeaderFiles[I]);
    end;
    
    WriteLn('Total symbols found: ', FSymbols.Count);
  finally
    HeaderFiles.Free;
  end;
end;

procedure TOpenSSLHeaderParser.GeneratePascalUnits(const OutputPath: String);
var
  Organizer: TModuleOrganizer;
begin
  FOutputPath := IncludeTrailingPathDelimiter(OutputPath);
  
  Organizer := TModuleOrganizer.Create;
  try
    Organizer.OrganizeSymbols(FSymbols);
    Organizer.GenerateModules(FOutputPath);
  finally
    Organizer.Free;
  end;
end;

{ TModuleOrganizer }

constructor TModuleOrganizer.Create;
begin
  inherited;
  FModules := TStringList.Create;
end;

destructor TModuleOrganizer.Destroy;
begin
  FModules.Free;
  inherited;
end;

procedure TModuleOrganizer.CreateModule(const ModuleName: String; Symbols: TStringList);
var
  ModuleFile: TStringList;
  I: Integer;
begin
  ModuleFile := TStringList.Create;
  try
    ModuleFile.Add('unit ' + ModuleName + ';');
    ModuleFile.Add('');
    ModuleFile.Add('{$mode objfpc}{$H+}');
    ModuleFile.Add('');
    ModuleFile.Add('interface');
    ModuleFile.Add('');
    ModuleFile.Add('uses');
    ModuleFile.Add('  SysUtils, DynLibs;');
    ModuleFile.Add('');
    
    // Add symbols
    for I := 0 to Symbols.Count - 1 do
      ModuleFile.Add(Symbols[I]);
      
    ModuleFile.Add('');
    ModuleFile.Add('implementation');
    ModuleFile.Add('');
    ModuleFile.Add('end.');
    
    // Save file
    ModuleFile.SaveToFile(ModuleName + '.pas');
  finally
    ModuleFile.Free;
  end;
end;

procedure TModuleOrganizer.OrganizeSymbols(Symbols: TSymbolMap);
var
  CoreSymbols, CryptoSymbols, X509Symbols, BIOSymbols: TStringList;
  I: Integer;
  Symbol: TSymbol;
begin
  CoreSymbols := TStringList.Create;
  CryptoSymbols := TStringList.Create;
  X509Symbols := TStringList.Create;
  BIOSymbols := TStringList.Create;
  
  try
    // Organize symbols by module
    for I := 0 to Symbols.Count - 1 do
    begin
      Symbol := Symbols.Data[I];
      
      // Categorize based on name prefix
      if Pos('SSL_', Symbol.Name) = 1 then
        CoreSymbols.Add('// ' + Symbol.Name)
      else if Pos('EVP_', Symbol.Name) = 1 then
        CryptoSymbols.Add('// ' + Symbol.Name)
      else if Pos('X509', Symbol.Name) = 1 then
        X509Symbols.Add('// ' + Symbol.Name)
      else if Pos('BIO_', Symbol.Name) = 1 then
        BIOSymbols.Add('// ' + Symbol.Name);
    end;
    
    // Store organized modules
    FModules.AddObject('fafafa.ssl.openssl.core', CoreSymbols);
    FModules.AddObject('fafafa.ssl.openssl.crypto', CryptoSymbols);
    FModules.AddObject('fafafa.ssl.openssl.x509', X509Symbols);
    FModules.AddObject('fafafa.ssl.openssl.bio', BIOSymbols);
  except
    CoreSymbols.Free;
    CryptoSymbols.Free;
    X509Symbols.Free;
    BIOSymbols.Free;
    raise;
  end;
end;

procedure TModuleOrganizer.GenerateModules(const OutputPath: String);
var
  I: Integer;
begin
  for I := 0 to FModules.Count - 1 do
  begin
    CreateModule(FModules[I], TStringList(FModules.Objects[I]));
    TStringList(FModules.Objects[I]).Free;
  end;
end;

{ Main Program }
var
  Parser: TOpenSSLHeaderParser;
  SourcePath, OutputPath: String;
begin
  WriteLn('OpenSSL Header Parser for Pascal');
  WriteLn('=================================');
  WriteLn;
  
  // Get paths from command line or use defaults
  if ParamCount >= 1 then
    SourcePath := ParamStr(1)
  else
    SourcePath := 'C:\OpenSSL\include\openssl';
    
  if ParamCount >= 2 then
    OutputPath := ParamStr(2)
  else
    OutputPath := '..\src\backend\openssl';
    
  WriteLn('Source Path: ', SourcePath);
  WriteLn('Output Path: ', OutputPath);
  WriteLn;
  
  if not DirectoryExists(SourcePath) then
  begin
    WriteLn('Error: Source directory not found!');
    Exit;
  end;
  
  ForceDirectories(OutputPath);
  
  Parser := TOpenSSLHeaderParser.Create;
  try
    Parser.ParseOpenSSLHeaders(SourcePath);
    Parser.GeneratePascalUnits(OutputPath);
    WriteLn('Conversion completed successfully!');
  finally
    Parser.Free;
  end;
end.