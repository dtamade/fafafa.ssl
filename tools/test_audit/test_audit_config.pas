{
  Test Audit Config - 测试审计系统配置管理

  管理审计系统的配置文件和运行时参数
}
unit test_audit_config;

{$mode objfpc}{$H+}{$J-}

interface

uses
  SysUtils, Classes, fpjson, jsonparser;

type
  { 审计配置 }
  TAuditConfig = class
  private
    FSourceDir: string;
    FTestDir: string;
    FOutputDir: string;
    FCoverageThreshold: Double;
    FBoundaryThreshold: Double;
    FErrorThreshold: Double;
    FCryptoThreshold: Double;
    FThreadThreshold: Double;
    FResourceThreshold: Double;
    FBackendThreshold: Double;
    FOverallThreshold: Double;
    FExcludePatterns: TStringList;
    FIncludePatterns: TStringList;
    FVerbose: Boolean;
    FIncrementalMode: Boolean;
    FBaselineFile: string;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string);
    procedure LoadDefaults;
    
    property SourceDir: string read FSourceDir write FSourceDir;
    property TestDir: string read FTestDir write FTestDir;
    property OutputDir: string read FOutputDir write FOutputDir;
    property CoverageThreshold: Double read FCoverageThreshold write FCoverageThreshold;
    property BoundaryThreshold: Double read FBoundaryThreshold write FBoundaryThreshold;
    property ErrorThreshold: Double read FErrorThreshold write FErrorThreshold;
    property CryptoThreshold: Double read FCryptoThreshold write FCryptoThreshold;
    property ThreadThreshold: Double read FThreadThreshold write FThreadThreshold;
    property ResourceThreshold: Double read FResourceThreshold write FResourceThreshold;
    property BackendThreshold: Double read FBackendThreshold write FBackendThreshold;
    property OverallThreshold: Double read FOverallThreshold write FOverallThreshold;
    property ExcludePatterns: TStringList read FExcludePatterns;
    property IncludePatterns: TStringList read FIncludePatterns;
    property Verbose: Boolean read FVerbose write FVerbose;
    property IncrementalMode: Boolean read FIncrementalMode write FIncrementalMode;
    property BaselineFile: string read FBaselineFile write FBaselineFile;
  end;

implementation

constructor TAuditConfig.Create;
begin
  inherited Create;
  FExcludePatterns := TStringList.Create;
  FIncludePatterns := TStringList.Create;
  LoadDefaults;
end;

destructor TAuditConfig.Destroy;
begin
  FExcludePatterns.Free;
  FIncludePatterns.Free;
  inherited Destroy;
end;

procedure TAuditConfig.LoadDefaults;
begin
  FSourceDir := 'src';
  FTestDir := 'tests';
  FOutputDir := 'reports/audit';
  FCoverageThreshold := 80.0;
  FBoundaryThreshold := 70.0;
  FErrorThreshold := 75.0;
  FCryptoThreshold := 90.0;
  FThreadThreshold := 70.0;
  FResourceThreshold := 75.0;
  FBackendThreshold := 80.0;
  FOverallThreshold := 75.0;
  FVerbose := False;
  FIncrementalMode := False;
  FBaselineFile := '';
  
  FExcludePatterns.Clear;
  FExcludePatterns.Add('*.inc');
  FExcludePatterns.Add('*_test.pas');
  
  FIncludePatterns.Clear;
  FIncludePatterns.Add('*.pas');
end;


procedure TAuditConfig.LoadFromFile(const AFileName: string);
var
  JSONData: TJSONData;
  JSONObj: TJSONObject;
  JSONArr: TJSONArray;
  FileStream: TFileStream;
  I: Integer;
begin
  if not FileExists(AFileName) then
    raise Exception.CreateFmt('Config file not found: %s', [AFileName]);
    
  FileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    JSONData := GetJSON(FileStream);
    try
      if not (JSONData is TJSONObject) then
        raise Exception.Create('Invalid config file format');
        
      JSONObj := TJSONObject(JSONData);
      
      // 读取目录配置
      if JSONObj.Find('sourceDir') <> nil then
        FSourceDir := JSONObj.Get('sourceDir', FSourceDir);
      if JSONObj.Find('testDir') <> nil then
        FTestDir := JSONObj.Get('testDir', FTestDir);
      if JSONObj.Find('outputDir') <> nil then
        FOutputDir := JSONObj.Get('outputDir', FOutputDir);
        
      // 读取阈值配置
      if JSONObj.Find('thresholds') <> nil then
      begin
        JSONObj := TJSONObject(JSONObj.Find('thresholds'));
        FCoverageThreshold := JSONObj.Get('coverage', FCoverageThreshold);
        FBoundaryThreshold := JSONObj.Get('boundary', FBoundaryThreshold);
        FErrorThreshold := JSONObj.Get('error', FErrorThreshold);
        FCryptoThreshold := JSONObj.Get('crypto', FCryptoThreshold);
        FThreadThreshold := JSONObj.Get('thread', FThreadThreshold);
        FResourceThreshold := JSONObj.Get('resource', FResourceThreshold);
        FBackendThreshold := JSONObj.Get('backend', FBackendThreshold);
        FOverallThreshold := JSONObj.Get('overall', FOverallThreshold);
        JSONObj := TJSONObject(JSONData);
      end;
      
      // 读取排除模式
      if JSONObj.Find('excludePatterns') <> nil then
      begin
        JSONArr := TJSONArray(JSONObj.Find('excludePatterns'));
        FExcludePatterns.Clear;
        for I := 0 to JSONArr.Count - 1 do
          FExcludePatterns.Add(JSONArr.Strings[I]);
      end;
      
      // 读取包含模式
      if JSONObj.Find('includePatterns') <> nil then
      begin
        JSONArr := TJSONArray(JSONObj.Find('includePatterns'));
        FIncludePatterns.Clear;
        for I := 0 to JSONArr.Count - 1 do
          FIncludePatterns.Add(JSONArr.Strings[I]);
      end;
      
      // 读取其他选项
      FVerbose := JSONObj.Get('verbose', FVerbose);
      FIncrementalMode := JSONObj.Get('incrementalMode', FIncrementalMode);
      FBaselineFile := JSONObj.Get('baselineFile', FBaselineFile);
      
    finally
      JSONData.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

procedure TAuditConfig.SaveToFile(const AFileName: string);
var
  JSONObj, ThresholdsObj: TJSONObject;
  ExcludeArr, IncludeArr: TJSONArray;
  I: Integer;
  OutputStr: string;
  FileStream: TFileStream;
begin
  JSONObj := TJSONObject.Create;
  try
    // 目录配置
    JSONObj.Add('sourceDir', FSourceDir);
    JSONObj.Add('testDir', FTestDir);
    JSONObj.Add('outputDir', FOutputDir);
    
    // 阈值配置
    ThresholdsObj := TJSONObject.Create;
    ThresholdsObj.Add('coverage', FCoverageThreshold);
    ThresholdsObj.Add('boundary', FBoundaryThreshold);
    ThresholdsObj.Add('error', FErrorThreshold);
    ThresholdsObj.Add('crypto', FCryptoThreshold);
    ThresholdsObj.Add('thread', FThreadThreshold);
    ThresholdsObj.Add('resource', FResourceThreshold);
    ThresholdsObj.Add('backend', FBackendThreshold);
    ThresholdsObj.Add('overall', FOverallThreshold);
    JSONObj.Add('thresholds', ThresholdsObj);
    
    // 排除模式
    ExcludeArr := TJSONArray.Create;
    for I := 0 to FExcludePatterns.Count - 1 do
      ExcludeArr.Add(FExcludePatterns[I]);
    JSONObj.Add('excludePatterns', ExcludeArr);
    
    // 包含模式
    IncludeArr := TJSONArray.Create;
    for I := 0 to FIncludePatterns.Count - 1 do
      IncludeArr.Add(FIncludePatterns[I]);
    JSONObj.Add('includePatterns', IncludeArr);
    
    // 其他选项
    JSONObj.Add('verbose', FVerbose);
    JSONObj.Add('incrementalMode', FIncrementalMode);
    JSONObj.Add('baselineFile', FBaselineFile);
    
    // 写入文件
    OutputStr := JSONObj.FormatJSON;
    FileStream := TFileStream.Create(AFileName, fmCreate);
    try
      FileStream.WriteBuffer(OutputStr[1], Length(OutputStr));
    finally
      FileStream.Free;
    end;
  finally
    JSONObj.Free;
  end;
end;

end.
