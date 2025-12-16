{
  fafafa.ssl.log - SSL/TLS 日志和调试单元
  
  版本: 1.0
  作者: fafafa.ssl 开发团队
  创建: 2025-09-28
  
  描述:
    提供 SSL/TLS 库的日志记录和调试功能，包括：
    - 多级日志记录
    - 日志输出管理
    - 调试信息追踪
    - 性能监控
}

unit fafafa.ssl.log;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  SysUtils, Classes, SyncObjs,
  fafafa.ssl.base,
  fafafa.ssl.exceptions,
  fafafa.ssl.utils,        // Phase 2.3.5 - SSL工具（GetErrorDetails）
  fafafa.ssl.debug.utils;  // Phase 2.3.5 - 调试工具（Dump函数）

type
  { 日志级别 }
  TSSLLogLevel = (
    sllNone,      // 不记录
    sllFatal,     // 致命错误
    sllError,     // 错误
    sllWarning,   // 警告
    sllInfo,      // 信息
    sllDebug,     // 调试
    sllTrace      // 追踪
  );

  { 日志输出目标 }
  TSSLLogTarget = (
    sltConsole,   // 控制台
    sltFile,      // 文件
    sltCallback,  // 回调函数
    sltMemory     // 内存缓冲
  );
  TSSLLogTargets = set of TSSLLogTarget;

  { 日志条目 }
  TSSLLogEntry = record
    Timestamp: TDateTime;
    Level: TSSLLogLevel;
    ThreadId: TThreadID;
    Module: string;
    Message: string;
    Data: string;
  end;
  PSSLLogEntry = ^TSSLLogEntry;

  { 日志回调函数 }
  TSSLLogCallback = procedure(const aEntry: TSSLLogEntry) of object;

  { ISSLLogger - 日志记录器接口 }
  ISSLLogger = interface
    ['{8F4A6B12-3C5D-4E7F-9A8B-1D2E3F4A5B6C}']
    procedure Log(aLevel: TSSLLogLevel; const aMessage: string; const aData: string = '');
    procedure LogFatal(const aMessage: string; const aData: string = '');
    procedure LogError(const aMessage: string; const aData: string = '');
    procedure LogWarning(const aMessage: string; const aData: string = '');
    procedure LogInfo(const aMessage: string; const aData: string = '');
    procedure LogDebug(const aMessage: string; const aData: string = '');
    procedure LogTrace(const aMessage: string; const aData: string = '');
    
    function GetLevel: TSSLLogLevel;
    procedure SetLevel(aLevel: TSSLLogLevel);
    function GetEnabled: Boolean;
    procedure SetEnabled(aEnabled: Boolean);
    procedure SetTargets(aTargets: TSSLLogTargets);
    procedure SetLogFile(const aFileName: string);
    
    property Level: TSSLLogLevel read GetLevel write SetLevel;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

  { TSSLLogger - 基础日志记录器 }
  TSSLLogger = class(TInterfacedObject, ISSLLogger)
  private
    FLevel: TSSLLogLevel;
    FEnabled: Boolean;
    FModule: string;
    FLock: TCriticalSection;
    FTargets: TSSLLogTargets;
    FLogFile: TextFile;
    FLogFileName: string;
    FLogToFile: Boolean;
    FCallback: TSSLLogCallback;
    FMemoryLog: TList;
    FMaxMemoryEntries: Integer;
    FAutoFlush: Boolean;
    FIncludeTimestamp: Boolean;
    FIncludeThreadId: Boolean;
    FDateTimeFormat: string;
    
    procedure DoLog(const aEntry: TSSLLogEntry);
    procedure WriteToConsole(const aEntry: TSSLLogEntry);
    procedure WriteToFile(const aEntry: TSSLLogEntry);
    procedure WriteToMemory(const aEntry: TSSLLogEntry);
    procedure InvokeCallback(const aEntry: TSSLLogEntry);
    function FormatLogEntry(const aEntry: TSSLLogEntry): string;
    procedure CheckRotateLogFile;
  protected
    function GetLevel: TSSLLogLevel;
    procedure SetLevel(aLevel: TSSLLogLevel);
    function GetEnabled: Boolean;
    procedure SetEnabled(aEnabled: Boolean);
  public
    constructor Create(const aModule: string = '');
    destructor Destroy; override;
    
    // ISSLLogger 实现
    procedure Log(aLevel: TSSLLogLevel; const aMessage: string; const aData: string = '');
    procedure LogFatal(const aMessage: string; const aData: string = '');
    procedure LogError(const aMessage: string; const aData: string = '');
    procedure LogWarning(const aMessage: string; const aData: string = '');
    procedure LogInfo(const aMessage: string; const aData: string = '');
    procedure LogDebug(const aMessage: string; const aData: string = '');
    procedure LogTrace(const aMessage: string; const aData: string = '');
    
    // 配置方法
    procedure SetTargets(aTargets: TSSLLogTargets);
    procedure SetLogFile(const aFileName: string);
    procedure SetCallback(aCallback: TSSLLogCallback);
    procedure SetMaxMemoryEntries(aMax: Integer);
    procedure SetDateTimeFormat(const aFormat: string);
    
    // 内存日志管理
    procedure ClearMemoryLog;
    function GetMemoryLog: TStringList;
    procedure FlushToFile;
    
    // 属性
    property Module: string read FModule write FModule;
    property Targets: TSSLLogTargets read FTargets write SetTargets;
    property LogFileName: string read FLogFileName;
    property AutoFlush: Boolean read FAutoFlush write FAutoFlush;
    property IncludeTimestamp: Boolean read FIncludeTimestamp write FIncludeTimestamp;
    property IncludeThreadId: Boolean read FIncludeThreadId write FIncludeThreadId;
  end;

  TSSLLoggerEntry = class
  public
    Logger: ISSLLogger;
  end;

  { TSSLLogManager - 全局日志管理器 }
  TSSLLogManager = class
  private
    FLoggers: TStringList;
    FGlobalLogger: ISSLLogger;
    FDefaultLevel: TSSLLogLevel;
    FDefaultTargets: TSSLLogTargets;
    
    constructor CreateInstance;
    class var FInstance: TSSLLogManager;
  public
    class function Instance: TSSLLogManager;
    class procedure FreeInstance;
    
    destructor Destroy; override;
    
    // 日志器管理
    function GetLogger(const aModule: string = ''): ISSLLogger;
    procedure RegisterLogger(const aModule: string; aLogger: ISSLLogger);
    procedure UnregisterLogger(const aModule: string);
    
    // 全局配置
    procedure SetGlobalLevel(aLevel: TSSLLogLevel);
    procedure SetGlobalTargets(aTargets: TSSLLogTargets);
    procedure SetGlobalLogFile(const aFileName: string);
    procedure EnableAll;
    procedure DisableAll;
    
    // 属性
    property GlobalLogger: ISSLLogger read FGlobalLogger;
    property DefaultLevel: TSSLLogLevel read FDefaultLevel write FDefaultLevel;
    property DefaultTargets: TSSLLogTargets read FDefaultTargets write FDefaultTargets;
  end;

  { TSSLDebugger - 调试辅助类 }
  TSSLDebugger = class
  private
    class var FEnabled: Boolean;
    class var FTraceEnabled: Boolean;
    class var FHexDumpEnabled: Boolean;
    class var FLogger: ISSLLogger;
  public
    class procedure Initialize;
    class function GetEnabled: Boolean; static;
    class procedure SetEnabled(aEnabled: Boolean); static;
    class function GetTraceEnabled: Boolean; static;
    class procedure SetTraceEnabled(aEnabled: Boolean); static;
    class function GetHexDumpEnabled: Boolean; static;
    class procedure SetHexDumpEnabled(aEnabled: Boolean); static;
    
    // 调试输出
    class procedure Debug(const aMessage: string);
    class procedure DebugFmt(const aFormat: string; const aArgs: array of const);
    class procedure Trace(const aProc, aMessage: string);
    class procedure TraceFmt(const aProc, aFormat: string; const aArgs: array of const);
    
    // 数据转储
    class procedure HexDump(const aData: TBytes; const aLabel: string = '');
    class procedure DumpConfig(const aConfig: TSSLConfig);
    class procedure DumpCertificate(const aInfo: TSSLCertificateInfo);
    class procedure DumpConnection(const aInfo: TSSLConnectionInfo);
    class procedure DumpException(E: Exception);
    
    // 性能追踪
    class function StartTimer(const aLabel: string): Int64;
    class procedure StopTimer(aStartTime: Int64; const aLabel: string);
    
    // 断言
    class procedure Assert(aCondition: Boolean; const aMessage: string);
    class procedure AssertNotNil(aObject: TObject; const aName: string);
    
    // 属性
    class property Enabled: Boolean read GetEnabled write SetEnabled;
    class property TraceEnabled: Boolean read GetTraceEnabled write SetTraceEnabled;
    class property HexDumpEnabled: Boolean read GetHexDumpEnabled write SetHexDumpEnabled;
  end;

  { TSSLProfiler - 性能分析器 }
  TSSLProfiler = class
  private
    type
      TProfileEntry = record
        Name: string;
        StartTime: Int64;
        EndTime: Int64;
        Count: Integer;
        TotalTime: Int64;
        MinTime: Int64;
        MaxTime: Int64;
      end;
    
  private
    FEntries: TStringList;
    FLock: TCriticalSection;
    FEnabled: Boolean;
    
    constructor CreateInstance;
    function GetEntry(const aName: string): TProfileEntry;
    procedure UpdateEntry(const aName: string; aElapsed: Int64);
    class var FInstance: TSSLProfiler;
  public
    class function Instance: TSSLProfiler;
    class procedure FreeInstance;
    
    destructor Destroy; override;
    
    // 性能测量
    function StartMeasure(const aName: string): Int64;
    procedure EndMeasure(const aName: string; aStartTime: Int64);
    procedure Measure(const aName: string; aProc: TProcedure);
    
    // 报告
    function GetReport: string;
    function GetReportAsJSON: string;
    procedure Clear;
    
    // 属性
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

const
  LOG_LEVEL_NAMES: array[TSSLLogLevel] of string = (
    'NONE', 'FATAL', 'ERROR', 'WARNING', 'INFO', 'DEBUG', 'TRACE'
  );
  
  DEFAULT_LOG_DATE_FORMAT = 'yyyy-mm-dd hh:nn:ss.zzz';
  DEFAULT_MAX_MEMORY_ENTRIES = 1000;
  DEFAULT_LOG_FILE_EXT = '.log';

// 全局便捷函数
function SSLLogger(const aModule: string = ''): ISSLLogger;
procedure SSLLog(aLevel: TSSLLogLevel; const aMessage: string; const aData: string = '');
procedure SSLLogError(const aMessage: string; const aData: string = '');
procedure SSLLogWarning(const aMessage: string; const aData: string = '');
procedure SSLLogInfo(const aMessage: string; const aData: string = '');
procedure SSLLogDebug(const aMessage: string; const aData: string = '');
procedure SSLLogTrace(const aMessage: string; const aData: string = '');

// 调试便捷函数
procedure SSLDebug(const aMessage: string);
procedure SSLTrace(const aProc, aMessage: string);
procedure SSLHexDump(const aData: TBytes; const aLabel: string = '');

// 性能测量便捷函数
function SSLStartTimer(const aLabel: string): Int64;
procedure SSLStopTimer(aStartTime: Int64; const aLabel: string);

implementation

uses
  fafafa.ssl.base,
  fafafa.ssl.exceptions,
  DateUtils, fpjson, jsonparser;

{ TSSLLogger }

constructor TSSLLogger.Create(const aModule: string);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FModule := aModule;
  FLevel := sllInfo;
  FEnabled := True;
  FTargets := [sltConsole];
  FMemoryLog := TList.Create;
  FMaxMemoryEntries := DEFAULT_MAX_MEMORY_ENTRIES;
  FAutoFlush := True;
  FIncludeTimestamp := True;
  FIncludeThreadId := False;
  FDateTimeFormat := DEFAULT_LOG_DATE_FORMAT;
  FLogToFile := False;
end;

destructor TSSLLogger.Destroy;
begin
  if FLogToFile then
  begin
    FlushToFile;
    CloseFile(FLogFile);
  end;
  
  ClearMemoryLog;
  FMemoryLog.Free;
  FLock.Free;
  inherited;
end;

function TSSLLogger.GetLevel: TSSLLogLevel;
begin
  Result := FLevel;
end;

procedure TSSLLogger.SetLevel(aLevel: TSSLLogLevel);
begin
  FLevel := aLevel;
end;

function TSSLLogger.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TSSLLogger.SetEnabled(aEnabled: Boolean);
begin
  FEnabled := aEnabled;
end;

procedure TSSLLogger.Log(aLevel: TSSLLogLevel; const aMessage: string; const aData: string);
var
  LEntry: TSSLLogEntry;
begin
  if not FEnabled or (aLevel > FLevel) or (FTargets = []) then
    Exit;
  
  LEntry.Timestamp := Now;
  LEntry.Level := aLevel;
  LEntry.ThreadId := GetCurrentThreadId;
  LEntry.Module := FModule;
  LEntry.Message := aMessage;
  LEntry.Data := aData;
  
  FLock.Enter;
  try
    DoLog(LEntry);
  finally
    FLock.Leave;
  end;
end;

procedure TSSLLogger.LogFatal(const aMessage: string; const aData: string);
begin
  Log(sllFatal, aMessage, aData);
end;

procedure TSSLLogger.LogError(const aMessage: string; const aData: string);
begin
  Log(sllError, aMessage, aData);
end;

procedure TSSLLogger.LogWarning(const aMessage: string; const aData: string);
begin
  Log(sllWarning, aMessage, aData);
end;

procedure TSSLLogger.LogInfo(const aMessage: string; const aData: string);
begin
  Log(sllInfo, aMessage, aData);
end;

procedure TSSLLogger.LogDebug(const aMessage: string; const aData: string);
begin
  Log(sllDebug, aMessage, aData);
end;

procedure TSSLLogger.LogTrace(const aMessage: string; const aData: string);
begin
  Log(sllTrace, aMessage, aData);
end;

procedure TSSLLogger.DoLog(const aEntry: TSSLLogEntry);
begin
  if sltConsole in FTargets then
    WriteToConsole(aEntry);
  
  if sltFile in FTargets then
    WriteToFile(aEntry);
  
  if sltMemory in FTargets then
    WriteToMemory(aEntry);
  
  if (sltCallback in FTargets) and Assigned(FCallback) then
    InvokeCallback(aEntry);
end;

procedure TSSLLogger.WriteToConsole(const aEntry: TSSLLogEntry);
var
  LOutput: string;
begin
  LOutput := FormatLogEntry(aEntry);
  
  {$IFDEF WINDOWS}
  // 在 Windows 上可以使用颜色
  case aEntry.Level of
    sllFatal, sllError: 
      begin
        TextColor(Red);
        WriteLn(LOutput);
        NormVideo;
      end;
    sllWarning:
      begin
        TextColor(Yellow);
        WriteLn(LOutput);
        NormVideo;
      end;
    sllDebug, sllTrace:
      begin
        TextColor(LightGray);
        WriteLn(LOutput);
        NormVideo;
      end;
    else
      WriteLn(LOutput);
  end;
  {$ELSE}
  WriteLn(LOutput);
  {$ENDIF}
end;

procedure TSSLLogger.WriteToFile(const aEntry: TSSLLogEntry);
var
  LOutput: string;
begin
  if not FLogToFile then
    Exit;
  
  LOutput := FormatLogEntry(aEntry);
  WriteLn(FLogFile, LOutput);
  
  if FAutoFlush then
    Flush(FLogFile);
  
  CheckRotateLogFile;
end;

procedure TSSLLogger.WriteToMemory(const aEntry: TSSLLogEntry);
var
  LEntryPtr: PSSLLogEntry;
begin
  if FMemoryLog.Count >= FMaxMemoryEntries then
  begin
    // 删除最旧的条目
    LEntryPtr := PSSLLogEntry(FMemoryLog[0]);
    Dispose(LEntryPtr);
    FMemoryLog.Delete(0);
  end;
  
  New(LEntryPtr);
  LEntryPtr^ := aEntry;
  FMemoryLog.Add(LEntryPtr);
end;

procedure TSSLLogger.InvokeCallback(const aEntry: TSSLLogEntry);
begin
  if Assigned(FCallback) then
    FCallback(aEntry);
end;

function TSSLLogger.FormatLogEntry(const aEntry: TSSLLogEntry): string;
var
  LSB: TStringBuilder;
begin
  LSB := TStringBuilder.Create;
  try
    // 时间戳
    if FIncludeTimestamp then
      LSB.Append('[').Append(FormatDateTime(FDateTimeFormat, aEntry.Timestamp)).Append('] ');
    
    // 日志级别
    LSB.Append('[').Append(LOG_LEVEL_NAMES[aEntry.Level]).Append('] ');
    
    // 线程 ID
    if FIncludeThreadId then
      LSB.Append('[T:').Append(IntToStr(aEntry.ThreadId)).Append('] ');
    
    // 模块名
    if aEntry.Module <> '' then
      LSB.Append('[').Append(aEntry.Module).Append('] ');
    
    // 消息
    LSB.Append(aEntry.Message);
    
    // 附加数据
    if aEntry.Data <> '' then
      LSB.Append(' | ').Append(aEntry.Data);
    
    Result := LSB.ToString;
  finally
    LSB.Free;
  end;
end;

procedure TSSLLogger.CheckRotateLogFile;
var
  LFileSize: Int64;
  LNewFileName: string;
  LStream: TFileStream;
begin
  // 检查文件大小，如果超过 10MB 则轮转
  if not FLogToFile then
    Exit;
  
  Flush(FLogFile);
  if not FileExists(FLogFileName) then
    Exit;
  
  LStream := TFileStream.Create(FLogFileName, fmOpenRead or fmShareDenyNone);
  try
    LFileSize := LStream.Size;
  finally
    LStream.Free;
  end;
  
  if LFileSize <= 10 * 1024 * 1024 then
    Exit;
  
  CloseFile(FLogFile);
  
  // 生成新文件名
  LNewFileName := ChangeFileExt(FLogFileName, 
    '.' + FormatDateTime('yyyymmdd_hhnnss', Now) + DEFAULT_LOG_FILE_EXT);
  RenameFile(FLogFileName, LNewFileName);
  
  // 重新打开日志文件
  AssignFile(FLogFile, FLogFileName);
  Rewrite(FLogFile);
  FLogToFile := True;
end;

procedure TSSLLogger.SetTargets(aTargets: TSSLLogTargets);
begin
  FTargets := aTargets;
end;

procedure TSSLLogger.SetLogFile(const aFileName: string);
begin
  if FLogToFile then
  begin
    CloseFile(FLogFile);
    FLogToFile := False;
  end;
  
  FLogFileName := aFileName;
  if aFileName <> '' then
  begin
    AssignFile(FLogFile, aFileName);
    if FileExists(aFileName) then
      Append(FLogFile)
    else
      Rewrite(FLogFile);
    FLogToFile := True;
    Include(FTargets, sltFile);
  end;
end;

procedure TSSLLogger.SetCallback(aCallback: TSSLLogCallback);
begin
  FCallback := aCallback;
  if Assigned(aCallback) then
    Include(FTargets, sltCallback)
  else
    Exclude(FTargets, sltCallback);
end;

procedure TSSLLogger.SetMaxMemoryEntries(aMax: Integer);
begin
  FMaxMemoryEntries := aMax;
end;

procedure TSSLLogger.SetDateTimeFormat(const aFormat: string);
begin
  FDateTimeFormat := aFormat;
end;

procedure TSSLLogger.ClearMemoryLog;
var
  I: Integer;
  LEntryPtr: PSSLLogEntry;
begin
  for I := 0 to FMemoryLog.Count - 1 do
  begin
    LEntryPtr := PSSLLogEntry(FMemoryLog[I]);
    Dispose(LEntryPtr);
  end;
  FMemoryLog.Clear;
end;

function TSSLLogger.GetMemoryLog: TStringList;
var
  I: Integer;
  LEntryPtr: PSSLLogEntry;
begin
  Result := TStringList.Create;
  FLock.Enter;
  try
    for I := 0 to FMemoryLog.Count - 1 do
    begin
      LEntryPtr := PSSLLogEntry(FMemoryLog[I]);
      Result.Add(FormatLogEntry(LEntryPtr^));
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TSSLLogger.FlushToFile;
var
  I: Integer;
  LEntryPtr: PSSLLogEntry;
begin
  if not FLogToFile or (FMemoryLog.Count = 0) then
    Exit;
  
  FLock.Enter;
  try
    for I := 0 to FMemoryLog.Count - 1 do
    begin
      LEntryPtr := PSSLLogEntry(FMemoryLog[I]);
      WriteLn(FLogFile, FormatLogEntry(LEntryPtr^));
    end;
    Flush(FLogFile);
  finally
    FLock.Leave;
  end;
end;

{ TSSLLogManager }

constructor TSSLLogManager.CreateInstance;
begin
  inherited Create;
  FLoggers := TStringList.Create;
  FLoggers.OwnsObjects := True;
  FGlobalLogger := TSSLLogger.Create('GLOBAL');
  FDefaultLevel := sllInfo;
  FDefaultTargets := [sltConsole];
  FGlobalLogger.Level := FDefaultLevel;
  FGlobalLogger.SetTargets(FDefaultTargets);
end;

destructor TSSLLogManager.Destroy;
begin
  FLoggers.Free;
  FGlobalLogger := nil;
  inherited;
end;

class function TSSLLogManager.Instance: TSSLLogManager;
begin
  if not Assigned(FInstance) then
    FInstance := TSSLLogManager.CreateInstance;
  Result := FInstance;
end;

class procedure TSSLLogManager.FreeInstance;
begin
  FreeAndNil(FInstance);
end;

function TSSLLogManager.GetLogger(const aModule: string): ISSLLogger;
var
  LIndex: Integer;
begin
  if aModule = '' then
  begin
    Result := FGlobalLogger;
    Exit;
  end;
  
  LIndex := FLoggers.IndexOf(aModule);
  if LIndex >= 0 then
    Result := TSSLLoggerEntry(FLoggers.Objects[LIndex]).Logger
  else
  begin
    Result := TSSLLogger.Create(aModule);
    Result.Level := FDefaultLevel;
    Result.SetTargets(FDefaultTargets);
    RegisterLogger(aModule, Result);
  end;
end;

procedure TSSLLogManager.RegisterLogger(const aModule: string; aLogger: ISSLLogger);
var
  LEntry: TSSLLoggerEntry;
begin
  LEntry := TSSLLoggerEntry.Create;
  LEntry.Logger := aLogger;
  FLoggers.AddObject(aModule, LEntry);
end;

procedure TSSLLogManager.UnregisterLogger(const aModule: string);
var
  LIndex: Integer;
begin
  LIndex := FLoggers.IndexOf(aModule);
  if LIndex >= 0 then
    FLoggers.Delete(LIndex);
end;

procedure TSSLLogManager.SetGlobalLevel(aLevel: TSSLLogLevel);
var
  I: Integer;
  LLogger: ISSLLogger;
begin
  FDefaultLevel := aLevel;
  FGlobalLogger.Level := aLevel;
  
  for I := 0 to FLoggers.Count - 1 do
  begin
    LLogger := TSSLLoggerEntry(FLoggers.Objects[I]).Logger;
    LLogger.Level := aLevel;
  end;
end;

procedure TSSLLogManager.SetGlobalTargets(aTargets: TSSLLogTargets);
var
  I: Integer;
  LLogger: ISSLLogger;
begin
  FDefaultTargets := aTargets;
  FGlobalLogger.SetTargets(aTargets);
  
  for I := 0 to FLoggers.Count - 1 do
  begin
    LLogger := TSSLLoggerEntry(FLoggers.Objects[I]).Logger;
    LLogger.SetTargets(aTargets);
  end;
end;

procedure TSSLLogManager.SetGlobalLogFile(const aFileName: string);
begin
  FGlobalLogger.SetLogFile(aFileName);
end;

procedure TSSLLogManager.EnableAll;
var
  I: Integer;
  LLogger: ISSLLogger;
begin
  FGlobalLogger.Enabled := True;
  
  for I := 0 to FLoggers.Count - 1 do
  begin
    LLogger := TSSLLoggerEntry(FLoggers.Objects[I]).Logger;
    LLogger.Enabled := True;
  end;
end;

procedure TSSLLogManager.DisableAll;
var
  I: Integer;
  LLogger: ISSLLogger;
begin
  FGlobalLogger.Enabled := False;
  
  for I := 0 to FLoggers.Count - 1 do
  begin
    LLogger := TSSLLoggerEntry(FLoggers.Objects[I]).Logger;
    LLogger.Enabled := False;
  end;
end;

{ TSSLDebugger }

class procedure TSSLDebugger.Initialize;
begin
  if not Assigned(FLogger) then
    FLogger := TSSLLogManager.Instance.GetLogger('DEBUG');
  FEnabled := {$IFDEF DEBUG}True{$ELSE}False{$ENDIF};
  FTraceEnabled := False;
  FHexDumpEnabled := False;
end;

class function TSSLDebugger.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

class procedure TSSLDebugger.SetEnabled(aEnabled: Boolean);
begin
  FEnabled := aEnabled;
end;

class function TSSLDebugger.GetTraceEnabled: Boolean;
begin
  Result := FTraceEnabled;
end;

class procedure TSSLDebugger.SetTraceEnabled(aEnabled: Boolean);
begin
  FTraceEnabled := aEnabled;
end;

class function TSSLDebugger.GetHexDumpEnabled: Boolean;
begin
  Result := FHexDumpEnabled;
end;

class procedure TSSLDebugger.SetHexDumpEnabled(aEnabled: Boolean);
begin
  FHexDumpEnabled := aEnabled;
end;

class procedure TSSLDebugger.Debug(const aMessage: string);
begin
  if FEnabled and Assigned(FLogger) then
    FLogger.LogDebug(aMessage);
end;

class procedure TSSLDebugger.DebugFmt(const aFormat: string; const aArgs: array of const);
begin
  Debug(Format(aFormat, aArgs));
end;

class procedure TSSLDebugger.Trace(const aProc, aMessage: string);
begin
  if FEnabled and FTraceEnabled and Assigned(FLogger) then
    FLogger.LogTrace(aProc + ': ' + aMessage);
end;

class procedure TSSLDebugger.TraceFmt(const aProc, aFormat: string; const aArgs: array of const);
begin
  Trace(aProc, Format(aFormat, aArgs));
end;

class procedure TSSLDebugger.HexDump(const aData: TBytes; const aLabel: string);
var
  LDump: string;
begin
  if FEnabled and FHexDumpEnabled and Assigned(FLogger) then
  begin
    LDump := TSSLDebugUtils.DumpBytes(aData);
    if aLabel <> '' then
      FLogger.LogDebug('HexDump [' + aLabel + ']:', LDump)
    else
      FLogger.LogDebug('HexDump:', LDump);
  end;
end;

class procedure TSSLDebugger.DumpConfig(const aConfig: TSSLConfig);
begin
  if FEnabled and Assigned(FLogger) then
    FLogger.LogDebug('Config', TSSLDebugUtils.DumpSSLConfig(aConfig));
end;

class procedure TSSLDebugger.DumpCertificate(const aInfo: TSSLCertificateInfo);
begin
  if FEnabled and Assigned(FLogger) then
    FLogger.LogDebug('Certificate', TSSLDebugUtils.DumpCertificateInfo(aInfo));
end;

class procedure TSSLDebugger.DumpConnection(const aInfo: TSSLConnectionInfo);
begin
  if FEnabled and Assigned(FLogger) then
    FLogger.LogDebug('Connection', TSSLDebugUtils.DumpConnectionInfo(aInfo));
end;

class procedure TSSLDebugger.DumpException(E: Exception);
begin
  if FEnabled and Assigned(FLogger) then
  begin
    FLogger.LogError('Exception: ' + E.ClassName + ': ' + E.Message);
    if E is ESSLException then
      FLogger.LogError('SSL Details', TSSLUtils.GetErrorDetails(ESSLException(E)));
  end;
end;

class function TSSLDebugger.StartTimer(const aLabel: string): Int64;
begin
  Result := GetTickCount64;
  if FEnabled and FTraceEnabled and Assigned(FLogger) then
    FLogger.LogTrace('Timer Start: ' + aLabel);
end;

class procedure TSSLDebugger.StopTimer(aStartTime: Int64; const aLabel: string);
var
  LElapsed: Int64;
begin
  if FEnabled and FTraceEnabled and Assigned(FLogger) then
  begin
    LElapsed := GetTickCount64 - aStartTime;
    FLogger.LogTrace(Format('Timer Stop: %s - Elapsed: %d ms', [aLabel, LElapsed]));
  end;
end;

class procedure TSSLDebugger.Assert(aCondition: Boolean; const aMessage: string);
begin
  if not aCondition then
  begin
    if FEnabled and Assigned(FLogger) then
      FLogger.LogFatal('Assertion Failed: ' + aMessage);
    raise ESSLException.Create('Assertion Failed: ' + aMessage);
  end;
end;

class procedure TSSLDebugger.AssertNotNil(aObject: TObject; const aName: string);
begin
  Assert(Assigned(aObject), aName + ' is nil');
end;

{ TSSLProfiler }

constructor TSSLProfiler.CreateInstance;
begin
  inherited Create;
  FEntries := TStringList.Create;
  FEntries.OwnsObjects := True;
  FLock := TCriticalSection.Create;
  FEnabled := True;
end;

destructor TSSLProfiler.Destroy;
begin
  FEntries.Free;
  FLock.Free;
  inherited;
end;

class function TSSLProfiler.Instance: TSSLProfiler;
begin
  if not Assigned(FInstance) then
    FInstance := TSSLProfiler.CreateInstance;
  Result := FInstance;
end;

class procedure TSSLProfiler.FreeInstance;
begin
  FreeAndNil(FInstance);
end;

function TSSLProfiler.GetEntry(const aName: string): TProfileEntry;
var
  LIndex: Integer;
  LEntryPtr: ^TProfileEntry;
begin
  LIndex := FEntries.IndexOf(aName);
  if LIndex >= 0 then
  begin
    LEntryPtr := Pointer(FEntries.Objects[LIndex]);
    Result := LEntryPtr^;
  end
  else
  begin
    FillChar(Result, SizeOf(Result), 0);
    Result.Name := aName;
    Result.MinTime := High(Int64);
  end;
end;

procedure TSSLProfiler.UpdateEntry(const aName: string; aElapsed: Int64);
var
  LIndex: Integer;
  LEntryPtr: ^TProfileEntry;
begin
  FLock.Enter;
  try
    LIndex := FEntries.IndexOf(aName);
    if LIndex < 0 then
    begin
      New(LEntryPtr);
      FillChar(LEntryPtr^, SizeOf(TProfileEntry), 0);
      LEntryPtr^.Name := aName;
      LEntryPtr^.MinTime := High(Int64);
      FEntries.AddObject(aName, TObject(LEntryPtr));
    end
    else
      LEntryPtr := Pointer(FEntries.Objects[LIndex]);
    
    Inc(LEntryPtr^.Count);
    Inc(LEntryPtr^.TotalTime, aElapsed);
    if aElapsed < LEntryPtr^.MinTime then
      LEntryPtr^.MinTime := aElapsed;
    if aElapsed > LEntryPtr^.MaxTime then
      LEntryPtr^.MaxTime := aElapsed;
  finally
    FLock.Leave;
  end;
end;

function TSSLProfiler.StartMeasure(const aName: string): Int64;
begin
  if FEnabled then
    Result := GetTickCount64
  else
    Result := 0;
end;

procedure TSSLProfiler.EndMeasure(const aName: string; aStartTime: Int64);
var
  LElapsed: Int64;
begin
  if not FEnabled or (aStartTime = 0) then
    Exit;
  
  LElapsed := GetTickCount64 - aStartTime;
  UpdateEntry(aName, LElapsed);
end;

procedure TSSLProfiler.Measure(const aName: string; aProc: TProcedure);
var
  LStart: Int64;
begin
  LStart := StartMeasure(aName);
  try
    aProc();
  finally
    EndMeasure(aName, LStart);
  end;
end;

function TSSLProfiler.GetReport: string;
var
  I: Integer;
  LEntryPtr: ^TProfileEntry;
  LAvg: Double;
  LSB: TSSLStringBuilder;
begin
  LSB := TSSLStringBuilder.Create;
  try
    LSB.AppendLine('性能分析报告');
    LSB.AppendLine(StringOfChar('=', 80));
    LSB.AppendFormat('%-30s %10s %10s %10s %10s %10s', 
      ['名称', '次数', '总计(ms)', '平均(ms)', '最小(ms)', '最大(ms)']);
    LSB.AppendLine(StringOfChar('-', 80));
    
    FLock.Enter;
    try
      for I := 0 to FEntries.Count - 1 do
      begin
        LEntryPtr := Pointer(FEntries.Objects[I]);
        if LEntryPtr^.Count > 0 then
          LAvg := LEntryPtr^.TotalTime / LEntryPtr^.Count
        else
          LAvg := 0;
        
        LSB.AppendFormat('%-30s %10d %10d %10.2f %10d %10d',
          [LEntryPtr^.Name, LEntryPtr^.Count, LEntryPtr^.TotalTime,
          LAvg, LEntryPtr^.MinTime, LEntryPtr^.MaxTime]);
      end;
    finally
      FLock.Leave;
    end;
    
    LSB.AppendLine(StringOfChar('=', 80));
    Result := LSB.ToString;
  finally
    LSB.Free;
  end;
end;

function TSSLProfiler.GetReportAsJSON: string;
var
  I: Integer;
  LEntryPtr: ^TProfileEntry;
  LRoot, LEntry: TJSONObject;
  LEntries: TJSONArray;
begin
  LRoot := TJSONObject.Create;
  try
    LRoot.Add('timestamp', DateTimeToStr(Now));
    LEntries := TJSONArray.Create;
    
    FLock.Enter;
    try
      for I := 0 to FEntries.Count - 1 do
      begin
        LEntryPtr := Pointer(FEntries.Objects[I]);
        LEntry := TJSONObject.Create;
        LEntry.Add('name', LEntryPtr^.Name);
        LEntry.Add('count', LEntryPtr^.Count);
        LEntry.Add('total_ms', LEntryPtr^.TotalTime);
        if LEntryPtr^.Count > 0 then
          LEntry.Add('avg_ms', LEntryPtr^.TotalTime / LEntryPtr^.Count)
        else
          LEntry.Add('avg_ms', 0.0);
        LEntry.Add('min_ms', LEntryPtr^.MinTime);
        LEntry.Add('max_ms', LEntryPtr^.MaxTime);
        LEntries.Add(LEntry);
      end;
    finally
      FLock.Leave;
    end;
    
    LRoot.Add('entries', LEntries);
    Result := LRoot.AsJSON;
  finally
    LRoot.Free;
  end;
end;

procedure TSSLProfiler.Clear;
var
  I: Integer;
  LEntryPtr: ^TProfileEntry;
begin
  FLock.Enter;
  try
    for I := 0 to FEntries.Count - 1 do
    begin
      LEntryPtr := Pointer(FEntries.Objects[I]);
      Dispose(LEntryPtr);
    end;
    FEntries.Clear;
  finally
    FLock.Leave;
  end;
end;

{ 全局便捷函数 }

function SSLLogger(const aModule: string): ISSLLogger;
begin
  Result := TSSLLogManager.Instance.GetLogger(aModule);
end;

procedure SSLLog(aLevel: TSSLLogLevel; const aMessage: string; const aData: string);
begin
  TSSLLogManager.Instance.GlobalLogger.Log(aLevel, aMessage, aData);
end;

procedure SSLLogError(const aMessage: string; const aData: string);
begin
  TSSLLogManager.Instance.GlobalLogger.LogError(aMessage, aData);
end;

procedure SSLLogWarning(const aMessage: string; const aData: string);
begin
  TSSLLogManager.Instance.GlobalLogger.LogWarning(aMessage, aData);
end;

procedure SSLLogInfo(const aMessage: string; const aData: string);
begin
  TSSLLogManager.Instance.GlobalLogger.LogInfo(aMessage, aData);
end;

procedure SSLLogDebug(const aMessage: string; const aData: string);
begin
  TSSLLogManager.Instance.GlobalLogger.LogDebug(aMessage, aData);
end;

procedure SSLLogTrace(const aMessage: string; const aData: string);
begin
  TSSLLogManager.Instance.GlobalLogger.LogTrace(aMessage, aData);
end;

procedure SSLDebug(const aMessage: string);
begin
  TSSLDebugger.Debug(aMessage);
end;

procedure SSLTrace(const aProc, aMessage: string);
begin
  TSSLDebugger.Trace(aProc, aMessage);
end;

procedure SSLHexDump(const aData: TBytes; const aLabel: string);
begin
  TSSLDebugger.HexDump(aData, aLabel);
end;

function SSLStartTimer(const aLabel: string): Int64;
begin
  Result := TSSLDebugger.StartTimer(aLabel);
end;

procedure SSLStopTimer(aStartTime: Int64; const aLabel: string);
begin
  TSSLDebugger.StopTimer(aStartTime, aLabel);
end;

initialization
  TSSLDebugger.Initialize;

finalization
  TSSLProfiler.FreeInstance;
  TSSLLogManager.FreeInstance;

end.
