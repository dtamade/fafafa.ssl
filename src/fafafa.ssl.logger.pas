unit fafafa.ssl.logger;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{ fafafa.ssl 生产级日志系统
  
  功能：
  - 多级别日志（Debug, Info, Warning, Error, Critical）
  - 文件输出
  - 自动日志轮转
  - 线程安全
  - 格式化输出
}

interface

uses
  SysUtils, Classes, SyncObjs;

type
  { 日志级别 }
  TLogLevel = (
    llDebug,      // 调试信息
    llInfo,       // 一般信息
    llWarning,    // 警告
    llError,      // 错误
    llCritical    // 严重错误
  );

  { 日志接口 }
  ILogger = interface
    ['{8F4A1B2C-3D5E-4F7A-8B9C-0D1E2F3A4B5C}']
    procedure Debug(const AMsg: string);
    procedure Info(const AMsg: string);
    procedure Warning(const AMsg: string);
    procedure Error(const AMsg: string);
    procedure Critical(const AMsg: string);
    procedure Log(ALevel: TLogLevel; const AMsg: string); virtual;
    procedure SetMinLevel(ALevel: TLogLevel);
    function GetMinLevel: TLogLevel;
    procedure Flush;
  end;

  { 日志实现 }
  TLogger = class(TInterfacedObject, ILogger)
  private
    FLogFile: string;
    FMinLevel: TLogLevel;
    FMaxSize: Int64;
    FMaxFiles: Integer;
    FStream: TFileStream;
    FLock: TCriticalSection;
    FCurrentSize: Int64;
    
    procedure WriteToFile(const AMsg: string);
    procedure RotateLogFile;
    function FormatLogMessage(ALevel: TLogLevel; const AMsg: string): string;
  public
    constructor Create(const ALogFile: string;
      AMinLevel: TLogLevel = llInfo;
      AMaxSize: Int64 = 10*1024*1024;  // 10MB
      AMaxFiles: Integer = 5);
    destructor Destroy; override;
    
    // ILogger接口实现
    procedure Debug(const AMsg: string);
    procedure Info(const AMsg: string);
    procedure Warning(const AMsg: string);
    procedure Error(const AMsg: string);
    procedure Critical(const AMsg: string);
    procedure Log(ALevel: TLogLevel; const AMsg: string); virtual;
    procedure SetMinLevel(ALevel: TLogLevel);
    function GetMinLevel: TLogLevel;
    procedure Flush;
  end;

  { 控制台日志（同时输出到控制台和文件） }
  TConsoleLogger = class(TLogger)
  private
    procedure WriteToConsole(ALevel: TLogLevel; const AMsg: string);
  public
    procedure Log(ALevel: TLogLevel; const AMsg: string); override;
  end;

  { 全局日志函数 }
  function GlobalLogger: ILogger;
  procedure SetGlobalLogger(ALogger: ILogger);

implementation

uses
  DateUtils
  {$IFDEF UNIX}, Unix{$ENDIF}
  {$IFDEF WINDOWS}, Windows{$ENDIF};

var
  FGlobalLogger: ILogger = nil;

function GlobalLogger: ILogger;
begin
  if FGlobalLogger = nil then
    FGlobalLogger := TLogger.Create('fafafa.ssl.log');
  Result := FGlobalLogger;
end;

procedure SetGlobalLogger(ALogger: ILogger);
begin
  FGlobalLogger := ALogger;
end;

{ TLogger }

constructor TLogger.Create(const ALogFile: string; AMinLevel: TLogLevel;
  AMaxSize: Int64; AMaxFiles: Integer);
var
  LDir: string;
begin
  inherited Create;
  FLogFile := ALogFile;
  FMinLevel := AMinLevel;
  FMaxSize := AMaxSize;
  FMaxFiles := AMaxFiles;
  FLock := TCriticalSection.Create;
  FCurrentSize := 0;
  FStream := nil;
  
  // 确保日志目录存在
  LDir := ExtractFilePath(ALogFile);
  if (LDir <> '') and not DirectoryExists(LDir) then
    ForceDirectories(LDir);
  
  // 打开日志文件（RAII合规：使用临时变量避免泄漏）
  var LNewStream: TFileStream;
  LNewStream := nil;
  try
    if FileExists(FLogFile) then
    begin
      LNewStream := TFileStream.Create(FLogFile, fmOpenReadWrite or fmShareDenyWrite);
      try
        LNewStream.Seek(0, soEnd);
        FCurrentSize := LNewStream.Size;
        FStream := LNewStream;  // 成功后赋值
        LNewStream := nil;  // 转移所有权，防止finally释放
      finally
        FreeAndNil(LNewStream);  // 失败时释放临时流
      end;
    end
    else
    begin
      LNewStream := TFileStream.Create(FLogFile, fmCreate or fmShareDenyWrite);
      try
        FCurrentSize := 0;
        FStream := LNewStream;  // 成功后赋值
        LNewStream := nil;  // 转移所有权
      finally
        FreeAndNil(LNewStream);
      end;
    end;
  except
    on E: Exception do
    begin
      // 如果无法打开日志文件，继续运行但不记录日志
      FStream := nil;
      WriteLn('警告: 无法打开日志文件: ', E.Message);
    end;
  end;
end;

destructor TLogger.Destroy;
begin
  if Assigned(FStream) then
  begin
    Flush;
    FreeAndNil(FStream);
  end;
  FreeAndNil(FLock);
  inherited;
end;

function TLogger.FormatLogMessage(ALevel: TLogLevel; const AMsg: string): string;
const
  LevelStr: array[TLogLevel] of string = (
    'DEBUG', 'INFO', 'WARN', 'ERROR', 'CRITICAL'
  );
var
  LTimeStr: string;
begin
  LTimeStr := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now);
  Result := Format('[%s] [%s] %s'#13#10, [LTimeStr, LevelStr[ALevel], AMsg]);
end;

procedure TLogger.WriteToFile(const AMsg: string);
var
  LBytes: TBytes;
begin
  if not Assigned(FStream) then
    Exit;
    
  LBytes := TEncoding.UTF8.GetBytes(AMsg);
  FStream.Write(LBytes[0], Length(LBytes));
  Inc(FCurrentSize, Length(LBytes));
  
  // 检查是否需要轮转
  if (FMaxSize > 0) and (FCurrentSize > FMaxSize) then
    RotateLogFile;
end;

procedure TLogger.RotateLogFile;
var
  i: Integer;
  LOldName, LNewName: string;
begin
  if not Assigned(FStream) then
    Exit;
    
  // 关闭当前文件
  FreeAndNil(FStream);
  
  // 删除最老的日志文件
  if FMaxFiles > 0 then
  begin
    LOldName := FLogFile + '.' + IntToStr(FMaxFiles);
    if FileExists(LOldName) then
      DeleteFile(LOldName);
    
    // 重命名现有日志文件
    for i := FMaxFiles - 1 downto 1 do
    begin
      LOldName := FLogFile + '.' + IntToStr(i);
      LNewName := FLogFile + '.' + IntToStr(i + 1);
      if FileExists(LOldName) then
        RenameFile(LOldName, LNewName);
    end;
    
    // 重命名当前日志文件
    if FileExists(FLogFile) then
      RenameFile(FLogFile, FLogFile + '.1');
  end;
  
  // 创建新日志文件（RAII合规：使用临时变量避免泄漏）
  var LNewStream: TFileStream;
  LNewStream := nil;
  try
    LNewStream := TFileStream.Create(FLogFile, fmCreate or fmShareDenyWrite);
    try
      FCurrentSize := 0;
      FStream := LNewStream;  // 成功后赋值
      LNewStream := nil;  // 转移所有权
    finally
      FreeAndNil(LNewStream);
    end;
  except
    FStream := nil;
  end;
end;

procedure TLogger.Log(ALevel: TLogLevel; const AMsg: string);
var
  LFormattedMsg: string;
begin
  if ALevel < FMinLevel then
    Exit;
    
  LFormattedMsg := FormatLogMessage(ALevel, AMsg);
  
  FLock.Enter;
  try
    WriteToFile(LFormattedMsg);
  finally
    FLock.Leave;
  end;
end;

procedure TLogger.Debug(const AMsg: string);
begin
  Log(llDebug, AMsg);
end;

procedure TLogger.Info(const AMsg: string);
begin
  Log(llInfo, AMsg);
end;

procedure TLogger.Warning(const AMsg: string);
begin
  Log(llWarning, AMsg);
end;

procedure TLogger.Error(const AMsg: string);
begin
  Log(llError, AMsg);
end;

procedure TLogger.Critical(const AMsg: string);
begin
  Log(llCritical, AMsg);
end;

procedure TLogger.SetMinLevel(ALevel: TLogLevel);
begin
  FMinLevel := ALevel;
end;

function TLogger.GetMinLevel: TLogLevel;
begin
  Result := FMinLevel;
end;

procedure TLogger.Flush;
begin
  if Assigned(FStream) then
  begin
    FLock.Enter;
    try
      {$IFDEF WINDOWS}
      // Windows下刷新缓冲区
      FlushFileBuffers(FStream.Handle);
      {$ELSE}
      // Unix下使用fsync
      FpFsync(FStream.Handle);
      {$ENDIF}
    finally
      FLock.Leave;
    end;
  end;
end;

{ TConsoleLogger }

procedure TConsoleLogger.WriteToConsole(ALevel: TLogLevel; const AMsg: string);
const
  LevelStr: array[TLogLevel] of string = (
    'DEBUG', 'INFO', 'WARN', 'ERROR', 'CRITICAL'
  );
var
  LTimeStr: string;
begin
  LTimeStr := FormatDateTime('hh:nn:ss', Now);
  WriteLn(Format('[%s] [%s] %s', [LTimeStr, LevelStr[ALevel], AMsg]));
end;

procedure TConsoleLogger.Log(ALevel: TLogLevel; const AMsg: string);
begin
  WriteToConsole(ALevel, AMsg);
  inherited Log(ALevel, AMsg);
end;

end.
