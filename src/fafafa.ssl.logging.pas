{
  fafafa.ssl.logging - Security Event Logging Infrastructure
  
  Provides a centralized interface for logging security-relevant events,
  audits, and alerts.
}

unit fafafa.ssl.logging;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

{$IFDEF USE_SYNCOBJS}
  {$DEFINE HAS_CRITICAL_SECTION}
{$ELSE}
  {$IFDEF UNIX}
    {$DEFINE HAS_PTHREAD_LOCK}
  {$ENDIF}
{$ENDIF}

type
  TSecurityEventLevel = (
    selInfo,      // General information (e.g., context created)
    selWarning,   // Potential issues (e.g., deprecated cipher used)
    selError,     // Operations failed (e.g., handshake failed)
    selAudit,     // Security audit events (e.g., key access, user login)
    selCritical   // Critical security breaches (e.g., integrity check failed)
  );
  
  { Security Logger Interface }
  ISecurityLogger = interface
    ['{A1B2C3D4-E5F6-4789-0123-456789ABCDEF}']
    
    // Generic log
    procedure Log(ALevel: TSecurityEventLevel; const ACategory, AMessage: string);
    
    // Helper for errors
    procedure LogError(const ACategory, AMessage: string; AException: Exception = nil);
    
    // Helper for security audits
    procedure LogAudit(const ACategory, AAction, AUser, ADetails: string);
  end;

  { Base Logger Implementation }
  TBaseLogger = class(TInterfacedObject, ISecurityLogger)
  protected
    function FormatMessage(ALevel: TSecurityEventLevel; const ACategory, AMessage: string): string; virtual;
    procedure WriteLog(const AMessage: string); virtual; abstract;
  public
    procedure Log(ALevel: TSecurityEventLevel; const ACategory, AMessage: string); virtual;
    procedure LogError(const ACategory, AMessage: string; AException: Exception = nil); virtual;
    procedure LogAudit(const ACategory, AAction, AUser, ADetails: string); virtual;
  end;

  { Console Logger - Writes to StdOut/StdErr }
  TConsoleLogger = class(TBaseLogger)
  protected
    procedure WriteLog(const AMessage: string); override;
  end;

  { File Logger - Writes to a secure log file }
  TFileLogger = class(TBaseLogger)
  private
    FFileName: string;
    FLock: TRTLCriticalSection;
  protected
    procedure WriteLog(const AMessage: string); override;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
  end;

  { Composite Logger - Broadcasts to multiple loggers }
  TCompositeLogger = class(TInterfacedObject, ISecurityLogger)
  private
    FLoggers: TInterfaceList;
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure AddLogger(ALogger: ISecurityLogger);
    procedure RemoveLogger(ALogger: ISecurityLogger);
    
    procedure Log(ALevel: TSecurityEventLevel; const ACategory, AMessage: string);
    procedure LogError(const ACategory, AMessage: string; AException: Exception = nil);
    procedure LogAudit(const ACategory, AAction, AUser, ADetails: string);
  end;

  { Global Access }
  TSecurityLog = class
  private
    class var FLogger: ISecurityLogger;
    class var FDefaultLogger: ISecurityLogger;
    class constructor Create;
  public
    class property Logger: ISecurityLogger read FLogger write FLogger;
    
    // Convenience methods forwarding to global Logger
    class procedure Info(const ACategory, AMessage: string);
    class procedure Warning(const ACategory, AMessage: string);
    class procedure Error(const ACategory, AMessage: string; AException: Exception = nil);
    class procedure Audit(const ACategory, AAction, AUser, ADetails: string);
    class procedure Critical(const ACategory, AMessage: string);
  end;

implementation

{ TBaseLogger }

function TBaseLogger.FormatMessage(ALevel: TSecurityEventLevel; const ACategory, AMessage: string): string;
var
  LLevelStr: string;
  LTimeStr: string;
begin
  case ALevel of
    selInfo:     LLevelStr := 'INFO';
    selWarning:  LLevelStr := 'WARN';
    selError:    LLevelStr := 'ERROR';
    selAudit:    LLevelStr := 'AUDIT';
    selCritical: LLevelStr := 'CRIT';
  end;
  
  LTimeStr := FormatDateTime('yyyy-mm-dd hh:nn:ss.z', Now);
  
  // Format: [Time] [Level] [Category] Message
  Result := Format('[%s] [%s] [%s] %s', [LTimeStr, LLevelStr, ACategory, AMessage]);
end;

procedure TBaseLogger.Log(ALevel: TSecurityEventLevel; const ACategory, AMessage: string);
begin
  WriteLog(FormatMessage(ALevel, ACategory, AMessage));
end;

procedure TBaseLogger.LogError(const ACategory, AMessage: string; AException: Exception);
var
  LMsg: string;
begin
  LMsg := AMessage;
  if AException <> nil then
    LMsg := LMsg + ' (Exception: ' + AException.ClassName + ': ' + AException.Message + ')';
  Log(selError, ACategory, LMsg);
end;

procedure TBaseLogger.LogAudit(const ACategory, AAction, AUser, ADetails: string);
var
  LMsg: string;
begin
  LMsg := Format('Action=%s User=%s Details=%s', [AAction, AUser, ADetails]);
  Log(selAudit, ACategory, LMsg);
end;

{ TConsoleLogger }

procedure TConsoleLogger.WriteLog(const AMessage: string);
begin
  // Thread-safe console writing is tricky in standard Pascal, but WriteLn is usually atomic enough for lines
  // or we could use a lock if needed. For now, simple WriteLn.
  WriteLn(AMessage);
end;

{ TFileLogger }

constructor TFileLogger.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
  InitCriticalSection(FLock);
end;

destructor TFileLogger.Destroy;
begin
  DoneCriticalSection(FLock);
  inherited;
end;

procedure TFileLogger.WriteLog(const AMessage: string);
var
  LFile: TextFile;
begin
  EnterCriticalSection(FLock);
  try
    AssignFile(LFile, FFileName);
    if FileExists(FFileName) then
      Append(LFile)
    else
      Rewrite(LFile);
    try
      WriteLn(LFile, AMessage);
    finally
      CloseFile(LFile);
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

{ TCompositeLogger }

constructor TCompositeLogger.Create;
begin
  inherited;
  FLoggers := TInterfaceList.Create;
  InitCriticalSection(FLock);
end;

destructor TCompositeLogger.Destroy;
begin
  DoneCriticalSection(FLock);
  FLoggers.Free;
  inherited;
end;

procedure TCompositeLogger.AddLogger(ALogger: ISecurityLogger);
begin
  EnterCriticalSection(FLock);
  try
    if FLoggers.IndexOf(ALogger) < 0 then
      FLoggers.Add(ALogger);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TCompositeLogger.RemoveLogger(ALogger: ISecurityLogger);
begin
  EnterCriticalSection(FLock);
  try
    FLoggers.Remove(ALogger);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TCompositeLogger.Log(ALevel: TSecurityEventLevel; const ACategory, AMessage: string);
var
  I: Integer;
begin
  EnterCriticalSection(FLock);
  try
    for I := 0 to FLoggers.Count - 1 do
      (FLoggers[I] as ISecurityLogger).Log(ALevel, ACategory, AMessage);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TCompositeLogger.LogError(const ACategory, AMessage: string; AException: Exception);
var
  I: Integer;
begin
  EnterCriticalSection(FLock);
  try
    for I := 0 to FLoggers.Count - 1 do
      (FLoggers[I] as ISecurityLogger).LogError(ACategory, AMessage, AException);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TCompositeLogger.LogAudit(const ACategory, AAction, AUser, ADetails: string);
var
  I: Integer;
begin
  EnterCriticalSection(FLock);
  try
    for I := 0 to FLoggers.Count - 1 do
      (FLoggers[I] as ISecurityLogger).LogAudit(ACategory, AAction, AUser, ADetails);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

{ TSecurityLog }

class constructor TSecurityLog.Create;
begin
  // Default to Console Logger
  FDefaultLogger := TConsoleLogger.Create;
  FLogger := FDefaultLogger;
end;

class procedure TSecurityLog.Info(const ACategory, AMessage: string);
begin
  if FLogger <> nil then FLogger.Log(selInfo, ACategory, AMessage);
end;

class procedure TSecurityLog.Warning(const ACategory, AMessage: string);
begin
  if FLogger <> nil then FLogger.Log(selWarning, ACategory, AMessage);
end;

class procedure TSecurityLog.Error(const ACategory, AMessage: string; AException: Exception);
begin
  if FLogger <> nil then FLogger.LogError(ACategory, AMessage, AException);
end;

class procedure TSecurityLog.Audit(const ACategory, AAction, AUser, ADetails: string);
begin
  if FLogger <> nil then FLogger.LogAudit(ACategory, AAction, AUser, ADetails);
end;

class procedure TSecurityLog.Critical(const ACategory, AMessage: string);
begin
  if FLogger <> nil then FLogger.Log(selCritical, ACategory, AMessage);
end;

end.
