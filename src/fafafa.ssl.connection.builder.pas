{
  fafafa.ssl.connection.builder - Fluent SSL Connection Builder
  
  Provides a modern, fluent API for SSL connection configuration, inspired by
  Rust's rustls ConnectionConfig pattern.
  
  Features:
  - Method chaining for readable code
  - Type-safe configuration
  - Safe defaults built-in
  - Separate client/server building
  
  Version: 1.0
  Created: 2025-12-12
}

unit fafafa.ssl.connection.builder;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base;

type
  { Forward declarations }
  ISSLConnectionBuilder = interface;

  { SSL Connection Builder Interface - Fluent API for connection configuration }
  ISSLConnectionBuilder = interface
    ['{A8B9C0D1-E2F3-4567-8901-23456789ABCD}']
    
    // Context configuration
    function WithContext(AContext: ISSLContext): ISSLConnectionBuilder;
    
    // Socket configuration
    function WithSocket(ASocket: THandle): ISSLConnectionBuilder;
    function WithStream(AStream: TStream): ISSLConnectionBuilder;
    
    // Connection options
    function WithTimeout(AMs: Integer): ISSLConnectionBuilder;
    function WithBlocking(ABlocking: Boolean): ISSLConnectionBuilder;
    function WithHostname(const AHostname: string): ISSLConnectionBuilder;
    
    // Session management
    function WithSession(ASession: ISSLSession): ISSLConnectionBuilder;
    function WithSessionReuse(AEnabled: Boolean): ISSLConnectionBuilder;
    
    // Build methods
    function BuildClient: ISSLConnection;
    function BuildServer: ISSLConnection;
    
    // Try-pattern build methods (non-throwing)
    function TryBuildClient(out AConnection: ISSLConnection): TSSLOperationResult;
    function TryBuildServer(out AConnection: ISSLConnection): TSSLOperationResult;
  end;

  { Factory class for creating connection builders }
  TSSLConnectionBuilder = class
  public
    class function Create: ISSLConnectionBuilder; static;
    class function CreateWithContext(AContext: ISSLContext): ISSLConnectionBuilder; static;
  end;

implementation

uses
  fafafa.ssl.factory,
  fafafa.ssl.exceptions;

type
  { Internal builder implementation }
  TSSLConnectionBuilderImpl = class(TInterfacedObject, ISSLConnectionBuilder)
  private
    FContext: ISSLContext;
    FSocket: THandle;
    FStream: TStream;
    FTimeout: Integer;
    FBlocking: Boolean;
    FHostname: string;
    FSession: ISSLSession;
    FSessionReuse: Boolean;
    FUseSocket: Boolean;
  public
    constructor Create;
    
    // ISSLConnectionBuilder
    function WithContext(AContext: ISSLContext): ISSLConnectionBuilder;
    function WithSocket(ASocket: THandle): ISSLConnectionBuilder;
    function WithStream(AStream: TStream): ISSLConnectionBuilder;
    function WithTimeout(AMs: Integer): ISSLConnectionBuilder;
    function WithBlocking(ABlocking: Boolean): ISSLConnectionBuilder;
    function WithHostname(const AHostname: string): ISSLConnectionBuilder;
    function WithSession(ASession: ISSLSession): ISSLConnectionBuilder;
    function WithSessionReuse(AEnabled: Boolean): ISSLConnectionBuilder;
    function BuildClient: ISSLConnection;
    function BuildServer: ISSLConnection;
    function TryBuildClient(out AConnection: ISSLConnection): TSSLOperationResult;
    function TryBuildServer(out AConnection: ISSLConnection): TSSLOperationResult;
  end;

{ TSSLConnectionBuilder }

class function TSSLConnectionBuilder.Create: ISSLConnectionBuilder;
begin
  Result := TSSLConnectionBuilderImpl.Create;
end;

class function TSSLConnectionBuilder.CreateWithContext(AContext: ISSLContext): ISSLConnectionBuilder;
begin
  Result := TSSLConnectionBuilderImpl.Create.WithContext(AContext);
end;

{ TSSLConnectionBuilderImpl }

constructor TSSLConnectionBuilderImpl.Create;
begin
  inherited Create;
  FContext := nil;
  FSocket := 0;
  FStream := nil;
  FTimeout := SSL_DEFAULT_HANDSHAKE_TIMEOUT;
  FBlocking := True;
  FHostname := '';
  FSession := nil;
  FSessionReuse := True;
  FUseSocket := True;
end;

function TSSLConnectionBuilderImpl.WithContext(AContext: ISSLContext): ISSLConnectionBuilder;
begin
  FContext := AContext;
  Result := Self;
end;

function TSSLConnectionBuilderImpl.WithSocket(ASocket: THandle): ISSLConnectionBuilder;
begin
  FSocket := ASocket;
  FStream := nil;
  FUseSocket := True;
  Result := Self;
end;

function TSSLConnectionBuilderImpl.WithStream(AStream: TStream): ISSLConnectionBuilder;
begin
  FStream := AStream;
  FSocket := 0;
  FUseSocket := False;
  Result := Self;
end;

function TSSLConnectionBuilderImpl.WithTimeout(AMs: Integer): ISSLConnectionBuilder;
begin
  FTimeout := AMs;
  Result := Self;
end;

function TSSLConnectionBuilderImpl.WithBlocking(ABlocking: Boolean): ISSLConnectionBuilder;
begin
  FBlocking := ABlocking;
  Result := Self;
end;

function TSSLConnectionBuilderImpl.WithHostname(const AHostname: string): ISSLConnectionBuilder;
begin
  FHostname := AHostname;
  Result := Self;
end;

function TSSLConnectionBuilderImpl.WithSession(ASession: ISSLSession): ISSLConnectionBuilder;
begin
  FSession := ASession;
  Result := Self;
end;

function TSSLConnectionBuilderImpl.WithSessionReuse(AEnabled: Boolean): ISSLConnectionBuilder;
begin
  FSessionReuse := AEnabled;
  Result := Self;
end;

function TSSLConnectionBuilderImpl.BuildClient: ISSLConnection;
var
  LResult: TSSLOperationResult;
begin
  LResult := TryBuildClient(Result);
  if not LResult.Success then
    raise ESSLConnectionException.CreateWithContext(
      LResult.ErrorMessage,
      LResult.ErrorCode,
      'TSSLConnectionBuilder.BuildClient'
    );
end;

function TSSLConnectionBuilderImpl.BuildServer: ISSLConnection;
var
  LResult: TSSLOperationResult;
begin
  LResult := TryBuildServer(Result);
  if not LResult.Success then
    raise ESSLConnectionException.CreateWithContext(
      LResult.ErrorMessage,
      LResult.ErrorCode,
      'TSSLConnectionBuilder.BuildServer'
    );
end;

function TSSLConnectionBuilderImpl.TryBuildClient(out AConnection: ISSLConnection): TSSLOperationResult;
begin
  AConnection := nil;
  
  // Validate required fields
  if FContext = nil then
  begin
    Result := TSSLOperationResult.Err(sslErrInvalidParam, 'Context is required');
    Exit;
  end;
  
  if FUseSocket and (FSocket = 0) then
  begin
    Result := TSSLOperationResult.Err(sslErrInvalidParam, 'Socket is required');
    Exit;
  end;
  
  if not FUseSocket and (FStream = nil) then
  begin
    Result := TSSLOperationResult.Err(sslErrInvalidParam, 'Stream is required');
    Exit;
  end;
  
  try
    // Set hostname for SNI if provided
    if FHostname <> '' then
      FContext.SetServerName(FHostname);
    
    // Create connection
    if FUseSocket then
      AConnection := FContext.CreateConnection(FSocket)
    else
      AConnection := FContext.CreateConnection(FStream);
    
    if AConnection = nil then
    begin
      Result := TSSLOperationResult.Err(sslErrConnection, 'Failed to create connection');
      Exit;
    end;
    
    // Configure connection
    AConnection.SetTimeout(FTimeout);
    AConnection.SetBlocking(FBlocking);
    
    // Set session for reuse
    if FSessionReuse and (FSession <> nil) then
      AConnection.SetSession(FSession);
    
    // Perform client handshake
    if not AConnection.Connect then
    begin
      Result := TSSLOperationResult.Err(sslErrHandshake, 'Client handshake failed: ' + AConnection.GetVerifyResultString);
      AConnection := nil;
      Exit;
    end;
    
    Result := TSSLOperationResult.Ok;
  except
    on E: Exception do
    begin
      AConnection := nil;
      Result := TSSLOperationResult.Err(sslErrConnection, E.Message);
    end;
  end;
end;

function TSSLConnectionBuilderImpl.TryBuildServer(out AConnection: ISSLConnection): TSSLOperationResult;
begin
  AConnection := nil;
  
  // Validate required fields
  if FContext = nil then
  begin
    Result := TSSLOperationResult.Err(sslErrInvalidParam, 'Context is required');
    Exit;
  end;
  
  if FUseSocket and (FSocket = 0) then
  begin
    Result := TSSLOperationResult.Err(sslErrInvalidParam, 'Socket is required');
    Exit;
  end;
  
  if not FUseSocket and (FStream = nil) then
  begin
    Result := TSSLOperationResult.Err(sslErrInvalidParam, 'Stream is required');
    Exit;
  end;
  
  try
    // Create connection
    if FUseSocket then
      AConnection := FContext.CreateConnection(FSocket)
    else
      AConnection := FContext.CreateConnection(FStream);
    
    if AConnection = nil then
    begin
      Result := TSSLOperationResult.Err(sslErrConnection, 'Failed to create connection');
      Exit;
    end;
    
    // Configure connection
    AConnection.SetTimeout(FTimeout);
    AConnection.SetBlocking(FBlocking);
    
    // Perform server accept
    if not AConnection.Accept then
    begin
      Result := TSSLOperationResult.Err(sslErrHandshake, 'Server accept failed: ' + AConnection.GetVerifyResultString);
      AConnection := nil;
      Exit;
    end;
    
    Result := TSSLOperationResult.Ok;
  except
    on E: Exception do
    begin
      AConnection := nil;
      Result := TSSLOperationResult.Err(sslErrConnection, E.Message);
    end;
  end;
end;

end.
