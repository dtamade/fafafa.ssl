{******************************************************************************}
{  Connection Pool Test                                                        }
{******************************************************************************}

program test_connection_pool;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.pool,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.api.core,
  test_openssl_base;

var
  Runner: TSimpleTestRunner;
  Pool: ISSLConnectionPool;
  Config: TSSLPoolConfig;

procedure TestPoolCreation;
begin
  WriteLn;
  WriteLn('=== Pool Creation ===');

  Pool := TSSLConnectionPool.Create;
  Runner.Check('Create connection pool', Pool <> nil);

  Runner.Check('Initial total connections', Pool.GetTotalConnections = 0);
  Runner.Check('Initial active connections', Pool.GetActiveConnections = 0);
  Runner.Check('Initial idle connections', Pool.GetIdleConnections = 0);
end;

procedure TestPoolConfiguration;
begin
  WriteLn;
  WriteLn('=== Pool Configuration ===');

  Config := TSSLPoolConfig.Default;
  Runner.Check('Default MaxPoolSize', Config.MaxPoolSize = 10);
  Runner.Check('Default IdleTimeoutSec', Config.IdleTimeoutSec = 300);
  Runner.Check('Default ConnectTimeoutMs', Config.ConnectTimeoutMs = 30000);
  Runner.Check('Default HealthCheckIntervalSec', Config.HealthCheckIntervalSec = 60);

  Config.MaxPoolSize := 20;
  Config.IdleTimeoutSec := 600;
  Pool.SetConfig(Config);

  Config := Pool.GetConfig;
  Runner.Check('Modified MaxPoolSize', Config.MaxPoolSize = 20);
  Runner.Check('Modified IdleTimeoutSec', Config.IdleTimeoutSec = 600);
end;

procedure TestGlobalPool;
var
  GlobalPool: ISSLConnectionPool;
begin
  WriteLn;
  WriteLn('=== Global Pool Singleton ===');

  GlobalPool := GlobalConnectionPool;
  Runner.Check('Global pool exists', GlobalPool <> nil);

  // Should return same instance
  Runner.Check('Global pool singleton', GlobalConnectionPool = GlobalPool);
end;

procedure TestClearAll;
begin
  WriteLn;
  WriteLn('=== Clear All Connections ===');

  Pool.ClearAll;
  Runner.Check('Clear all - total connections', Pool.GetTotalConnections = 0);
  Runner.Check('Clear all - active connections', Pool.GetActiveConnections = 0);
end;

begin
  WriteLn('Connection Pool Tests');
  WriteLn('====================');

  Runner := TSimpleTestRunner.Create;
  try
    Runner.RequireModules([osmCore]);

    if not Runner.Initialize then
    begin
      WriteLn('ERROR: Failed to initialize test environment');
      Halt(1);
    end;

    WriteLn('OpenSSL Version: ', GetOpenSSLVersionString);

    TestPoolCreation;
    TestPoolConfiguration;
    TestGlobalPool;
    TestClearAll;

    Runner.PrintSummary;
    Halt(Runner.FailCount);
  finally
    Pool := nil;
    Runner.Free;
  end;
end.
