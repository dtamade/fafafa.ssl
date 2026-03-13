program test_p2_ct_http_hooks;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.net.hooks,
  fafafa.ssl.ct.log;

procedure Check(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
  begin
    WriteLn('❌ FAIL: ', AMessage);
    Halt(1);
  end;
  WriteLn('✅ PASS: ', AMessage);
end;

type
  TTestHooks = class
  public
    Called: Boolean;
    LastURL: string;
    LastTimeoutMs: Integer;
    function HTTPGet(const AURL: string; ATimeoutMs: Integer): TSSLDataResult;
  end;

function TTestHooks.HTTPGet(const AURL: string; ATimeoutMs: Integer): TSSLDataResult;
const
  PAYLOAD = '{"stub":"ct-log-list"}';
var
  LBytes: TBytes;
begin
  Called := True;
  LastURL := AURL;
  LastTimeoutMs := ATimeoutMs;

  LBytes := TEncoding.UTF8.GetBytes(PAYLOAD);
  Result := TSSLDataResult.Ok(LBytes);
end;

procedure TestDownloadCTLogListUsesHooks;
var
  LHooks: TTestHooks;
  LScope: TSSLHTTPHooksScope;
  LText: string;
begin
  WriteLn('--- Test: DownloadCTLogList uses HTTPGet hooks ---');

  LHooks := TTestHooks.Create;
  try
    LScope := TSSLHTTPHooksScope.Push(TSSLHTTPHooks.Create(@LHooks.HTTPGet, nil));
    try
      LText := DownloadCTLogList('https://example.test/ct/log_list.json');
    finally
      LScope.Pop;
    end;

    Check(LHooks.Called, 'HTTPGet callback called');
    Check(LHooks.LastURL = 'https://example.test/ct/log_list.json', 'URL forwarded');
    Check(LHooks.LastTimeoutMs = 10000, 'Default timeout forwarded');
    Check(LText = '{"stub":"ct-log-list"}', 'Downloaded text matches stub');
  finally
    LHooks.Free;
  end;
end;

procedure TestMissingHooksReturnsEmpty;
var
  LText: string;
begin
  WriteLn('--- Test: missing hooks -> DownloadCTLogList returns empty ---');
  LText := DownloadCTLogList('https://example.test/ct/log_list.json');
  Check(LText = '', 'Returned empty string when hooks missing');
end;

begin
  try
    TestDownloadCTLogListUsesHooks;
    WriteLn;
    TestMissingHooksReturnsEmpty;
    WriteLn;
    WriteLn('All CT HTTP hooks tests passed.');
    Halt(0);
  except
    on E: Exception do
    begin
      WriteLn('❌ Unhandled exception: ', E.Message);
      Halt(1);
    end;
  end;
end.
