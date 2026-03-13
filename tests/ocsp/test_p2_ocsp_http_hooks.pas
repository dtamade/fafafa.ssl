program test_p2_ocsp_http_hooks;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.net.hooks,
  fafafa.ssl.http.client;

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
    LastContentType: string;
    LastTimeoutMs: Integer;
    LastBodyLen: Integer;
    function HTTPPost(const AURL, AContentType: string; const ABody: TBytes;
      ATimeoutMs: Integer): TSSLDataResult;
  end;

function TTestHooks.HTTPPost(const AURL, AContentType: string; const ABody: TBytes;
  ATimeoutMs: Integer): TSSLDataResult;
var
  LResp: TBytes;
begin
  Called := True;
  LastURL := AURL;
  LastContentType := AContentType;
  LastTimeoutMs := ATimeoutMs;
  LastBodyLen := Length(ABody);

  SetLength(LResp, 3);
  LResp[0] := 1;
  LResp[1] := 2;
  LResp[2] := 3;
  Result := TSSLDataResult.Ok(LResp);
end;

procedure TestSimpleHTTPClientUsesHooks;
var
  LClient: TSimpleHTTPClient;
  LHooks: TTestHooks;
  LScope: TSSLHTTPHooksScope;
  LReq: TBytes;
  LResp: TBytes;
begin
  WriteLn('--- Test: TSimpleHTTPClient.Post uses thread HTTP hooks ---');

  SetLength(LReq, 4);
  LReq[0] := 7;
  LReq[1] := 8;
  LReq[2] := 9;
  LReq[3] := 10;

  LHooks := TTestHooks.Create;
  try
    LScope := TSSLHTTPHooksScope.Push(TSSLHTTPHooks.Create(nil, @LHooks.HTTPPost));
    try
      LClient := TSimpleHTTPClient.Create;
      try
        LClient.Timeout := 1234;
        LClient.ContentType := 'application/test';
        LResp := LClient.Post('http://example.test/ocsp', LReq);
      finally
        LClient.Free;
      end;
    finally
      LScope.Pop;
    end;

    Check(LHooks.Called, 'HTTPPost callback called');
    Check(LHooks.LastURL = 'http://example.test/ocsp', 'URL forwarded');
    Check(LHooks.LastContentType = 'application/test', 'Content-Type forwarded');
    Check(LHooks.LastTimeoutMs = 1234, 'Timeout forwarded');
    Check(LHooks.LastBodyLen = Length(LReq), 'Body forwarded');
    Check(Length(LResp) = 3, 'Response length matches stub');
    Check((LResp[0] = 1) and (LResp[1] = 2) and (LResp[2] = 3), 'Response bytes match stub');
  finally
    LHooks.Free;
  end;
end;

procedure TestMissingHooksFails;
var
  LClient: TSimpleHTTPClient;
  LReq: TBytes;
  LGotException: Boolean;
begin
  WriteLn('--- Test: missing hooks -> Post raises ---');

  SetLength(LReq, 1);
  LReq[0] := 42;

  LGotException := False;
  LClient := TSimpleHTTPClient.Create;
  try
    try
      LClient.Post('http://example.test/ocsp', LReq);
    except
      on E: Exception do
      begin
        LGotException := True;
        WriteLn('[INFO] expected exception: ', E.Message);
      end;
    end;
  finally
    LClient.Free;
  end;

  Check(LGotException, 'Post fails without hooks');
end;

begin
  try
    TestSimpleHTTPClientUsesHooks;
    WriteLn;
    TestMissingHooksFails;
    WriteLn;
    WriteLn('All HTTP hooks tests passed.');
    Halt(0);
  except
    on E: Exception do
    begin
      WriteLn('❌ Unhandled exception: ', E.Message);
      Halt(1);
    end;
  end;
end.
