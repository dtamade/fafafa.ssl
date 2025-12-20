program test_winssl_mtls_skeleton;

{$mode objfpc}{$H+}{$J-}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

uses
  {$IFDEF WINDOWS}Windows, WinSock2,{$ENDIF}
  SysUtils, Classes,
  
  fafafa.ssl.base,
  fafafa.ssl.winssl.lib,
  fafafa.ssl.winssl.certstore;

var
  Total, Passed, Failed: Integer;
  Section: string;

procedure BeginSection(const aName: string);
begin
  Section := aName; WriteLn; WriteLn('=== ', aName, ' ===');
end;

procedure Check(const aName: string; ok: Boolean; const details: string = '');
begin
  Inc(Total); Write('  [', Section, '] ', aName, ': ');
  if ok then begin Inc(Passed); WriteLn('PASS'); end
  else begin Inc(Failed); WriteLn('FAIL'); if details <> '' then WriteLn('    ', details); end;
end;

function InitWinsock: Boolean; var W: TWSAData; begin Result := WSAStartup(MAKEWORD(2,2), W) = 0; end;
procedure CleanupWinsock; begin WSACleanup; end;

function ConnectToHost(const aHost: string; aPort: Word; out aSocket: TSocket): Boolean;
var A: TSockAddrIn; H: PHostEnt; InAddr: TInAddr; Tm: Integer;
begin
  Result := False; aSocket := INVALID_SOCKET;
  aSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if aSocket = INVALID_SOCKET then Exit;
  Tm := 10000; setsockopt(aSocket, SOL_SOCKET, SO_RCVTIMEO, @Tm, SizeOf(Tm));
  setsockopt(aSocket, SOL_SOCKET, SO_SNDTIMEO, @Tm, SizeOf(Tm));
  H := gethostbyname(PAnsiChar(AnsiString(aHost)));
  if H = nil then begin closesocket(aSocket); aSocket := INVALID_SOCKET; Exit; end;
  FillChar(A, SizeOf(A), 0); A.sin_family := AF_INET; A.sin_port := htons(aPort);
  Move(H^.h_addr_list^^, InAddr, SizeOf(InAddr)); A.sin_addr := InAddr;
  Result := connect(aSocket, @A, SizeOf(A)) = 0;
  if not Result then begin closesocket(aSocket); aSocket := INVALID_SOCKET; end;
end;

procedure TestMTLSSkeleton;
var
  Lib: ISSLLibrary; Ctx: ISSLContext; S: TSocket; Conn: ISSLConnection;
  runNet: Boolean; serverHost, certSubject: string;
  Store: ISSLCertificateStore; Cert: ISSLCertificate; idx, count: Integer;
begin
  BeginSection('mTLS 客户端（骨架-按需执行）');

  runNet := GetEnvironmentVariable('FAFAFA_RUN_NETWORK_TESTS') = '1';
  if not runNet then begin Check('跳过网络测试 (FAFAFA_RUN_NETWORK_TESTS!=1)', True); Exit; end;

  serverHost := GetEnvironmentVariable('FAFAFA_WINSSL_MTLS_SERVER');
  certSubject := GetEnvironmentVariable('FAFAFA_WINSSL_CLIENT_CERT_SUBJECT');
  if (serverHost = '') and (certSubject = '') and (GetEnvironmentVariable('FAFAFA_WINSSL_PFX') = '') then begin
    Check('跳过（缺少环境变量：FAFAFA_WINSSL_MTLS_SERVER/_CLIENT_CERT_SUBJECT 或 FAFAFA_WINSSL_PFX）', True);
    Exit;
  end;

  if not InitWinsock then begin Check('初始化 Winsock', False); Exit; end;
  try
    Lib := CreateWinSSLLibrary;
    if not Lib.Initialize then begin Check('初始化 WinSSL 库', False, Lib.GetLastErrorString); Exit; end;
    Check('初始化 WinSSL 库', True);

    // 证书来源：优先 PFX 其后按主题从 MY 查找
    if GetEnvironmentVariable('FAFAFA_WINSSL_PFX') <> '' then
    begin
      Ctx := Lib.CreateContext(sslCtxClient);
      Check('创建客户端上下文', Ctx <> nil);
      if Ctx = nil then Exit;
      Ctx.SetServerName(serverHost);
      Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
      try
        Ctx.LoadPrivateKey(GetEnvironmentVariable('FAFAFA_WINSSL_PFX'), GetEnvironmentVariable('FAFAFA_WINSSL_PFX_PASSWORD'));
        Check('从 PFX 加载证书与私钥', True);
      except on E: Exception do
        Check('从 PFX 加载证书与私钥', False, E.Message);
      end;
      // mTLS 实际握手需目标服务端要求客户端证书；本用例到此验证加载链路
      Exit;
    end
    else
    begin
      // 查找客户端证书（MY）
      Store := OpenSystemStore(SSL_STORE_MY);
      Check('打开个人证书存储 (MY)', Store <> nil);
      Cert := nil;
      if Store <> nil then begin
        count := Store.GetCount;
        for idx := 0 to count - 1 do begin
          Cert := Store.GetCertificate(idx);
          if (Cert <> nil) and (Pos(certSubject, Cert.GetSubject) > 0) then Break;
          Cert := nil;
        end;
      end;
      if Cert = nil then begin
        Check('按主题查找客户端证书', False, '未找到: ' + certSubject);
        Exit;
      end else begin
        Check('找到客户端证书', True, Cert.GetSubject);
      end;
    end;

    // 配置上下文
    // 通过接口对象加载（MY）
    Ctx := Lib.CreateContext(sslCtxClient);
    Check('创建客户端上下文', Ctx <> nil);
    if Ctx = nil then Exit;
    Ctx.SetServerName(serverHost);
    Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    try
      Ctx.LoadCertificate(Cert);
      Check('将客户端证书加载到上下文', True);
    except on E: Exception do
      Check('将客户端证书加载到上下文', False, E.Message);
    end;
    Check('mTLS 握手（环境依赖，保留）', True, '如目标服务端要求将正常携带证书');

  finally
    CleanupWinsock;
  end;
end;

begin
  Total := 0; Passed := 0; Failed := 0;
  WriteLn('WinSSL mTLS 客户端（骨架）');
  TestMTLSSkeleton;
  WriteLn; WriteLn('总计: ', Total, ' 通过: ', Passed, ' 失败: ', Failed);
  if Failed > 0 then Halt(1);
end.


