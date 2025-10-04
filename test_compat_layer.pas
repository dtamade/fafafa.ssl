program test_compat_layer;

{$mode objfpc}{$H+}

uses
  fafafa.ssl.types,  // 使用兼容层
  fafafa.ssl.intf;

var
  LibType: TSSLLibraryType;
  VerifyMode: TSSLVerifyModes;
  CtxType: TSSLContextType;
  Config: TSSLConfig;
begin
  // 测试枚举值
  LibType := sslOpenSSL;
  VerifyMode := [sslVerifyPeer];
  CtxType := sslCtxClient;
  
  // 测试记录
  Config.LibraryType := sslOpenSSL;
  Config.ContextType := sslCtxClient;
  Config.VerifyMode := [sslVerifyPeer];
  
  WriteLn('Compatible layer test: OK');
  WriteLn('LibType = ', Ord(LibType));
  WriteLn('VerifyMode has sslVerifyPeer = ', sslVerifyPeer in VerifyMode);
end.
