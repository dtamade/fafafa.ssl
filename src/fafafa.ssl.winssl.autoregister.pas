{**
 * Unit: fafafa.ssl.winssl.autoregister
 * Purpose: 自动注册 WinSSL 后端
 *
 * 使用方法:
 * 在程序的 uses 子句中添加此单元，WinSSL 后端将自动注册。
 *
 * 注意:
 * - 此单元必须在 fafafa.ssl.factory 之后导入
 * - 如果不需要自动注册，可以直接使用 fafafa.ssl.winssl.lib 并手动调用 RegisterWinSSLBackend
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-06
 *}

unit fafafa.ssl.winssl.autoregister;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.factory,      // 必须先导入 factory 以确保初始化顺序
  fafafa.ssl.winssl.lib;   // 导入 WinSSL 库实现

implementation

initialization
  {$IFDEF WINDOWS}
  // 在程序启动时自动注册 WinSSL 后端
  RegisterWinSSLBackend;
  {$ENDIF}

finalization
  // 清理由 fafafa.ssl.winssl.lib 的 finalization 处理

end.
