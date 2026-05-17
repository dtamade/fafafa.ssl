{**
 * Unit: fafafa.ssl.context.compat
 * Purpose: context-level compatibility helpers shared across backends
 *
 * 封装仍然保留的历史兼容读取，避免每个 backend 构造器都直接依赖
 * deprecated `ISSLContext.GetServerName`。
 *}

unit fafafa.ssl.context.compat;

{$mode ObjFPC}{$H+}

interface

uses
  fafafa.ssl.base;

function GetContextLevelServerNameCompatibilityValue(
  const AContext: ISSLContext
): string;

implementation

function GetContextLevelServerNameCompatibilityValue(
  const AContext: ISSLContext
): string;
begin
  Result := '';
  if AContext = nil then
    Exit;

  // New connections must now receive hostname/SNI explicitly on the
  // connection. Deprecated context-level ServerName may still remain visible on
  // the context API itself, but it no longer auto-flows into new connections.
end;

end.
