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

uses
  fafafa.ssl.connection.base;

function GetContextLevelServerNameCompatibilityValue(
  const AContext: ISSLContext
): string;
begin
  Result := '';
  if AContext = nil then
    Exit;

  if not ContextTypeSupportsClientConnectionRole(AContext.GetContextType) then
    Exit;

  {$PUSH}{$WARN 6058 off}{$WARN SYMBOL_DEPRECATED OFF}
  Result := AContext.GetServerName;
  {$POP}
end;

end.
