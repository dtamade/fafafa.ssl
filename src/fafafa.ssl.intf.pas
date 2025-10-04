{
  fafafa.ssl.intf - SSL/TLS 接口定义（向后兼容层）
  
  版本: 2.0
  作者: fafafa.ssl 开发团队
  创建: 2025-09-28
  更新: 2025-10-04
  
  描述:
    此模块是向后兼容层,重新导出 fafafa.ssl.abstract.intf 中的所有接口。
    保留此模块是为了不破坏现有代码,所有新代码应直接使用 fafafa.ssl.abstract.intf。
    
  架构位置:
    兼容层 - 导出抽象接口以保持向后兼容
    
  迁移说明:
    为了使用新架构,建议将:
      uses fafafa.ssl.intf;
    改为:
      uses fafafa.ssl.abstract.intf;
}

unit fafafa.ssl.intf;

{$mode ObjFPC}{$H+}
{$INTERFACES CORBA}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  SysUtils, Classes, 
  fafafa.ssl.types,
  fafafa.ssl.abstract.intf;

// 重新导出所有接口类型
type
  ISSLLibrary = fafafa.ssl.abstract.intf.ISSLLibrary;
  ISSLContext = fafafa.ssl.abstract.intf.ISSLContext;
  ISSLConnection = fafafa.ssl.abstract.intf.ISSLConnection;
  ISSLCertificate = fafafa.ssl.abstract.intf.ISSLCertificate;
  ISSLCertificateStore = fafafa.ssl.abstract.intf.ISSLCertificateStore;
  ISSLSession = fafafa.ssl.abstract.intf.ISSLSession;
  
  TSSLCertificateArray = fafafa.ssl.abstract.intf.TSSLCertificateArray;

// 重新导出辅助函数
function SSLErrorToString(aError: TSSLErrorCode): string;
function ProtocolVersionToString(aVersion: TSSLProtocolVersion): string;
function LibraryTypeToString(aLibType: TSSLLibraryType): string;

implementation

function SSLErrorToString(aError: TSSLErrorCode): string;
begin
  Result := fafafa.ssl.abstract.intf.SSLErrorToString(aError);
end;

function ProtocolVersionToString(aVersion: TSSLProtocolVersion): string;
begin
  Result := fafafa.ssl.abstract.intf.ProtocolVersionToString(aVersion);
end;

function LibraryTypeToString(aLibType: TSSLLibraryType): string;
begin
  Result := fafafa.ssl.abstract.intf.LibraryTypeToString(aLibType);
end;

end.
