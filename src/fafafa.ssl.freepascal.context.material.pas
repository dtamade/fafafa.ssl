{**
 * Unit: fafafa.ssl.freepascal.context.material
 * Purpose: 纯 FreePascal 后端上下文材料访问扩展接口（可选）
 *
 * 说明：
 * - 仅供 FreePascal 后端内部握手实现读取已加载证书/私钥原始数据
 * - 不修改全局 ISSLContext 标准接口，保持跨后端兼容
 *}

unit fafafa.ssl.freepascal.context.material;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  fafafa.ssl.base;

type
  TFreePascalPinInfo = record
    Hash: TBytes;
    PinType: Integer;
    Description: string;
    IsBackup: Boolean;
  end;

  TFreePascalPinInfoArray = array of TFreePascalPinInfo;

  IFreePascalContextMaterial = interface
    ['{6B661525-EA6C-4D8F-8307-3AA51866FC71}']
    function HasCertificateMaterial: Boolean;
    function HasPrivateKeyMaterial: Boolean;
    function GetCertificateMaterial: TBytes;
    function GetPrivateKeyMaterial: TBytes;
  end;

  IFreePascalContextTrustStore = interface
    ['{9D970527-1D2E-4A6A-8B66-7F4A868E7A8A}']
    function GetCertificateStore: ISSLCertificateStore;
    function GetCAFile: string;
    function GetCAPath: string;
    function GetVerifyCallback: TSSLVerifyCallback;
    function GetInfoCallback: TSSLInfoCallback;
    function GetCertificatePinningEnabled: Boolean;
    function GetPins: TFreePascalPinInfoArray;
  end;

implementation

end.
