# fafafa.ssl 快速入门

## 5分钟上手

### 安装

1. **下载源代码**:
```bash
git clone https://github.com/yourusername/fafafa.ssl.git
cd fafafa.ssl
```

2. **确保OpenSSL已安装**:
```bash
# Linux
sudo apt-get install libssl-dev

# macOS
brew install openssl

# Windows
# 下载并安装 OpenSSL from https://wiki.openssl.org/index.php/Binaries
```

3. **编译示例**:
```bash
cd examples
fpc -Mobjfpc -Sh -Fu../src your_program.pas
```

---

## 第一个程序

### 证书信息查看器

创建`my_first_ssl.pas`:

```pascal
program my_first_ssl;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.cert.utils;

var
  LCertPEM: string;
  LInfo: TCertInfo;
begin
  //  加载证书
  LCertPEM := TCertificateUtils.LoadFromFile('mycert.pem');
  
  // 获取信息
  LInfo := TCertificateUtils.GetInfo(LCertPEM);
  
  // 显示
  WriteLn('Subject: ', LInfo.Subject);
  WriteLn('Issuer: ', LInfo.Issuer);
  WriteLn('Valid: ', DateTimeToStr(LInfo.NotBefore), 
          ' to ', DateTimeToStr(LInfo.NotAfter));
  
  if Assigned(LInfo.SubjectAltNames) then
    LInfo.SubjectAltNames.Free;
end.
```

编译运行:
```bash
fpc -Mobjfpc -Sh -Fu../src my_first_ssl.pas
./my_first_ssl
```

---

## 常见场景

### 1. 生成自签名证书

```pascal
uses
  fafafa.ssl.cert.utils;

var
  LOptions: TCertGenOptions;
  LCert, LKey: string;
begin
  // 配置选项
  LOptions := TCertificateUtils.DefaultGenOptions;
  LOptions.CommonName := 'example.com';
  LOptions.Organization := 'My Company';
  LOptions.ValidDays := 365;
  
  // 生成证书
  if TCertificateUtils.GenerateSelfSigned(LOptions, LCert, LKey) then
  begin
    TCertificateUtils.SaveToFile('cert.pem', LCert);
    TCertificateUtils.SaveToFile('key.pem', LKey);
    WriteLn('✓ Certificate generated!');
  end;
end.
```

### 2. PEM/DER格式转换

```pascal
uses
  fafafa.ssl.cert.utils;

var
  LPEM: string;
  LDER: TBytes;
begin
  // PEM → DER
  LPEM := TCertificateUtils.LoadFromFile('cert.pem');
  LDER := TCertificateUtils.PEMToDER(LPEM);
  WriteLn('DER size: ', Length(LDER), ' bytes');
  
  // DER → PEM
  LPEM := TCertificateUtils.DERToPEM(LDER);
  WriteLn('Converted back to PEM');
end.
```

### 3. 比较证书DN

```pascal
uses
  fafafa.ssl.cert.utils;

begin
  // 大小写不敏感比较
  if TCertificateUtils.CompareX509Names(
    'CN=example.com,O=Company',
    'O=Company,CN=example.com'  // 顺序不同
  ) then
    WriteLn('✓ DNs match!');
end.
```

### 4. 验证证书有效期

```pascal
uses
  fafafa.ssl.cert.utils;

var
  LCertPEM: string;
begin
  LCertPEM := TCertificateUtils.LoadFromFile('cert.pem');
  
  if TCertificateUtils.IsValid(LCertPEM) then
    WriteLn('✓ Certificate is valid')
  else
    WriteLn('⚠ Certificate expired or not yet valid');
end.
```

---

## 实用工具

### 证书信息查看器
```bash
cd examples
fpc cert_info_viewer.pas
./cert_info_viewer mycert.pem
```

### PEM/DER转换器
```bash
fpc pem_der_converter.pas

# PEM → DER
./pem_der_converter pem2der input.pem output.der

# DER → PEM
./pem_der_converter der2pem input.der output.pem
```

---

## API概览

### TCertificateUtils

核心证书工具类，全部静态方法：

```pascal
// 生成
GenerateSelfSigned(AOptions, out ACert, AKey): Boolean
GenerateSigned(AOptions, ACACert, ACAKey, out ACert, AKey): Boolean

// 加载/保存
LoadFromFile(AFileName): string
SaveToFile(AFileName, ACertPEM): Boolean

// 信息
GetInfo(ACertPEM): TCertInfo
GetFingerprint(ACertPEM): string  // SHA256
IsValid(ACertPEM): Boolean

// 格式转换
PEMToDER(APEM): TBytes
DERToPEM(ADER): string
ConvertFormat(AInput, AFrom, ATo): TBytes

// 比较
CompareX509Names(AN ame1, AName2, ACaseInsensitive): Boolean

// 验证
VerifyChain(ACertPEM, ACAPath): Boolean
```

---

## 下一步

- 📖 查看[API参考](API_Reference.md)完整文档
- 🔧 查看[examples/](../examples/)目录更多示例
- ❓ 有问题？查看[FAQ](FAQ.md)

---

**快速链接**:
- [GitHub仓库](https://github.com/yourusername/fafafa.ssl)
- [API文档](API_Reference.md)
- [常见问题](FAQ.md)
