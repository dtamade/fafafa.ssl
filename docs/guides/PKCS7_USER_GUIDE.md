# PKCS#7 使用指南

## 概述

PKCS#7 是用于签名、加密、封装证书和消息数据的标准格式。`fafafa.ssl` 当前发布的是 `OpenSSL` backend 上的 PKCS#7 surface，既包括 low-level raw API，也包括同单元里的高入口 helper。

本指南讨论的是 `OpenSSL` backend 暴露的 PKCS#7 raw API + helper surface，不代表所有 backend 都发布同等能力。

PKCS#7 当前没有一对一 capability 字段，支持判断以 `LoadPKCS7Functions`、模块加载状态 `osmPKCS7` 与 focused tests 为准。

## 推荐入口

推荐从同一个单元 `fafafa.ssl.openssl.api.pkcs7` 进入：

- 高入口 helper：`SignData` / `VerifySignedData` / `EncryptData` / `DecryptData`
- raw API：`LoadPKCS7Functions` / `PKCS7_sign` / `PKCS7_verify` / `PKCS7_encrypt` / `PKCS7_decrypt`

如果你的目标是把 `TBytes` 做签名、验签、加密、解密，优先用 helper。只有在你明确需要操作 `PPKCS7`、`PBIO`、`PSTACK_OF_X509` 这类 OpenSSL 指针时，才下沉到 raw API。

## 快速开始

### 示例 1：使用 helper 做分离式签名

```pascal
uses
  SysUtils,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.api.pkcs7;

var
  InputData: TBytes;
  SignedData: TBytes;
begin
  LoadOpenSSLCore;
  if not LoadPKCS7Functions then
    raise Exception.Create('PKCS7 functions unavailable');

  InputData := BytesOf('hello pkcs7');
  SignedData := SignData(
    InputData,
    MyCert,
    MyKey,
    nil,
    PKCS7_DETACHED or PKCS7_BINARY
  );
end;
```

### 示例 2：使用 raw API 创建加密消息

```pascal
uses
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.pkcs7;

var
  P7: PPKCS7;
  DataBio: PBIO;
  RecipStack: PSTACK_OF_X509;
begin
  LoadOpenSSLCore;
  if not LoadPKCS7Functions then
    raise Exception.Create('PKCS7 functions unavailable');

  DataBio := BIO_new_mem_buf(PAnsiChar(MyData), Length(MyData));
  RecipStack := OPENSSL_sk_new_null();
  OPENSSL_sk_push(RecipStack, RecipientCert);

  P7 := PKCS7_encrypt(RecipStack, DataBio, EVP_aes_256_cbc(), 0);
  OPENSSL_sk_free(RecipStack);
end;
```

### 示例 3：使用 helper 解密二进制 PKCS#7 数据

```pascal
uses
  fafafa.ssl.openssl.loader,
  fafafa.ssl.openssl.api.pkcs7;

var
  PlainData: TBytes;
begin
  LoadOpenSSLCore;
  if not LoadPKCS7Functions then
    raise Exception.Create('PKCS7 functions unavailable');

  if not DecryptData(EncryptedData, RecipientCert, RecipientKey, PlainData, 0) then
    raise Exception.Create('PKCS7 decrypt failed');
end;
```

## 验证入口

当前推荐把这些文件当成 PKCS#7 的验证入口：

- `tests/certificate/test_p2_pkcs7_comprehensive.pas`
- `tests/certificate/test_p2_pkcs7.pas`
- `tests/certificate/test_p2_pkcs7_boundary.pas`
- `tests/certificate/test_p2_pkcs7_encrypt_decrypt.pas`
- `tests/certificate/test_pkcs7_workflow.pas`
- `tests/test_pkcs7_helper_bio_contract.pas`
- `tests/test_pkcs7_sign_symbol_contract.pas`
- `tests/test_pkcs7_verify_symbol_contract.pas`

这些测试文件和命令是当前可执行验证入口，但不要把固定的总测试数、通过率、性能数字或历史输出文本当成当前接口 truth。

### 示例命令 1：跑综合能力测试

```bash
cd /home/dtamade/projects/fafafa.ssl
fpc -B -Mobjfpc -Sh -Fu./src -Fi./src -FU./lib/test \
    tests/certificate/test_p2_pkcs7_comprehensive.pas \
    -o./bin/test_p2_pkcs7_comprehensive
./bin/test_p2_pkcs7_comprehensive
```

成功标准：

- 测试程序成功编译并运行
- PKCS#7 函数加载、签名、验签、加密、解密相关断言通过
- 如果当前输出摘要和历史记录不同，以当前运行结果为准

### 示例命令 2：跑工作流验证

```bash
cd /home/dtamade/projects/fafafa.ssl
fpc -B -Mobjfpc -Sh -Fu./src -Fi./src -FU./lib/test \
    tests/certificate/test_pkcs7_workflow.pas \
    -o./bin/test_pkcs7_workflow
./bin/test_pkcs7_workflow
```

成功标准：

- 测试程序成功编译并运行
- sign -> verify、encrypt -> decrypt、sign+encrypt workflow 断言通过
- helper 或 raw API 的历史截图、旧统计数字不应作为当前验收依据

## 重要内存管理规则

### 1. `PKCS7_dataInit()` 会接管输入 BIO

```pascal
// 错误：data_bio 的所有权已经转交给 PKCS7_dataInit
data_bio := BIO_new_mem_buf(...);
out_bio := PKCS7_dataInit(p7, data_bio);
BIO_free(data_bio);

// 正确：只释放返回的 out_bio
data_bio := BIO_new_mem_buf(...);
out_bio := PKCS7_dataInit(p7, data_bio);
BIO_free(out_bio);
```

### 2. `PKCS7_sign` / `PKCS7_encrypt` 调用后不要假设输入 BIO 仍归你管理

```pascal
data_bio := BIO_new_mem_buf(...);
p7 := PKCS7_sign(cert, key, nil, data_bio, flags);
// 保持和现有测试一致：不要在这里立刻 BIO_free(data_bio)
```

### 3. helper 与 raw API 的所有权模型不同

- helper `SignData` / `EncryptData` / `VerifySignedData` / `DecryptData` 负责内部 `BIO` 生命周期
- raw API 路径下，调用方要自己维护 `PPKCS7`、`PBIO`、`PSTACK_OF_X509` 的释放时机

## 何时选 PKCS#7，何时选 CMS

- 需要兼容既有 PKCS#7 生态或现有消息格式时，继续使用 PKCS#7
- 新项目如果没有兼容性包袱，优先评估 `CMS_USER_GUIDE.md`
- 在本仓库里，PKCS#7 和 CMS 都依赖 OpenSSL 模块加载与 focused tests，不要把某个历史阶段描述当成能力证明
