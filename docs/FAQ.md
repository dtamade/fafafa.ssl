# fafafa.ssl 常见问题 (FAQ)

## 一般问题

### Q: fafafa.ssl支持哪些平台？

**A**: 支持所有主流平台：
- ✅ Linux (x86_64, ARM)
- ✅ Windows (x86_64)
- ✅ macOS (Intel, Apple Silicon)
- ✅ FreeBSD

唯一要求：系统安装OpenSSL 1.1.1+或3.x。

---

### Q: 需要什么Free Pascal版本？

**A**: FPC 3.2.0或更高版本。推荐使用FPC 3.2.2+。

---

### Q: 如何链接OpenSSL库？

**A**: 库会自动加载系统OpenSSL：

```pascal
// 自动初始化
uses fafafa.ssl.factory;

// 或手动控制
TSSLLibrary.Instance.Initialize;
```

**自定义路径**:
```pascal
TSSLLibrary.Instance.SetCustomLibraryPath('/custom/path/libssl.so');
```

---

## 证书问题

### Q: 生成的证书浏览器不信任？

**A**: 自签名证书默认不被信任。解决方案：

1. **开发环境**：手动添加到信任列表
   ```bash
   # Linux
   sudo cp cert.pem /usr/local/share/ca-certificates/mycert.crt
   sudo update-ca-certificates
   ```

2. **生产环境**：使用CA签名证书（Let's Encrypt, DigiCert等）

3. **内部网络**：部署企业CA

---

### Q: 如何生成带SAN的证书？

**A**:
```pascal
var
  LOptions: TCertGenOptions;
  LSANs: TStringList;
begin
  LOptions := TCertificateUtils.DefaultGenOptions;
  LOptions.CommonName := 'example.com';
  
  // 添加SAN
  LSANs := TStringList.Create;
  try
    LSANs.Add('example.com');
    LSANs.Add('www.example.com');
    LSANs.Add('*.example.com');
    LOptions.SubjectAltNames := LSANs;
    
    TCertificateUtils.GenerateSelfSigned(LOptions, Cert, Key);
  finally
    LSANs.Free;
  end;
end;
```

---

### Q: 证书有效期如何设置？

**A**: 通过`ValidDays`选项：

```pascal
LOptions.ValidDays := 365;  // 1年
LOptions.ValidDays := 825;  // ~2年（最大推荐）
```

**注意**：现代浏览器限制证书有效期≤825天。

---

### Q: 如何验证证书链？

**A**:
```pascal
// 方法1：单个证书 + CA路径
if TCertificateUtils.VerifyChain(LeafCert, '/etc/ssl/certs') then
  WriteLn('Valid');

// 方法2：证书bundle
LBundle := LeafCert + IntermediateCert;
if TCertificateUtils.VerifyChain(LBundle, RootCAPath) then
  WriteLn('Valid');
```

---

### Q: PEM和DER有什么区别？

**A**:
- **PEM**: Base64编码，文本格式，有`-----BEGIN/END-----`标记
- **DER**: 二进制格式，更紧凑

**转换**:
```pascal
// PEM → DER
LDER := TCertificateUtils.PEMToDER(LPEM);

// DER → PEM  
LPEM := TCertificateUtils.DERToPEM(LDER);
```

**何时使用**:
- PEM: 配置文件、传输、人类可读
- DER: Java keystores、Windows证书、嵌入式系统

---

## 性能问题

### Q: 证书生成很慢怎么办？

**A**: 

**原因**：RSA密钥生成需要大量计算。

**优化方案**:

1. **使用ECDSA**（快10-100倍）:
```pascal
LOptions.KeyType := ktECDSA;
LOptions.ECCurve := 'prime256v1';  // = secp256r1
```

2. **降低RSA位数**（仅测试环境）:
```pascal
LOptions.KeyBits := 2048;  // 而非4096
```

3. **预生成证书**：生产环境离线生成

**性能对比**:
- RSA 2048: ~100-500ms
- RSA 4096: ~1-3秒
- ECDSA P-256: ~10-50ms

---

### Q: 如何提高TLS握手速度？

**A**:

1. **Session重用**
2. **使用ECDSA证书**
3. **启用TLS 1.3**
4. **减少证书链长度**

---

### Q: 内存使用过高？

**A**:

**常见原因**:
1. 忘记释放`TCertInfo.SubjectAltNames`

```pascal
// ❌ 错误
LInfo := TCertificateUtils.GetInfo(Cert);
// 泄漏！

// ✅ 正确
LInfo := TCertificateUtils.GetInfo(Cert);
if Assigned(LInfo.SubjectAltNames) then
  LInfo.SubjectAltNames.Free;
```

2. 缓存过多证书数据

**解决**:
- 使用HeapTrc检测泄漏
- 及时释放不需要的证书数据
- 考虑证书缓存策略

---

## 错误处理

### Q: 如何调试OpenSSL错误？

**A**:

```pascal
uses
  fafafa.ssl.openssl.api.err;

// 获取OpenSSL错误
var LErrCode: Cardinal;
begin
  LErrCode := ERR_get_error();
  WriteLn('OpenSSL Error: ', ERR_error_string(LErrCode, nil));
end;
```

---

### Q: "Failed to load OpenSSL" 错误？

**A**:

**原因**: 找不到OpenSSL库

**解决**:

1. **检查安装**:
```bash
# Linux
ldconfig -p | grep libssl

# macOS
ls /usr/local/opt/openssl/lib/

# Windows
where libssl-*.dll
```

2. **指定路径**:
```pascal
TSSLLibrary.Instance.SetCustomLibraryPath('/path/to/openssl');
```

3. **版本兼容**:
   - 确保OpenSSL 1.1.1+或3.x
   - 避免使用OpenSSL 1.0.x（已EOL）

---

### Q: "Access violation" 错误？

**A**:

**常见原因**:

1. **未初始化**:
```pascal
// ✅ 确保初始化
uses fafafa.ssl.factory;
// 自动初始化
```

2. **多线程竞争**（虽然API线程安全，但不当使用仍可能出错）

3. **OpenSSL版本不兼容**

**调试**:
```bash
# 使用HeapTrc
fpc -gh -gl program.pas
./program
```

---

## 跨平台注意事项

### Q: Windows上DLL加载失败？

**A**:

1. **确保DLL在PATH**:
```bash
set PATH=%PATH%;C:\OpenSSL\bin
```

2. **检查架构**:
   - x64程序需x64 DLL
   - x86程序需x86 DLL

3. **依赖DLL**:
   确保同时有`libssl-*.dll`和`libcrypto-*.dll`

---

### Q: macOS上找不到OpenSSL？

**A**:

```bash
# 安装
brew install openssl@3

# 链接
brew link openssl@3 --force

# 或指定路径
export DYLD_LIBRARY_PATH=/usr/local/opt/openssl@3/lib
```

---

### Q: Linux不同发行版路径不同？

**A**:

库会自动尝试常见路径：
- `/usr/lib/libssl.so`
- `/usr/lib/x86_64-linux-gnu/libssl.so`
- `/lib/libssl.so`

如失败，手动指定：
```pascal
TSSLLibrary.Instance.SetCustomLibraryPath('/custom/path');
```

---

## 高级话题

### Q: 如何实现OCSP验证？

**A**: 当前版本未内置OCSP，计划在未来版本添加。

**临时方案**: 使用外部OCSP工具：
```bash
openssl ocsp -issuer ca.pem -cert cert.pem \
  -url http://ocsp.example.com -resp_text
```

---

### Q: 支持PKCS#12 (.pfx/.p12)吗？

**A**: 当前版本专注于PEM/DER，PKCS#12支持计划中。

**转换方案**:
```bash
# PKCS#12 → PEM
openssl pkcs12 -in cert.p12 -out cert.pem -nodes

# PEM → PKCS#12
openssl pkcs12 -export -in cert.pem -inkey key.pem -out cert.p12
```

---

### Q: 如何生成CSR（证书签名请求）？

**A**: 计划在下一版本添加。

**临时方案**: 使用OpenSSL命令行。

---

## 获取帮助

- **GitHub Issues**: https://github.com/yourusername/fafafa.ssl/issues
- **文档**: [QuickStart.md](QuickStart.md), [API_Reference.md](API_Reference.md)
- **示例**: `examples/` 目录

---

## 版本说明

本FAQ基于fafafa.ssl v1.0。功能可能在新版本中变化。

---

**未找到答案？** [提交Issue](https://github.com/yourusername/fafafa.ssl/issues/new)
