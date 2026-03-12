# OpenSSL 模块验证报告


> **Historical snapshot:** This page captures a point-in-time testing or validation snapshot and may not reflect the current repository state. For current verification commands and active workflow guidance, start with `docs/testing/TESTING_README.md`.

**生成时间:** 2025-10-03 01:37:04  
**验证范围:** 所有 OpenSSL 模块 (65个)  
**验证策略:** 分层快速验证

---

## 📊 总体统计

- **总模块数:** 64
- **编译成功:** 40 ✅
- **编译失败:** 24 ❌
- **警告数量:** 0 ⚠️
- **成功率:** 62.5%

---

## 📋 分组验证结果

### P0_Core
**状态:** 4/5 模块通过

| 模块 | 状态 |
|------|------|
| fafafa.ssl.openssl.core.pas | ✅ 通过 |
| fafafa.ssl.openssl.evp.pas | ✅ 通过 |
| fafafa.ssl.openssl.hmac.pas | ✅ 通过 |
| fafafa.ssl.openssl.kdf.pas | ❌ 失败 |
| fafafa.ssl.openssl.rand.pas | ✅ 通过 |

### P1_Asymmetric
**状态:** 3/3 模块通过

| 模块 | 状态 |
|------|------|
| fafafa.ssl.openssl.rsa.pas | ✅ 通过 |
| fafafa.ssl.openssl.ecdsa.pas | ✅ 通过 |
| fafafa.ssl.openssl.dsa.pas | ✅ 通过 |

### P1_BigNum
**状态:** 4/1 模块通过

| 模块 | 状态 |
|------|------|
| fafafa.ssl.openssl.bn.pas | ✅ 通过 |

### P1_PKI
**状态:** 4/5 模块通过

| 模块 | 状态 |
|------|------|
| fafafa.ssl.openssl.x509.pas | ✅ 通过 |
| fafafa.ssl.openssl.x509v3.pas | ❌ 失败 |
| fafafa.ssl.openssl.pem.pas | ✅ 通过 |
| fafafa.ssl.openssl.asn1.pas | ✅ 通过 |
| fafafa.ssl.openssl.bio.pas | ✅ 通过 |

### P2_Helpers
**状态:** 4/4 模块通过

| 模块 | 状态 |
|------|------|
| fafafa.ssl.openssl.err.pas | ✅ 通过 |
| fafafa.ssl.openssl.buffer.pas | ❌ 失败 |
| fafafa.ssl.openssl.obj.pas | ❌ 失败 |
| fafafa.ssl.openssl.stack.pas | ❌ 失败 |

### P2_KeyExchange
**状态:** 2/2 模块通过

| 模块 | 状态 |
|------|------|
| fafafa.ssl.openssl.dh.pas | ✅ 通过 |
| fafafa.ssl.openssl.ecdh.pas | ✅ 通过 |

### P2_PKCS
**状态:** 2/4 模块通过

| 模块 | 状态 |
|------|------|
| fafafa.ssl.openssl.pkcs7.pas | ❌ 失败 |
| fafafa.ssl.openssl.pkcs12.pas | ❌ 失败 |
| fafafa.ssl.openssl.pkcs.pas | ✅ 通过 |
| fafafa.ssl.openssl.cms.pas | ✅ 通过 |

### P2_SSL
**状态:** 4/1 模块通过

| 模块 | 状态 |
|------|------|
| fafafa.ssl.openssl.ssl.pas | ✅ 通过 |

### P3_Ciphers
**状态:** 3/7 模块通过

| 模块 | 状态 |
|------|------|
| fafafa.ssl.openssl.aes.pas | ✅ 通过 |
| fafafa.ssl.openssl.des.pas | ✅ 通过 |
| fafafa.ssl.openssl.chacha.pas | ❌ 失败 |
| fafafa.ssl.openssl.aria.pas | ❌ 失败 |
| fafafa.ssl.openssl.seed.pas | ❌ 失败 |
| fafafa.ssl.openssl.legacy_ciphers.pas | ✅ 通过 |
| fafafa.ssl.openssl.modes.pas | ❌ 失败 |

### P3_Hash
**状态:** 0/5 模块通过

| 模块 | 状态 |
|------|------|
| fafafa.ssl.openssl.sha.pas | ❌ 失败 |
| fafafa.ssl.openssl.sha3.pas | ❌ 失败 |
| fafafa.ssl.openssl.sha3.evp.pas | ❌ 失败 |
| fafafa.ssl.openssl.blake2.pas | ❌ 失败 |
| fafafa.ssl.openssl.md.pas | ❌ 失败 |

### P3_MAC
**状态:** 0/2 模块通过

| 模块 | 状态 |
|------|------|
| fafafa.ssl.openssl.cmac.pas | ❌ 失败 |
| fafafa.ssl.openssl.cmac.evp.pas | ❌ 失败 |

### P3_Special
**状态:** 2/2 模块通过

| 模块 | 状态 |
|------|------|
| fafafa.ssl.openssl.sm.pas | ✅ 通过 |
| fafafa.ssl.openssl.scrypt_whirlpool.pas | ✅ 通过 |

### P4_Advanced
**状态:** 7/9 模块通过

| 模块 | 状态 |
|------|------|
| fafafa.ssl.openssl.ocsp.pas | ❌ 失败 |
| fafafa.ssl.openssl.ts.pas | ✅ 通过 |
| fafafa.ssl.openssl.ct.pas | ✅ 通过 |
| fafafa.ssl.openssl.store.pas | ✅ 通过 |
| fafafa.ssl.openssl.srp.pas | ✅ 通过 |
| fafafa.ssl.openssl.conf.pas | ✅ 通过 |
| fafafa.ssl.openssl.param.pas | ✅ 通过 |
| fafafa.ssl.openssl.engine.pas | ✅ 通过 |
| fafafa.ssl.openssl.provider.pas | ❌ 失败 |

### P5_Infrastructure
**状态:** 10/14 模块通过

| 模块 | 状态 |
|------|------|
| fafafa.ssl.openssl.types.pas | ✅ 通过 |
| fafafa.ssl.openssl.consts.pas | ✅ 通过 |
| fafafa.ssl.openssl.api.pas | ✅ 通过 |
| fafafa.ssl.openssl.crypto.pas | ✅ 通过 |
| fafafa.ssl.openssl.utils.pas | ✅ 通过 |
| fafafa.ssl.openssl.thread.pas | ✅ 通过 |
| fafafa.ssl.openssl.async.pas | ❌ 失败 |
| fafafa.ssl.openssl.lhash.pas | ❌ 失败 |
| fafafa.ssl.openssl.txt_db.pas | ✅ 通过 |
| fafafa.ssl.openssl.ui.pas | ✅ 通过 |
| fafafa.ssl.openssl.dso.pas | ✅ 通过 |
| fafafa.ssl.openssl.aead.pas | ✅ 通过 |
| fafafa.ssl.openssl.comp.pas | ❌ 失败 |
| fafafa.ssl.openssl.rand_old.pas | ❌ 失败 |

---

## ❌ 失败模块详情
- **fafafa.ssl.openssl.kdf.pas**
- **fafafa.ssl.openssl.x509v3.pas**
- **fafafa.ssl.openssl.buffer.pas**
- **fafafa.ssl.openssl.obj.pas**
- **fafafa.ssl.openssl.stack.pas**
- **fafafa.ssl.openssl.pkcs7.pas**
- **fafafa.ssl.openssl.pkcs12.pas**
- **fafafa.ssl.openssl.chacha.pas**
- **fafafa.ssl.openssl.aria.pas**
- **fafafa.ssl.openssl.seed.pas**
- **fafafa.ssl.openssl.modes.pas**
- **fafafa.ssl.openssl.sha.pas**
- **fafafa.ssl.openssl.sha3.pas**
- **fafafa.ssl.openssl.sha3.evp.pas**
- **fafafa.ssl.openssl.blake2.pas**
- **fafafa.ssl.openssl.md.pas**
- **fafafa.ssl.openssl.cmac.pas**
- **fafafa.ssl.openssl.cmac.evp.pas**
- **fafafa.ssl.openssl.ocsp.pas**
- **fafafa.ssl.openssl.provider.pas**
- **fafafa.ssl.openssl.async.pas**
- **fafafa.ssl.openssl.lhash.pas**
- **fafafa.ssl.openssl.comp.pas**
- **fafafa.ssl.openssl.rand_old.pas**

---

## 🎯 下一步建议

### ⚠️ 存在编译失败模块

**建议行动:**
1. 优先修复 P1 高优先级模块
2. 检查依赖关系是否正确
3. 确认 OpenSSL 版本兼容性
4. 修复后重新验证

---

**验证日志:** D:\projects\Pascal\lazarus\My\libs\fafafa.ssl\docs\validation\compile_20251003_013646.log  
**报告文件:** D:\projects\Pascal\lazarus\My\libs\fafafa.ssl\docs\validation\validation_report_20251003_013646.md

