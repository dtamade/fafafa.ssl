# FreePascal TLS 1.2 + TLS 1.3 Full Stack Roadmap

> **创建**: 2026-05-26
> **目标**: FreePascal 后端达到 OpenSSL 同等完成度（TLS 1.2 + 1.3）
> **方法**: TDD 驱动，每阶段先写 RED 测试再实现

---

## 总览

| Phase | 目标 | 状态 | 退出标准 |
|-------|------|------|----------|
| P0 | 纯 Pascal 与安全策略基线 | ✅ 完成 | 无 C 库依赖，capability truth 对齐 |
| P1 | 纯 Pascal 密码基础 | ✅ 完成 | AES-256-GCM + HMAC-SHA384 + TLS1.2 PRF 纯 Pascal |
| P2 | RSA 与 CBC 兼容密码 | ✅ 完成 | RSA KEX/签名 + CBC+HMAC 常量时间 |
| P3 | X.509 完整链验证 | 待开始 | RFC 5280 路径验证 + 主机名 + 信任锚 |
| P4 | CRL/OCSP 完整吊销 | 待开始 | CRL + OCSP stapling + online OCSP |
| P5 | TLS 1.2 record/handshake 核心 | 待开始 | record 层 + ClientHello/ServerHello + extensions |
| P6 | TLS 1.2 KEX/认证 | 待开始 | RSA/ECDHE KEX + 证书验证 + 会话复用 |
| P7 | TLS 1.2 cipher suites | 待开始 | AEAD 优先 + CBC 兼容 |
| P8 | TLS 1.3 回归统一 | 待开始 | 新组件接入 TLS 1.3，零回退 |
| P9 | 互操作矩阵 | 待开始 | 真实服务器验证 |
| P10 | 性能/发布门禁 | 待开始 | ≤ OpenSSL 2-3x |

---

## 依赖关系

```
P0
└─ P1
   ├─ P2 ─┐
   ├─ P3 ─┬─ P4 ─┐
   └─ P5 ─┴─ P6 ─┬─ P7
                  ├─ P8
                  └─ P9 ─ P10
```

可并行：P3 和 P5 可在 P1 后并行。

---

## Phase 详细

### P0: 纯 Pascal 与安全策略基线

**TDD 测试清单：**
- `test_freepascal_no_c_library_dependency_contract.sh`
- `test_freepascal_tls_policy_defaults.pas`
- `test_capability_matrix_freepascal_tls12_truth_contract.sh`
- `test_freepascal_reject_tls10_tls11.pas`

**实现要点：**
- 确认 FreePascal 后端不加载 OpenSSL/WinSSL/MbedTLS/WolfSSL
- TLS 1.3 默认，TLS 1.2 AEAD 默认
- RSA KEX/CBC 仅兼容档 opt-in
- 修正 capability matrix 中 TLS 1.2 的真实状态

---

### P1: 纯 Pascal 密码基础

**TDD 测试清单：**
- RFC 4231 HMAC-SHA256/SHA384 向量测试
- TLS 1.2 PRF master_secret/key_block/Finished 向量测试
- NIST AES-GCM 加密/解密/tag-fail 向量测试
- AES-256-GCM-SHA384 端到端测试

**实现要点：**
- 纯 Pascal AES-256-GCM（不依赖 EVP）
- HMAC-SHA384
- TLS 1.2 PRF (P_SHA256, P_SHA384)
- 常量时间工具

---

### P2: RSA 与 CBC 兼容密码

**TDD 测试清单：**
- RSA SPKI 解析测试
- RSA 加密/解密已知向量
- Bad padding/bad version 统一错误边界
- RSA-SHA256/SHA384 签名验证
- AES-CBC-HMAC record 向量测试
- Padding/MAC 错误常量时间 bucket

**实现要点：**
- RSA PKCS#1 v1.5 KEX 和签名
- CBC+HMAC 兼容套件
- Padding oracle 防护（常量时间）
- 仅在 LegacyCompatibility profile 下可协商

---

### P3: X.509 完整链验证

**TDD 测试清单：**
- leaf → intermediate → root 链验证
- AKI/SKI 消歧义
- Subject collision 处理
- BasicConstraints critical/pathLen
- KeyUsage keyCertSign
- EKU serverAuth/clientAuth
- SAN DNS/IP/wildcard 匹配
- expired/future/untrusted/partial-chain fail-closed

**实现要点：**
- RFC 5280 路径构建
- 签名验证（RSA/ECDSA）
- 约束检查
- 主机名验证
- 信任锚管理

---

### P4: CRL/OCSP 完整吊销

**TDD 测试清单：**
- CRL signature/issuer/thisUpdate/nextUpdate
- CRL revoked/good/unknown
- CRL DP HTTP hook
- OCSP CertID SHA1/SHA256
- BasicOCSPResponse 签名验证
- Nonce 验证
- Delegated responder OCSPSigning
- Stapled/online hard-fail/soft-fail 策略

---

### P5: TLS 1.2 record/handshake 核心

**TDD 测试清单：**
- Record length/type/version 解析
- ClientHello SNI/ALPN/supported_groups/signature_algorithms
- EMS/renegotiation_info/fallback SCSV
- ServerHello 解析
- Finished verify_data
- Alert 映射

---

### P6: TLS 1.2 KEX/认证

**TDD 测试清单：**
- RSA pre_master_secret version check
- key_block 派生
- ECDHE ServerKeyExchange 签名验证
- Certificate/CertificateRequest/CertificateVerify
- Session ID/ticket + EMS binding
- Bad cert abort

---

### P7: TLS 1.2 cipher suites

**TDD 测试清单：**
- TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256
- TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384
- ECDSA variants
- Static RSA AES-GCM
- CBC SHA256/SHA384 opt-in
- Sequence-number MAC

---

### P8: TLS 1.3 回归统一

**TDD 测试清单：**
- 现有 TLS 1.3 completeness gate 全绿
- AES-256-GCM 纯 Pascal 路径
- 证书链 fail-closed
- OCSP/CRL 策略
- PSK/Early Data 绑定 SNI/ALPN/cipher/cert policy

---

### P9: 互操作矩阵

**TDD 测试清单：**
- Local `openssl s_server` TLS1.2 RSA/ECDHE/CBC/GCM
- badssl.com 证书负例
- 真实 TLS1.3/TLS1.2 AEAD 站点
- OCSP stapling 站点
- 抓包 fixture replay

---

### P10: 性能/发布门禁

**TDD 测试清单：**
- Handshake 200/1000 iter benchmark
- Record throughput AES-GCM/ChaCha/CBC
- Cert-chain cache benchmark
- Parser fuzz/property tests
- Allocation budget

**目标：** handshake/record ≤ OpenSSL 2-3x

---

## 风险控制

| 风险 | 等级 | 控制措施 |
|------|------|----------|
| RSA KEX/CBC oracle 漏洞 | 高 | 默认禁用，统一错误，常量时间 |
| X.509 验证"看起来能用但验证错" | 高 | RFC 5280 fixture 矩阵，fail-closed |
| 纯 Pascal AES-GCM 侧信道 | 高 | 先正确性/常量时间，再优化 |
| OCSP/CRL 公网不稳定 | 中高 | 离线 fixture 为主，公网 nightly |
| TLS 1.2 兼容性范围膨胀 | 中高 | 只支持 AEAD/ECDHE 默认 |
| 现有 TLS 1.3 回归 | 中 | 每阶段跑 completeness gate |

---

## 参考标准

- TLS 1.2: RFC 5246
- TLS 1.3: RFC 8446
- TLS 安全配置: RFC 9325
- X.509 路径验证: RFC 5280
- OCSP: RFC 6960
- Extended Master Secret: RFC 7627
