# 示例编译修复追踪

> **Batch**: B71
> **Status**: draft
> **Created**: 2026-02-07

## 概述

本文档追踪 `examples/` 目录下编译失败示例的修复进度。

## 统计摘要

| 指标 | 数值 |
|------|------|
| 总计 | 75 |
| 通过 | 37 |
| 失败 | 34 |
| 跳过 | 4 |
| 通过率 | 52.1% |

## 失败分类

### 类别 1: 内联变量声明 (var inline)

FPC ObjFPC 模式不支持 `var x := ...` 语法，需要改为传统声明。

**影响文件**:
- `examples/02_generate_certificate.pas`
- `examples/03_file_encryption.pas`
- `examples/06_digital_signature.pas`

**修复方案**: 将内联变量声明移到 `var` 块中。

### 类别 2: 缺少 API 绑定

部分示例使用了尚未绑定的 OpenSSL API。

**影响文件**:
- `examples/02_generate_certificate.pas` - `EVP_PKEY_assign_RSA`, `X509_set_notBefore`
- `examples/certificate_verification_example.pas`
- `examples/example_aes_gcm_aead.pas`

**修复方案**: 添加缺失的 API 绑定或使用替代 API。

### 类别 3: 类型不匹配

`PAnsiChar` vs `PByte` 等类型兼容性问题。

**影响文件**:
- `examples/02_generate_certificate.pas`
- `examples/pkcs7_*.pas` 系列

**修复方案**: 添加类型转换或修改 API 签名。

### 类别 4: 缺少依赖单元

引用了不存在或未导出的单元。

**影响文件**:
- `examples/demo_fluent_api.pas`
- `examples/example_https_api.pas`
- `examples/example_json_api.pas`

**修复方案**: 创建缺失单元或移除依赖。

### 类别 5: 平台特定

依赖特定平台功能（已通过跳过处理）。

**影响文件**:
- `examples/winssl_*.pas` - WinSSL 示例（Linux 下跳过）
- `examples/09_winssl_fips.pas`

**修复方案**: 保持跳过策略。

## 优先级排序

### P1 - 高优先级（核心示例）

| 文件 | 问题类别 | 状态 |
|------|----------|------|
| `01_tls_client.pas` | - | ✅ 通过 |
| `02_generate_certificate.pas` | 1, 2, 3 | ❌ 待修复 |
| `03_file_encryption.pas` | 1 | ❌ 待修复 |
| `04_https_rest_client.pas` | - | ✅ 通过 |
| `05_https_server.pas` | - | ✅ 通过 |
| `06_digital_signature.pas` | 1 | ❌ 待修复 |
| `07_certificate_chain.pas` | - | ✅ 通过 |
| `08_mutual_tls.pas` | - | ✅ 通过 |
| `10_cert_renewal.pas` | - | ✅ 通过 |

### P2 - 中优先级（常用示例）

| 文件 | 问题类别 | 状态 |
|------|----------|------|
| `hello_ssl.pas` | - | ✅ 通过 |
| `hash_calculator.pas` | - | ✅ 通过 |
| `pkcs12_example.pas` | - | ✅ 通过 |
| `session_reuse_example.pas` | - | ✅ 通过 |

### P3 - 低优先级（高级/实验性示例）

其余失败示例归入此类，可考虑归档或延后修复。

## 修复进度

| 批次 | 范围 | 状态 |
|------|------|------|
| B71 | 核心示例 (01-10) | 进行中 |
| B72 | API 变更相关 | 待开始 |
| B73+ | 其他示例 | 待规划 |

## 本轮基线更新（2026-02-08）

### 基线（批次启动前）

- 报告：`docs/archive/reports/examples-compile-history/examples_compile_latest.json`
- 结果：`total=75, passed=36, failed=35, skipped=4, pass_rate=50.7%`

### B79 后（核心示例修复）

- 报告：`docs/archive/reports/examples-compile-history/examples_compile_after_b79_partial.json`
- 结果：`total=75, passed=41, failed=30, skipped=4, pass_rate=57.7%`
- 净变化：`+5 passed`，`-5 failed`，通过率 `+7.0pp`

### 已完成的核心修复点

- `examples/02_generate_certificate.pas`
  - 修复 ObjFPC 不支持的内联变量声明。
  - 适配现有 OpenSSL 绑定：`EVP_PKEY_set1_RSA`、`X509_set1_notBefore/After`。
  - 统一 `X509_NAME_add_entry_by_txt` 入参为 `PByte(PAnsiChar(...))`。
- `examples/03_file_encryption.pas`
  - 移除无效单元依赖并补齐 `rand/scrypt_whirlpool`。
  - 修复 `EVP_Encrypt/Decrypt(Update|Final)` 参数签名匹配。
  - 新增 `GetFileSize` 辅助函数替代不兼容 `FileSize(string)` 调用。
- `examples/06_digital_signature.pas`
  - 将 `DigestSign/DigestVerify` 调用链适配为 `Init + Update + Final`。
  - 修复 `PPEVP_PKEY_CTX` 参数传递方式（`@LPKeyCtx`）。
- `examples/certificate_verification_example.pas`
  - 验证标志改为 `sslCertVerifyDefault`。
  - 补充 `Math` 依赖以支持 `Min`。

### B80 子项首轮（API 变更示例）

- `examples/demo_fluent_api.pas`：`TCryptoUtils.BytesToHex` → `TEncodingUtils.BytesToHex(..., False)`。
- `examples/example_streaming_operations.pas`：同上。
- `examples/example_https_api.pas`：修复异常块结构导致的编译错误。

### B80/B83 最新门禁（2026-02-08 01:25 +0800）

- 报告：`docs/archive/reports/examples-compile-history/examples_compile_gate_b83.json`
- 结果：`total=75, passed=53, failed=18, skipped=4, pass_rate=74.6%`
- 对比启动基线：`+17 passed`（36→53），`-17 failed`（35→18），通过率 `+23.9pp`

### 额外修复（脚本口径）

- `scripts/verify_examples_compile.sh`
  - 编译参数新增 `-Fu"$EXAMPLES_DIR"`，解决示例共享单元（`fafafa.examples.*`）在验证脚本下不可见导致的误报。
  - 修复后 `https_client/*` 与 `production/https_client_*` 多文件由 FAIL 转 PASS。

## 相关文档

- `scripts/verify_examples_compile.sh` - 编译验证脚本
- `docs/plans/PHASE3_EXAMPLES_COMPILE_VERIFY_DRAFT.md` - 验证脚本草案

### B80 收口门禁（2026-02-08 01:50 +0800）

- 报告：`docs/archive/reports/examples-compile-history/examples_compile_gate_b84.json`
- 结果：`total=75, passed=60, failed=11, skipped=4, pass_rate=84.5%`
- 对比 B83：`+7 passed`（53→60），`-7 failed`（18→11），通过率 `+9.9pp`
- 对比启动基线：`+24 passed`（36→60），`-24 failed`（35→11），通过率 `+33.8pp`

### B80 Wave A 本轮转绿示例

- `examples/https_client_production.pas`
- `examples/password_hash_v2.pas`
- `examples/example_cert_pinning.pas`
- `examples/digital_signature/digital_signature.pas`
- `examples/security_enhancements_demo.pas`
- `examples/pkcs7_basic_example.pas`
- `examples/pkcs7_sign_verify_simple.pas`

### 当前剩余失败（11）

- `examples/example_aes_gcm_aead.pas`
- `examples/example_json_api.pas`
- `examples/https_simple_get.pas`
- `examples/password_hash/password_hash.pas`
- `examples/pkcs7_data_example.pas`
- `examples/pkcs7_encrypt_decrypt_example.pas`
- `examples/pkcs7_sign_verify_example.pas`
- `examples/practical_examples.pas`
- `examples/probe_sockets.pas`
- `examples/simple_ssl_connection.pas`
- `examples/simple_test.pas`


### B85 持续门禁（2026-02-08 02:20 +0800）

- 报告：`docs/archive/reports/examples-compile-history/examples_compile_gate_b85.json`
- 结果：`total=75, passed=62, failed=9, skipped=4, pass_rate=87.3%`
- 对比 B84：`+2 passed`（60→62），`-2 failed`（11→9），通过率 `+2.8pp`

### B85 本轮转绿示例

- `examples/probe_sockets.pas`
- `examples/simple_test.pas`

### 当前剩余失败（9）

- `examples/example_aes_gcm_aead.pas`
- `examples/example_json_api.pas`
- `examples/https_simple_get.pas`
- `examples/password_hash/password_hash.pas`
- `examples/pkcs7_data_example.pas`
- `examples/pkcs7_encrypt_decrypt_example.pas`
- `examples/pkcs7_sign_verify_example.pas`
- `examples/practical_examples.pas`
- `examples/simple_ssl_connection.pas`

### B86 阶段门禁（2026-02-08 02:45 +0800）

- 报告：`docs/archive/reports/examples-compile-history/examples_compile_gate_b86.json`
- 结果：`total=75, passed=66, failed=5, skipped=4, pass_rate=92.9%`
- 对比 B85：`+4 passed`（62→66），`-4 failed`（9→5），通过率 `+5.6pp`

#### B86 本轮转绿示例

- `examples/https_simple_get.pas`
- `examples/simple_ssl_connection.pas`
- `examples/example_json_api.pas`
- `examples/password_hash/password_hash.pas`

### B87 收口门禁（2026-02-08 02:53 +0800）

- 报告：`docs/archive/reports/examples-compile-history/examples_compile_gate_b87.json`
- 结果：`total=75, passed=71, failed=0, skipped=4, pass_rate=100.0%`
- 对比 B86：`+5 passed`（66→71），`-5 failed`（5→0），通过率 `+7.1pp`

#### B87 本轮转绿示例

- `examples/example_aes_gcm_aead.pas`
- `examples/pkcs7_data_example.pas`
- `examples/pkcs7_encrypt_decrypt_example.pas`
- `examples/pkcs7_sign_verify_example.pas`
- `examples/practical_examples.pas`

### 当前剩余失败（0）

- 无。

### Wave B Linux Gate 同步（2026-02-08 02:57 +0800）

- 报告：`docs/archive/reports/wave-b-history/wave_b_ci_gate_summary_20260208_025426.md`
- 结果：`compile=PASS, modules=PASS, examples=PASS, overall=PASS`
- 示例口径：`docs/archive/reports/examples-compile-history/examples_compile_ci_gate.json`（`71/75`, `failed=0`, `pass_rate=100.0%`）

### B88 脚本稳健性修复（2026-02-08 03:43 +0800）

- 变更文件：`scripts/verify_examples_compile.sh`
- 问题：当 `failed=0` 时，JSON 报告 `failed_files` 输出为 `[""]`。
- 修复：在 JSON 输出分支按数组长度判断，空失败集输出 `[]`。

#### B88 门禁结果

- 报告：`docs/archive/reports/examples-compile-history/examples_compile_gate_b88.json`
- 结果：`total=75, passed=71, failed=0, skipped=4, pass_rate=100.0%`
- 格式验证：`failed_files=[]`（已修复）

#### Wave B 同步验证

- 报告：`docs/archive/reports/wave-b-history/wave_b_ci_gate_summary_20260208_034029.md`
- 结果：overall PASS，`examples_compile_ci_gate.json` 中 `failed_files=[]`。
