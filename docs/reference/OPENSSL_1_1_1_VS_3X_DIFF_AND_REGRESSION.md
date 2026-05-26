# OpenSSL 1.1.1 vs 3.x 差异清单与回归策略（P2 模块）

**状态**：Phase 1 已落盘（2026-02-06）  
**范围**：PKCS7 / CMS / PKCS12 / OCSP / CT / TS / Store  
**当前基线**：Linux + FPC 3.3.1 + OpenSSL 3.5.4

---

## 先看结论

- `fafafa.ssl` 已采用“动态加载 + 版本分支”策略，核心 P2 模块在 OpenSSL 3.x 可用。
- 兼容实现不是“强行兼容所有旧符号”，而是：
  - 生产路径优先现代 API；
  - 旧版本/可选 API 以“可选加载 + 测试分支跳过”处理。
- Phase 1 当前已完成 3.x 基线验证（`15/15` 通过），并补齐了 7 模块离线失败夹具。

---

## 版本探测与装载策略（实现依据）

1. `TOpenSSLLoader.DetectVersion` 通过 `OpenSSL_version_num` 获取版本号，旧版本回退到 `SSLeay`。
2. `LoadOpenSSLCore` 先尝试加载 OpenSSL 3.x，失败再回退 OpenSSL 1.1.x。
3. 测试通过 `TOpenSSLLoader.IsOpenSSL3` 做版本分支，明确哪些检查是 1.x only。

---

## 差异清单（符号 / 行为 / 测试策略）

| 类别 | OpenSSL 1.1.1 | OpenSSL 3.x | 当前适配方式 | 回归关注点 |
|---|---|---|---|---|
| 证书获取 API | `SSL_get_peer_certificate` | 重命名为 `SSL_get1_peer_certificate` | 先加载旧名，失败后回退新名并复用指针 | 握手后取对端证书不应回归为 `nil` |
| 协议方法 API | `SSLv23_*` 可用 | 被 `TLS_*` 取代 | `SSLv23_*` 加载失败时绑定到 `TLS_*` | 客户端/服务端上下文创建应保持可用 |
| TS BIO 符号 | `TS_REQ_d2i_bio` / `TS_RESP_d2i_bio` 命名风格 | 使用 `d2i_TS_REQ_bio` / `d2i_TS_RESP_bio` | 在 TS 模块中按 3.x 符号名加载到统一函数指针 | TS 请求/响应解析在 3.x 下可正常调用 |
| PKCS7 高级函数 | `PKCS7_get_recip_info` 常见可用 | 在 3.x 可能不可用 | 测试中将“3.x 不可用”视为预期通过 | 生产主路径不依赖该函数 |
| CMS 收据 API | `CMS_get1_Receipt` 可见于旧接口语境 | 3.x 不保证提供 | 测试保留备注，不作为阻断项 | 收据主流程依赖 `CMS_verify_receipt` 等可用函数 |
| PKCS12 辅助函数集 | 辅助函数较完整 | 部分辅助函数不可用/行为收敛 | 统一按 `ExpectedToFailInOpenSSL3` 记为 SKIP（预期） | 重点保证 `PKCS12_create/parse` 与 BIO I/O 主路径 |
| OCSP 旧接口 | `OCSP_RESPONSE_status` / `OCSP_BASICRESP_verify` / `OCSP_parse_url` 等可见 | 多个旧接口按 1.x only 处理 | 测试在 3.x 分支跳过这类函数加载断言 | OCSP 解析、nonce、validity 主流程需通过 |
| CT 序列化 | 部分历史资料关注 DER/BIO SCT 编码函数 | 3.x 语义转向其他机制 | 测试中明确“序列化通过其他机制实现” | CT 日志加载与校验函数可用 |
| STORE Loader 细粒度接口 | `OSSL_STORE_LOADER_set_*` 族在 1.x 路径使用 | 3.x 下部分接口不再作为主路径 | 测试在 3.x 跳过 1.x only 的 `set_*` 检查 | 证书加载/搜索主路径不受影响 |

---

## PKCS12：3.x 预期不可用函数（测试显式记录）

`tests/certificate/test_p2_pkcs12_comprehensive.pas` 已将下列符号按“3.x 不可用可跳过”处理：

- `PKCS12_pbe_crypt`
- `PKCS12_crypt`
- `PKCS12_get_cert`
- `PKCS12_get_pkey`
- `PKCS12_get1_certs`
- `PKCS12_certbag`
- `PKCS12_keybag`
- `PKCS12_secretbag`
- `PKCS12_add_key_bag`
- `PKCS12_get_private_key`
- `PKCS12_SAFEBAG_get0_certs`
- `PKCS12_SAFEBAG_get_bag_type`

> 注：当前 3.5.4 基线报告显示 PKCS12 综合测试通过且包含预期 SKIP。

---

## 回归策略（版本感知）

### Tier 1：每次改动必跑（当前环境）

```bash
python3 scripts/compile_all_modules.py
bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT
```

### Tier 2：模块级综合测试（建议在发布前）

```bash
./bin/test_p2_pkcs7_comprehensive
./bin/test_p2_cms_comprehensive
./bin/test_p2_pkcs12_comprehensive
./bin/test_p2_ocsp_comprehensive
./bin/test_p2_ct_comprehensive
./bin/test_p2_ts_comprehensive
./bin/test_p2_store_comprehensive
```

### Tier 3：版本矩阵（至少 1.1.1 + 3.x）

- 在 OpenSSL 3.x runner：要求 P2 聚焦回归全通过，且 SKIP 原因必须可解释。
- 在 OpenSSL 1.1.1 runner：要求同一命令集可运行，1.x only 符号检查应不再走 SKIP 分支。
- 若矩阵结果不一致，优先补充：
  1. 差异说明（符号级）；
  2. 回归用例（成功 + 失败）；
  3. 文档中的“预期行为”描述。

---

## 验收标准（DoD）

- 差异项有“符号名 + 行为 + 测试策略”三元说明。
- 3.x 基线命令可复现，报告可追溯。
- 1.1.1 与 3.x 的分支行为有明确解释，不以“偶然通过”作为结论。

---

## 关联文档

- `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md`
- `docs/testing/P2_OFFLINE_FIXTURE_GUIDE.md`

