# 2026-05-21 PKCS11_USER_GUIDE 当前 public import 真相对齐

## Goal

修复 `docs/guides/PKCS11_USER_GUIDE.md`
里三段 active
builder 示例
仍在使用
`fafafa.ssl.base`
导入的问题，
让这份 PKCS#11
用户指南继续保留：

- builder/runtime
  能力边界
- `pmCallback`
  /
  `pmInteractive`
  的 lower-level
  叙事
- OpenSSL-only
  published path

但不再偏离
当前 public import truth。

## Scope

- Add:
  - `docs/plans/2026-05-21-pkcs11-user-guide-current-public-import-truth.md`
- Update:
  - `docs/guides/PKCS11_USER_GUIDE.md`
  - `tests/scripts/test_pkcs11_docs_builder_guidance_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

本批不做：

- 不改 PKCS#11 运行时实现
- 不改 `docs/reference/PKCS11_ARCHITECTURE.md`
- 不把 lower-level callback 示例错误改成 facade-only

## Architecture Truth

- `TSSLContextBuilder`
  当前来自：
  - `fafafa.ssl.context.builder`
- `ISSLContext`
  /
  `sslOpenSSL`
  当前可直接来自：
  - `fafafa.ssl`
- `TPKCS11PINMethod`
  /
  `TPKCS11Config`
  /
  `TPKCS11ConfigDefault`
  /
  `IPKCS11Backend`
  /
  `TPKCS11BackendFactory`
  仍然属于
  PKCS#11
  专用单元，
  不应伪装成
  主门面导出
- 因此：
  - 三段 active
    builder 示例
    当前都应从
    `fafafa.ssl.base`
    收回到
    `fafafa.ssl`
  - 但涉及
    `pmEnvironment`
    /
    `pmFile`
    的示例
    继续保留：
    - `fafafa.ssl.pkcs11.types`

## Steps

1. 收紧现有
   `tests/scripts/test_pkcs11_docs_builder_guidance_contract.sh`：
   - 继续冻结
     builder/runtime
     语义边界
   - 新增冻结：
     - 三段 active
       builder 示例
       必须使用：
       - `fafafa.ssl`
     - 不得继续出现：
       - `fafafa.ssl.base`
2. 用 `HEAD`
   guide snapshot
   跑同一条合同，
   先拿到 RED。
3. 最小修改 `PKCS11_USER_GUIDE.md` 三处导入。
4. 重跑 focused contract、
   相关 PKCS11 高入口合同、
   `git diff --check`。

## Verification

```bash
bash -n tests/scripts/test_pkcs11_docs_builder_guidance_contract.sh
PKCS11_GUIDE_DOC=/tmp/fafafa_ssl_pkcs11_user_guide_head.md bash tests/scripts/test_pkcs11_docs_builder_guidance_contract.sh
bash tests/scripts/test_pkcs11_docs_builder_guidance_contract.sh
bash tests/scripts/test_api_inventory_pkcs11_high_entry_truth_contract.sh
git diff --check
```

## Expected Result

- `PKCS11_USER_GUIDE`
  三段 active
  builder 示例
  不再继续教学
  `fafafa.ssl.base`
- `fafafa.ssl`
  /
  `fafafa.ssl.context.builder`
  /
  `fafafa.ssl.pkcs11.types`
  的当前边界
  会更清楚
- lower-level
  callback 示例
  继续保留在
  PKCS#11
  专用单元
  上

## Execution Result

- PASS
- focused contract
  先补齐了：
  - `PKCS11_GUIDE_DOC`
    覆盖入口，
    允许同一条
    focused contract
    对
    `HEAD`
    旧版 guide
    做 RED
  - active builder
    facade import
    计数
    当前固定为
    3 处
- focused RED
  通过
  `HEAD`
  snapshot
  真实暴露：
  - 三段 active
    builder 示例
    都还在使用
    `fafafa.ssl.base`
  - 合同输出：
    - `expected 3 facade import lines, found: 0`
- 最小修复后：
  - 三段 active
    builder 示例
    已统一回到：
    - `fafafa.ssl`
  - `fafafa.ssl.context.builder`
    /
    `fafafa.ssl.pkcs11.types`
    的专用边界
    继续保留
  - lower-level
    callback 示例
    未被错误拉回
    facade-only
- focused verification：
  - `bash -n tests/scripts/test_pkcs11_docs_builder_guidance_contract.sh`
    - PASS
  - `PKCS11_GUIDE_DOC=/tmp/fafafa_ssl_pkcs11_user_guide_head.md bash tests/scripts/test_pkcs11_docs_builder_guidance_contract.sh`
    - FAIL
  - `bash tests/scripts/test_pkcs11_docs_builder_guidance_contract.sh`
    - PASS
  - `bash tests/scripts/test_api_inventory_pkcs11_high_entry_truth_contract.sh`
    - PASS
  - `git diff --check`
    - PASS
