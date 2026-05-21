# 2026-05-21 Backend Capability Matrix Active Import Truth And Contract Hardening

## Goal

修复两份仍然高可见的 backend capability matrix
在“后端标识 / 快速示例”处继续教学
`uses fafafa.ssl.base;`
的问题，
并补上现有 focused contract
对单行导入形式的漏检，
避免再次出现：

- 文档仍然保留旧 public import
- contract 却误报 PASS
- working memory
  被假绿带偏

## Scope

- Add:
  - `docs/plans/2026-05-21-backend-capability-matrix-active-import-truth-and-contract-hardening.md`
- Update:
  - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
  - `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
  - `tests/scripts/test_winssl_store_active_docs_truth_contract.sh`
  - `tests/scripts/test_mbedtls_active_docs_capability_truth_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

本批不做：

- 不改 backend capability 语义
- 不改 runtime 实现
- 不重跑重型 compile / gate
- 不重开已收口的 native-handle quick ref 或其它 examples/doc batches

## Architecture Truth

- 这两份矩阵页当前都还在“后端标识”代码块里写：
  - `uses fafafa.ssl.base;`
- 但当前普通 public entry
  已经回到：
  - `fafafa.ssl`
- 当代码块继续使用：
  - `TSSLContextBuilder.Create`
  时，
  builder entry
  也应显式写成：
  - `fafafa.ssl.context.builder`
- 当前 workflow 问题不只是文档 drift，
  还有 contract 漏检：
  - WinSSL focused contract
    只禁止了
    `  fafafa.ssl.base,`
  - 没有覆盖
    `uses fafafa.ssl.base;`
    这种单行写法

## Steps

1. 新增本批计划，明确这是
   `active doc truth + contract hardening`
   联合收口。
2. 先收紧两个 focused contract：
   - WinSSL contract
     明确禁止
     `uses fafafa.ssl.base;`
   - MbedTLS contract
     补上当前 public import truth
3. 运行 focused contract，
   在当前 HEAD 上先拿到 RED。
4. 最小修改两份 matrix：
   - 改为
     `fafafa.ssl`
   - 补上
     `fafafa.ssl.context.builder`
5. 重跑 focused contract
   与
   `git diff --check`
   收口。

## Verification

```bash
bash -n tests/scripts/test_winssl_store_active_docs_truth_contract.sh
bash tests/scripts/test_winssl_store_active_docs_truth_contract.sh
bash -n tests/scripts/test_mbedtls_active_docs_capability_truth_contract.sh
bash tests/scripts/test_mbedtls_active_docs_capability_truth_contract.sh
git diff --check
```

## Expected Result

- `WINSSL_BACKEND_CAPABILITY_MATRIX`
  与
  `MBEDTLS_BACKEND_CAPABILITY_MATRIX`
  不再继续教学
  `fafafa.ssl.base`
- active backend-identifier snippet
  回到当前
  `fafafa.ssl`
  +
  `fafafa.ssl.context.builder`
  真相
- focused contract
  不再把
  `uses fafafa.ssl.base;`
  这种单行旧导入漏检成绿灯
