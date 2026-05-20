# Optional Backends PKCS12 Runtime FreePascal Coverage Completeness

## Goal

修复
`tests/test_optional_backends_pkcs12_capability_truth_contract.pas`
里的一个 runtime coverage hole：

- 测试正文已经调用
  `CheckBackendCapability(sslFreePascal, False);`
- 但测试自身没有
  `uses fafafa.ssl.freepascal.lib`
- 导致当前 Linux host
  上这条 focused runtime contract
  把
  `FreePascal`
  错误地走成
  `backend not available`
  skip

这批只做：

- 补齐 `FreePascal` runtime registration / coverage
- 新增一个静态 contract，
  防止这条 focused runtime proof
  再次漏掉 `FreePascal`
- 账本同步

## Why This Batch

这和之前
`tests/test_capability_matrix_v12.pas`
漏掉
`FreePascal`
是同类问题：

- 不是
  `FreePascal`
  实现 fresh drift
- 而是
  审查入口自身
  的 coverage hole

当前仓库里：

- shared capability regression
  已能执行
  `FreePascal`
- 但 PKCS12 这条更窄的 runtime contract
  仍然把它 skip 掉

如果不补，
我们后面继续说
“各 backend 的 PKCS12 capability truth
  都有 focused runtime proof”
就会留下一个实际空洞。

## Scope

- Add:
  - `docs/plans/2026-05-21-optional-backends-pkcs12-runtime-freepascal-coverage-completeness.md`
  - `tests/scripts/test_optional_backends_pkcs12_runtime_freepascal_coverage_contract.sh`
- Update:
  - `tests/test_optional_backends_pkcs12_capability_truth_contract.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Minimal Fix

1. 在
   `tests/test_optional_backends_pkcs12_capability_truth_contract.pas`
   的 `uses`
   里补上：
   - `fafafa.ssl.freepascal.lib`
2. 保持现有 runtime assertions
   不变，
   只让
   `sslFreePascal`
   真正被注册并执行
3. 新增静态 contract：
   - 测试必须继续显式依赖
     `fafafa.ssl.freepascal.lib`
   - 测试必须继续显式调用
     `CheckBackendCapability(sslFreePascal, False);`

## Verification

```bash
bash -n tests/scripts/test_optional_backends_pkcs12_runtime_freepascal_coverage_contract.sh
bash tests/scripts/test_optional_backends_pkcs12_runtime_freepascal_coverage_contract.sh
bash tests/scripts/test_optional_backends_pkcs12_capability_truth_contract.sh
mkdir -p tmp/test_optional_backends_pkcs12_capability_truth_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_optional_backends_pkcs12_capability_truth_contract -FEtmp/test_optional_backends_pkcs12_capability_truth_contract -otmp/test_optional_backends_pkcs12_capability_truth_contract/test_optional_backends_pkcs12_capability_truth_contract tests/test_optional_backends_pkcs12_capability_truth_contract.pas
./tmp/test_optional_backends_pkcs12_capability_truth_contract/test_optional_backends_pkcs12_capability_truth_contract
git diff --check
```

## Expected Result

- focused PKCS12 runtime contract
  不再把
  `FreePascal`
  误判成
  `backend not available`
- 当前 Linux host
  上这条 runtime proof
  应直接看到：
  - `FreePascal SupportsPKCS12 = False`
  - `OpenSSL SupportsPKCS12 = True`
  - `MbedTLS / WolfSSL SupportsPKCS12 = False`
  - `WinSSL`
    继续因平台原因 skip
