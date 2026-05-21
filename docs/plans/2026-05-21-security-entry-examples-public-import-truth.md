# Security Entry Examples Public Import Truth

## Goal

收口三份仍在公开示例里保留历史
`fafafa.ssl.base`
/
`fafafa.ssl.factory`
导入的安全相关 example，
让它们回到当前真实入口：

- 通用 facade surface
  直接来自
  `fafafa.ssl`
- specialized pinning / rotation surface
  继续来自
  `fafafa.ssl.cert.pinning`
  /
  `fafafa.ssl.cert.rotation`
  /
  `fafafa.ssl.context.builder`
- 不再因为历史残留，
  继续误导调用方拆分导入

## Scope

- Update:
  - `examples/simple_test.pas`
  - `examples/example_cert_pinning_simple.pas`
  - `examples/security_enhancements_demo.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`
- Add:
  - `docs/plans/2026-05-21-security-entry-examples-public-import-truth.md`
  - `tests/scripts/test_security_entry_examples_public_import_truth_contract.sh`

不做：

- 不改 runtime 实现
- 不重开 broader facade export closure
- 不扩大到 `examples/production/*`

## Why This Batch

当前 `fafafa.ssl` 主门面已经 re-export：

- `sslHashSHA256`
- `ISSLContext`
- `ISSLConnection`
- `TSSLFactory`

而 pinning / rotation 的 specialized surface
本来就由专属 unit 发布：

- `ptPublicKey`
- `TRotationEventType`
- `TRotationConfig`
- `TCertificateRotationManager`

这说明三份示例里的残余：

- `examples/simple_test.pas`
  继续写
  `fafafa.ssl.base`
- `examples/example_cert_pinning_simple.pas`
  为了 `ptPublicKey`
  继续带入
  `fafafa.ssl.base`
- `examples/security_enhancements_demo.pas`
  同时保留
  `fafafa.ssl.base`
  /
  `fafafa.ssl.factory`

都已经不再是实现能力缺口，
而是活跃 security examples
继续教学旧入口的 guidance drift。

## Minimal Fix

1. 为这三份示例
   新增 focused import contract
2. 把普通 facade surface
   收回到
   `fafafa.ssl`
3. 保留确实需要的 specialized unit
   并去掉
   `base`
   /
   `factory`
4. 跑 focused contract
   与最小 compile proof，
   确认当前入口真相在源码层真实成立

## Verification

```bash
bash -n tests/scripts/test_security_entry_examples_public_import_truth_contract.sh
bash tests/scripts/test_security_entry_examples_public_import_truth_contract.sh

mkdir -p tmp/example_import_truth_simple_test
fpc -B -Fu./src \
  -FUtmp/example_import_truth_simple_test \
  -FEtmp/example_import_truth_simple_test \
  -otmp/example_import_truth_simple_test/simple_test \
  examples/simple_test.pas

mkdir -p tmp/example_import_truth_cert_pinning_simple
fpc -B -Fu./src \
  -FUtmp/example_import_truth_cert_pinning_simple \
  -FEtmp/example_import_truth_cert_pinning_simple \
  -otmp/example_import_truth_cert_pinning_simple/example_cert_pinning_simple \
  examples/example_cert_pinning_simple.pas

mkdir -p tmp/example_import_truth_security_enhancements_demo
fpc -B -Fu./src \
  -FUtmp/example_import_truth_security_enhancements_demo \
  -FEtmp/example_import_truth_security_enhancements_demo \
  -otmp/example_import_truth_security_enhancements_demo/security_enhancements_demo \
  examples/security_enhancements_demo.pas

git diff --check
```

## Expected Outcome

- `simple_test`
  不再因为 hash enum
  退回
  `fafafa.ssl.base`
- `example_cert_pinning_simple`
  不再为了
  `ptPublicKey`
  带入
  `fafafa.ssl.base`
- `security_enhancements_demo`
  不再继续发布
  `base`
  /
  `factory`
  的历史拆分入口

## Execution Result

- PASS
- focused RED 最终证明的是
  真实 security examples import drift，
  不是实现缺口：
  - `bash tests/scripts/test_security_entry_examples_public_import_truth_contract.sh`
    在修复前
    因
    `examples/simple_test.pas`
    仍保留
    `fafafa.ssl.base`
    而失败
- 最小修复后：
  - `examples/simple_test.pas`
  - `examples/example_cert_pinning_simple.pas`
  - `examples/security_enhancements_demo.pas`
    现已回到
    façade
    +
    specialized unit
    的当前入口真相
- focused verification：
  - `bash -n tests/scripts/test_security_entry_examples_public_import_truth_contract.sh`
    - PASS
  - `bash tests/scripts/test_security_entry_examples_public_import_truth_contract.sh`
    - PASS
  - compile proof：
    - `examples/simple_test.pas`
      - PASS
    - `examples/example_cert_pinning_simple.pas`
      - PASS
    - `examples/security_enhancements_demo.pas`
      - PASS
  - `git diff --check`
    - PASS
- 备注：
  - 编译日志中仍存在仓库既有 warning/note，
    但这批 import 调整未引入新的编译失败
