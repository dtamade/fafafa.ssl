# Active Examples Public Import Truth Alignment

## Goal

收口一批当前仍会把调用方带回
`fafafa.ssl.base`
/
`fafafa.ssl.factory`
的活跃示例导入漂移，
让这些 example/helper
重新回到当前已补齐 supporting types 之后的
public facade truth：

- 普通示例优先直接
  `uses fafafa.ssl`
- 只有明确依赖 specialized helper unit 时，
  才额外引入对应单元
- 不再因为历史 `base` / `factory`
  用法残留，
  继续误导调用方拆分导入

## Scope

- Update:
  - `examples/04_https_rest_client.pas`
  - `examples/07_certificate_chain.pas`
  - `examples/demo_fluent_api.pas`
  - `examples/winssl_health_checker.pas`
  - `examples/winssl_rest_client.pas`
  - `examples/fafafa.examples.tcp.pas`
  - `examples/validation/real_world_test.pas`
  - `task_plan.md`
- Add:
  - `tests/scripts/test_active_examples_public_import_truth_contract.sh`
  - `docs/plans/2026-05-21-active-examples-public-import-truth-alignment.md`

不做：

- 不改 runtime 实现
- 不扩大到 `examples/production/*`
- 不重开 broader facade slimming / unit reorganization

## Why This Batch

当前 `fafafa.ssl` 主门面已经补齐：

- `TSSLFactory`
- `ISSLLibrary` / `ISSLContext` / `ISSLConnection`
- `ISSLCertificateVerification`
- `TSSLDataResult`
- `sslErrInvalidParam`
- `TBytesView`

以及一批 examples 常用 supporting types。

但多份当前仍可见的 example/helper
还在继续写：

- `fafafa.ssl.base`
- `fafafa.ssl.factory`

这已经不是实现能力缺口，
而是一个真实的 public import guidance drift：

- 调用方会误以为
  示例仍然要求拆分
  `base` / `factory`
- helper unit
  也会把这种旧入口继续扩散到下游示例

## Minimal Fix

1. 为目标 examples
   新增一个 focused import contract
2. 将这些文件的 `uses`
   收回到当前主门面 truth
3. 跑 focused contract
   与最小 compile proof，
   确认门面导入在活跃 examples 上真实可用

## Verification

```bash
bash -n tests/scripts/test_active_examples_public_import_truth_contract.sh
bash tests/scripts/test_active_examples_public_import_truth_contract.sh

mkdir -p tmp/example_import_truth_04_https_rest_client
fpc -B -Fu./src \
  -FUtmp/example_import_truth_04_https_rest_client \
  -FEtmp/example_import_truth_04_https_rest_client \
  -otmp/example_import_truth_04_https_rest_client/04_https_rest_client \
  examples/04_https_rest_client.pas

mkdir -p tmp/example_import_truth_07_certificate_chain
fpc -B -Fu./src \
  -FUtmp/example_import_truth_07_certificate_chain \
  -FEtmp/example_import_truth_07_certificate_chain \
  -otmp/example_import_truth_07_certificate_chain/07_certificate_chain \
  examples/07_certificate_chain.pas

mkdir -p tmp/example_import_truth_demo_fluent_api
fpc -B -Fu./src \
  -FUtmp/example_import_truth_demo_fluent_api \
  -FEtmp/example_import_truth_demo_fluent_api \
  -otmp/example_import_truth_demo_fluent_api/demo_fluent_api \
  examples/demo_fluent_api.pas

mkdir -p tmp/example_import_truth_real_world
fpc -B -Fu./src -Fu./examples \
  -FUtmp/example_import_truth_real_world \
  -FEtmp/example_import_truth_real_world \
  -otmp/example_import_truth_real_world/real_world_test \
  examples/validation/real_world_test.pas

git diff --check
```

## Expected Outcome

- 这些活跃 examples
  不再继续教学
  `fafafa.ssl.base`
  /
  `fafafa.ssl.factory`
- 调用方从 example
  可以直接学到当前门面入口真相
- 下游 helper
  `fafafa.examples.tcp`
  也不会继续把
  `fafafa.ssl.base`
  扩散给依赖它的测试示例

## Execution Result

- PASS
- focused RED 首轮证明的是
  真实 examples import drift，
  不是实现缺口：
  - `HEAD` 快照下
    新 contract
    第一条就因
    `examples/04_https_rest_client.pas`
    仍未回到
    `fafafa.ssl`
    而失败
- 最小修复后：
  - `examples/04_https_rest_client.pas`
  - `examples/07_certificate_chain.pas`
  - `examples/demo_fluent_api.pas`
  - `examples/winssl_health_checker.pas`
  - `examples/winssl_rest_client.pas`
  - `examples/fafafa.examples.tcp.pas`
  - `examples/validation/real_world_test.pas`
    现已统一回到当前 public facade import truth
- focused verification：
  - `bash -n tests/scripts/test_active_examples_public_import_truth_contract.sh`
    - PASS
  - `bash tests/scripts/test_active_examples_public_import_truth_contract.sh`
    - PASS
  - `HEAD` snapshot contract
    - FAIL
    - 证明旧 examples
      确实还在残留
      `base` / `factory`
      导入
  - compile proof：
    - `examples/04_https_rest_client.pas`
      - PASS
    - `examples/07_certificate_chain.pas`
      - PASS
    - `examples/demo_fluent_api.pas`
      - PASS
    - `examples/validation/real_world_test.pas`
      - PASS
  - `git diff --check`
    - PASS
- 备注：
  - 编译日志中仍存在仓库既有 warning/note，
    但这批 import 调整未引入新的编译失败
