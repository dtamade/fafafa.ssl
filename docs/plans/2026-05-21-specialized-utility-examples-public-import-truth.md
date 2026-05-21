# Specialized / Utility Examples Public Import Truth

## Goal

收口一组仍然活跃、
但不属于 production
示例主线的
specialized / utility examples
里残留的历史
`fafafa.ssl.base`
/
`fafafa.ssl.factory`
入口漂移，
让它们回到当前更准确的边界：

- façade 已公开的
  通用类型 / 常量 / 接口
  直接来自
  `fafafa.ssl`
- specialized / raw OpenSSL
  owner unit
  继续保留
- 纯文字 guidance
  也不再继续教学
  旧 split import

## Scope

- Add:
  - `docs/plans/2026-05-21-specialized-utility-examples-public-import-truth.md`
  - `tests/scripts/test_specialized_utility_examples_public_import_truth_contract.sh`
- Update:
  - `examples/example_cert_pinning.pas`
  - `examples/example_error_handling.pas`
  - `examples/example_result_type.pas`
  - `examples/example_streaming_operations.pas`
  - `examples/test_ssl_context.lpr`
  - `examples/02_generate_certificate.pas`
  - `examples/09_winssl_fips.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

不做：

- 不扩大到 `examples/production/*`
- 不改 runtime 实现
- 不重开 OpenSSL raw API /
  WinSSL FIPS
  行为设计

## Why This Batch

当前主门面
`fafafa.ssl`
已经 re-export：

- `ISSLContext`
- `ISSLLibrary`
- `ISSLCertificate`
- `TSSLOperationResult`
- `TSSLDataResult`
- `TSSLStringResult`
- `TBytesView`
- `sslCtxServer`
- `sslWinSSL`
- `sslErrGeneral`
- `sslErrInvalidParam`
- `sslErrMemory`
- `sslErrTimeout`

而这批示例里的
specialized / owner surface
本来就已经有
各自正确的 unit：

- `fafafa.ssl.context.builder`
- `fafafa.ssl.cert.pinning`
- `fafafa.ssl.cert.utils`
- `fafafa.ssl.crypto.utils`
- `fafafa.ssl.encoding`
- `fafafa.ssl.openssl.backed`
- `fafafa.ssl.openssl.api.*`
- `fafafa.ssl.exceptions`

这说明当前残余
不是实现能力缺口，
而是：

- 一些 utility / result / error-handling
  示例还在保留
  `fafafa.ssl.base`
- 一些 raw OpenSSL
  示例仍用
  `fafafa.ssl.base`
  获取本已 façade 化的
  interface / enum
- `09_winssl_fips`
  只是打印出来的
  guidance 字符串
  还在教学
  `factory + base`
  旧 split import

## Minimal Fix

1. 新增 focused contract，
   冻结这批示例的
   public import truth
2. 把 façade 已公开的
   通用类型 / 常量
   收回到
   `fafafa.ssl`
3. 保留真正的
   specialized / raw owner unit
4. 跑 focused contract
   和最小 compile proof，
   确认修复只影响 guidance，
   不影响示例能力边界

## Verification

```bash
bash -n tests/scripts/test_specialized_utility_examples_public_import_truth_contract.sh
bash tests/scripts/test_specialized_utility_examples_public_import_truth_contract.sh

mkdir -p tmp/example_import_truth_example_cert_pinning
fpc -B -Fu./src \
  -FUtmp/example_import_truth_example_cert_pinning \
  -FEtmp/example_import_truth_example_cert_pinning \
  -otmp/example_import_truth_example_cert_pinning/example_cert_pinning \
  examples/example_cert_pinning.pas

mkdir -p tmp/example_import_truth_example_error_handling
fpc -B -Fu./src \
  -FUtmp/example_import_truth_example_error_handling \
  -FEtmp/example_import_truth_example_error_handling \
  -otmp/example_import_truth_example_error_handling/example_error_handling \
  examples/example_error_handling.pas

mkdir -p tmp/example_import_truth_example_result_type
fpc -B -Fu./src \
  -FUtmp/example_import_truth_example_result_type \
  -FEtmp/example_import_truth_example_result_type \
  -otmp/example_import_truth_example_result_type/example_result_type \
  examples/example_result_type.pas

mkdir -p tmp/example_import_truth_example_streaming_operations
fpc -B -Fu./src \
  -FUtmp/example_import_truth_example_streaming_operations \
  -FEtmp/example_import_truth_example_streaming_operations \
  -otmp/example_import_truth_example_streaming_operations/example_streaming_operations \
  examples/example_streaming_operations.pas

mkdir -p tmp/example_import_truth_test_ssl_context
fpc -B -Fu./src \
  -FUtmp/example_import_truth_test_ssl_context \
  -FEtmp/example_import_truth_test_ssl_context \
  -otmp/example_import_truth_test_ssl_context/test_ssl_context \
  examples/test_ssl_context.lpr

mkdir -p tmp/example_import_truth_generate_certificate
fpc -B -Fu./src \
  -FUtmp/example_import_truth_generate_certificate \
  -FEtmp/example_import_truth_generate_certificate \
  -otmp/example_import_truth_generate_certificate/generate_certificate \
  examples/02_generate_certificate.pas

git diff --check
```

## Expected Outcome

- utility / result / error-handling
  示例
  不再继续发布
  `fafafa.ssl.base`
- streaming example
  对
  `TBytesView`
  走 façade truth，
  同时保留
  crypto / encoding
  owner units
- raw OpenSSL
  示例
  保留
  `openssl.backed`
  与
  `openssl.api.*`
  owner surface，
  但 façade types
  不再回退
  `base`
- `09_winssl_fips`
  打印出来的示例代码
  不再教学
  `fafafa.ssl.factory`
  /
  `fafafa.ssl.base`

## Execution Result

- PASS
- focused contract:
  - `bash -n tests/scripts/test_specialized_utility_examples_public_import_truth_contract.sh`
    - PASS
  - `bash tests/scripts/test_specialized_utility_examples_public_import_truth_contract.sh`
    - FAIL -> PASS
    - RED summary:
      - `examples/example_error_handling.pas`
        still lacked
        façade import truth
- focused compile proof:
  - `examples/example_cert_pinning.pas`
    - PASS
  - `examples/example_error_handling.pas`
    - PASS
  - `examples/example_result_type.pas`
    - PASS
  - `examples/example_streaming_operations.pas`
    - PASS
  - `examples/test_ssl_context.lpr`
    - PASS
    - note:
      - 额外清掉了
        一个 stale
        `fafafa.ssl.openssl.types`
        旧 unit 引用，
        它不是这次 façade 改动
        引入的新问题
  - `examples/02_generate_certificate.pas`
    - PASS
- hygiene:
  - `git diff --check`
    - PASS
- follow-up sweep:
  - `rg -n "fafafa\\.ssl\\.(base|factory)" examples --glob '!examples/production/**'`
    - remaining source residual:
      - none
    - only residual project metadata:
      - `examples/test_winssl.lpi`
      - `examples/test_openssl.lpi`
- note:
  - compile 输出
    仍有仓库既有
    warning/note，
    但没有新的失败信号
