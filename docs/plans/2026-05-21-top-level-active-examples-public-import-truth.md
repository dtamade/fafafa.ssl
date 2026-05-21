# Top-Level Active Examples Public Import Truth

## Goal

收口一组当前仍在顶层活跃示例里保留历史
`fafafa.ssl.base`
/
`fafafa.ssl.factory`
拆分导入的 example，
让这些高可见入口回到当前真实 public facade truth：

- 普通 public surface
  优先直接来自
  `fafafa.ssl`
- 只有确实需要的
  OpenSSL API specialized unit
  才继续保留
- 不再因为旧示例残留，
  继续误导调用方拆分导入

## Scope

- Update:
  - `examples/example_factory_usage.pas`
  - `examples/certificate_verification_example.pas`
  - `examples/winssl_https_downloader.pas`
  - `examples/05_https_server.pas`
  - `examples/06_digital_signature.pas`
  - `examples/08_mutual_tls.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`
- Add:
  - `docs/plans/2026-05-21-top-level-active-examples-public-import-truth.md`
  - `tests/scripts/test_top_level_active_examples_public_import_truth_contract.sh`

不做：

- 不改 runtime 实现
- 不扩大到 `examples/production/*`
- 不在这一批里重开 `https_server/*` / `https_client/*` helper 链的后续收口

## Why This Batch

当前 `fafafa.ssl` 主门面已经 re-export：

- `TSSLFactory`
- `ISSLLibrary`
- `ISSLContext`
- `ISSLConnection`
- `ISSLClientConnection`
- `ISSLCertificate`
- `ISSLCertificateStore`
- `ISSLSession`
- `ISSLSessionResumption`
- `TSSLConfig`
- `TSSLCertVerifyResult`
- `sslOpenSSL`
- `sslWinSSL`
- `sslCtxClient`
- `sslCtxServer`
- `sslProtocolTLS12`
- `sslProtocolTLS13`
- `sslVerifyPeer`
- `sslVerifyFailIfNoPeerCert`
- `LibraryTypeToString(...)`
- `ProtocolVersionToString(...)`

这说明以下顶层示例里的残余：

- `example_factory_usage`
- `certificate_verification_example`
- `winssl_https_downloader`
- `05_https_server`
- `06_digital_signature`
- `08_mutual_tls`

大多已经不是实现能力缺口，
而是仍在继续教学
`base`
/
`factory`
旧入口的 guidance drift。

其中唯一需要顺手收口的
小语义残留是：

- `example_factory_usage`
  仍直接读取
  `SSL_LIBRARY_NAMES[...]`
  这个 base-owner 常量

当前更合适的 public path
是切到
`LibraryTypeToString(...)`。

## Minimal Fix

1. 为目标示例新增一个 focused import contract
2. 将这些文件的普通入口收回到 `fafafa.ssl`
3. 对 `example_factory_usage`
   用 `LibraryTypeToString(...)`
   替代
   `SSL_LIBRARY_NAMES[...]`
4. 跑 focused contract 与最小 compile proof，
   确认当前顶层示例入口真相真实成立

## Verification

```bash
bash -n tests/scripts/test_top_level_active_examples_public_import_truth_contract.sh
bash tests/scripts/test_top_level_active_examples_public_import_truth_contract.sh

mkdir -p tmp/example_import_truth_factory_usage
fpc -B -Fu./src \
  -FUtmp/example_import_truth_factory_usage \
  -FEtmp/example_import_truth_factory_usage \
  -otmp/example_import_truth_factory_usage/example_factory_usage \
  examples/example_factory_usage.pas

mkdir -p tmp/example_import_truth_cert_verify
fpc -B -Fu./src \
  -FUtmp/example_import_truth_cert_verify \
  -FEtmp/example_import_truth_cert_verify \
  -otmp/example_import_truth_cert_verify/certificate_verification_example \
  examples/certificate_verification_example.pas

mkdir -p tmp/example_import_truth_winssl_downloader
fpc -B -Fu./src \
  -FUtmp/example_import_truth_winssl_downloader \
  -FEtmp/example_import_truth_winssl_downloader \
  -otmp/example_import_truth_winssl_downloader/winssl_https_downloader \
  examples/winssl_https_downloader.pas

mkdir -p tmp/example_import_truth_https_server
fpc -B -Fu./src \
  -FUtmp/example_import_truth_https_server \
  -FEtmp/example_import_truth_https_server \
  -otmp/example_import_truth_https_server/05_https_server \
  examples/05_https_server.pas

mkdir -p tmp/example_import_truth_digital_signature
fpc -B -Fu./src \
  -FUtmp/example_import_truth_digital_signature \
  -FEtmp/example_import_truth_digital_signature \
  -otmp/example_import_truth_digital_signature/06_digital_signature \
  examples/06_digital_signature.pas

mkdir -p tmp/example_import_truth_mutual_tls
fpc -B -Fu./src \
  -FUtmp/example_import_truth_mutual_tls \
  -FEtmp/example_import_truth_mutual_tls \
  -otmp/example_import_truth_mutual_tls/08_mutual_tls \
  examples/08_mutual_tls.pas

git diff --check
```

## Expected Outcome

- 这批顶层活跃示例
  不再继续教学
  `fafafa.ssl.base`
  /
  `fafafa.ssl.factory`
- `example_factory_usage`
  不再通过
  `SSL_LIBRARY_NAMES[...]`
  把调用方引回 base-owner constant
- 调用方从这些高可见 examples
  可以直接学到当前门面入口真相

## Execution Result

- PASS
- focused RED 首轮证明的是
  真实顶层活跃示例 import drift，
  不是实现缺口：
  - `bash tests/scripts/test_top_level_active_examples_public_import_truth_contract.sh`
    在修复前
    因
    `examples/example_factory_usage.pas`
    仍保留
    `fafafa.ssl.base`
    而失败
- 最小修复后：
  - `examples/example_factory_usage.pas`
  - `examples/certificate_verification_example.pas`
  - `examples/winssl_https_downloader.pas`
  - `examples/05_https_server.pas`
  - `examples/06_digital_signature.pas`
  - `examples/08_mutual_tls.pas`
    现已回到当前 public facade import truth
- focused verification：
  - `bash -n tests/scripts/test_top_level_active_examples_public_import_truth_contract.sh`
    - PASS
  - `bash tests/scripts/test_top_level_active_examples_public_import_truth_contract.sh`
    - PASS
  - compile proof：
    - `examples/example_factory_usage.pas`
      - PASS
    - `examples/certificate_verification_example.pas`
      - PASS
    - `examples/winssl_https_downloader.pas`
      - PASS
    - `examples/05_https_server.pas`
      - PASS
    - `examples/06_digital_signature.pas`
      - PASS
    - `examples/08_mutual_tls.pas`
      - PASS
  - `git diff --check`
    - PASS
- 备注：
  - 编译日志中仍存在仓库既有 warning/note，
    但这批 import 调整未引入新的编译失败
