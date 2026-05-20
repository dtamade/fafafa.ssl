# Managed Result Init Safety Wave 5

## Goal

继续收掉 shared TLS 1.3 application-schedule / ServerHello builder 单元里同类的 managed `TBytes` result 初始化 warning，
并顺手收掉与之同批编译暴露的一个测试 helper warning：

- `src/fafafa.ssl.tls13.appschedule.pas`
- `src/fafafa.ssl.tls13.serverhello.pas`
- `tests/test_tls13_resumption.pas`

这批仍然不改 public 语义，只修 Pascal managed result 的初始化路径。

## Scope

- `src/fafafa.ssl.tls13.appschedule.pas`
- `src/fafafa.ssl.tls13.serverhello.pas`
- `tests/test_tls13_resumption.pas`
- `tests/scripts/test_managed_result_init_safety_wave5_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Why This Batch

上一批已经收掉：

- `tls13.keyschedule`
- `tls13.clienthello`

但 focused compile 继续暴露了下一组同家族 warning：

- `fafafa.ssl.tls13.appschedule.pas`
  - `TLS13ComputeResumptionMasterSecretFromTranscriptHash(...)`
  - `TLS13DeriveResumptionPSKFromTranscriptHash(...)`
- `fafafa.ssl.tls13.serverhello.pas`
  - `BuildTLS13ServerHelloBody(...)`
  - `BuildTLS13ServerHelloHandshake(...)`
  - `BuildTLS13ServerHelloHandshakeWithSelectedPSK(...)`
- `tests/test_tls13_resumption.pas`
  - `HexToBytes(...)`

同时静态扫读也说明，这两个生产单元里还有同家族的空结果初始化写法，虽然未必每条都恰好是当前告警行：

- `appschedule`
  - `HashTranscriptForSuite(...)`
  - `HKDFExtractForSuite(...)`
  - `HKDFExpandLabelForSuite(...)`
- `serverhello`
  - `BuildExtensionHeader(...)`

这些函数都属于 TLS 1.3 shared runtime path，而不是边角 demo：

- `appschedule`
  承担 resumption master secret / resumption PSK 及 application secret 派生
- `serverhello`
  承担最小 TLS 1.3 ServerHello / selected PSK ServerHello 组包

## Expected Result

- 所有目标函数都在首次使用空 `Result` 前显式初始化 `Result := nil`
- 目标函数不再通过未初始化结果上的 `SetLength(Result, 0)` 兜底
- focused compile 中：
  - `tls13.appschedule`
  - `tls13.serverhello`
  - `tests/test_tls13_resumption.pas`
  这批 managed-result warning 消失

## Verification

```bash
bash -n tests/scripts/test_managed_result_init_safety_wave5_contract.sh
bash tests/scripts/test_managed_result_init_safety_wave5_contract.sh
mkdir -p tmp/tls13_appschedule_units tmp/tls13_appschedule_bin
fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_appschedule_units -FEtmp/tls13_appschedule_bin -otest_tls13_appschedule tests/test_tls13_appschedule.pas
./tmp/tls13_appschedule_bin/test_tls13_appschedule
mkdir -p tmp/tls13_serverhello_units tmp/tls13_serverhello_bin
fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_serverhello_units -FEtmp/tls13_serverhello_bin -otest_tls13_serverhello_builder tests/test_tls13_serverhello_builder.pas
./tmp/tls13_serverhello_bin/test_tls13_serverhello_builder
mkdir -p tmp/tls13_resumption_units tmp/tls13_resumption_bin
fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_resumption_units -FEtmp/tls13_resumption_bin -otest_tls13_resumption tests/test_tls13_resumption.pas
./tmp/tls13_resumption_bin/test_tls13_resumption
fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_resumption_units -FEtmp/tmp -otest_tls13_resumption tests/test_tls13_resumption.pas 2>&1 | rg "tls13\\.appschedule|tls13\\.serverhello|test_tls13_resumption|Warning: Function result variable of a managed type does not seem to be initialized"
git diff --check
```
