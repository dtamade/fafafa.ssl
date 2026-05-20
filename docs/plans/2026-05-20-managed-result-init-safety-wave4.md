# Managed Result Init Safety Wave 4

## Goal

继续收掉 shared TLS 1.3 key-schedule / ClientHello builder 单元里同类的 managed `TBytes` result 初始化 warning，
把上一波从 `tls13.primitives` / `constant_time` 再推进到：

- `src/fafafa.ssl.tls13.keyschedule.pas`
- `src/fafafa.ssl.tls13.clienthello.pas`

这批仍然不改 public 语义，只修 Pascal managed result 的初始化路径。

## Scope

- `src/fafafa.ssl.tls13.keyschedule.pas`
- `src/fafafa.ssl.tls13.clienthello.pas`
- `tests/scripts/test_managed_result_init_safety_wave4_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Why This Batch

上一批已经收掉：

- `tls13.primitives`
  的 shared HKDF / byte helper
- `crypto.constant_time`
  的 `Select(...)`

但 focused compile 继续暴露了下一组同类 warning：

- `fafafa.ssl.tls13.keyschedule.pas(228,19)`
  - `TLS13ComputePSKBinderForCipherSuite(...)`
- `fafafa.ssl.tls13.clienthello.pas`
  - `BuildExtensionPreSharedKey(...)`
  - `BuildTLS13ClientHelloBody(...)`
  - `BuildTLS13ClientHelloBodyWithPSKCore(...)`
  - `BuildTLS13ClientHelloHandshake(...)`
  - `BuildTLS13ClientHelloHandshakeWithPSK(...)`
  - `BuildTLS13ClientHelloHandshakeWithComputedPSKBinder(...)`

同时静态扫读也说明，这两个单元里还有同家族的空结果初始化写法，虽然未必每条都恰好触发当前编译 warning：

- `HashTranscriptForSuite(...)`
- `HKDFExtractForSuite(...)`
- `HKDFExpandLabelForSuite(...)`
- `BuildExtensionServerName(...)`
- `BuildExtensionALPN(...)`

这些函数都属于 TLS 1.3 shared runtime path，而不是 demo：

- `keyschedule`
  承担 PSK binder / transcript / HKDF label 派生
- `clienthello`
  承担普通 ClientHello、PSK ClientHello、early-data ClientHello 的统一组包

## Expected Result

- 所有目标函数都在首次使用空 `Result` 前显式初始化 `Result := nil`
- 目标函数不再通过未初始化结果上的 `SetLength(Result, 0)` 兜底
- focused compile 中：
  - `tls13.keyschedule`
  - `tls13.clienthello`
  这批 managed-result warning 消失

## Verification

```bash
bash -n tests/scripts/test_managed_result_init_safety_wave4_contract.sh
bash tests/scripts/test_managed_result_init_safety_wave4_contract.sh
mkdir -p tmp/tls13_foundation_units tmp/tls13_foundation_bin
fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_foundation_units -FEtmp/tls13_foundation_bin -otest_tls13_foundation tests/test_tls13_foundation.pas
./tmp/tls13_foundation_bin/test_tls13_foundation
mkdir -p tmp/tls13_resumption_units tmp/tls13_resumption_bin
fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_resumption_units -FEtmp/tls13_resumption_bin -otest_tls13_resumption tests/test_tls13_resumption.pas
./tmp/tls13_resumption_bin/test_tls13_resumption
fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_foundation_units -FEtmp/tls13_foundation_bin -otest_tls13_foundation tests/test_tls13_foundation.pas 2>&1 | rg "tls13\\.keyschedule|tls13\\.clienthello|Warning: Function result variable of a managed type does not seem to be initialized"
git diff --check
```
