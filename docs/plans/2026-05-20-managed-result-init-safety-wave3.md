# Managed Result Init Safety Wave 3

## Goal

继续收掉 shared crypto/TLS helper 单元里同类的 managed `TBytes` result 初始化 warning，
把上一波从 `tls13.wire` / `freepascal.session` 再推进到：

- `src/fafafa.ssl.tls13.primitives.pas`
- `src/fafafa.ssl.crypto.constant_time.pas`

这批仍然不改 public 语义，只修 Pascal managed result 的初始化路径。

## Scope

- `src/fafafa.ssl.tls13.primitives.pas`
- `src/fafafa.ssl.crypto.constant_time.pas`
- `tests/unit/test_constant_time.pas`
- `tests/scripts/test_managed_result_init_safety_wave3_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Why This Batch

上一批已经收掉：

- `BuildTLSPlaintext(...)`
- `ReadVector16(...)`
- `TFreePascalSession.Serialize(...)`

但 focused compile 继续暴露了下一组同类 warning：

- `fafafa.ssl.tls13.primitives.pas`
  - `CopyBytes(...)`
  - `ConcatBytes(...)`
  - `BuildTLS13HKDFLabel(...)`
  - `HKDF_Expand_SHA256(...)`
  - `HKDF_Expand_SHA384(...)`
- `fafafa.ssl.crypto.constant_time.pas`
  - `TConstantTime.Select(...)`

这些函数都属于 shared helper，而不是边角 demo：

- `HKDF_Expand_*`
  是 TLS 1.3 key schedule / transcript 相关基础件
- `BuildTLS13HKDFLabel(...)`
  是 TLS 1.3 `ExpandLabel` 路径的中心 helper
- `TConstantTime.Select(...)`
  是 shared constant-time byte selection helper
- focused verification also depends on `tests/unit/test_constant_time.pas`;
  its old wall-clock variance assertion used millisecond-resolution
  `GetTickCount64` around very short loops, which can fail randomly on normal
  scheduler noise even when the constant-time semantics are correct

## Expected Result

- 所有目标函数都在首次 `SetLength(Result, ...)` 之前显式初始化 `Result`
- 零长度 `TBytes` 返回不再通过未初始化结果上的 `SetLength(Result, 0)`
- focused compile 中：
  - `tls13.primitives`
  - `crypto.constant_time`
  这批 managed-result warning 消失
- `tests/unit/test_constant_time.pas` keeps deterministic constant-time API
  semantics checks without failing on low-resolution timing jitter

## Verification

```bash
bash -n tests/scripts/test_managed_result_init_safety_wave3_contract.sh
bash tests/scripts/test_managed_result_init_safety_wave3_contract.sh
mkdir -p tmp/tls13_foundation_units tmp/tls13_foundation_bin
fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_foundation_units -FEtmp/tls13_foundation_bin -otest_tls13_foundation tests/test_tls13_foundation.pas
./tmp/tls13_foundation_bin/test_tls13_foundation
mkdir -p tmp/constant_time_units tmp/constant_time_bin
fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/constant_time_units -FEtmp/constant_time_bin -otest_constant_time tests/unit/test_constant_time.pas
./tmp/constant_time_bin/test_constant_time
git diff --check
```

## Execution Result

- Wave3 production targets were already in the intended type-safe result
  initialization shape on current head.
- `tests/unit/test_constant_time.pas` had a flaky timing-variance assertion:
  it measured 100 short compare loops with `GetTickCount64`, so the average was
  near zero and scheduler noise could report thousands of percent deviation.
- The timing check now keeps deterministic equal/different compare sanity loops
  and no longer treats low-resolution wall-clock variance as a pass/fail signal.
- Focused verification passed:
  - `bash -n tests/scripts/test_managed_result_init_safety_wave3_contract.sh`
  - `bash tests/scripts/test_managed_result_init_safety_wave3_contract.sh`
  - `/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_foundation_units -FEtmp/tls13_foundation_bin -otest_tls13_foundation tests/test_tls13_foundation.pas`
  - `./tmp/tls13_foundation_bin/test_tls13_foundation`
  - `/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/constant_time_units -FEtmp/constant_time_bin -otest_constant_time tests/unit/test_constant_time.pas`
  - `./tmp/constant_time_bin/test_constant_time`
- Compile-log grep found no remaining
  `Warning: Function result variable of a managed type` in the two focused
  compile logs.
