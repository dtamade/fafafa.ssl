# Managed Result Init Safety Wave 2

## Goal

继续收掉 shared implementation 单元里同类的 managed `TBytes` result 初始化 warning，
把第一波从 public facade / connection base 扩展到：

- `src/fafafa.ssl.tls13.wire.pas`
- `src/fafafa.ssl.freepascal.session.pas`

这批仍然不改 public 语义，只修 Pascal managed result 的初始化路径。

## Scope

- `src/fafafa.ssl.tls13.wire.pas`
- `src/fafafa.ssl.freepascal.session.pas`
- `tests/test_freepascal_client_session_resumption.pas`
- `tests/scripts/test_managed_result_init_safety_wave2_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Why This Batch

上一批已经收掉：

- `CreateDefaultConfig(...)`
- `TBaseSSLConnection.GetConnectionInfo(...)`
- `TBaseSSLConnection.GetDiagnosticInfo(...)`
- 两条空 `TBytes` 默认返回

但 focused compile 继续暴露了下一组同类 warning：

- `fafafa.ssl.tls13.wire.pas(153,19)`
  - `BuildTLSPlaintext(...)`
- `fafafa.ssl.freepascal.session.pas(207,19)`
  - `ReadVector16(...)`
- `fafafa.ssl.freepascal.session.pas(351,19)`
  - `TFreePascalSession.Serialize(...)`

这几处都属于 shared implementation，而不是边角 demo：

- `BuildTLSPlaintext(...)`
  被大量 FreePascal/TLS13/runtime tests 复用
- `TFreePascalSession.Serialize/Deserialize`
  是 session resumption / early-data 相关 runtime path 的基础件
- focused compile also exposed the same warning class in
  `tests/test_freepascal_client_session_resumption.pas`; those helpers build
  TLS 1.3 handshake messages for the runtime proof, so the harness should be
  warning-clean for this batch too

## Expected Result

- `BuildTLSPlaintext(...)`
  在 `SetLength(Result, ...)` 前先显式初始化 `Result`
- `ReadVector16(...)`
  在 `SetLength(Result, ...)` 前先显式初始化 `Result`
- `TFreePascalSession.Serialize(...)`
  以类型安全方式初始化空 `TBytes` 结果
- `tests/test_freepascal_client_session_resumption.pas`
  中的 TLS 1.3 handshake helper result buffers 改为 `Result := nil`
- focused compile 中：
  - `tls13.wire`
  - `freepascal.session`
  - `test_freepascal_client_session_resumption`
  这批 managed-result warning 消失

## Verification

```bash
bash -n tests/scripts/test_managed_result_init_safety_wave2_contract.sh
bash tests/scripts/test_managed_result_init_safety_wave2_contract.sh
mkdir -p tmp/tls13_foundation_units tmp/tls13_foundation_bin
fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_foundation_units -FEtmp/tls13_foundation_bin -otest_tls13_foundation tests/test_tls13_foundation.pas
./tmp/tls13_foundation_bin/test_tls13_foundation
mkdir -p tmp/fp_session_units tmp/fp_session_bin
fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/fp_session_units -FEtmp/fp_session_bin -otest_freepascal_client_session_resumption tests/test_freepascal_client_session_resumption.pas
./tmp/fp_session_bin/test_freepascal_client_session_resumption
git diff --check
```

## Execution Result

- `BuildTLSPlaintext(...)`、`ReadVector16(...)`、
  `TFreePascalSession.Serialize(...)` 当前均已是类型安全初始化形态。
- 本轮 focused compile 暴露并修复了验证 harness 中同类 `Result`
  初始化 warning：
  - `HashTranscriptForSuite`
  - `BuildFinishedMessage`
  - `BuildNewSessionTicketMessage`
  - `BuildServerHelloWithSelectedPSK`
  - `BuildEncryptedExtensionsMessage`
- `tests/scripts/test_managed_result_init_safety_wave2_contract.sh`
  已扩展到同时守住生产单元和 harness helper。
- Focused verification passed:
  - `bash -n tests/scripts/test_managed_result_init_safety_wave2_contract.sh`
  - `bash tests/scripts/test_managed_result_init_safety_wave2_contract.sh`
  - `/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_foundation_units -FEtmp/tls13_foundation_bin -otest_tls13_foundation tests/test_tls13_foundation.pas`
  - `./tmp/tls13_foundation_bin/test_tls13_foundation`
  - `/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/fp_session_units -FEtmp/fp_session_bin -otest_freepascal_client_session_resumption tests/test_freepascal_client_session_resumption.pas`
  - `./tmp/fp_session_bin/test_freepascal_client_session_resumption`
- Compile-log grep found no remaining
  `Warning: Function result variable of a managed type` in the two focused
  compile logs.
