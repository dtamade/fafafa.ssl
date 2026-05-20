# Shared Capability Matrix Runtime Truth Hardening

## Goal

把
`tests/test_capability_matrix_v12.pas`
从
“shared capability snapshot printer + 少量 cipher contract”
推进成
“会对当前主线 capability truth 真实报警的 focused regression”。

这批继续不改生产实现，
只做：

- shared capability regression 的 runtime hard assertions
- 一个静态 contract，防止测试退回只打印不报警
- plan / findings / progress 账本同步

## Why This Batch

上一批已经补齐了
`FreePascal`
覆盖，
但当前这条 shared regression
仍然存在明显弱点：

- 多数字段只是打印
- 对 support-level / legacy bool projection
  没有 shared entrypoint 级别的硬断言
- 对
  `OpenSSL`
  /
  `FreePascal`
  这两个当前 Linux 上最容易稳定执行的 backend，
  也还缺少 backend-specific truth guard

这会导致：

- capability drift
  更容易在 matrix 页面里悄悄漂过去
- 我们仍然要依赖更窄的 contract
  或人工读输出来发现问题

## Scope

- Add:
  - `docs/plans/2026-05-21-shared-capability-matrix-runtime-truth-hardening.md`
  - `tests/scripts/test_capability_matrix_v12_runtime_truth_contract.sh`
- Update:
  - `tests/test_capability_matrix_v12.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Runtime Truth To Lock

### Shared projection truth

- `BackendType` 必须匹配请求 backend
- paired support-level feature
  的 legacy bool projection
  必须保持一致：
  - `SupportsSNI`
  - `SupportsALPN`
  - `SupportsOCSPStapling`
  - `SupportsCertificateTransparency`
  - `SupportsSessionTickets`

### OpenSSL

- `BackendImplType = sslImplCLibrary`
- `RequiresExternalLibrary = True`
- `SupportsTLS13 = True`
- `SNISupport = sslSupportStable`
- `ALPNSupport = sslSupportStable`
- `OCSPStaplingSupport = sslSupportStable`
- `CertTransparencySupport = sslSupportNone`

### FreePascal

- `BackendImplType = sslImplNative`
- `RequiresExternalLibrary = False`
- `SupportsTLS13 = True`
- `SNISupport = sslSupportExperimental`
- `ALPNSupport = sslSupportExperimental`
- `OCSPStaplingSupport = sslSupportExperimental`
- `CertTransparencySupport = sslSupportExperimental`
- `SessionTicketsSupport = sslSupportExperimental`
- `EarlyDataSupport = sslSupportExperimental`
- `SupportsPKCS12 = False`
- `SupportsPasswordProtectedKeys = False`
- `SupportsCustomCipherSuites = False`
- `SupportsCallbacks = False`

## Verification

```bash
bash -n tests/scripts/test_capability_matrix_v12_runtime_truth_contract.sh
bash tests/scripts/test_capability_matrix_v12_runtime_truth_contract.sh
mkdir -p tmp/test_capability_matrix_v12 && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_capability_matrix_v12 -FEtmp/test_capability_matrix_v12 -otmp/test_capability_matrix_v12/test_capability_matrix_v12 tests/test_capability_matrix_v12.pas
./tmp/test_capability_matrix_v12/test_capability_matrix_v12
git diff --check
```

## Expected Result

- shared capability-matrix regression
  不再只是打印
  `OpenSSL / FreePascal`
  truth
- 如果 support-level/runtime truth 发生漂移，
  这条 shared regression
  会直接 fail
- 当前 host
  上验证应继续显示：
  - `OpenSSL`
    executed
  - `FreePascal`
    executed
  - 其余 backend
    按当前环境正常 skip
