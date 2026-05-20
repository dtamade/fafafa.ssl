# Shared Capability Matrix Session And Publication Hardening

## Goal

继续沿着
`tests/test_capability_matrix_v12.pas`
这条 shared audit entrypoint
推进，
把已经在 focused contracts
里被证实的
session/publication truth
再上收到共享 runtime regression。

这批不改生产实现，
只做：

- shared capability regression
  的 runtime hard assertions
- 一个静态 contract，
  防止这些新增锚点再次悄悄丢失
- `task_plan.md` /
  `findings.md` /
  `progress.md`
  账本同步

## Why This Batch

上一批已经把
`OpenSSL / FreePascal`
的核心 capability truth
从
“只打印”
推进到
“会 fail 的 shared regression”。

但 shared entrypoint
仍然还缺一层高价值 truth：

- `OpenSSL`
  的
  `SessionTicketsSupport`
  /
  `SessionCacheSupport`
  /
  已发布 `PKCS#12`
  /
  custom-cipher
  /
  callback surface
- `FreePascal`
  的
  `SessionCacheSupport`
  /
  `ZeroRTTSupport`

这些真相并不是没证据，
而是现在还主要散落在：

- `tests/openssl/test_openssl_features.pas`
- `tests/test_backend_custom_cipher_capability_truth_contract.pas`
- `tests/test_backend_callback_capability_truth_contract.pas`
- `tests/test_optional_backends_pkcs12_capability_truth_contract.pas`
- `tests/test_capability_cache.pas`
- `tests/test_freepascal_backend_basic.pas`

如果 shared capability regression
不把这些最常看的 published truth
一起收进来，
后面仍然容易出现：

- shared entrypoint 看起来没问题
- 实际 drift
  要等 focused contract
  或人工翻更多测试才发现

## Scope

- Add:
  - `docs/plans/2026-05-21-shared-capability-matrix-session-and-publication-hardening.md`
  - `tests/scripts/test_capability_matrix_v12_session_and_publication_contract.sh`
- Update:
  - `tests/test_capability_matrix_v12.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Runtime Truth To Lock

### Shared session probe/capability parity

- `IsFeatureSupported(sslFeatSessionCache)`
  必须与
  `SessionCacheSupport <> sslSupportNone`
  一致
- `IsFeatureSupported(sslFeatSessionTickets)`
  必须与
  `SessionTicketsSupport <> sslSupportNone`
  一致

### OpenSSL

- `SessionTicketsSupport = sslSupportStable`
- `SupportsPKCS12 = True`
- `SupportsCustomCipherSuites = True`
- `SupportsCallbacks = True`

### FreePascal

- `SessionCacheSupport = sslSupportExperimental`
- `ZeroRTTSupport = sslSupportExperimental`

## Notes

- 这批只把当前 Linux host
  上已经稳定成立、
  且已有 focused proof
  支撑的 truth
  提升到 shared regression。
- `OpenSSL`
  的
  `SessionCacheSupport`
  /
  `EarlyDataSupport`
  /
  `ZeroRTTSupport`
  /
  `SupportsPasswordProtectedKeys`
  仍然保持由更窄的 focused contracts
  守护，
  本批不额外扩大 shared entrypoint
  的环境敏感面。

## Verification

```bash
bash -n tests/scripts/test_capability_matrix_v12_session_and_publication_contract.sh
bash tests/scripts/test_capability_matrix_v12_session_and_publication_contract.sh
mkdir -p tmp/test_capability_matrix_v12 && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_capability_matrix_v12 -FEtmp/test_capability_matrix_v12 -otmp/test_capability_matrix_v12/test_capability_matrix_v12 tests/test_capability_matrix_v12.pas
./tmp/test_capability_matrix_v12/test_capability_matrix_v12
mkdir -p tmp/test_backend_custom_cipher_capability_truth_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_backend_custom_cipher_capability_truth_contract -FEtmp/test_backend_custom_cipher_capability_truth_contract -otmp/test_backend_custom_cipher_capability_truth_contract/test_backend_custom_cipher_capability_truth_contract tests/test_backend_custom_cipher_capability_truth_contract.pas
./tmp/test_backend_custom_cipher_capability_truth_contract/test_backend_custom_cipher_capability_truth_contract
mkdir -p tmp/test_backend_callback_capability_truth_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_backend_callback_capability_truth_contract -FEtmp/test_backend_callback_capability_truth_contract -otmp/test_backend_callback_capability_truth_contract/test_backend_callback_capability_truth_contract tests/test_backend_callback_capability_truth_contract.pas
./tmp/test_backend_callback_capability_truth_contract/test_backend_callback_capability_truth_contract
mkdir -p tmp/test_optional_backends_pkcs12_capability_truth_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_optional_backends_pkcs12_capability_truth_contract -FEtmp/test_optional_backends_pkcs12_capability_truth_contract -otmp/test_optional_backends_pkcs12_capability_truth_contract/test_optional_backends_pkcs12_capability_truth_contract tests/test_optional_backends_pkcs12_capability_truth_contract.pas
./tmp/test_optional_backends_pkcs12_capability_truth_contract/test_optional_backends_pkcs12_capability_truth_contract
git diff --check
```

## Expected Result

- shared capability regression
  会更早直接报警：
  - session probe/capability parity drift
  - `OpenSSL`
    session-ticket/publication drift
  - `FreePascal`
    session-cache / 0-RTT support-level drift
- 当前 Linux host
  上应继续显示：
  - `OpenSSL`
    executed
  - `FreePascal`
    executed
  - `WolfSSL / MbedTLS / WinSSL`
    按当前环境 skip
