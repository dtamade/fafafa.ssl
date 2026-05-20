# API Reference TSSLErrorCode Truth Alignment

## Goal

修复
`docs/reference/API_REFERENCE.md`
里
`TSSLErrorCode`
文档块
与源码当前真相的漂移，
让活跃 API 参考不再继续发布旧错误码名字。

这批不改 runtime，
只做：

- active API reference truth repair
- 一个静态 contract，
  防止错误码文档再次漂回旧名字
- 账本同步

## Why This Batch

刚才的
`tests/contract/test_error_mapping_contract.pas`
fresh RED
已经证明：

- 当前 no-error truth
  是
  `sslErrNone`
- 不是旧的
  `sslErrOK`

进一步静态扫描又确认：

- `docs/reference/API_REFERENCE.md`
  里的
  `TSSLErrorCode`
  代码块
  仍然停留在更早的旧枚举集合

这会直接误导：

- 新调用方
- 接口审查
- 后续 focused contracts

所以这条 docs drift
现在就应该收口。

## Scope

- Add:
  - `docs/plans/2026-05-21-api-reference-tsslerrorcode-truth-alignment.md`
  - `tests/scripts/test_api_reference_tsslerrorcode_truth_contract.sh`
- Update:
  - `docs/reference/API_REFERENCE.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Minimal Fix

1. 把
   `docs/reference/API_REFERENCE.md`
   中
   `TSSLErrorCode`
   代码块
   对齐到
   `src/fafafa.ssl.base.pas`
   当前真实枚举
2. 新增静态 guard：
   - 必须包含当前 shipped names
   - 禁止旧名字继续出现：
     - `sslErrInvalidParameter`
     - `sslErrOutOfMemory`
     - `sslErrConnectionClosed`
     - `sslErrHandshakeFailed`
     - `sslErrCertificateVerifyFailed`
     - `sslErrCipherNotSupported`
     - `sslErrProtocolNotSupported`

## Verification

```bash
bash -n tests/scripts/test_api_reference_tsslerrorcode_truth_contract.sh
bash tests/scripts/test_api_reference_tsslerrorcode_truth_contract.sh
bash tests/scripts/test_error_mapping_contract_enum_and_registration_guard.sh
git diff --check
```

## Expected Result

- `API_REFERENCE`
  不再发布过时的
  `TSSLErrorCode`
  名字
- 文档里的错误码枚举
  与当前源码/focused contract
  保持同一套真相源
