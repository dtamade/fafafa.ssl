# Error Mapping Contract Enum And Registration Alignment

## Goal

修复
`tests/contract/test_error_mapping_contract.pas`
当前的两层问题：

1. 编译期：
   - 仍然引用已经不存在的
     `sslErrOK`
2. runtime coverage：
   - 只依赖
     `fafafa.ssl.base/factory/errors`
   - 没有把 backend registration
     明确带进来

这批要把它收口成：

- 能编译
- 能走当前 error-code truth
- 能真实执行当前可用 backend

## Why This Batch

这是一个比普通 coverage hole
更直接的 fresh RED：

- 当前 focused contract
  根本编不过
- 就算把旧符号名改对，
  如果 backend registration
  还没进来，
  也容易再次退回
  “全靠 skip”

用户目标里包含：

- 测试完整
- 接口设计/实现真相一致

所以这类
“contract 已经失去当前 API truth”
的点，
必须优先修。

## Scope

- Add:
  - `docs/plans/2026-05-21-error-mapping-contract-enum-and-registration-alignment.md`
  - `tests/scripts/test_error_mapping_contract_enum_and_registration_guard.sh`
- Update:
  - `tests/contract/test_error_mapping_contract.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Minimal Fix

1. 把
   `SSLErrorToString(sslErrOK)`
   改成当前真实枚举：
   - `SSLErrorToString(sslErrNone)`
2. 让测试走主门面注册路径：
   - `uses fafafa.ssl`
3. 新增静态 guard：
   - 不允许再出现过时的
     `sslErrOK`
   - 必须保持
     `SSLErrorToString(sslErrNone)`
   - 必须保持
     `uses fafafa.ssl`

## Verification

```bash
bash -n tests/scripts/test_error_mapping_contract_enum_and_registration_guard.sh
bash tests/scripts/test_error_mapping_contract_enum_and_registration_guard.sh
mkdir -p tmp/test_error_mapping_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_error_mapping_contract -FEtmp/test_error_mapping_contract -otmp/test_error_mapping_contract/test_error_mapping_contract tests/contract/test_error_mapping_contract.pas
./tmp/test_error_mapping_contract/test_error_mapping_contract
git diff --check
```

## Expected Result

- error-mapping contract
  不再因为
  `sslErrOK`
  编译失败
- 当前 Linux host
  上应至少真实执行：
  - `OpenSSL`
  - `FreePascal`
- `MbedTLS / WolfSSL / WinSSL`
  按当前 enable/platform 条件正常 skip
