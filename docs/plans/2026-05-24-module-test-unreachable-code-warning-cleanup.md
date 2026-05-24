# Module Test Unreachable-Code Warning Cleanup

## Goal

清掉 `run_all_module_tests.sh --fast-local` 暴露的模块测试
`Warning: Unreachable code` 批次，同时保持测试断言语义不变。

## Architecture Rationale

这些 warning 来自测试直接比较编译期常量：

```pascal
if (SOME_CONST <> ExpectedValue) then
  FailTest(...);
```

FreePascal 会在编译期折叠这些恒真/恒假的分支，于是把失败路径报成
`Unreachable code`。测试的价值仍然存在：它们应该继续验证 Pascal binding
里的常量值没有漂移。本批不删除断言、不改 OpenSSL API 常量，只把比较包装成
运行时整数读取，让失败路径保持可编译、可执行。

## Files

- `tests/certificate/test_p2_pkcs12.pas`
- `tests/certificate/test_p2_ocsp.pas`
- `tests/certificate/test_p2_ts.pas`
- `tests/certificate/test_p2_cms.pas`
- `tests/certificate/test_p2_ct.pas`
- `tests/certificate/test_p2_pkcs7.pas`
- `tests/crypto/test_p2_store.pas`
- `tests/crypto/test_p2_comp.pas`
- `tests/crypto/test_p4_engine.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Implementation

- Add a tiny local `RuntimeInteger(AValue: Integer): Integer` helper in each
  affected test file.
- Wrap only the compile-time constant operands that caused unreachable-code
  warnings.
- Keep existing expected values, failure messages, and pass/fail flow unchanged.

## Verification

```bash
mkdir -p tmp/unreachable_code_wave1_all_units tmp/unreachable_code_wave1_all_bin
for test_file in \
  tests/certificate/test_p2_pkcs12.pas \
  tests/certificate/test_p2_ocsp.pas \
  tests/certificate/test_p2_ts.pas \
  tests/certificate/test_p2_cms.pas \
  tests/certificate/test_p2_ct.pas \
  tests/certificate/test_p2_pkcs7.pas \
  tests/crypto/test_p2_store.pas \
  tests/crypto/test_p2_comp.pas \
  tests/crypto/test_p4_engine.pas
do
  name=$(basename "$test_file" .pas)
  /opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc \
    -Mobjfpc -Sh -O2 \
    -Fu./src -Fu./src/openssl -Fu./src/winssl -Fu./tests \
    -FUtmp/unreachable_code_wave1_all_units \
    -FEtmp/unreachable_code_wave1_all_bin \
    "$test_file" >"tmp/unreachable_code_wave1_${name}_compile.log" 2>&1
done

rg -n "Warning: Unreachable code" tmp/unreachable_code_wave1_*_compile.log || true

FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc \
FAFAFA_FAST_LOCAL=1 \
FAFAFA_FPC_UNIT_OUTPUT_DIR=tmp/unreachable_code_wave1_module_units \
bash scripts/run_all_module_tests.sh --fast-local \
  2>&1 | tee tmp/unreachable_code_wave1_run_all_module_tests.log

rg -n "Warning: Unreachable code" \
  tmp/unreachable_code_wave1_run_all_module_tests.log \
  tmp/test-reports/*20260524_235928_1700710*_compile.log || true

rg -n "Warning:" tmp/test-reports/*20260524_235928_1700710*_compile.log || true
git diff --check
```

## Execution Result

- Focused compile for all 9 target files passed.
- Focused compile-log grep found no `Warning: Unreachable code`.
- Full module test gate passed:
  - run id: `20260524_235928_1700710`
  - result: `22` passed, `0` failed, `0` skipped
- Broad module-test compile-log grep found no `Warning:` entries.

## Next

Continue from fresh compile evidence. Do not broaden this batch into unrelated
TLS 1.3 case/range/string-conversion warnings; those should be separate named
rounds if selected.
