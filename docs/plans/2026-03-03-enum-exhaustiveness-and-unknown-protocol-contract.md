# Enum Exhaustiveness and Unknown Protocol Contract Hardening (2026-03-03)

## Goal
收敛 `case` 枚举处理的隐式分支，补齐 `sslProtocolUnknown` 语义合同，降低新增枚举时的静默行为漂移风险。

## Architecture
- 范围限定在 OpenSSL backend 协议判定与 builder option 联动逻辑。
- 不改变既有默认业务语义，只做分支显式化 + 合同断言补强。

## Files
- Modify: `src/fafafa.ssl.context.builder.pas`
- Modify: `src/fafafa.ssl.openssl.backed.pas`
- Modify: `tests/openssl/test_openssl_features.pas`

## Steps
1. 在 builder 的 `WithOption/WithoutOption` 中为非 OCSP 选项补 `else` 分支，避免 warning-prone 非穷举 `case`。
2. 在 OpenSSL `IsProtocolSupported` 中显式处理 `sslProtocolUnknown`，并增加 `else` 默认返回 `False`。
3. 在 OpenSSL feature 合同测试中增加 `sslProtocolUnknown` 必须返回 `False` 的断言。
4. 跑 focused regressions：
   - `fpc -Fu./src -Fu./src/openssl -Fi./src tests/openssl/test_openssl_features.pas -otmp/test_openssl_features && ./tmp/test_openssl_features`
   - `fpc -Fu./src -Fi./src tests/test_transformation_methods.pas -otmp/test_transformation_methods && ./tmp/test_transformation_methods`
5. 跑主干编译门禁：
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- OpenSSL features 合同测试通过，新增 unknown protocol 断言生效。
- transformation 方法测试通过。
- `compile_all_modules` 保持 179/179 通过。
