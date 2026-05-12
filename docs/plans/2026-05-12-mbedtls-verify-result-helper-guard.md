# MbedTLS Verify Result Helper Guard

## Goal
修复 `TMbedTLSConnection.GetVerifyResult` 在 `FSSLContext = nil` 或 `mbedtls_ssl_get_verify_result` helper 缺失时误报 `0/OK` 的 public contract drift。

## Architecture
- 这批只收口 MbedTLS 连接级 verify-result 查询语义，不扩到握手实现、证书验证流程、或 capability 框架。
- `GetVerifyResult` 的真相边界应与 OpenSSL 的 guard 类似：
  - helper 不可用时不能伪装成“验证通过”
  - 需要稳定降级为 non-success 值
- `GetVerifyResultString` 也应避免在 helper 缺失时继续给出空白或 OK 假象。
- 最小修法优先：
  - focused framework RED 证明 helper-loss 仍返回 `0`
  - 连接层改为 `-1` 降级
  - 为 string getter 提供稳定 unavailable 诊断

## Files
- Modify: `tests/test_mbedtls_framework.pas`
- Modify: `src/fafafa.ssl.mbedtls.connection.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps
1. 在 `test_mbedtls_framework` 增加 helper-loss verify-result contract：
   - create real MbedTLS context/connection
   - temporarily clear `mbedtls_ssl_get_verify_result`
   - assert `GetVerifyResult` must not return `0`
   - assert `GetVerifyResultString` exposes unavailable-style diagnostic
2. 在 `src/fafafa.ssl.mbedtls.connection.pas` 做最小 guard：
   - `DoGetVerifyResult` on nil/missing helper => `-1`
   - `DoGetVerifyResultString` on nil/missing helper => stable unavailable message
3. 跑 focused GREEN 与编译验证：
   - `fpc -B -Fu./src -Fu./tests -FUtmp/mbedtls_framework_units -FEtmp/mbedtls_framework_units -otmp/mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas`
   - `./tmp/mbedtls_framework_units/test_mbedtls_framework`
   - `python3 scripts/compile_all_modules.py`
   - `git diff --check`
