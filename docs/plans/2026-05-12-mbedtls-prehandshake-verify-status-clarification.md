# MbedTLS Pre-Handshake Verify Status Clarification

## Goal
修复 `TMbedTLSConnection.GetVerifyResult` / `GetVerifyResultString` 在尚未完成握手时误报 verify success 的公共语义漂移。

## Architecture
- 这批只收口 MbedTLS 连接级 pre-handshake verify-status 语义，不扩到 helper-loss 之外的更多 MbedTLS 验证流程。
- 复用现有 `tests/test_mbedtls_framework.pas`：
  - 保留已存在的 helper-loss guard contract
  - 增加 fresh connection pre-handshake contract，要求未握手前不能返回 `0/OK`
- 生产修复限制在 `src/fafafa.ssl.mbedtls.connection.pas`：
  - pre-handshake => `GetVerifyResult = -1`
  - pre-handshake => `GetVerifyResultString = Not verified`
  - completed-handshake path 保持当前 flag-based truth

## Files
- Modify: `tests/test_mbedtls_framework.pas`
- Modify: `src/fafafa.ssl.mbedtls.connection.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps
1. 在 `test_mbedtls_framework` 增加 fresh pre-handshake contract：
   - create real MbedTLS client context + stream connection
   - assert `GetVerifyResult = -1`
   - assert `GetVerifyResultString` contains `not verified`
2. 在 `src/fafafa.ssl.mbedtls.connection.pas` 做最小 getter 修法：
   - pre-handshake short-circuit to `-1`
   - pre-handshake string short-circuit to `Not verified`
3. 跑 focused GREEN 与编译验证：
   - `fpc -B -Fu./src -Fu./tests -FUtmp/mbedtls_framework_units -FEtmp/mbedtls_framework_units -otmp/mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas`
   - `./tmp/mbedtls_framework_units/test_mbedtls_framework`
   - `python3 scripts/compile_all_modules.py`
   - `git diff --check`
