# OpenSSL Pre-Handshake Verify Status Clarification

## Goal
修复 `TOpenSSLConnection.GetVerifyResult` / `GetVerifyResultString` 在尚未完成握手时可能误报 verify success 的公共语义漂移。

## Architecture
- 这批只收口 OpenSSL 连接级 pre-handshake verify-status 语义，不扩到证书验证流程、helper-loss contract 之外的更多 OpenSSL 重构。
- 复用现有 `tests/test_openssl_connection_verify_result_contract.pas`：
  - 保留已存在的 helper-loss guard contract
  - 增加 fresh stream connection pre-handshake contract，要求未握手前不能返回 `0/OK`
- 生产修复限制在 `src/fafafa.ssl.openssl.connection.pas`：
  - pre-handshake => `GetVerifyResult = -1`
  - pre-handshake => `GetVerifyResultString = Not verified`
  - completed-handshake path 保持现有 helper-based truth

## Files
- Modify: `tests/test_openssl_connection_verify_result_contract.pas`
- Modify: `src/fafafa.ssl.openssl.connection.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps
1. 在 `test_openssl_connection_verify_result_contract` 增加 fresh pre-handshake contract：
   - create real OpenSSL client context and stream connection
   - assert `GetVerifyResult = -1`
   - assert `GetVerifyResultString` exposes `Not verified`
2. 在 `src/fafafa.ssl.openssl.connection.pas` 做最小 getter 修法：
   - pre-handshake short-circuit to `-1`
   - pre-handshake string short-circuit to `Not verified`
3. 跑 focused GREEN 与编译验证：
   - `fpc -B -Fu./src -Fu./tests -otmp/test_openssl_connection_verify_result_contract tests/test_openssl_connection_verify_result_contract.pas`
   - `./tmp/test_openssl_connection_verify_result_contract`
   - `python3 scripts/compile_all_modules.py`
   - `git diff --check`
