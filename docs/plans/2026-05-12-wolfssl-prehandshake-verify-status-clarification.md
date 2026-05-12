# WolfSSL Pre-Handshake Verify Status Clarification

## Goal
修复 `TWolfSSLConnection.GetVerifyResult` / `GetVerifyResultString` 在尚未完成握手时误报 verify success 的公共语义漂移。

## Architecture
- 这批只收口 WolfSSL 连接级 pre-handshake verify-status 语义，不扩到证书验证流程、OCSP、或更多 WolfSSL 握手逻辑。
- 复用现有 `tests/test_wolfssl_framework.pas`：
  - 增加一个 fresh connection pre-handshake contract
  - 要求未握手前不能返回 `0/OK`
- 生产修复限制在 `src/fafafa.ssl.wolfssl.connection.pas`：
  - pre-handshake => `GetVerifyResult = -1`
  - pre-handshake => `GetVerifyResultString = Not verified`
  - completed-handshake success 保持当前 `0 / OK`
  - real native error 继续优先暴露

## Files
- Modify: `tests/test_wolfssl_framework.pas`
- Modify: `src/fafafa.ssl.wolfssl.connection.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps
1. 在 `test_wolfssl_framework` 增加 pre-handshake verify-status contract：
   - create real WolfSSL client context + stream connection
   - assert `GetVerifyResult = -1`
   - assert `GetVerifyResultString` contains `Not verified`
2. 在 `src/fafafa.ssl.wolfssl.connection.pas` 做最小 getter 修法：
   - native error present => preserve it
   - no native error and no handshake complete => `-1 / Not verified`
3. 跑 focused GREEN 与编译验证：
   - `fpc -B -Fu./src -Fu./tests -FUtmp/wolfssl_framework_units -FEtmp/wolfssl_framework_units -otmp/wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas`
   - `./tmp/wolfssl_framework_units/test_wolfssl_framework`
   - `python3 scripts/compile_all_modules.py`
   - `git diff --check`
