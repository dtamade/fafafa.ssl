# FreePascal Verify Result Status Clarification

## Goal
修复 `TFreePascalConnection.GetVerifyResult` / `GetVerifyResultString` 的状态语义漂移：
- 新建但未握手的连接不能伪装成 verify success
- 已成功完成可信握手的连接不能继续返回 `Not verified`

## Architecture
- 这批只收口 FreePascal 连接级 verify-status getter 语义，不扩到证书校验流程、链验证规则、或更多 backend 对齐工作。
- 用现有 `tests/test_freepascal_client_chain_trust_runtime.pas` 的 scripted trust handshake 夹具补两条 focused contract：
  - fresh pre-handshake connection => non-success verify result + `Not verified`
  - trusted CA-backed successful handshake => `GetVerifyResult = 0` + `GetVerifyResultString = OK`
- 生产修复限制在 `src/fafafa.ssl.freepascal.connection.pas`：
  - 让 verify-result getter 区分 “尚未验证” 与 “已验证且成功”
  - 保持已有 failure path 继续返回实际 `FLastErrorCode` / `FLastErrorString`

## Files
- Modify: `tests/test_freepascal_client_chain_trust_runtime.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps
1. 在 `test_freepascal_client_chain_trust_runtime` 增加 focused RED：
   - fresh connection before handshake must not report verify success
   - trusted successful handshake must expose `OK` verify string
2. 在 `src/fafafa.ssl.freepascal.connection.pas` 做最小 getter 修法：
   - pre-handshake and no error => `GetVerifyResult = -1`
   - handshake complete and no error => `GetVerifyResultString = 'OK'`
3. 跑 focused GREEN 与编译验证：
   - `fpc -B -Fu./src -Fu./tests -otmp/test_freepascal_client_chain_trust_runtime tests/test_freepascal_client_chain_trust_runtime.pas`
   - `./tmp/test_freepascal_client_chain_trust_runtime`
   - `python3 scripts/compile_all_modules.py`
   - `git diff --check`
