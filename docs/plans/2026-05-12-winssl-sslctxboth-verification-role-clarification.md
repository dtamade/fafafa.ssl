# WinSSL sslCtxBoth Verification Role Clarification

## Goal
修复 `sslCtxBoth` 在 `WinSSL` 显式 `Connect` / `Accept` 路径上的证书校验角色漂移：握手入口已经显式选定 client/server，但 `ValidatePeerCertificate(...)` 仍按 `ContextType` 猜角色，导致 dual-context 连接可能跳过客户端 hostname / peer-cert 校验，或在服务端用错 `AUTHTYPE_*`。

## Architecture
- 这批只修 WinSSL 的“证书校验角色来源”，不重开更大的 dual-role handshake 状态机设计。
- 角色真相源应来自显式握手入口：
  - `Connect` => client validation role
  - `Accept` => server validation role
- `ValidatePeerCertificate(...)` 不应再直接根据 `FContext.GetContextType` 推导：
  - 是否必须有 peer certificate
  - 是否执行 hostname verification
  - `CERT_CHAIN_POLICY_SSL` 的 `AUTHTYPE_SERVER` / `AUTHTYPE_CLIENT`
- `DoGetVerifyResult` / `DoGetVerifyResultString` 也必须复用同一角色真相，而不是重新按 context type 猜。

## Files
- Add: `tests/scripts/test_winssl_sslctxboth_verification_role_contract.sh`
- Modify: `src/fafafa.ssl.winssl.connection.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps
1. 写 focused RED source contract：
   - dual-role WinSSL verification 不应再只有 `ValidatePeerCertificate(out ...)`
   - `DoConnect` / `DoAccept` 必须把显式 client/server 角色传入验证路径
   - verify-result getter 也必须复用显式角色，而不是回落到 `ContextType`
2. 做最小修法：
   - 给 WinSSL verification path 增加显式角色参数
   - 为 dual-context 显式握手记住最近一次 verification role，供 verify-result getter 复用
   - 不扩到新的公共接口或跨后端重构
3. 跑 focused GREEN 与 compile proof：
   - `bash tests/scripts/test_winssl_sslctxboth_verification_role_contract.sh`
   - 选定 Win64 WinSSL 测试交叉编译
   - `python3 scripts/compile_all_modules.py`
4. 更新 working-memory、review、commit。

## Current Evidence
- focused RED:
  - `bash tests/scripts/test_winssl_sslctxboth_verification_role_contract.sh`
  - result before fix: FAIL on missing explicit peer-validation role state, missing role-parameterized `ValidatePeerCertificate(...)`, and missing explicit role wiring in `DoConnect` / `DoAccept` / `DoGetVerifyResult`
- minimal implementation:
  - `src/fafafa.ssl.winssl.connection.pas`
    - added connection-local peer-validation role state
    - parameterized `ValidatePeerCertificate(...)` with explicit role
    - wired explicit `Connect` / `Accept` / verify-result getter to the same role truth
    - fixed a Pascal semicolon slip caught by Win64 cross-compile during landing
- focused GREEN:
  - `bash tests/scripts/test_winssl_sslctxboth_verification_role_contract.sh`: PASS
  - `fpc -Twin64 -B -Fu./src -Fu./tests -FUtmp/winssl_role_client_win64 -FEtmp/winssl_role_client_win64 -otmp/winssl_role_client_win64/test_winssl_hostname_mismatch_online.exe tests/winssl/test_winssl_hostname_mismatch_online.pas`: PASS
  - `fpc -Twin64 -B -Fu./src -Fu./tests -FUtmp/winssl_role_server_win64 -FEtmp/winssl_role_server_win64 -otmp/winssl_role_server_win64/test_winssl_mtls_e2e_local.exe tests/winssl/test_winssl_mtls_e2e_local.pas`: PASS
  - `python3 scripts/compile_all_modules.py`: PASS, `185/185`
  - `git diff --check`: PASS
