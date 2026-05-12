# WinSSL Pre-Handshake Verify Status Clarification

## Goal
修复 `TWinSSLConnection.GetVerifyResult` / `GetVerifyResultString` 在尚未完成握手时暴露误导性 verify-status 诊断的公共语义漂移。

## Architecture
- 这批只收口 WinSSL 连接级 pre-handshake verify-status 语义，不扩到更大的 Schannel 握手重构、Windows runtime proof、或证书校验策略设计。
- 当前 WinSSL getter 的整数结果在 fresh connection 上通常已经是 `-1`，但 string getter 会把这个状态表述成 `Certificate not available`：
  - 这会把 “尚未验证” 混同成 “证书缺失/不可用”
  - 与 OpenSSL / WolfSSL / MbedTLS / FreePascal 刚刚收口后的 `Not verified` 边界不一致
- 最小修法应基于 WinSSL 现有握手状态机：
  - `sslHsNotStarted` / `sslHsInProgress` => `GetVerifyResult = -1`
  - `sslHsNotStarted` / `sslHsInProgress` => `GetVerifyResultString = Not verified`
  - `sslHsFailed` 与 `sslHsCompleted` 继续复用现有 role-resolved validation truth，不掩盖真实验证失败
- Linux 主机上这批沿用 WinSSL 既有 workflow：
  - 先用 focused source contract 锁住 getter 的 pre-handshake guard
  - 再用 Win64 交叉编译 + repo compile gate 证明没有误伤 WinSSL surface

## Files
- Add: `tests/scripts/test_winssl_prehandshake_verify_status_contract.sh`
- Add: `docs/plans/2026-05-12-winssl-prehandshake-verify-status-clarification.md`
- Modify: `src/fafafa.ssl.winssl.connection.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps
1. 写 focused RED source contract：
   - fresh WinSSL pre-handshake getter 必须显式 short-circuit
   - `DoGetVerifyResult` 要在 `sslHsNotStarted` / `sslHsInProgress` 返回 `-1`
   - `DoGetVerifyResultString` 要在同样状态返回 `Not verified`
2. 在 `src/fafafa.ssl.winssl.connection.pas` 做最小修法：
   - pre-handshake guard 只覆盖 `not started` / `in progress`
   - 保留 `sslHsFailed` / `sslHsCompleted` 的现有 role-resolved validation path
3. 跑 focused GREEN 与 compile proof：
   - `bash tests/scripts/test_winssl_prehandshake_verify_status_contract.sh`
   - `fpc -Twin64 -B -Fu./src -Fu./tests -FUtmp/winssl_preverify_host_win64 -FEtmp/winssl_preverify_host_win64 -otmp/winssl_preverify_host_win64/test_winssl_hostname_mismatch_online.exe tests/winssl/test_winssl_hostname_mismatch_online.pas`
   - `fpc -Twin64 -B -Fu./src -Fu./tests -FUtmp/winssl_preverify_revocation_win64 -FEtmp/winssl_preverify_revocation_win64 -otmp/winssl_preverify_revocation_win64/test_winssl_revocation_online.exe tests/winssl/test_winssl_revocation_online.pas`
   - `python3 scripts/compile_all_modules.py`
   - `git diff --check`
4. 更新 working-memory、review、commit。
