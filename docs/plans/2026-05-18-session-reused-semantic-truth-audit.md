# 2026-05-18 Session Reused Semantic Truth Audit

## Goal

聚焦验证 `IsSessionReused` / `GetConnectionInfo.IsResumed` 的语义是否仍然表示“当前握手实际命中了恢复路径”，并修复 WinSSL / MbedTLS 中把“配置了 session”误报成“已经复用”的实现偏差。

## Scope

- 只收 `session reused` 真值语义，不重跑已闭环的 Windows runtime evidence capture 工作流。
- 优先做轻量、可重复的 focused contract：
  - WinSSL：源码合同
  - MbedTLS：源码合同 + 本地可跑的小型运行合同
- 不在本批假装补齐 WinSSL 真正的 Schannel resumption runtime 实现；本批只修“不能提前误报 reused=true”。

## Files

- `src/fafafa.ssl.winssl.connection.pas`
- `src/fafafa.ssl.mbedtls.connection.pas`
- `tests/test_mbedtls_connection_session_reused_contract.pas`
- `tests/scripts/test_session_reused_semantic_truth_contract.sh`
- `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `SetSession(...)` 的职责是“配置下一次握手可尝试恢复的 session”，不是直接宣布当前连接已经 resumed。
- `IsSessionReused` / `GetConnectionInfo.IsResumed` 的职责是“报告当前握手的实际结果”。
- 因此：
  - `OpenSSL` / `WolfSSL` 应继续读 native `*_session_reused` 真相
  - `FreePascal` 应继续在 `DoSetSession` 清空 reuse 状态，并只在真实恢复路径命中后翻成 `True`
  - `WinSSL` / `MbedTLS` 至少不能在 `DoSetSession` 里预置 `FSessionReused := True`

## Steps

1. 新增 focused contracts，让当前错误语义先红：
   - 源码合同禁止 WinSSL / MbedTLS 在 `DoSetSession` 中直接 `FSessionReused := True`
   - MbedTLS 运行合同模拟 `mbedtls_ssl_set_session(...) = 0` 成功返回，验证握手前 `IsSessionReused` 仍必须为 `False`
2. 最小修复生产代码：
   - WinSSL `DoSetSession` 仅记录 configured/current session，不再预置 `FSessionReused`
   - MbedTLS `DoSetSession` 仅尝试配置 native session，不再把成功配置等同于 resumed handshake
3. 聚焦验证：
   - `bash -n` + `bash` 新源码合同
   - focused Pascal compile/run：`tests/test_mbedtls_connection_session_reused_contract.pas`
   - `git diff --check`
4. 同步记录与报告：
   - 更新 WinSSL backend status report
   - 更新 `task_plan.md` / `findings.md` / `progress.md`

## Expected Outputs

- 新源码合同从 RED 变 GREEN
- 新 MbedTLS 运行合同从 RED 变 GREEN
- WinSSL / MbedTLS 不再在握手前误报 `IsSessionReused=True`
- 记录文件明确写出：
  - Windows runtime evidence capture 已闭环
  - WinSSL 真正的 resumption runtime 仍属后续高风险 lane
  - 当前批次只修复 reused semantic false positive

## Commands

```bash
bash -n tests/scripts/test_session_reused_semantic_truth_contract.sh
bash tests/scripts/test_session_reused_semantic_truth_contract.sh
mkdir -p tmp/test_mbedtls_connection_session_reused_contract
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_mbedtls_connection_session_reused_contract \
  -FEtmp/test_mbedtls_connection_session_reused_contract \
  -otmp/test_mbedtls_connection_session_reused_contract/test_mbedtls_connection_session_reused_contract \
  tests/test_mbedtls_connection_session_reused_contract.pas
./tmp/test_mbedtls_connection_session_reused_contract/test_mbedtls_connection_session_reused_contract
git diff --check
```

## Execution Result

- RED:
  - 新源码合同先失败，直接指出 WinSSL `DoSetSession` 仍在握手前预置 `FSessionReused := True`
  - 新 MbedTLS 运行合同先失败，直接证明 `SetSession(...)` 成功后 `IsSessionReused` 被提前翻成 `True`
- GREEN:
  - WinSSL / MbedTLS 的 `DoSetSession` 都已改成先清空 reuse 状态，不再把“configured session”误写成“resumed handshake”
  - focused 源码合同与 MbedTLS 运行合同都已通过
