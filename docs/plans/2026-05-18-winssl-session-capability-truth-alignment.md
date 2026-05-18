# 2026-05-18 WinSSL Session Capability Truth Alignment

## Goal

把 WinSSL backend 对 `session resumption / session tickets` 的 public capability、KnownIssues 文案、以及活跃参考文档收紧到当前 GitHub Windows runtime truth，避免继续把“配置了 session”误写成“backend 已稳定支持真实 resumed handshake”。

## Scope

- 不在本批硬上新的 WinSSL native resumed-handshake 实现。
- 不重开已经关闭的 Windows runtime capture / shared session-info crash lane。
- 只收三类 truth drift：
  1. `src/fafafa.ssl.winssl.lib.pas` 的 capability/wording 仍把 session tickets 当 stable
  2. 活跃 WinSSL 参考文档仍把 session resumption 写成完整/稳定成功路径
  3. 仓库缺少 focused contract 守住新的 runtime truth

## Files

- `src/fafafa.ssl.winssl.lib.pas`
- `docs/reference/API_REFERENCE.md`
- `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
- `docs/reference/WINSSL_PERFORMANCE_TUNING.md`
- `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
- `tests/scripts/test_winssl_capability_source_contract.sh`
- `tests/scripts/test_isslsessionresumption_active_guidance_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- GitHub Windows live run `26037518301` 已证明当前 WinSSL bridge lane 不再 crash，broader suite 7/7 PASS。
- 同一 artifact 也明确记录：
  - `attempts=4`
  - `observed_reuse=false`
  - `require_reuse=false`
  - `session_configured=true`
- 因而当前最准确的 public truth 不是“WinSSL session tickets stable”，而是：
  - API surface 存在
  - shared path 已安全
  - native resumed-handshake 行为仍未被当前 runtime proof 证实

## Steps

1. 先做 RED：扩 focused contracts，直接抓出 WinSSL capability/docs 仍在发布过强承诺。
2. 最小更新 `src/fafafa.ssl.winssl.lib.pas`：
   - 收紧 `SessionTicketsSupport`
   - 收紧 `KnownIssues`
3. 更新活跃参考文档：
   - 明确当前 runtime truth
   - 移除/收紧“稳定复用成功”口径
   - 在 WinSSL 文档里也统一使用 `ISSLSessionResumption` 活跃指导路径
4. 跑 focused contracts、Win64 compile、`git diff --check`。

## Commands

```bash
bash -n tests/scripts/test_winssl_capability_source_contract.sh
bash tests/scripts/test_winssl_capability_source_contract.sh
bash -n tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh
bash tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh
bash -n tests/scripts/test_isslsessionresumption_active_guidance_contract.sh
bash tests/scripts/test_isslsessionresumption_active_guidance_contract.sh
mkdir -p tmp/winssl_session_capability_truth_win64
fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/winssl_session_capability_truth_win64 \
  -FEtmp/winssl_session_capability_truth_win64 \
  -otmp/winssl_session_capability_truth_win64/test_winssl_session_resumption.exe \
  tests/winssl/test_winssl_session_resumption.pas
git diff --check
```

## Execution Result

- PASS
- `SessionTicketsSupport` 已从 `sslSupportStable` 收紧到 `sslSupportExperimental`
- `KnownIssues` 已固定当前 dedicated Windows runtime truth：
  - `observed_reuse=false`
  - `session_configured=true`
- `API_REFERENCE.md` / `WINSSL_BACKEND_CAPABILITY_MATRIX.md` / `WINSSL_PERFORMANCE_TUNING.md` / `WINSSL_BACKEND_STATUS_REPORT.md`
  已统一去掉“WinSSL session resumption 已稳定闭环 / 已有通用 70-90% 收益”的过强承诺
- focused verification：
  - `bash -n tests/scripts/test_winssl_capability_source_contract.sh`
  - `bash tests/scripts/test_winssl_capability_source_contract.sh`
  - `bash -n tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh`
  - `bash tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh`
  - `bash -n tests/scripts/test_isslsessionresumption_active_guidance_contract.sh`
  - `bash tests/scripts/test_isslsessionresumption_active_guidance_contract.sh`
  - `mkdir -p tmp/winssl_session_capability_truth_win64 && fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/winssl_session_capability_truth_win64 -FEtmp/winssl_session_capability_truth_win64 -otmp/winssl_session_capability_truth_win64/test_winssl_session_resumption.exe tests/winssl/test_winssl_session_resumption.pas`
  - `git diff --check`
