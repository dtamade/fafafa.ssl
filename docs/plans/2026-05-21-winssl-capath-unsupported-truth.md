# 2026-05-21 WinSSL CAPath Unsupported Truth Alignment

## Goal

把 `WinSSL + CAPath` 这条 backend-specific unsupported surface 的实现真相、active docs、以及 capability guidance 收成一条明确、可复用的当前事实：

- WinSSL runtime 当前会消费 `CAPath`
- 但一旦是非空路径，就会 fail-fast 抛出 unsupported
- Windows / Schannel 的推荐 trust path 仍是：
  - system roots
  - 显式 `CAFile`
  - 或 backend-specific certificate-store surface

避免上一批 `CAFile` / `CAPath` trust-loading parity 修完后，active docs 继续把 `CAPath` 误教成 WinSSL 可移植方案。

## Architecture

- runtime truth
  - `src/fafafa.ssl.winssl.context.pas`
- active docs truth
  - `docs/CA_CERTIFICATE_AUTO_LOADING.md`
  - `docs/guides/TROUBLESHOOTING.md`
  - `docs/guides/WINSSL_BEST_PRACTICES.md`
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
  - `docs/zh/FAQ.md`
- focused verification
  - `tests/scripts/test_winssl_capath_unsupported_active_docs_truth_contract.sh`

## TDD

### RED

1. 新增 focused source/docs contract，锁住：
   - WinSSL runtime 仍明确把 non-empty `LoadCAPath(...)` 定义为 unsupported
   - `CA_CERTIFICATE_AUTO_LOADING.md` 不再把 `.WithCAPath` 说成 cross-backend portable compose
   - `TROUBLESHOOTING.md` / `WINSSL_BEST_PRACTICES.md` 不再把 `LoadCAPath(...)` 当成 WinSSL/Windows 修法
   - `API_REFERENCE.md` 明确记录：
     - 字段会被消费
     - 不等于所有 backend 共享同一条 runtime 语义
     - WinSSL 对非空 `CAPath` 会 fail-fast
   - `WINSSL_BACKEND_CAPABILITY_MATRIX.md` 与中文 FAQ 同步这条 truth
2. 运行 contract，观察当前 docs truth 失败。

### GREEN

- 只修 active docs / capability truth，不改 runtime 代码
- 删除 WinSSL 专页里的 Linux `LoadCAPath(...)` 教程
- 在 API / troubleshooting / FAQ 中补 backend caveat

### REGRESSION

- `bash -n tests/scripts/test_winssl_capath_unsupported_active_docs_truth_contract.sh`
- `bash tests/scripts/test_winssl_capath_unsupported_active_docs_truth_contract.sh`
- `git diff --check`

## Files

- Add: `docs/plans/2026-05-21-winssl-capath-unsupported-truth.md`
- Add: `tests/scripts/test_winssl_capath_unsupported_active_docs_truth_contract.sh`
- Update: `docs/CA_CERTIFICATE_AUTO_LOADING.md`
- Update: `docs/guides/TROUBLESHOOTING.md`
- Update: `docs/guides/WINSSL_BEST_PRACTICES.md`
- Update: `docs/reference/API_REFERENCE.md`
- Update: `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
- Update: `docs/zh/FAQ.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Commands

1. `bash -n tests/scripts/test_winssl_capath_unsupported_active_docs_truth_contract.sh`
2. `bash tests/scripts/test_winssl_capath_unsupported_active_docs_truth_contract.sh`
3. `git diff --check`

## Expected Outcome

- WinSSL `CAPath` 的 public truth 不再被“字段存在”误读成“Windows 可直接用 CA 目录”
- active docs 会统一表达：
  - Linux/OpenSSL-family 可以用 `CAPath`
  - WinSSL 对 non-empty `CAPath` fail-fast unsupported
  - Windows trust roots 应优先走 `UseSystemRoots` / `.WithSystemRoots`
