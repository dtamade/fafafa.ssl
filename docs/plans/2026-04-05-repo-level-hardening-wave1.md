# 2026-04-05 Repo-level Hardening Wave 1

## Goal
- 以当前 dirty worktree 为前提，快速推进一批 repo-level 高优先级收口项，不做无边界重构。
- 本轮优先解决会影响“默认入口可信度”或“核心运行时契约可信度”的问题。
- 批次顺序按 ROI 排：
  1. `scripts/compile_all_modules.py` fail-open compile gate
  2. `scripts/run_minimal_ci_gate.sh` 的 `eval` 执行方式
  3. `src/fafafa.ssl.factory.pas` 的 publish-before-initialize / 并发初始化语义
  4. `src/fafafa.ssl.openssl.loader.pas` 与 AES/SHA/Modes 模块的 ready contract

## Architecture
- 当前默认入口：
  - `python3 scripts/compile_all_modules.py`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`
- 当前产品主线：
  - `docs/ROADMAP.md`
  - pure Pascal backend completeness 主线继续保留，但 repo-level gate/runtime hardening 仍是本地开发可信度前置条件
- 本轮实现边界：
  - 每个行为变更都先补 focused contract，再做最小实现修复
  - 不回滚当前 worktree 中用户已有改动

## Files
- `docs/plans/2026-04-05-repo-level-hardening-wave1.md`
- `task_plan.md`
- `findings.md`
- `progress.md`
- `scripts/compile_all_modules.py`
- `scripts/run_minimal_ci_gate.sh`
- `src/fafafa.ssl.factory.pas`
- `src/fafafa.ssl.openssl.loader.pas`
- `src/fafafa.ssl.openssl.api.aes.pas`
- `src/fafafa.ssl.openssl.api.sha.pas`
- `src/fafafa.ssl.openssl.api.modes.pas`
- `src/fafafa.ssl.openssl.api.blake2.pas`
- `src/fafafa.ssl.openssl.api.ssl.pas`
- `tests/test_openssl_ssl_load_contract.pas`
- `tests/scripts/`
- `tests/`

## Steps
1. 恢复上下文并确认 fresh baseline
   - 读取 `docs/ROADMAP.md`、`task_plan.md`、`findings.md`、`progress.md`
   - 检查 `git status --short`
   - 重新运行默认 compile gate 与 minimal gate
2. 固化本轮 backlog 证据
   - 抽取 factory / compile gate / minimal gate / OpenSSL loader 的当前源码证据
   - 确认哪些问题仍没有 focused contract
3. 批次 A: gate hardening
   - 先写 `compile_all_modules.py` fail-closed focused contract
   - 再写 `run_minimal_ci_gate.sh` 去 `eval` focused contract
   - 观察 RED 后做最小实现修复
4. 批次 B: runtime contract hardening
   - 为 `TSSLFactory.GetLibrary(...)` / `IsLibraryAvailable(...)` 增加并发/发布时序 contract
   - 为 OpenSSL loader required-symbol readiness 增加 focused contract
   - 观察 RED 后做最小实现修复
5. 批次 C: OpenSSL SSL unload contract hardening
   - 为 `UnloadOpenSSLSSL` 增加 focused contract，锁定“module unloaded 时 helper surface 也必须清空”
   - 观察 RED，确认失败点来自 stale function pointers 而不是测试噪音
   - 在 `src/fafafa.ssl.openssl.api.ssl.pas` 里做最小实现修复
6. 批次 D: OpenSSL SSL load-side contract hardening
   - 为 `LoadOpenSSLSSL` 增加 focused contract，锁定“libssl 已导出的 info/state helpers 必须被绑定”
   - 观察 RED，确认失败点来自实际漏绑的 helper 而不是 host-specific macro drift
   - 在 `src/fafafa.ssl.openssl.api.ssl.pas` 里做最小实现修复
   - 继续扩展同一 focused contract，锁定宿主已导出的 session-ticket / PSK helpers 也必须被绑定
7. Fresh verification
   - 先跑本批 focused tests
   - 再跑 `python3 scripts/compile_all_modules.py`
   - 再跑 `bash scripts/run_minimal_ci_gate.sh --fast-local`
8. 回填 working memory
   - 更新 `task_plan.md` / `findings.md` / `progress.md`

## Commands
- `git status --short`
- `python3 scripts/compile_all_modules.py`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `rg -n "GetLibrary\\(|IsLibraryAvailable\\(|Initialize\\(|FLibraries\\[" src/fafafa.ssl.factory.pas`
- `rg -n "success_rate|target_rate|98.0" scripts/compile_all_modules.py`
- `rg -n "eval|run_cmd\\(|MODULE_SET" scripts/run_minimal_ci_gate.sh`
- `rg -n "Required|LoadFunctions\\(|SetModuleLoaded\\(" src/fafafa.ssl.openssl.loader.pas src/fafafa.ssl.openssl.api.aes.pas src/fafafa.ssl.openssl.api.sha.pas src/fafafa.ssl.openssl.api.modes.pas`
- `python3 - <<'PY' ... ctypes probe of libssl helper surface ... PY`
- `mkdir -p tmp/openssl_ssl_load_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_ssl_load_contract -FEtmp/openssl_ssl_load_contract -otmp/openssl_ssl_load_contract/test_openssl_ssl_load_contract tests/test_openssl_ssl_load_contract.pas && ./tmp/openssl_ssl_load_contract/test_openssl_ssl_load_contract`
- `mkdir -p tmp/openssl_ssl_unload_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_ssl_unload_contract -FEtmp/openssl_ssl_unload_contract -otmp/openssl_ssl_unload_contract/test_openssl_ssl_unload_contract tests/test_openssl_ssl_unload_contract.pas && ./tmp/openssl_ssl_unload_contract/test_openssl_ssl_unload_contract`
- `mkdir -p tmp/openssl_features && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_features -FEtmp/openssl_features -otmp/openssl_features/test_openssl_features tests/openssl/test_openssl_features.pas && ./tmp/openssl_features/test_openssl_features`

## Expected Outcome
- 默认 compile gate 不再以 `98%` 成功率 fail-open 通过
- minimal gate 不再通过 `eval` 执行拼接命令
- factory 不再在初始化完成前缓存/发布共享库实例
- OpenSSL AES/SHA/Modes 模块不会在关键符号未就绪时被标记为 loaded
- `UnloadOpenSSLSSL` 不再留下 stale SSL helper pointers；`osmSSL` 的 unload state 与 helper surface 一致
- `LoadOpenSSLSSL` 不再漏绑宿主机已导出的 info/state + session-ticket / PSK helpers；OpenSSL 上层的 info callback / state string / feature-probe 路径不再被误判为 unsupported
