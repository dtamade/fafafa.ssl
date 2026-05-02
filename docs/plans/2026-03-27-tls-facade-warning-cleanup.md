# TLS Facade Warning Cleanup Plan

**Goal:** 用一个最小、可复现的编译合同收口 `src/fafafa.ssl.tls.pas` 当前稳定出现的 3 条 warning，并在不改变公共行为的前提下完成修复。

**Architecture:** 这批只触碰 `fafafa.ssl.tls` 的编译面：

- 新增一个 `tests/scripts` 合同，直接用仓库主编译参数编译 `src/fafafa.ssl.tls.pas`
- 合同只盯当前已复现的 warning surface，不扩展到其它单元
- 生产改动限定在：
  - `TSSLStream.Seek`
  - `TSSLConnector.FromContext`
  - `TSSLAcceptor.FromContext`
- 保持现有 facade 语义不变：
  - `Seek` 仍然抛 `EStreamError`
  - connector/acceptor 默认 timeout / blocking / session 语义不变

## Task 1: RED - Add focused warning contract

**Files:**
- Add: `tests/scripts/test_tls_facade_warning_contract.sh`

**Steps:**
- 写一个 shell contract，直接运行：
  - `fpc -B -Fu./src -FUtmp/... -Mobjfpc -Scgi -O2 -g -gl -vewnhi src/fafafa.ssl.tls.pas`
- 断言编译成功
- 断言 `fafafa.ssl.tls.pas` 不应再出现任何 `Warning:` 行
- 先运行合同，确认当前树为 RED

**Expected:**
- 现状下合同失败，并暴露 `fafafa.ssl.tls.pas` 的 3 条 warning

## Task 2: GREEN - Minimal warning cleanup

**Files:**
- Modify: `src/fafafa.ssl.tls.pas`

**Steps:**
- 在 `TSSLStream.Seek` 中先给 `Result` 一个稳定值，再抛出 `EStreamError`
- 去掉 `TSSLConnector.FromContext` / `TSSLAcceptor.FromContext` 对 advanced record result 的 `FillChar`
- 改为显式字段初始化，避免 managed result warning
- 不触碰同文件其他已有未提交改动

**Expected:**
- `fafafa.ssl.tls.pas` 不再产生当前 3 条 warning
- facade 行为保持不变

## Task 3: Verification

**Run:**
- `bash tests/scripts/test_tls_facade_warning_contract.sh`
- `mkdir -p tmp/tls_connector_hostname_override_precedence_warning_regression && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls_connector_hostname_override_precedence_warning_regression -FEtmp/tls_connector_hostname_override_precedence_warning_regression -otmp/tls_connector_hostname_override_precedence_warning_regression/test_tls_connector_hostname_override_precedence tests/test_tls_connector_hostname_override_precedence.pas && ./tmp/tls_connector_hostname_override_precedence_warning_regression/test_tls_connector_hostname_override_precedence`
- `python3 scripts/compile_all_modules.py`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `git diff --check -- docs/plans/2026-03-27-tls-facade-warning-cleanup.md src/fafafa.ssl.tls.pas tests/scripts/test_tls_facade_warning_contract.sh task_plan.md findings.md progress.md`

**Expected:**
- 新合同通过
- 现有 connector precedence 回归继续通过
- baseline compile gate 与 minimal CI gate 继续绿
