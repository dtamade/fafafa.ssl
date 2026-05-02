# TLS 1.3 ClientHello PSK Extension Validation Plan

**Goal:** 修复 pure Pascal TLS 1.3 `ClientHello` 解析/PSK binder transcript 中遗漏的协议约束：`pre_shared_key` 必须是最后一个 extension，且 `early_data` 只能和 `pre_shared_key` 一起出现。

**Architecture:** 当前 builder 会生成合法顺序，但 parser/binder-transcript helper 没有 fail-closed 校验。这会让非法 `ClientHello` 被解析为有效，并使 binder transcript 在 `pre_shared_key` 后存在 trailing extensions 时与真实握手字节脱节。最小修复是先补 focused RED，再把校验集中收敛到 `fafafa.ssl.tls13.clienthello.parser`。

**Files:**
- `src/fafafa.ssl.tls13.clienthello.parser.pas`
- `tests/test_tls13_resumption.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Task 1: Add Focused RED
- 在 `tests/test_tls13_resumption.pas` 增加两个 contract：
  - `pre_shared_key` 后追加任意扩展时，parser 与 binder transcript helper 都必须失败。
  - `early_data` 存在但 `pre_shared_key` 不存在时，parser 必须失败。

## Task 2: Minimal Production Fix
- 在 `src/fafafa.ssl.tls13.clienthello.parser.pas`：
  - 跟踪 `pre_shared_key` 是否出现、是否位于最后一个 extension。
  - 在 `TryParseTLS13ClientHelloFromHandshake(...)` 中对非法顺序和非法组合 fail-closed。
  - 在 `TryBuildTLS13ClientHelloPSKBinderTranscript(...)` 中对 `pre_shared_key` trailing extensions fail-closed。

## Task 3: Verify
- Focused:
  - `mkdir -p tmp/tls13_resumption_psk_validation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_resumption_psk_validation -FEtmp/tls13_resumption_psk_validation -otmp/tls13_resumption_psk_validation/test_tls13_resumption tests/test_tls13_resumption.pas && ./tmp/tls13_resumption_psk_validation/test_tls13_resumption`
- Adjacent:
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local`
