# 2026-03-11 pure Pascal TLS1.2 external resumption matrix

## Goal
- 把 pure Pascal TLS1.2 resumption 从“只有 local OpenSSL oracle”推进到更明确的公网真相。
- 这波重点不是立即补完所有公网 resumed path，而是先把外网当前状态扫清：
  - 哪些 host 已经能 reused
  - 哪些 host 当前仍不 reused
  - 由此推导下一条实现主线

## Findings Snapshot
- 2026-03-11 当前外网矩阵结果：
  - `www.apache.org`：`reused=True`
  - `www.perl.org`：`reused=False`
  - `rsa2048.badssl.com`：`reused=False`
- 对照 `openssl s_client -tls1_2 -reconnect`，上述 host 都支持 TLS1.2 resumed handshake。
- 这说明当前 pure Pascal TLS1.2 session-id resumption 已经具备一部分公网互操作，但并不能覆盖所有 public-host 策略。

## Hypothesis
- 当前最可能的下一条根因不是 TLS1.2 abbreviated handshake 本身，而是：
  - 一部分公网主机更依赖/偏向 session ticket 路径
  - 纯 Pascal 当前只实现了 TLS1.2 session-id resumption，尚未实现 TLS1.2 ticket resumption

## Files
- `tests/integration/test_freepascal_tls12_resumption_runtime.pas`
- `tests/scripts/test_freepascal_tls12_resumption_runtime_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification
- `FAFAFA_RUN_NETWORK_TESTS=1 FAFAFA_TLS12_RESUMPTION_HOST=www.apache.org FAFAFA_TLS12_RESUMPTION_REQUIRE_REUSE=1 ./tmp/test_fp_tls12_resumption_runtime`
- `FAFAFA_RUN_NETWORK_TESTS=1 FAFAFA_TLS12_RESUMPTION_HOSTS='www.apache.org,www.perl.org,rsa2048.badssl.com' ./tmp/test_fp_tls12_resumption_runtime`
