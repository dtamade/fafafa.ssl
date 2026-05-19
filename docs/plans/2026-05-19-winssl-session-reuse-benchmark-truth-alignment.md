# WinSSL Session-Reuse Benchmark Truth Alignment

## Goal

把 WinSSL 专项 session-reuse benchmark 程序与指南收口到当前 backend-specific runtime truth：

- `tests/winssl/test_winssl_session_reuse_benchmark.pas` 不再继续使用 direct core session mirrors
- benchmark 输出不再把 `70-90%` / “快速握手” 当成当前已证实结论
- benchmark 程序修掉当前 metrics 被覆盖与 success-count 为 0 时的分母风险
- `tests/winssl/SESSION_REUSE_BENCHMARK_GUIDE.md` 对齐当前 dedicated Windows CI truth：
  - `observed_reuse=false`
  - `session_configured=true`

## Scope

- `tests/winssl/test_winssl_session_reuse_benchmark.pas`
- `tests/winssl/SESSION_REUSE_BENCHMARK_GUIDE.md`
- `tests/scripts/test_winssl_session_reuse_benchmark_truth_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不修改 `src/fafafa.ssl.winssl.connection.pas` 共享 reconnect 实现
- 不重开 WinSSL native resumed-handshake / session tickets 的生产实现调查
- 不把 benchmark 结果重新包装成“native reuse 已被 runtime 证实”

## Why This Batch

前面的 active guides / capability docs 已经基本对齐当前 conservative truth，但 WinSSL 专项 benchmark 还留着一组高价值 residual：

- benchmark 程序仍直接使用：
  - `LConn.GetSession`
  - `LConn.SetSession`
  - `LConn.IsSessionReused`
- benchmark 指南仍继续承诺：
  - `70-90%` 性能提升
  - `>95%` Session 复用率
  - “快速握手”
- benchmark 程序本身还有一条真实逻辑 bug：
  - `RunSessionReuseBenchmark` 里先拿 `BenchmarkWithoutSessionReuse(...)`
  - 随后又整条覆盖成 `BenchmarkWithSessionReuse(...)`
  - 导致 comparison report 实际拿不到完整双侧 metrics

这批 therefore 更像 backend-specific runtime truth 和 benchmark harness 自身的收口，而不是普通文档 polish。

## Planned Changes

1. 新增 focused shell contract，锁住 benchmark 程序/指南不再回退到旧 session 语义与过强承诺。
2. 更新 `tests/winssl/test_winssl_session_reuse_benchmark.pas`：
   - 切到 `ISSLSessionResumption` owner path
   - 区分：
     - `session_configured`
     - `observed_reuse`
   - 修掉 metrics 覆盖与除零风险
3. 更新 `tests/winssl/SESSION_REUSE_BENCHMARK_GUIDE.md`：
   - 改成当前 conservative runtime truth
   - 不再把 benchmark 当 native resumed-handshake 已证实的证明

## Verification

```bash
bash -n tests/scripts/test_winssl_session_reuse_benchmark_truth_contract.sh
bash tests/scripts/test_winssl_session_reuse_benchmark_truth_contract.sh
mkdir -p tmp/winssl_session_reuse_benchmark_win64
fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/winssl_session_reuse_benchmark_win64 \
  -FEtmp/winssl_session_reuse_benchmark_win64 \
  -otmp/winssl_session_reuse_benchmark_win64/test_winssl_session_reuse_benchmark.exe \
  tests/winssl/test_winssl_session_reuse_benchmark.pas
git diff --check
```

## Expected Outcome

- WinSSL benchmark lane stops teaching stale direct-core session mirrors
- benchmark docs/program align with `observed_reuse=false / session_configured=true`
- comparison report becomes structurally correct again and no longer silently drops one side of the metrics
