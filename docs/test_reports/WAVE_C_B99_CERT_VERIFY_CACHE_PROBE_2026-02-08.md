# Wave C B99 Cert Verify Cache Probe（2026-02-08）

- probe_id: 20260208_043015
- compile_log: test-reports/wave_c_b99_cert_cache_compile_20260208_043015.log
- run_log: test-reports/wave_c_b99_cert_cache_run_20260208_043015.log

## 执行命令

fpc -Mobjfpc -Sh -O2 -Fu./src -Fu./src/openssl -Fu./tests/benchmarks -Fu./examples -Fi./src -FE./tests/benchmarks/bin tests/benchmarks/benchmark_cert_verify_cache.pas

./tests/benchmarks/bin/benchmark_cert_verify_cache

## 结果摘要

- compile: exit=0（1 warning）
- run: exit=0
- 关键输出（来自 run log）：
  - cache hit rate: 99.9%
  - speedup factor: 5.9x
  - time saved: 83.1%

## 结论

- 证书验证缓存基准在当前环境可稳定运行，具备进入 Wave C 候选优化主线的证据基础。
