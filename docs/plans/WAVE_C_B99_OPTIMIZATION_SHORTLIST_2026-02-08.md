# Wave C B99 Optimization Shortlist（2026-02-08）

## 输入证据

- baseline readiness：`docs/test_reports/WAVE_C_BASELINE_READINESS_2026-02-08.md`
- baseline draft：`docs/test_reports/PHASE2_BASELINE_DRAFT_20260208_042713.md`
- cert verify cache probe：`docs/test_reports/WAVE_C_B99_CERT_VERIFY_CACHE_PROBE_2026-02-08.md`

---

## 候选清单（按优先级）

| Priority | Candidate | 预期收益 | 实施风险 | 推荐度 |
|----------|-----------|----------|----------|--------|
| P1 | 证书验证缓存命中路径扩展（Cert Verify Cache） | 中高（重复证书链场景显著） | 中 | 高 |
| P2 | 随机池参数调优（Random Pool refill/阈值） | 中（高频随机请求场景） | 中 | 中高 |
| P3 | AES/GCM 基准路径降噪（迭代与采样策略） | 中（提高优化判断可信度） | 低 | 中 |

---

## B100 推荐执行项（单线程 WIP=1）

### 目标

以最小改动完成一项“可证明优化”，优先选择 **P1（证书验证缓存命中路径）**。

### 最小验收链路

1. 编译/基线：
   - `python3 scripts/compile_all_modules.py`
   - `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`
2. 性能证据：
   - `fpc -Mobjfpc -Sh -O2 -Fu./src -Fu./src/openssl -Fu./tests/benchmarks -Fu./examples -Fi./src -FE./tests/benchmarks/bin tests/benchmarks/benchmark_cert_verify_cache.pas`
   - `./tests/benchmarks/bin/benchmark_cert_verify_cache`
3. 输出要求：
   - 形成“优化前/后”对照报告（同命令、同环境、同迭代参数）。

---

## 风险与控制

- 风险 1：低迭代导致 0ms 噪声，掩盖真实变化。
  - 控制：B100 起 baseline 采样提升迭代（建议 >=500，按耗时调整）。
- 风险 2：把缓存收益误认为全链路收益。
  - 控制：明确 benchmark 作用域（cert verify path）并与模块回归一同报告。
