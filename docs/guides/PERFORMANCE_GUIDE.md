# 性能指南

## 概述

本指南只负责说明当前 benchmark 入口、适用边界和结果解读；不要把某次历史
Phase/benchmark 报告里的固定吞吐量、延迟、倍率、成功率或完成度写成当前长期
truth。

性能结果会受到 backend、CPU、操作系统、编译器选项、OpenSSL/系统 TLS 栈、
测试参数、网络路径和目标端点影响。当前默认以 repo 内脚本、baseline 文件和
fresh output 为准。

## 当前真相源

当前真相源优先看：

- `scripts/run_phase2_performance_baseline.sh`
- `tests/benchmarks/run_all_benchmarks.sh`
- `tests/benchmarks/baselines/crypto_baseline.json`
- `tests/benchmarks/baselines/random_pool_baseline.json`
- `tests/benchmarks/baselines/tls_handshake_baseline.json`
- `docs/test_reports/PHASE2_PERFORMANCE_METRICS_TEMPLATE.md`

如果要发布新的性能结论，请附带本次命令、运行环境、输出目录和生成时间。

## 当前基准覆盖面

### 默认 Phase 2 lane

- `benchmark_crypto_comprehensive`
  - 覆盖哈希、对称加解密、随机数等基础密码学基准
- `benchmark_random_pool`
  - 覆盖随机数池在不同请求尺寸下的收益与边界
- `benchmark_cert_verify_cache`
  - 覆盖证书验证缓存的命中/未命中路径
- `benchmark_tls_handshake`
  - 覆盖 TLS 握手与会话复用的 baseline
  - 默认可以通过 `--skip-tls` 跳过网络相关部分

### 辅助诊断 lane

- `tests/benchmarks/benchmark_tls_handshake_diagnostic.pas`
  - 适合拆分 DNS/TCP/TLS 时间
  - 适合区分 loopback 与公网端点
- `tests/benchmarks/benchmark_aesgcm_pool.pas`
  - 当前不在默认 Phase 2 baseline runner 里
  - 如果要使用它发布结论，必须附带 fresh run 与适用场景说明

## 推荐运行方式

在仓库根目录执行：

```bash
# 先看当前解析后的输出路径，不污染工作树
bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local

# 本地快速基线，默认把结果写到 tmp/
bash scripts/run_phase2_performance_baseline.sh --fast-local --iterations 100 --tls-iterations 10 --skip-tls

# 需要完整 benchmark runner 时，直接运行统一入口
bash tests/benchmarks/run_all_benchmarks.sh --iterations 1000 --tls-iterations 100 --output tmp/bench_results --bin-dir tmp/bench_bin
```

如果需要 TLS 网络样本，再显式加上 `--with-tls`，并在记录里写明目标站点、网络
条件和超时设置。

## 结果解读

默认成功标准不是命中某个固定毫秒数或 ops/s，而是：

- 命令按预期退出 0
- 输出目录里生成 fresh `benchmark_summary_*.txt`
- 对应 baseline 文件或草案报告能说明本次环境和命令
- 回归判断基于“当前 run 与 baseline 的差异解释”，而不是照搬历史截图里的数字

解读时请分开看：

- 本地 loopback 结果
  - 更接近库本身与主机环境的开销
- 网络端点结果
  - 会叠加 DNS、TCP、代理、防火墙、目标站点策略等外部变量
- backend 差异
  - OpenSSL、WinSSL、MbedTLS、FreePascal 的热点不完全相同
- 编译/运行参数
  - 迭代次数、是否 `--skip-tls`、是否 `--fast-local` 都会影响结果

## 优化建议

### 随机数池

- 高频小块请求优先评估 `PooledRandomBytes`
- 是否真的收益更高，以 `benchmark_random_pool` 的当前运行结果为准
- 如果请求尺寸经常超过 `MaxRequestSize`，不要假设池化一定更快

### TLS 握手与会话复用

- 优先复用 `ISSLContext`
- 只在同一主机/SNI、服务端允许复用且当前 backend 真正拿到 ticket/session 时，
  才期待复用收益
- 相关示例和验证请优先看 `docs/guides/PERFORMANCE_OPTIMIZATION_GUIDE.md`

### 证书验证缓存

- 先用 `benchmark_cert_verify_cache` 或 Wave C 相关脚本验证命中/未命中边界
- 不要把某次 speedup factor 直接推广到所有部署环境

### AES-GCM 池化

- 当前不作为默认 Phase 2 baseline 的 shipped 结论
- 如需启用，先用目标 workload 自己跑 `benchmark_aesgcm_pool`
- 记录数据块大小、加密/解密方向和并发条件，再决定是否上线

## 排查建议

### benchmark 看起来变慢了

优先核对：

- 是否换了 backend、编译器版本或 OpenSSL 版本
- 是否从 loopback 切到了公网端点
- 是否修改了迭代次数、超时、CPU governor 或宿主机负载
- 是否把 `--fast-local` 与默认输出目录混用了，导致比较对象不一致

### 想把结果写回文档

建议只写：

- 命令
- 环境
- 输出目录
- 结论适用边界
- 与 baseline 的差异说明

不要直接把历史 `ops/s`、固定 `P99`、固定“提升 N 倍”或“完成某个 Phase”写回
当前指南正文。

## 相关资源

- `scripts/run_phase2_performance_baseline.sh`
- `tests/benchmarks/run_all_benchmarks.sh`
- `tests/benchmarks/benchmark_tls_handshake_diagnostic.pas`
- `tests/benchmarks/baselines/crypto_baseline.json`
- `tests/benchmarks/baselines/random_pool_baseline.json`
- `tests/benchmarks/baselines/tls_handshake_baseline.json`
- `docs/test_reports/PHASE2_PERFORMANCE_METRICS_TEMPLATE.md`
