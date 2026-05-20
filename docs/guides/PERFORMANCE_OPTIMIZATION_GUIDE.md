# TLS 性能优化指南

## 概述

本指南聚焦 TLS 握手、会话复用、上下文复用和诊断入口。TLS 握手延迟和会话复用收
益会受到 backend、TLS 版本、目标站点、网络路径、runner/主机与是否拿到票据等
因素影响，不应该把某次本地或公网测试的固定毫秒数、P99、倍率写成长期 truth。

如需当前 baseline，请先跑 `scripts/run_phase2_performance_baseline.sh` 或
`tests/benchmarks/run_all_benchmarks.sh`，再结合输出目录和环境记录解读结果。

## 当前真相源

- `scripts/run_phase2_performance_baseline.sh`
- `tests/benchmarks/run_all_benchmarks.sh`
- `tests/benchmarks/benchmark_tls_handshake.pas`
- `tests/benchmarks/benchmark_tls_handshake_diagnostic.pas`
- `tests/benchmarks/baselines/tls_handshake_baseline.json`

## 会话复用优先走 owner path

核心 `ISSLConnection.GetSession` / `SetSession` / `IsSessionReused` 在新代码里只
作为 compatibility mirror 保留；性能相关示例优先走
`ISSLSessionResumption`。

```pascal
uses
  SysUtils,
  fafafa.ssl.base;

var
  Conn1, Conn2: ISSLConnection;
  Session: ISSLSession;
  Resumption1, Resumption2: ISSLSessionResumption;
begin
  if Conn1.Connect and Supports(Conn1, ISSLSessionResumption, Resumption1) then
    Session := Resumption1.GetSession;

  if Supports(Conn2, ISSLSessionResumption, Resumption2) and Assigned(Session) then
    Resumption2.SetSession(Session);

  if Conn2.Connect and Supports(Conn2, ISSLSessionResumption, Resumption2) then
    WriteLn(BoolToStr(Resumption2.IsSessionReused, True));
end;
```

请同时满足这些前提，再判断是否值得期待复用收益：

- 使用同一个 `ISSLContext`
- 保持同一主机/SNI 与相近的连接策略
- 在握手前注入待恢复会话
- 服务端和当前 backend 真的允许/拿到了可复用的 session 或 ticket

对于 TLS 1.3、特定服务端策略或 Windows/公网端点，请用当前 backend 的实际 run
结果验证，不要从一次旧实验推断成通用规律。

当前 backend-specific caveat：

- `WinSSL`: dedicated Windows runtime truth 仍以 `observed_reuse=false` / `session_configured=true` 为准；没有 target-specific validation 时，不要把 `SetSession(...)` 直接当成已稳定命中的 resumed-handshake 收益。
- `MbedTLS`: 当前 public surface 可以保存并注入 session candidate，但由于没有与 `SSL_session_reused` / `wolfSSL_session_reused` 对称的 public reused getter，当前 contract truth 只证明“configured session 不会被误报成 observed reuse”；不要把一次 `SetSession(...)` 直接读成通用 runtime 收益。

## 诊断与性能指标优先走 owner path

核心 `ISSLConnection.GetPerformanceMetrics` 在新代码里只作为 compatibility
mirror 保留；性能/诊断采样优先走 `ISSLDiagnostics`。

```pascal
uses
  fafafa.ssl.base;

var
  Stream: TSSLStream;
  Diag: ISSLDiagnostics;
  Perf: TSSLPerformanceMetrics;
begin
  if Supports(Stream.Connection, ISSLDiagnostics, Diag) then
  begin
    Perf := Diag.GetPerformanceMetrics;
    WriteLn('performance snapshot captured');
  end;
end;
```

如果你要上报健康状态、诊断信息或性能指标，同样优先通过 `ISSLDiagnostics`，
不要继续把 core mirror 当成主入口。

## 复用上下文

避免为每条连接重新创建上下文。默认建议是创建一次、复用多次：

```pascal
Ctx := TSSLContextBuilder.Create
  .WithTLS12And13
  .WithVerifyPeer
  .WithSystemRoots
  .WithSessionCache(True)
  .BuildClient;
```

何时需要分裂成多个 context：

- 不同证书/私钥
- 不同 trust roots
- 不同 TLS 版本策略
- 不同 client auth / ALPN / timeout 需求

## TLS 版本策略

默认优先从 `WithTLS12And13` 开始，让端点自行协商。只有在下面几类场景里才建议
固定版本：

- 兼容性要求明确
- 服务端策略明确
- 真实 benchmark 证明某个版本在你的 workload 下更合适
- 你在做协议级排障，需要缩小变量

不要把“某个版本曾经更快”写成通用结论。比较时至少要固定：

- 同一个 backend
- 同一个目标端点
- 同一条网络路径
- 同样的超时与迭代次数

## 推荐测量方式

在仓库根目录执行：

```bash
# 先确认输出路径和命令解析结果
bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local

# 本地快速基线
bash scripts/run_phase2_performance_baseline.sh --fast-local --iterations 100 --tls-iterations 10 --skip-tls

# 统一 benchmark runner
bash tests/benchmarks/run_all_benchmarks.sh --iterations 1000 --tls-iterations 100 --output tmp/bench_results --bin-dir tmp/bench_bin
```

如果你要拆解 TLS 时间分布，请优先看：

- `tests/benchmarks/benchmark_tls_handshake_diagnostic.pas`

并把 loopback 与公网端点分开记录。不要把公网端点的尾延迟直接归因到库实现。

## 如何解读结果

建议把结果拆成三层：

- loopback
  - 更接近本机和库的热点
- 目标站点
  - 会叠加 DNS、TCP、代理、CDN、服务端 ticket 策略等外部变量
- 当前 backend
  - OpenSSL、WinSSL、MbedTLS、FreePascal 的握手路径和 session 行为不完全相同

记录时至少保留：

- 命令
- 运行主机或 runner
- backend
- 目标端点
- 迭代次数
- 输出目录

## 常见问题

### 会话没有复用

优先检查：

- 是否真的复用了同一个 `ISSLContext`
- 是否在握手前通过 `ISSLSessionResumption` 注入 session
- 是否用了同一个主机/SNI
- 服务端是否启用了 session id 或 ticket
- 当前 backend 是否需要先完成首条连接后的 ticket 接收流程

### TLS 1.3 看起来比 TLS 1.2 慢

先在同一端点、同一网络条件下比较，再看：

- 服务端协商策略
- 是否存在代理/CDN 干预
- 是否把 loopback 样本和公网样本混在一起比较
- 是否只看了单次运行，没有 fresh baseline

### 尾延迟很高

先把问题拆给网络与端点，而不是直接归因到库：

- DNS 抖动
- TCP 建连慢
- 代理/防火墙
- 目标站点负载
- runner 资源争用

## 其它性能 lane

Random Pool、证书验证缓存等非 TLS 热点，统一放在 `PERFORMANCE_GUIDE.md` 与默认
Phase 2 runner 下维护。需要补充结论时，也要沿用同一条“命令 + 环境 + output +
baseline 差异”的记录规则。
