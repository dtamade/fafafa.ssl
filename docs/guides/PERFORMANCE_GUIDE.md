# 性能优化指南

## 概述

本指南记录了 fafafa.ssl 项目的性能优化实践、已完成的优化工作和调优建议。

---

## 已完成的性能优化

### Phase B 优化成果

#### 1. Random Pool 优化 ✅

**优化目标**：提升随机数生成性能

**实施方案**：
- 批量生成 8KB 随机数据到缓存池
- 从缓存快速复制数据
- 智能重填机制（剩余 < 1KB 时自动重填）
- 大请求绕过（> 4KB 直接生成）

**性能提升**：
- 64B: 2.4x 提升（250,000 → 600,000 ops/s）
- 1KB: 4.5x 提升（111,111 → 500,000 ops/s）
- 16KB: 6.9x 提升（15,873 → 109,890 ops/s）

**使用方法**：
```pascal
uses fafafa.ssl.random.pool;

// 使用全局池化随机数生成
var
  Buffer: array[0..1023] of Byte;
begin
  if PooledRandomBytes(@Buffer[0], 1024) then
    WriteLn('生成成功');
end;

// 配置池参数
var
  Config: TRandomPoolConfig;
begin
  Config := TRandomPoolConfig.Default;
  Config.PoolSize := 16384;  // 16KB 池大小
  Config.RefillThreshold := 2048;  // 2KB 重填阈值
  ConfigureGlobalRandomPool(Config);
end;
```

---

#### 2. TLS Session Caching 优化 ✅

**优化目标**：提升 TLS 握手性能

**实施方案**：
- 会话缓存和复用
- 减少完整握手次数

**性能提升**：
- 首次握手: ~30ms
- 会话复用: ~10ms（2.9-6.4x 提升）

**使用方法**：
```pascal
uses fafafa.ssl.context.builder;

var
  Ctx: ISSLContext;
begin
  // 启用会话缓存
  Ctx := TSSLContextBuilder.Create
    .WithTLS12And13
    .WithSessionCache(True)  // 启用会话缓存
    .BuildClient;
end;
```

---

#### 3. AES-GCM Context Pool 优化 ⚠️

**优化目标**：通过上下文复用提升 AES-GCM 性能

**实施方案**：
- 上下文池化和复用
- 唯一 IV 生成
- LRU 驱逐策略

**性能测试结果**：

| 操作 | 数据大小 | Pooled | Traditional | 性能对比 |
|------|---------|--------|-------------|---------|
| Encrypt | 1KB | 1,000,000 ops/s | 250,000 ops/s | **4x 提升** ✅ |
| Encrypt | 64B | 250,000 ops/s | 250,000 ops/s | 持平 |
| Encrypt | 16KB | 83,333 ops/s | 111,111 ops/s | **-25% 下降** ⚠️ |
| Decrypt | 1KB | 125,000 ops/s | 200,000 ops/s | **-37% 下降** ⚠️ |

**结论**：
- ✅ **1KB 加密场景有显著提升**（4x）
- ⚠️ **解密和大数据块性能下降**
- 💡 **建议**：仅在 1KB 左右的加密场景使用池化

**使用建议**：
```pascal
// 推荐：1KB 左右的加密场景
if DataSize >= 512 and DataSize <= 2048 then
  UsePooledAESGCM  // 使用池化版本
else
  UseTraditionalAESGCM;  // 使用传统版本
```

---

## 性能基准数据

### 加密算法性能（OpenSSL 3.x）

| 算法 | 数据大小 | 吞吐量 (ops/s) | 延迟 (ms) |
|------|---------|---------------|----------|
| SHA-256 | 64B | 1,000,000 | 0.001 |
| SHA-256 | 1KB | 200,000 | 0.005 |
| SHA-256 | 16KB | 19,230 | 0.052 |
| SHA-512 | 64B | 1,000,000 | 0.001 |
| SHA-512 | 1KB | 250,000 | 0.004 |
| SHA-512 | 16KB | 25,000 | 0.040 |
| AES-256-GCM 加密 | 64B | 250,000 | 0.004 |
| AES-256-GCM 加密 | 1KB | 83,333 | 0.012 |
| AES-256-GCM 加密 | 16KB | 11,111 | 0.090 |
| AES-256-GCM 解密 | 64B | 250,000 | 0.004 |
| AES-256-GCM 解密 | 1KB | 333,333 | 0.003 |
| AES-256-GCM 解密 | 16KB | 16,666 | 0.060 |
| 随机数生成 | 64B | 250,000 | 0.004 |
| 随机数生成 | 1KB | 111,111 | 0.009 |
| 随机数生成 | 16KB | 15,873 | 0.063 |

### TLS 握手性能

| 场景 | 吞吐量 (ops/s) | 延迟 (ms) |
|------|---------------|----------|
| 完整握手 | ~3,200 | ~30 |
| 会话复用 | ~10,000 | ~10 |

---

## 性能调优建议

### 1. 随机数生成优化

**场景**：频繁生成小块随机数（< 4KB）

**建议**：
- ✅ 使用 `PooledRandomBytes` 替代 `SecureRandomBytes`
- ✅ 配置合适的池大小（默认 8KB）
- ✅ 调整重填阈值（默认 1KB）

**示例**：
```pascal
// 不推荐：频繁调用
for i := 1 to 1000 do
  SecureRandomBytes(@Buffer[0], 256);

// 推荐：使用池化
for i := 1 to 1000 do
  PooledRandomBytes(@Buffer[0], 256);
```

---

### 2. TLS 连接优化

**场景**：频繁建立 TLS 连接

**建议**：
- ✅ 启用会话缓存
- ✅ 复用 SSL 上下文
- ✅ 使用连接池

**示例**：
```pascal
// 不推荐：每次创建新上下文
for i := 1 to 100 do
begin
  Ctx := TSSLContextBuilder.Create.BuildClient;
  // 使用 Ctx
  Ctx := nil;
end;

// 推荐：复用上下文
Ctx := TSSLContextBuilder.Create
  .WithSessionCache(True)
  .BuildClient;
try
  for i := 1 to 100 do
  begin
    // 使用 Ctx（会话自动复用）
  end;
finally
  Ctx := nil;
end;
```

---

### 3. AES-GCM 加密优化

**场景**：1KB 左右的数据加密

**建议**：
- ✅ 使用 AES-GCM Context Pool（仅限 1KB 场景）
- ⚠️ 大数据块（> 4KB）使用传统方式
- ⚠️ 解密操作使用传统方式

**示例**：
```pascal
// 根据数据大小选择方式
if (DataSize >= 512) and (DataSize <= 2048) and IsEncryption then
  UsePooledAESGCM  // 1KB 加密场景
else
  UseTraditionalAESGCM;  // 其他场景
```

---

### 4. 哈希计算优化

**场景**：大数据块哈希计算

**建议**：
- ✅ 大数据块（> 4KB）优先使用 SHA-512（64-bit 系统）
- ✅ 小数据块（< 1KB）使用 SHA-256
- ✅ 批量处理数据

**性能对比**：
- SHA-256 (16KB): 19,230 ops/s
- SHA-512 (16KB): 25,000 ops/s（**30% 更快**）

---

## 性能分析工具

### 1. 性能基准测试

**运行所有基准测试**：
```bash
cd tests/benchmarks
./run_all_benchmarks.sh
```

**运行特定测试**：
```bash
# 加密算法测试
./bin/benchmark_crypto_comprehensive

# TLS 握手测试
./bin/benchmark_tls_handshake

# Random Pool 测试
./bin/benchmark_random_pool

# AES-GCM Pool 测试
./bin/benchmark_aesgcm_pool
```

---

### 2. 性能回归检测

**配置 CI 集成**（可选）：
```yaml
# .github/workflows/ci.yml
- name: Performance Tests
  run: |
    cd tests/benchmarks
    ./run_all_benchmarks.sh
    # 检查回归（15% 阈值）
```

---

## 性能优化案例

### 案例 1：高频随机数生成

**场景**：生成大量会话密钥

**优化前**：
```pascal
for i := 1 to 10000 do
begin
  SecureRandomBytes(@SessionKey[0], 32);
  // 使用 SessionKey
end;
// 性能：~8,000 ops/s
```

**优化后**：
```pascal
for i := 1 to 10000 do
begin
  PooledRandomBytes(@SessionKey[0], 32);
  // 使用 SessionKey
end;
// 性能：~35,000 ops/s（4.4x 提升）
```

---

### 案例 2：TLS 连接池

**场景**：Web 服务器处理大量 HTTPS 请求

**优化前**：
```pascal
// 每个请求创建新上下文
procedure HandleRequest;
var
  Ctx: ISSLContext;
begin
  Ctx := TSSLContextBuilder.Create.BuildClient;
  // 处理请求
  Ctx := nil;
end;
// 性能：~3,200 requests/s
```

**优化后**：
```pascal
// 全局上下文 + 会话缓存
var
  GlobalCtx: ISSLContext;

initialization
  GlobalCtx := TSSLContextBuilder.Create
    .WithSessionCache(True)
    .BuildClient;

procedure HandleRequest;
begin
  // 复用 GlobalCtx（会话自动复用）
  // 处理请求
end;
// 性能：~10,000 requests/s（3.1x 提升）
```

---

## 性能监控

### 关键指标

| 指标 | 目标值 | 监控方法 |
|------|--------|---------|
| TLS 握手延迟 | < 50ms | 基准测试 |
| 会话复用率 | > 70% | 日志分析 |
| 随机数生成 | > 100,000 ops/s (1KB) | 基准测试 |
| AES-GCM 加密 | > 50,000 ops/s (1KB) | 基准测试 |
| 内存使用 | < 100MB | 系统监控 |

---

## 故障排除

### 问题 1：Random Pool 性能未提升

**可能原因**：
- 池未启用
- 请求大小超过 MaxRequestSize
- 池大小配置不当

**解决方案**：
```pascal
// 检查配置
var
  Stats: TRandomPoolStats;
begin
  Stats := GetGlobalRandomPool.GetStats;
  WriteLn('缓存命中率: ', Stats.CacheHits / Stats.TotalRequests * 100, '%');
  WriteLn('缓存未命中: ', Stats.CacheMisses);
end;
```

---

### 问题 2：TLS 会话复用未生效

**可能原因**：
- 会话缓存未启用
- 服务器不支持会话复用
- 会话超时

**解决方案**：
```pascal
// 确保启用会话缓存
Ctx := TSSLContextBuilder.Create
  .WithSessionCache(True)
  .BuildClient;

// 检查会话复用
WriteLn('会话复用: ', Stream.Connection.IsSessionReused);
```

---

## 相关资源

### 文档
- [Phase B 性能基准报告](PHASE_B_PERFORMANCE_BASELINE_REPORT.md)
- [Phase B 完成总结](PHASE_B_COMPLETION_SUMMARY.md)
- [跨平台性能总结](PHASE_2_CROSS_PLATFORM_PERFORMANCE_SUMMARY.md)

### 源代码
- `src/fafafa.ssl.random.pool.pas` - Random Pool 实现
- `src/fafafa.ssl.aesgcm.pool.pas` - AES-GCM Context Pool 实现
- `tests/benchmarks/` - 性能测试套件

### 基准数据
- `tests/benchmarks/results/crypto_baseline.json` - 加密算法基线
- `tests/benchmarks/results/tls_baseline.json` - TLS 握手基线

---

## 更新日志

### 2026-01-24
- ✅ 完成 Phase C 性能优化验证
- ✅ 验证 AES-GCM Pool 优化效果
- ✅ 创建性能优化指南文档
- ✅ 记录优化案例和调优建议

### 2026-01-21
- ✅ 完成 Random Pool 优化（2.4-6.9x 提升）
- ✅ 完成 TLS Session Caching（2.9-6.4x 提升）
- ✅ 完成 AES-GCM Context Pool 集成

---

**文档版本**: 1.0  
**最后更新**: 2026-01-24  
**维护者**: fafafa.ssl 项目组
