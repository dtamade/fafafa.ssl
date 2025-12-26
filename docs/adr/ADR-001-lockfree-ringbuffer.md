# ADR-001: 无锁 vs 加锁环形缓冲区

## 状态
已接受 (2025-12-26)

## 上下文
SSL/TLS 数据流需要高效的缓冲区来处理加密/解密操作。原始 `TRingBuffer` 实现使用 `TCriticalSection` 保护所有操作，包括简单的属性访问（如 `GetAvailable`），导致高锁竞争。

性能基准显示：
- 100万次迭代，256字节块
- 加锁版本标准接口：115ms
- 加锁版本零拷贝：203ms

## 决策
我们决定添加无锁 `TLockFreeRingBuffer` 实现，专门用于单生产者-单消费者 (SPSC) 场景：

1. **保留两个版本**：
   - `TRingBuffer`：多生产者-多消费者 (MPMC) 场景
   - `TLockFreeRingBuffer`：SPSC 场景（SSL 数据流典型用例）

2. **技术实现**：
   - 使用内存屏障 (`sfence`, `lfence`, `mfence`) 替代锁
   - 缓存行填充防止伪共享
   - 位运算快速取模（容量必须为2的幂）

## 后果

### 正面
- 标准接口加速 1.51x（115ms → 76ms）
- 零拷贝接口加速 2.99x（203ms → 68ms）
- 吞吐量达到 7.2 GB/s

### 负面
- 仅适用于 SPSC 场景
- 多个生产者/消费者时仍需使用 `TRingBuffer`
- 增加代码复杂度

### 风险
- 内存屏障在不同 CPU 架构上行为可能不同
- 错误使用可能导致数据竞争

## 替代方案

1. **原子操作**：使用 `InterlockedXXX` 系列函数
   - 否决：SPSC 场景下内存屏障更高效

2. **自旋锁**：使用轻量级自旋锁替代临界区
   - 否决：对于短临界区可能降低性能

3. **仅优化现有实现**：减少锁获取次数
   - 否决：无法消除锁开销

## 参考
- `src/fafafa.ssl.ringbuffer.pas` - 原始加锁实现
- `src/fafafa.ssl.ringbuffer.lockfree.pas` - 无锁 SPSC 实现
- `tests/benchmark_ringbuffer.pas` - 性能基准测试
