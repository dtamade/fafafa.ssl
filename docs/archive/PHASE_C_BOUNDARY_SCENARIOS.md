# Phase C 边界场景清单

**阶段**: Phase C - 测试覆盖率提升
**生成日期**: 2026-01-31
**状态**: ✅ 完成

---

## 执行摘要

识别了 8 大类边界场景,共 40+ 个具体测试场景。这些场景覆盖了内存限制、数据大小边界、空输入、并发、超时、资源耗尽、安全攻击等关键错误路径。

**关键发现**:
- **边界场景类别**: 8 大类
- **具体测试场景**: 40+ 个
- **优先级分布**: P0 (16 个), P1 (16 个), P2 (8 个)
- **预计测试文件**: 8-12 个新测试文件

---

## 边界场景分类

### 1. 内存限制场景 (Memory Limits)

**优先级**: P0 (高)

| 场景 | 描述 | 预期行为 | 测试文件 |
|------|------|---------|---------|
| 大数据块加密 | 加密 > 16MB 数据 | 抛出 ESSLResourceException 或分块处理 | test_memory_limits.pas |
| 大数据块解密 | 解密 > 16MB 数据 | 抛出 ESSLResourceException 或分块处理 | test_memory_limits.pas |
| 证书链过长 | 证书链 > 10 层 | 抛出 ESSLCertificateException | test_cert_chain_limits.pas |
| 并发连接内存 | 1000+ 并发连接 | 优雅降级或限流 | test_concurrent_limits.pas |

**实现建议**:
- 使用 mock 内存分配失败
- 测试分块处理逻辑
- 验证错误消息清晰

---

### 2. 数据大小边界 (Data Size Boundaries)

**优先级**: P0 (高)

| 场景 | 描述 | 预期行为 | 测试文件 |
|------|------|---------|---------|
| 空输入 | 加密/解密空字符串 | 返回空结果或抛出 ESSLInvalidArgument | test_data_boundaries.pas |
| 超小数据块 | 加密 1 字节数据 | 正常处理 | test_data_boundaries.pas |
| 标准数据块 | 加密 64B-16KB 数据 | 正常处理 | test_data_boundaries.pas |
| 超大数据块 | 加密 > 16MB 数据 | 抛出异常或分块处理 | test_data_boundaries.pas |
| 边界值 | 加密 4KB, 8KB, 16KB (边界) | 正常处理 | test_data_boundaries.pas |

**实现建议**:
- 测试 0, 1, 63, 64, 4095, 4096, 8191, 8192, 16383, 16384 字节
- 验证随机池和 AES-GCM 池的边界行为
- 确保边界值不会触发意外错误

---

### 3. 空输入/Nil 指针场景 (Null/Nil Inputs)

**优先级**: P1 (中)

| 场景 | 描述 | 预期行为 | 测试文件 |
|------|------|---------|---------|
| nil 证书 | LoadCertificate(nil) | 抛出 ESSLInvalidArgument | test_nil_inputs.pas |
| nil 密钥 | LoadPrivateKey(nil) | 抛出 ESSLInvalidArgument | test_nil_inputs.pas |
| nil 上下文 | 使用 nil ISSLContext | 抛出 ESSLInvalidArgument | test_nil_inputs.pas |
| 空字符串路径 | LoadCertificateFromFile('') | 抛出 ESSLInvalidArgument | test_nil_inputs.pas |
| nil 回调函数 | SetVerifyCallback(nil) | 抛出 ESSLInvalidArgument 或使用默认 | test_nil_inputs.pas |

**实现建议**:
- 测试所有公共 API 的 nil 参数
- 验证错误消息包含参数名称
- 确保不会崩溃或段错误

---

### 4. 并发场景 (Concurrency)

**优先级**: P0 (高)

| 场景 | 描述 | 预期行为 | 测试文件 |
|------|------|---------|---------|
| 并发连接 | 100+ 并发 TLS 连接 | 正常处理或优雅降级 | test_concurrent_connections.pas |
| 竞争条件 | 多线程访问同一上下文 | 线程安全或抛出异常 | test_race_conditions.pas |
| 随机池并发 | 多线程调用 GetRandomBytes | 线程安全,无数据竞争 | test_random_pool_concurrent.pas |
| AES-GCM 池并发 | 多线程加密/解密 | 线程安全,无数据竞争 | test_aesgcm_pool_concurrent.pas |

**实现建议**:
- 使用 Free Pascal 的 TThread
- 测试 10, 100, 1000 并发
- 验证无死锁、无数据竞争
- 使用 heaptrc 检测内存泄漏

---

### 5. 超时场景 (Timeouts)

**优先级**: P1 (中)

| 场景 | 描述 | 预期行为 | 测试文件 |
|------|------|---------|---------|
| 连接超时 | 连接到不存在的服务器 | 抛出 ESSLConnectionException (超时) | test_timeouts.pas |
| 握手超时 | TLS 握手超过 30 秒 | 抛出 ESSLConnectionException (超时) | test_timeouts.pas |
| 读超时 | 读取数据超时 | 抛出 ESSLConnectionException (超时) | test_timeouts.pas |
| 写超时 | 写入数据超时 | 抛出 ESSLConnectionException (超时) | test_timeouts.pas |
| OCSP 超时 | OCSP 响应超时 | 使用缓存或失败 | test_ocsp_timeout.pas |

**实现建议**:
- 使用 mock 服务器模拟超时
- 测试超时配置 (1s, 5s, 30s)
- 验证超时后资源正确释放

---

### 6. 资源耗尽场景 (Resource Exhaustion)

**优先级**: P1 (中)

| 场景 | 描述 | 预期行为 | 测试文件 |
|------|------|---------|---------|
| 文件描述符耗尽 | 打开 > ulimit 个连接 | 抛出 ESSLResourceException | test_resource_exhaustion.pas |
| 内存耗尽 | 分配大量内存 | 抛出 ESSLResourceException | test_resource_exhaustion.pas |
| 线程池耗尽 | 创建 > 最大线程数 | 排队或抛出异常 | test_resource_exhaustion.pas |
| 证书缓存满 | 缓存 > 1000 个证书 | LRU 淘汰或扩展 | test_cert_cache_limits.pas |

**实现建议**:
- 使用 ulimit 限制资源
- 测试资源耗尽后的恢复
- 验证错误消息清晰

---

### 7. 安全攻击场景 (Security Attacks)

**优先级**: P0 (高)

| 场景 | 描述 | 预期行为 | 测试文件 |
|------|------|---------|---------|
| 协议降级攻击 | 强制使用 TLS 1.0 | 拒绝连接 | test_security_attacks.pas |
| 重放攻击 | 重放 TLS 握手消息 | 检测并拒绝 | test_security_attacks.pas |
| 中间人攻击 | 伪造证书 | 证书验证失败 | test_security_attacks.pas |
| 证书固定绕过 | 使用不同证书 | 固定验证失败 | test_cert_pinning_bypass.pas |
| 时间攻击 | 测量加密时间差异 | 常量时间操作 | test_timing_attacks.pas |
| 填充预言攻击 | 无效填充 | 常量时间错误 | test_padding_oracle.pas |

**实现建议**:
- 使用 mock 攻击者
- 验证安全机制有效
- 测试常量时间操作

---

### 8. 配置边界场景 (Configuration Boundaries)

**优先级**: P2 (低)

| 场景 | 描述 | 预期行为 | 测试文件 |
|------|------|---------|---------|
| 无效协议版本 | 设置 TLS 0.9 | 抛出 ESSLConfigurationException | test_config_boundaries.pas |
| 无效密码套件 | 设置不存在的密码套件 | 抛出 ESSLConfigurationException | test_config_boundaries.pas |
| 配置冲突 | 同时启用互斥选项 | 抛出 ESSLConfigurationException | test_config_boundaries.pas |
| 缺失必需配置 | 未设置证书 | 抛出 ESSLConfigurationException | test_config_boundaries.pas |

**实现建议**:
- 测试所有配置参数的边界值
- 验证配置验证逻辑
- 确保错误消息指出具体问题

---

## 边界场景优先级

### P0 (高优先级) - 16 个场景

**必须覆盖的关键场景**:
1. 大数据块加密/解密 (内存限制)
2. 空输入/超小数据块 (数据边界)
3. 并发连接 (并发)
4. 协议降级攻击 (安全)
5. 中间人攻击 (安全)
6. 证书链过长 (内存限制)
7. 竞争条件 (并发)
8. 随机池并发 (并发)

**理由**: 这些场景直接影响安全性、稳定性和性能,必须优先覆盖。

---

### P1 (中优先级) - 16 个场景

**重要但非关键的场景**:
1. nil 指针 (空输入)
2. 超时场景 (连接、握手、读写)
3. 资源耗尽 (文件描述符、内存)
4. 证书固定绕过 (安全)
5. 时间攻击 (安全)

**理由**: 这些场景影响用户体验和健壮性,应该覆盖但不阻塞发布。

---

### P2 (低优先级) - 8 个场景

**可选的边界场景**:
1. 配置边界 (无效配置)
2. 填充预言攻击 (安全)
3. 证书缓存满 (资源)

**理由**: 这些场景较少发生或影响有限,可以延后覆盖。

---

## 测试文件规划

### 新增测试文件 (8-12 个)

| 测试文件 | 场景数 | 优先级 | 预计工作量 |
|---------|--------|--------|-----------|
| test_memory_limits.pas | 4 | P0 | 1 天 |
| test_data_boundaries.pas | 5 | P0 | 1 天 |
| test_nil_inputs.pas | 5 | P1 | 0.5 天 |
| test_concurrent_connections.pas | 4 | P0 | 1 天 |
| test_race_conditions.pas | 2 | P0 | 1 天 |
| test_timeouts.pas | 5 | P1 | 1 天 |
| test_resource_exhaustion.pas | 4 | P1 | 1 天 |
| test_security_attacks.pas | 6 | P0 | 1.5 天 |
| test_config_boundaries.pas | 4 | P2 | 0.5 天 |

**总计**: 9 个文件, 39 个场景, 8.5 天工作量

---

## 实施建议

### Week 2-4 实施计划

**Week 2: TLS 握手失败场景** (5 天)
- 实现 test_tls_handshake_failures.pas
- 覆盖 8 个握手失败场景
- 集成到 CI

**Week 3: 证书验证失败场景** (5 天)
- 实现 test_cert_validation_failures.pas
- 覆盖 8 个验证失败场景
- 集成到 CI

**Week 4: 资源限制与并发测试** (5 天)
- 实现 test_memory_limits.pas
- 实现 test_concurrent_connections.pas
- 实现 test_security_attacks.pas
- 集成到 CI

---

## 测试策略

### Mock 策略

| 场景类型 | Mock 方法 |
|---------|----------|
| 内存不足 | Mock 内存分配失败 |
| 超时 | Mock 慢速服务器 |
| 资源耗尽 | 使用 ulimit 限制 |
| 安全攻击 | Mock 攻击者 |

### 验证策略

| 验证项 | 方法 |
|--------|------|
| 异常类型 | Assert 异常类型正确 |
| 错误消息 | Assert 错误消息清晰 |
| 资源释放 | heaptrc 检测内存泄漏 |
| 线程安全 | 并发测试无数据竞争 |

---

## 下一步行动

1. ✅ **Week 1.1**: 错误路径分析完成
2. ✅ **Week 1.2**: 边界场景识别完成
3. ⏳ **Week 1.3**: 评估现有 184 个边界/错误测试文件的覆盖缺口
4. ⏳ **Week 1.4**: 完成错误路径分析报告
5. ⏳ **Week 1.5**: 完成边界场景清单 (本文档)

---

**报告生成日期**: 2026-01-31
**报告生成者**: Claude (Sisyphus)
**Phase C 进度**: Week 1.2 完成 (40%)
