# 测试质量指南

本文档说明 fafafa.ssl 项目的测试策略、质量指标和最佳实践。

## 测试架构

### 测试目录结构

```
tests/
├── unit/              # 单元测试
├── integration/       # 集成测试
├── crypto/            # 加密功能测试
├── security/          # 安全测试
├── certificate/       # 证书相关测试
├── connection/        # 连接测试
├── openssl/           # OpenSSL特定测试
├── winssl/            # WinSSL特定测试
├── stress/            # 压力测试
├── fuzz/              # 模糊测试
├── benchmarks/        # 性能基准测试
└── framework/         # 测试框架
```

### 测试类型

| 类型 | 目的 | 位置 |
|------|------|------|
| 单元测试 | 测试单个函数/类 | `tests/unit/` |
| 集成测试 | 测试模块间交互 | `tests/integration/` |
| 安全测试 | 验证安全属性 | `tests/security/` |
| 压力测试 | 测试高负载场景 | `tests/stress/` |
| 模糊测试 | 发现边界问题 | `tests/fuzz/` |

## 质量指标

### 目标阈值

| 指标 | 目标 | 说明 |
|------|------|------|
| 综合评分 | ≥70% | 所有指标的加权平均 |
| 错误处理 | ≥60% | 错误路径测试覆盖 |
| 线程安全 | ≥60% | 并发场景测试 |
| 后端一致性 | ≥60% | OpenSSL/WinSSL一致性 |
| 加密测试 | ≥60% | KAT向量和边界测试 |
| 代码覆盖 | ≥60% | 函数级覆盖率 |
| 边界测试 | ≥80% | 边界条件覆盖 |

### 质量评分计算

综合评分使用加权平均：

```
Overall = Coverage × 0.15 +
          BoundaryTesting × 0.20 +
          ErrorHandling × 0.15 +
          CryptoTesting × 0.20 +
          ThreadSafety × 0.10 +
          ResourceManagement × 0.10 +
          BackendConsistency × 0.10
```

## 测试最佳实践

### 错误处理测试

```pascal
// 测试无效参数
procedure TestInvalidParameter;
begin
  try
    SomeFunction(nil);  // 传入nil
    Fail('Should raise exception');
  except
    on E: EInvalidParameter do
      Pass('Correctly rejected nil');
  end;
end;
```

### 边界条件测试

```pascal
// 测试边界值
procedure TestBoundaryValues;
begin
  // 空输入
  Check(ProcessData(nil, 0) = ERR_INVALID_INPUT);
  
  // 最小有效输入
  Check(ProcessData(@Data[0], 1) = SUCCESS);
  
  // 最大有效输入
  Check(ProcessData(@Data[0], MAX_SIZE) = SUCCESS);
  
  // 超出范围
  Check(ProcessData(@Data[0], MAX_SIZE + 1) = ERR_SIZE_EXCEEDED);
end;
```

### 线程安全测试

```pascal
// 并发访问测试
procedure TestConcurrentAccess;
var
  Threads: array[0..9] of TThread;
  I: Integer;
begin
  for I := 0 to 9 do
    Threads[I] := TWorkerThread.Create(SharedResource);
    
  for I := 0 to 9 do
    Threads[I].Start;
    
  for I := 0 to 9 do
    Threads[I].WaitFor;
    
  Check(SharedResource.IsConsistent);
end;
```

### 加密测试 (KAT向量)

```pascal
// 使用已知答案测试
procedure TestSHA256_KAT;
const
  // NIST测试向量
  Input = 'abc';
  Expected = 'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad';
var
  Result: TBytes;
begin
  Result := SHA256(TEncoding.UTF8.GetBytes(Input));
  Check(BytesToHex(Result) = Expected);
end;
```

## 运行测试

### 运行所有测试

```bash
./ci_pipeline.sh test
```

### 运行特定测试

```bash
# 编译并运行单个测试
fpc -Fusrc tests/test_hash.pas
./tests/test_hash
```

### 运行质量审计

```bash
./ci_pipeline.sh audit
```

## 测试报告

### 审计报告位置

- `reports/audit/audit_*.md` - Markdown报告
- `reports/audit/audit_*.json` - JSON报告
- `reports/audit/quality_trend.csv` - 趋势数据

### 报告内容

审计报告包含：

1. 综合评分和各维度分数
2. 未覆盖的函数列表
3. 缺失的测试类型
4. 改进建议和优先级

## 持续改进

### 添加新测试

1. 确定测试类型和位置
2. 使用项目测试框架
3. 包含正向和负向测试
4. 添加边界条件测试
5. 运行审计验证覆盖

### 修复测试缺口

1. 运行 `./ci_pipeline.sh audit`
2. 查看报告中的改进任务
3. 按优先级实现测试
4. 重新运行审计验证

---

**更新日期**: 2026-01-06
**适用版本**: fafafa.ssl v1.0.0-rc
