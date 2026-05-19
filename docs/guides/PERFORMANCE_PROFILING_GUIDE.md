# fafafa.ssl 性能热点剖析指南

> **Batch**: B65
> **Status**: draft
> **Created**: 2026-02-07

## 概述

本指南介绍如何识别和优化 fafafa.ssl 应用中的性能热点。

## 性能剖析工具

### 1. 内置计时

```pascal
uses SysUtils;

var
  StartTime: TDateTime;
  ElapsedMs: Double;
begin
  StartTime := Now;

  // 执行操作
  Ctx := TSSLContextBuilder.Create.BuildClient;

  ElapsedMs := (Now - StartTime) * 24 * 60 * 60 * 1000;
  WriteLn('Context creation: ', ElapsedMs:0:2, ' ms');
end;
```

### 2. Valgrind (Linux)

```bash
# 安装
sudo apt install valgrind

# 运行 callgrind
valgrind --tool=callgrind ./your_program

# 查看结果
kcachegrind callgrind.out.*
```

### 3. Instruments (macOS)

```bash
# 使用 Time Profiler
instruments -t "Time Profiler" ./your_program

# 或使用 Xcode Instruments GUI
open -a Instruments
```

### 4. FPC 内置剖析

```bash
# 编译时启用剖析
fpc -pg your_program.pas

# 运行生成 gmon.out
./your_program

# 分析
gprof your_program gmon.out > profile.txt
```

## 常见热点

### 1. TLS 握手

**症状**: 首次连接慢

**原因**:

- 密钥交换计算密集
- 证书链验证
- OCSP 查询（如启用）

**优化**:

```pascal
// 启用 Session 复用
var
  Session: ISSLSession;
  Resumption1, Resumption2: ISSLSessionResumption;

Ctx := TSSLContextBuilder.Create
  .WithSessionCache(1000)  // 缓存 1000 个 session
  .BuildClient;

// 复用 session
if Supports(Conn1, ISSLSessionResumption, Resumption1) then
  Session := Resumption1.GetSession;
if Assigned(Session) and Supports(Conn2, ISSLSessionResumption, Resumption2) then
  Resumption2.SetSession(Session);
```

**预期提升**: 70-90% 握手时间减少

### 2. 证书验证

**症状**: 每次连接都慢

**原因**:

- 证书链遍历
- CRL/OCSP 检查
- 签名验证

**优化**:

```pascal
// 缓存验证结果（仅限可信环境）
var
  CertCache: TDictionary<string, Boolean>;

function VerifyWithCache(Cert: PX509): Boolean;
var
  Fingerprint: string;
begin
  Fingerprint := GetCertFingerprint(Cert);
  if CertCache.TryGetValue(Fingerprint, Result) then
    Exit;

  Result := VerifyCertificate(Cert);
  CertCache.Add(Fingerprint, Result);
end;
```

### 3. 密钥生成

**症状**: 证书生成慢

**原因**: RSA 密钥生成需要大量计算

**优化**:

```pascal
// 使用 ECDSA 替代 RSA
Options.KeyType := ktECDSA;
Options.ECCurve := 'prime256v1';

// 性能对比
// RSA 2048: ~100-500ms
// RSA 4096: ~1-3s
// ECDSA P-256: ~10-50ms
```

### 4. 大数据加密

**症状**: 加密大文件慢

**原因**: 内存分配和复制

**优化**:

```pascal
// 使用流式处理
const
  CHUNK_SIZE = 64 * 1024;  // 64KB chunks
var
  Buffer: array[0..CHUNK_SIZE-1] of Byte;
  BytesRead: Integer;
begin
  while not InputStream.EOF do
  begin
    BytesRead := InputStream.Read(Buffer, CHUNK_SIZE);
    Cipher.Update(Buffer, BytesRead, OutBuffer);
    OutputStream.Write(OutBuffer, OutLen);
  end;
  Cipher.Final(OutBuffer);
  OutputStream.Write(OutBuffer, OutLen);
end;
```

### 5. 内存分配

**症状**: 频繁 GC 或内存增长

**原因**: 临时对象过多

**优化**:

```pascal
// 复用缓冲区
var
  SharedBuffer: TBytes;

procedure ProcessData(const Data: TBytes);
begin
  if Length(SharedBuffer) < Length(Data) then
    SetLength(SharedBuffer, Length(Data));

  Move(Data[0], SharedBuffer[0], Length(Data));
  // 处理 SharedBuffer
end;
```

## 基准测试脚本

### 握手性能测试

```pascal
program benchmark_handshake;

const
  ITERATIONS = 100;

var
  i: Integer;
  StartTime: TDateTime;
  TotalMs: Double;
begin
  TotalMs := 0;

  for i := 1 to ITERATIONS do
  begin
    StartTime := Now;

    // 创建连接并握手
    Socket := ConnectTCP(Host, 443);
    Conn := Ctx.CreateConnection(Socket);
    (Conn as ISSLClientConnection).SetServerName(Host);
    Conn.Connect;
    Conn.Shutdown;
    CloseSocket(Socket);

    TotalMs := TotalMs + (Now - StartTime) * 86400000;
  end;

  WriteLn('Average handshake: ', (TotalMs / ITERATIONS):0:2, ' ms');
end.
```

### 加密吞吐测试

```pascal
program benchmark_encrypt;

const
  DATA_SIZE = 1024 * 1024;  // 1MB
  ITERATIONS = 100;

var
  Data: TBytes;
  StartTime: TDateTime;
  TotalMs: Double;
  Throughput: Double;
begin
  SetLength(Data, DATA_SIZE);
  FillChar(Data[0], DATA_SIZE, $AA);

  StartTime := Now;

  for i := 1 to ITERATIONS do
    Encrypted := AESEncrypt(Data, Key, IV);

  TotalMs := (Now - StartTime) * 86400000;
  Throughput := (DATA_SIZE * ITERATIONS / 1024 / 1024) / (TotalMs / 1000);

  WriteLn('Throughput: ', Throughput:0:2, ' MB/s');
end.
```

## 性能检查清单

### 连接性能

- [ ] 启用 Session 复用
- [ ] 使用连接池
- [ ] 避免频繁创建/销毁 Context
- [ ] 使用 TLS 1.3（更快握手）

### 加密性能

- [ ] 使用 AES-NI（硬件加速）
- [ ] 选择合适的密钥长度
- [ ] 使用流式处理大数据
- [ ] 避免不必要的内存复制

### 证书性能

- [ ] 使用 ECDSA 证书
- [ ] 缓存证书验证结果
- [ ] 预加载信任链
- [ ] 考虑 OCSP Stapling

### 内存性能

- [ ] 复用缓冲区
- [ ] 及时释放 OpenSSL 对象
- [ ] 使用 HeapTrc 检测泄漏
- [ ] 监控内存增长

## 性能目标参考

| 操作             | 目标时间   | 说明     |
| ---------------- | ---------- | -------- |
| TLS 1.3 握手     | < 50ms     | 本地网络 |
| TLS 1.2 握手     | < 100ms    | 本地网络 |
| Session 复用握手 | < 10ms     | 本地网络 |
| AES-256-GCM 加密 | > 500 MB/s | 现代 CPU |
| RSA 2048 签名    | < 5ms      | 现代 CPU |
| ECDSA P-256 签名 | < 1ms      | 现代 CPU |

## 相关文档

- `scripts/run_phase2_performance_baseline.sh` - 性能基准脚本
- `docs/test_reports/PHASE2_BASELINE_EXECUTION_SUMMARY.md` - 基准结果
- `docs/test_reports/PHASE2_PERFORMANCE_METRICS_TEMPLATE.md` - 指标模板
