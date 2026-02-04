# fafafa.ssl 架构迭代升级路线图

**创建日期**: 2026-02-05
**当前版本**: v1.0.0-rc
**目标版本**: v2.0.0

---

## 一、当前架构分析

### 1.1 现有架构优点

```
┌─────────────────────────────────────┐
│          用户应用层                   │
├─────────────────────────────────────┤
│      fafafa.ssl 统一接口层           │  ← ISSLContext, ISSLConnection
├─────────────────────────────────────┤
│     TBaseSSLConnection 基类          │  ← 21 个抽象方法，~50 通用实现
├─────────┬─────────┬─────────┬───────┤
│ OpenSSL │ WolfSSL │ MbedTLS │WinSSL │  ← 4 个后端实现
└─────────┴─────────┴─────────┴───────┘
```

**已完成优化**:
- ✅ TBaseSSLConnection 抽象基类（减少 ~800 行重复代码）
- ✅ TBytesView 零拷贝视图类型
- ✅ TRandomPool 随机数缓存池
- ✅ TAESGCMContextPool 加密上下文池
- ✅ 统一错误处理和日志系统

### 1.2 当前瓶颈

| 领域 | 问题 | 影响 |
|------|------|------|
| 内存分配 | TBytes 频繁分配/释放 | 性能开销、GC 压力 |
| 接口调用 | 虚方法调用开销 | 热路径性能损失 |
| 锁竞争 | TCriticalSection 全局锁 | 高并发场景瓶颈 |
| 错误处理 | 异常机制开销 | 错误路径性能差 |

---

## 二、升级路线

### Phase 1: 零拷贝 I/O 优化 (v1.1.0)

**目标**: 消除数据传输中的不必要拷贝

#### 1.1 引入 Buffer Pool（内存池）

```pascal
type
  { 可重用缓冲区 }
  TPooledBuffer = record
    Data: PByte;
    Capacity: Integer;
    Length: Integer;
    RefCount: Integer;  // 引用计数
  end;

  { 缓冲区池 }
  TBufferPool = class
  private
    FSmallBuffers: array of TPooledBuffer;  // 4KB
    FMediumBuffers: array of TPooledBuffer; // 16KB
    FLargeBuffers: array of TPooledBuffer;  // 64KB
  public
    function Acquire(ASize: Integer): TPooledBuffer;
    procedure Release(var ABuffer: TPooledBuffer);
  end;
```

**收益**:
- 减少 90%+ 内存分配
- 读取吞吐量提升 30-50%

#### 1.2 Scatter/Gather I/O 支持

```pascal
type
  TIOVector = record
    Base: PByte;
    Length: Integer;
  end;
  TIOVectors = array of TIOVector;

  ISSLConnectionV2 = interface(ISSLConnection)
    { 向量化读写 - 一次系统调用处理多个缓冲区 }
    function ReadV(const ABuffers: TIOVectors): Integer;
    function WriteV(const ABuffers: TIOVectors): Integer;
  end;
```

**收益**:
- 减少系统调用次数
- HTTP/2 多路复用性能提升

---

### Phase 2: 无锁并发优化 (v1.2.0)

**目标**: 降低高并发场景的锁竞争

#### 2.1 Lock-Free Ring Buffer

```pascal
type
  { 无锁环形缓冲区 }
  TLockFreeRingBuffer = record
  private
    FBuffer: array of Byte;
    FCapacity: Integer;
    FHead: Integer;  // 原子操作
    FTail: Integer;  // 原子操作
  public
    function TryWrite(const AData: PByte; ALen: Integer): Boolean;
    function TryRead(AData: PByte; ALen: Integer): Boolean;
    function Available: Integer;
  end;
```

**适用场景**:
- 单生产者单消费者 (SPSC)
- 网络 I/O 缓冲
- 日志写入

#### 2.2 分片锁 (Sharded Locks)

```pascal
type
  { 分片会话缓存 }
  TShardedSessionCache = class
  private
    const SHARD_COUNT = 16;
    FShards: array[0..SHARD_COUNT-1] of record
      Lock: TRTLCriticalSection;
      Cache: TStringList;  // SessionID -> Session
    end;
    function GetShardIndex(const ASessionID: string): Integer; inline;
  public
    procedure Store(const ASessionID: string; ASession: ISSLSession);
    function Retrieve(const ASessionID: string): ISSLSession;
  end;
```

**收益**:
- 并发吞吐量提升 8-16 倍
- 连接建立延迟降低 40%

---

### Phase 3: 异步 I/O 支持 (v1.3.0)

**目标**: 支持高效的异步/非阻塞模式

#### 3.1 事件驱动接口

```pascal
type
  TSSLEventType = (
    sslEvtReadable,     // 可读
    sslEvtWritable,     // 可写
    sslEvtConnected,    // 连接完成
    sslEvtHandshakeDone,// 握手完成
    sslEvtError,        // 错误
    sslEvtClosed        // 关闭
  );

  TSSLEventCallback = procedure(AConnection: ISSLConnection;
    AEvent: TSSLEventType; AUserData: Pointer) of object;

  ISSLAsyncConnection = interface(ISSLConnection)
    procedure SetEventCallback(ACallback: TSSLEventCallback; AUserData: Pointer);
    procedure ProcessEvents;  // 处理待处理事件
    function GetPendingEvents: TSSLEventTypes;
  end;
```

#### 3.2 协程/纤程支持 (可选)

```pascal
type
  { 协程友好的 SSL 连接 }
  ISSLCoroutineConnection = interface(ISSLConnection)
    { 异步读取 - 返回 Future }
    function ReadAsync(ACount: Integer): IFuture<TBytes>;
    { 异步写入 }
    function WriteAsync(const AData: TBytes): IFuture<Integer>;
    { 异步握手 }
    function HandshakeAsync: IFuture<Boolean>;
  end;
```

---

### Phase 4: Result<T, E> 错误处理 (v1.4.0)

**目标**: 消除异常开销，提供类型安全的错误处理

#### 4.1 泛型 Result 类型

```pascal
type
  { Rust 风格的 Result 类型 }
  generic TResult<T, E> = record
  private
    FValue: T;
    FError: E;
    FIsOk: Boolean;
  public
    class function Ok(const AValue: T): TResult; static;
    class function Err(const AError: E): TResult; static;

    function IsOk: Boolean; inline;
    function IsErr: Boolean; inline;
    function Unwrap: T;  // 失败时 panic
    function UnwrapOr(const ADefault: T): T;
    function Map<U>(AFunc: specialize TMapFunc<T, U>): specialize TResult<U, E>;
  end;

  { SSL 专用 Result }
  TSSLResult<T> = specialize TResult<T, TSSLError>;
```

#### 4.2 新 API 设计

```pascal
type
  ISSLConnectionV3 = interface
    { 返回 Result 而非抛异常 }
    function TryRead(var ABuffer; ACount: Integer): TSSLResult<Integer>;
    function TryWrite(const ABuffer; ACount: Integer): TSSLResult<Integer>;
    function TryHandshake: TSSLResult<Boolean>;

    { 链式操作 }
    function ReadThen(ACount: Integer): TSSLReadBuilder;
  end;
```

**收益**:
- 错误路径零开销
- 编译时错误检查
- 更好的代码可读性

---

### Phase 5: 编译时优化 (v2.0.0)

**目标**: 利用泛型和内联消除运行时开销

#### 5.1 泛型后端适配器

```pascal
type
  { 编译时确定的后端适配器 }
  generic TSSLAdapter<TBackend> = class
  public
    class function Connect(const AHost: string; APort: Word): TSSLResult<ISSLConnection>; static; inline;
    class function CreateContext(AType: TSSLContextType): TSSLResult<ISSLContext>; static; inline;
  end;

  { 具体后端 }
  TOpenSSLAdapter = specialize TSSLAdapter<TOpenSSLBackend>;
  TWinSSLAdapter = specialize TSSLAdapter<TWinSSLBackend>;
```

#### 5.2 内联关键路径

```pascal
type
  TSSLConnection = class
  public
    { 内联热路径方法 }
    function Read(var ABuffer; ACount: Integer): Integer; inline;
    function Write(const ABuffer; ACount: Integer): Integer; inline;
    function Available: Integer; inline;
  end;
```

---

## 三、兼容性策略

### 3.1 版本兼容

| 版本 | 兼容性 | 说明 |
|------|--------|------|
| v1.x | 完全向后兼容 | 现有 API 保持不变 |
| v2.0 | API 演进 | 新增 V2/V3 接口，旧接口标记 deprecated |
| v3.0 | 破坏性更改 | 移除旧 API，仅保留新设计 |

### 3.2 迁移路径

```pascal
// v1.x 用法（保持支持）
LConn := TSSLFactory.CreateContext(sslClient).CreateConnection(ASocket);
LConn.Handshake;
LData := LConn.ReadString;

// v2.x 推荐用法
LResult := TSSLFactory.Connect(AHost, APort);
if LResult.IsOk then
begin
  LConn := LResult.Unwrap;
  LData := LConn.ReadThen(1024).Decode(TEncoding.UTF8).Unwrap;
end;
```

---

## 四、优先级排序

| 阶段 | 特性 | 复杂度 | 收益 | 优先级 |
|------|------|--------|------|--------|
| Phase 1 | Buffer Pool | 中 | 高 | P0 |
| Phase 1 | Scatter/Gather I/O | 中 | 中 | P1 |
| Phase 2 | Lock-Free Ring Buffer | 高 | 高 | P1 |
| Phase 2 | Sharded Locks | 中 | 高 | P0 |
| Phase 3 | 异步 I/O | 高 | 高 | P1 |
| Phase 4 | Result<T,E> | 中 | 中 | P2 |
| Phase 5 | 泛型优化 | 高 | 中 | P2 |

---

## 五、基准测试目标

| 指标 | 当前值 | v1.5 目标 | v2.0 目标 |
|------|--------|-----------|-----------|
| 握手延迟 (TLS 1.3) | ~15ms | ~10ms | ~8ms |
| 吞吐量 (单连接) | ~500 MB/s | ~700 MB/s | ~900 MB/s |
| 并发连接 (同时) | ~10K | ~50K | ~100K |
| 内存/连接 | ~64KB | ~32KB | ~16KB |
| 随机数生成 | 200K ops/s | 500K ops/s | 1M ops/s |

---

## 六、实施建议

### 6.1 立即可行 (本周)

1. **Buffer Pool 原型**
   - 基于现有 TRandomPool 设计
   - 先在读取路径试点

2. **分片会话缓存**
   - 替换现有单锁缓存
   - 无 API 变更

### 6.2 短期 (1-2 周)

1. **TBytesView 扩展**
   - 添加 WriteView 支持
   - 优化 Read 返回值

2. **基准测试框架完善**
   - 自动化性能回归检测
   - 对比测试报告

### 6.3 中期 (1 个月)

1. **Lock-Free Ring Buffer**
   - 用于网络 I/O 缓冲
   - 日志异步写入

2. **Result<T,E> 实验**
   - 新模块试点
   - 收集反馈

---

*文档版本: 1.0*
*最后更新: 2026-02-05*
