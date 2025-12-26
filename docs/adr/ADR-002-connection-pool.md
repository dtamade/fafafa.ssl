# ADR-002: SSL 连接池架构

## 状态
已接受 (2025-12-26)

## 上下文
TLS 握手是昂贵的操作（通常需要 1-3 个网络往返）。对于频繁连接到同一主机的应用程序，这会产生显著的延迟开销。

典型场景：
- 微服务间通信
- API 网关
- 数据库连接

## 决策
实现 `TSSLConnectionPool` 提供连接复用：

```pascal
ISSLConnectionPool = interface
  function Acquire(AContext: ISSLContext; const AHost: string; APort: Word;
    out AConnection: ISSLConnection): Boolean;
  procedure Release(AConnection: ISSLConnection);
  procedure CloseConnection(AConnection: ISSLConnection);
  procedure ClearHost(const AHost: string; APort: Word);
  procedure ClearAll;
end;
```

### 设计决策

1. **每主机池化**：
   - 连接按 `host:port` 分组
   - 每主机独立的最大连接数限制

2. **配置选项**：
   ```pascal
   TSSLPoolConfig = record
     MaxPoolSize: Integer;           // 默认: 10
     IdleTimeoutSec: Integer;        // 默认: 300秒
     ConnectTimeoutMs: Integer;      // 默认: 30000ms
     HealthCheckIntervalSec: Integer; // 默认: 60秒
   end;
   ```

3. **全局单例**：
   - `GlobalConnectionPool` 提供进程级共享池
   - 支持创建独立池实例

4. **后台清理**：
   - 独立线程定期清理过期连接
   - 健康检查确保连接可用性

## 后果

### 正面
- 减少 TLS 握手次数（对于复用连接）
- 降低延迟，提高吞吐量
- 线程安全的连接管理

### 负面
- 内存占用增加（保持空闲连接）
- 需要处理连接过期场景
- 增加复杂度

### 风险
- 连接泄漏（忘记 Release）
- 过期连接上操作失败
- 资源耗尽（达到 MaxPoolSize）

## 替代方案

1. **无池化**：每次创建新连接
   - 否决：TLS 握手开销太大

2. **会话恢复**：使用 TLS Session Tickets
   - 部分采用：与连接池互补

3. **HTTP/2 连接复用**：
   - 适用于 HTTP/2 场景，不适用于通用 TLS

## 使用示例

```pascal
var
  Pool: ISSLConnectionPool;
  Conn: ISSLConnection;
begin
  Pool := GlobalConnectionPool;

  if Pool.Acquire(Context, 'api.example.com', 443, Conn) then
  try
    Conn.Write(Request);
    Conn.Read(Response);
  finally
    Pool.Release(Conn);
  end;
end;
```

## 参考
- `src/fafafa.ssl.pool.pas` - 连接池实现
- `tests/test_connection_pool.pas` - 单元测试
