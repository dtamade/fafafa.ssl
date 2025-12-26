# ADR-003: 安全优先设计原则

## 状态
已接受 (2025-12-26)

## 上下文
作为 SSL/TLS 库，安全是首要关注点。我们需要确保：
- 敏感数据（密钥、密码）得到保护
- 防止常见的密码学攻击
- 废弃协议/算法得到警告

## 决策

### 1. 安全内存清零 (CVE-FSSSL-001 修复)

**问题**：编译器可能优化掉对"死代码"的内存清零操作。

**解决方案**：
```pascal
procedure SecureZeroMemory(P: Pointer; Size: NativeUInt);
asm
  {$IFDEF CPUX86_64}
  mov rcx, P
  mov rdx, Size
  xor eax, eax
@@loop:
  test rdx, rdx
  jz @@done
  mov byte [rcx], al
  inc rcx
  dec rdx
  jmp @@loop
@@done:
  {$ENDIF}
end;
```

**应用位置**：
- `TSecureString.ZeroMemory`
- `TSecureBytes.ZeroMemory`
- 所有密码/密钥清理代码

### 2. 常量时间比较 (CVE-FSSSL-003 修复)

**问题**：普通字符串比较会提前退出，导致时序侧信道攻击。

**解决方案**：
```pascal
function SecureCompare(const A, B: TBytes): Boolean;
var
  I, LMaxLen: Integer;
  LDiff: Byte;
begin
  LDiff := 0;
  LMaxLen := Max(Length(A), Length(B));

  for I := 0 to LMaxLen - 1 do
  begin
    // 常量时间访问
    if I < Length(A) then ByteA := A[I] else ByteA := 0;
    if I < Length(B) then ByteB := B[I] else ByteB := 0;
    LDiff := LDiff or (ByteA xor ByteB);
  end;

  Result := (LDiff = 0) and (Length(A) = Length(B));
end;
```

### 3. 废弃协议警告 (SEC-002 修复)

**问题**：用户可能不知道某些协议已被废弃。

**解决方案**：
```pascal
procedure SetProtocolVersions(AVersions: TSSLProtocolVersions);
begin
  if sslProtocolSSL2 in AVersions then
    TSecurityLog.Warning('SSL 2.0 已废弃且不安全');
  if sslProtocolSSL3 in AVersions then
    TSecurityLog.Warning('SSL 3.0 存在 POODLE 漏洞');
  if sslProtocolTLS10 in AVersions then
    TSecurityLog.Warning('TLS 1.0 已废弃');
  if sslProtocolTLS11 in AVersions then
    TSecurityLog.Warning('TLS 1.1 已废弃');
end;
```

### 4. 安全审计日志

所有安全相关操作记录到 `TSecurityLog`：
- 证书加载/验证
- 私钥操作
- 密钥存储访问
- 协议协商

## 后果

### 正面
- 防止已知密码学攻击
- 符合行业安全最佳实践
- 用户收到安全风险警告

### 负面
- 汇编代码降低可移植性
- 常量时间操作稍慢
- 警告可能产生日志噪音

## 替代方案

1. **依赖 OpenSSL 安全功能**：
   - 否决：不适用于 WinSSL 后端

2. **禁止废弃协议**：
   - 否决：需要兼容旧系统

3. **可选安全检查**：
   - 否决：安全不应是可选的

## 参考
- `src/fafafa.ssl.memutils.pas` - 安全内存操作
- `src/fafafa.ssl.secure.pas` - 安全存储和比较
- `src/fafafa.ssl.logging.pas` - 安全日志
