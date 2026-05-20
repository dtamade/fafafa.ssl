# 2026-05-21 C-Library Session Reuse Owner Truth

## Goal

把 `MbedTLS` / `WolfSSL` 在连接侧的 session reuse 语义继续收紧到更接近真实 owner path：

- `MbedTLS`
  - 证明“真实反序列化出来的 session 注入连接”仍然只表示
    `configured session`
    而不是
    `observed resumed handshake`
- `WolfSSL`
  - 证明“真实反序列化出来的 session 注入连接”后，
    `ISSLSessionResumption.IsSessionReused`
    继续读取 native `wolfSSL_session_reused(...)`
    真值

同时把这条静态/focused 结论记入台账，避免之后又把
`session injected`
和
`session actually reused`
混成一层。

## Scope

- 不在本批承诺：
  - 本地直接拿到公网/真实服务端的 resumed-handshake 运行证明
  - WinSSL runtime 复用真值重开
  - 把所有 backend 都拉进同一轮运行合同
- 不重开：
  - `session metadata truth`
    旧 lane
  - `deserialize metadata completeness`
    旧 lane
  - `SetSession(...)` 提前误报 `IsSessionReused=True`
    旧 lane
- 只收以下缺口：
  1. `MbedTLS` 现有 connection reused contract 不再只吃 mock session，要吃真实 `Deserialize(...)` 出来的 native session
  2. `WolfSSL` 新增 owner-path focused contract，锁住：
     - `Deserialize(...) -> SetSession(...)`
       仍会把 native session handle 注入连接
     - `IsSessionReused`
       继续读取 native `wolfSSL_session_reused(...)`
       真值，而不是把 injected session 当成 observed reuse
  3. 记录 `MbedTLS` 当前 local-header truth：
     - 有 `mbedtls_ssl_set_session(...)`
     - 但当前 public helper surface 没有像
       `SSL_session_reused`
       /
       `wolfSSL_session_reused`
       这样的直接 getter

## Files

- `tests/test_mbedtls_connection_session_reused_contract.pas`
- `tests/test_wolfssl_connection_session_reused_contract.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `SetSession(...)`
  的 owner 语义仍然只是：
  - 为下一次握手配置一个“可尝试恢复”的 session
- `IsSessionReused`
  的 owner 语义仍然必须是：
  - 当前连接/握手是否**实际命中**恢复路径
- 所以这批 focused proof 的关键不是“session 能不能放进去”，而是：
  - `session injected`
    和
    `session reused observed`
    不能混成同一个布尔值
- `OpenSSL` / `WolfSSL`
  这条线的真值来自 native reused getter
- `MbedTLS`
  当前 local header truth
  只明确给了：
  - `mbedtls_ssl_set_session`
  - `mbedtls_ssl_get_session`
  - `mbedtls_ssl_session_load/save`
  但没有对称的 public reused getter；
  因而当前连接侧 truth
  仍只能保守停在：
  - 配置 session 不等于 observed reuse

## Steps

1. 先把 `MbedTLS` focused contract 从 mock session 收紧成真实反序列化 session。
2. 新增 `WolfSSL` focused owner-path contract：
   - 真实 `Deserialize(...)` session
   - owner `ISSLSessionResumption.SetSession(...)`
   - native `wolfSSL_session_reused(...)`
     真值切换
3. focused 运行：
   - `tests/test_mbedtls_connection_session_reused_contract.pas`
   - `tests/test_wolfssl_connection_session_reused_contract.pas`
   - `tests/scripts/test_session_reused_semantic_truth_contract.sh`
   - `tests/scripts/test_isslsessionresumption_runtime_residual_classification_contract.sh`
4. `git diff --check`
5. 更新 `task_plan.md` / `findings.md` / `progress.md`

## Commands

```bash
mkdir -p tmp/test_mbedtls_connection_session_reused_contract
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_mbedtls_connection_session_reused_contract \
  -FEtmp/test_mbedtls_connection_session_reused_contract \
  -otmp/test_mbedtls_connection_session_reused_contract/test_mbedtls_connection_session_reused_contract \
  tests/test_mbedtls_connection_session_reused_contract.pas

mkdir -p tmp/test_wolfssl_connection_session_reused_contract
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_wolfssl_connection_session_reused_contract \
  -FEtmp/test_wolfssl_connection_session_reused_contract \
  -otmp/test_wolfssl_connection_session_reused_contract/test_wolfssl_connection_session_reused_contract \
  tests/test_wolfssl_connection_session_reused_contract.pas

bash -n tests/scripts/test_session_reused_semantic_truth_contract.sh
bash tests/scripts/test_session_reused_semantic_truth_contract.sh

bash -n tests/scripts/test_isslsessionresumption_runtime_residual_classification_contract.sh
bash tests/scripts/test_isslsessionresumption_runtime_residual_classification_contract.sh
```
