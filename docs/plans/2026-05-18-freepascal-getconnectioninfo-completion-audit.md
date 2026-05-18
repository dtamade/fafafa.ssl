# FreePascal `GetConnectionInfo` Completion Audit

## Goal

对当前 `GetConnectionInfo` implementation-completeness 主线做一次真正的 FreePascal completion audit，确认这个 backend 是否还存在必须单独补的 low-level truth，还是已经可以依赖 shared TLS 1.3 truth 正式收口。

## Scope

- `src/fafafa.ssl.freepascal.connection.pas`
- `src/fafafa.ssl.freepascal.session.pas`
- `tests/test_freepascal_server_accept_skeleton.pas`
- `tests/test_freepascal_client_session_resumption.pas`
- `tests/scripts/test_freepascal_connectioninfo_completion_contract.sh`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不重开 `OpenSSL` / `WolfSSL` / `MbedTLS` 已完成批次
- 不直接跳进更大的 owner / deprecation wording 改造
- 不重跑整条重型 gate

## Audit Questions

1. `FreePascal` backend 是否存在 dedicated `GetConnectionInfo` override？
2. 如果没有，它内部是否仍掌握 backend-local `CipherSuiteId` / `KeySize` / `MacSize` 真相，但 shared path 没吃到？
3. client / server 两条 runtime path 是否都把标准 TLS 1.3 suite 名称喂给了 shared `GetConnectionInfo`？
4. 当前是否还存在高价值未补的 backend-local truth？

## Planned Evidence

1. 静态 contract：
   - `FreePascal` 不应长出单独 `GetConnectionInfo` override
   - client / server path 都应把 `FCipherName` 设为 `TLS13CipherSuiteToString(...)`
   - session/resumption path 应保留 cipher-suite word truth
2. runtime proof：
   - server skeleton path:
     - `GetConnectionInfo.CipherSuiteId`
     - `KeySize`
     - `MacSize`
   - client initial/resumed path:
     - `CipherSuiteId`
     - `KeySize`
     - `MacSize`
     - `ServerName`
     - `IsResumed`
     - `SessionId`

## Verification

```bash
bash tests/scripts/test_freepascal_connectioninfo_completion_contract.sh
mkdir -p tmp/test_freepascal_server_accept_skeleton && fpc -B -Fu./src -Fu./tests -FUtmp/test_freepascal_server_accept_skeleton -FEtmp/test_freepascal_server_accept_skeleton -otmp/test_freepascal_server_accept_skeleton/test_freepascal_server_accept_skeleton tests/test_freepascal_server_accept_skeleton.pas && ./tmp/test_freepascal_server_accept_skeleton/test_freepascal_server_accept_skeleton
mkdir -p tmp/test_freepascal_client_session_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_client_session_resumption -FEtmp/test_freepascal_client_session_resumption -otmp/test_freepascal_client_session_resumption/test_freepascal_client_session_resumption tests/test_freepascal_client_session_resumption.pas && ./tmp/test_freepascal_client_session_resumption/test_freepascal_client_session_resumption
git diff --check
```

## Expected Outcome

- 若证据全部成立：
  - `FreePascal` backend 在当前 `GetConnectionInfo` 主线上可视为完成收口
  - 下一步应进入 route-level completion audit / next-route selection
- 若证据失败：
  - 失败点就是新的高价值 implementation gap，应在本批直接修掉

## Result

- 静态 audit 已确认：
  - `TFreePascalConnection` 没有 dedicated `GetConnectionInfo` override
  - `FreePascal` backend 当前只额外提供：
    - `DoGetConnectionInfoServerName`
  - client / server TLS 1.3 runtime path 都会把 negotiated suite truth 写成：
    - `FCipherName := TLS13CipherSuiteToString(...)`
  - session / resumption path 继续保留：
    - `FCipherSuite: Word`
- focused proof 已通过：
  - `bash tests/scripts/test_freepascal_connectioninfo_completion_contract.sh`
  - `tests/test_freepascal_server_accept_skeleton.pas`
  - `tests/test_freepascal_client_session_resumption.pas`
- 当前结论：
  - `FreePascal` 不像 `OpenSSL` / `WolfSSL` / `MbedTLS` 那样还需要单独再补一条 backend-local `GetConnectionInfo` helper 路径
  - shared `GetConnectionInfo` 已能吃到它当前提供的 TLS 1.3 suite-name truth，并推导出：
    - `CipherSuiteId`
    - `Hash`
    - `KeySize`
    - `MacSize`
  - 当前 `GetConnectionInfo` implementation-completeness 主线可视为基本收口
  - 默认下一步应切回 owner / deprecation wording route，而不是继续盲找 backend helper
