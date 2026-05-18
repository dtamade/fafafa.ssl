# 2026-05-18 WinSSL Integration-Multi HTTP Status Stability

## Goal

把 `tests/winssl/test_winssl_integration_multi.pas` 的外部 HTTP 状态断言从脆弱的 “必须 2xx/3xx” 收紧为更符合集成测试真实目标的 “状态行可解析且不是 5xx”，避免 `api.github.com` 这类外部服务因为鉴权/限流/策略变化而把本来成功的 WinSSL 握手与收发链路误判成失败。

## Scope

- 不修改 WinSSL backend 生产代码。
- 不重开 native probe / shared reconnect / session-resumption 旧 lane。
- 只修 `integration_multi` 对外部 HTTP 响应状态的错误假设，并把它锁成 focused contract。

## Files

- `tests/winssl/test_winssl_integration_multi.pas`
- `tests/scripts/test_winssl_integration_multi_http_status_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- 这条测试的核心目标是验证：
  - TCP connect
  - TLS handshake
  - SNI
  - HTTP request send / response receive
  - application-layer response framing
- 对 `api.github.com` 这种外部服务来说，`401/403/404` 之类响应并不表示 WinSSL transport path 失败。
- 真正应该阻断集成测试的，是：
  - 状态行不可解析
  - 5xx server failure
  - 握手/收发路径本身失败

## Steps

1. 给 `test_winssl_integration_multi.pas` 增加 HTTP status parser helper。
2. 把旧的 `2xx/3xx` 字符串匹配断言改成：
   - `响应状态码可解析`
   - `响应状态码不是 5xx`
3. 新增 focused contract，禁止回归到 `2xx/3xx`-only 假设。
4. 做 focused Win64 compile 与 `git diff --check`。
5. push 后重跑 GitHub Windows runner，确认 broader suite 不再因为 `api.github.com` 断言失败而红。

## Commands

```bash
bash -n tests/scripts/test_winssl_integration_multi_http_status_contract.sh
bash tests/scripts/test_winssl_integration_multi_http_status_contract.sh
mkdir -p tmp/winssl_integration_multi_http_status_win64
fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/winssl_integration_multi_http_status_win64 \
  -FEtmp/winssl_integration_multi_http_status_win64 \
  -otmp/winssl_integration_multi_http_status_win64/test_winssl_integration_multi.exe \
  tests/winssl/test_winssl_integration_multi.pas
git diff --check
gh workflow run wave-b-b2-manual.yml --ref master -f run_id=<custom-id>
```

## Execution Result

- RED:
  - GitHub Windows live run `26043523820` 里 `WinSSL Integration Tests (Multi-Scenario)` 失败点已经压清：
    - `api.github.com` 的 TCP/TLS/send/receive/status-line 都 PASS
    - 只有“响应状态码正常 (2xx/3xx)”这条断言 FAIL
- GREEN:
  - 本地 follow-up 已落地：
    - 加入 `TryExtractHTTPStatusCode(...)`
    - 改成 `响应状态码可解析` + `响应状态码不是 5xx`
    - 新 focused contract 已补齐
    - focused Win64 compile / `git diff --check` 通过
- PENDING:
  - 还需要新的 GitHub Windows rerun，把这条 flaky assertion 修正后的 broader suite 结果重新取证
