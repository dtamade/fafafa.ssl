# 2026-05-21 Server Validation VerifyMode Classification

## Goal

把 `TSSLContextBuilder.ValidateClient` / `ValidateServer` 在 `VerifyMode` 上的验证口径拆开，避免普通单向 TLS server 继续继承 client-only 的
`Certificate verification is disabled - insecure for production`
警告，同时保留 server 在显式开启客户端证书校验但未配置 CA 时的提醒。

## Scope

- 修改：
  - `src/fafafa.ssl.context.builder.pas`
  - `docs/reference/API_REFERENCE.md`
  - `tests/contract/test_server_validation_verifymode_classification_entry.pas`
  - `tests/scripts/test_server_validation_verifymode_classification_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`
- 不修改：
  - 不改变 `BuildServer` / `CreateDefaultConfig(sslCtxServer)` 当前默认 `VerifyMode`
  - 不重构 `.WithVerifyNone` / `.WithMutualTLS(...)` public surface
  - 不在这一批里决定 server 默认是否仍应保持 `[sslVerifyPeer]`

## Architecture Truth

- client path：
  - 未启用 `sslVerifyPeer`
    仍应视为
    no-verify
    并给出生产风险 warning
- server path：
  - 普通单向 TLS server
    不校验客户端证书
    本身不是
    “禁用对端证书验证的 client 风险”
  - 因而显式
    `.WithVerifyNone`
    /
    `VerifyMode := []`
    不应再继承 client-only warning
- server 侧真正需要提醒的是：
  - 如果显式启用了
    `sslVerifyPeer`
    但没有配置 CA
    则仍应提示：
    `Client verification enabled but no CA certificates configured`

## Steps

1. 新增 focused contract，先固定当前 drift：
   - client `WithVerifyNone` 仍要报 insecure warning
   - server `WithVerifyNone` 不应再报这条 warning
   - server `WithVerifyPeer` 但无 CA 时仍保留现有 server-specific warning
2. 最小修改 `ValidateCommonBuilderSettings(...)`，按 `AForServer` 分流 verify warning。
3. 在 `API_REFERENCE` 补一条当前 validation truth。
4. 更新 `task_plan.md` / `findings.md` / `progress.md`。
5. 跑 focused contract、相关 validation suite 与 `git diff --check` 收口。

## Commands

```bash
bash -n tests/scripts/test_server_validation_verifymode_classification_contract.sh
bash tests/scripts/test_server_validation_verifymode_classification_contract.sh
mkdir -p tmp/config_validation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_validation -FEtmp/config_validation -otmp/config_validation/test_config_validation tests/config/test_config_validation.pas && ./tmp/config_validation/test_config_validation
git diff --check
```

## Expected Outcome

- `ValidateClient`
  继续对 no-verify 给出生产风险 warning。
- `ValidateServer`
  不再把普通单向 TLS server 误报成 client-style insecure configuration。
- server validation 仍能保留：
  - mTLS / client verification
    缺 CA
    的提醒
- 路线图上会更清楚地暴露下一层问题：
  - server 默认 `VerifyMode`
    设计本身是否还合理
