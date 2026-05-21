# 2026-05-21 Server Default VerifyMode Baseline

## Goal

把 server 高入口的默认 verify 基线，从当前混杂着 client 默认值历史包袱的
`[sslVerifyPeer]`
收成更符合普通单向 TLS server 直觉的
no-verify baseline，
同时保留：

- client 默认仍是 verify-peer
- builder 上显式 `.WithVerifyPeer` / `.WithMutualTLS(...)`
  的 server 意图不被吞掉

## Scope

- 修改：
  - `src/fafafa.ssl.factory.pas`
  - `src/fafafa.ssl.pas`
  - `src/fafafa.ssl.context.builder.pas`
  - `src/fafafa.ssl.openssl.backed.pas`
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `src/fafafa.ssl.mbedtls.lib.pas`
  - `src/fafafa.ssl.wolfssl.lib.pas`
  - `src/fafafa.ssl.winssl.lib.pas`
  - `docs/reference/API_REFERENCE.md`
  - `docs/guides/OCSP_USAGE_GUIDE.md`
  - `tests/contract/test_server_helper_verifymode_default_entry.pas`
  - `tests/scripts/test_server_helper_verifymode_default_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`
- 不修改：
  - 不重构 `TSSLVerifyMode` public enum
  - 不要求调用方额外设置新的 public `VerifyModeExplicit` 字段
  - 不在这一批里重开整个 mTLS/pinned-cert/application-auth 设计线

## Architecture Truth

- 普通单向 TLS server：
  - 默认不校验客户端证书
- mTLS / client-cert verification：
  - 必须显式表达
  - builder:
    `.WithMutualTLS(...)`
    或 `.WithVerifyPeer` + CA roots
  - config/direct-context:
    `VerifyMode := [sslVerifyPeer, ...]`
    并配置 CA / system roots
- client 默认仍应保持：
  - `sslVerifyPeer`

当前真正的设计难点是：

- `TSSLContextBuilder`
  在 build 之前并不知道最终是 client 还是 server
- 但当前只有一份
  `FVerifyMode`
  默认值

所以这批采用两层修法：

1. factory / direct-library / convenience `CreateDefaultConfig`
   按 `sslCtxServer`
   解析出 ordinary one-way TLS baseline = `[]`
2. builder
   增加内部
   “verify mode 是否显式设置过”
   状态，
   让：
   - default `BuildClient` -> `[sslVerifyPeer]`
   - default `BuildServer` -> `[]`
   - 显式 `.WithVerifyPeer`
     不会被 server path 吞掉

## Steps

1. 先把现有 helper/default contract 改成新的目标真相，并补上：
   - client 默认仍是 verify-peer
   - server 默认是 no-verify
   - builder JSON/INI round-trip 后仍保留这层默认分流语义
2. 最小修改源码：
   - factory/direct-library server raw context 默认 verify 解析
   - builder 内部 explicit-state + build/validate/import/export/merge 对齐
3. 更新高入口文档：
   - API reference
   - OCSP guide
4. 跑 focused contract + 相关 builder import/export / snapshot / validation suite
5. 更新 `task_plan.md` / `findings.md` / `progress.md`

## Commands

```bash
bash -n tests/scripts/test_server_helper_verifymode_default_contract.sh
bash tests/scripts/test_server_helper_verifymode_default_contract.sh
mkdir -p tmp/config_import_export && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_import_export -FEtmp/config_import_export -otmp/config_import_export/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/config_import_export/test_config_import_export
mkdir -p tmp/config_snapshot_clone && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_snapshot_clone -FEtmp/config_snapshot_clone -otmp/config_snapshot_clone/test_config_snapshot_clone tests/config/test_config_snapshot_clone.pas && ./tmp/config_snapshot_clone/test_config_snapshot_clone
mkdir -p tmp/config_validation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_validation -FEtmp/config_validation -otmp/config_validation/test_config_validation tests/config/test_config_validation.pas && ./tmp/config_validation/test_config_validation
git diff --check
```

## Expected Outcome

- `CreateDefaultConfig(sslCtxServer)` 返回 ordinary one-way TLS server baseline。
- raw `CreateContext(sslCtxServer, ...)`
  /
  `CreateServerContext(...)`
  /
  `QuickServer(...)`
  /
  default builder `BuildServer`
  重新回到同一套 server baseline。
- client 默认不被带歪。
- builder export/import/clone/reset/merge
  不再把
  client-default verify semantics
  错投到 server 默认路径上。
