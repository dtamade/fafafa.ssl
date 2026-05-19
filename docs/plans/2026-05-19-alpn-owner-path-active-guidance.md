# ALPN Owner Path Active Guidance

## Goal

收口活跃 guide / example 中 `GetSelectedALPNProtocol` 的 owner-path 漂移，明确当前 shipped source truth：

- `ISSLConnection.GetSelectedALPNProtocol`
  - 当前只作为 `v1.x` compatibility-core mirror 保留
  - 源码声明已经是编译期 `deprecated`
- 新代码应优先通过：
  - `ISSLConnectionInfo.GetSelectedALPNProtocol`

## Why This Batch

虽然 `API_REFERENCE` / `INTERFACE_DESIGN_V2` 已经把 ALPN owner truth 讲清楚，但活跃指导面仍有残口：

- `docs/guides/WINSSL_USER_GUIDE.md`
  - 仍把 “GetSelectedALPNProtocol” 写成普通协商结果入口
- `examples/https_server/https_server_alpn.pas`
  - 仍直接从 `ISSLConnection` 调用 `GetSelectedALPNProtocol`

这会继续把已经 demote 的 mirror surface 误教成普通主路径。

## Scope

- 不改 runtime 行为
- 不改 public Pascal signature
- 只修活跃 guide/example guidance 与 focused contract

## Files

- Add: `docs/plans/2026-05-19-alpn-owner-path-active-guidance.md`
- Add: `tests/scripts/test_alpn_owner_path_active_guidance_contract.sh`
- Update: `docs/guides/WINSSL_USER_GUIDE.md`
- Update: `examples/https_server/https_server_alpn.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps

1. 写 focused shell contract，锁定 ALPN owner-path truth。
2. 修正活跃 WinSSL guide 表述。
3. 修正 ALPN server example，优先通过 `ISSLConnectionInfo` 获取协商结果。
4. 运行 focused contract 与 example 编译。
5. 同步 planning files，提交并推送。

## Verification

1. `bash -n tests/scripts/test_alpn_owner_path_active_guidance_contract.sh`
2. `bash tests/scripts/test_alpn_owner_path_active_guidance_contract.sh`
3. `fpc -B -Fu./src -Fu./examples -FUtmp/example_https_server_alpn -FEtmp/example_https_server_alpn -otmp/example_https_server_alpn/https_server_alpn examples/https_server/https_server_alpn.pas`
4. `git diff --check`

## Risks

- 不要把 scope 扩大到整个 ALPN runtime/backends。
- 不要去改 design docs 里用于解释 deprecation 的必要提及。
- 不要把 intentional compatibility fallback helper 和普通活跃 example 混为一类。
