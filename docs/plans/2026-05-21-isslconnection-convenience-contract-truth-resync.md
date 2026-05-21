# `ISSLConnection` Convenience Contract Truth Resync

## Goal

修复 `tests/scripts/test_isslconnection_convenience_surface_classification_contract.sh`
在后续 owner-path 收口后留下的假红，使 focused contract 重新对齐当前源码真相：

- `ReadString` / `WriteString` 仍是 `v1.x` convenience-core 文本 helper
- `SetTimeout` / `GetTimeout`
- `SetBlocking` / `GetBlocking`

其中 timeout / blocking 的 current shipped truth
已经不只是 builder-first，
还包括：

- 默认 runtime owner 已切到 `ISSLConnectionControl`
- `ISSLConnection` core 侧继续保留 convenience mirror / override

## Scope

本批只修：

- focused contract wording
- focused batch plan record
- 台账同步

本批不做：

- 不改 public Pascal surface
- 不改 backend 实现
- 不重开更大的 `TSSLConfig` extraction
- 不重跑重型 Pascal compile gate

## Why This Batch

当前源码与主文档其实已经完成后续收口：

- `src/fafafa.ssl.base.pas`
  已把 timeout / blocking 注释升级为
  builder-first + `ISSLConnectionControl` owner-path + convenience override
- `docs/reference/INTERFACE_DESIGN_V2.md`
  已把这两组方法从
  “仍在 `ISSLConnection` 上的 convenience 面”
  进一步明确为
  “默认 owner 在 `ISSLConnectionControl`，core 侧保留 mirror”

但 focused contract 还停在更早一轮的旧措辞，
导致它把已经更完整的源码/文档判成失败。

## Files

- Add: `docs/plans/2026-05-21-isslconnection-convenience-contract-truth-resync.md`
- Update: `tests/scripts/test_isslconnection_convenience_surface_classification_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps

1. 记录这次 contract truth resync 的边界。
2. 更新 focused contract：
   - source comment 断言改为匹配当前
     `ISSLConnectionControl` owner-path truth
   - design doc 断言改为匹配当前
     `ISSLConnectionControl` 迁移位置
3. 重跑 focused contract，确认旧假红消失。
4. 同步总台账。

## Verification

1. `bash -n tests/scripts/test_isslconnection_convenience_surface_classification_contract.sh`
2. `bash tests/scripts/test_isslconnection_convenience_surface_classification_contract.sh`
3. `git diff --check`
4. `git status --short`

## Risks

- 不要把 contract 修成“只认旧 builder-first 说法”，否则会把后续 owner-path 收口重新打回假红。
- 不要把 scope 扩成 `ISSLConnection` 拆接口；这一批只是 workflow truth repair。
