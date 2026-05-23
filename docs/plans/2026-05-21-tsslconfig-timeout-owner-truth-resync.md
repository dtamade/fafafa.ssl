# `TSSLConfig` Timeout Owner Truth Resync

## Goal

把
`TSSLConfig.HandshakeTimeout`
这条已经在
`ISSLConnection`
convenience-surface
主线中完成 owner-path 收口的事实，
同步回
factory /
direct-library
reject 文案、
高入口 example、
architecture 参考
以及 focused contracts，
避免调用方继续被旧文案导向
`ISSLConnection.SetTimeout(...)`
这条 compatibility mirror。

## Scope

本批只处理：

- timeout reject wording
- active guidance wording
- focused contracts / targeted tests
- 台账同步

本批不做：

- 不改 `TSSLConfig` record shape
- 不重开 `BufferSize` 设计
- 不改 backend runtime 行为
- 不重跑整仓重型 compile gate

## Why This Batch

当前这条线已经出现一处典型的后续收口漂移：

- `src/fafafa.ssl.base.pas`
  已明确：
  - runtime owner
    优先是
    `ISSLConnectionControl.SetTimeout(...)`
  - `ISSLConnection.SetTimeout(...)`
    仅保留为
    `v1.x`
    convenience mirror
- `docs/reference/API_REFERENCE.md`
  也已经记录了同样的推荐入口

但下列高入口面
仍在把用户导向旧路径：

- `src/fafafa.ssl.factory.pas`
- `src/fafafa.ssl.context.config.pas`
- `examples/example_factory_usage.pas`
- `docs/reference/ARCHITECTURE.md`
- 对应 focused contracts

## Files

- Add: `docs/plans/2026-05-21-tsslconfig-timeout-owner-truth-resync.md`
- Update: `src/fafafa.ssl.factory.pas`
- Update: `src/fafafa.ssl.context.config.pas`
- Update: `examples/example_factory_usage.pas`
- Update: `docs/reference/ARCHITECTURE.md`
- Update: `tests/scripts/test_tsslconfig_active_guidance_truth_contract.sh`
- Update: `tests/scripts/test_direct_library_connection_scope_clarification_contract.sh`
- Update: `tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
- Update: `tests/test_factory_connection_scope_clarification.pas`
- Update: `tests/test_freepascal_library_default_config_connection_scope_clarification.pas`
- Update: `tests/test_clibrary_library_default_config_connection_scope_clarification.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps

1. 先把 focused contracts / targeted tests 改成要求：
   - reject 文案提到
     `ISSLConnectionControl.SetTimeout(...)`
   - active guidance
     不再把 runtime override
     教成
     `ISSLConnection.SetTimeout(...)`
2. 运行 focused checks，拿到 RED。
3. 对 source/doc 做最小修复。
4. 重跑 focused checks。
5. 同步总台账。

## Verification

1. `bash -n tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
2. `bash -n tests/scripts/test_direct_library_connection_scope_clarification_contract.sh`
3. `bash -n tests/scripts/test_tsslconfig_active_guidance_truth_contract.sh`
4. `bash tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
5. `bash tests/scripts/test_direct_library_connection_scope_clarification_contract.sh`
6. `bash tests/scripts/test_tsslconfig_active_guidance_truth_contract.sh`
7. `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_factory_connection_scope_clarification/test_factory_connection_scope_clarification tests/test_factory_connection_scope_clarification.pas && ./tmp/test_factory_connection_scope_clarification/test_factory_connection_scope_clarification`
8. `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_freepascal_library_default_config_connection_scope_clarification/test_freepascal_library_default_config_connection_scope_clarification tests/test_freepascal_library_default_config_connection_scope_clarification.pas && ./tmp/test_freepascal_library_default_config_connection_scope_clarification/test_freepascal_library_default_config_connection_scope_clarification`
9. `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_clibrary_library_default_config_connection_scope_clarification/test_clibrary_library_default_config_connection_scope_clarification tests/test_clibrary_library_default_config_connection_scope_clarification.pas && ./tmp/test_clibrary_library_default_config_connection_scope_clarification/test_clibrary_library_default_config_connection_scope_clarification`
10. `git diff --check`

## Expected Outcome

- reject 文案和高入口指导面
  不再把
  `HandshakeTimeout`
  的 runtime owner
  说回旧的 core mirror
- `ISSLConnectionControl`
  这条 owner-path
  在
  source / docs / focused tests
  上重新说同一张图

## Execution Result

- PASS
- 当前 head 已经把 `HandshakeTimeout` 的 owner truth 固定在 `ISSLConnectionControl.SetTimeout(...)` / `TSSLConnector.WithTimeout(...)` / `TSSLAcceptor.WithTimeout(...)` 这条线，现有 focused contracts 也全部通过。
- 实际可用的 timeout / direct-library contract 组合如下：
  - `bash tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
  - `bash tests/scripts/test_tsslconfig_active_guidance_truth_contract.sh`
  - `bash tests/scripts/test_direct_library_connection_scope_clarification_contract.sh`
  - `bash tests/scripts/test_connector_timeout_safety_contract.sh`
  - `bash tests/scripts/test_context_builder_session_timeout_safety_contract.sh`
- 这份 plan 现在可视为已完成的历史记录，不再代表当前未完成事项。
