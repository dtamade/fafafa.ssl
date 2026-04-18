# FreePascal Client CT Required Policy Fail-Closed Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 让 pure Pascal TLS 1.3 client 在已有 raw SCT / validation surface 基础上，新增一个明确的 `CT required` 配置位；当服务端缺失 SCT、validation 不可用或默认 policy 不满足时，在 client full-handshake 路径上 fail-closed。

**Architecture:** 这批继续沿当前 CT 路径做最小连续收口，不扩到 OCSP-delivered SCT source、log-store 配置或正向 policy fixture。配置层新增一个单独的 `required` option，并在 context builder 上提供导入/导出/override/clone/merge 的一致语义；运行时复用连接层已经缓存好的 raw SCT list、validation result 和 policy truth，只在 verify-peer 的非 resumed client full-handshake 上追加 required gate。若没有 SCT、没有 validation result 或 policy failed，直接按 certificate failure 语义拒绝握手。

**Tech Stack:** FreePascal (ObjFPC), `TSSLOption`, `TSSLContextBuilder`, `TFreePascalConnection`, `ISSLCertificateTransparencyValidation`, scripted TLS 1.3 handshake tests, builder config snapshot tests, file-based working memory.

---

## Task 1: RED - Prove the repo still lacks CT required policy wiring

**Files:**
- Modify: `tests/test_freepascal_client_ct_sct_surface.pas`
- Modify: `tests/config/test_context_builder_try.pas`
- Modify: `tests/config/test_context_builder_merge_advanced_option_snapshot_semantics.pas`
- Modify: `tests/test_transformation_methods.pas`

**Step 1: Add runtime required-policy contracts**
- 在 `tests/test_freepascal_client_ct_sct_surface.pas` 新增至少两个场景：
  - `required + missing SCT` => handshake fail-closed
  - `required + dummy SCT list` => handshake fail-closed（若 CT validation 可用则应落到 policy failed；若不可用则应落到 validation unavailable）
- 继续保持既有 raw surface / validation surface 契约不变。

**Step 2: Add builder contracts for the new option**
- 在 builder 相关测试里新增：
  - `Override('certificate_transparency_required', 'true')` 可成功 build client，且 context options 持久化包含新 option
  - merge 场景下，source `certificate_transparency_required=false` 能清掉 stale state，`true` 能复制进目标
  - transformation/export 场景下，override 后 JSON 可见 `certificate_transparency_required=true`

**Command (RED):**
```bash
mkdir -p tmp/freepascal_client_ct_sct_surface && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_client_ct_sct_surface \
  -FEtmp/freepascal_client_ct_sct_surface \
  -otmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface \
  tests/test_freepascal_client_ct_sct_surface.pas && \
./tmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface

mkdir -p tmp/test_context_builder_try && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_context_builder_try \
  -FEtmp/test_context_builder_try \
  -otmp/test_context_builder_try/test_context_builder_try \
  tests/config/test_context_builder_try.pas && \
./tmp/test_context_builder_try/test_context_builder_try

mkdir -p tmp/test_context_builder_merge_advanced_option_snapshot_semantics && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_context_builder_merge_advanced_option_snapshot_semantics \
  -FEtmp/test_context_builder_merge_advanced_option_snapshot_semantics \
  -otmp/test_context_builder_merge_advanced_option_snapshot_semantics/test_context_builder_merge_advanced_option_snapshot_semantics \
  tests/config/test_context_builder_merge_advanced_option_snapshot_semantics.pas && \
./tmp/test_context_builder_merge_advanced_option_snapshot_semantics/test_context_builder_merge_advanced_option_snapshot_semantics

mkdir -p tmp/test_transformation_methods && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_transformation_methods \
  -FEtmp/test_transformation_methods \
  -otmp/test_transformation_methods/test_transformation_methods \
  tests/test_transformation_methods.pas && \
./tmp/test_transformation_methods/test_transformation_methods
```

**Expected RED:**
- 当前实现不存在 `ssoRequireCertificateTransparency` / builder wiring / runtime required gate，因此测试会在编译期缺符号或在运行期错误放行。

---

## Task 2: GREEN - Add the smallest CT required configuration + runtime gate

**Files:**
- Modify: `src/fafafa.ssl.base.pas`
- Modify: `src/fafafa.ssl.context.builder.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Add a new context option**
- 在 `src/fafafa.ssl.base.pas` 追加新的 `TSSLOption` 成员，例如：
  - `ssoRequireCertificateTransparency`
- 保持历史序号兼容注释风格，不复用已有 OCSP option。

**Step 2: Wire the option through the builder**
- 在 `src/fafafa.ssl.context.builder.pas`：
  - 新增 builder field，例如 `FCertificateTransparencyRequired`
  - 新增 sync helper，保证 option set 与布尔字段一致
  - 提供 fluent 方法 `WithCertificateTransparencyRequired(...)`
  - 打通 `WithOption` / `WithoutOption` / `WithOptions` / `Extend` / `Override`
  - 打通 `ExportToJSON` / `ImportFromJSON` / `ExportToINI` / `ImportFromINI` / `Clone` / `Reset` / `Merge`
- 这批只增加 `required`，不发明单独的 CT enable flag。

**Step 3: Add runtime fail-closed enforcement**
- 在 `src/fafafa.ssl.freepascal.connection.pas` 新增 helper，例如 `ValidateClientCertificateTransparency`
- 语义：
  - `sslVerifyPeer` 未启用或 session reused => 直接跳过
  - 未设置 `ssoRequireCertificateTransparency` => 直接跳过
  - missing SCT list => fail-closed
  - validation result unavailable => fail-closed
  - policy not satisfied => fail-closed
- 在现有 client handshake 顺序里，把 gate 插在：
  - trust
  - flags
  - OCSP
  - CT required
  - `SendClientFinished`

**Step 4: Keep the boundary explicit**
- 不改 CT request 触发条件：仍由 `sslVerifyPeer` 驱动
- 不扩到 OCSP-delivered SCT source
- 不为了正例去扩 log-store / custom policy /真实可通过的 SCT fixture
- 这批只承诺 fail-closed negative paths

---

## Task 3: Verification

**Commands:**
```bash
mkdir -p tmp/freepascal_client_ct_sct_surface && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_client_ct_sct_surface \
  -FEtmp/freepascal_client_ct_sct_surface \
  -otmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface \
  tests/test_freepascal_client_ct_sct_surface.pas && \
./tmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface

mkdir -p tmp/test_context_builder_try && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_context_builder_try \
  -FEtmp/test_context_builder_try \
  -otmp/test_context_builder_try/test_context_builder_try \
  tests/config/test_context_builder_try.pas && \
./tmp/test_context_builder_try/test_context_builder_try

mkdir -p tmp/test_context_builder_merge_advanced_option_snapshot_semantics && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_context_builder_merge_advanced_option_snapshot_semantics \
  -FEtmp/test_context_builder_merge_advanced_option_snapshot_semantics \
  -otmp/test_context_builder_merge_advanced_option_snapshot_semantics/test_context_builder_merge_advanced_option_snapshot_semantics \
  tests/config/test_context_builder_merge_advanced_option_snapshot_semantics.pas && \
./tmp/test_context_builder_merge_advanced_option_snapshot_semantics/test_context_builder_merge_advanced_option_snapshot_semantics

mkdir -p tmp/test_transformation_methods && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_transformation_methods \
  -FEtmp/test_transformation_methods \
  -otmp/test_transformation_methods/test_transformation_methods \
  tests/test_transformation_methods.pas && \
./tmp/test_transformation_methods/test_transformation_methods

mkdir -p tmp/test_freepascal_client_peer_certificate_surface && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_freepascal_client_peer_certificate_surface \
  -FEtmp/test_freepascal_client_peer_certificate_surface \
  -otmp/test_freepascal_client_peer_certificate_surface/test_freepascal_client_peer_certificate_surface \
  tests/test_freepascal_client_peer_certificate_surface.pas && \
./tmp/test_freepascal_client_peer_certificate_surface/test_freepascal_client_peer_certificate_surface

mkdir -p tmp/test_freepascal_client_certificateverify_runtime && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_freepascal_client_certificateverify_runtime \
  -FEtmp/test_freepascal_client_certificateverify_runtime \
  -otmp/test_freepascal_client_certificateverify_runtime/test_freepascal_client_certificateverify_runtime \
  tests/test_freepascal_client_certificateverify_runtime.pas && \
./tmp/test_freepascal_client_certificateverify_runtime/test_freepascal_client_certificateverify_runtime

mkdir -p tmp/test_freepascal_client_chain_trust_runtime && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_freepascal_client_chain_trust_runtime \
  -FEtmp/test_freepascal_client_chain_trust_runtime \
  -otmp/test_freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime \
  tests/test_freepascal_client_chain_trust_runtime.pas && \
./tmp/test_freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime

mkdir -p tmp/test_freepascal_client_ocsp_stapling_runtime && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_freepascal_client_ocsp_stapling_runtime \
  -FEtmp/test_freepascal_client_ocsp_stapling_runtime \
  -otmp/test_freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime \
  tests/test_freepascal_client_ocsp_stapling_runtime.pas && \
./tmp/test_freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime

mkdir -p tmp/test_freepascal_client_session_resumption && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_freepascal_client_session_resumption \
  -FEtmp/test_freepascal_client_session_resumption \
  -otmp/test_freepascal_client_session_resumption/test_freepascal_client_session_resumption \
  tests/test_freepascal_client_session_resumption.pas && \
./tmp/test_freepascal_client_session_resumption/test_freepascal_client_session_resumption

mkdir -p tmp/test_freepascal_tls13_early_data && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_freepascal_tls13_early_data \
  -FEtmp/test_freepascal_tls13_early_data \
  -otmp/test_freepascal_tls13_early_data/test_freepascal_tls13_early_data \
  tests/test_freepascal_tls13_early_data.pas && \
./tmp/test_freepascal_tls13_early_data/test_freepascal_tls13_early_data

python3 scripts/compile_all_modules.py

git diff --check -- \
  docs/plans/2026-04-09-freepascal-client-ct-required-policy.md \
  src/fafafa.ssl.base.pas \
  src/fafafa.ssl.context.builder.pas \
  src/fafafa.ssl.freepascal.connection.pas \
  tests/test_freepascal_client_ct_sct_surface.pas \
  tests/config/test_context_builder_try.pas \
  tests/config/test_context_builder_merge_advanced_option_snapshot_semantics.pas \
  tests/test_transformation_methods.pas \
  task_plan.md findings.md progress.md
```

**Expected:**
- new CT required runtime contracts => PASS
- builder try / merge / transformation contracts => PASS
- adjacent FreePascal regressions => PASS
- `python3 scripts/compile_all_modules.py` => PASS
- targeted `git diff --check` => PASS

---

## Execution Notes

- RED observed as expected:
  - `tests/test_freepascal_client_ct_sct_surface.pas` failed at compile time with:
    - `Identifier not found "ssoRequireCertificateTransparency"`
  - builder contracts also failed at compile time with:
    - missing `ssoRequireCertificateTransparency`
    - missing `WithCertificateTransparencyRequired`
- Final implementation kept the batch intentionally narrow:
  - `TSSLOption` 只新增一个 required-only CT option
  - CT request 触发条件仍保持为 `sslVerifyPeer`
  - builder 打通了 required state 的 fluent / override / JSON / INI / merge / clone / build 持久化
  - FreePascal runtime gate 只在 verify-peer、非 resumed client full-handshake 上执行
  - gate 条件收敛为：
    - missing SCT list
    - validation result unavailable
    - CT policy failed
- Final verification results:
  - `tests/test_freepascal_client_ct_sct_surface.pas` => PASS
  - `tests/config/test_context_builder_try.pas` => PASS
  - `tests/config/test_context_builder_merge_advanced_option_snapshot_semantics.pas` => PASS
  - `tests/test_transformation_methods.pas` => PASS
  - `tests/test_freepascal_client_peer_certificate_surface.pas` => PASS
  - `tests/test_freepascal_client_certificateverify_runtime.pas` => PASS
  - `tests/test_freepascal_client_chain_trust_runtime.pas` => PASS
  - `tests/test_freepascal_client_ocsp_stapling_runtime.pas` => PASS
  - `tests/test_freepascal_client_session_resumption.pas` => PASS
  - `tests/test_freepascal_tls13_early_data.pas` => PASS
  - `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
  - targeted `git diff --check` => PASS
