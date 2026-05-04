# Completion Audit And Capability Truth Alignment Plan

**Goal:** 对“各个后端的接口和实现都完整”做一次真实 completion audit，把当前已经锁住的 interface surface、仍未完成的 implementation caveat、以及文档里高估能力等级的条目区分清楚；本批先收口 `docs/BACKEND_CAPABILITY_MATRIX.md` 与代码/测试/路线图之间的 FreePascal capability truth drift。

**Architecture:** 当前 `tests/contract/test_backend_contract.pas` 已经把 connection optional public surface 收到 `Contract 21`。因此下一步不能继续假设“还有一个同级 interface 没审”，而必须回到 objective 本身：哪些实现层 caveat 还存在，哪些文档把 bounded/experimental feature 写成了 complete/production。当前最明确、可直接落地的 drift 是：
- `docs/ROADMAP.md` 把 FreePascal `0-RTT / early data` 明确标成 experimental
- `src/fafafa.ssl.freepascal.lib.pas` 当前把 `ZeroRTTSupport` / `EarlyDataSupport` / `KnownIssues` 发布为 experimental + single-process in-memory anti-replay caveat
- `tests/test_capability_cache.pas` 直接锁住了这条 runtime capability truth
- 但 `docs/BACKEND_CAPABILITY_MATRIX.md` 仍把 FreePascal `Early Data` 写成“完整支持（生产就绪）”，并且在快速参考表里把 `Early Data` / `OCSP Stapling` / `Certificate Transparency` 都写成 `✅`

因此本批只做文档 truth alignment，不改 backend 实现，不伪造 `WinSSL` runtime proof，也不重开新的 OCSP/CT/validation/early-data 行为线。

**Files:**
- Modify: `docs/BACKEND_CAPABILITY_MATRIX.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: Audit the actual completion state

Inspect:

- `src/fafafa.ssl.base.pas`
- `tests/contract/test_backend_contract.pas`
- `docs/ROADMAP.md`
- `src/fafafa.ssl.freepascal.lib.pas`
- `tests/test_capability_cache.pas`
- `docs/BACKEND_CAPABILITY_MATRIX.md`

Audit questions:

- 当前 interface surface 是否还有未做 completion audit 的同级 optional public interface？
- 当前 objective 尚未完成的证据，是 interface drift、implementation caveat，还是 platform-specific runtime-proof blocker？
- 能力矩阵里哪些描述已经比实际代码/测试/路线图更乐观？

## Task 2: Align the capability matrix to runtime truth

Change:

- `docs/BACKEND_CAPABILITY_MATRIX.md`
  - FreePascal `Early Data (0-RTT)` 从 `✅` 收到 `⚠️`
  - FreePascal `OCSP Stapling` / `Certificate Transparency` 的文案改成“public surface 已暴露，但 capability 仍按 experimental 发布”
  - 零依赖部署建议不再写成“完整功能”

Constraints:

- 不改 `src/` 下任何实现
- 不伪造 `WinSSL` Windows runtime proof
- 不重写整份能力矩阵，只改最明确的 truth drift

## Task 3: Verify the documentation against code truth

Run:

```bash
mkdir -p tmp/capability_cache_units
fpc -B -Fu./src -Fu./tests -FUtmp/capability_cache_units -FEtmp/capability_cache_units -otmp/capability_cache_units/test_capability_cache tests/test_capability_cache.pas
./tmp/capability_cache_units/test_capability_cache
git diff --check
```

If formatting is available:

```bash
yarn prettier --write docs/BACKEND_CAPABILITY_MATRIX.md
```

## Definition Of Done

- completion audit 明确区分“已锁住的 interface surface”和“仍未完成的 implementation caveat”
- `docs/BACKEND_CAPABILITY_MATRIX.md` 不再把 FreePascal bounded/experimental capability 写成 production complete
- focused capability test 与 diff hygiene 通过

## Execution Result

- completion audit 结论：
  - `tests/contract/test_backend_contract.pas` 目前已经把 connection optional public surface 收到 `Contract 21`
  - broad objective 仍未完成，当前最明确的 remaining gaps 是：
    - `FreePascal` `0-RTT / early data` 仍按 `experimental` 发布，默认 shipped path 仍是单进程内存 anti-replay ledger
    - `WinSSL` 仍缺 Windows 主机 runtime proof
- focused capability truth 证据：
  - `tests/test_capability_cache.pas` 运行通过
  - 输出直接确认 `KnownIssues: 0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`
  - 同一 focused test 也锁住 `ZeroRTTSupport` / `EarlyDataSupport` / `OCSPStaplingSupport` / `CertTransparencySupport` 的 runtime truth
- 文档收口结果：
  - `docs/BACKEND_CAPABILITY_MATRIX.md` 不再把 FreePascal `Early Data` 写成“完整支持（生产就绪）”
  - FreePascal `OCSP Stapling` / `Certificate Transparency` 也不再在快速参考表里写成 `✅`
  - 零依赖部署建议不再把 FreePascal 描述成“完整功能”
- hygiene：
  - `prettier` 对 `docs/BACKEND_CAPABILITY_MATRIX.md` 返回 `unchanged`
  - `git diff --check` 通过
