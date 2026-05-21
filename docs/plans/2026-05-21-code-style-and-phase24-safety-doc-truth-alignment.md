# 2026-05-21 CODE_STYLE 与 Phase 2.4 safety 文档真相对齐

## Goal

修复两类仍会误导调用方或后续文档维护者的 drift：

1. `docs/guides/CODE_STYLE.md`
   仍发布旧的无参
   `LContext.CreateConnection;`
   示例，
   没有反映当前 transport-first
   连接语义。
2. `docs/guides/MIGRATION_GUIDE_PHASE_2.4.md`
   虽然仍有历史保留价值，
   但继续反复发布已经换名的旧单元
   `fafafa.ssl.types.safe`，
   且没有把自己标清为历史阶段说明。

## Why Now

- `CODE_STYLE`
  是活跃指南，
  示例即使只是“代码风格”上下文，
  也会被复制到真实接入代码里。
- `Phase 2.4`
  主题本身并未失效，
  当前源码和测试仍保留
  `TSSLVersion`
  /
  `TSecureData<T>`
  /
  `TResult<T, E>`
  等类型安全内容；
  但真实单元名已经是
  `fafafa.ssl.safety`，
  不能继续由历史文档发布旧名。

## Scope

- Add:
  - `docs/plans/2026-05-21-code-style-and-phase24-safety-doc-truth-alignment.md`
  - `tests/scripts/test_code_style_and_phase24_safety_doc_truth_contract.sh`
- Update:
  - `docs/guides/CODE_STYLE.md`
  - `docs/guides/MIGRATION_GUIDE_PHASE_2.4.md`
  - `src/fafafa.ssl.safety.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Minimal Fix

1. 把 `CODE_STYLE`
   中的旧无参连接示例
   改成当前 transport-first truth：
   `LContext.CreateConnection(YourConnectedSocket)`
2. 给
   `MIGRATION_GUIDE_PHASE_2.4.md`
   补上历史定位说明，
   并明确：
   当前 active migration truth
   以
   `MIGRATION_GUIDE.md`
   等现行文档为准
3. 把
   `fafafa.ssl.types.safe`
   全部切回当前源码真相：
   `fafafa.ssl.safety`
4. 顺手修掉
   `src/fafafa.ssl.safety.pas`
   顶部注释里的旧单元名，
   避免源码内嵌说明也继续漂移

## Verification

```bash
bash -n tests/scripts/test_code_style_and_phase24_safety_doc_truth_contract.sh
bash tests/scripts/test_code_style_and_phase24_safety_doc_truth_contract.sh
git diff --check
```

## Expected Result

- 活跃 style guide
  不再发布旧连接形态
- 历史 phase 文档
  继续保留背景价值，
  但不再假装自己是当前 active API truth
- type-safety 相关单元名
  在源码注释和文档里都回到同一套当前命名

## Post-Closeout Correction

- later static audit found that
  this batch only froze
  `CreateConnection(...)`
  transport-first truth,
  but did not freeze
  the active public import
  snippet in `CODE_STYLE.md`
- `docs/guides/CODE_STYLE.md`
  still retained:
  - `fafafa.ssl.base`
- the actual closeout for
  `CODE_STYLE`
  public import truth
  is therefore continued in:
  - `docs/plans/2026-05-22-code-style-public-import-truth-hardening.md`
