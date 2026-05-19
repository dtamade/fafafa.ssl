# Integration Guide Canonical Path Truth（2026-05-20）

## Goal
- 把 `INTEGRATION_GUIDE` 的 active truth 收回到唯一权威路径：
  - `docs/INTEGRATION_GUIDE.md`
- 消除 `docs/guides/INTEGRATION_GUIDE.md` 这份已漂移副本继续制造双真相。

## Why now
- 当前 repo 同时存在：
  - `docs/INTEGRATION_GUIDE.md`
  - `docs/guides/INTEGRATION_GUIDE.md`
- 两份文件内容已经分叉：
  - 根目录版本承载了最近多轮 owner-path / verification / session-resumption / early-data truth
  - `guides/` 版本更像旧的集成教程副本，还残留 split-unit import 与过时结构
- 更危险的是：
  - 一部分 contract / README / 文档索引盯根目录版本
  - 另一部分 contract 仍盯 `guides/` 副本
- 这会让“推荐入口 / 权威集成指南”出现双路径真相。

## Scope
- `docs/INTEGRATION_GUIDE.md`
- `docs/guides/INTEGRATION_GUIDE.md`
- `tests/scripts/test_integration_guide_canonical_path_truth_contract.sh`
- `tests/scripts/test_facade_main_entry_truth_contract.sh`
- `tests/scripts/test_landing_docs_connection_level_sni_guidance_contract.sh`
- `tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Non-Goals
- 不重写集成指南的整体章节结构。
- 不扩大到所有 guide 的全面 import 风格治理。
- 不改任何生产实现逻辑。

## Approach
1. 先新增 focused contract，直接冻结：
   - `docs/INTEGRATION_GUIDE.md` 是 canonical path
   - active contracts 不再指向 `docs/guides/INTEGRATION_GUIDE.md`
2. 更新 canonical guide 的 active code snippets：
   - 尽量统一走 `uses fafafa.ssl;`
   - 仅在确有必要时补 `fafafa.ssl.context.builder`
3. 删除已漂移的 `docs/guides/INTEGRATION_GUIDE.md` 副本。
4. 运行 focused contracts，确认：
   - canonical path 唯一
   - facade/import/SNI guidance 全部转到根目录权威文件

## Commands
```bash
bash -n tests/scripts/test_integration_guide_canonical_path_truth_contract.sh
bash tests/scripts/test_integration_guide_canonical_path_truth_contract.sh

bash -n tests/scripts/test_facade_main_entry_truth_contract.sh
bash tests/scripts/test_facade_main_entry_truth_contract.sh

bash -n tests/scripts/test_landing_docs_connection_level_sni_guidance_contract.sh
bash tests/scripts/test_landing_docs_connection_level_sni_guidance_contract.sh

bash -n tests/scripts/test_public_unit_import_guidance_truth_contract.sh
bash tests/scripts/test_public_unit_import_guidance_truth_contract.sh

git diff --check
```

## Expected Outputs
- repo 只保留一份 active `INTEGRATION_GUIDE` truth source
- contract/test 路径统一指向 `docs/INTEGRATION_GUIDE.md`
- canonical guide 的 active snippets 不再回退到 split-unit / non-facade import teaching

## Execution Result
- completed
- RED:
  - `bash tests/scripts/test_integration_guide_canonical_path_truth_contract.sh`
    first failed with:
    - `stale shadow integration guide still exists at docs/guides/INTEGRATION_GUIDE.md`
- GREEN:
  - deleted `docs/guides/INTEGRATION_GUIDE.md`
  - updated active contracts to target `docs/INTEGRATION_GUIDE.md`
  - normalized canonical guide imports toward `uses fafafa.ssl;`
- verification:
  - `bash -n tests/scripts/test_integration_guide_canonical_path_truth_contract.sh`
  - `bash tests/scripts/test_integration_guide_canonical_path_truth_contract.sh`
  - `bash tests/scripts/test_facade_main_entry_truth_contract.sh`
  - `bash tests/scripts/test_landing_docs_connection_level_sni_guidance_contract.sh`
  - `bash tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
  - `bash tests/scripts/test_isslconnectioninfo_active_guidance_contract.sh`
  - `bash tests/scripts/test_isslsessionresumption_active_guidance_contract.sh`
  - `bash tests/scripts/test_isslcertificateverification_active_guidance_contract.sh`
  - `bash tests/scripts/test_docs_readme_integration_guide_exists_contract.sh`
  - `git diff --check`
  - all PASS
