# Phase 4 一致性偏差修复建议草案（Draft）

**目标**：整合 B33 一致性检查、B36 关闭校验与 B32 阻断项信息，生成可执行修复建议清单。  
**阶段**：Batch B37

---

## 1. 交付物

- 模板：`docs/test_reports/ARCHIVE_AUDIT_CONSISTENCY_REMEDIATION_TEMPLATE.md`
- 生成脚本：`scripts/generate_archive_audit_consistency_remediation_draft.sh`

---

## 2. 修复建议口径

- 输入来源：
  - 一致性报告：`consistency_status`、`critical_fail_count`、`warning_count`
  - 关闭校验：`closure_status`、`critical_unclosed`、`high_unclosed`
  - 阻断清单：`blockers_critical/high/medium`
- 输出字段：
  - `critical_actions/high_actions/medium_actions`
  - `remediation_status`
  - `release_guidance`
  - 分级建议表（priority/area/owner/target_window）

---

## 3. 常用命令

```bash
# dry-run 参数检查
bash scripts/generate_archive_audit_consistency_remediation_draft.sh \
  --dry-run \
  --plan-id b37_dryrun_sample

# 生成样例修复建议
bash scripts/generate_archive_audit_consistency_remediation_draft.sh \
  --consistency-report docs/test_reports/ARCHIVE_AUDIT_WEEKLY_CHECKLIST_CONSISTENCY_SAMPLE_B33.md \
  --closure-record docs/test_reports/ARCHIVE_AUDIT_BLOCKER_CLOSURE_WAIVER_RECORD_SAMPLE_B36.md \
  --blockers docs/test_reports/PRE_RELEASE_AUDIT_BLOCKERS_SAMPLE_B32.md \
  --plan-id b37_sample_20260207_1100 \
  --output docs/test_reports/ARCHIVE_AUDIT_CONSISTENCY_REMEDIATION_SAMPLE_B37.md

# 严格模式（修复状态非 pass 则失败）
bash scripts/generate_archive_audit_consistency_remediation_draft.sh \
  --consistency-report docs/test_reports/ARCHIVE_AUDIT_WEEKLY_CHECKLIST_CONSISTENCY_SAMPLE_B33.md \
  --closure-record docs/test_reports/ARCHIVE_AUDIT_BLOCKER_CLOSURE_WAIVER_RECORD_SAMPLE_B36.md \
  --blockers docs/test_reports/PRE_RELEASE_AUDIT_BLOCKERS_SAMPLE_B32.md \
  --strict
```

---

## 4. 验收口径（B37）

- 支持 `--consistency-report/--closure-record/--blockers/--strict/--dry-run`。
- 输出分级修复建议与 release guidance。
- strict 模式可作为“修复建议执行前门禁”草案。

---

## 5. 后续任务

- B38：阈值策略回测与漂移监控草案。
- B39：执行回执签批链路草案。
- B40：阻断项重测与回归门禁草案。
