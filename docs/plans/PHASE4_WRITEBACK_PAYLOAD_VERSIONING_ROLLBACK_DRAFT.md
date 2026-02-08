# Phase 4 回写载荷版本化与回滚草案（Draft）

**目标**：在 B42 回写载荷与 B44 联动一致性基础上，输出可执行的版本化回写计划与回滚队列。  
**阶段**：Batch B46

---

## 1. 交付物

- 模板：`docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_TEMPLATE.md`
- 生成脚本：`scripts/manage_archive_audit_writeback_payload_versioning_rollback_draft.sh`

---

## 2. 版本化口径

- 输入来源：
  - 回写报告：`writeback_status`、`writeback_signaled_items`、`writeback_changed_items`、`Receipt Writeback Payload`
  - 联动报告：`linkage_status`、`mismatch_rows`、`missing_payload_rows`
- 版本化规则：
  - 为每条回写载荷写入 `target_version`
  - `pending/in-progress/unknown` 默认进入回滚观察队列
  - `done` 保持上线版本，`waived` 标记豁免
- 状态评估：
  - `versioning_status=fail`：联动不一致或 writeback 变更覆盖为 0
  - `versioning_status=warn`：存在回滚候选或链路告警
  - `versioning_status=pass`：可直接推进版本化回写
- 输出字段：
  - `rollback_candidates`
  - `Versioned Payload Rows`
  - `Rollback Queue`
  - `release_advice`

---

## 3. 常用命令

```bash
# dry-run 参数检查
bash scripts/manage_archive_audit_writeback_payload_versioning_rollback_draft.sh \
  --dry-run \
  --version-id b46_dryrun_sample

# 生成样例版本化与回滚计划
bash scripts/manage_archive_audit_writeback_payload_versioning_rollback_draft.sh \
  --writeback-report docs/test_reports/ARCHIVE_AUDIT_EXECUTION_RECEIPT_WRITEBACK_SAMPLE_B42.md \
  --linkage-report docs/test_reports/ARCHIVE_AUDIT_RETEST_APPROVAL_WRITEBACK_LINKAGE_SAMPLE_B44.md \
  --version-id b46_sample_20260207_1530 \
  --target-version wbv-b46-sample \
  --rollback-version wbv-b45-prev \
  --output docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_SAMPLE_B46.md

# 严格模式（versioning_status 非 pass 则失败）
bash scripts/manage_archive_audit_writeback_payload_versioning_rollback_draft.sh \
  --writeback-report docs/test_reports/ARCHIVE_AUDIT_EXECUTION_RECEIPT_WRITEBACK_SAMPLE_B42.md \
  --linkage-report docs/test_reports/ARCHIVE_AUDIT_RETEST_APPROVAL_WRITEBACK_LINKAGE_SAMPLE_B44.md \
  --strict
```

---

## 4. 验收口径（B46）

- 支持 `--writeback-report/--linkage-report/--target-version/--rollback-version/--strict/--dry-run`。
- 输出版本化载荷明细与回滚队列。
- strict 模式可作为“回写载荷版本化门禁”草案。

---

## 5. 后续任务

- B47：证据巡检异常分级处置草案。
- B48：签批链路 SLA 违约预警草案。
- B49：回写变更覆盖率修复追踪草案。
