# Phase 4 归档豁免（hold）标记流程草案（Draft）

**目标**：定义归档豁免（hold）标记的最小流程与命令入口，避免清理过程中误删关键调查证据。  
**阶段**：Batch B21

---

## 1. 适用场景

可设置 hold 的典型场景：

- 关联未关闭 P0/P1 缺陷
- 关联发布事故 RCA
- 处于审计或外部安全调查窗口

---

## 2. 脚本入口

- `scripts/mark_ci_artifact_hold_draft.sh`

核心行为：

- 默认 dry-run。
- `--apply` 才会写入/删除 hold 标记。
- 设置 hold 会写入：
  - `<run_dir>/.hold`
  - `<run_dir>/.hold.meta`

---

## 3. 常用命令

```bash
# 查看设置计划（不落盘）
bash scripts/mark_ci_artifact_hold_draft.sh \
  --run-id b11_smoke_20260207_0420 \
  --reason "release regression investigation" \
  --owner "qa-oncall" \
  --expires-on 2026-03-01 \
  --dry-run

# 实际设置 hold
bash scripts/mark_ci_artifact_hold_draft.sh \
  --run-id b11_smoke_20260207_0420 \
  --reason "release regression investigation" \
  --owner "qa-oncall" \
  --expires-on 2026-03-01 \
  --apply

# 清除 hold
bash scripts/mark_ci_artifact_hold_draft.sh \
  --run-id b11_smoke_20260207_0420 \
  --clear \
  --apply
```

---

## 4. 与 B18 清理脚本关系

- `cleanup_ci_artifacts_draft.sh` 会跳过以下目录：
  - 存在 `.hold`
  - 或 `manifest.md` 含 `hold: true`

因此 hold 流程是清理自动化前的保护阀。

---

## 5. 验收口径（B21）

- 设置 hold 可生成 `.hold` 与 `.hold.meta`。
- 清除 hold 可删除上述标记文件。
- dry-run 与 apply 行为明确区分。

---

## 6. 后续任务

- B22：归档清理执行记录模板草案。
- B23：归档审计抽样记录草案。
