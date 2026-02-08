# Phase 4 归档清理自动化命令草案（Draft）

**目标**：为 `artifacts/ci` 提供可重复、默认安全的清理命令草案，支持 profile 驱动与 hold 豁免。  
**阶段**：Batch B18

---

## 1. 脚本入口

- `scripts/cleanup_ci_artifacts_draft.sh`

核心设计：

- 默认 `dry-run`，不删除。
- 仅 `--apply` 时执行删除。
- 支持 `pr/nightly/release` 的默认保留天数。
- 支持 `--older-than-days` 覆盖阈值。
- 支持 `hold` 豁免：`.hold` 文件或 `manifest.md` 中 `hold: true`。

---

## 2. 常用命令

```bash
# 默认 dry-run（pr=30 天）
bash scripts/cleanup_ci_artifacts_draft.sh --profile pr

# 夜间策略 dry-run
bash scripts/cleanup_ci_artifacts_draft.sh --profile nightly

# 发布策略 dry-run
bash scripts/cleanup_ci_artifacts_draft.sh --profile release

# 强制阈值（用于演练）
bash scripts/cleanup_ci_artifacts_draft.sh --profile pr --older-than-days 0 --dry-run

# 真实删除（谨慎）
bash scripts/cleanup_ci_artifacts_draft.sh --profile pr --apply
```

---

## 3. 验收口径（B18）

- `--dry-run` 模式可输出候选清单与 summary。
- `--apply` 明确仅删除候选目录。
- `hold` 标记目录不会进入删除。

---

## 4. 与 B17 的关系

- B17 提供保留策略与冻结/豁免原则。
- B18 把该原则转成可执行清理命令草案。

---

## 5. 后续任务

- B19：归档保留策略合规核查清单。
- B20：跨平台 Gate 结果聚合摘要模板。
- B21：归档豁免（hold）标记流程草案。
