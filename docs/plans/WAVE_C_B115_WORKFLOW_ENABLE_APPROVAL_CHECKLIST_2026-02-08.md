# Wave C B115 Workflow Enable Approval Checklist（2026-02-08）

## 目标

在启用 `.github/workflows/wave-c-quick-sprint-manual.yml.disabled` 前，确保所有前置条件可审计并由人工批准。

## 必备条件

1. B114 闭环验收报告为 PASS。
2. B113 签核记录状态从 `READY_FOR_APPROVAL` 变为 `APPROVED`。
3. B115 前置检查脚本输出 `READY_FOR_ENABLE`。

## 检查命令

```bash
bash scripts/check_wave_c_workflow_enable_prereq.sh --strict
```

## 启用动作（仅批准后执行）

```bash
mv .github/workflows/wave-c-quick-sprint-manual.yml.disabled \
   .github/workflows/wave-c-quick-sprint-manual.yml
```

## 风险控制

- 未批准不得启用 workflow。
- 启用后仅允许 `workflow_dispatch` 手动触发，不添加自动触发条件。
