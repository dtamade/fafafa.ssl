# Phase 3 CI Workflow 分层接入草案（Draft）

**目标**：将 B11~B14 的策略落为可执行的 CI workflow 蓝图，覆盖 Linux/macOS/Windows 分层门禁与统一归档。  
**阶段**：Batch B15

---

## 1. 推荐 workflow 拆分

建议采用「主入口 + 场景化 job」模式：

1. `pr-gate`：Pull Request 快速阻断（L0+L1）。
2. `nightly-extended`：夜间扩展验证（L0+L1+L2）。
3. `release-deep`：发布前深度验证（L0+L1+L2+L3）。
4. `archive`：统一归档步骤（调用 `archive_ci_artifacts_draft.sh`）。

---

## 2. 触发策略（Draft）

```yaml
on:
  pull_request:
    branches: [ main, develop ]
  schedule:
    - cron: '0 1 * * *'   # nightly
  push:
    tags:
      - 'v*.*.*'
  workflow_dispatch:
```

---

## 3. Job 分层建议

### 3.1 Linux（基础必跑）

- PR：
  - `python3 scripts/compile_all_modules.py`
  - `bash scripts/run_minimal_ci_gate.sh --dry-run`
- Nightly/Release：
  - `bash scripts/run_linux_openssl_matrix_draft.sh --dry-run`
  - Release 可追加 `--skip-phase2-dryrun --verbose`

### 3.2 macOS（路径与兼容）

- PR：
  - `bash scripts/run_macos_openssl_path_check_draft.sh --dry-run`
- Nightly/Release：
  - `bash scripts/run_macos_openssl_path_check_draft.sh --skip-phase2-dryrun`
  - Release 可追加完整模块链路（待实机验证）

### 3.3 Windows（WinSSL 主路径）

- PR：
  - `lazbuild` 编译核心与 WinSSL 测试目标
  - 运行 `test_winssl_comprehensive.exe` 与核心 P2 comprehensive
- Nightly/Release：
  - 追加 `run_winssl_tests.ps1` 与 `run_openssl_tests.ps1` 对照链路

---

## 4. 统一归档步骤（B11 对接）

建议在每个平台 job 末尾加入归档步骤（`if: always()`）：

```yaml
- name: Build CI archive
  if: always()
  run: |
    bash scripts/archive_ci_artifacts_draft.sh \
      --profile ${{ inputs.archive_profile || 'pr' }} \
      --run-id ${{ github.job }}_${{ github.run_id }}

- name: Upload CI archive
  if: always()
  uses: actions/upload-artifact@v4
  with:
    name: CI-Archive-${{ github.job }}-${{ github.run_id }}
    path: artifacts/ci/
    retention-days: 30
```

> 注：`retention-days` 可与 `profile`（pr/nightly/release）对齐调整。

---

## 5. 最小 workflow 样例骨架（Draft）

```yaml
name: Layered CI Draft

jobs:
  linux-pr-gate:
    if: github.event_name == 'pull_request'
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - run: python3 scripts/compile_all_modules.py
      - run: bash scripts/run_minimal_ci_gate.sh --dry-run
      - run: bash scripts/archive_ci_artifacts_draft.sh --profile pr --run-id linux_${{ github.run_id }}

  macos-pr-check:
    if: github.event_name == 'pull_request'
    runs-on: macos-latest
    steps:
      - uses: actions/checkout@v4
      - run: bash scripts/run_macos_openssl_path_check_draft.sh --dry-run
      - run: bash scripts/archive_ci_artifacts_draft.sh --profile pr --run-id macos_${{ github.run_id }}

  windows-pr-gate:
    if: github.event_name == 'pull_request'
    runs-on: windows-latest
    steps:
      - uses: actions/checkout@v4
      - run: powershell -ExecutionPolicy Bypass -File run_winssl_tests.ps1
      - shell: bash
        run: bash scripts/archive_ci_artifacts_draft.sh --profile pr --run-id windows_${{ github.run_id }}
```

---

## 6. 验收口径（B15）

- 明确 PR/Nightly/Release 三类触发的 job 分层。
- 明确 Linux/macOS/Windows 每个平台的命令入口。
- 明确统一归档步骤与 artifact 命名建议。

---

## 7. 后续任务

- B16：门禁证据模板统一化（平台统一字段/阈值/判定）。
- B17：发布级归档保留策略与清理窗口草案。
- B18：归档清理自动化命令草案。
