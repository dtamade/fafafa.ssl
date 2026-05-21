# Linux Quickstart Current Public Truth Alignment

## Goal

收口 `docs/guides/LINUX_QUICKSTART.md`
这个高可见 Linux 入口页里仍残留的一组旧 public truth，
让新用户不再被带回：

- 旧的 `fafafa.ssl.factory` 直导入
- 旧的裸工厂 helper 名称
- 已不存在的示例文件 / backend 单元名
- 过期版本元数据

## Scope

- Update:
  - `docs/guides/LINUX_QUICKSTART.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`
- Add:
  - `docs/plans/2026-05-21-linux-quickstart-current-public-truth-alignment.md`
  - `tests/contract/test_linux_quickstart_public_entry_probe.pas`
  - `tests/scripts/test_linux_quickstart_current_public_truth_contract.sh`

不做：

- 不改 runtime 实现
- 不扩到其他 quickstart / zh docs
- 不重写整份 Linux 安装指南结构

## Why This Batch

`LINUX_QUICKSTART.md`
当前仍混着多层旧快照：

- 示例 1 仍写：
  - `uses fafafa.ssl.factory;`
  - `DetectBestLibrary`
  - `GetLibraryTypeName`
  - `GetLibraryInstance(...)`
- 示例 3 仍引用不存在的：
  - `examples/01_basic_ssl_client.pas`
- 项目结构仍写：
  - `fafafa.ssl.factory.pas    # 工厂模式（推荐入口）`
  - `fafafa.ssl.openssl.pas`
- 页脚元数据仍停在：
  - `v1.0.0-rc`
  - `2025-10-28`
- GitHub URL 仍是：
  - `yourusername`

这不是历史说明允许的 source-truth 引用，
而是会直接误导当前新用户的 active entry doc drift。

## Minimal Fix

1. 新增 focused contract，
   锁住这页 Linux quickstart 的当前 public truth
2. 用 façade-first 示例替换旧工厂写法：
   - `uses fafafa.ssl;`
   - `TSSLFactory.GetLibraryInstance(sslAutoDetect)`
   - `LibraryTypeToString(...)`
3. 把示例路径 / 项目结构 / FAQ 标题 / 元数据
   拉回当前仓库真相
4. 跑 focused contract + compile probe

## Verification

```bash
bash -n tests/scripts/test_linux_quickstart_current_public_truth_contract.sh
bash tests/scripts/test_linux_quickstart_current_public_truth_contract.sh
bash tests/scripts/test_active_docs_historical_reference_labels_contract.sh
git diff --check
```

## Expected Outcome

- Linux 新用户入口不再继续教学旧 factory-only 导入
- quickstart 示例能反映当前 façade/factory 公共入口边界
- 不存在的示例文件与 backend 单元名不再继续出现在活跃入口页

## Execution Result

- PASS
- focused RED 首轮证明的是
  真实 Linux 入口文档漂移，
  不是措辞微调：
  - `HEAD` 快照下
    新 contract
    第一条就因
    示例 1
    仍未使用
    `fafafa.ssl`
    主门面
    而失败
- 最小修复后：
  - `LINUX_QUICKSTART`
    现在已改回：
    - `uses fafafa.ssl;`
    - `TSSLFactory.GetLibraryInstance(sslAutoDetect)`
    - `LibraryTypeToString(...)`
  - 已移除：
    - `examples/01_basic_ssl_client.pas`
    - `fafafa.ssl.openssl.pas`
    - `v1.0.0-rc`
    - placeholder GitHub URL
- focused verification：
  - `bash -n tests/scripts/test_linux_quickstart_current_public_truth_contract.sh`
    - PASS
  - `HEAD` snapshot contract
    - FAIL
  - `bash tests/scripts/test_linux_quickstart_current_public_truth_contract.sh`
    - PASS
  - `bash tests/scripts/test_active_docs_historical_reference_labels_contract.sh`
    - PASS
  - `examples/01_tls_client.pas`
    focused compile
    - PASS
  - `git diff --check`
    - PASS
- 备注：
  - `01_tls_client`
    编译日志中的 warning/note
    属于仓库既有噪音，
    不是这次 quickstart truth 调整引入的新失败
