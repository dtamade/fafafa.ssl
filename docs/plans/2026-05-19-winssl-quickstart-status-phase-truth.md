# WinSSL Quickstart Status Phase Truth

## Goal

把 `docs/guides/WINSSL_QUICKSTART.md` 里的 `100% 完成`、`Phase 完成`、固定性能参考值、以及与当前 WinSSL public/runtime truth 冲突的选择建议收紧到当前可验证口径。

## Architecture

这批保持 docs-only：

- 新增 focused shell contract，冻结 `WINSSL_QUICKSTART` 的 status/phase/performance truth 边界
- 只修改 `docs/guides/WINSSL_QUICKSTART.md`
- 不改 WinSSL 生产实现
- 不把 scope 扩到 `WINSSL_USER_GUIDE.md`、`ARCHITECTURE.md` 或其它 quickstart

## Files

- Add: `docs/plans/2026-05-19-winssl-quickstart-status-phase-truth.md`
- Add: `tests/scripts/test_winssl_quickstart_status_phase_truth_contract.sh`
- Modify: `docs/guides/WINSSL_QUICKSTART.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

`WINSSL_QUICKSTART.md` 当前仍保留几类第一接触页最危险的旧口径：

- FAQ 里仍写：
  - `WinSSL 已完整实现服务器模式（Phase 5 完成）`
  - `WinSSL 已实现完整的自动证书验证（Phase 1 完成）`
- FAQ / 比较段还保留：
  - `WinSSL 性能与 OpenSSL 相当，甚至在某些场景下更快`
  - 固定 `~150ms / ~160ms / ~80 MB/s / ~85 MB/s` 这类参考值
- 使用建议里仍把：
  - `需要服务器模式（当前）`
  - `需要完整证书验证（当前）`
  推给 OpenSSL
- 页尾仍保留：
  - `WinSSL 后端 100% 完成（所有 6 个阶段）`

这些内容会直接影响第一次接触 WinSSL 的调用方心智，比普通历史输出块更危险。

## Verification

```bash
bash -n tests/scripts/test_winssl_quickstart_status_phase_truth_contract.sh
bash tests/scripts/test_winssl_quickstart_status_phase_truth_contract.sh
bash tests/scripts/test_winssl_quickstart_runtime_truth_contract.sh
bash tests/scripts/test_public_unit_import_guidance_truth_contract.sh
npx prettier --write docs/guides/WINSSL_QUICKSTART.md
git diff --check
```

## Expected Outcome

- `WINSSL_QUICKSTART.md` 保留：
  - 当前零依赖客户端快速入门角色
  - 当前 verify/mTLS/SNI 示例
  - 当前 public/runtime truth 入口
- 但不再把：
  - `100% 完成`
  - `Phase 5 完成`
  - `Phase 1 完成`
  - 固定 benchmark 数字
  - “需要服务器模式/完整证书验证请改用 OpenSSL”
  写成当前正文 truth
