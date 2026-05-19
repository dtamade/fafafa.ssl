# WinSSL Platform Support Doc Truth Alignment

## Goal

收口 `WinSSL` 平台支持表述里的 residual drift，
把 `Windows 7 SP1`、`Windows Server 2019`
以及 `TLS 1.3` 门槛相关活跃文档
重新拉回当前 source truth：

- `Initialize`
  当前只要求：
  - `Windows Vista+`
- `TLS 1.1 / 1.2`
  当前按：
  - `Windows 7+`
- `TLS 1.3`
  当前按：
  - `Windows 10 1903+`
  - 即 `Build >= 18362`

## Scope

- 新增 focused shell contract，锁住当前 source / active-doc truth
- 最小修正以下活跃文档：
  - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
  - `docs/PLATFORM_SUPPORT.md`
  - `docs/ZERO_DEPENDENCY_DEPLOYMENT.md`
- 更新 `task_plan.md` / `findings.md` / `progress.md`

不做：

- 不改 `WinSSL` 实现
- 不重开 `session / early-data / OCSP` 线
- 不做新的 Windows 主机 runtime 实验

## Architecture Truth

- `src/fafafa.ssl.winssl.lib.pas`
  当前明确：
  - `Initialize`: `Windows Vista+`
  - `sslProtocolTLS11`: `Windows 7+`
  - `sslProtocolTLS12`: `Windows 7+`
  - `sslProtocolTLS13`: `Windows 10 Build 18362+`
- 但活跃文档当前仍存在几类 drift：
  - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
    - `Windows 7 SP1` 仍写成：
      - `⚠️ 部分 | 需更新`
    - `Windows Server 2019`
      仍写成：
      - `TLS 1.3 = ⚠️`
  - `docs/PLATFORM_SUPPORT.md`
    - 仍写：
      - `TLS 1.3 支持: Windows 10 20348+ 或 Windows 11`
  - `docs/ZERO_DEPENDENCY_DEPLOYMENT.md`
    - 版本矩阵仍按：
      - `Windows 10 (< 20348)`
      - `Windows 10 (≥ 20348)`
    - FAQ 也仍写：
      - `Windows 10 (≥ 20348) / Server 2022+`

## Files

- `src/fafafa.ssl.winssl.lib.pas`
- `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
- `docs/PLATFORM_SUPPORT.md`
- `docs/ZERO_DEPENDENCY_DEPLOYMENT.md`
- `tests/scripts/test_winssl_platform_support_doc_truth_contract.sh`
- `docs/plans/2026-05-20-winssl-platform-support-doc-truth-alignment.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. 新增 focused shell contract
2. 先跑 contract，确认当前 active docs 先 RED
3. 最小修正 WinSSL 平台支持相关活跃文档
4. 重新跑 focused verification
5. 更新 planning files，准备 commit / push

## Verification

```bash
bash -n tests/scripts/test_winssl_platform_support_doc_truth_contract.sh
bash tests/scripts/test_winssl_platform_support_doc_truth_contract.sh
git diff --check
```

## Expected Outcome

- `WinSSL` 活跃文档不再把：
  - `Windows 7 SP1`
    讲成“部分支持 / 需更新”
  - `Windows Server 2019`
    讲成 `TLS 1.3 = ⚠️`
  - `TLS 1.3`
    讲成 `20348+`
- 活跃文档会重新统一到：
  - `Vista+` baseline
  - `Windows 7+` for TLS 1.1 / 1.2
  - `Windows 10 1903+` for TLS 1.3
