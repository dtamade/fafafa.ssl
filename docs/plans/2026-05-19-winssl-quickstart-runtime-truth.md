# WinSSL Quickstart Runtime Truth

## Goal

把 `docs/guides/WINSSL_QUICKSTART.md` 重新锚回当前 WinSSL runtime/source truth，避免它继续把自动证书验证、mTLS、`LoadCAFile` 与连接级 SNI 讲成“待实现”或旧接口用法。

## Architecture

这批保持 docs-only：

- 新增 focused shell contract，冻结 `WINSSL_QUICKSTART` 的 verify/mTLS/CA/SNI truth
- 只修 `docs/guides/WINSSL_QUICKSTART.md`
- 不改 WinSSL 生产实现
- 不重跑重型 Windows/runtime 门禁；继续复用已有源码与测试证据

## Files

- Add: `docs/plans/2026-05-19-winssl-quickstart-runtime-truth.md`
- Add: `tests/scripts/test_winssl_quickstart_runtime_truth_contract.sh`
- Modify: `docs/guides/WINSSL_QUICKSTART.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

当前 `WINSSL_QUICKSTART` 仍残留一组明显误导实现/接口认知的高入口 drift：

- 把 `Ctx.SetVerifyMode([sslVerifyPeer])` 写成“⏳ 待实现”
- 把 `sslVerifyFailIfNoPeerCert` / mTLS 路径写成“⏳ 待实现”
- 把 `LoadCAFile('custom-ca.crt')` 写成“⏳ 待实现”
- 故障排查里仍写“证书验证失败（未实现时使用手动模式）”
- 调试 SNI 示例仍使用 deprecated 的 `Ctx.GetServerName`
- 同一页 FAQ 却已经承认：
  - 自动证书验证已实现
  - 双向 TLS 已支持

这类矛盾会直接把 WinSSL backend 的当前 runtime truth 重新拉回旧阶段认知。

## Verification

```bash
bash -n tests/scripts/test_winssl_quickstart_runtime_truth_contract.sh
bash tests/scripts/test_winssl_quickstart_runtime_truth_contract.sh
bash tests/scripts/test_public_unit_import_guidance_truth_contract.sh
bash tests/scripts/test_winssl_private_key_format_truth_contract.sh
npx prettier --write docs/guides/WINSSL_QUICKSTART.md
git diff --check
```

## Expected Outcome

- `WINSSL_QUICKSTART.md` 不再把 verify/mTLS/CA path 讲成“待实现”
- quickstart 示例统一回到当前 public API：
  - `SetVerifyMode([sslVerifyPeer])`
  - `SetVerifyMode([sslVerifyPeer, sslVerifyFailIfNoPeerCert])`
  - `LoadCAFile(...)`
  - per-connection SNI access
- WinSSL 高入口 quickstart 与当前实现 truth 再次闭环
