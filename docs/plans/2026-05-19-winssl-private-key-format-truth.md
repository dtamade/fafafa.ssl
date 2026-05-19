# 2026-05-19 WinSSL Private Key Format Truth

## Goal

继续沿着 capability / public-surface truth 主线推进，收口 WinSSL private-key format 这一条残余：

- `WinSSL` 当前仍把：
  - `SupportsDERPrivateKey`
  - `SupportsPKCS8PrivateKey`
  发布为 `True`
- 但现有 `LoadPrivateKey*` 实际只发布 `PFX/P12` import path
- 并且 `LoadPrivateKey(AStream, APassword)` 在收到 non-PFX 数据时当前存在 silent-success 漏洞：
  - else 分支错误写成 `if AStream = nil then raise ...`
  - 导致普通 PEM/DER 私钥流可能既不加载，也不 fail-fast

## Scope

- 只处理 WinSSL private-key format truth：
  - `src/fafafa.ssl.winssl.lib.pas`
  - `src/fafafa.ssl.winssl.context.pas`
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
  - `docs/reference/WINSSL_DESIGN.md`
  - `docs/guides/WINSSL_QUICKSTART.md`
  - `docs/guides/WINSSL_BEST_PRACTICES.md`
  - `docs/guides/WINSSL_USER_GUIDE.md`
- 用 focused shell contract + Pascal runtime contract 锁住 capability truth 与 fail-closed 语义
- 不重做 WinSSL 裸 DER/PKCS#8 私钥导入实现
- 不扩展到其它 backend 的 key-format 审计

## Files

- `src/fafafa.ssl.winssl.lib.pas`
- `src/fafafa.ssl.winssl.context.pas`
- `docs/reference/API_REFERENCE.md`
- `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
- `docs/reference/WINSSL_DESIGN.md`
- `docs/guides/WINSSL_QUICKSTART.md`
- `docs/guides/WINSSL_BEST_PRACTICES.md`
- `docs/guides/WINSSL_USER_GUIDE.md`
- `tests/scripts/test_winssl_private_key_format_truth_contract.sh`
- `tests/test_winssl_private_key_format_truth_contract.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `WinSSL`
  - `SupportsPKCS12=True`
    - 当前 published path = `LoadPrivateKey(file/stream, password)` 导入 `PFX/P12`
  - `SupportsDERPrivateKey=False`
    - 当前没有 shipped bare DER private-key load path
  - `SupportsPKCS8PrivateKey=False`
    - 当前没有 shipped bare PKCS#8 private-key load path
  - non-PFX private-key input
    - 必须 fail-closed `unsupported`
    - 不能 silent-success

## Steps

1. 补 focused contract，让 capability 假阳性和 non-PFX silent-success 先 RED。
2. 把 WinSSL key-format capability 收回到真实范围，并修正 non-PFX fail-closed 语义。
3. 同步 WinSSL active docs / guides，把错误示例改成 `PFX/P12` 路径。
4. 跑 focused contracts，回写台账并提交。

## Commands

```bash
bash -n tests/scripts/test_winssl_private_key_format_truth_contract.sh
bash tests/scripts/test_winssl_private_key_format_truth_contract.sh
mkdir -p tmp/test_winssl_private_key_format_truth && \
  fpc -B -Fu./src -Fu./tests \
    -FUtmp/test_winssl_private_key_format_truth \
    -FEtmp/test_winssl_private_key_format_truth \
    -otmp/test_winssl_private_key_format_truth/test_winssl_private_key_format_truth_contract \
    tests/test_winssl_private_key_format_truth_contract.pas && \
  ./tmp/test_winssl_private_key_format_truth/test_winssl_private_key_format_truth_contract
git diff --check
```

## Expected Result

- WinSSL 不再把 bare DER / PKCS#8 private-key loading 发布成已实现 capability
- non-PFX private-key input 不再 silent-success，而是 fail-closed `unsupported`
- WinSSL 专属文档和示例不再继续鼓励 `client.key` / `server.key` 这种误导性调用
