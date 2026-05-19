# macOS Batch Loader Regression Closure（2026-05-20）

## Goal
- 把当前 macOS `modules` 红面从“OpenSSL path/loader 猜测”收回到真正的回归边界：
  - 同一份 `OpenSSL 3.6.2 7 Apr 2026` runtime
  - direct symbol 仍然命中
  - 但 `EVP/PEM/PKCS12/CMS/OCSP` 这批 batch-loader 模块重新失效
- 为这条线补上 durable 诊断和最小修法，避免后续再反复回到旧的 loader/path 怀疑。

## Why now
- 旧证据已经证明：
  - `tmp/gh-run-26048015976/.../wave_b_macos_loader_symbol_probe_*.json`
    曾经在同类 macOS gate 上给出全绿 module truth
  - `tmp/gh-run-26108902159/.../wave_b_macos_loader_symbol_probe_*.json`
    现在却变成：
    - direct symbols `true`
    - `evp/pem/pkcs12/cms/ocsp` module truth `false`
- 这说明当前问题是回归，不是“macOS 一直不支持这些符号”。
- 旧的 `OPENSSL_ROOT` / loader path 方向已经在 2026-05-18 收口，不应再被当主 blocker 重拉。

## Non-Goals
- 本批不回头重写 WinSSL native probe lane。
- 本批不重新治理 macOS path/root 优先级。
- 本批不跑重型全仓门禁；优先 focused contract、focused compile、以及后续 GitHub macOS gate 复证。

## Files
- `docs/plans/2026-05-20-macos-batch-loader-regression-closure.md`
- `task_plan.md`
- `findings.md`
- `progress.md`
- `src/fafafa.ssl.openssl.loader.pas`
- `src/fafafa.ssl.openssl.api.evp.pas`
- `src/fafafa.ssl.openssl.api.pem.pas`
- `src/fafafa.ssl.openssl.api.pkcs12.pas`
- `src/fafafa.ssl.openssl.api.cms.pas`
- `src/fafafa.ssl.openssl.api.ocsp.pas`
- `tests/diagnostic/test_macos_openssl_loader_symbol_probe.pas`
- `tests/scripts/test_macos_batch_loader_regression_closure_contract.sh`

## Approach
1. 先把新的真实边界写死到 plan / findings / progress：
   - 旧 run `26048015976` 同类 probe 曾经全绿
   - 新 run `26108902159` 变成 direct symbol 绿、batch-loader 模块红
2. 新增 focused shell contract，冻结两件事：
   - 这批回归模块的 binding table 必须保留 runtime storage，而不是 fragile const-only 形态
   - macOS loader probe 必须继续输出 per-module batch diagnostics，方便后续不用再翻运行日志猜
3. 在 `TOpenSSLLoader.LoadFunctions(...)` 增加 lightweight diagnostics：
   - 上一次加载命中的 symbol 数
   - 缺失的 required binding 名单
4. 对当前失败的 batch-loader 模块做最小稳态修法：
   - `EVP / PEM / PKCS12 / CMS / OCSP` 的 binding table 切到 runtime storage
   - `PEM` 的 published-ready 判定回到真实 read surface，而不是把写路径缺口误升格成整模块失败
5. 用 focused compile / contract 校验收口，再把结果记回 planning files。

## Commands
```bash
bash -n tests/scripts/test_macos_batch_loader_regression_closure_contract.sh
bash tests/scripts/test_macos_batch_loader_regression_closure_contract.sh

fpc \
  -Fu./src \
  -Fu./tests \
  -Fu./tests/framework \
  -FUtmp/test_macos_batch_loader_probe_units \
  -FEtmp/test_macos_batch_loader_probe_bin \
  tests/diagnostic/test_macos_openssl_loader_symbol_probe.pas

./tmp/test_macos_batch_loader_probe_bin/test_macos_openssl_loader_symbol_probe \
  tmp/test_macos_batch_loader_probe.json

git diff --check
```

## Expected Outputs
- focused contract 证明：
  - runtime-binding 存储和 probe 诊断字段不会被回退
  - PEM ready 语义不会再被过严写路径判定误伤
- focused compile 证明：
  - macOS probe 程序在当前源码上继续可编译
- local probe 至少应继续产出 JSON，供我们检查新增 diagnostics 字段结构。

## Execution Result
- in progress
