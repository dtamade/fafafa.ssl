# macOS OpenSSL Root Loader Priority（2026-05-18）

## Goal
- 修正 `src/fafafa.ssl.openssl.loader.pas` 的 macOS/OpenSSL 动态库选择顺序，让 loader 在存在 `OPENSSL_ROOT` 时先尝试该 root 下的绝对 `libcrypto/libssl` 路径，避免 generic fallback 回退到错误库面，导致 `PEM/EVP/PKCS12/CMS/OCSP` 符号缺失。

## Why now
- GitHub Actions live run `26044471873` 已确认本轮 `windows-gate` / `linux-gate` 全绿，唯一失败点只剩 `macos-gate`。
- macOS artifact 表明环境探测拿到的是 Homebrew `OpenSSL 3.6.2`，但模块测试仍出现：
  - `PEM 模块加载失败`
  - `LoadOpenSSLCMS returned False`
  - `LoadOpenSSLOCSP returned False`
  - `PKCS12_new not loaded`
- 这与此前路线文档里“macOS loader 应优先尝试 `libcrypto.3.dylib/libssl.3.dylib`，避免误加载系统 `libcrypto.dylib/libssl.dylib`”的方向一致，但当前代码还没把 `OPENSSL_ROOT` 绝对路径优先级落成。

## Non-Goals
- 不在本批次里重写 `PEM/EVP/PKCS12/CMS/OCSP` 模块加载器本身。
- 不把 `macos-gate` 的模块通过率阈值改成更松。
- 不回头重开已经收口的 WinSSL native-probe / integration-multi lane。

## Files
- `src/fafafa.ssl.openssl.loader.pas`
- `tests/scripts/test_openssl_loader_macos_openssl_root_priority_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Approach
1. 先加 focused source contract，锁定 loader 必须：
   - 读取 `OPENSSL_ROOT`
   - 为 `libcrypto` / `libssl` 生成 `OPENSSL_ROOT/lib/...` 绝对候选
   - 在 generic `TryLoadLibrary([...])` 之前优先尝试这些绝对路径
2. 观察 contract 初始失败，证明当前源码还没有这条优先级。
3. 在 `TOpenSSLLoader.GetLibraryHandle(...)` 内加入 `OPENSSL_ROOT` 优先加载逻辑。
4. 重新跑 focused contract，并补 `git diff --check`。
5. 如本地静态验证通过，则 push 后交给 GitHub macOS CI 复核实际运行时效果。

## Commands
```bash
bash -n tests/scripts/test_openssl_loader_macos_openssl_root_priority_contract.sh
bash tests/scripts/test_openssl_loader_macos_openssl_root_priority_contract.sh
git diff --check
```

## Expected Outputs
- contract 初始为 FAIL，指出 loader 尚未优先使用 `OPENSSL_ROOT`。
- 修复后 contract 转为 PASS。
- `git diff --check` PASS。
- 后续 GitHub macOS 复核时，应优先观察：
  - `PEM` / `EVP` 是否恢复加载
  - `PKCS12/CMS/OCSP` 的核心符号是否不再成片缺失

## Execution Result
- RED:
  - `bash tests/scripts/test_openssl_loader_macos_openssl_root_priority_contract.sh`
    初始失败，明确指出 loader 还没有读取 `OPENSSL_ROOT` 并在 generic fallback 前优先使用它
- GREEN:
  - `src/fafafa.ssl.openssl.loader.pas` 已新增 `TryLoadLibraryFromOpenSSLRoot(...)`
  - `libcrypto` / `libssl` 现在都会先尝试 `OPENSSL_ROOT/lib/...` 绝对候选
  - focused source contract 已完成 `RED -> GREEN`
  - `tests/test_openssl_loader_ready_contract.pas`
  - `tests/test_openssl_loader_required_symbol_contract.pas`
    两条 Pascal loader contract 均已编译并运行通过
- PENDING:
  - 仍需新的 GitHub macOS rerun，确认 live module lane 不再卡在 `PEM/EVP/PKCS12/CMS/OCSP` 的成片符号缺失
