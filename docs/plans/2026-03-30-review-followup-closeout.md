# 2026-03-30 Review Follow-up Closeout

## Goal
- 收口 2026-03-29 repo review 中本轮用户确认要直接处理的几项问题：
  - WinSSL `SetCertificateStore(...)` / `GetCAStoreHandle(...)` 语义脱节
  - active docs 仍错误教授 `.WithVerifyHostname`
  - `docs/CA_CERTIFICATE_AUTO_LOADING.md` 仍宣称 OpenSSL client context 自动加载系统 CA
  - `docs/PLATFORM_SUPPORT.md` 的自动后端选择说明仍过度简化

## Architecture
- Runtime fix:
  - `src/fafafa.ssl.winssl.context.pas`
- Docs fix:
  - `docs/guides/COMMON_PITFALLS.md`
  - `docs/guides/security-best-practices.md`
  - `docs/CA_CERTIFICATE_AUTO_LOADING.md`
  - `docs/PLATFORM_SUPPORT.md`
  - optional comment alignment in adjacent source docs if needed
- Regression/contracts:
  - `tests/scripts/test_active_tls_guidance_contract.sh`
  - `tests/scripts/test_winssl_context_external_store_contract.sh`

## Steps
1. RED
   - 新增 active-doc guidance contract：
     - active docs 不得再出现 `.WithVerifyHostname`
     - CA autoload doc 不得继续宣称 client context 自动加载 system CA
     - platform support doc 需要明确“按可用性 + 优先级选择”，而不是硬编码 Linux/macOS=OpenSSL
   - 新增 WinSSL source contract：
     - `GetCAStoreHandle(...)` 必须能回落到 external certificate store handle
   - 运行新 contracts，确认当前树为 RED
2. GREEN
   - 最小修改 `src/fafafa.ssl.winssl.context.pas`
   - 最小修改 active docs 到当前真实语义
3. VERIFY
   - 重跑新增 contracts
   - 重跑 repo 推荐 baseline：
     - `python3 scripts/compile_all_modules.py`
     - `bash scripts/run_minimal_ci_gate.sh --fast-local`
   - 视情况补跑 docs/script syntax check

## Expected Outcome
- WinSSL 不再静默丢弃 external/system store 注入
- active docs 统一到“连接级 ServerName + 显式 WithSystemRoots”语义
- 自动后端选择文档和当前优先级实现对齐
