# 2026-03-10 pure Pascal client chain verification path

## Goal
- 把 pure Pascal / FreePascal 客户端的证书验证从“leaf 直看 trust store”推进到“消费 peer chain + trust roots”。
- 让真实客户端握手路径支持 `leaf + intermediate`，而不要求 intermediate 必须预先放进 trust store。

## Architecture
- 先写 scripted client-handshake RED：
  - 现场生成 `root CA -> intermediate CA -> leaf server cert`
  - server 发送 `leaf + intermediate`
  - client trust store 只放 `root`
  - 当前 pure Pascal 应失败，因为它还不会把 peer chain intermediates 纳入验证
- 然后最小修复：
  - 把 peer chain intermediates 合并进 effective validation store
  - 收口 `VerifyCertificate(...)` 到可沿 issuer 链继续走到 trusted self-signed root
- 最后用同一修复重试外网 `WithSystemRoots` 探针，确认真实系统根路径也受益。

## Files
- `src/fafafa.ssl.freepascal.lib.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/test_freepascal_client_chain_verification_path.pas`
- `docs/reference/PURE_PASCAL_CLIENT_M1_CHECKLIST.md`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## RED
- `fpc -Fu./src tests/test_freepascal_client_chain_verification_path.pas -otmp/test_fp_client_chain && ./tmp/test_fp_client_chain`
- Expected:
  - `leaf + intermediate` / trusted-root-only 路径当前失败

## GREEN
- 不追求一次性做成完整 PKIX 实现。
- 这波只收口 pure Pascal 当前最缺的实际运行点：
  - peer chain intermediates 进入验证路径
  - issuer 链能走到 trusted self-signed root

## Verification
- `fpc -Fu./src tests/test_freepascal_client_chain_verification_path.pas -otmp/test_fp_client_chain && ./tmp/test_fp_client_chain`
- `fpc -Fu./src tests/test_freepascal_client_custom_ca_sources_path.pas -otmp/test_fp_client_custom_ca && ./tmp/test_fp_client_custom_ca`
- `fpc -Fu./src tests/test_freepascal_client_hostname_verification_path.pas -otmp/test_fp_client_hostname && ./tmp/test_fp_client_hostname`
- `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic`
- `python3 -u scripts/compile_all_modules.py`
