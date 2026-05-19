# `ISSLSessionResumption` Runtime Residual Classification Tightening

## Goal

把 session-resumption direct-core residual 的剩余文件集正式分类冻结：

- ordinary runtime drift 不再继续误混进 residual 集合
- intentional compatibility / semantic proof 明确标记
- mock/save-logic helper 不再被误扫成 public owner-path 漂移

## Scope

本批只处理：

- `src/fafafa.ssl.connection.base.pas`
- `tests/contract/test_backend_contract.pas`
- `tests/test_mbedtls_connection_session_reused_contract.pas`
- `tests/test_openssl_connection_session_reused_contract.pas`
- `tests/winssl/test_session_save_logic.pas`
- `tests/scripts/test_isslsessionresumption_runtime_residual_classification_contract.sh`
- `docs/plans/2026-05-19-isslsessionresumption-runtime-residual-classification-tightening.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

暂不处理：

- WinSSL 真正 resumed runtime proof
- backend implementation 新能力补齐
- session-resumption public owner-path 已完成的普通 runtime 文件

## Why This Batch

wave 1 / wave 2 已把 ordinary runtime owner-path migration 收得很窄，但当前 residual
里仍混着三种不同性质的文件：

1. compatibility mirror proof
2. backend semantic truth proof
3. mock/save-logic helper

如果不把它们正式分类，后续每次“继续深度审查接口设计/实现完整性”都会被同一批 residual
反复拉起。

## Planned Changes

1. 新增 focused shell contract，锁住 session-resumption residual 的准确文件集与标记语义。
2. 给 backend semantic truth proof 文件补 explicit intent markers。
3. 把 `tests/winssl/test_session_save_logic.pas` 的 mock getter 改成不再冒充 public
   `GetSession` surface。
4. 同步 `src/fafafa.ssl.connection.base.pas` 的 residual note，使源码注释 truth
   与当前 residual 分类一致。

## Verification

```bash
bash -n tests/scripts/test_isslsessionresumption_runtime_residual_classification_contract.sh
bash tests/scripts/test_isslsessionresumption_runtime_residual_classification_contract.sh
mkdir -p tmp/test_mbedtls_connection_session_reused_contract && \
  fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_mbedtls_connection_session_reused_contract \
  -FEtmp/test_mbedtls_connection_session_reused_contract \
  -otmp/test_mbedtls_connection_session_reused_contract/test_mbedtls_connection_session_reused_contract \
  tests/test_mbedtls_connection_session_reused_contract.pas && \
  ./tmp/test_mbedtls_connection_session_reused_contract/test_mbedtls_connection_session_reused_contract
mkdir -p tmp/test_openssl_connection_session_reused_contract && \
  fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_openssl_connection_session_reused_contract \
  -FEtmp/test_openssl_connection_session_reused_contract \
  -otmp/test_openssl_connection_session_reused_contract/test_openssl_connection_session_reused_contract \
  tests/test_openssl_connection_session_reused_contract.pas && \
  ./tmp/test_openssl_connection_session_reused_contract/test_openssl_connection_session_reused_contract
mkdir -p tmp/test_winssl_session_save_logic && \
  fpc -B -Fu./src -Fu./tests -Fu./tests/winssl \
  -FUtmp/test_winssl_session_save_logic \
  -FEtmp/test_winssl_session_save_logic \
  -otmp/test_winssl_session_save_logic/test_session_save_logic \
  tests/winssl/test_session_save_logic.pas && \
  ./tmp/test_winssl_session_save_logic/test_session_save_logic
git diff --check
```

## Expected Outcome

- session-resumption residual set 收窄成真正 intentional 的 direct-core proof 文件
- `test_session_save_logic` 不再作为 public owner-path 漂移噪音出现
- 源码注释、focused contract、planning files 对 residual 分类说同一套真话
