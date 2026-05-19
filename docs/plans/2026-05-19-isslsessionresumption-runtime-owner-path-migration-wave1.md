# `ISSLSessionResumption` Runtime Owner-Path Migration Wave 1

## Goal

把一批明显属于 ordinary runtime usage 的 session-resumption 测试从
`ISSLConnection.GetSession` / `SetSession` / `IsSessionReused` 迁到
`ISSLSessionResumption` owner path，进一步压缩 direct-core residual 集合。

## Scope

本批只迁移一组小而清晰的测试文件：

- `src/fafafa.ssl.connection.builder.pas`
- `src/fafafa.ssl.tls.pas`
- `tests/test_connection_builder_hostname_precedence.pas`
- `tests/test_freepascal_client_certificate_flight_requirements.pas`
- `tests/test_freepascal_client_session_resumption.pas`
- `tests/test_freepascal_server_session_resumption.pas`
- `tests/test_openssl_wolfssl_early_data_connection_contract.pas`
- `tests/test_tls_connector_early_data_contract.pas`
- `tests/scripts/test_isslsessionresumption_runtime_owner_path_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

暂不处理：

- `tests/test_freepascal_tls13_early_data.pas`
- backend-specific session semantic contracts
- WinSSL session runtime residuals

## Why This Batch

上一批已经把 `ISSLConnection` 上的 session-resumption mirrors 收成 compiler
`deprecated` compatibility surface。当前继续留在 ordinary runtime tests 里的 direct
core calls，会让 residual 集合过大，也会让后续 interface 完整性审查重复拉起。

## Planned Changes

1. 把这 5 个测试里的 ordinary runtime session usage 迁到 `ISSLSessionResumption`。
2. 把 `TSSLConnectionBuilder` / `TSSLConnector` 里的 ordinary production session usage
   也迁到 `ISSLSessionResumption`。
3. 新增 focused shell contract，锁住这批文件不再直接调用 core session mirrors。
4. 运行 focused compile / execute proof，确认迁移没有引入行为回归。

## Verification

```bash
bash -n tests/scripts/test_isslsessionresumption_runtime_owner_path_contract.sh
bash tests/scripts/test_isslsessionresumption_runtime_owner_path_contract.sh
mkdir -p tmp/test_builder_session_owner_path && \
  fpc -B -Fu./src -Fu./tests -FUtmp/test_builder_session_owner_path \
  -FEtmp/test_builder_session_owner_path \
  -otmp/test_builder_session_owner_path/test_connection_builder_hostname_precedence \
  tests/test_connection_builder_hostname_precedence.pas && \
  ./tmp/test_builder_session_owner_path/test_connection_builder_hostname_precedence
mkdir -p tmp/test_fp_client_cert_flight && \
  fpc -B -Fu./src -Fu./tests -FUtmp/test_fp_client_cert_flight \
  -FEtmp/test_fp_client_cert_flight \
  -otmp/test_fp_client_cert_flight/test_freepascal_client_certificate_flight_requirements \
  tests/test_freepascal_client_certificate_flight_requirements.pas && \
  ./tmp/test_fp_client_cert_flight/test_freepascal_client_certificate_flight_requirements
mkdir -p tmp/test_fp_client_resumption && \
  fpc -B -Fu./src -Fu./tests -FUtmp/test_fp_client_resumption \
  -FEtmp/test_fp_client_resumption \
  -otmp/test_fp_client_resumption/test_freepascal_client_session_resumption \
  tests/test_freepascal_client_session_resumption.pas && \
  ./tmp/test_fp_client_resumption/test_freepascal_client_session_resumption
mkdir -p tmp/test_fp_server_resumption && \
  fpc -B -Fu./src -Fu./tests -FUtmp/test_fp_server_resumption \
  -FEtmp/test_fp_server_resumption \
  -otmp/test_fp_server_resumption/test_freepascal_server_session_resumption \
  tests/test_freepascal_server_session_resumption.pas && \
  ./tmp/test_fp_server_resumption/test_freepascal_server_session_resumption
mkdir -p tmp/test_openssl_wolfssl_earlydata_contract && \
  fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_wolfssl_earlydata_contract \
  -FEtmp/test_openssl_wolfssl_earlydata_contract \
  -otmp/test_openssl_wolfssl_earlydata_contract/test_openssl_wolfssl_early_data_connection_contract \
  tests/test_openssl_wolfssl_early_data_connection_contract.pas && \
  ./tmp/test_openssl_wolfssl_earlydata_contract/test_openssl_wolfssl_early_data_connection_contract
mkdir -p tmp/test_tls_connector_earlydata_contract && \
  fpc -B -Fu./src -Fu./tests -FUtmp/test_tls_connector_earlydata_contract \
  -FEtmp/test_tls_connector_earlydata_contract \
  -otmp/test_tls_connector_earlydata_contract/test_tls_connector_early_data_contract \
  tests/test_tls_connector_early_data_contract.pas && \
  ./tmp/test_tls_connector_earlydata_contract/test_tls_connector_early_data_contract
git diff --check
```

## Expected Outcome

- 这批普通 runtime 测试统一转向 `ISSLSessionResumption`
- `builder` / `tls facade` 的普通生产路径也统一转向 `ISSLSessionResumption`
- direct-core session mirrors 的 residual 集合进一步变小
- 下一批可以专门处理更大的 `test_freepascal_tls13_early_data.pas`
