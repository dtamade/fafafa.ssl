# Managed Result Initialization Safety

## Goal

收掉当前 public facade / shared connection base 里一类真实的 Pascal managed-record / managed-array 初始化风险：

- 不再对带 `string` / 动态数组的 `Result` 直接 `FillChar(...)`
- 不再让默认空 `TBytes` 返回值继续靠 `SetLength(Result, 0)` 触发
  `managed type result variable does not seem to be initialized`
  warning
- 保持现有 public 行为不变，只把初始化路径收回类型安全写法

## Scope

- `src/fafafa.ssl.pas`
- `src/fafafa.ssl.connection.base.pas`
- `tests/test_connection_builder_hostname_precedence.pas`
- `tests/scripts/test_managed_result_init_safety_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Why This Batch

当前继续深挖接口/实现残口时，发现两条高可见 shared surface
还留着和上一批 capability serializer 同类的静态坑：

- `CreateDefaultConfig(...)`
  的 fallback 路径对 `TSSLConfig` 直接 `FillChar(Result, SizeOf(Result), 0);`
- `TBaseSSLConnection` 的 shared getter 里：
  - `GetConnectionInfo`
  - `GetDiagnosticInfo`
  - `DoGetOCSPResponse`
  - `DoGetSignedCertificateTimestampList`
  仍在使用会触发 managed-result warning 的初始化方式

这不是纯代码风格问题：

- `TSSLConfig`
  / `TSSLConnectionInfo`
  / `TSSLDiagnosticInfo`
  都带 `string` / 动态数组字段
- 这类写法会持续制造编译期 warning，
  也给后续 public helper / owner-surface 安全性留下不必要噪音
- verification harness `tests/test_connection_builder_hostname_precedence.pas`
  也在用同类空 `TBytes` 默认返回写法，所以这批会顺手把它一起收口

## Expected Result

- `CreateDefaultConfig(...)`
  fallback 改为 `Default(TSSLConfig)`
- `TBaseSSLConnection.GetConnectionInfo`
  改为 `Default(TSSLConnectionInfo)`
- `TBaseSSLConnection.GetDiagnosticInfo`
  改为 `Default(TSSLDiagnosticInfo)`
- `DoGetOCSPResponse`
  /
  `DoGetSignedCertificateTimestampList`
  改为显式 `Result := nil`
- `TMockCertificate.SaveToDER`
  /
  `TMockSession.Serialize`
  改为显式 `Result := nil`
- focused compile 中，上述 `connection.base` managed-result warnings 消失
- focused compile 中，`tests/test_connection_builder_hostname_precedence.pas`
  的同类 managed-result warnings 也消失

## Verification

```bash
bash -n tests/scripts/test_managed_result_init_safety_contract.sh
bash tests/scripts/test_managed_result_init_safety_contract.sh
mkdir -p tmp/defaultcfg_units tmp/defaultcfg_bin
fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/defaultcfg_units -FEtmp/defaultcfg_bin -otest_default_config tests/config/test_default_config.pas
./tmp/defaultcfg_bin/test_default_config
mkdir -p tmp/conninfo_units tmp/conninfo_bin
fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/conninfo_units -FEtmp/conninfo_bin -otest_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas
./tmp/conninfo_bin/test_connection_builder_hostname_precedence
git diff --check
```
