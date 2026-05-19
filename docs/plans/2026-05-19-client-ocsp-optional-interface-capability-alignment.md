# 2026-05-19 Client OCSP Optional Interface Capability Alignment

## Goal

继续沿着 optional-interface capability alignment 主线推进，把 `OpenSSL` / `WolfSSL` client-side `ISSLOCSPStapling` 暴露面也收回 capability-aware truth，避免出现：

- library capability 说 `OCSPStaplingSupport = none`
- 但 `CreateConnection(...)` 返回的对象仍然 `Supports(..., ISSLOCSPStapling, ...) = True`

这种会误导 caller / contract 的结构性漂移。

## Scope

- 只处理：
  - `OpenSSL` client connection `ISSLOCSPStapling`
  - `WolfSSL` client connection `ISSLOCSPStapling`
- 沿用现有 early-data gating 的 subclass matrix 模式
- 复用现有 focused source contract：
  - `tests/scripts/test_optional_interface_capability_alignment_contract.sh`
- 不改 runtime OCSP 逻辑
- 不动 `FreePascal` / `MbedTLS` / `WinSSL`

## Files

- `src/fafafa.ssl.openssl.context.pas`
- `src/fafafa.ssl.openssl.connection.pas`
- `src/fafafa.ssl.wolfssl.context.pas`
- `src/fafafa.ssl.wolfssl.connection.pas`
- `tests/scripts/test_optional_interface_capability_alignment_contract.sh`
- `docs/plans/2026-05-19-client-ocsp-optional-interface-capability-alignment.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `tests/contract/test_backend_contract.pas` 的 `Contract 10` 已经明确表达：
  - `OCSPStaplingSupport <> none` 时，client connection 应暴露 `ISSLOCSPStapling`
  - `OCSPStaplingSupport = none` 时，不应暴露 `ISSLOCSPStapling`
- 但当前 `TOpenSSLConnection` / `TWolfSSLConnection` 仍直接实现 `ISSLOCSPStapling`
- 与之前已收口的：
  - `ISSLEarlyDataContext`
  - `ISSLEarlyDataConnection`
  - `ISSLServerOCSPStaplingContext`
  属于同一类 capability/interface 漂移

## Steps

1. 扩展 focused source contract，让 client-side OCSP connection gating 先 RED。
2. 给 OpenSSL / WolfSSL connection 增加 capability-aware subclass matrix：
   - `base`
   - `ocsp`
   - `early-data`
   - `early-data + ocsp`
3. 让 `CreateConnection(ASocket/AStream)` 依据 current capability truth 选择正确 subclass。
4. 跑 focused contract 与最小编译验证。

## Commands

```bash
bash -n tests/scripts/test_optional_interface_capability_alignment_contract.sh
bash tests/scripts/test_optional_interface_capability_alignment_contract.sh
mkdir -p tmp/test_backend_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_backend_contract -FEtmp/test_backend_contract -otmp/test_backend_contract/test_backend_contract tests/contract/test_backend_contract.pas
git diff --check
```

## Expected Result

- OpenSSL / WolfSSL 的 client-side `ISSLOCSPStapling` 暴露重新与 capability truth 对齐
- `CreateConnection(...)` 不再把 capability=`none` 的 backend 误暴露成 OCSP-capable connection
- optional-interface completion audit 的结构性假设继续保持一致
