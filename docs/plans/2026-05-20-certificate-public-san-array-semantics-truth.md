# Certificate Public SAN Array Semantics Truth

## Goal

把当前 `ISSLCertificate`
公开证书扩展 surface
里的 SAN / key-usage array truth
在活跃文档和代表性测试文件中重新对齐到源码事实，
避免仓库继续同时存在：

- 源码/API reference
  明确声明
  `TSSLStringArray`
- 活跃指南 / 活跃测试
  却仍按旧 `TStringList`
  心智教学或编写

## Scope

- 修改：
  - `docs/guides/TROUBLESHOOTING.md`
  - `tests/certificate/test_certificate_unit.pas`
  - `tests/scripts/test_certificate_public_san_array_semantics_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

- 不做：
  - 不扩到 `TCertificateUtils.GetInfo` / `TCertInfo`
    那条独立 `TStringList` 语义
  - 不重开 broader certificate redesign
  - 不顺手清扫整仓所有历史遗留测试

## Why This Batch

当前 source truth 已经很明确：

- `TSSLCertificateInfo.SubjectAltNames`
  是 `TSSLStringArray`
- `ISSLCertificate.GetSubjectAltNames`
  / `GetKeyUsage`
  / `GetExtendedKeyUsage`
  都返回 `TSSLStringArray`

但活跃面上仍有两处明显漂移：

1. `docs/guides/TROUBLESHOOTING.md`
   还在用：
   - `LAltNames.Count`
   - `LAltNames.Free`
2. `tests/certificate/test_certificate_unit.pas`
   仍把：
   - `GetSubjectAltNames`
   - `GetKeyUsage`
   - `GetExtendedKeyUsage`
   当成 `TStringList`
   使用，已经会直接编译失败

## TDD Steps

1. 先保留 RED 证据：
   - `test_certificate_unit.pas`
     当前编译失败
   - `TROUBLESHOOTING.md`
     当前仍命中
     `LAltNames.Count` /
     `LAltNames.Free`
2. 最小修法：
   - 文档改成 `Length(...)` / `High(...)`
   - 代表性测试改成 `TSSLStringArray`
     与 `ArrayContains(...)`
3. 新增 focused shell contract
   锁住这条 public array truth
4. 跑 compile/runtime proof

## Verification

```bash
bash -n tests/scripts/test_certificate_public_san_array_semantics_contract.sh
bash tests/scripts/test_certificate_public_san_array_semantics_contract.sh
fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_certificate_unit_units -FEtmp/test_certificate_unit_bin -otmp/test_certificate_unit_bin/test_certificate_unit tests/certificate/test_certificate_unit.pas
./tmp/test_certificate_unit_bin/test_certificate_unit
git diff --check
```

## Expected Outcome

- 活跃 public guide
  不再错误教学
  `ISSLCertificate.GetSubjectAltNames`
  的 owner/list semantics
- `test_certificate_unit.pas`
  重新变成可编译、可运行的代表性 OpenSSL 证书测试
- 这条 API truth
  被 focused contract 持续锁住
