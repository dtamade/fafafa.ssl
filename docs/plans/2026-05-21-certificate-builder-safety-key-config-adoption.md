# Certificate Builder Safety Key Config Adoption

## Goal

把
`type-safety`
从
“主门面 non-generic surface 已可见”
继续推进到
“真实 certificate builder / high-level certificate path
开始采用 type-safe key config”。

这批 focused 目标是：

- 给 `ICertificateBuilder`
  补齐
  `TKeySize`
  /
  `TEllipticCurve`
  overload
- 保持旧
  `Integer`
  /
  `string`
  overload
  继续可用，
  不破坏兼容
- 让
  `src/fafafa.ssl.cert.pas`
  /
  `src/fafafa.ssl.quick.pas`
  这两条高层真实路径
  率先采用新的 type-safe overload
- 用 focused compile/runtime contract
  锁住：
  - public builder surface
  - 高层采用事实
  - ECDSA curve invalid boundary

## Why This Batch

上一批已经把
`fafafa.ssl`
主门面的 non-generic safety surface
收口成当前真相：

- `TKeySize`
- `TEllipticCurve`
- 相关 helper

但静态审查说明：

- `src/fafafa.ssl.cert.builder.pas`
  仍只暴露：
  - `WithRSAKey(ABits: Integer = 2048)`
  - `WithECDSAKey(const ACurve: string = 'prime256v1')`
- `src/fafafa.ssl.cert.pas`
  /
  `src/fafafa.ssl.quick.pas`
  也仍在喂裸
  `Integer`
  /
  `string`

这会让 type-safety 继续停留在：

- isolated safety unit
- 文档说明
- 单独测试

而没有真正进入
“用户最可能直接调用的证书生成路径”。

## Current Architecture Truth

- `src/fafafa.ssl.safety.pas`
  里的
  `TKeySize`
  是 unit-safe bits wrapper，
  `TEllipticCurve`
  是 public curve enum
- `src/fafafa.ssl.cert.utils.pas`
  仍使用自己的内部：
  - `TKeyType = (ktRSA, ktECDSA, ktEd25519)`
  - `KeyBits: Integer`
  - `ECCurve: string`
- `GenerateECKey(const ACurve: string)`
  实际吃的是
  OpenSSL curve token：
  - `prime256v1`
  - `secp384r1`
  - `secp521r1`
  - `brainpoolP256r1`
  - `brainpoolP384r1`
  - `brainpoolP512r1`
- `EllipticCurveToString(...)`
  返回的是展示名：
  - `P-256`
  - `P-384`
  - `P-521`
  - `X25519`
  - `Brainpool P-256`

因此：

- 不能直接把
  `EllipticCurveToString(...)`
  当作 builder 底层 token
- `ec_X25519`
  /
  `ec_X448`
  必须在 builder ECDSA path
  明确 reject，
  因为它们是 ECDH-only curve，
  不是当前证书 builder
  的 signing curve truth

## Scope

- Add:
  - `docs/plans/2026-05-21-certificate-builder-safety-key-config-adoption.md`
  - `tests/contract/test_certificate_builder_safety_key_config_entry.pas`
  - `tests/scripts/test_certificate_builder_safety_key_config_contract.sh`
- Update:
  - `src/fafafa.ssl.cert.builder.pas`
  - `src/fafafa.ssl.cert.builder.impl.pas`
  - `src/fafafa.ssl.cert.pas`
  - `src/fafafa.ssl.quick.pas`
  - `README.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Minimal Fix

1. `ICertificateBuilder`
   增加 overload：
   - `WithRSAKey(const ASize: TKeySize)`
   - `WithECDSAKey(ACurve: TEllipticCurve)`
2. `TCertificateBuilderImpl`
   实现 bridge：
   - `TKeySize -> ToBits`
   - `TEllipticCurve -> OpenSSL curve token`
3. 对
   `ec_X25519`
   /
   `ec_X448`
   明确抛
   `ESSLInvalidArgument`
4. 高层真实路径采用：
   - `TCertificate.CreateSelfSigned`
   - `TCertificate.CreateServerCert`
   - `TCertificate.CreateClientCert`
   - `TSSLQuick.GenerateSelfSigned`
   - `TSSLQuick.GenerateServerCert`
   - `TSSLQuick.GenerateCACert`
5. README 示例切到
   `TKeySize.Bits(2048)`

## Verification

```bash
bash -n tests/scripts/test_certificate_builder_safety_key_config_contract.sh
bash tests/scripts/test_certificate_builder_safety_key_config_contract.sh
git diff --check
```

contract 脚本内部会完成：

- public builder surface 静态检查
- high-level adoption 静态检查
- compile/run focused probe：
  - `WithRSAKey(TKeySize.Bits(2048)).SelfSigned`
  - `WithECDSAKey(ec_P256).SelfSigned`
  - `WithECDSAKey(ec_X25519)` 必须 reject

## Expected Result

- builder key config
  不再只停留在
  `Integer`
  /
  `string`
  public surface
- 高层证书创建路径
  开始真实采用
  type-safe key config
- `X25519/X448`
  不会被错误伪装成
  当前 ECDSA cert path
  的合法 curve
- 这条 adoption
  被 focused contract
  固定下来，
  后续不会轻易漂回旧入口
