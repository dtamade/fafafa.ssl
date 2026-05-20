# OpenSSL And WinSSL Certificate Store Serial Query Parity

## Goal

把 `OpenSSL` / `WinSSL` 证书存储对象的
`FindBySerialNumber`
收紧到与当前仓库既有
`FreePascal` / `MbedTLS` / `WolfSSL`
一致的 normalized query truth，
避免调用方在这两个 backend 上继续遇到：

- 同一张证书
  只因序列号大小写不同
  就查不到
- 同一张证书
  只因带 `:`
  / 空格
  的展示格式不同
  就查不到

## Scope

- 修改：
  - `src/fafafa.ssl.openssl.certstore.pas`
  - `src/fafafa.ssl.winssl.certstore.pas`
  - `tests/openssl/test_openssl_certstore_serial_query_contract.pas`
  - `tests/winssl/test_winssl_certstore.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

- 不做：
  - 不重开 `FindBySubject` / `FindByIssuer`
  - 不重开 broader store index/cache family
  - 不扩大到 fingerprint query follow-up

## Architecture Truth

- 当前仓库里，
  `FindBySerialNumber`
  的 public contract
  已经在：
  - `FreePascal`
  - `MbedTLS`
  - `WolfSSL`
  收口到
  normalized hex truth
- 但当前源码里：
  - `TOpenSSLCertificateStore`
    仍把 serial
    直接按
    `UpperCase(...)`
    建索引和查询
  - `TWinSSLCertificateStore`
    仍对
    `Cert.GetSerialNumber`
    与输入做
    `SameText(...)`
    原样比较
- 这说明
  `ISSLCertificateStore.FindBySerialNumber`
  在两个主 backend
  上仍留着真实 query drift，
  不只是测试空白

## Steps

1. 新增 OpenSSL focused contract：
   - lower-case
   - 带 `:`
   - 带首尾空白
   的 serial query
   仍能命中同一张证书
2. 在 WinSSL 现有 certstore 测试里补同类 memory-backed store 断言
3. 先跑 OpenSSL focused proof，
   观察 RED
4. 在 OpenSSL / WinSSL store 实现中补 serial normalize helper
5. 再跑 OpenSSL focused proof，
   并等 GitHub Windows/CI 做 WinSSL runtime proof

## Focused Proof

```bash
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_openssl_certstore_serial_query_contract_units \
  -FEtmp/test_openssl_certstore_serial_query_contract_units \
  -otmp/test_openssl_certstore_serial_query_contract_units/test_openssl_certstore_serial_query_contract \
  tests/openssl/test_openssl_certstore_serial_query_contract.pas

./tmp/test_openssl_certstore_serial_query_contract_units/test_openssl_certstore_serial_query_contract

git diff --check
```

## Execution Result

- 首轮
  `OpenSSL`
  focused RED
  证明这批不只是 store query
  没归一化：
  - `Fixture exposes serial number`
  - `FindBySerialNumber supports normalized serial query variant`
    同时失败
- 继续缩边界后，
  真实根因收窄成两层：
  - `TOpenSSLCertificate.GetSerialNumber`
    在 native serial helper
    尚未 ready
    时会直接退出，
    导致后面的 pure-Pascal fallback
    实际上走不到
  - `SaveToDER`
    也仍依赖
    已经加载好的 export helper，
    会把 fallback
    一起卡死
- 本批最终修法：
  - `OpenSSL`
    / `WinSSL`
    store
    都补
    serial normalize helper
  - `TOpenSSLCertificate.SaveToDER`
    补 lazy-load
  - `TOpenSSLCertificate.GetSerialNumber`
    改成：
    - 先尝试 lazy-load native helper
    - native path
      仍取不到 serial
      时，
      回退到
      DER / PEM 导出
      + `TX509Certificate`
      parser
- 当前本地 proof：
  - `./tmp/test_openssl_certstore_serial_query_contract_units/test_openssl_certstore_serial_query_contract`
    已通过：
    `9 passed / 0 failed`
  - `git diff --check`
    通过
- 当前剩余 proof：
  - `WinSSL`
    runtime
    继续交给
    GitHub Windows CI
