# Certificate Store DN Query Canonical Contract

## Goal

把 `ISSLCertificateStore` 的两条 DN 查询入口：

- `FindBySubject`
- `FindByIssuer`

在当前仍有分叉的三组后端上：

- `FreePascal`
- `OpenSSL`
- `WinSSL`

收口到一条更一致、可复用、对调用方更可预测的 shared contract：

- query 先做 DN 归一化
- 支持 partial DN fragment lookup
- empty query -> `nil`

这样调用方就不必继续为：

- `O=Test Org,CN=Test Signer`
- `o = test org , cn = test signer`
- `Test Signer`

这些只是展示格式不同的人类输入，
在不同 backend 上得到不同结果。

## Scope

- 修改：
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `src/fafafa.ssl.openssl.certstore.pas`
  - `src/fafafa.ssl.winssl.certstore.pas`
  - `tests/test_freepascal_backend_basic.pas`
  - `tests/openssl/test_openssl_certstore_dn_query_contract.pas`
  - `tests/winssl/test_winssl_certstore.pas`
  - `tests/run_winssl_tests.ps1`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

不做：

- 不重写 `FindBySerialNumber`
- 不回头再改 `MbedTLS` / `WolfSSL`
  已经落地的 optional-backend query parity
- 不新开全仓库重量级 gate

## Architecture Truth

- 现在的真实分叉：
  - `FreePascal`
    - `FindBySubject`
      已做 normalized exact match
    - `FindByIssuer`
      仍是 `SameText` exact match
  - `OpenSSL`
    - `FindBySubject`
      / `FindByIssuer`
      是 uppercase substring match
    - 但没有空格/分隔符归一化
    - empty query 还会 fail-open
  - `WinSSL`
    - `FindBySubject`
      / `FindByIssuer`
      依赖
      `CERT_FIND_SUBJECT_STR_W`
      / `CERT_FIND_ISSUER_STR_W`
    - substring 语义存在，
      但 repo 级 normalized DN contract
      没有被明确编码
  - `MbedTLS` / `WolfSSL`
    现在已经是 normalized substring match

- 当前仓库里，
  `FindBySubject('DigiCert')`
  / `FindByIssuer('CA')`
  这类 partial query
  已经被当作正常用法存在于 store tests / docs / system-store smoke 中；
  所以本批不再回到
  “只允许 full DN exact match”
  的方向。

- 但内部链构建路径
  常常会用 full issuer DN
  去调用 `FindBySubject(...)`；
  因此实现层最好保留：
  - normalized exact 优先
  - 再做 substring fallback
  这样不改变外部 partial-query 可用性的同时，
  也尽量不把内部 full-DN path
  放宽得过粗。

## Steps

1. 先制造 RED：
   - `FreePascal`
     - normalized partial subject query
     - normalized partial issuer query
     - empty issuer query fail-closed
   - `OpenSSL`
     - deterministic fixture-based DN query contract
   - `WinSSL`
     - deterministic memory-store DN query contract
     - 并把该 test 接进 `tests/run_winssl_tests.ps1`
2. 最小修复：
   - `FreePascal`
     - subject / issuer
       统一成 normalized exact-first + substring fallback
   - `OpenSSL`
     - subject / issuer cache
       改存 normalized DN
     - empty query fail-closed
   - `WinSSL`
     - 不再只依赖 `CertFindCertificateInStore`
     - 改为基于 `FCertificates`
       做 normalized exact-first + substring fallback
3. Focused verification：
   - `fpc -B -Fu./src -Fu./tests -FUtmp/test_freepascal_backend_basic_units -FEtmp/test_freepascal_backend_basic_units -otmp/test_freepascal_backend_basic_units/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas`
   - `./tmp/test_freepascal_backend_basic_units/test_freepascal_backend_basic`
   - `fpc -B -Fu./src -Fu./src/openssl -Fu./tests -FEtmp/test_openssl_certstore_dn_query_contract -FUtmp/test_openssl_certstore_dn_query_contract_units -otmp/test_openssl_certstore_dn_query_contract/test_openssl_certstore_dn_query_contract tests/openssl/test_openssl_certstore_dn_query_contract.pas`
   - `./tmp/test_openssl_certstore_dn_query_contract/test_openssl_certstore_dn_query_contract`
   - `git diff --check`
4. Windows evidence：
   - push 后观察 `.github/workflows/winssl-tests.yml`
     是否编译并运行新增的
     `test_winssl_certstore.lpi`

## Expected Result

- `FreePascal` / `OpenSSL` / `WinSSL`
  对 `FindBySubject`
  / `FindByIssuer`
  不再各说各话
- full DN、
  normalized DN、
  partial DN fragment
  这三类人类输入
  在主要 backend 上
  都有一致的基础语义
- empty query
  统一 fail-closed
- Windows lane
  对 WinSSL 这条契约
  有自动证据

## Execution Result

- PASS
- `FreePascal`
  focused RED
  首轮打出：
  - `Certificate store should find certificate by normalized subject fragment query`
  修复后：
  - `tests/test_freepascal_backend_basic.pas`
    通过
- `OpenSSL`
  focused RED
  首轮打出：
  - `FindBySubject supports normalized partial DN fragment query`
  - `FindByIssuer supports normalized partial DN fragment query`
  修复后：
  - `tests/openssl/test_openssl_certstore_dn_query_contract.pas`
    `12 passed / 0 failed`
- 这批同时确认：
  - `signer_cert.pem`
    的实际 DN 序列化顺序
    是
    `CN -> O -> L -> ST -> C`
  - 所以 contract
    应强调
    partial DN fragment lookup
    与 normalization，
    不该把测试死绑到某一种 RDN 排列想象
- `WinSSL`
  本地不做伪运行；
  这批已把
  `test_winssl_certstore.lpi`
  接入
  `tests/run_winssl_tests.ps1`
  由现有 GitHub Windows workflow
  在 push 后自动验证
- `git diff --check`
  通过
