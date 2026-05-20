# WinSSL Certificate Identity Getter Full-DN Truth

## Goal

把
`TWinSSLCertificate.GetSubject`
/
`GetIssuer`
从当前的
simple display name
语义
收口到与当前仓库其它 backend
以及 WinSSL certstore query
一致的
full X.500 DN public truth，
避免调用方继续遇到：

- `GetSubject`
  只返回
  `Test Signer`
  这类 display name
- `GetIssuer`
  只返回
  `Test CA`
  这类 display name
- certstore query
  已经支持 full DN，
  但 public getter
  仍停在更弱语义

## Scope

- 修改：
  - `src/fafafa.ssl.winssl.certificate.pas`
  - `tests/winssl/test_winssl_certstore.pas`
  - `tests/scripts/test_winssl_certificate_identity_getter_truth_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

不做：

- 不重开
  `FindBySubject`
  /
  `FindByIssuer`
  已经收口完成的
  query implementation lane
- 不重开
  `OpenSSL`
  /
  `WolfSSL`
  /
  `MbedTLS`
  已完成的
  certificate identity getter lane
- 不跑整条重型 gate；
  只做 focused static contract
  + push 后 Windows CI proof

## Architecture Truth

- `OpenSSL`
  当前
  `GetSubject`
  /
  `GetIssuer`
  走
  `X509NameToString(...)`
  full DN path
- `WolfSSL`
  /
  `MbedTLS`
  当前都优先走
  parser truth，
  返回完整 DN 信息
- `WinSSL certstore`
  当前内部已经通过
  `CertNameToStrW(..., CERT_X500_NAME_STR or CERT_NAME_STR_COMMA_FLAG, ...)`
  读取 full DN
- 但
  `TWinSSLCertificate.GetSubject`
  /
  `GetIssuer`
  仍走
  `CertGetNameStringW(..., CERT_NAME_SIMPLE_DISPLAY_TYPE, ...)`
- `tests/winssl/test_winssl_certstore.pas`
  已经在
  Windows runtime lane
  中加载确定性 fixture，
  适合作为这条 public getter truth
  的现成 runtime proof

## TDD Steps

1. 先补 focused static contract，
   要求：
   - `WinSSL` getter
     不再使用
     `CERT_NAME_SIMPLE_DISPLAY_TYPE`
   - `test_winssl_certstore`
     必须显式断言
     getter 保留
     `CN=` / `O=`
     这些 full-DN component
2. 在
   `tests/winssl/test_winssl_certstore.pas`
   把确定性 fixture
   的 getter truth
   加成 runtime 断言
3. 观察 static RED
4. 最小修复
   `src/fafafa.ssl.winssl.certificate.pas`
5. Focused verification：
   - `bash -n tests/scripts/test_winssl_certificate_identity_getter_truth_contract.sh`
   - `bash tests/scripts/test_winssl_certificate_identity_getter_truth_contract.sh`
   - `git diff --check`
6. push 后观察：
   - `CI`
   - `WinSSL Runtime Gate`

## Expected Result

- `WinSSL`
  public
  `GetSubject`
  /
  `GetIssuer`
  与 certstore query
  不再语义分裂
- 确定性 fixture
  在 Windows runner
  上能证明：
  getter 至少保留
  `CN=` / `O=`
  这些 DN component
- 这条 backend completeness gap
  进入可重复记录，
  后续不需要再从静态审查重新拉起
