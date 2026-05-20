# WinSSL CertStore Test API Drift Alignment

## Goal

修掉 `WinSSL Runtime Gate`
里 `tests/winssl/test_winssl_certstore.pas`
对旧 API 的编译漂移，
让这份 runtime suite
重新测试当前真实的
`TWinSSLCertificateStore`
公开面，而不是把
`ISSLCertificateStore`
当成 concrete class 来调用。

## Scope

- 修改：
  - `tests/winssl/test_winssl_certstore.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

不做：

- 不扩大到 `WinSSL` 运行时逻辑重写
- 不给公共 `ISSLCertificateStore` 补回旧 concrete-only 方法
- 不重开非 certstore 的 Windows lane

## Architecture Truth

- 当前源码里：
  - `ISSLCertificateStore`
    只公开
    - `AddCertificate`
    - `RemoveCertificate`
    - `Contains`
    - `GetCount`
    - `GetCertificate`
    - `LoadSystemStore`
    - `FindBy*`
    - `VerifyCertificate`
    - `BuildCertificateChain`
  - `TWinSSLCertificateStore`
    额外公开：
    - `Open`
    - `Close`
    - `IsOpen`
    - `GetAllCertificates`
    - `GetNativeHandle`
- 当前 `test_winssl_certstore.pas`
  却把变量声明成
  `ISSLCertificateStore`，
  然后再去调用上面这些 concrete-only 方法，
  所以 Windows runner
  直接在编译期炸掉
- 同时
  `TWinSSLCertificateStore.Create(const AStoreName: string)`
  当前真实语义是：
  - 当 `AStoreName <> ''`
    会立即打开对应系统存储
  - 所以“`Create('MY')` 后未打开”
    这个测试前提本身也过时

## Steps

1. 把 WinSSL-specific runtime test 的 store 变量改成 concrete type
2. 增加本地 helper，
   明确区分：
   - `Create('')`
     用于测试 unopened initial state
   - `Create('ROOT'/'MY'/'CA')`
     用于测试 constructor-open truth
3. 保留真正 shared 的 interface contract 断言，
   但不再让接口类型背负 concrete-only surface
4. 推到 `master`
   交给 GitHub Windows CI 验证

## Expected Result

- `test_winssl_certstore.lpi`
  至少先恢复编译通过
- WinSSL broader runtime suite
  不再被一份陈旧 test file
  在编译期拦死

## Execution Result

- PASS
- `gh run view 26137704210 --job 76876360188 --log-failed`
  明确证明这不是
  `TWinSSLCertificateStore`
  少了方法，
  而是
  `test_winssl_certstore.pas`
  把
  `ISSLCertificateStore`
  当成 concrete class
  去调用：
  - `Open`
  - `Close`
  - `IsOpen`
  - `GetAllCertificates`
  - `GetNativeHandle`
- 同时也纠正了一个语义漂移：
  - `TWinSSLCertificateStore.Create('MY')`
    当前真实行为是立即打开该 store
  - 所以“Create 后仍未打开”
    的旧断言是错的
- 本批改动：
  - 把 WinSSL-specific store 变量切回 concrete type
  - 加了 `OpenConcreteSystemStore(...)`
    helper
  - 把需要“未打开初始态”的测试
    改成 `Create('')`
- 本地可验证部分：
  - `git diff --check`
    - PASS
- 最终 Windows compile/runtime truth：
  - `26138267777`
    workflow `CI`
    - `success`
  - `26138267809`
    workflow `WinSSL Runtime Gate`
    - `success`
