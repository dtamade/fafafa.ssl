# WinSSL CertStore Chain Runtime Contract

## Goal

补齐
`TWinSSLCertificateStore.BuildCertificateChain`
在 Windows runtime
下的 partial/full chain contract，
并修掉当前实现里
把 `ISSLCertificate`
裸指针塞进 `TList`
导致的接口保活风险，确保：

- memory store 只有 intermediate 时，
  返回最小链
  `leaf -> intermediate`
- memory store 同时有
  `intermediate + root` 时，
  返回完整链
  `leaf -> intermediate -> root`
- WinSSL certstore test
  一旦出现断言失败，
  会用非零退出码
  真实传给 CI

## Scope

- 修改：
  - `src/fafafa.ssl.winssl.certstore.pas`
  - `src/fafafa.ssl.winssl.certificate.pas`
  - `tests/winssl/test_winssl_certstore.pas`
  - `tests/run_winssl_tests.ps1`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

不做：

- 不扩大到更广的
  WinSSL HTTPS / session / handshake
  lane
- 不重写
  Windows 原生链引擎
  的信任判定策略
- 不跑本地重型 compile gate

## Architecture Truth

- 当前
  `TWinSSLCertificateStore.BuildCertificateChain`
  走的是
  `CertGetCertificateChain`
  原生链引擎
- 但结果收集时：
  - 先把 `ISSLCertificate`
    直接转成 `Pointer`
    放进 `TList`
  - 后面再从 `TList`
    转回接口数组
- 这会绕开
  interface refcount，
  使得循环里较早的
  `ChainCert`
  在下一次赋值时
  提前释放，
  留下悬空指针
- 同时，
  `tests/winssl/test_winssl_certstore.pas`
  当前即使有断言失败，
  也不会在结尾
  `Halt(1)`，
  CI 可能把失败测试
  误看成成功执行
- 这批远端红灯还进一步暴露出：
  - `TWinSSLCertificate.LoadFromFile`
    当前只走 DER
  - 遇到仓库长期在用的
    `*.pem` fixture
    不会自动 fallback 到
    `LoadFromPEM`

## TDD Steps

1. 在
   `tests/winssl/test_winssl_certstore.pas`
   增加 focused contract：
   - 生成
     `root -> intermediate -> leaf`
   - case A：
     memory store
     只放 intermediate，
     期望长度 `2`
   - case B：
     memory store
     放
     `intermediate + root`，
     期望长度 `3`
   - 同时要求
     返回链上的 fingerprint
     仍可安全读取
2. 修正 test harness：
   - 若 `GTestsFailed > 0`
     必须以非零退出
3. 最小修复：
   - `BuildCertificateChain`
     不再用 `TList`
     存裸接口指针
   - 直接写入
     `TSSLCertificateArray`
     保持引用计数
   - `TWinSSLCertificate.LoadFromStream`
     在 DER 失败时
     fallback 到 PEM
4. 验证：
   - push 后
     等待
     `WinSSL Runtime Gate`
   - 看
     `test_winssl_certstore`
     所在 suite
     是否真实通过

## Expected Result

- 静态上：
  - WinSSL chain builder
    不再有
    interface lifetime hole
- 远端 Windows runtime 上：
  - certstore test
    的 partial/full chain
    新 contract 通过
  - 若断言失败，
    suite 会真实红灯
