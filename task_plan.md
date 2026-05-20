# Task Plan - Interface Design And Backend Implementation Verification

## Goal

全面验证 `fafafa.ssl` 的公共接口设计、门面/工厂/builder/config 语义、以及各 backend 实现与 capability 发布是否一致；把发现写成可复用记录，并在边界清晰时直接修复高价值问题，避免后续反复从旧 release / old roadmap 入口重新拉起。

> note:
> - 本轮用户要求“执行一个 goal 全面的验证并记录”。
> - 线程级 goal 当前仍处于 active 状态；这份 `task_plan.md` 与新增 `docs/plans/...` 继续作为该总 goal 下各个 focused 批次的权威执行记录。

## Current Status

- [completed] `winssl cert verifyex custom trust engine`
  当前 focused 目标：
  - 把
    `WinSSL`
    证书级
    `Verify`
    /
    `VerifyEx`
    对
    `ACAStore`
    的语义
    从
    “只是 additional store”
    收紧成真正可作为 trust anchor 的 public truth
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-winssl-cert-verifyex-custom-trust-engine.md`
  - 修改实现：
    - `src/fafafa.ssl.winssl.base.pas`
    - `src/fafafa.ssl.winssl.api.pas`
    - `src/fafafa.ssl.winssl.certificate.pas`
  - 修改 focused test：
    - `tests/winssl/test_winssl_cert_verify_ex.pas`
  当前实施判断：
  - 上一批 push 后的
    Windows runtime truth
    已经证明：
    - `ACAStore`
      只作为
      `hAdditionalStore`
      时，
      不会把
      `ca_cert.pem`
      自动视为 trusted root
    - 因而
      `expired-signer.pem`
      的 expiry lane
      被
      `CERT_E_UNTRUSTEDROOT`
      抢先遮住
  - 继续依赖
    `CurrentUser\ROOT`
    workaround
    会让 focused contract
    依赖系统状态，
    不是稳定 public truth
  - 所以这批最小正确修法是：
    - 给 WinSSL 补
      `CERT_CHAIN_ENGINE_CONFIG`
      相关绑定
    - 对 custom store
      创建专用 chain engine
    - 通过
      `hExclusiveRoot`
      +
      `cAdditionalStore`
      同时提供 trust anchor 与建链来源
    - 把 expiry 测试
      收回纯 memory-store fixture
  当前 focused proof：
  - `git diff --check`
    - PASS
  - 本地限制：
    - Linux 环境无法本地编译/运行
      WinSSL
    - 真正的运行时证明
      仍需看 push 后的
      GitHub Actions
      `WinSSL Runtime Gate`
  最新远端反馈：
  - 新 run
    `26152137388`
    已确认：
    - 前置 compile / quick smoke / wave-b gate
      全部通过
    - 只在
      `WinSSL Certificate VerifyEx Flag Parity`
      里失败
  - 更具体地说：
    - 第一次
      `VerifyEx(..., [], ...)`
      已经正确返回
      `Certificate has expired`
    - 第二次
      `VerifyEx(..., [sslCertVerifyIgnoreExpiry], ...)`
      直接打出
      `EAccessViolation`
  - 这把当前 residual
    收窄成了
    custom chain-engine helper
    的实现细节，
    而不是 trust-direction 本身：
    - 当前 helper
      把
      `CERT_CHAIN_ENGINE_CONFIG.rghAdditionalStore`
      指到了栈上的临时数组
    - follow-up
      应改成：
      - engine 只持有
        `hExclusiveRoot`
      - 每次
        `CertGetCertificateChain(...)`
        再显式传
        `hAdditionalStore`
  最新远端反馈（再下一轮）：
  - 新 run
    `26152785337`
    证明上一枪 still not enough：
    - 去掉 helper-local
      `rghAdditionalStore`
      后，
      `EAccessViolation`
      仍然完全原位复现
    - 仍是：
      - baseline
        `VerifyEx(..., [], ...)`
        正常给出
        `Certificate has expired`
      - 一进入
        `VerifyEx(..., [sslCertVerifyIgnoreExpiry], ...)`
        就崩
  - 所以当前 residual
    再次收窄：
    - 问题不在 custom trust engine
      生命周期
    - 更像
      `CERT_CHAIN_POLICY_BASE`
      cert-level
      在
      nonzero `dwFlags`
      下的 native path
      本身不稳定
  - 当前 follow-up
    应改成：
    - 保留 zero-flag native baseline
    - 不再直接依赖
      `CERT_CHAIN_POLICY_PARA.dwFlags`
      去兑现
      `IgnoreExpiry`
      /
      `AllowSelfSigned`
    - 改为：
      - baseline policy
        先产出 native error
      - 然后在
        public contract
        层做窄范围 success override
  - 当前最新静态收口：
    - 现有实现已经切到
      zero-flag native baseline
      + public-contract override
    - 但最新 Windows run
      `26153510516`
      仍在
      `expired/ignore-expiry`
      第二次调用时抛
      `EAccessViolation`
    - 本轮 follow-up
      先最小移除两个潜在干扰项：
      - success override
        路径上的
        `Format(...0x%x...)`
      - focused test
        外层同样的
        `Format(...)`
        结果格式化
    - 同时给
      `tests/winssl/test_winssl_cert_verify_ex.pas`
      增加阶段 trace，
      让下一轮 Windows CI
      能明确区分：
      - 崩在
        `VerifyEx`
        内部
      - 还是崩在
        返回后的结果渲染
  - 最新远端反馈（本轮 follow-up 第一枪）：
    - push
      `406179f`
      的
      Windows run
      `26158271807`
      没有走到 runtime suite，
      而是更早在
      `Run quick WinSSL smoke`
      失败
    - 根因不是新的 runtime truth，
      而是一个直接的编译回归：
      - `src/fafafa.ssl.winssl.certificate.pas`
        里把布尔链改成
        `and then`
      - 当前
        FPC / ObjFPC
        配置下这不是合法语法
      - 具体报错：
        `Illegal expression`
        /
        `Syntax error, "THEN" expected but "(" found`
    - 当前立刻跟进的修复就是：
      - 保留
        trace
        与
        `Format(...)`
        去除
      - 只把
        `and then`
        改回当前仓库可编译的布尔写法
  - 最新远端反馈（第二枪）：
    - 新 run
      `26158902571`
      已证明：
      - `quick smoke`
        恢复为
        PASS
      - `Windows Wave B gate`
        也是
        PASS
      - 失败重新收敛回
        `Run broader WinSSL runtime suite`
        的
        `WinSSL Certificate VerifyEx Flag Parity`
    - 新增 trace
      还给出了更强的边界：
      - 能看到
        `[INFO] VerifyEx start: expired/no-flags/initial`
        和
        `end`
      - 但在第二次
        `expired/ignore-expiry`
        调用前后，
        没有出现
        `VerifyEx end`
      - 说明这次
        `EAccessViolation`
        的确发生在
        `TWinSSLCertificate.VerifyEx`
        函数体内部，
        而不是测试外层结果格式化
    - 当前 follow-up
      继续再缩一层：
      - override success
        路径先不再写
        `DetailedInfo`
        或额外 success
        说明字符串
      - 只返回：
        - `Success=True`
        - `ErrorCode=0`
        - `ChainStatus=0`
      - 目标是验证
        crash
        是否就卡在
        override 成功分支的字符串处理
  - 当前最新静态审查收口：
    - `tests/winssl/test_winssl_cert_verify_ex.pas`
      仍把
      memory-backed store
      持有为
      `TWinSSLCertificateStore`
      类引用，
      然后反复临时转换成
      `ISSLCertificateStore`
      传给
      `VerifyExWithTrace(...)`
      /
      `Verify(...)`
    - 对
      `TInterfacedObject`
      这会制造经典生命周期洞：
      - 第一次调用时的临时接口
        `_AddRef`
        /
        `_Release`
        后，
        对象可能已经析构
      - 第二次调用在真正进入 helper 前，
        仅做参数求值/接口转换
        就可能直接
        `EAccessViolation`
    - 这和最新 Windows trace
      完全一致：
      - 第二次
        `expired/ignore-expiry`
        根本没有打印
        `VerifyEx start`
      - 所以 fault boundary
        比
        `VerifyEx`
        函数体还要更外一层
    - 当前跟进修复：
      - focused WinSSL test
        改为始终用
        `ISSLCertificateStore`
        接口持有
        memory store
      - 先修掉测试生命周期洞，
        再重新验证
        WinSSL implementation
        自身是否还剩真实 AV
  - 最新远端反馈（闭环证明）：
    - 新 run
      `26159931322`
      `WinSSL Runtime Gate`
      已全绿
    - 同批
      `26159931316`
      `CI`
      也全绿
    - 这说明：
      - 前一轮 lingering
        `EAccessViolation`
        并不是
        WinSSL
        `VerifyEx`
        实现残余
      - 真正根因就是
        focused test
        把
        `TWinSSLCertificateStore`
        当类引用持有导致的
        `TInterfacedObject`
        生命周期洞
      - 一旦改成接口持有，
        `quick smoke`
        /
        `Wave B gate`
        /
        `broader runtime suite`
        全部恢复为
        PASS
  - 当前 batch
    done criteria
    已满足：
    - custom store
      trust-anchor
      public truth
      已被 Windows runtime 证明
    - `Verify`
      /
      `VerifyEx`
      focused WinSSL parity 契约已进入并通过真实
      Windows suite
    - 后续主线应切回
      更高层的
      public interface
      / backend completeness
      / docs completeness
      收口
- [completed] `winssl certificate verifyex flag parity`
  当前 focused 目标：
  - 把
    `WinSSL`
    证书对象的
    `ISSLCertificate.VerifyEx`
    在三条已发布 flags 上的 live 语义收紧成与其它 backend 一致的 public truth：
    - `sslCertVerifyIgnoreExpiry`
    - `sslCertVerifyAllowSelfSigned`
    - `sslCertVerifyStrictChain`
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-winssl-certificate-verifyex-flag-parity.md`
  - 修改实现：
    - `src/fafafa.ssl.winssl.certificate.pas`
  - 修改 focused tests / runtime suite：
    - `tests/winssl/test_winssl_cert_verify_ex.pas`
    - `tests/winssl/test_winssl_cert_verify_ex.lpi`
    - `tests/run_winssl_tests.ps1`
  当前实施判断：
  - 这次真实 residual
    不在
    `WinSSL connection`
    路径，
    而是在证书级
    `VerifyEx`
    自己：
    - `IgnoreExpiry`
      之前没有映射到
      `CERT_CHAIN_POLICY_IGNORE_NOT_TIME_VALID_FLAG`
    - `AllowSelfSigned`
      之前没有兑现到
      cert-level
      `VerifyEx`
    - `StrictChain`
      之前只是 API round-trip，
      没有 leaf `serverAuth` EKU 的 fail-closed
  - 同时还存在一个 workflow gap：
    - `tests/winssl/test_winssl_cert_verify_ex.lpi`
      还残留了
      `TargetOS=linux`
    - `tests/run_winssl_tests.ps1`
      之前也没有真正执行这个 focused test
  - 首轮 Windows CI
    进一步证明了一个
    WinSSL-specific 细节：
    - `CERT_CHAIN_POLICY_BASE`
      下，
      memory-backed additional store
      不会自动把那张 CA
      当成 trusted root
    - 所以
      `expired-signer.pem + ca_cert.pem`
      会先暴露
      `untrusted root`
      而不是 expiry
    - 因而这批里
      `IgnoreExpiry`
      不能直接靠 additional store 设计
  - 第二轮 Windows CI
    又补充了另一条真实边界：
    - 运行时生成的
      expired self-signed leaf
      一触发
      `AllowSelfSigned`
      分支
      就在
      WinSSL cert-level `VerifyEx`
      上打出
      `EAccessViolation`
    - 所以这批最终稳定契约
      已收成：
      - 临时把
        `ca_cert.pem`
        加入
        `CurrentUser\ROOT`
      - 让
        `expired-signer.pem`
        真正只剩 expiry 变量
      - 再验证
        `IgnoreExpiry`
        是否真正改变结果
  - 最小正确修法
    不是重开连接层 hostname / SSL policy，
    而是：
    - 在 cert-level
      `VerifyEx`
      上补齐
      per-call policy flags
    - 对
      `StrictChain`
      明确要求
      `serverAuth`
      EKU
    - 把 focused runtime contract
      接回
      Windows suite
  当前 focused proof：
  - `git diff --check`
    - PASS
  - `xmllint --noout tests/winssl/test_winssl_cert_verify_ex.lpi`
    - PASS
  - 本地限制：
    - 当前 Linux 环境没有
      `pwsh`
      / Windows runtime，
      所以真正的编译与运行证明
      交给 push 后的
      `WinSSL Runtime Gate`
  当前批收口后的默认下一步：
  - 看 push 后的
    `WinSSL Runtime Gate`
    是否一次性把这组 cert-level residual 关掉
  - 若绿色，
    再回到
    `ISSLConnection`
    /
    `TSSLConfig`
    /
    `ISSLServerConnection`
    这组三个更大的 public completeness 主线
- [completed] `freepascal certificate verifyex selfsigned/ocsp parity`
  当前 focused 目标：
  - 把
    `FreePascal`
    证书对象的
    `ISSLCertificate.VerifyEx`
    在两个已发布 flags 上的 live 语义收紧成与其它 backend 一致的 public truth：
    - `sslCertVerifyAllowSelfSigned`
    - `sslCertVerifyCheckOCSP`
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-freepascal-certificate-verifyex-selfsigned-ocsp-parity.md`
  - 修改实现：
    - `src/fafafa.ssl.freepascal.lib.pas`
  - 新增 focused tests：
    - `tests/freepascal/test_freepascal_verify_ex_flag_parity_contract.pas`
  当前实施判断：
  - `FreePascal certificate.VerifyEx`
    这次最真实的 residual
    不是
    `IgnoreExpiry`
    /
    `StrictChain`
    路径，
    而是：
    - `AllowSelfSigned`
      被静默忽略
    - `CheckOCSP`
      没有 fail-closed
      分支
  - 最小正确修法
    不是重写整个
    `certchain`
    公共层，
    而是：
    - 仅在
      self-signed leaf
      且显式请求
      `AllowSelfSigned`
      时，
      对当前调用放行
    - 对
      `CheckOCSP`
      比照其它 backend
      明确 fail-closed
  当前 focused proof：
  - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_verify_ex_flag_parity_contract_units -FEtmp/test_freepascal_verify_ex_flag_parity_contract_units -otmp/test_freepascal_verify_ex_flag_parity_contract_units/test_freepascal_verify_ex_flag_parity_contract tests/freepascal/test_freepascal_verify_ex_flag_parity_contract.pas`
    - PASS
  - `./tmp/test_freepascal_verify_ex_flag_parity_contract_units/test_freepascal_verify_ex_flag_parity_contract`
    - PASS
    - 同时覆盖：
      - `AllowSelfSigned`
        真正生效
      - `CheckOCSP`
        明确 fail-closed
  - `git diff --check`
    - PASS
  当前批收口后的默认下一步：
  - 继续沿
    `certificate.VerifyEx`
    的 published-flag parity
    审 residual gaps
  - 优先看：
    - `WinSSL certificate.VerifyEx`
      当前静态 residual：
      - `IgnoreExpiry`
      - `AllowSelfSigned`
      - `StrictChain`
    - 其后再回看
      `OpenSSL`
      的
      `CheckRevocation`
      /
      `CheckCRL`
      per-call scope
      残余风险
- [completed] `openssl certificate verifyex store flag isolation`
  当前 focused 目标：
  - 把
    `OpenSSL`
    证书对象的
    `ISSLCertificate.VerifyEx`
    在两个按次 exception flags 上的 live 语义收紧成真正的 public truth：
    - `sslCertVerifyIgnoreExpiry`
    - `sslCertVerifyAllowSelfSigned`
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-openssl-certificate-verifyex-store-flag-isolation.md`
  - 修改实现：
    - `src/fafafa.ssl.openssl.certificate.pas`
  - 新增 focused tests：
    - `tests/openssl/test_openssl_verify_ex_store_flag_isolation_contract.pas`
  当前实施判断：
  - 重新跑当前未提交实现后，
    先证实了：
    `IgnoreExpiry`
    通过
    `X509_STORE_CTX`
    参数路径
    已经不再污染同一个 store
    上的后续调用
  - 真正的 residual
    不是继续扩 OpenSSL binding，
    而是：
    `sslCertVerifyAllowSelfSigned`
    虽然对外已发布，
    但旧实现靠
    `X509_V_FLAG_PARTIAL_CHAIN`
    并没有真正放行 self-signed leaf
  - 最小正确修法
    不是继续把
    `AllowSelfSigned`
    塞进 shared/native verify flags，
    而是：
    - 先保留 native
      `X509_verify_cert`
      作为基础验证真相
    - 仅在
      leaf 确认 self-signed
      且错误属于
      self-signed / trust failure
      时，
      对当前调用做窄范围 override
  当前 focused proof：
  - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_openssl_verify_ex_store_flag_isolation_contract_units -FEtmp/test_openssl_verify_ex_store_flag_isolation_contract_units -otmp/test_openssl_verify_ex_store_flag_isolation_contract_units/test_openssl_verify_ex_store_flag_isolation_contract tests/openssl/test_openssl_verify_ex_store_flag_isolation_contract.pas`
    - PASS
  - `./tmp/test_openssl_verify_ex_store_flag_isolation_contract_units/test_openssl_verify_ex_store_flag_isolation_contract`
    - PASS
    - 同时覆盖：
      - `IgnoreExpiry` 不泄漏
      - `AllowSelfSigned` 生效且不泄漏
  - `git diff --check`
    - PASS
  当前批收口后的默认下一步：
  - 继续沿
    `certificate.VerifyEx`
    的 published-flag parity
    审 residual gaps
  - 优先看：
    - `FreePascal`
      /
      `WinSSL`
      的
      `certificate.VerifyEx`
      是否仍有
      已发布 flag
      只做 round-trip
    - `OpenSSL`
      的
      `CheckRevocation`
      /
      `CheckCRL`
      是否还存在
      同类 per-call scope
      残余风险
- [completed] `openssl certificate verifyex strict-chain parity`
  当前 focused 目标：
  - 把
    `OpenSSL`
    证书对象的
    `ISSLCertificate.VerifyEx`
    在已发布 flag
    `sslCertVerifyStrictChain`
    上的 live 语义
    收紧成与其它 backend 一致的 public truth
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-openssl-certificate-verifyex-strict-chain-parity.md`
  - 修改实现：
    - `src/fafafa.ssl.openssl.certificate.pas`
  - 新增 focused tests：
    - `tests/openssl/test_openssl_verify_ex_strict_chain_contract.pas`
  当前实施判断：
  - 真正的 residual gap
    不是
    `OpenSSL`
    整体 verify pipeline 崩坏，
    而是
    `certificate.VerifyEx`
    明显缺了一条
    `sslCertVerifyStrictChain`
    分支
  - 现成 fixture
    `tests/certificate/test_certs/signer_cert.pem`
    本身就没有
    `extendedKeyUsage`
    扩展，
    这让它成为非常干净的 strict-chain RED
  - 修法上的关键点
    不是简单信任
    `GetExtendedKeyUsage()`，
    因为在
    `OpenSSL`
    这层，
    “没有 EKU 扩展”
    不能被误当成
    “允许 serverAuth”
  当前 focused proof：
  - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_openssl_verify_ex_strict_chain_contract_units -FEtmp/test_openssl_verify_ex_strict_chain_contract_units -otmp/test_openssl_verify_ex_strict_chain_contract_units/test_openssl_verify_ex_strict_chain_contract tests/openssl/test_openssl_verify_ex_strict_chain_contract.pas`
    - PASS
  - `./tmp/test_openssl_verify_ex_strict_chain_contract_units/test_openssl_verify_ex_strict_chain_contract`
    - PASS
  - `git diff --check`
    - PASS
  当前批收口后的默认下一步：
  - 继续沿
    certificate VerifyEx
    的已发布 flag parity
    审 residual gaps
  - 优先看：
    - `OpenSSL`
      的
      `IgnoreExpiry`
      /
      `AllowSelfSigned`
      是否存在
      shared store flag
      污染
    - `FreePascal`
      /
      `WinSSL`
      的
      `certificate.VerifyEx`
      是否仍有
      已发布 flag
      只做 round-trip
- [completed] `optional backends certificate verify flags expiry/self-signed parity`
  当前 focused 目标：
  - 把
    `MbedTLS`
    /
    `WolfSSL`
    证书对象的
    `VerifyEx`
    在两个已发布 exception flags 上的 live 语义收紧成一致的 public truth：
    - `sslCertVerifyIgnoreExpiry`
    - `sslCertVerifyAllowSelfSigned`
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-optional-backends-certificate-verify-flags-expiry-selfsigned-parity.md`
  - 新增 fixture：
    - `tests/certs/expired-signer.pem`
  - 修改实现：
    - `src/fafafa.ssl.mbedtls.certificate.pas`
  - 修改 focused tests：
    - `tests/test_wolfssl_framework.pas`
    - `tests/test_mbedtls_framework.pas`
  当前实施判断：
  - `WolfSSL`
    在这条 lane 上
    已经是稳定 control group，
    不是真 bug 根因
  - 真正的 residual gap
    是
    `MbedTLS VerifyEx`
    之前虽然接受
    `IgnoreExpiry`
    /
    `AllowSelfSigned`
    这两个 flags，
    但并没有根据 native verify bits
    真正改变结果
  - 最小正确修法
    不是绕开
    `mbedtls_x509_crt_verify`
    做大重构，
    而是：
    - 对
      `MBEDTLS_X509_BADCERT_EXPIRED`
      /
      `MBEDTLS_X509_BADCERT_FUTURE`
      仅在请求
      `sslCertVerifyIgnoreExpiry`
      时掩码放行
    - 对 self-signed leaf 的
      `MBEDTLS_X509_BADCERT_NOT_TRUSTED`
      仅在请求
      `sslCertVerifyAllowSelfSigned`
      时掩码放行
  当前 focused proof：
  - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_mbedtls_framework_units -FEtmp/test_mbedtls_framework_units -otmp/test_mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas`
    - PASS
  - `./tmp/test_mbedtls_framework_units/test_mbedtls_framework`
    - PASS
    - `211 passed / 0 failed`
  - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_framework_units -FEtmp/test_wolfssl_framework_units -otmp/test_wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas`
    - PASS
  - `./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
    - PASS
    - `227 passed / 0 failed`
  - `git diff --check`
    - PASS
  当前批收口后的默认下一步：
  - 继续沿
    optional backend
    certificate verification
    审 residual parity
  - 优先看
    其余 backend
    是否还存在
    已发布
    `VerifyEx`
    flags
    只做 API round-trip、
    但没有真正改变 live result
- [completed] `optional backends certificate verification truth`
  当前 focused 目标：
  - 把
    `WolfSSL`
    /
    `MbedTLS`
    证书对象的
    `Verify`
    /
    `VerifyEx`
    从
    假成功 / flag 静默忽略 / 空壳结果
    收紧成
    更接近真实验证语义的 public truth
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-optional-backends-certificate-verification-truth.md`
  - 新增 fixture：
    - `tests/certs/ca-subject-imposter.pem`
  - 修改实现：
    - `src/fafafa.ssl.wolfssl.certificate.pas`
    - `src/fafafa.ssl.mbedtls.certificate.pas`
    - `src/fafafa.ssl.openssl.certificate.pas`
    - `src/fafafa.ssl.winssl.certificate.pas`
    - `src/fafafa.ssl.freepascal.lib.pas`
  - 修改 focused tests：
    - `tests/test_wolfssl_framework.pas`
    - `tests/test_mbedtls_framework.pas`
  当前实施判断：
  - `WolfSSL`
    当前最高价值的实现缺口
    不是小文档漂移，
    而是
    `Verify`
    仍用
    issuer/subject
    文本命中
    代替真实签名验证
  - `MbedTLS`
    这批顺手暴露了第二个真问题：
    `TMbedTLSCertificateStore.AddCertificate`
    之前只进
    interface list，
    没有同步进
    native CA chain，
    导致
    `Verify`
    /
    `VerifyEx`
    对真实 CA
    也会失败
  - 新增 RED
    还顺手打出了一个
    shared safety smell：
    `TSSLCertVerifyResult`
    含有
    `string`
    字段，
    多 backend
    的
    `VerifyEx`
    还在用
    `FillChar`
    初始化
  当前 focused proof：
  - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_framework_units -FEtmp/test_wolfssl_framework_units -otmp/test_wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas`
    - PASS
  - `./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
    - PASS
    - `217 passed / 0 failed`
  - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_mbedtls_framework_units -FEtmp/test_mbedtls_framework_units -otmp/test_mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas`
    - PASS
  - `./tmp/test_mbedtls_framework_units/test_mbedtls_framework`
    - PASS
    - `201 passed / 0 failed`
  - `git diff --check`
    - PASS
  当前批收口后的默认下一步：
  - 继续沿
    optional backend
    certificate verification
    审 residual parity
  - 优先看
    `MbedTLS`
    /
    `WolfSSL`
    是否还存在
    strict-chain /
    OCSP /
    chain-status
    结果字段细节漂移
- [completed] `optional backends certificate time truth`
  当前 focused 目标：
  - 把
    `MbedTLS`
    /
    `WolfSSL`
    证书对象的
    time surface
    从
    默认值壳 / 弱解析路径
    收紧成
    更接近真实
    X.509 validity
    的 public truth
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-optional-backends-certificate-time-truth.md`
  - 修改实现：
    - `src/fafafa.ssl.mbedtls.certificate.pas`
  - 修改 focused tests：
    - `tests/test_mbedtls_framework.pas`
    - `tests/test_wolfssl_framework.pas`
  当前实施判断：
  - 这批最初怀疑的
    `WolfSSL DER/native`
    时间丢失
    在 focused RED
    里并没有出现；
    它更适合作为
    cross-backend control proof
  - 真正红灯的是
    `MbedTLS`
    空证书仍会把
    `GetNotBefore`
    /
    `GetNotAfter`
    伪造成
    `Now +/- 365`
  - 更稳的最小修复
    不是再补 native text parser，
    而是：
    - `GetNotBefore/GetNotAfter`
      优先复用
      `TryLoadX509Parser(...)`
    - unknown time
      回到
      `0`
    - `IsExpired`
      /
      `GetDaysUntilExpiry`
      改成 fail-closed
  当前 focused proof：
  - `gh run view 26143487129 --json status,conclusion,jobs,url`
    - PASS
    - `CI`
      当前已
      `success`
  - `fpc ... tests/test_mbedtls_framework.pas`
    - PASS
  - `./tmp/test_mbedtls_framework_units/test_mbedtls_framework`
    - FAIL -> PASS
  - `fpc ... tests/test_wolfssl_framework.pas`
    - PASS
  - `./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
    - PASS
  - `git diff --check`
    - PASS
  当前批收口后的默认下一步：
  - 继续沿
    optional backend
    certificate surface
    审 residual completeness
  - 优先找
    仍会
    “伪造默认真相”
    或
    “已发布但未被 focused contract 钉住”
    的下一条非文档 lane
- [completed] `optional backends certificate version truth`
  当前 focused 目标：
  - 把
    `MbedTLS`
    /
    `WolfSSL`
    的
    `ISSLCertificate.GetVersion`
    从
    “默认 v3”
    的弱证据
    收紧成
    有真实非 v3 fixture
    覆盖的 public truth
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-optional-backends-certificate-version-truth.md`
  - 新增 fixture：
    - `tests/certs/version1-cert.pem`
  - 修改实现：
    - `src/fafafa.ssl.mbedtls.certificate.pas`
  - 修改 focused tests：
    - `tests/test_mbedtls_framework.pas`
    - `tests/test_wolfssl_framework.pas`
  当前实施判断：
  - 这批不是
    新文档漂移，
    也不是
    broader certificate redesign；
    而是
    `MbedTLS.GetVersion`
    仍停在
    固定 `3`
  - `WolfSSL`
    已经有
    non-default version path，
    所以更合适的收口方式是：
    - 用同一个真实
      v1 fixture
      做 cross-backend 对照
    - 只修
      `MbedTLS`
      这一处实现
  当前 focused proof：
  - `openssl req -new -x509 -x509v1 ...`
    生成真实
    `Version: 1`
    fixture
    - PASS
  - `fpc ... tests/test_mbedtls_framework.pas`
    - PASS
  - `./tmp/test_mbedtls_framework_units/test_mbedtls_framework`
    - FAIL -> PASS
  - `fpc ... tests/test_wolfssl_framework.pas`
    - PASS
  - `./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
    - PASS
  - `git diff --check`
    - PASS
  当前批收口后的默认下一步：
  - 继续沿
    certificate surface
    审 residual completeness
  - 优先找
    “已发布但仍缺强证据”
    的下一条非文档 lane
- [completed] `winssl certificate identity getter full-dn truth`
  当前 focused 目标：
  - 把
    `TWinSSLCertificate.GetSubject`
    /
    `GetIssuer`
    从当前
    simple display name
    语义
    收口到和
    `WinSSL certstore`
    以及其它 backend
    一致的
    full DN truth
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-winssl-certificate-identity-getter-full-dn-truth.md`
  - 新增 focused static contract：
    - `tests/scripts/test_winssl_certificate_identity_getter_truth_contract.sh`
  - 修改实现：
    - `src/fafafa.ssl.winssl.certificate.pas`
  - 修改 Windows runtime suite：
    - `tests/winssl/test_winssl_certstore.pas`
  当前实施判断：
  - 这批不是
    `FindBySubject`
    /
    `FindByIssuer`
    再次失真，
    而是
    query lane
    已经对齐 full DN 之后，
    public getter
    仍停在更弱语义
  - 更稳的修法
    不是再走
    `CertGetNameStringW(..., CERT_NAME_SIMPLE_DISPLAY_TYPE, ...)`，
    而是直接复用
    native
    `CERT_INFO.Subject/Issuer`
    的 X.500 name blob
  当前 focused proof：
  - `bash -n tests/scripts/test_winssl_certificate_identity_getter_truth_contract.sh`
    - PASS
  - `bash tests/scripts/test_winssl_certificate_identity_getter_truth_contract.sh`
    - FAIL -> PASS
  - `git diff --check`
    - PASS
  当前批收口后的默认下一步：
  - push 当前 batch
  - 观察
    `CI`
    /
    `WinSSL Runtime Gate`
    是否在真实 Windows runner
    上继续转绿
- [completed] `migration guide raw connection truth alignment`
  当前 focused 目标：
  - 把
    `MIGRATION_GUIDE`
    在 raw
    `ISSLConnection`
    客户端示例上的 current truth
    重新钉实：
    - 用
      `Supports(..., ISSLClientConnection, ...)`
      进入 client-role surface
    - 保持 generic
      compatibility wording，
      不再回退到
      frozen surface
      literal-name 列表
  - 同步修掉
    `test_migration_guide_active_truth_contract.sh`
    自己仍要求旧 literal-name 真相
    的 stale contract
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-migration-guide-raw-connection-truth-alignment.md`
  - 修改文档：
    - `docs/guides/MIGRATION_GUIDE.md`
  - 修改 focused contract：
    - `tests/scripts/test_migration_guide_active_truth_contract.sh`
  当前实施判断：
  - 这批不是新的
    runtime/interface bug，
    而是上一轮
    `MIGRATION_GUIDE`
    收紧之后，
    raw-connection 示例
    与旧 contract
    之间形成了新的真相错位
  - 正确收口方式是：
    - guide 继续不点名 frozen surface literal 名称
    - 但 raw
      `ISSLConnection`
      示例必须明确展示：
      `Supports(..., ISSLClientConnection, ...)`
      + per-connection SNI
  当前 focused proof：
  - `bash -n tests/scripts/test_migration_guide_active_truth_contract.sh`
    - PASS
  - `bash tests/scripts/test_migration_guide_active_truth_contract.sh`
    - PASS
  - `bash -n tests/scripts/test_tsslconfig_servername_surface_truth_contract.sh`
    - PASS
  - `bash tests/scripts/test_tsslconfig_servername_surface_truth_contract.sh`
    - PASS
  - `bash -n tests/scripts/test_withsni_surface_truth_contract.sh`
    - PASS
  - `bash tests/scripts/test_withsni_surface_truth_contract.sh`
    - PASS
  - `bash -n tests/scripts/test_direct_context_servername_surface_truth_contract.sh`
    - PASS
  - `bash tests/scripts/test_direct_context_servername_surface_truth_contract.sh`
    - PASS
  - `git diff --check`
    - PASS
  当前批收口后的默认下一步：
  - 继续审
    `ISSLConnection`
    / `TSSLConfig`
    主线里
    仍然存在的
    active-guide / canonical-reference
    残余 drift
- [completed] `context servername migration guide drift closeout`
  当前 focused 目标：
  - 收掉
    `docs/guides/MIGRATION_GUIDE.md`
    把 frozen
    `context-level ServerName`
    compatibility surface
    literal 名称
    重新写回活跃指南层
    的文档漂移
  - 顺手把
    direct context
    的 shell contract
    从
    “只拦调用示例”
    收紧到
    “literal API 名称也只能留在 API_REFERENCE”
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-context-servername-migration-guide-drift-closeout.md`
  - 修改文档：
    - `docs/guides/MIGRATION_GUIDE.md`
  - 修改 focused contract：
    - `tests/scripts/test_direct_context_servername_surface_truth_contract.sh`
  当前实施判断：
  - 当前 drift
    不是 runtime regression，
    而是 active guide
    把已经冻结的 compatibility surface
    literal 名称
    又教回了指南层
  - 现有
    `test_tsslconfig_servername_surface_truth_contract.sh`
    与
    `test_withsni_surface_truth_contract.sh`
    已经能抓到这条红灯；
    direct context
    自己的 contract
    还缺一条同向的 literal-name guard
  当前 focused proof：
  - `bash -n tests/scripts/test_tsslconfig_servername_surface_truth_contract.sh`
    - PASS
  - `bash tests/scripts/test_tsslconfig_servername_surface_truth_contract.sh`
    - PASS
  - `bash -n tests/scripts/test_withsni_surface_truth_contract.sh`
    - PASS
  - `bash tests/scripts/test_withsni_surface_truth_contract.sh`
    - PASS
  - `bash -n tests/scripts/test_direct_context_servername_surface_truth_contract.sh`
    - PASS
  - `bash tests/scripts/test_direct_context_servername_surface_truth_contract.sh`
    - PASS
  - `git diff --check`
    - PASS
  当前批收口后的默认下一步：
  - 继续回到
    `context-level ServerName`
    剩余 public surface
    的最终 slimming / freeze
  - 若继续沿文档/contract 真相线推进，
    下一个值得审的是：
    - `API_REFERENCE` 顶部 compatibility note
      与
      `MIGRATION_GUIDE`
      的职责边界
- [completed] `context servername dead seam removal`
  当前 focused 目标：
  - 把
    `context-level ServerName`
    迁移主线里
    已经恒为 no-op 的
    shared compatibility seam
    从源码里彻底删掉
  - 避免后续审查
    再把
    `src/fafafa.ssl.context.compat.pas`
    误判成
    仍然有效的 inherited fallback 兼容桥
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-context-servername-dead-seam-removal.md`
  - 删除实现：
    - `src/fafafa.ssl.context.compat.pas`
  - 修改实现：
    - `src/fafafa.ssl.openssl.connection.pas`
    - `src/fafafa.ssl.wolfssl.connection.pas`
    - `src/fafafa.ssl.mbedtls.connection.pas`
    - `src/fafafa.ssl.winssl.connection.pas`
  - 修改 focused contract：
    - `tests/scripts/test_context_server_name_compat_shim_contract.sh`
  - 修改路线图 / 报告：
    - `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
    - `docs/plans/2026-05-18-shared-client-context-sni-fallback-cut.md`
    - `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
  当前实施判断：
  - 这批不是新增功能，
    也不是再改
    compatibility behavior；
    而是删除
    “行为已经切断、源码仍保留”
    的 dead seam
  - helper 继续存在的代价
    已经大于价值：
    - 它不再转发 context-level `ServerName`
    - 却持续误导后续审查，
      让人以为 backend 还保留 inherited fallback
  当前 focused proof：
  - `bash -n tests/scripts/test_context_server_name_compat_shim_contract.sh`
    - PASS
  - `bash tests/scripts/test_context_server_name_compat_shim_contract.sh`
    - PASS
  - `tests/test_cross_backend_client_context_server_name_clarification.pas`
    - PASS (`20 passed, 0 failed, 1 skipped`)
  - `git diff --check`
    - PASS
  当前批收口后的默认下一步：
  - 回到
    `context-level ServerName`
    剩余 public surface
    的最终 slimming / freeze：
    - `TSSLConfig.ServerName`
    - direct `ISSLContext.SetServerName/GetServerName`
    - `WithSNI(...)`
- [completed] `winssl certstore dn query runtime closeout`
  当前 focused 目标：
  - 收掉
    GitHub Windows runtime
    上仅剩的
    `WinSSL CertStore DN Query Contract`
    红灯
  - 让
    `FindBySubject`
    / `FindByIssuer`
    对 full DN component query、
    loose normalized query、
    plain text fragment query
    都继续满足当前 repo 共享契约
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-winssl-certstore-dn-query-runtime-closeout.md`
  - 修改实现：
    - `src/fafafa.ssl.winssl.certstore.pas`
  - 修改 focused test：
    - `tests/winssl/test_winssl_certstore.pas`
  当前预判：
  - 当前失败证据
    已收敛到：
    GitHub Actions
    `WinSSL Runtime Gate`
    run `26139989408`
    的
    `TestDeterministicDNQueryContract`
  - 当前红灯只剩两条断言：
    - `按归一化主题片段查询成功`
    - `按归一化颁发者片段查询成功`
  - 静态阅读源码后确认：
    `FindBySubject`
    / `FindByIssuer`
    当前归一化的是
    `TWinSSLCertificate.GetSubject`
    / `GetIssuer`
    的返回值
  - 但
    `TWinSSLCertificate.GetSubject`
    / `GetIssuer`
    现在走的是
    `CERT_NAME_SIMPLE_DISPLAY_TYPE`
    而不是 full X.500 DN，
    所以像
    `CN=Test Signer,O=Test Org`
    这类 component query
    在 WinSSL 上天然匹配不到
  当前实施策略：
  - 先把
    `TestDeterministicDNQueryContract`
    的 query
    明确改成
    逆序 component 变体，
    确保它真正锁住
    order-insensitive DN contract
  - 最小修复：
    - 在
      `TWinSSLCertificateStore`
      内直接从
      native `CERT_CONTEXT`
      读取
      full subject / issuer DN
    - 用
      `CertNameToStrW(..., CERT_X500_NAME_STR ...)`
      生成 canonical candidate
    - 保留
      normalized exact-first
      再加
      component-subset / substring
      fallback
  当前总路线图进度：
  - `接口设计`
    继续把
    certstore query
    的 shared contract
    真正收口到
    WinSSL runtime
  - `测试完整性`
    现在重点不再是
    harness / fixture path，
    而是
    Windows 上剩余的
    DN semantic drift
  当前最终收口证据：
  - 首次 push
    `77e55dc`
    虽然方向正确，
    但
    GitHub Actions
    `WinSSL Runtime Gate`
    run `26140587186`
    在 quick smoke
    编译期暴露出：
    `CERT_CONTEXT.pCertInfo`
    需要先显式转成
    `PCERT_INFO`
  - 二次修正提交：
    `b4a93d3`
    `fix(winssl): cast cert info before dn extraction`
    后：
    - `CI`
      run `26140837184`
      `success`
    - `WinSSL Runtime Gate`
      run `26140837156`
      `success`
  - 第二次 Windows gate
    已明确通过：
    - `Run quick WinSSL smoke`
    - `Run Windows Wave B gate`
    - `Run broader WinSSL runtime suite`
  当前批收口后的默认下一步：
  - 回到更大的
    interface-design / backend completeness
    主线：
    - `ISSLConnection`
    - `TSSLConfig`
    - `ISSLServerConnection`
  - 如果继续沿 cert surface 深挖，
    一个值得单独排期的问题是：
    `TWinSSLCertificate.GetSubject`
    / `GetIssuer`
    目前仍更像 display-oriented getter，
    是否要继续和其它 backend 的 full-DN surface
    做公开语义对齐
- [completed] `winssl certstore chain runtime contract`
  当前 focused 目标：
  - 补齐
    `TWinSSLCertificateStore.BuildCertificateChain`
    的
    partial/full chain
    runtime contract
  - 同时修掉
    结果收集阶段
    把 `ISSLCertificate`
    裸指针塞进 `TList`
    的接口保活风险
  当前最终收口证据：
  - 已落地提交：
    - `7f9dfd6`
      `fix(winssl): harden certstore chain runtime`
    - `46fb048`
      `fix(winssl): accept pem certificate files`
    - `995417b`
      `fix(winssl): resolve certstore fixture paths`
  - GitHub Windows runtime
    已证明：
    - partial/full chain
      contract 通过
    - PEM fixture
      加载通过
    - fixture path
      解析通过
  - 同一条 runtime
    最终把剩余红灯
    精确收敛到
    `DN Query Contract`
- [completed] `openssl certstore full-chain termination contract`
  当前 focused 目标：
  - 修掉
    `TOpenSSLCertificateStore.BuildCertificateChain`
    把整个 store
    直接当 trusted store
    导致 intermediate
    被过早当成 trust anchor
    的终止语义错误
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-openssl-certstore-full-chain-termination-contract.md`
  - 新增 focused test：
    - `tests/openssl/test_openssl_certstore_chain_contract.pas`
  - 预计修改实现：
    - `src/fafafa.ssl.openssl.certstore.pas`
  当前预判：
  - 现在
    `BuildCertificateChain`
    直接：
    - `SetTrustedStore(Self)`
    - `BuildChain(ACert, Result)`
  - 但 shared verifier
    的
    `IsRootCertificate`
    只看：
    - `FTrustedStore.Contains(CurrentCert)`
  - 所以只要
    intermediate
    在 store 里，
    它就会在第二跳
    被提前当成 anchor，
    链停在
    `leaf -> intermediate`
  当前实施策略：
  - 先用 OpenSSL focused contract
    锁两条语义：
    - store 只有 intermediate
      时返回最小链
      `leaf -> intermediate`
    - store 同时有
      `intermediate + root`
      时继续返回完整链
      `leaf -> intermediate -> root`
  - 最小修复：
    - 在
      `BuildCertificateChain`
      内临时拆分 store：
      - self-signed certs
        -> trusted store
      - non-self-signed certs
        -> intermediate store
    - 调 generic verifier
      时显式加上
      `cvoAllowPartialChain`
  当前最终收口证据：
  - RED 明确暴露：
    - store 只有 intermediate
      -> 长度 `2`
    - store 有
      `intermediate + root`
      旧实现仍只返回
      长度 `2`
  - 修复后：
    - partial-chain case
      仍返回
      `leaf -> intermediate`
    - full-chain case
      继续返回
      `leaf -> intermediate -> root`
  focused verification：
  - `tests/openssl/test_openssl_certstore_chain_contract.pas`
    - PASS
  - `git diff --check`
    - PASS
  当前结论：
  - 这条 OpenSSL drift
    的根因不是
    issuer lookup
    少一步，
    而是
    trusted-anchor
    与 intermediate
    的建模边界被混在一个 store surface
  - 修复后，
    `BuildCertificateChain`
    的 partial/full
    两条 public 语义
    已经都被 focused contract 锁住
  当前总路线图进度：
  - `接口设计`
    继续从
    shared verifier
    往
    OpenSSL certstore
    的 trust-anchor
    终止语义收口
  - `后端实现`
    正在补
    `BuildCertificateChain`
    对 full chain / partial chain
    的真实 public contract
  当前下一条真实工作：
  - 继续顺着
    certstore / verifier /
    chain-building
    这条 shared lane
    看其它 backend
    是否仍有
    trust-anchor /
    root-detection /
    partial-chain
    漂移
- [completed] `certchain trusted store subject anchor contract`
  当前 focused 目标：
  - 修掉
    generic
    `TSSLCertificateChainVerifier`
    在 trusted store
    上把 issuer lookup
    走错查询面的 bug
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-certchain-trusted-store-subject-anchor-contract.md`
  - 修改实现：
    - `src/fafafa.ssl.certchain.pas`
  - 新增 focused test：
    - `tests/test_certchain_trusted_store_subject_lookup_contract.pas`
  当前预判：
  - 当前 `FindIssuer`
    对 trusted store
    调用的是
    `FindByIssuer`
  - 这会让
    trusted intermediate anchor
    明明存在，
    仍然构不出链
  当前最终收口证据：
  - 新 test
    生成：
    - root
    - intermediate
    - leaf
  - 然后只把
    `intermediate`
    放进 trusted store
  - 修复后：
    - `BuildChain(leaf)`
      能返回
      `leaf -> intermediate`
  focused verification：
  - `tests/test_certchain_trusted_store_subject_lookup_contract.pas`
    - PASS
  当前结论：
  - 这不是某个 backend
    的局部字符串比较问题，
    而是 shared
    chain verifier
    自己把 trusted-store
    issuer lookup
    指到了错误查询面
  当前总路线图进度：
  - `接口设计`
    已经继续推进到
    shared chain-building core
  - `实现完整性`
    不再只修 backend wrapper，
    开始收 shared verifier
    的真实 drift
  当前下一条真实工作：
  - 审查
    `OpenSSL BuildCertificateChain`
    继续把 entire store
    当 trusted store
    时，
    是否还存在
    trust-anchor / full-chain
    终止语义分叉
- [completed] `winssl certstore test api drift alignment`
  当前 focused 目标：
  - 把
    `tests/winssl/test_winssl_certstore.pas`
    从旧 API 记忆
    拉回到
    当前
    `TWinSSLCertificateStore`
    真实公开面
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-winssl-certstore-test-api-drift-alignment.md`
  - 修改 focused test：
    - `tests/winssl/test_winssl_certstore.pas`
  当前预判：
  - 远端
    `WinSSL Runtime Gate`
    红灯
    不是 runtime 逻辑本身，
    而是这份 WinSSL certstore test
    编译期 API 漂移
  当前最终收口证据：
  - 失败日志明确显示：
    - `identifier idents no member "IsOpen"`
    - `Open`
    - `Close`
    - `GetAllCertificates`
    - `GetNativeHandle`
  - 但当前源码里，
    这些方法都在
    `TWinSSLCertificateStore`
    concrete class
    上，
    不在
    `ISSLCertificateStore`
    上
  - 旧测试同时还写反了
    constructor 语义：
    - `Create('MY')`
      真实会自动打开 store
    - 旧断言却说
      “新创建的存储未打开”
  当前实现策略：
  - WinSSL-specific runtime test
    改用 concrete type
  - 需要未打开初始态时
    使用 `Create('')`
  - 需要打开系统 store 时
    使用本地 helper
    `OpenConcreteSystemStore(...)`
  当前本地验证：
  - `git diff --check`
    - PASS
  当前远端验证：
  - `26138267777`
    workflow `CI`
    - `success`
  - `26138267809`
    workflow `WinSSL Runtime Gate`
    - `success`
  当前结论：
  - 这条红灯
    最终证明确实首先是 test drift，
    不是 backend 缺方法
  - 下一步
    可以从 WinSSL certstore
    这条 lane
    转去下一条 shared contract
  当前总路线图进度：
  - `接口设计`
    已经从 shared public contract
    下沉到
    WinSSL-specific test/runtime truth
  - `测试完整性`
    开始清理
    旧 concrete API 记忆
    对当前接口面的误绑
  当前下一条真实工作：
  - 等新 push
    的
    `WinSSL Runtime Gate`
    回来
  - 如果继续红，
    再看是不是
    compile 之后的
    真实 runtime 失败
- [completed] `optional backends buildcertificatechain issuer-link parity`
  当前 focused 目标：
  - 把
    `MbedTLS` /
    `WolfSSL`
    的
    `BuildCertificateChain`
    从只靠 store lookup
    的简化实现，
    收口到与
    `FreePascal`
    更一致的 public chain truth
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-optional-backends-build-certificate-chain-issuer-link-parity.md`
  - 修改实现：
    - `src/fafafa.ssl.mbedtls.certificate.pas`
    - `src/fafafa.ssl.wolfssl.certificate.pas`
  - 修改 focused tests：
    - `tests/test_freepascal_backend_basic.pas`
    - `tests/test_mbedtls_framework.pas`
    - `tests/test_wolfssl_framework.pas`
  当前预判：
  - `FreePascal`
    已经会先吃
    `GetIssuerCertificate()`
    再 fallback 到
    store lookup
  - `MbedTLS` /
    `WolfSSL`
    还停在
    `FindBySubject(GetIssuer)`
    级别
  当前最终收口证据：
  - `FreePascal`
    旧测试里那个
    “chain dedup”
    实际被 self-signed fixture
    短路，
    并没有真的锁住
    non-self-signed issuer-link path
  - 新测试改成：
    - leaf:
      `tests/certificate/test_certs/signer_cert.pem`
    - issuer:
      `tests/certificate/test_certs/ca_cert.pem`
    - store 中故意不放 issuer
  - 修复后：
    - `MbedTLS`
      `BuildCertificateChain`
      会先跟随显式 issuer-link
    - `WolfSSL`
      也同样先跟随显式 issuer-link
    - 两者都在追加下一跳前
      做 object / fingerprint 去重
  focused verification：
  - `tests/test_freepascal_backend_basic.pas`
    - PASS
  - `tests/test_mbedtls_framework.pas`
    - `171 passed / 0 failed`
  - `tests/test_wolfssl_framework.pas`
    - `185 passed / 0 failed`
  当前结论：
  - earlier
    `issuer-link truth`
    修复，
    现在终于贯通到
    certstore chain building
  - optional backends
    不再把
    “证书自己已经带着 issuer”
    这条公共真相
    直接丢掉
  当前总路线图进度：
  - `接口设计`
    已经从
    query / clone semantics
    推进到
    chain-building semantics
  - `后端实现`
    `MbedTLS` /
    `WolfSSL`
    又补齐一层 shared contract
  当前下一条真实工作：
  - 继续看
    `OpenSSL` /
    `WinSSL`
    的
    `BuildCertificateChain`
    / generic chain verifier
    是否还存在
    shared contract drift
- [completed] `mbedtls certstore clone fingerprint parity`
  当前 focused 目标：
  - 把
    `TMbedTLSCertificateStore`
    仍然只按对象身份判断的
    store semantics：
    - `Contains`
    - `RemoveCertificate`
    - duplicate `AddCertificate`
    收口到与
    `FreePascal` /
    `WolfSSL`
    更一致的 fingerprint truth
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-mbedtls-certstore-clone-fingerprint-parity.md`
  - 修改实现：
    - `src/fafafa.ssl.mbedtls.certificate.pas`
  - 修改 focused tests：
    - `tests/test_mbedtls_framework.pas`
  - 修改 WinSSL workflow fallout：
    - `tests/winssl/test_winssl_certstore.lpi`
  当前预判：
  - `MbedTLS`
    当前 certstore
    对 clone
    仍按对象身份判断
  - 这和
    `FreePascal` /
    `WolfSSL`
    已经存在的
    fingerprint semantics
    继续分叉
  当前最终收口证据：
  - `tests/test_mbedtls_framework.pas`
    新增 RED：
    - `Contains clone should be true by fingerprint`
    - `Add clone duplicate returns false`
    - `Remove clone should remove by fingerprint`
  - 修复后：
    - `TMbedTLSCertificateStore`
      现在对：
      - `Contains`
      - `RemoveCertificate`
      - `AddCertificate`
      都支持 fingerprint fallback
  - focused verification：
    - `tests/test_mbedtls_framework.pas`
      `166 passed / 0 failed`
  - 同步修掉上一批远端 WinSSL lane 暴露的真实配置错误：
    - `test_winssl_certstore.lpi`
      硬编码了
      `TargetOS=linux`
    - 这会让 Windows runner
      上的 `lazbuild`
      直接去编 Linux target
    - 现在已删除这段错误 target truth
  当前结论：
  - optional backends
    在 certstore query family
    收口之后，
    下一层真实 drift
    确实是 store ownership / duplicate semantics
  - `MbedTLS`
    不再是
    “同一张证书 clone 后仍被当成另一张”
  当前总路线图进度：
  - `接口设计`
    继续从 query semantics
    推进到 duplicate/clone semantics
  - `后端实现`
    `MbedTLS` certstore
    又补齐了一块 shared contract
  当前下一条真实工作：
  - push 这一批后
    先看
    `WinSSL Runtime Gate`
    是否因 `.lpi` 修正转绿
  - 再继续审查：
    - `BuildCertificateChain`
      dedup / loop semantics
    - 以及其他
      `ISSLCertificateStore`
      剩余 cross-backend drift
- [completed] `certificate store dn query canonical contract`
  当前 focused 目标：
  - 把
    `ISSLCertificateStore`
    的两条 DN 查询入口：
    - `FindBySubject`
    - `FindByIssuer`
    在当前仍有明显分叉的
    `FreePascal` /
    `OpenSSL` /
    `WinSSL`
    三个后端上
    收口到一条更一致的 shared contract
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-certificate-store-dn-query-canonical-contract.md`
  - 修改实现：
    - `src/fafafa.ssl.freepascal.lib.pas`
    - `src/fafafa.ssl.openssl.certstore.pas`
    - `src/fafafa.ssl.winssl.certstore.pas`
  - 修改 focused tests / runtime suite：
    - `tests/test_freepascal_backend_basic.pas`
    - `tests/openssl/test_openssl_certstore_dn_query_contract.pas`
    - `tests/winssl/test_winssl_certstore.pas`
    - `tests/run_winssl_tests.ps1`
  当前预判：
  - optional backends
    已经到达
    normalized substring query truth
  - 但主仓库另三组实现
    还在分叉：
    - `FreePascal`
      偏 exact
    - `OpenSSL`
      偏 raw uppercase substring
    - `WinSSL`
      偏 native store substring
  当前实现策略：
  - 对外 contract
    统一到：
    - normalized DN query
    - 支持 partial DN fragment lookup
    - empty query -> `nil`
  - 实现层
    继续保留：
    - exact-first
    - substring fallback
    这样 internal full-DN lookup
    不会被我们自己放宽得过粗
  当前最终收口证据：
  - `FreePascal`
    首轮 RED：
    - `Certificate store should find certificate by normalized subject fragment query`
  - `OpenSSL`
    首轮 RED：
    - `FindBySubject supports normalized partial DN fragment query`
    - `FindByIssuer supports normalized partial DN fragment query`
  - 修复后：
    - `FreePascal`
      subject / issuer
      都支持 normalized partial DN fragment query
    - `OpenSSL`
      subject / issuer cache
      现在缓存 normalized DN
    - `WinSSL`
      subject / issuer
      改为基于 `FCertificates`
      做 normalized lookup
    - `tests/run_winssl_tests.ps1`
      现在会编译并运行
      `test_winssl_certstore.lpi`
  focused verification 已通过：
  - `tests/test_freepascal_backend_basic.pas`
    - PASS
  - `tests/openssl/test_openssl_certstore_dn_query_contract.pas`
    - `12 passed / 0 failed`
  - `git diff --check`
    - PASS
  当前结论：
  - 这批收掉的是
    `ISSLCertificateStore`
    在 DN query family
    上最真实的一层全局设计/实现分叉
  - 不是文档漂移，
    也不是 optional backends
    的残余补洞
  当前总路线图进度：
  - `接口设计`
    已经从 optional backend 局部补洞
    上升到并收掉了
    certstore DN query 的 shared contract 分叉
  - `后端实现`
    现在：
    - optional backends
      query family
      已闭环
    - `FreePascal` / `OpenSSL` / `WinSSL`
      也在同一条 DN query truth
      上收口
  当前下一条真实工作：
  - 继续沿着
    `ISSLCertificateStore`
    / `ISSLCertificate`
    这条 shared public contract
    往下找剩余 cross-backend drift
  - 优先看：
    - 还有没有
      “public interface 已定义，
      但 backend 行为仍不一致”
      的高频 surface
- [completed] `optional backends certificate store issuer query parity`
  当前 focused 目标：
  - 把
    `MbedTLS` / `WolfSSL`
    证书存储对象的
    `FindByIssuer`
    从原始字符串比较
    收口到与当前 store query family
    一致的可用程度
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-optional-backends-certificate-store-issuer-query-parity.md`
  - 修改实现：
    - `src/fafafa.ssl.mbedtls.certificate.pas`
    - `src/fafafa.ssl.wolfssl.certificate.pas`
  - 修改 focused tests：
    - `tests/test_mbedtls_framework.pas`
    - `tests/test_wolfssl_framework.pas`
  当前预判：
  - `MbedTLS` / `WolfSSL`
    当前 `FindByIssuer`
    都还是原始：
    - `Pos(AIssuer, LCert.GetIssuer) > 0`
  - 但上一批已经把
    `FindBySubject` /
    `FindBySerialNumber`
    收口到 normalized query truth
  - 所以当前最自然的 bounded 方向
    不是立刻重定义
    全仓库 issuer-search 契约，
    而是先把 optional backends
    收口到与自己同一家族 query surface
    一致
  当前验证策略：
  - 用
    `tests/certificate/test_certs/signer_cert.pem`
    作为 distinct-issuer fixture，
    避免
    `subject = issuer`
    把字段拿错也掩盖掉
  - 继续只跑
    framework focused tests
  当前 done 条件：
  - `MbedTLS` / `WolfSSL`
    都支持 normalized issuer query
  - empty issuer query
    继续 fail-closed
  - focused tests
    通过
  - `git diff --check`
    通过
  当前最终收口证据：
  - `MbedTLS`
    新增 issuer-query contract
    首轮 RED
    打出 1 个失败：
    - `FindByIssuer supports normalized query variant`
  - `WolfSSL`
    同类 contract
    首轮 RED
    也打出 1 个失败：
    - `FindByIssuer supports normalized query variant`
  - GREEN 后证明：
    - `TMbedTLSCertificateStore`
      现在会对 issuer query
      做归一化匹配
    - `TWolfSSLCertificateStore`
      现在也会对 issuer query
      做归一化匹配
  focused verification 已通过：
  - `tests/test_mbedtls_framework.pas`: `161 passed / 0 failed`
  - `tests/test_wolfssl_framework.pas`: `180 passed / 0 failed`
  - `git diff --check`
  当前结论：
  - 这批继续收掉的是
    optional backends
    在 certificate store query family
    上的实现缺口
  - 但更上层的
    “全仓库 `FindByIssuer` 契约”
    仍然没有统一：
    - `FreePascal`
      仍偏 exact match
    - `OpenSSL` / `WinSSL`
      仍偏 substring match
  当前总路线图进度：
  - `发布/控制面`
    已闭环，
    当前不再是主阻塞
  - `接口设计`
    在 optional backend store query family
    上已继续补齐，
    但全局 issuer-search 语义
    仍是下一层设计债
  - `后端实现`
    optional backends
    的 certificate + certstore 族
    已连续收口：
    - `algorithm metadata`
    - `extension metadata`
    - `public surface`
    - `identity getters`
    - `store subject/serial parity`
    - `store issuer parity`
  - `测试与文档`
    bounded batch
    台账继续保持闭环
  当前下一条真实工作：
  - 先决定
    `FindByIssuer`
    的全仓库 canonical contract：
    - exact normalized match
    - 还是 substring match
  - 如果暂不进入这条设计线，
    再回到 certificate getter
    的剩余直接疑点：
    - `GetVersion`
    但这条线需要先补非 v3 fixture
- [completed] `optional backends certificate store query parity`
  当前 focused 目标：
  - 把
    `MbedTLS` / `WolfSSL`
    证书存储对象
    两条高频查询 surface
    收口到更稳定的一致语义：
    - `FindBySubject`
    - `FindBySerialNumber`
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-optional-backends-certificate-store-query-parity.md`
  - 修改实现：
    - `src/fafafa.ssl.mbedtls.certificate.pas`
    - `src/fafafa.ssl.wolfssl.certificate.pas`
  - 修改 focused tests：
    - `tests/test_mbedtls_framework.pas`
    - `tests/test_wolfssl_framework.pas`
  当前预判：
  - `FreePascal`
    已经有：
    - subject 归一化查询
    - serial 归一化查询
  - `WolfSSL`
    当前只补了
    `FindBySubject`
    的文本归一化，
    `FindBySerialNumber`
    仍是裸字符串比较
  - `MbedTLS`
    当前两条查询
    都还停留在原始比较：
    - `FindBySubject`
      原样 `Pos(...)`
    - `FindBySerialNumber`
      原样 `=`
  当前验证策略：
  - 继续复用
    framework tests，
    不新开重量级 cross-backend gate
  - 直接把
    `FreePascal`
    已经锁住的 normalized query contract
    向 optional backends 对齐
  当前 done 条件：
  - `MbedTLS` / `WolfSSL`
    都支持 normalized subject query
  - `MbedTLS` / `WolfSSL`
    都支持 normalized serial query
  - `MbedTLS`
    空 subject query
    不再错误命中第一张证书
  - focused tests
    通过
  - `git diff --check`
    通过
  当前最终收口证据：
  - `MbedTLS`
    新增 store-query contract
    首轮 RED
    打出 2 个失败：
    - `FindBySubject supports normalized query variant`
    - `FindBySerialNumber supports normalized query variant`
  - `WolfSSL`
    同类 contract
    首轮 RED
    打出 1 个失败：
    - `FindBySerialNumber supports normalized query variant`
  - GREEN 后证明：
    - `TMbedTLSCertificateStore`
      现在会对 subject / serial
      做归一化查询
    - `TWolfSSLCertificateStore`
      现在会对 serial
      做归一化查询
  focused verification 已通过：
  - `tests/test_mbedtls_framework.pas`: `155 passed / 0 failed`
  - `tests/test_wolfssl_framework.pas`: `174 passed / 0 failed`
  - `git diff --check`
  当前结论：
  - 这批收掉的是
    optional backends
    在 certificate store query
    上的直接实现缺口，
    不是文档或辅助脚本问题
  - `FreePascal`
    已经存在的 normalized query truth
    现在不再只停留在单一 backend
  当前总路线图进度：
  - `发布/控制面`
    已闭环，
    当前不再是主阻塞
  - `接口设计`
    在
    `ISSLCertificateStore`
    这一层，
    optional backends
    已继续向 shared query contract
    收口
  - `后端实现`
    optional backends
    的 certificate 族
    已连续收口：
    - `algorithm metadata`
    - `extension metadata`
    - `public surface`
    - `identity getters`
    - `store query parity`
  - `测试与文档`
    bounded batch
    台账继续保持闭环
  当前下一条真实工作：
  - 继续审
    certificate store
    剩余查询语义：
    - `FindByIssuer`
  - 或者切回
    certificate getter
    剩余直接疑点：
    - `GetVersion`
    但这条线需要先补非 v3 fixture
- [completed] `optional backends certificate identity getter completeness`
  当前 focused 目标：
  - 把
    `MbedTLS` / `WolfSSL`
    证书对象的
    identity getter
    收口到
    parser truth：
    - `GetSubject`
    - `GetIssuer`
    - `GetSerialNumber`
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-optional-backends-certificate-identity-getter-completeness.md`
  - 预期修改实现：
    - `src/fafafa.ssl.mbedtls.certificate.pas`
    - `src/fafafa.ssl.wolfssl.certificate.pas`
  - 预期修改 focused tests：
    - `tests/test_mbedtls_framework.pas`
    - `tests/test_wolfssl_framework.pas`
  当前预判：
  - 上一批把
    `GetPublicKey` /
    `GetExtension`
    收口后，
    optional backends
    在 certificate identity surface
    上仍残留三条高价值 truth gap：
    - `GetSubject`
    - `GetIssuer`
    - `GetSerialNumber`
  - 其中最强的真 bug
    不是文案漂移，
    而是
    `TWolfSSLCertificate.GetSerialNumber`
    把 serial 指针地址
    当作返回值
  - `GetVersion`
    仍值得继续审，
    但当前仓库夹具全是 v3；
    在没有新夹具前，
    很难形成有意义的 RED，
    所以这批先不把它和 identity getter batch 绑死
  当前验证策略：
  - 继续复用
    `TX509Certificate`
    作为 identity truth owner
  - 用
    `signer_ecdsa_cert.pem`
    同时覆盖：
    - `CN=Test Signer ECDSA`
    - serial 真值
  当前 done 条件：
  - `MbedTLS` / `WolfSSL`
    的 `GetSubject`
    含正确 CN
  - `MbedTLS` / `WolfSSL`
    的 `GetIssuer`
    含正确 CN
  - `MbedTLS` / `WolfSSL`
    的 `GetSubjectCN`
    为
    `Test Signer ECDSA`
  - `MbedTLS` / `WolfSSL`
    的 serial getter
    归一化后与 fixture 真值一致
  - focused tests
    通过
  - `git diff --check`
    通过
  当前最终收口证据：
  - `MbedTLS`
    新增 identity contract
    首轮即 GREEN，
    证明：
    - `signer_ecdsa_cert.pem`
      上的 subject / issuer / CN / serial truth
      已可稳定读取
    - 这批需要的是把该 truth
      显式冻结到 focused contract
  - `WolfSSL`
    同类 contract
    首轮暴露的不只是格式 drift，
    而是
    `GetSerialNumber`
    直接触发 `EAccessViolation`
  - 在把测试改成
    “serial getter 必须安全且值正确”
    的 fail-closed 断言后，
    稳定 RED
    收敛成 1 个 serial failure；
    修复后最终：
    - `tests/test_wolfssl_framework.pas`
      `172 passed / 0 failed`
  - 最终实现收口：
    - `TMbedTLSCertificate`
      / `TWolfSSLCertificate`
      都会优先通过
      `TX509Certificate`
      发布：
      - `Subject.ToString`
      - `Issuer.ToString`
      - `SerialNumberAsHex`
    - native text / oneline path
      只保留为 parser 不可用时的 fallback
  focused verification 已通过：
  - `tests/test_mbedtls_framework.pas`: `147 passed / 0 failed`
  - `tests/test_wolfssl_framework.pas`: `172 passed / 0 failed`
  - `git diff --check`
  当前结论：
  - 这批收掉的是
    optional backends
    在 certificate identity getter
    上最基础的一层实现缺口，
    不是单纯格式美化
  - `TX509Certificate`
    已经足够作为
    optional backends
    的 identity truth owner；
    后续若没有 parser 缺口，
    不应再优先走分散的 native text helper
  当前总路线图进度：
  - `发布/控制面`
    已闭环，
    当前不再是主阻塞
  - `接口设计`
    已有
    `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
    作为静态审查基线；
    当前正沿 optional backend certificate surface
    做实装收口
  - `后端实现`
    在 optional backends
    上已连续收口：
    - `algorithm metadata`
    - `extension metadata`
    - `public surface`
    - `identity getters`
  - `测试与文档`
    当前每个 bounded batch
    都有：
    - focused contract
    - `docs/plans/...`
    - `task_plan.md` / `findings.md` / `progress.md`
    形成可复用记录
  当前下一条真实工作：
  - 优先继续审
    certificate store query parity：
    - `FindBySerialNumber`
    - `FindBySubject`
  - 再决定是否为
    `GetVersion`
    专门补一张非 v3 fixture，
    让这条线先有意义的 RED
- [completed] `optional backends certificate public surface completeness`
  当前 focused 目标：
  - 把
    `MbedTLS` / `WolfSSL`
    证书对象
    剩余两个
    已发布但仍为空壳的
    certificate surface
    收口：
    - `GetPublicKey`
    - `GetExtension`
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-optional-backends-certificate-public-surface-completeness.md`
  - 修改实现：
    - `src/fafafa.ssl.mbedtls.certificate.pas`
    - `src/fafafa.ssl.wolfssl.certificate.pas`
  - 修改 focused tests：
    - `tests/test_mbedtls_framework.pas`
    - `tests/test_wolfssl_framework.pas`
  当前预判：
  - 上一批已经收掉
    算法元数据
    与扩展类 metadata
  - 但 optional backends
    仍在更基础的
    certificate public surface
    上留下统一空壳：
    - `GetPublicKey = ''`
    - `GetExtension = ''`
  当前验证策略：
  - 延续既有最小 contract：
    - `OpenSSL.GetPublicKey`
      当前返回算法名
    - `FreePascal.GetPublicKey`
      也已返回算法名
  - `GetExtension`
    继续复用
    `TX509Certificate.Extensions`
    的 parser truth
  - 夹具选用：
    - `signer_ecdsa_cert.pem`
      同时覆盖：
      - `GetPublicKey <> ''`
      - `GetPublicKey = GetPublicKeyAlgorithm`
      - `GetExtension('2.5.29.14') <> ''`
  当前 done 条件：
  - `MbedTLS` / `WolfSSL`
    的 `GetPublicKey`
    不再返回空串
  - `MbedTLS` / `WolfSSL`
    对已知存在的 `Subject Key Identifier`
    能返回非空 extension truth
  - focused tests
    通过
  - `git diff --check`
    通过
  当前最终收口证据：
  - `MbedTLS`
    首轮 RED
    精确打出 3 个失败：
    - `GetPublicKey <> ''`
    - `GetPublicKey = GetPublicKeyAlgorithm`
    - `GetExtension('2.5.29.14') <> ''`
    修复后最终：
    - `tests/test_mbedtls_framework.pas`
      `142 passed / 0 failed`
  - `WolfSSL`
    同类 contract
    首轮 RED
    同样是 3 个失败，
    修复后最终：
    - `tests/test_wolfssl_framework.pas`
      `167 passed / 0 failed`
  当前关键结论：
  - `GetPublicKey`
    在本仓库当前 contract
    下
    不是完整公钥导出 API，
    而是：
    - 与 `OpenSSL` / `FreePascal`
      保持一致的
      算法标识字符串
  - `GetExtension`
    对 optional backends
    也已经不再是空壳，
    而是复用
    `TX509Certificate.Extensions`
    发布 parser truth
  当前下一条真实工作：
  - 继续审 optional backends
    remaining certificate truth
    里更可疑的 placeholder/fallback：
    - `GetSubject`
    - `GetIssuer`
    - `GetSerialNumber`
    - `GetVersion`
- [completed] `optional backends certificate extension metadata completeness`
  当前 focused 目标：
  - 把
    `MbedTLS` / `WolfSSL`
    证书对象
    在扩展类元数据上的
    published surface
    从残缺快照
    收紧成
    与真实 X.509 扩展一致的
    truth
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-optional-backends-certificate-extension-metadata-completeness.md`
  - 修改实现：
    - `src/fafafa.ssl.mbedtls.certificate.pas`
    - `src/fafafa.ssl.wolfssl.certificate.pas`
  - 修改 focused tests：
    - `tests/test_mbedtls_framework.pas`
    - `tests/test_wolfssl_framework.pas`
  当前预判：
  - 上一批已经收掉
    算法元数据默认壳
  - 但当前 optional backends
    仍在下列字段上
    存在真实残缺：
    - `IsCA`
    - `SubjectAltNames`
    - `KeyUsage`
    - `ExtendedKeyUsage`
    - `GetInfo.PublicKeySize`
    - `GetInfo.IsCA`
    - `GetInfo.SubjectAltNames`
    - `GetInfo.KeyUsage`
  当前验证策略：
  - 继续复用
    `TX509Certificate`
    作为单一 truth source
  - 用 3 组现成夹具
    做 focused RED：
    - `signer_ecdsa_cert.pem`
      用于
      `PublicKeySize=256`
      和
      `IsCA=True`
    - `san-test.pem`
      用于
      `SAN`
      快照完整性
    - `keyusage_cert.pem`
      用于
      `KeyUsage` /
      `ExtendedKeyUsage`
      与
      `GetInfo.KeyUsage`
      bitfield
  当前 done 条件：
  - `MbedTLS` / `WolfSSL`
    对上面 3 组夹具
    都能发布真实扩展元数据
  - `GetInfo`
    不再遗漏
    `PublicKeySize` /
    `IsCA` /
    `SubjectAltNames` /
    `KeyUsage`
  - focused tests
    通过
  - `git diff --check`
    通过
  当前最终收口证据：
  - `WolfSSL`
    新增扩展元数据 contract
    首次运行即 GREEN，
    说明 parser-based 路径
    直接补齐了：
    - `IsCA`
    - `SAN`
    - `KeyUsage`
    - `ExtendedKeyUsage`
    - `GetInfo` 快照字段
  - `MbedTLS`
    新增 contract
    首次运行打出 14 个失败，
    暴露的不是 parser 能力缺失，
    而是：
    - 多次 `LoadFromFile(...)`
      后
      `FDERData` /
      `FPEMData`
      缓存没有清掉，
      导致后续 cert metadata
      仍读到前一张证书快照
  - GREEN 后证明：
    - `TMbedTLSCertificate`
      现在不再跨证书复用旧缓存
    - `TMbedTLSCertificate` /
      `TWolfSSLCertificate`
      都能对现有夹具
      发布真实扩展 metadata
    - `GetInfo`
      已补齐：
      - `PublicKeySize`
      - `IsCA`
      - `PathLength`
      - `PathLenConstraint`
      - `KeyUsage`
      - `SubjectAltNames`
  focused verification 已通过：
  - `tests/test_mbedtls_framework.pas`: `139 passed / 0 failed`
  - `tests/test_wolfssl_framework.pas`: `164 passed / 0 failed`
  - `tests/contract/test_backend_contract.pas`: `Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
  - `git diff --check`
  当前结论：
  - 这批收掉的是
    optional backends
    在
    扩展类 certificate metadata
    和
    `GetInfo` snapshot
    上的真实实现缺口，
    并顺手修掉了
    `MbedTLS`
    的 stale-cache 证书加载 bug
  当前下一条真实工作：
  - 继续审查
    optional backends
    剩余 certificate surface：
    - `GetPublicKey`
    - `GetExtension`
    - 可能仍缺的
      `issuer-link`
      / snapshot parity
      残余点
- [completed] `optional backends certificate algorithm metadata completeness`
  当前 focused 目标：
  - 把
    `MbedTLS` / `WolfSSL`
    证书对象
    对外发布的
    算法元数据
    从固定默认壳
    收紧成
    与真实证书内容一致的
    public surface
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-optional-backends-certificate-algorithm-metadata-completeness.md`
  - 修改实现：
    - `src/fafafa.ssl.mbedtls.certificate.pas`
    - `src/fafafa.ssl.wolfssl.certificate.pas`
  - 修改 focused tests：
    - `tests/test_mbedtls_framework.pas`
    - `tests/test_wolfssl_framework.pas`
  当前预判：
  - 最新 `CI` run `26131410258`
    已全部 `success`
  - 所以下一条真实高价值缺口
    不再是
    workflow / Windows runtime
    控制面
  - 而是：
    - `TMbedTLSCertificate.GetPublicKeyAlgorithm`
      仍固定返回
      `RSA`
    - `TMbedTLSCertificate.GetSignatureAlgorithm`
      仍固定返回
      `SHA256withRSA`
    - `TWolfSSLCertificate`
      也保留同样默认壳
  当前验证策略：
  - 先把两组 framework tests
    从
    “默认值就是当前真相”
    改成
    “加载真实 `ECDSA` 夹具后要暴露真实算法”
  - 再复用仓库已有
    `TX509Certificate`
    解析器
    补齐：
    - `GetPublicKeyAlgorithm`
    - `GetSignatureAlgorithm`
    - `GetInfo`
      中对应字段
  当前 done 条件：
  - `MbedTLS` / `WolfSSL`
    都能对
    `tests/certificate/test_certs/signer_ecdsa_cert.pem`
    发布：
    - `ecPublicKey`
    - `ecdsa-with-SHA256`
    这类真实算法元数据
  - focused framework tests
    通过
  - `git diff --check`
    通过
  当前最终收口证据：
  - 新增两组 framework RED
    首次运行直接各自打出 4 个失败：
    - `MbedTLS`:
      `GetPublicKeyAlgorithm` /
      `GetSignatureAlgorithm` /
      `GetInfo` 对应字段
    - `WolfSSL`:
      同样 4 个点
  - GREEN 后证明：
    - `TMbedTLSCertificate`
      会对 `ECDSA` 夹具发布：
      - `ecPublicKey`
      - `ecdsa-with-SHA256`
    - `TWolfSSLCertificate`
      也会发布同样 truth
    - `GetInfo`
      中两字段
      已与 getter 保持一致
  focused verification 已通过：
  - `tests/test_mbedtls_framework.pas`: `119 passed / 0 failed`
  - `tests/test_wolfssl_framework.pas`: `144 passed / 0 failed`
  - `git diff --check`
  当前结论：
  - 这批收掉的是
    optional backends
    在
    `ISSLCertificate`
    算法元数据 surface
    上的真实实现缺口，
    不是单纯文档措辞问题
  当前下一条真实工作：
  - 继续沿着
    `certificate metadata completeness`
    主线，
    审查是否还存在：
    - `PublicKeySize`
    - `GetPublicKey`
    - 更多 `GetInfo`
      字段
    在 optional backends
    上仍为壳值或残缺值
- [completed] `winssl auto runtime gate activation`
  当前 focused 目标：
  - 把
    WinSSL 的
    Windows runtime 证据链
    从
    `workflow_dispatch`
    为主
    提升成
    会在相关改动上自动触发的
    GitHub Actions lane
  当前 batch 范围：
  - 新增活跃 workflow：
    - `.github/workflows/winssl-tests.yml`
  - 同步 dormant template：
    - `.github/workflows/winssl-tests.yml.disabled`
  - 更新 focused contract：
    - `tests/scripts/test_workflow_winssl_tests_truth_contract.sh`
  - 更新说明：
    - `.github/README.md`
  - 新增计划：
    - `docs/plans/2026-05-20-winssl-auto-runtime-gate-activation.md`
  当前预判：
  - 当前缺口不是
    “没有 Windows runtime 证据链”
  - 而是：
    - 活跃 push / PR CI
      没有自动 Windows lane
    - 真正的 Windows runtime proof
      主要仍藏在
      `wave-b-b2-manual.yml`
      这条手动 lane 里
  - 所以高价值修复
    不该是继续静态审文档，
    而该是把
    已验证过的 Windows checklist
    提升成自动 gate
  当前验证策略：
  - 新自动 workflow
    直接复用：
    - quick smoke
    - Windows Wave B gate
    - broader WinSSL suite
  - 用 focused workflow contract
    冻结：
    - active + disabled
      `winssl-tests`
      都要保留真实 runtime chain
    - 不允许退回旧的“production ready”模板词
  当前最终收口证据：
  - 新 `winssl-tests` workflow truth contract
    GREEN
  - `wave-b-b2` Windows workflow contract
    继续 GREEN
  - Node24 checkout / upload-artifact
    全局 workflow contracts
    继续 GREEN
  - 这说明：
    - 新自动 Windows lane
      没有绕开现有 runtime checklist
    - 也没有破坏仓库当前 workflow hygiene
  focused verification 已通过：
  - `bash tests/scripts/test_workflow_winssl_tests_truth_contract.sh`
  - `bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`
  - `bash tests/scripts/test_workflow_checkout_node24_contract.sh`
  - `bash tests/scripts/test_workflow_upload_artifact_node24_contract.sh`
  - `git diff --check`
  当前结论：
  - 这批收掉的是
    “WinSSL 自动验证缺口”，
    不是 WinSSL 新实现问题
  当前下一条真实工作：
  - 观察新自动 Windows workflow
    的真实 GitHub run
  - 然后继续回到
    backend capability residual drift
    或
    WinSSL shared-path runtime truth
    的剩余缺口
- [completed] `winssl tls13 capability consistency alignment`
  当前 focused 目标：
  - 收掉
    `WinSSL`
    在同一能力主题下
    自己互相打架的 source truth：
    - `SupportsTLS13`
      走
      `18362`
    - `IsProtocolSupported(sslProtocolTLS13)`
      却走
      `20348`
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-winssl-tls13-capability-consistency-alignment.md`
  - 新增 focused contract：
    - `tests/scripts/test_winssl_tls13_capability_consistency_contract.sh`
  - 收口源码：
    - `src/fafafa.ssl.winssl.lib.pas`
  - 收口测试说明：
    - `tests/winssl/test_winssl_unit_comprehensive.pas`
  当前预判：
  - canonical matrix
    与
    dedicated WinSSL page
    都已经把
    `TLS 1.3`
    写成
    `Windows 10 1903+`
    这条条件 truth
  - 但 source 内部
    却分叉成：
    - capability record
      `18362`
    - runtime protocol probe
      `20348`
  - 这说明问题已经不是 docs drift，
    而是 backend 实现
    自己破坏了
    `GetCapabilities`
    和
    `IsProtocolSupported(...)`
    的一致性约束
  当前验证策略：
  - 用 focused shell contract
    同时冻结：
    - `SupportsTLS13`
      的
      `18362`
      truth
    - `sslProtocolTLS13`
      的门槛必须和前者一致
    - canonical / dedicated docs
      继续锚定
      `1903+`
    - WinSSL comprehensive unit test
      不能再保留
      `Windows 11-only`
      旧叙事
  当前最终收口证据：
  - 新 contract 第一次 RED
    就直接打在：
    - `IsProtocolSupported(sslProtocolTLS13)`
      旁边的旧注释
    说明当前 source
    仍沿用旧的更严格 TLS 1.3 门槛
  - GREEN 后证明：
    - `SupportsTLS13`
      与
      `IsProtocolSupported(sslProtocolTLS13)`
      已统一回到
      `Build >= 18362`
    - WinSSL unit test
      也不再继续暗示
      只有
      `Windows 11`
      才支持
      `TLS 1.3`
  focused verification 已通过：
  - `bash -n tests/scripts/test_winssl_tls13_capability_consistency_contract.sh`
  - `bash tests/scripts/test_winssl_tls13_capability_consistency_contract.sh`
  - `git diff --check`
  当前结论：
  - 这批收掉的是
    WinSSL backend
    的真实 capability/runtime inconsistency，
    不是单纯文档措辞问题
  当前下一条真实工作：
  - 继续审查剩余 candidate
    是否还有
    “docs 已经对了，
    但 source / test / capability probe
    彼此分叉”
    的问题
  - 下一优先队列：
    - `MbedTLS`:
      `Ed25519`
      `异步操作`
    - `WinSSL`:
      `Windows 7 SP1`
      平台支持表
- [completed] `mbedtls tls13 capability doc truth alignment`
  当前 focused 目标：
  - 收掉
    `MBEDTLS_BACKEND_CAPABILITY_MATRIX`
    把
    `TLS 1.3`
    写成无条件
    `✅ 支持`
    的专页漂移
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-mbedtls-tls13-capability-doc-truth-alignment.md`
  - 新增 focused contract：
    - `tests/scripts/test_mbedtls_tls13_capability_doc_truth_contract.sh`
  - 收口文档：
    - `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
  当前预判：
  - source 已经明确发布：
    - `MBEDTLS_MIN_VERSION = $03000000`
    - `HasTLS13 := VersionNumber >= MBEDTLS_MIN_VERSION`
    - `IsProtocolSupported(sslProtocolTLS13) := HasTLS13`
    - `SupportsTLS13 := HasTLS13`
  - top-level `docs/BACKEND_CAPABILITY_MATRIX.md`
    也已经把
    `MbedTLS TLS 1.3`
    记为
    `⚠️`
  - 但 dedicated MbedTLS page
    仍写：
    - `TLS 1.3 | ✅ 支持 | MbedTLS 3.x 支持`
  - 这说明：
    - source truth 是条件能力
    - canonical matrix 是条件能力
    - dedicated page 却把条件能力扁平化成了无条件支持
  当前验证策略：
  - 用 focused shell contract
    同时冻结：
    - `MBEDTLS_MIN_VERSION`
    - `HasTLS13` 的 runtime/version gating
    - `sslProtocolTLS13`
      与
      `SupportsTLS13`
      的条件 truth
    - canonical matrix
      里的
      `MbedTLS = ⚠️`
    - dedicated page
      新旧两行的 present / absent truth
  当前最终收口证据：
  - `git show HEAD:docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
    直接证明：
    编辑前 active dedicated page
    仍保留
    `| TLS 1.3 | ✅ 支持 | MbedTLS 3.x 支持 |`
  - GREEN 后证明：
    - dedicated MbedTLS page
      不再把
      `TLS 1.3`
      发布成无条件能力
    - 活跃专页已重新回到
      `SupportsTLS13` / `sslProtocolTLS13`
      取决于 runtime version detection
      的 source truth
  focused verification 已通过：
  - `bash -n tests/scripts/test_mbedtls_tls13_capability_doc_truth_contract.sh`
  - `bash tests/scripts/test_mbedtls_tls13_capability_doc_truth_contract.sh`
  - `git diff --check`
  当前结论：
  - 这批收掉的是
    MbedTLS dedicated backend page
    对 runtime-gated TLS 1.3 capability
    的扁平化误报，
    不是新的 TLS 1.3 实现缺口
  当前下一条真实工作：
  - 继续审查 active backend 专页
    里剩余的
    runtime-gated / platform-gated / application-layer possibility
    是否又被写成
    `✅` / `⚠️ 部分`
  - 优先再看：
    - `MbedTLS`:
      `Ed25519`
      `异步操作`
    - `WinSSL`:
      `ChaCha20-Poly1305`
      `x25519`
      `Context callbacks`
      `Password-protected private keys`
      `Windows 7 SP1`
- [completed] `mbedtls protocol capability doc truth alignment`
  当前 focused 目标：
  - 收掉
    `MBEDTLS_BACKEND_CAPABILITY_MATRIX`
    协议支持表里
    `TLS 1.0 / TLS 1.1 / DTLS 1.0 / DTLS 1.2`
    仍沿用旧 capability 表
    的漂移
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-mbedtls-protocol-capability-doc-truth-alignment.md`
  - 新增 focused contract：
    - `tests/scripts/test_mbedtls_protocol_capability_doc_truth_contract.sh`
  - 收口文档：
    - `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
  当前预判：
  - source 已经明确发布：
    - `sslProtocolTLS10=False`
    - `sslProtocolTLS11=False`
    - `sslProtocolDTLS10=False`
    - `sslProtocolDTLS12=False`
    - `MinTLSVersion=sslProtocolTLS12`
    - `SupportsDTLS=False`
  - 但 dedicated MbedTLS page
    仍把：
    - `TLS 1.0 / 1.1`
      写成
      `⚠️ 可选`
    - `DTLS 1.0`
      写成
      `⚠️ 可选`
    - `DTLS 1.2`
      写成
      `✅ 支持`
  - 这说明专页保留的是旧协议兼容表，
    不是当前 published capability truth
  当前验证策略：
  - 用 focused shell contract
    同时冻结：
    - MbedTLS source protocol truth
    - `test_mbedtls_framework`
      里的 DTLS unsupported truth
    - dedicated matrix 的四行新表述
    - 旧 optional / supported 行必须消失
  当前最终收口证据：
  - 新 contract 第一次 RED
    先暴露的是 contract 自己对反引号行的 quoting 问题，
    不是产品 drift
  - 修正 quoting 后，
    第一处真实 RED
    就是
    `TLS 1.0`
    仍未收回到
    `当前 capability 不发布`
  - GREEN 后证明：
    - MbedTLS dedicated protocol table
      不再发布
      `TLS 1.0 / 1.1`
      与
      `DTLS 1.0 / 1.2`
      的旧支持承诺
    - 活跃专页已重新回到
      `TLS 1.2+`
      与
      `SupportsDTLS=False`
      这组 source truth
  focused verification 已通过：
  - `bash -n tests/scripts/test_mbedtls_protocol_capability_doc_truth_contract.sh`
  - `bash tests/scripts/test_mbedtls_protocol_capability_doc_truth_contract.sh`
  - `git diff --check`
  当前结论：
  - 这批收掉的是
    MbedTLS dedicated backend page
    的协议 capability-table drift，
    不是 MbedTLS 新协议实现缺口
  当前下一条真实工作：
  - 继续按同一方法审查 active backend / guide / reference docs
  - 优先再找：
    - 旧平台表
    - `⚠️ 部分`
    - “上游/平台潜力”
    被单独保留在活跃入口里，
    但 source truth 已经收紧的地方
- [completed] `winssl dtls doc truth alignment`
  当前 focused 目标：
  - 收掉
    `WINSSL_BACKEND_CAPABILITY_MATRIX`
    里把
    `DTLS 1.0 / 1.2`
    写成当前已支持/部分支持
    的直接能力表漂移
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-winssl-dtls-doc-truth-alignment.md`
  - 新增 focused contract：
    - `tests/scripts/test_winssl_dtls_doc_truth_contract.sh`
  - 收口文档：
    - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
  当前预判：
  - `src/fafafa.ssl.winssl.lib.pas`
    已经明确发布：
    - `SupportsDTLS=False`
    - 注释直接写
      `Schannel 不支持 DTLS`
  - 但 dedicated WinSSL page
    仍保留：
    - `DTLS 1.0 | ✅ / ✅ / ⚠️`
    - `DTLS 1.2 | ✅ / ⚠️ / ❌`
    这种平台表
  - 这已经不是 wording 模糊，
    而是专页 capability 表
    和 source truth
    直接冲突
  当前验证策略：
  - 用 focused shell contract
    同时冻结：
    - WinSSL source `SupportsDTLS=False`
    - dedicated WinSSL matrix 新的 DTLS 两行
    - 旧的支持/部分支持行必须消失
  当前最终收口证据：
  - 新 contract 第一次 RED
    就直接证明：
    dedicated WinSSL matrix
    还没有把
    `DTLS 1.0`
    收回到
    `当前 capability 不发布`
  - 第二次 RED
    暴露的是 contract 对反引号行的 quoting 问题，
    不是产品 drift
  - GREEN 后证明：
    - WinSSL dedicated matrix
      不再发布任何
      `DTLS 1.0 / 1.2`
      支持承诺
    - 两行都已回到
      `SupportsDTLS=False`
      这条 source truth
  focused verification 已通过：
  - `bash -n tests/scripts/test_winssl_dtls_doc_truth_contract.sh`
  - `bash tests/scripts/test_winssl_dtls_doc_truth_contract.sh`
  - `git diff --check`
  当前结论：
  - 这批收掉的是
    WinSSL dedicated backend page
    的 DTLS capability-table drift，
    不是 WinSSL 新实现缺口
  当前下一条真实工作：
  - 继续审查活跃 backend / guide / reference docs
    是否还残留
    source truth 已明确，
    但 dedicated doc 单独保留旧平台表/旧支持行
    的入口
- [completed] `mbedtls ocsp capability doc truth alignment`
  当前 focused 目标：
  - 收掉
    `MBEDTLS_BACKEND_CAPABILITY_MATRIX`
    里把：
    - `OCSP`
    - `OCSP Stapling`
    写成
    “只差调用方手动实现”
    的模糊叙事
  - 明确区分：
    - 当前 backend 已发布 capability
    - 应用层在库外自行补的 revocation workflow
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-mbedtls-ocsp-capability-doc-truth-alignment.md`
  - 新增 focused contract：
    - `tests/scripts/test_mbedtls_ocsp_capability_doc_truth_contract.sh`
  - 收口文档：
    - `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
  当前预判：
  - source / tests 已经持续表达：
    - `OCSPStaplingSupport=sslSupportNone`
    - `sslFeatOCSPStapling=False`
    - `ISSLServerOCSPStaplingContext` 不暴露
    - unsupported OCSP backend 不应暴露 `ISSLOCSPStapling`
  - 但 dedicated MbedTLS page
    仍把：
    - `OCSP | ⚠️ 部分 | 需手动实现`
    - `OCSP Stapling | ❌ 不支持 | 需外部实现`
    混写在一起，
    容易让读者误读成
    “当前 backend 只差 integration glue”
  当前验证策略：
  - 用 focused shell contract
    同时冻结：
    - MbedTLS source capability truth
    - builder fail-fast truth
    - MbedTLS context / backend contract 里的 interface-absence truth
    - dedicated MbedTLS page 当前必须出现/必须消失的 wording
  当前最终收口证据：
  - 新 contract 第一次 RED
    就直接证明：
    dedicated MbedTLS page
    还没有把 generic `OCSP`
    收回到
    `当前 capability 不发布`
  - 第二次 RED
    暴露的是 contract 自己对反引号行的 quoting 问题，
    不是产品 drift
  - GREEN 后证明：
    - `OCSP`
      不再写成
      `⚠️ 部分`
    - `OCSP Stapling`
      不再只写成
      `需外部实现`
    - 限制说明
      也改成
      “当前 backend 不发布 capability；
      若需要相关 workflow，
      需在 fafafa.ssl 已发布 surface 之外自行实现”
  focused verification 已通过：
  - `bash -n tests/scripts/test_mbedtls_ocsp_capability_doc_truth_contract.sh`
  - `bash tests/scripts/test_mbedtls_ocsp_capability_doc_truth_contract.sh`
  - `git diff --check`
  当前结论：
  - 这批收掉的是
    MbedTLS dedicated backend page
    对
    `OCSP`
    /
    `OCSP Stapling`
    的 capability classification drift，
    不是 MbedTLS 新实现缺口
  当前下一条真实工作：
  - 继续 backend-specific / guide / design docs completeness 审计
  - 优先再找：
    - 其他 dedicated backend pages
    - performance / selection / security guides
    是否还有
    “平台/应用层潜力”
    被写成
    “当前 published capability”
    的入口
- [completed] `backend selector design doc truth alignment`
  当前 focused 目标：
  - 收掉
    `BACKEND_ABSTRACTION_LAYER_DESIGN`
    /
    `BACKEND_SELECTOR_DESIGN`
    里仍在传播的：
    - 旧 selector / builder 草案 API
    - `FreePascal (Future)` 叙事
    - 设计层重复维护旧 capability 表
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-backend-selector-design-doc-truth-alignment.md`
  - 新增 focused contract：
    - `tests/scripts/test_backend_selector_design_doc_truth_contract.sh`
  - 收口 design docs：
    - `docs/reference/BACKEND_ABSTRACTION_LAYER_DESIGN.md`
    - `docs/reference/BACKEND_SELECTOR_DESIGN.md`
  当前预判：
  - 这批的真实问题
    不是 backend source 失真，
    而是 design/reference 层
    继续把：
    - `WithRequirements([br...])`
    - `WithAutoBackend`
    - `TBackendSelector`
    - `WithPreferredBackend`
    - selector env vars
    这些草案 surface
    写成当前 public API
  - 同时还把
    `FreePascal`
    留在
    `Future`
    叙事里，
    会持续把后续路线判断带偏
  当前验证策略：
  - 用一条 source-backed focused shell contract
    同时冻结：
    - selector / builder 当前真实 public API
    - `FreePascal` 活跃 backend 真相
    - `WinSSL`
      `OCSPStaplingSupport=sslSupportNone`
      /
      `EarlyDataSupport=sslSupportNone`
      真相
    - design docs 当前必须出现/必须消失的表述
  当前最终收口证据：
  - 新 contract 第一次运行时，
    先暴露的是 contract 自己的反引号 quoting 问题，
    不是产品 drift
  - 修正 quoting 后，
    第一处真实 RED
    就是 abstraction doc
    仍未声明
    `FreePascal`
    已是活跃 backend
  - GREEN 后证明：
    - abstraction doc
      不再把
      `FreePascal`
      写成 future
    - selector doc
      不再把旧 draft API
      写成当前 public surface
    - design docs
      改成引用 canonical capability truth，
      而不是再维护一套易漂移大表
  focused verification 已通过：
  - `bash -n tests/scripts/test_backend_selector_design_doc_truth_contract.sh`
  - `bash tests/scripts/test_backend_selector_design_doc_truth_contract.sh`
  - `git diff --check`
  当前结论：
  - 这批收掉的是
    “设计文档继续定义错误接口地图”
    这类路线级 drift，
    不是 selector 实现缺口
  当前下一条真实工作：
  - 继续审查
    backend-specific / guide / design docs
    是否还残留
    “应用层工作流”
    和
    “backend 已发布 capability”
    混写的入口
  - `MbedTLS`
    `OCSP Stapling`
    相关表述
    仍是高价值下一候选
- [completed] `server-side optional surface active-doc truth contract`
  当前 focused 目标：
  - 给
    `ISSLServerOCSPStaplingContext` /
    `ISSLEarlyDataContext` /
    `ISSLEarlyDataConnection`
    这条 cross-backend active-doc truth
    补一条 focused shell contract
  - 避免以后 source / runtime capability
    已经一致，
    但：
    - `API_REFERENCE`
    - `BACKEND_CAPABILITY_MATRIX`
    - dedicated backend pages
    - active guides
    之间重新漂开
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-server-side-optional-surface-active-doc-truth-contract.md`
  - 新增 focused contract：
    - `tests/scripts/test_server_side_optional_surface_active_docs_truth_contract.sh`
  当前预判：
  - 刚刚两批 closeout
    已经说明：
    - WinSSL dedicated page
      可能独自漂离 source / top-level matrix
    - FreePascal durable-default replay truth
      可能在 active docs / old contract
      中残留旧表述
  - 当前缺的不是再补实现，
    而是一条专门冻结
    server-side optional surface active-doc truth
    的 focused contract
  当前验证策略：
  - 新增 shell contract，
    同时覆盖：
    - `API_REFERENCE`
    - top-level `BACKEND_CAPABILITY_MATRIX`
    - `WINSSL_BACKEND_CAPABILITY_MATRIX`
    - `MBEDTLS_BACKEND_CAPABILITY_MATRIX`
    - `EARLY_DATA_GUIDE`
    - `OCSP_USAGE_GUIDE`
  - 跑：
    - `bash -n ...`
    - `bash ...`
    - `git diff --check`
  当前最终收口证据：
  - 首次运行时暴露的不是产品 drift，
    而是新 contract 自己的反引号语法错误
  - 修正 shell quoting 后，
    contract 全绿
  focused verification 已通过：
  - `bash -n tests/scripts/test_server_side_optional_surface_active_docs_truth_contract.sh`
  - `bash tests/scripts/test_server_side_optional_surface_active_docs_truth_contract.sh`
  - `git diff --check`
  当前结论：
  - 这批没有发现新的 live active-doc drift
  - 但把原来无人冻结的一组
    cross-backend server-side optional surface truth
    真正变成了自动校验
  当前下一条真实工作：
  - 继续围绕 backend capability truth
    做 completeness 审计
  - 优先再找：
    - dedicated backend docs
    - active guides
    - builder/factory/public-surface 语义
    之间是否还有“当前 capability truth 已变，
    但 active docs 没被 focused contract 守住”的残留
- [completed] `winssl none-capability surface doc alignment`
  当前 focused 目标：
  - 把 WinSSL backend 专页中
    `OCSP Stapling`
    与
    `0-RTT`
    这两行，
    从“Schannel 可能有平台潜力”的叙事，
    收回到当前 fafafa.ssl
    真正发布的 capability / public surface truth
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-winssl-none-capability-surface-doc-alignment.md`
  - 新增 focused contract：
    - `tests/scripts/test_winssl_none_capability_surface_doc_truth_contract.sh`
  - 收口文档：
    - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
  当前预判：
  - WinSSL source 已明确发布：
    - `OCSPStaplingSupport=sslSupportNone`
    - `EarlyDataSupport=sslSupportNone`
  - top-level `BACKEND_CAPABILITY_MATRIX`
    也已经把 WinSSL 的
    `Early Data` / `OCSP Stapling`
    汇总成 `❌`
  - 但 dedicated `WINSSL_BACKEND_CAPABILITY_MATRIX`
    仍写成：
    - `OCSP Stapling | ⚠️ 部分`
    - `0-RTT | ⚠️ 部分`
  当前验证策略：
  - 先加 focused shell contract 做 RED
  - 然后只改 dedicated WinSSL matrix 两行 wording
  - 再跑 focused contract + `git diff --check`
  当前最终收口证据：
  - focused shell contract 先红后绿：
    - 初始 RED：
      `WinSSL dedicated matrix must describe OCSP stapling as none-published capability`
      直接证明专页和 source / top-level matrix 已经漂移
    - GREEN 后：
      - WinSSL 专页不再把 none-published capability
        写成
        `⚠️ 部分`
      - 当前叙事已经明确区分：
        - Schannel 的平台潜力
        - fafafa.ssl 当前 shipped public capability
  focused verification 已通过：
  - `bash -n tests/scripts/test_winssl_none_capability_surface_doc_truth_contract.sh`
  - `bash tests/scripts/test_winssl_none_capability_surface_doc_truth_contract.sh`
  - `git diff --check`
  当前结论：
  - 这批收掉的是 dedicated backend page 的 capability classification drift，
    不是 WinSSL 实现缺口
  - WinSSL 的 `0-RTT` / `Server OCSP Stapling`
    现在在：
    - source
    - top-level matrix
    - dedicated backend matrix
    三层重新一致
  当前下一条真实工作：
  - 继续 server-side optional surface cross-backend audit
  - 优先再看 dedicated backend pages
    是否还有“平台潜力 / library public surface”
    混写造成的 capability 漂移
- [completed] `freepascal default durable replay doc truth alignment`
  当前 focused 目标：
  - 把 FreePascal server-side early-data 默认 durable replay-store
    这条 live truth，
    在 active docs 与 focused contract 中重新对齐
  - 收掉
    “源码 / runtime capability 已经是 durable-by-default，
    但活跃文档和旧 contract 仍把 default path
    写成 in-memory single-process”
    这条 drift
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-freepascal-default-durable-replay-doc-truth-alignment.md`
  - 收口文档 / contract：
    - `docs/reference/API_REFERENCE.md`
    - `docs/INTEGRATION_GUIDE.md`
    - `docs/guides/security-best-practices.md`
    - `tests/scripts/test_freepascal_early_data_public_optin_docs_contract.sh`
  当前预判：
  - source truth 已经明确：
    - `TFreePascalContext`
      server path 默认创建
      `TFreePascalDefaultPersistentEarlyDataReplayLedger`
    - `TFreePascalSSLLibrary.GetCapabilities.KnownIssues`
      已改成
      `local persistent anti-replay replay-store path ... fail-closed`
  - 但 active docs / focused contract
    仍残留旧真相：
    - `docs/INTEGRATION_GUIDE.md`
      还在说 default path 是
      `in-memory single-process anti-replay ledger`
    - `docs/guides/security-best-practices.md`
      还在引用旧的
      `KnownIssues`
      句子
    - `docs/reference/API_REFERENCE.md`
      前后自相矛盾：
      前面说默认 shipped path 已改为持久化，
      后面又说
      “不代表默认路径已经改成持久化”
    - 旧 focused contract
      还要求 README 保留 retired wording
  当前验证策略：
  - 先用现有 docs contract 做 RED
  - 然后只改：
    - active docs wording
    - focused contract truth
    - planning files
  - 再补一条现有 capability runtime test 作为 source/runtime truth 证据
  当前最终收口证据：
  - focused shell contract 先红后绿：
    - 初始 RED：
      `README.md must keep the default in-memory single-process anti-replay wording`
      直接证明旧 contract
      还在反向冻结 retired truth
    - GREEN 后：
      - docs contract
        现在冻结的是 durable-default truth，
        不是旧的 in-memory wording
      - `API_REFERENCE`
        不再自相矛盾
      - `INTEGRATION_GUIDE`
        不再把 default path
        写成单进程内存 anti-replay
      - `security-best-practices`
        已改用当前
        `KnownIssues`
        真值
  focused verification 已通过：
  - `bash -n tests/scripts/test_freepascal_early_data_public_optin_docs_contract.sh`
  - `bash tests/scripts/test_freepascal_early_data_public_optin_docs_contract.sh`
  - `fpc ... tests/test_capability_cache.pas && ./tmp/capability_cache_bin/test_capability_cache`
  - `git diff --check`
  当前结论：
  - 这批收掉的不是 replay-store 实现缺口，
    而是 durable-default 落地后的
    active-doc / focused-contract 残余 drift
  - 当前 FreePascal server-side early-data
    default shipped path 的 live truth
    已经重新统一到：
    - source constructor
    - runtime capability `KnownIssues`
    - active docs
    - focused docs contract
  当前下一条真实工作：
  - 回到 server-side optional surface cross-backend truth audit
  - 继续核对：
    - `ISSLServerOCSPStaplingContext`
    - `ISSLEarlyDataContext`
    - `ISSLEarlyDataConnection`
    - builder / factory / matrix / guide
    是否还有其它 active truth 漂移
- [completed] `facade optional owner surface export alignment`
  当前 focused 目标：
  - 让 `uses fafafa.ssl;` 这个主门面入口，
    真正显式重导出当前活跃文档已教学的
    connection-side optional owner surfaces
  - 同时补齐这些 surface 依赖的 supporting types，
    避免 facade 入口继续停在“文档说能用，
    实际还得回退到 `fafafa.ssl.base`”的半完成状态
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-facade-optional-owner-surface-export-alignment.md`
  - 新增 focused contract / compile proof：
    - `tests/scripts/test_facade_optional_owner_surface_export_contract.sh`
    - `tests/contract/test_facade_optional_owner_surface_entry.pas`
  - 收口源码：
    - `src/fafafa.ssl.pas`
  当前预判：
  - `docs/README.md` / `guides/QUICKSTART.md`
    以及多条活跃 guide
    已经把 `uses fafafa.ssl;`
    当成主入口
  - 但主门面仍缺：
    - `ISSLConnectionInfo`
    - `ISSLDiagnostics`
    - `ISSLSessionResumption`
    - `ISSLCertificateVerification`
    - `ISSLOCSPStapling`
    - `ISSLCertificateTransparency`
    - `ISSLCertificateTransparencyValidation`
    以及 supporting types：
    - `TSSLHealthStatus`
    - `TSSLPerformanceMetrics`
    - `TSSLDiagnosticInfo`
    - `TSSLCertificateArray`
  当前验证策略：
  - 先新增 focused shell contract + facade-only compile proof 做 RED
  - 然后只补：
    - 主门面 alias
    - focused plan / findings / progress
    - `git diff --check`
  - 不拉大门禁
  当前最终收口证据：
  - focused shell contract 先红后绿：
    - 初始 RED：
      `src/fafafa.ssl.pas`
      中没有
      `TSSLHealthStatus = fafafa.ssl.base.TSSLHealthStatus;`
      这条 alias
    - GREEN 后：
      `fafafa.ssl`
      已显式重导出：
      - `ISSLConnectionInfo`
      - `ISSLDiagnostics`
      - `ISSLSessionResumption`
      - `ISSLCertificateVerification`
      - `ISSLOCSPStapling`
      - `ISSLCertificateTransparency`
      - `ISSLCertificateTransparencyValidation`
      - `TSSLHealthStatus`
      - `TSSLPerformanceMetrics`
      - `TSSLDiagnosticInfo`
      - `TSSLCertificateArray`
  focused verification 已通过：
  - `bash -n tests/scripts/test_facade_optional_owner_surface_export_contract.sh`
  - `bash tests/scripts/test_facade_optional_owner_surface_export_contract.sh`
  - `git diff --check`
  当前结论：
  - 这不是“文档措辞有点散”的问题，
    而是主门面的真实 public completeness gap
  - 这批之后，
    活跃文档把 `fafafa.ssl`
    作为主入口的说法，
    才重新和源码 façade truth 对齐
  当前下一条真实工作：
  - 回到 server-side optional surface cross-backend truth audit
  - 优先继续核对：
    - 总 capability matrix
    - direct-library / builder replay-store 语义
    - backend contract
    之间是否还有 symmetry / completeness 漂移
- [completed] `API reference optional public interface coverage`
  当前 focused 目标：
  - 补齐 `docs/reference/API_REFERENCE.md`
    对当前 shipped optional public interfaces 的 canonical 覆盖，
    尤其是：
    - `ISSLHttpHooksAccess`
    - `ISSLServerOCSPStaplingContext`
    - `ISSLEarlyDataContext`
    - `ISSLEarlyDataConnection`
    - `ISSLConnectionInfo`
    - `ISSLDiagnostics`
    - `ISSLSessionResumption`
    - `ISSLCertificateVerification`
    - `ISSLOCSPStapling`
  - 同时把
    “当前 public Pascal source 尚未声明 `ISSLServerConnection`，
    server-side 特有能力主要通过 context optional surfaces 暴露”
    提升进 canonical API reference
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-api-reference-optional-interface-coverage.md`
  - 新增 focused contract：
    - `tests/scripts/test_api_reference_optional_interface_coverage_contract.sh`
  - 收口文档：
    - `docs/reference/API_REFERENCE.md`
  当前预判：
  - 当前活跃指南 / `API_DOCUMENTATION.md`
    已经在使用这些 optional public interfaces，
    但 canonical `API_REFERENCE.md`
    仍主要只列了 `ISSLNativeHandleAccess`
  - 这会让：
    - source / facade 已导出的 public surface
    - secondary docs 已教学的 owner surface
    - canonical reference 的“完整 API 面”
    三者出现文档层 completeness gap
  当前验证策略：
  - 先补一条 focused shell contract，
    钉住 canonical API reference 必须覆盖的 optional public interfaces
  - 然后只做：
    - 新 contract
    - `git diff --check`
  - 不拉起大编译
  当前最终收口证据：
  - focused shell contract 先红后绿：
    - 初始 RED：
      `API_REFERENCE.md`
      还没有记录
      `ISSLServerConnection`
      当前缺位与 server-side context surface 真相
    - GREEN 后：
      - canonical API reference
        已补齐：
        `ISSLHttpHooksAccess`
        `ISSLServerOCSPStaplingContext`
        `ISSLEarlyDataContext`
        `ISSLEarlyDataConnection`
        `ISSLConnectionInfo`
        `ISSLDiagnostics`
        `ISSLSessionResumption`
        `ISSLCertificateVerification`
        `ISSLOCSPStapling`
      - 主参考已明确：
        当前 public Pascal source 尚未声明
        `ISSLServerConnection`
      - server-side 特有能力当前主要通过
        context optional surfaces
        暴露
  focused verification 已通过：
  - `bash tests/scripts/test_api_reference_optional_interface_coverage_contract.sh`
  - `git diff --check`
  当前结论：
  - 这批收掉的是 canonical docs completeness gap，
    不是新的 runtime / backend bug
  - 之前这些 interface
    虽然在源码、二级文档和一部分指南里都已经是 live surface，
    但主参考没有把它们拼成完整地图
  - 现在查“当前 shipped API 全貌”，
    不再需要在
    `API_REFERENCE`
    和二级文档之间来回跳
  当前下一条真实工作：
  - 继续 server-side symmetry 主线，
    但下一刀更值钱的已经不是“主参考是否记得这些接口”，
    而是：
    - server-side optional surface
      在各 backend 上的 capability / exposure / docs 是否完全一致
    - 以及是否存在值得单独抽象成
      `ISSLServerConnection`
      的稳定最小公共面
- [completed] `GetPeerCertificateChain compiler deprecation alignment`
  当前 focused 目标：
  - 把 `ISSLConnection.GetPeerCertificateChain`
    从“owner path 已存在但 core 仍像普通 surface”
    收成和当前
    `ISSLCertificateVerification`
    真相一致的 compiler-deprecated compatibility mirror
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-getpeercertificatechain-compiler-deprecation.md`
  - 新增 focused contract：
    - `tests/scripts/test_getpeercertificatechain_compiler_deprecated_contract.sh`
  - 收口源码 / docs / ordinary guidance / residual proofs：
    - `src/fafafa.ssl.base.pas`
    - `docs/reference/API_REFERENCE.md`
    - `docs/reference/INTERFACE_DESIGN_V2.md`
    - `docs/guides/TROUBLESHOOTING.md`
    - `tests/examples/test_certchain.pas`
    - `tests/contract/test_backend_contract.pas`
    - `tests/test_openssl_connection_peer_certificate_surface.pas`
    - `tests/test_mbedtls_connection_peer_certificate_contract.pas`
    - `tests/connection/test_wolfssl_client_peer_certificate_surface.pas`
    - `tests/test_openssl_connection_peer_certificate_chain_contract.pas`
    - `tests/test_freepascal_client_peer_certificate_surface.pas`
    - `tests/winssl/test_winssl_connection_info.pas`
    - `tests/winssl/test_winssl_peer_certificate_surface.pas`
  当前预判：
  - `GetVerifyResult*` 已经完成 compiler-deprecated 收口，
    所以下一刀最值钱的不是再做 verify-result archaeology，
    而是把仍停在半收口状态的
    `GetPeerCertificateChain`
    也推进到 source/doc/compiler 同步状态
  当前验证策略：
  - 先跑 focused shell contract 做 RED
  - 然后只做
    - 新 contract
    - `git diff --check`
    - 1-3 个代表性 Pascal 编译
  - 不重新拉大门禁
  当前最终收口证据：
  - focused shell contract 先红后绿：
    - 初始 RED：
      `src/fafafa.ssl.base.pas`
      中
      `GetPeerCertificateChain`
      的 compiler-deprecated 声明匹配数为 `0`
    - GREEN 后：
      - core declaration 已进入
        `deprecated 'Use ISSLCertificateVerification.GetPeerCertificateChain'`
      - `API_REFERENCE` / `INTERFACE_DESIGN_V2`
        已同步记录为 compiler-deprecated compatibility mirror
      - `TROUBLESHOOTING` / `tests/examples/test_certchain.pas`
        已切到
        `ISSLCertificateVerification.GetPeerCertificateChain`
      - residual direct-core file set
        已显式 warning quarantine
  focused verification 已通过：
  - `bash tests/scripts/test_getpeercertificatechain_compiler_deprecated_contract.sh`
  - `fpc ... tests/contract/test_backend_contract.pas`
  - `fpc ... tests/test_openssl_connection_peer_certificate_surface.pas`
  - `fpc ... tests/test_mbedtls_connection_peer_certificate_contract.pas`
  - `git diff --check`
  当前结论：
  - `ISSLCertificateVerification`
    现在不再只是“文档上的 owner”
  - `GetPeerCertificateChain`
    已经和相邻的
    `GetVerifyResult*`
    一样进入 source/doc/compiler 三层对齐
  - 这批真正收掉的是
    “普通教学入口仍把 direct-core getter 当默认用法”
    这条 drift，
    同时保留了必要的 backend/runtime mirror proofs
  当前下一条真实工作：
  - 继续沿
    `ISSLConnection` slimming / client-server symmetry
    主线前进
  - 更值钱的下一刀优先再看：
    - `ISSLServerConnection`
      的建模不对称
    - 或 `ISSLConnection` 上还未明确 owner / compatibility 分层的剩余 surface
- [completed] `isslocspstapling compiler deprecation alignment`
  当前 focused 目标：
  - 把 `ISSLConnection` 上的 4 个 OCSP compatibility-core mirrors
    - `GetOCSPStaplingEnabled`
    - `GetOCSPResponse`
    - `IsOCSPResponseVerified`
    - `GetOCSPResponseStatus`
    收成和当前 owner-path truth 一致的 compiler-deprecated public surface
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-isslocspstapling-compiler-deprecation.md`
  - 新增 focused contract：
    - `tests/scripts/test_isslocspstapling_compiler_deprecated_contract.sh`
  - 收口源码 / docs / residual tests：
    - `src/fafafa.ssl.base.pas`
    - `docs/reference/API_REFERENCE.md`
    - `docs/reference/INTERFACE_DESIGN_V2.md`
    - `tests/mbedtls/test_mbedtls_ocsp_capability.pas`
    - `tests/openssl/test_ocsp_connection_verification_regression.pas`
    - `tests/test_openssl_connection_ocsp_storectx_issuer_contract.pas`
    - `tests/test_wolfssl_ocsp_stapling_contract.pas`
    - `tests/scripts/test_isslocspstapling_residual_classification_contract.sh`
  当前最终收口证据：
  - focused compiler-deprecated contract 先红后绿：
    - 初始 RED：
      `GetOCSPStaplingEnabled`
      还没有任何 compiler-deprecated 声明
    - GREEN 后：
      四个 core `GetOCSP*` 声明
      都已经进入
      `deprecated 'Use ISSLOCSPStapling....'`
  - 相关 OCSP truth contracts 继续通过：
    - `tests/scripts/test_isslocspstapling_compiler_deprecated_contract.sh`
    - `tests/scripts/test_isslocspstapling_residual_classification_contract.sh`
    - `tests/scripts/test_isslocspstapling_active_guidance_contract.sh`
  - intentional residual tests 已重新编译通过：
    - `tests/mbedtls/test_mbedtls_ocsp_capability.pas`
    - `tests/openssl/test_ocsp_connection_verification_regression.pas`
    - `tests/test_openssl_connection_ocsp_storectx_issuer_contract.pas`
    - `tests/test_wolfssl_ocsp_stapling_contract.pas`
  当前结论：
  - OCSP 这组 surface 现在不再只是
    “注释和文档说它是 compatibility mirror”
  - source declaration 自己也已经进入 compiler-deprecated，
    与 `ISSLOCSPStapling` owner path 对齐
  - 这批收掉的是 `ISSLConnection` core fatness 的一条真实源码切片，
    不是单纯 docs 治理
  当前下一条真实工作：
  - 继续沿 `ISSLConnection` slimming 主线，
    优先考虑还没进入 compiler-deprecated / owner-primacy 的 core residual
  - 与此同时继续盯
    client / server public surface
    是否还存在未明确建模的不对称残口
- [completed] `capability support-level source normalization`
  当前 focused 目标：
  - 把 backend `GetCapabilities` 的 paired capability producer
    收成 support-level 单真相，
    不再让各 backend 同时手工写
    `SupportsSNI` / `SupportsALPN` /
    `SupportsOCSPStapling` /
    `SupportsCertificateTransparency` /
    `SupportsSessionTickets`
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-capability-support-level-source-normalization.md`
  - 新增 focused contract：
    - `tests/scripts/test_capability_support_level_source_normalization_contract.sh`
  - 收口源码：
    - `src/fafafa.ssl.openssl.backed.pas`
    - `src/fafafa.ssl.freepascal.lib.pas`
    - `src/fafafa.ssl.winssl.lib.pas`
    - `src/fafafa.ssl.mbedtls.lib.pas`
    - `src/fafafa.ssl.wolfssl.lib.pas`
  当前最终收口证据：
  - 先红后绿的 focused shell contract 已证明：
    - 初始 RED：
      `src/fafafa.ssl.openssl.backed.pas`
      仍直接赋值 `Result.SupportsSNI := LSNIReady;`
    - GREEN 后：
      五个 live backend
      都只保留 support-level producer，
      paired legacy bool 统一走
      `NormalizeLegacyCapabilityBooleans(Result);`
  - cross-backend runtime contract 已通过：
    - `tests/contract/test_capabilities_contract.pas`
    - 结果：
      `63 passed, 0 failed, 1 skipped`
    - Linux 可用 backend：
      - `OpenSSL`
      - `WolfSSL`
      - `MbedTLS`
      - `FreePascal Native`
      仍全部满足：
      - support-level truth 存在
      - legacy bool 与 support-level 投影一致
    - `Windows Schannel`
      在当前 Linux host 上按预期 `SKIP`
  当前结论：
  - capability dual truth 的 producer 入口现在进一步收紧：
    - backend source 不再暗示 legacy bool 也是主发布口
    - shared normalization helper
      现在成为 paired capability bool 的唯一 live projection 点
  - 这批收掉的是 source-shape / producer residual，
    不是新的 runtime capability regression
  当前下一条真实工作：
  - 继续接口设计 / backend completeness 主线，
    但不要再回头做：
    - `ISSLServerConnection` 文档修正
    - `TSSLConfig` 的重复 docs 治理
  - 下一条更值钱的审查方向：
    - `ISSLConnection` 是否仍承担过宽职责
    - client / server public surface 是否仍存在实现不对称残口
- [completed] `auto-backend os-native preference truth`
  当前 focused 目标：
  - 给 `PreferOSNative` / auto-backend selection
    补一条 runtime-aware focused contract，
    证明 `BackendImplType = sslImplOSNative`
    会真实进入 selector 的 score / 排序，
    并且 builder 下游沿用同一个 selection truth
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-auto-backend-os-native-preference-truth-contract.md`
  - 新增 focused contract：
    - `tests/test_auto_backend_os_native_preference_truth_contract.pas`
  当前预判：
  - 当前 Linux runtime 没有真实可用的 OS-native backend，
    所以这批最值钱的不是做半截 negative-only proof，
    而是用 controlled mock runtime
    把 selector / builder 对 `sslImplOSNative`
    的消费链完整钉住
  当前最终收口证据：
  - focused contract 使用两组 requirements 对照：
    - baseline：
      `CreateDefaultRequirements(optBalanced)` + 三项最低分数门槛清零
    - preferred：
      baseline +
      `PlatformPreferences.PreferOSNative := True`
  - 合同通过 mock `sslOpenSSL` / `sslWinSSL`
    构造 controlled runtime，
    证明：
    - baseline 时 `sslImplCLibrary` backend 领先
    - 开启 `PreferOSNative` 后
      `sslImplOSNative` backend
      按当前公式获得固定加分并反超
    - `SelectBestBackend(...)`
      返回 `SelectBestBackends(...)`
      preferred 排序后的第一名
    - `TSSLContextBuilder.Create.WithAutoBackendSelection(...).TryBuildClient(...)`
      成功，并沿用 selector 选中的 OS-native backend
  focused verification 已通过：
  - `mkdir -p tmp/test_auto_backend_os_native_truth_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_auto_backend_os_native_truth_units -FEtmp/test_auto_backend_os_native_truth_units -otmp/test_auto_backend_os_native_truth_units/test_auto_backend_os_native_preference_truth_contract tests/test_auto_backend_os_native_preference_truth_contract.pas && ./tmp/test_auto_backend_os_native_truth_units/test_auto_backend_os_native_preference_truth_contract`
  当前结论：
  - 当前 selector / builder
    已经真实消费
    `BackendImplType = sslImplOSNative`
    这条 published truth
  - 这批收掉的是 preference downstream proof gap，
    不是新的 backend implementation bug
  当前总路线图进度：
  - selector / builder focused downstream proof
    已完成：
    - `RequirePKCS11Support`
    - `RequireTPM`
    - `RequireSystemCertStore`
    - `PreferHardwareAccel`
    - `PreferOSNative`
  - 这一组“platform preference / requirement 的 downstream proof”
    现在已经基本闭环
  当前下一条真实工作：
  - 切回更大的接口设计 / backend completeness 主线，
    继续沿
    `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
    处理更高价值的 public-surface 结构债
  - 优先再看：
    - `ISSLServerConnection` 文档/源码不一致
    - `ISSLConnection` 核心接口过宽
    - `TSSLConfig` 跨层职责混杂
- [completed] `auto-backend hardware-accel preference truth`
  当前 focused 目标：
  - 给 `PreferHardwareAccel` / auto-backend selection
    补一条 runtime-aware focused contract，
    证明 `HasHardwareAcceleration`
    会真实进入 selector 的 score / 排序，
    并且 builder 下游沿用同一个 selection truth
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-auto-backend-hardware-accel-preference-truth-contract.md`
  - 新增 focused contract：
    - `tests/test_auto_backend_hardware_accel_preference_truth_contract.pas`
  当前预判：
  - `HasHardwareAcceleration` 的 source truth
    本身没有先暴露出新的 backend drift，
    真正缺的是 selector / builder 是否真实消费了这条 preference truth
  当前最终收口证据：
  - focused contract 用两组 requirements 对照：
    - baseline：
      `CreateDefaultRequirements(optBalanced)` + 三项最低分数门槛清零
    - preferred：
      baseline +
      `PlatformPreferences.PreferHardwareAccel := True`
  - 当前合同已证明：
    - qualifying backend 集合保持一致
    - `HasHardwareAcceleration=True` 的 backend
      在 preferred requirements 下按当前公式获得固定加分
    - `HasHardwareAcceleration=False` 的 backend
      分数保持不变
    - `SelectBestBackend(...)`
      返回 `SelectBestBackends(...)` preferred 排序后的第一名
    - `TSSLContextBuilder.Create.WithAutoBackendSelection(...).TryBuildClient(...)`
      成功，并沿用 selector 选中的 backend
  focused verification 已通过：
  - `mkdir -p tmp/test_auto_backend_hardware_accel_truth_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_auto_backend_hardware_accel_truth_units -FEtmp/test_auto_backend_hardware_accel_truth_units -otmp/test_auto_backend_hardware_accel_truth_units/test_auto_backend_hardware_accel_preference_truth_contract tests/test_auto_backend_hardware_accel_preference_truth_contract.pas && ./tmp/test_auto_backend_hardware_accel_truth_units/test_auto_backend_hardware_accel_preference_truth_contract`
  当前结论：
  - 当前 selector / builder
    已经真实消费 `HasHardwareAcceleration` published truth
  - 这批收掉的是 preference downstream proof gap，
    不是新的 backend implementation bug
  当前总路线图进度：
  - selector / builder focused downstream proof
    已完成：
    - `RequirePKCS11Support`
    - `RequireTPM`
    - `RequireSystemCertStore`
    - `PreferHardwareAccel`
  - 当前最直接未收口的同类残口：
    - `PreferOSNative`
  当前下一条真实工作：
  - 继续沿 selector / builder 主线，
    补 `PreferOSNative` 的 runtime-aware preference proof
  - 然后再回到更大的接口设计与 backend completeness 主线
- [completed] `auto-backend system-cert-store capability truth`
  当前 focused 目标：
  - 给 `RequireSystemCertStore` / auto-backend selection
    补一条 runtime-aware focused contract，
    证明 selector / builder 的下游结果
    确实跟随当前已发布的 `SupportsSystemCertStore` capability truth
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-auto-backend-system-cert-store-capability-truth-contract.md`
  - 新增 focused contract：
    - `tests/test_auto_backend_system_cert_store_capability_truth_contract.pas`
  当前预判：
  - `SupportsSystemCertStore` 的 source truth
    与 selector/builder 消费路径本身并没有先验证出新的实现漂移，
    真正缺的是一条 runtime-aware downstream proof
  当前最终收口证据：
  - focused contract 会先遍历当前已注册且可用 backend，
    推导是否存在任一 backend 发布
    `SupportsSystemCertStore=True`
  - 若存在：
    - `SelectBestBackend(...)` 必须成功
    - 选中的 backend 也必须发布
      `SupportsSystemCertStore=True`
    - `TSSLContextBuilder.Create.WithAutoBackendSelection(...).TryBuildClient(...)`
      必须成功
  - 若不存在：
    - selector 必须失败
    - builder 必须失败，并返回
      `No suitable SSL backend found for requirements`
  - focused contract 已在本机编译并运行通过
  当前关键排障结论：
  - 第一版 RED 不是生产 bug，
    而是 focused proof 自己把
    `CreateDefaultRequirements(optBalanced)` 的默认评分阈值
    混进了 `RequireSystemCertStore` requirement truth
  - 把：
    - `MinSecurityScore := 0`
    - `MinPerformanceScore := 0`
    - `MinCompatibilityLevel := 0`
    显式清零后，
    这条合同就只验证 `RequireSystemCertStore`，
    不再被 balanced 默认阈值噪音误伤
  focused verification 已通过：
  - `mkdir -p tmp/test_auto_backend_system_cert_store_truth_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_auto_backend_system_cert_store_truth_units -FEtmp/test_auto_backend_system_cert_store_truth_units -otmp/test_auto_backend_system_cert_store_truth_units/test_auto_backend_system_cert_store_capability_truth_contract tests/test_auto_backend_system_cert_store_capability_truth_contract.pas && ./tmp/test_auto_backend_system_cert_store_truth_units/test_auto_backend_system_cert_store_capability_truth_contract`
  当前结论：
  - 当前 selector / builder 与
    `SupportsSystemCertStore` published capability truth
    已经对齐
  - 这批收掉的是 downstream proof gap，
    不是新的 backend implementation bug
  当前下一条真实工作：
  - 继续沿 selector / builder 主线，
    找其它 runtime-aware requirement / preference
    还缺 focused downstream proof 的残口
  - 优先再看：
    - `PreferOSNative`
    - `PreferHardwareAccel`
    - 或其它 capability-aware requirement / preference
- [completed] `backend feature capability parity runtime proof`
  当前 focused 目标：
  - 给 `ISSLLibrary.IsFeatureSupported(...)` 与
    `ISSLLibrary.GetCapabilities` 之间补一条 runtime consumer parity proof，
    锁住当前 `TSSLFeature` 枚举 7 条 feature 的发布口径一致性
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-backend-feature-capability-parity.md`
  - 新增 focused contract：
    - `tests/test_backend_feature_capability_parity_contract.pas`
  当前预判：
  - capability dual-truth 的 producer / serializer / selector 路线已经收紧，
    更值钱的 residual 是补齐 runtime consumer proof，
    防止 `IsFeatureSupported(...)` 和 capability record 再次分叉
  当前最终收口证据：
  - focused contract 在本机编译并运行通过：
    - `OpenSSL`
    - `WolfSSL`
    - `MbedTLS`
    - `FreePascal Native`
  - `Windows Schannel` 在非 Windows 环境被正确标记为
    `[SKIP] not available`
  - 当前 7 条 feature：
    - `sslFeatSNI`
    - `sslFeatALPN`
    - `sslFeatSessionCache`
    - `sslFeatSessionTickets`
    - `sslFeatRenegotiation`
    - `sslFeatOCSPStapling`
    - `sslFeatCertificateTransparency`
    都满足：
    - `LLib.IsFeatureSupported(AFeature) =
       (对应 *Support <> sslSupportNone)`
  focused verification 已通过：
  - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_backend_feature_capability_parity_contract -FEtmp/test_backend_feature_capability_parity_contract -otmp/test_backend_feature_capability_parity_contract/test_backend_feature_capability_parity_contract tests/test_backend_feature_capability_parity_contract.pas`
  - `./tmp/test_backend_feature_capability_parity_contract/test_backend_feature_capability_parity_contract`
  当前结论：
  - 这轮没有再暴露新的 backend source drift；
    真正缺的是 proof，而不是实现修复
  - 现在 capability dual-truth 路线已经补上了
    runtime consumer parity 这层 durable 基线
  当前下一条真实工作：
  - 继续沿 `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
    回到更大的接口/实现结构债
  - 优先再看：
    - 还有没有其他 runtime consumer / facade surface
      在 capability published truth 之外保留第二套语义
    - 或审计报告里更高价值的接口设计问题
- [completed] `troubleshooting winssl session truth`
  当前 focused 目标：
  - 把 `TROUBLESHOOTING.md` 里 WinSSL session 排障段收回当前 truth，
    避免高入口故障页继续把 `SetSession(...)` + `Connect`
    误教成默认已命中的 resumed-handshake
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-troubleshooting-winssl-session-truth.md`
  - 新增 focused contract：
    - `tests/scripts/test_troubleshooting_winssl_session_truth_contract.sh`
  - 同步更新：
    - `docs/guides/TROUBLESHOOTING.md`
    - `docs/guides/MIGRATION_GUIDE.md`
  当前预判：
  - 这页当前 owner path / SNI 示例本身不一定错，
    真正的缺口是排障页还把实验性 WinSSL session owner surface
    写成了默认成功路径
  当前最终收口证据：
  - `TROUBLESHOOTING.md` 明确：
    - direct `CreateConnection(...)` + `ISSLSessionResumption`
      是排障时为了观察 session owner surface
    - 普通跨后端 HTTPS 客户端仍优先
      `TSSLContextBuilder` + `TSSLConnector` + `TSSLStream`
    - 当前 dedicated Windows runtime truth 仍按
      `observed_reuse=false` / `session_configured=true`
      理解
    - 不再保留 `启用 Session 复用` / `快速复用` / `快速握手`
      这类把示例误读成稳定复用命中的 wording
  - `MIGRATION_GUIDE.md` 低层 `ISSLConnection` 迁移示例再次显式展示
    连接级 `ISSLClientConnection.SetServerName(...)`
  focused verification 已通过：
  - `bash -n tests/scripts/test_troubleshooting_winssl_session_truth_contract.sh`
  - `bash tests/scripts/test_troubleshooting_winssl_session_truth_contract.sh`
  - `bash tests/scripts/test_session_resumption_guide_old_name_truth_contract.sh`
  - `bash tests/scripts/test_migration_troubleshooting_connection_level_sni_omissions_contract.sh`
  - `bash tests/scripts/test_diagnostics_connection_override_classification_contract.sh`
  - `bash tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh`
  - `git diff --check`
  当前结论：
  - `TROUBLESHOOTING` 当前并不是
    `ISSLSessionResumption` 接口名或 session owner-path API 本身错了，
    而是排障页还把实验性 WinSSL session surface 写成了默认已命中的复用收益。
  - 这轮回归还顺手暴露并收掉了 `MIGRATION_GUIDE`
    的连接级 SNI 文案漂移，避免旧合同以后反复误报。
  当前下一条真实工作：
  - 继续扫 remaining high-entry / reference pages：
    - 看还有没有 fixed snapshot / blanket recommendation /
      unexplained direct path residual
  - 当高入口文档残口进一步缩小后，切回
    `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
    所指向的接口设计与 backend 实现一致性主线

- [completed] `readme performance + session truth`
  当前 focused 目标：
  - 把根 `README.md` 里的高入口性能/会话口径收回当前 truth，
    避免仓库首页继续把固定性能数字和 session public surface 写成长期结论
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-readme-performance-session-truth.md`
  - 新增 focused contract：
    - `tests/scripts/test_readme_performance_session_truth_contract.sh`
  - 同步更新：
    - `README.md`
  当前预判：
  - 根问题不是 README 接口名错了，
    而是首页还在用固定性能快照和固定 session 收益改写当前第一印象
  当前最终收口证据：
  - `README.md` 明确：
    - 性能相关结论回到 benchmark/baseline 入口
    - session public surface 是 backend-specific truth
    - 不再保留固定 `10,000x+` / `70-90%` current-truth 口径
  focused verification 已通过：
  - `bash -n tests/scripts/test_readme_performance_session_truth_contract.sh`
  - `bash tests/scripts/test_readme_performance_session_truth_contract.sh`
  - `bash tests/scripts/test_landing_quickstarts_direct_path_classification_contract.sh`
  - `bash tests/scripts/test_performance_guides_benchmark_truth_contract.sh`
  - `bash tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh`
  - `git diff --check`
  当前结论：
  - 根 README 当前并不是 landing direct-path 分层出错，
    而是首页还在用固定性能收益和固定 session 收益改写当前项目第一印象。
  - 现在 README / landing quickstarts / WinSSL guides / profiling guides
    这几层高入口文档已经开始统一回到同一套 benchmark/session truth。
  当前下一条真实工作：
  - 继续扫 remaining high-entry / reference pages：
    - 看还有没有固定 benchmark snapshot / blanket recommendation
      被写成 current truth
    - 同时继续找 direct `CreateConnection(...)` 已是 intentional path、
      但原因还没写透的 residual

- [completed] `performance profiling guide truth`
  当前 focused 目标：
  - 把 `PERFORMANCE_PROFILING_GUIDE` 里的过强 session/performance truth 收回当前口径，
    并补 profiling 场景下 direct-path 的使用原因说明，
    避免高可见性能页继续把固定数字和实验性 session surface 误教成 current truth
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-performance-profiling-guide-truth.md`
  - 新增 focused contract：
    - `tests/scripts/test_performance_profiling_guide_truth_contract.sh`
  - 同步更新：
    - `docs/guides/PERFORMANCE_PROFILING_GUIDE.md`
  当前预判：
  - 这页当前 owner path / SNI 用法本身不一定错，
    真正的缺口是把固定性能目标和 WinSSL session public surface 讲成了 current truth
  当前最终收口证据：
  - `PERFORMANCE_PROFILING_GUIDE.md` 明确：
    - profiling direct path 是 intentional path
    - session public surface 当前仍是实验性 public surface
    - 固定性能目标不再被写成 current truth
  focused verification 已通过：
  - `bash -n tests/scripts/test_performance_profiling_guide_truth_contract.sh`
  - `bash tests/scripts/test_performance_profiling_guide_truth_contract.sh`
  - `bash tests/scripts/test_active_owner_path_docs_alignment_contract.sh`
  - `bash tests/scripts/test_secondary_guides_connection_level_sni_api_drift_contract.sh`
  - `bash tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh`
  - `git diff --check`
  当前结论：
  - `PERFORMANCE_PROFILING_GUIDE` 当前并不是 profiling helper 名或 owner-path 用法本身出错，
    而是高可见性能页还把固定量级和 WinSSL session public surface 写成了 current truth。
  - 现在 WinSSL quickstart / user guide / best-practices / performance profiling
    这几层高可见文档已经回到了同一套 conservative session/runtime truth。
  当前下一条真实工作：
  - 继续扫 remaining active performance / specialized pages：
    - 看还有没有固定 benchmark snapshot / blanket recommendation
      被写成 current truth
    - 同时继续找 direct `CreateConnection(...)` 已是 intentional path、
      但原因还没写透的 residual

- [completed] `winssl best-practices session truth`
  当前 focused 目标：
  - 把 `WINSSL_BEST_PRACTICES` 里的 WinSSL session public surface
    真相写清楚，并补 page-level direct-path 分类，
    避免高入口最佳实践页继续把实验性 session surface 误教成默认优化路径
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-winssl-best-practices-session-truth.md`
  - 新增 focused contract：
    - `tests/scripts/test_winssl_best_practices_session_truth_contract.sh`
  - 同步更新：
    - `docs/guides/WINSSL_BEST_PRACTICES.md`
  当前预判：
  - 这页当前 owner path / capability 行本身不一定错，
    真正的缺口是还把 WinSSL session public surface 讲成默认最佳实践
  当前最终收口证据：
  - `WINSSL_BEST_PRACTICES.md` 明确：
    - direct connection/session path 属于 WinSSL-specific / backend-facing path
    - session public surface 当前仍是实验性 public surface
    - checklist 不再把 Session public surface 当默认最佳实践
  focused verification 已通过：
  - `bash -n tests/scripts/test_winssl_best_practices_session_truth_contract.sh`
  - `bash tests/scripts/test_winssl_best_practices_session_truth_contract.sh`
  - `bash tests/scripts/test_active_owner_path_docs_alignment_contract.sh`
  - `bash tests/scripts/test_secondary_guides_connection_level_sni_api_drift_contract.sh`
  - `bash tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh`
  - `git diff --check`
  当前结论：
  - `WINSSL_BEST_PRACTICES` 当前并不是 owner-path 接口名或 WinSSL capability 行写错，
    而是高入口最佳实践页还把实验性 session public surface 讲成了默认优化路径。
  - 现在 WinSSL quickstart / user guide / best-practices 这三层已经回到了同一套
    WinSSL-specific path 与 conservative session truth。
  当前下一条真实工作：
  - 继续扫 remaining active performance / best-practice pages：
    - 例如 `PERFORMANCE_PROFILING_GUIDE`
      是否也还把 session / performance 叙事写成过强 current truth
    - 同时继续找 direct `CreateConnection(...)` 已是 intentional path、
      但原因还没写透的 residual

- [completed] `winssl user guide direct-path classification`
  当前 focused 目标：
  - 把 `WINSSL_USER_GUIDE` 中 direct `ISSLConnection` /
    `CreateConnection(...)` 的使用原因写清楚，
    避免读者把 WinSSL 入口页里的 backend-facing 示例误解成 generic facade 主路径
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-winssl-user-guide-direct-path-classification.md`
  - 新增 focused contract：
    - `tests/scripts/test_winssl_user_guide_direct_path_classification_contract.sh`
  - 同步更新：
    - `docs/guides/WINSSL_USER_GUIDE.md`
  当前预判：
  - 这页当前 capability / runtime truth 没问题，
    缺口更像“为什么入口页会直接展示 WinSSL-specific / connection-level path”的解释层
  当前最终收口证据：
  - `WINSSL_USER_GUIDE.md` 明确：
    - direct path 属于 WinSSL-specific / backend-facing path
    - generic facade 主入口仍是
      `TSSLContextBuilder` + `TSSLConnector` + `TSSLStream`
    - SNI 连接级 published surface 的原因被写清楚
  focused verification 已通过：
  - `bash -n tests/scripts/test_winssl_user_guide_direct_path_classification_contract.sh`
  - `bash tests/scripts/test_winssl_user_guide_direct_path_classification_contract.sh`
  - `bash tests/scripts/test_winssl_quickstart_runtime_truth_contract.sh`
  - `bash tests/scripts/test_active_direct_context_servername_surface_classification_contract.sh`
  - `bash tests/scripts/test_winssl_user_guide_performance_truth_contract.sh`
  - `git diff --check`
  当前结论：
  - `WINSSL_USER_GUIDE` 当前并不是 WinSSL capability / runtime truth 出错，
    而是入口页还需要把 direct `ISSLConnection` 标回 WinSSL-specific / connection-owned path。
  - 现在 WinSSL 用户入口与 WinSSL quickstart 也回到了同一套主路径/专项路径分层体系。
  当前下一条真实工作：
  - 继续扫 remaining active guides / WinSSL 专题页：
    - 例如 `WINSSL_BEST_PRACTICES` / `PERFORMANCE_PROFILING_GUIDE`
      这类仍展示 direct `CreateConnection(...)` 的页面，
      是否也还缺“为什么这里必须走 backend-facing / low-level path”的说明

- [completed] `early-data owner-surface reasoning`
  当前 focused 目标：
  - 把 `EARLY_DATA_GUIDE` 中 direct context/connection owner path 的使用原因写清楚，
    避免读者把 early-data 示例误解成 generic facade 主路径
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-early-data-owner-surface-reasoning.md`
  - 新增 focused contract：
    - `tests/scripts/test_early_data_owner_surface_reasoning_contract.sh`
  - 同步更新：
    - `docs/guides/EARLY_DATA_GUIDE.md`
  当前预判：
  - 这页当前使用 `ISSLEarlyDataContext` / `ISSLEarlyDataConnection`
    本身是对的，缺口更像“为什么这里必须回到 owner surface”的解释层
  当前最终收口证据：
  - `EARLY_DATA_GUIDE.md` 明确：
    - 这页 direct path 是为了读取/配置 early-data owner surface
    - 普通握手入口仍是 `TSSLConnector` / `TSSLStream`
  focused verification 已通过：
  - `bash -n tests/scripts/test_early_data_owner_surface_reasoning_contract.sh`
  - `bash tests/scripts/test_early_data_owner_surface_reasoning_contract.sh`
  - `bash tests/scripts/test_early_data_docs_truth_contract.sh`
  - `git diff --check`
  当前结论：
  - `EARLY_DATA_GUIDE` 当前并不是 early-data optional interface 或 capability truth 出错，
    而是还需要把“为什么这里必须下到 context/connection owner surface”讲透。
  - 现在 early-data 这页也被拉回到了同一套 direct-path 分层体系。
  当前下一条真实工作：
  - 继续扫 remaining active guides / specialized pages：
    - 还有没有 direct `CreateConnection(...)` 已经是 intentional path，
      但仍缺“为什么这里要走 owner surface / low-level path”的 residual

- [completed] `specialized owner-surface reasoning`
  当前 focused 目标：
  - 把 specialized optional-interface guides 中 direct connection owner path 的
    使用原因写清楚，避免读者把 owner-surface 示例误解成 generic facade 主路径
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-specialized-owner-surface-reasoning.md`
  - 新增 focused contract：
    - `tests/scripts/test_specialized_owner_surface_reasoning_contract.sh`
  - 同步更新：
    - `docs/guides/OCSP_USAGE_GUIDE.md`
    - `docs/guides/CT_IMPLEMENTATION_GUIDE.md`
  当前最终收口证据：
  - `OCSP_USAGE_GUIDE.md` 现在明确：
    - direct `CreateConnection(...)` 是因为 stapled OCSP runtime state 通过
      `ISSLOCSPStapling` 挂在连接对象上
    - 握手失败时的 verify 结果也通过
      `ISSLCertificateVerification` 从连接侧读取
    - 不需要这层 owner surface 时，普通客户端仍可把握手入口保持在
      `TSSLConnector` / `TSSLStream`
  - `CT_IMPLEMENTATION_GUIDE.md` 现在明确：
    - direct `CreateConnection(...)` 是因为
      `ISSLCertificateTransparency` /
      `ISSLCertificateTransparencyValidation`
      挂在连接对象上
    - 不需要读取 CT owner surface 时，
      普通客户端仍可把握手入口保持在 `TSSLConnector` / `TSSLStream`
  focused verification 已通过：
  - `bash -n tests/scripts/test_specialized_owner_surface_reasoning_contract.sh`
  - `bash tests/scripts/test_specialized_owner_surface_reasoning_contract.sh`
  - `bash tests/scripts/test_isslcertificateverification_active_guidance_contract.sh`
  - `bash tests/scripts/test_isslocspstapling_active_guidance_contract.sh`
  - `git diff --check`
  当前结论：
  - specialized owner-surface guides 当前并不是接口名或 runtime truth 出错，
    而是还需要把“为什么这里必须走 connection owner path”讲透。
  - 现在 OCSP / CT 这两页也被拉回到了同一套 direct-path 分层体系。
  当前下一条真实工作：
  - 继续从 remaining specialized guides / owner-surface docs 往下扫：
    - 例如 session / diagnostics / certificate-verification 之外
      还有没有类似“示例是 intentional owner path，但原因没写透”的 residual

- [completed] `high-frequency guides direct-path reasoning`
  当前 focused 目标：
  - 把几份高频 active 页面里 direct `CreateConnection(...)` 的使用原因讲清楚，
    避免读者把场景化示例误解成 generic facade 主路径
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-high-frequency-guides-direct-path-reasoning.md`
  - 新增 focused contract：
    - `tests/scripts/test_high_frequency_guides_direct_path_reasoning_contract.sh`
  - 同步更新：
    - `docs/guides/COMMON_PITFALLS.md`
    - `docs/guides/security-best-practices.md`
    - `docs/guides/ERROR_HANDLING_BEST_PRACTICES.md`
  当前最终收口证据：
  - `COMMON_PITFALLS.md` 现在明确：
    - direct `CreateConnection(...)` 对比是为了把
      “没设 SNI vs 正确设 SNI”写成最短 pitfall 对照
    - 普通客户端仍可优先 `TSSLConnector.ConnectSocket(..., host)`
  - `security-best-practices.md` 现在明确：
    - direct `ISSLConnection` 示例是为了把 hostname/SNI 的连接级责任显式展开
    - 不需要这层低层控制时，继续使用 connector 也同样正确
  - `ERROR_HANDLING_BEST_PRACTICES.md` 现在明确：
    - direct `CreateConnection(...)` 是因为示例正在讨论
      URL 解析后的 socket ownership、连接异常、以及 Result/exception 边界
    - 不需要这层低层控制时可把握手入口收回 `TSSLConnector`
  focused verification 已通过：
  - `bash -n tests/scripts/test_high_frequency_guides_direct_path_reasoning_contract.sh`
  - `bash tests/scripts/test_high_frequency_guides_direct_path_reasoning_contract.sh`
  - `bash tests/scripts/test_active_tls_guidance_contract.sh`
  - `bash tests/scripts/test_secondary_guides_connection_level_sni_api_drift_contract.sh`
  - `bash tests/scripts/test_error_handling_best_practices_url_driven_sni_guidance_contract.sh`
  - `bash tests/scripts/test_security_best_practices_pinning_helper_truth_contract.sh`
  - `git diff --check`
  当前结论：
  - 这批暴露的不是 direct `CreateConnection(...)` 不该存在，
    而是高频页面也要明确说明“为什么这里要下到 low-level path”。
  - 现在 generic guides、landing、backend quickstarts、diagnostics、
    以及这组三个高频专题页的 direct-path 语义都开始统一起来了。
  当前下一条真实工作：
  - 继续扫尚未纳入 focused contract 的 specialized owner-surface guides：
    - `OCSP_USAGE_GUIDE`
    - `CT_IMPLEMENTATION_GUIDE`
    - 以及其它通过连接对象暴露 optional interface 的页面
    - 优先判断是否还缺“为什么这里要走 connection owner path”的说明

- [completed] `diagnostics connection override classification`
  当前 focused 目标：
  - 把 active diagnostics / backend guide 里的 `SetTimeout(...)` / `SetBlocking(...)`
    重新标回当前主路径 truth：
    - 它们仍然存在
    - 但在这些页面里主要是 direct-connection diagnostic override
    - 普通新代码仍优先 builder/connector/acceptor 与外围 timer/event-loop
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-diagnostics-connection-override-classification.md`
  - 新增 focused contract：
    - `tests/scripts/test_diagnostics_connection_override_classification_contract.sh`
  - 同步更新：
    - `docs/guides/TROUBLESHOOTING.md`
    - `docs/guides/MBEDTLS_USER_GUIDE.md`
  当前最终收口证据：
  - `TROUBLESHOOTING.md` 现在明确：
    - `LConn.SetTimeout(...)` 是 direct-connection 诊断 override
    - `LConn.SetBlocking(False)` 是 direct-connection 调试入口
    - 如果已经走 builder/connector/acceptor 或自有 event-loop，
      仍应优先让构建阶段与外围 timer/poller 管理真实超时和非阻塞状态
  - `MBEDTLS_USER_GUIDE.md` 现在明确：
    - timeout 故障小节里的 `Connection.SetTimeout(...)`
      只是 connection-level override
    - 普通跨后端客户端仍优先统一的 builder/connector/transport timer 路线
  focused verification 已通过：
  - `bash -n tests/scripts/test_diagnostics_connection_override_classification_contract.sh`
  - `bash tests/scripts/test_diagnostics_connection_override_classification_contract.sh`
  - `bash tests/scripts/test_mbedtls_active_docs_capability_truth_contract.sh`
  - `bash tests/scripts/test_active_guide_convenience_surface_classification_contract.sh`
  - `git diff --check`
  当前结论：
  - 这批暴露的不是 `SetTimeout` / `SetBlocking` 自己不该存在，
    而是诊断类页面也需要明确它们只是在 current docs 中承担
    connection-level diagnostic override 角色。
  - generic guides、landing quickstarts、backend quickstarts、diagnostics guides
    这几层现在已经开始形成统一的主路径/低层入口分层。
  当前下一条真实工作：
  - 继续从 active diagnostics / backend-specific guides 里找剩余 residual：
    - 优先扫还没纳入 focused contract 的 `COMMON_PITFALLS` /
      `SECURITY_GUIDE` / `ERROR_HANDLING_BEST_PRACTICES`
      这些高频页面里的 direct-connection 语义

- [completed] `backend quickstarts direct-path classification`
  当前 focused 目标：
  - 把 backend-specific quickstarts 中 direct `ISSLConnection` 的使用原因讲清楚，
    避免把 backend 深入示例误读成通用 facade 主路径
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-backend-quickstarts-direct-path-classification.md`
  - 新增 focused contract：
    - `tests/scripts/test_backend_quickstarts_direct_path_classification_contract.sh`
  - 同步更新：
    - `docs/guides/MBEDTLS_USER_GUIDE.md`
    - `docs/guides/WINSSL_QUICKSTART.md`
  当前最终收口证据：
  - `MBEDTLS_USER_GUIDE.md` 现在明确：
    - 简单 HTTPS 示例直接走 `Context.CreateConnection(...)`
      是为了展示 backend raw shipped surface
    - 普通跨后端 HTTPS 客户端仍优先通用的
      `TSSLContextBuilder` + `TSSLConnector` + `TSSLStream`
  - `WINSSL_QUICKSTART.md` 现在明确：
    - 这页聚焦 Windows-native / WinSSL-specific path，
      所以会直接展示 `ISSLConnection`
    - 普通跨后端 HTTPS 客户端仍优先通用 facade 主路径
  focused verification 已通过：
  - `bash -n tests/scripts/test_backend_quickstarts_direct_path_classification_contract.sh`
  - `bash tests/scripts/test_backend_quickstarts_direct_path_classification_contract.sh`
  - `bash tests/scripts/test_mbedtls_active_docs_capability_truth_contract.sh`
  - `bash tests/scripts/test_winssl_quickstart_status_phase_truth_contract.sh`
  - `bash tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
  - `git diff --check`
  当前结论：
  - 这批暴露的不是 backend 文档接口名错了，而是 backend-specific quickstarts
    也需要显式写清“为什么这里要用 direct path”。
  - 现在 generic landing docs 与 backend-specific quickstarts 的主路径分层
    已经重新说成一张图。
  当前下一条真实工作：
  - 继续从 active diagnostics / backend-specific guides 里找剩余 residual：
    - 重点看还没纳入 focused contract 的 timeout/blocking /
      direct-connection troubleshooting 示例
    - 仍然优先 docs/contract 收口，不重开已绿的 runtime/CI 线

- [completed] `landing quickstarts direct-path classification`
  当前 focused 目标：
  - 把最高入口文档里仍展示 direct `ISSLConnection` 的地方统一标回当前主路径 truth：
    - 普通新代码优先 `TSSLContextBuilder` + `TSSLConnector` / `TSSLAcceptor` + `TSSLStream`
    - direct `ISSLConnection` 仍是 shipped 的低层/高级/特定场景入口
    - WinSSL session-resumption 之类的连接级能力示例，需要显式说明为什么要回到 direct path
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-landing-quickstarts-direct-path-classification.md`
  - 新增 focused contract：
    - `tests/scripts/test_landing_quickstarts_direct_path_classification_contract.sh`
  - 同步更新：
    - `README.md`
    - `docs/guides/GETTING_STARTED.md`
    - `docs/guides/QUICKSTART.md`
  当前最终收口证据：
  - `README.md` 现在明确：
    - `核心 API -> TLS 连接` 代码块只是底层 core surface reference
    - 普通新代码仍优先使用前面的 builder + connector + stream 快速路径
  - `GETTING_STARTED.md` 现在明确：
    - 第 4 节 direct `ISSLConnection` 仍是 shipped 的低层入口
    - 普通客户端/服务端接入优先 `TSSLConnector` / `TSSLAcceptor` / `TSSLStream`
  - `QUICKSTART.md` 现在明确：
    - WinSSL session-resumption 示例之所以回到 direct `ISSLConnection`
      是因为 `ISSLSessionResumption` 当前挂在连接对象上
    - 这不替代前面普通 HTTPS 客户端的 connector + stream 主路径
  focused verification 已通过：
  - `bash -n tests/scripts/test_landing_quickstarts_direct_path_classification_contract.sh`
  - `bash tests/scripts/test_landing_quickstarts_direct_path_classification_contract.sh`
  - `bash tests/scripts/test_facade_main_entry_truth_contract.sh`
  - `bash tests/scripts/test_landing_docs_connection_level_sni_guidance_contract.sh`
  - `git diff --check`
  当前结论：
  - 这批暴露的不是接口本身有错，而是 landing quickstarts 还缺少一层
    “主路径 vs 低层入口 / 特定能力路径”的明确分层。
  - 现在 root README / quickstart 系列与之前已收口的 integration / guide truth
    已经重新对齐。
  当前下一条真实工作：
  - 继续交叉审 active backend-specific guides / examples：
    - 哪些 direct `ISSLConnection` / backend-specific helper 示例
      仍缺少“为什么需要 direct path”的解释
    - 优先看高入口但尚未纳入 focused contract 的 active 页面

- [completed] `active guide convenience-surface classification`
  当前 focused 目标：
  - 把 active guides 中仍然直接使用的 `ISSLConnection` convenience surface
    重新标回当前 shipped truth：
    - `ReadString` / `WriteString` = `v1.x` 文本 convenience helper
    - `SetTimeout` / `SetBlocking` = builder-first / connector-first，
      连接侧调用只作为 direct-connection convenience override
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-active-guide-convenience-surface-classification.md`
  - 新增 focused contract：
    - `tests/scripts/test_active_guide_convenience_surface_classification_contract.sh`
  - 同步更新：
    - `docs/INTEGRATION_GUIDE.md`
    - `docs/guides/MIGRATION_GUIDE.md`
    - `docs/guides/USER_GUIDE.md`
  当前最终收口证据：
  - `INTEGRATION_GUIDE` 现在明确：
    - `Conn.SetTimeout` / `Conn.SetBlocking` 在 direct `ISSLConnection` 示例里只是
      local override
    - 若走 `TSSLConnectionBuilder` / `TSSLConnector` / `TSSLAcceptor`，
      timeout/blocking 仍优先在构建阶段配置
  - `MIGRATION_GUIDE` 现在明确：
    - direct `ISSLConnection` 控制方式仍是 shipped surface
    - 框架/transport 集成优先 `TSSLStream` 或 `Read` / `Write`
    - `WriteString` 只是 `v1.x` convenience-core 文本 helper
  - `MIGRATION_GUIDE` 还顺手补回了当前 `ReadString(out ...)` 用法示例，
    不再只展示单向 `WriteString`
  - `USER_GUIDE` 现在明确：
    - client/server 文本往返示例里保留 `ReadString` / `WriteString`
      只是为了快速演示
    - 更复杂的框架 / event-loop / framed-protocol 集成应优先
      `Read` / `Write` 或 `TSSLStream`
  - `GETTING_STARTED` 已复核，当前仍然正确地把主路径放在
    builder + connector + stream 上，因此这批无需改动
  focused verification 已通过：
  - `bash -n tests/scripts/test_active_guide_convenience_surface_classification_contract.sh`
  - `bash tests/scripts/test_active_guide_convenience_surface_classification_contract.sh`
  - `bash tests/scripts/test_readstring_active_example_signature_truth_contract.sh`
  - `bash tests/scripts/test_isslconnection_convenience_surface_classification_contract.sh`
  - `bash tests/scripts/test_migration_guide_active_truth_contract.sh`
  - `git diff --check`
  当前结论：
  - 这批暴露的不是接口实现缺口，而是活跃指南层仍可能把
    still-shipped convenience helper 误读成推荐主路径。
  - 现在 active guides / canonical docs / source comments 对这组 surface
    已经重新说成一张图。
  当前下一条真实工作：
  - 继续从“高可见 active guides + shipped source + backend capability truth”
    交叉审还有没有类似 residual：
    - 例如其它 direct `ISSLConnection` / backend-specific helper 示例
      是否仍缺少 `推荐入口` 与 `兼容/便捷入口` 的明确分层

- [completed] `helper surface classification truth`
  当前 focused 目标：
  - 把 shipped helper surfaces 的权威分级说明收回到同一张图
  - 修正 `API_REFERENCE` 对 WinSSL enterprise helper 主路径的漂移
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-helper-surface-classification-truth.md`
  - 新增 focused contract：
    - `tests/scripts/test_helper_surface_classification_truth_contract.sh`
  - 同步更新：
    - `docs/reference/API_REFERENCE.md`
    - `src/fafafa.ssl.pas`
    - `src/fafafa.ssl.factory.pas`
  当前最终收口证据：
  - canonical `API_REFERENCE` 现在明确：
    - `TSSLFactory.GetLibraryInstance(...)` / `TSSLConnector` / `TSSLAcceptor` /
      `TSSLStream` 是 TLS bootstrap 主入口
    - `CreateDefaultConfig` / `TSSLHelper` / `QuickServer` /
      `CreateOCSPClient` / `CreateCRLManager` 是 convenience helper surface
  - `WinSSL enterprise` 当前主路径已和活跃 guides/source 对齐到：
    - `TSSLEnterpriseConfig.IsFIPSEnabled`
    - `GetTrustedRoots`
    - `GetAllPolicies`
  - old globals:
    - `IsFIPSModeEnabled(...)`
    - `GetEnterpriseTrustedRoots(...)`
    现在只作为 legacy convenience wrappers 记录
  focused verification 已通过：
  - `bash tests/scripts/test_helper_surface_classification_truth_contract.sh`
  - `bash tests/scripts/test_migration_guide_active_truth_contract.sh`
  - `bash tests/scripts/test_active_fips_docs_truth_contract.sh`
  - `git diff --check`
  当前结论：
  - 这次暴露的不是 helper 被删了，而是 exported helper surface 如果不分级，
    调用方会把 facade 里“仍然 shipped”的所有 helper 误判成同等级主入口。
  - canonical API docs 现在已经把 bootstrap main entry、convenience helpers、
    以及 WinSSL enterprise legacy wrappers 分开讲清楚。
  当前下一条真实工作：
  - 继续回到接口/实现完整性：
    - 审 `ISSLConnection` 里 remaining convenience-core / compatibility-core
      residual 是否还有高可见 active docs 漂移
    - 特别是 `ReadString` / `WriteString` / `SetTimeout` 这类仍 shipped 的
      convenience-core surface 是否已经在 active docs 里被正确 classification

- [completed] `integration guide canonical path truth`
  当前 focused 目标：
  - 把 active integration guide truth 收回到唯一权威路径
    `docs/INTEGRATION_GUIDE.md`
  - 消除 `docs/guides/INTEGRATION_GUIDE.md` 继续制造双真相
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-integration-guide-canonical-path-truth.md`
  - 新增 focused contract：
    - `tests/scripts/test_integration_guide_canonical_path_truth_contract.sh`
  - 同步更新：
    - `tests/scripts/test_facade_main_entry_truth_contract.sh`
    - `tests/scripts/test_landing_docs_connection_level_sni_guidance_contract.sh`
    - `tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
    - `docs/INTEGRATION_GUIDE.md`
  当前最终收口证据：
  - `docs/guides/INTEGRATION_GUIDE.md` 已删除
  - active docs 索引 / README / focused contracts 现在统一指向
    `docs/INTEGRATION_GUIDE.md`
  - canonical integration guide 的 active snippets 已收回到：
    - `uses fafafa.ssl;`
    - `fafafa.ssl.context.builder`
    - 不再继续教学 `fafafa.ssl.base` / `fafafa.ssl.tls` 直引
  focused verification 已通过：
  - `bash tests/scripts/test_integration_guide_canonical_path_truth_contract.sh`
  - `bash tests/scripts/test_facade_main_entry_truth_contract.sh`
  - `bash tests/scripts/test_landing_docs_connection_level_sni_guidance_contract.sh`
  - `bash tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
  - `bash tests/scripts/test_isslconnectioninfo_active_guidance_contract.sh`
  - `bash tests/scripts/test_isslsessionresumption_active_guidance_contract.sh`
  - `bash tests/scripts/test_isslcertificateverification_active_guidance_contract.sh`
  - `bash tests/scripts/test_docs_readme_integration_guide_exists_contract.sh`
  - `git diff --check`
  当前结论：
  - 这次暴露的不是单页文档过期，而是 active docs/contract 自己把
    integration guide 分成了两条路径。
  - 现在根目录 `docs/INTEGRATION_GUIDE.md` 已重新成为唯一权威入口。
  当前下一条真实工作：
  - 继续回到 facade helper / compatibility 路线审查：
    - `TSSLHelper`
    - `QuickServer`
    - `CreateOCSPClient` / `CreateCRLManager`
  - 判断这些 shipped helper 目前是否已经在 active docs 里被明确分成
    `推荐入口` 与 `兼容/便捷入口`

- [completed] `macOS batch-loader regression closure`
  当前 focused 目标：
  - 不再把这次 macOS 新失败重判成旧的 loader/path 问题
  - 直接围绕 `26108902159` 的真实回归面收口：
    - `direct_symbols = true`
    - `evp/pem/pkcs12/cms/ocsp module_results = false`
  - 并把这条线写成 durable 记录，避免后面反复拉起同一段怀疑
  已确认的新事实：
  - `tmp/gh-run-26048015976/.../wave_b_macos_loader_symbol_probe_*.json`
    证明同类 macOS gate 在 `2026-05-18` 曾经给出：
    - `evp/pem/pkcs12/cms/ocsp` module truth 全绿
  - `tmp/gh-run-26108902159/.../wave_b_macos_loader_symbol_probe_*.json`
    现在却变成：
    - same `OpenSSL 3.6.2 7 Apr 2026`
    - same direct symbol truth
    - but `evp/pem/pkcs12/cms/ocsp` 全部掉成 `false`
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-macos-batch-loader-regression-closure.md`
  - 新增 focused contract：
    - `tests/scripts/test_macos_batch_loader_regression_closure_contract.sh`
  - 准备落地的最小修法：
    - 给 `TOpenSSLLoader.LoadFunctions(...)` 加 per-call diagnostics
    - 把当前红面的 batch binding table 切到 runtime storage
    - 把 `LoadOpenSSLPEM(...)` 的 loaded 判定收回到真实 read surface
  当前已完成的 focused 验证：
  - `bash -n tests/scripts/test_macos_batch_loader_regression_closure_contract.sh`
  - `bash tests/scripts/test_macos_batch_loader_regression_closure_contract.sh`
  - `fpc ... tests/diagnostic/test_macos_openssl_loader_symbol_probe.pas`
  - `./tmp/test_macos_batch_loader_probe_bin/test_macos_openssl_loader_symbol_probe tmp/test_macos_batch_loader_probe.json`
  - `FAFAFA_FAST_LOCAL=1 ... bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,OCSP --stop-on-fail`
  当前最终收口证据：
  - GitHub run `26110676557`
    - `status=completed`
    - `conclusion=success`
    - `setup/linux-gate/macos-gate/windows-gate/summary` 全部 `success`
  - artifact:
    - `tmp/gh-run-26110676557/wave_b_macos_gate_summary_macos_batch_loader_closure_20260520_89c2a2e.md`
      - `overall: PASS`
    - `tmp/gh-run-26110676557/wave_b_macos_loader_symbol_probe_macos_batch_loader_closure_20260520_89c2a2e.json`
      - same `OpenSSL 3.6.2 7 Apr 2026`
      - direct symbols 全 true
      - `evp/pem/pkcs12/cms/ocsp` module truth 全绿
      - CI loaded-count diagnostics 与本机 baseline 对齐
  当前结论：
  - 这次问题已经被确认并收口为 batch-loader 回归修复，不再是旧的 path/root 怀疑。
  - Windows lane 也随同这次 GitHub run 一并成功，不需要把旧 WinSSL probe 线重新拉起。
  当前下一条真实工作：
  - 回到“接口设计 + 各 backend 实现一致性”总 goal
  - 继续优先静态审查 `TSSLConfig` mixed-scope public record 与 facade 推荐入口，
    只盯当前仍可能误导调用方的 active surface，而不是继续平台 runtime 排障

- [completed] `WinSSL session injection semantics` truth alignment
  已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-winssl-session-injection-semantics-truth.md`
  - 新增 focused contract：
    - `tests/scripts/test_winssl_session_injection_semantics_truth_contract.sh`
  - 当前已收紧的 source/doc truth：
    - `src/fafafa.ssl.winssl.connection.pas`
      现已在 `DoSetSession(...)` 旁明确：
      - caller-supplied session 当前只是 compatibility metadata
      - shared client reconnect 仍主要依赖 Schannel automatic cache key
        (`target name + credential handle`)
    - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
      现已把 `Resumption2.SetSession(Session)` 降级为 compatibility metadata
      说明，而不是显式 native session 注入暗示
    - `docs/guides/WINSSL_USER_GUIDE.md`
      `Phase 6` 现已显式写清：
      - `SetSession(...)` 当前不等于稳定显式恢复语义
    - `docs/BACKEND_SELECTION_GUIDE.md`
      `Windows 应用` 场景现已补清：
      - 如果把 session resumption / tickets 当成已稳定 runtime-proven 能力，
        不应只因为“Windows + 零依赖”就默认停在 WinSSL
  - focused verification 已通过：
    - `bash -n tests/scripts/test_winssl_session_injection_semantics_truth_contract.sh`
    - `bash tests/scripts/test_winssl_session_injection_semantics_truth_contract.sh`
    - `bash tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh`
    - `npx prettier --write docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md docs/guides/WINSSL_USER_GUIDE.md docs/BACKEND_SELECTION_GUIDE.md`
    - `git diff --check`
  - 当前结论：
    - WinSSL 这条线当前最危险的不是“完全没 public surface”
    - 而是 `SetSession(...)` 太容易被高入口示例误读成
      OpenSSL 式显式 session restore 语义
    - 这条 semantic boundary 现在已经在 source 和高入口文档里同步收口
  - 当前下一条真实剩余工作：
    - 继续判断 WinSSL `SessionCacheSupport=sslSupportStable`
      与当前 shared reconnect truth 是否仍然匹配
    - 也就是进一步区分：
      - Schannel automatic cache availability
      - fafafa.ssl caller-visible resumed-handshake semantics
- [completed] `BACKEND_CAPABILITY_MATRIX` version-history truth alignment
  已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-backend-capability-matrix-version-history-truth.md`
  - 新增 focused contract：
    - `tests/scripts/test_backend_capability_matrix_version_history_truth_contract.sh`
  - 当前已收紧的根入口版本口径：
    - `docs/BACKEND_CAPABILITY_MATRIX.md`
      现已先指向：
      - 当前稳定版本 `v1.5.0`
      - `ROADMAP.md`
      - `RELEASE_READINESS_V1.5.0.md`
      - `RELEASE_NOTES.md`
    - 原先裸列的 `v1.4.1` / `v1.4.0` / `v1.3.0`
      现在都已降级成 historical capability milestone
  - focused verification 已通过：
    - `bash -n tests/scripts/test_backend_capability_matrix_version_history_truth_contract.sh`
    - `bash tests/scripts/test_backend_capability_matrix_version_history_truth_contract.sh`
    - `npx prettier --write docs/BACKEND_CAPABILITY_MATRIX.md`
    - `git diff --check`
  - 当前结论：
    - 这次暴露的不是 capability 内容本身，而是根入口底部仍在拿旧 milestone
      冒充当前 release truth
    - 现在这页已经不会再把 `v1.4.x` 历史条目误读成当前 `v1.5.0`
      发布口径
  - 当前下一条真实剩余工作：
    - 继续从根入口 capability/doc truth 线往外扩，
      审查还有哪些 active docs 仍保留旧 milestone /
      phase-snapshot / release-announcement 式口径
- [completed] `BACKEND_CAPABILITY_MATRIX` performance/selection truth alignment
  已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-backend-capability-matrix-performance-selection-truth.md`
  - 新增 focused contract：
    - `tests/scripts/test_backend_capability_matrix_performance_selection_truth_contract.sh`
  - 当前已收紧的根入口 truth：
    - `docs/BACKEND_CAPABILITY_MATRIX.md`
      后半段现已不再维护固定后端性能相对值表
    - 根入口性能说明现已统一回到：
      - `scripts/run_phase2_performance_baseline.sh`
      - `tests/benchmarks/run_all_benchmarks.sh`
      - `docs/guides/PERFORMANCE_GUIDE.md`
      - `docs/guides/PERFORMANCE_OPTIMIZATION_GUIDE.md`
    - 选型建议现已改成 capability-aware recommendation：
      - `WinSSL` 保留 Windows 专有客户端 / 零依赖优势
      - 但同时显式写清 Early Data / caller-provided server OCSP stapling /
        session-resumption runtime truth caveat
      - `OpenSSL` / `MbedTLS` / `WolfSSL` / `FreePascal`
        也都回到各自当前 published capability 边界
  - focused verification 已通过：
    - `bash -n tests/scripts/test_backend_capability_matrix_performance_selection_truth_contract.sh`
    - `bash tests/scripts/test_backend_capability_matrix_performance_selection_truth_contract.sh`
    - `npx prettier --write docs/BACKEND_CAPABILITY_MATRIX.md`
    - `git diff --check`
  - 当前结论：
    - 这次暴露的不是 capability 行本身错，而是“根入口后半段仍拿历史 benchmark
      snapshot 和 blanket recommendation 当当前 truth”
    - 现在性能/选型段也已经和当前 benchmark truth source /
      backend-specific capability 边界收敛到同一口径
  - 当前下一条真实剩余工作：
    - 继续审 `docs/BACKEND_CAPABILITY_MATRIX.md`
      以及相邻高入口文档里剩余的历史快照/版本公告式内容，
      尤其确认 `版本历史` 这类根入口 summary
      是否还会误导当前 v1.5.0 路线判断
- [completed] `BACKEND_CAPABILITY_MATRIX` quick-reference truth alignment
  已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-backend-capability-matrix-quick-reference-truth.md`
  - 新增 focused contract：
    - `tests/scripts/test_backend_capability_matrix_quick_reference_truth_contract.sh`
  - 当前已收紧的 summary truth：
    - `docs/BACKEND_CAPABILITY_MATRIX.md`
      顶层 quick reference 现已和 source / backend-specific truth 对齐：
      - `WinSSL TLS 1.3` 不再写成无条件 `✅`
      - `WinSSL PSK` 不再写成 `⚠️`
      - `FreePascal ALPN / SNI` 不再写成稳定 `✅`
    - 顶层说明现已补清：
      - `WinSSL TLS 1.3` 受 Windows / Schannel 版本门控
      - `FreePascal ALPN / SNI` 当前按 `sslSupportExperimental` 解读
      - `WinSSL PSK` 当前按 unsupported 解读
  - focused verification 已通过：
    - `bash -n tests/scripts/test_backend_capability_matrix_quick_reference_truth_contract.sh`
    - `bash tests/scripts/test_backend_capability_matrix_quick_reference_truth_contract.sh`
    - `npx prettier --write docs/BACKEND_CAPABILITY_MATRIX.md`
    - `git diff --check`
  - 当前结论：
    - `docs/BACKEND_CAPABILITY_MATRIX.md` 这次暴露的不是 section 细节错，
      而是 quick reference 自己已经跑得比 source truth 更快
    - 这种“顶层摘要比下钻文档更激进”的漂移现在已被 focused contract
      冻结住
  - 当前下一条真实剩余工作：
    - 继续审查 `docs/BACKEND_CAPABILITY_MATRIX.md`
      里其它非自动映射行/摘要说明，确认是否还存在
      `summary > source/backend-specific truth` 的残留
- [completed] `ISSLSessionResumption` runtime residual classification tightening 已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-isslsessionresumption-runtime-residual-classification-tightening.md`
  - 新增 focused contract：
    - `tests/scripts/test_isslsessionresumption_runtime_residual_classification_contract.sh`
  - 当前已冻结的 residual truth：
    - `tests/contract/test_backend_contract.pas`
      - intentional compatibility mirror proof
    - `tests/test_mbedtls_connection_session_reused_contract.pas`
      - intentional backend semantic truth proof
    - `tests/test_openssl_connection_session_reused_contract.pas`
      - intentional backend semantic truth proof
  - 当前已去除的 residual 噪音：
    - `tests/winssl/test_session_save_logic.pas`
      - mock getter 已改成 `GetSavedSession`
      - 不再继续冒充 public `GetSession` owner-path 漂移
  - 当前已同步的 source truth：
    - `src/fafafa.ssl.connection.base.pas`
      现已明确：
      - ordinary docs/tests 默认走 `ISSLSessionResumption`
      - direct core session-resumption 当前只剩
        `contract mirror proof + backend-specific semantic truth proofs`
  - focused verification 已通过：
    - `bash -n tests/scripts/test_isslsessionresumption_runtime_residual_classification_contract.sh`
    - `bash tests/scripts/test_isslsessionresumption_runtime_residual_classification_contract.sh`
    - `fpc + run tests/test_mbedtls_connection_session_reused_contract.pas`
    - `fpc + run tests/test_openssl_connection_session_reused_contract.pas`
    - `fpc + run tests/winssl/test_session_save_logic.pas`
    - `rg -lP "\\b(?:Conn|LConn|LConn1|LConn2|ResumedConn|InitialConn|LTLSStream\\.Connection)\\.(?:GetSession|SetSession|IsSessionReused)\\b" tests --glob '!tests/scripts/**' | sort`
    - `git diff --check`
  - 当前结论：
    - session-resumption ordinary runtime lane 与 residual classification lane
      现在都已经收口
    - 后续不应再把 `mbedtls/openssl semantic proof` 或 `mock save helper`
      混同为 owner-path migration 漂移
  - 当前下一条真实剩余工作：
    - 跳出 session-resumption 这条线，继续核对其它公共接口 /
      backend implementation completeness 的真实缺口
- [completed] `ISSLSessionResumption` runtime owner-path migration wave 2
  (`tests/test_freepascal_tls13_early_data.pas`) 已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-isslsessionresumption-runtime-owner-path-migration-wave2-freepascal-tls13-early-data.md`
  - 新增 focused contract：
    - `tests/scripts/test_isslsessionresumption_tls13_early_data_owner_path_contract.sh`
  - 当前已收口的 ordinary runtime truth：
    - `tests/test_freepascal_tls13_early_data.pas`
      现在通过统一 helper：
      - `RequireSessionResumption(...)`
      - `AssertSessionReused(...)`
      来访问 `ISSLSessionResumption` owner path
    - 这份大文件里的 direct-core：
      - `GetSession`
      - `SetSession`
      - `IsSessionReused`
      已全部清掉
  - focused verification 已通过：
    - `bash -n tests/scripts/test_isslsessionresumption_tls13_early_data_owner_path_contract.sh`
    - `bash tests/scripts/test_isslsessionresumption_tls13_early_data_owner_path_contract.sh`
    - `fpc + run tests/test_freepascal_tls13_early_data.pas`
    - `rg -lP "\\b(?:Conn|LConn|LConn1|LConn2|ResumedConn|InitialConn|LTLSStream\\.Connection)\\.(?:GetSession|SetSession|IsSessionReused)\\b" tests --glob '!tests/scripts/**' | sort`
    - `git diff --check`
  - 当前 residual snapshot 已进一步收窄为：
    - `tests/contract/test_backend_contract.pas`
    - `tests/test_mbedtls_connection_session_reused_contract.pas`
    - `tests/test_openssl_connection_session_reused_contract.pas`
    - `tests/winssl/test_session_save_logic.pas`
  - 当前下一条真实剩余工作：
    - 判断 `mbedtls/openssl` 这两份 contract
      是否应继续作为 intentional direct-core semantic proof 保留
    - 明确 `tests/winssl/test_session_save_logic.pas`
      是否只是 mock/save helper residual
    - `tests/contract/test_backend_contract.pas`
      继续作为 compatibility mirror proof，不和 ordinary runtime lane 混淆
- [completed] `ISSLSessionResumption` runtime owner-path migration wave 1 已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-isslsessionresumption-runtime-owner-path-migration-wave1.md`
  - 新增 focused contract：
    - `tests/scripts/test_isslsessionresumption_runtime_owner_path_contract.sh`
  - 当前已迁移的 ordinary runtime / production 用法：
    - `src/fafafa.ssl.connection.builder.pas`
    - `src/fafafa.ssl.tls.pas`
    - `tests/test_connection_builder_hostname_precedence.pas`
    - `tests/test_freepascal_client_certificate_flight_requirements.pas`
    - `tests/test_freepascal_client_session_resumption.pas`
    - `tests/test_freepascal_server_session_resumption.pas`
    - `tests/test_openssl_wolfssl_early_data_connection_contract.pas`
  - 当前已补的直接证据：
    - `tests/test_tls_connector_early_data_contract.pas`
      已重新编译运行，证明 `TSSLConnector.WithSession(...)` 仍按
      `session -> servername -> earlydata -> connect` 顺序工作
  - focused verification 已通过：
    - `bash -n tests/scripts/test_isslsessionresumption_runtime_owner_path_contract.sh`
    - `bash tests/scripts/test_isslsessionresumption_runtime_owner_path_contract.sh`
    - `fpc + run tests/test_connection_builder_hostname_precedence.pas`
    - `fpc + run tests/test_freepascal_client_certificate_flight_requirements.pas`
    - `fpc + run tests/test_freepascal_client_session_resumption.pas`
    - `fpc + run tests/test_freepascal_server_session_resumption.pas`
    - `fpc + run tests/test_openssl_wolfssl_early_data_connection_contract.pas`
    - `fpc + run tests/test_tls_connector_early_data_contract.pas`
    - `git diff --check`
  - 当时的 residual snapshot 已进一步收窄为：
    - `tests/contract/test_backend_contract.pas`
    - `tests/test_freepascal_tls13_early_data.pas`
    - `tests/test_mbedtls_connection_session_reused_contract.pas`
    - `tests/test_openssl_connection_session_reused_contract.pas`
    - `tests/winssl/test_winssl_session_resumption.pas`
    - `tests/winssl/test_session_save_logic.pas`（mock/save-logic helper，不是公共接口 owner-path truth）
  - 当前下一条真实剩余工作：
    - 先处理体量最大的 `tests/test_freepascal_tls13_early_data.pas`
    - 再逐项判断 `mbedtls/openssl` semantic contracts 和 `WinSSL` runtime proof
      是否属于 intentional residual
- [completed] `ISSLSessionResumption` compiler deprecation alignment 已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-isslsessionresumption-compiler-deprecation-alignment.md`
  - 新增 focused contract：
    - `tests/scripts/test_isslsessionresumption_compiler_deprecated_contract.sh`
  - 当前已修正的 source/doc/test truth：
    - `src/fafafa.ssl.base.pas`
      - `GetSession` / `SetSession` / `IsSessionReused`
        现已补齐 `@preferred-access` / `@owner-note` / compiler `deprecated`
    - `src/fafafa.ssl.connection.base.pas`
      - session-resumption residual note 现已明确：
        ordinary docs/tests 默认走 `ISSLSessionResumption` owner path
    - `docs/reference/API_REFERENCE.md`
      - session-resumption core 摘要签名现已明确为：
        - 编译期 deprecated
        - 仅兼容保留
        - 新代码优先走 `ISSLSessionResumption`
    - `docs/reference/INTERFACE_DESIGN_V2.md`
      - session-resumption migration truth 现已提升到：
        - 默认 owner 已切到 `ISSLSessionResumption`
        - core 侧仅兼容保留
        - 源码声明已是编译期 deprecated
    - `tests/contract/test_backend_contract.pas`
      - 保留一条 cross-backend direct-core session mirror proof
      - direct-core `GetSession` / `IsSessionReused` 调用已做局部 warning quarantine
  - focused verification 已通过：
    - `bash -n tests/scripts/test_isslsessionresumption_compiler_deprecated_contract.sh`
    - `bash tests/scripts/test_isslsessionresumption_compiler_deprecated_contract.sh`
    - `bash tests/scripts/test_isslsessionresumption_active_guidance_contract.sh`
    - `mkdir -p tmp/test_backend_contract_session_resumption_deprecation && fpc -B -Fu./src -Fu./tests -FUtmp/test_backend_contract_session_resumption_deprecation -FEtmp/test_backend_contract_session_resumption_deprecation -otmp/test_backend_contract_session_resumption_deprecation/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/test_backend_contract_session_resumption_deprecation/test_backend_contract`
    - `git diff --check`
  - 当前结论：
    - session-resumption 这组方法已不再停留在“owner path 已存在但 core 还像主入口”的中间态
    - source / docs / focused contracts / cross-backend compile proof 现已统一到
      `ISSLSessionResumption owner-first + direct-core compatibility mirror only`
  - 当前下一条真实剩余工作：
    - runtime/semantic 测试里仍有一批 direct-core session calls 尚未完全迁移到 owner path
    - 这批更像“runtime residual migration”，不再是 compiler-surface truth 缺口
- [completed] `ISSLDiagnostics` compiler deprecation alignment 已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-issldiagnostics-compiler-deprecation-alignment.md`
  - 新增 focused contract：
    - `tests/scripts/test_issldiagnostics_compiler_deprecated_contract.sh`
  - 当前已修正的 source/doc/test truth：
    - `src/fafafa.ssl.base.pas`
      - `GetHealthStatus` / `IsHealthy` / `GetDiagnosticInfo` / `GetPerformanceMetrics`
        现已补齐 `@preferred-access` / `@owner-note` / compiler `deprecated`
    - `src/fafafa.ssl.connection.base.pas`
      - diagnostics residual note 现已明确：
        ordinary docs/tests 默认走 `ISSLDiagnostics` owner path
    - `docs/reference/API_REFERENCE.md`
      - diagnostics core getter 摘要签名现已明确为：
        - 编译期 deprecated
        - 仅兼容保留
        - 新代码优先走 `ISSLDiagnostics`
    - `docs/reference/INTERFACE_DESIGN_V2.md`
      - diagnostics migration truth 现已提升到：
        - 默认 owner 已切到 `ISSLDiagnostics`
        - core 侧仅兼容保留
        - 源码声明已是编译期 deprecated
    - `tests/contract/test_backend_contract.pas`
      - 保留一条 cross-backend direct-core diagnostics mirror proof
      - direct-core diagnostics 调用已做局部 warning quarantine
    - `tests/winssl/test_winssl_session_resumption.pas`
      - `GetPerformanceMetrics` 已切回 `ISSLDiagnostics` owner path
  - 当前 direct-core diagnostics residual set 已收窄为：
    - `tests/contract/test_backend_contract.pas`
    - `tests/winssl/test_winssl_connection_edge_cases.pas`
    - `tests/winssl/test_winssl_monitoring.pas`
  - focused verification 已通过：
    - `bash -n tests/scripts/test_issldiagnostics_compiler_deprecated_contract.sh`
    - `bash tests/scripts/test_issldiagnostics_compiler_deprecated_contract.sh`
    - `bash tests/scripts/test_issldiagnostics_active_guidance_contract.sh`
    - `mkdir -p tmp/test_backend_contract_diagnostics_deprecation && fpc -B -Fu./src -Fu./tests -FUtmp/test_backend_contract_diagnostics_deprecation -FEtmp/test_backend_contract_diagnostics_deprecation -otmp/test_backend_contract_diagnostics_deprecation/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/test_backend_contract_diagnostics_deprecation/test_backend_contract`
    - `git diff --check`
  - 当前结论：
    - diagnostics 这组 getter 已不再停留在“owner path 已存在但 core 还像主入口”的中间态
    - source / docs / focused contracts / cross-backend compile proof 现已统一到
      `ISSLDiagnostics owner-first + direct-core compatibility mirror only`
- [completed] WinSSL callback runtime proof markers 已完成收口：
  - 新增计划：
    - `docs/plans/2026-05-19-winssl-callback-runtime-proof-markers.md`
  - 当前已确认的 proof gap：
    - 已下载 Windows artifact：
      - workflow `26092105397`
      - artifact `wave-b-windows-winssl_callback_markers_20260519_184245`
  - 当前已确认的失败事实不是 marker 缺失，而是：
      - `[WINSSL-RUNTIME] callback_surface verify=missing password=missing info=missing`
    - 当前 root cause 已锁定：
      - `test_winssl_unit_comprehensive.lpi`
        实际对应 `tests/winssl/test_winssl_unit_comprehensive.pas`
      - 之前 callback truth 在 `tests/unit/test_winssl_comprehensive.pas`
        里，但 broader suite 并不会运行那份源文件
      - 所以 `tests/run_winssl_tests.ps1`
        的提取逻辑之前从一开始就在抓错 truth source
    - 第二轮 Windows CI (`26092828923`) 已进一步证明：
      - callback marker 本身已经修正为：
        - `[WINSSL-RUNTIME] callback_surface verify=pass password=unsupported info=pass`
      - 但 broader suite 仍失败在：
        - `WinSSL Unit Tests (Comprehensive)`
      - 当前新 root cause 不是 library 语义错误，而是：
        - `tests/winssl/test_winssl_unit_comprehensive.pas`
          把 password callback 的 fail-closed 提示文案判断得过窄
        - 实际 runtime 抛出的 message 为：
          - `Password callback is not published by the current WinSSL backend runtime...`
        - 这与当前已发布 truth 一致，但没有被测试接受为 unsupported 同义证据
  - 当前已落地的本地收口：
    - `tests/winssl/test_winssl_unit_comprehensive.pas`
      已补实际 Windows callback configuration tests
      并已放宽 password callback 断言以接受当前真实 fail-closed 文案
    - `tests/run_winssl_tests.ps1`
      新增 `callback_surface` runtime marker 汇总逻辑
    - `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md`
      已补 callback marker 检索口径并指向真实 Windows test source
    - 新增 focused shell contract：
      - `tests/scripts/test_winssl_runtime_callback_markers_contract.sh`
  - 当前本地验证已通过：
    - `bash -n tests/scripts/test_winssl_runtime_callback_markers_contract.sh`
    - `bash tests/scripts/test_winssl_runtime_callback_markers_contract.sh`
    - `git diff --check`
  - 当前最终验证已完成：
    - commit:
      - `12e62a2`
      - `26bad43`
    - GitHub Actions:
      - 首轮 root-cause fix 验证：
        - run `26092828923`
        - 证明 marker 已收敛到：
          - `[WINSSL-RUNTIME] callback_surface verify=pass password=unsupported info=pass`
        - 同时暴露出 Windows comprehensive test 对 fail-closed 文案判断过窄
      - 第二轮 follow-up 验证：
        - run `26093405878`
        - `windows-gate` / `linux-gate` / `macos-gate` / `summary` 全部 `success`
        - Windows artifact 现已同时证明：
          - `[WINSSL-RUNTIME] callback_surface verify=pass password=unsupported info=pass`
          - `[WINSSL-RUNTIME] suite_summary passed=8 failed=0 total=8 success_rate=100`
          - `[WINSSL-RUNTIME] suite_end status=PASS`
  - 当前结论：
    - WinSSL callback runtime proof marker 已从“抓错测试对象导致的 `missing/missing/missing`”
      收敛到
      artifact 可 grep 的稳定 Windows runtime truth
    - broader WinSSL runtime suite 也已回到全绿
- [completed] WinSSL FIPS capability truth tightening 已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-winssl-fips-capability-truth-tightening.md`
  - 新增/收紧 focused contracts：
    - `tests/scripts/test_active_fips_docs_truth_contract.sh`
    - `tests/scripts/test_active_capability_docs_runtime_truth_contract.sh`
    - `tests/test_backend_fips_capability_truth_contract.pas`
  - 当前已修正的真实 implementation/capability drift：
    - `src/fafafa.ssl.winssl.lib.pas`
      不再继续误发：
      - `SupportsFIPSMode=True`
    - 当前 WinSSL FIPS 相关 public/source truth 现在统一回到：
      - `fafafa.ssl.winssl.enterprise`
        只提供 Windows FIPS policy / enterprise helper 检测
      - `ISSLLibrary.GetCapabilities.SupportsFIPSMode`
        不再把这条 helper/policy 检测发布成 backend capability
  - 当前已同步收口的活跃 docs truth：
    - `docs/reference/WINSSL_DESIGN.md`
    - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
    - `docs/guides/WINSSL_USER_GUIDE.md`
    - `docs/PLATFORM_SUPPORT.md`
    - `docs/reference/BACKEND_SELECTOR_DESIGN.md`
    - `docs/reference/BACKEND_ABSTRACTION_LAYER_DESIGN.md`
    - `docs/reference/API_REFERENCE.md`
    - `docs/MIGRATION_GUIDE_V1.1.md`
    - `docs/guides/MIGRATION_GUIDE.md`
    - `docs/guides/USER_GUIDE.md`
    - `docs/guides/TROUBLESHOOTING.md`
  - focused verification 已通过：
    - `bash -n tests/scripts/test_active_fips_docs_truth_contract.sh`
    - `bash tests/scripts/test_active_fips_docs_truth_contract.sh`
    - `bash -n tests/scripts/test_active_capability_docs_runtime_truth_contract.sh`
    - `bash tests/scripts/test_active_capability_docs_runtime_truth_contract.sh`
    - `fpc -B -Fu./src -Fu./tests -FUtmp/test_backend_fips_capability_truth -FEtmp/test_backend_fips_capability_truth -otmp/test_backend_fips_capability_truth/test_backend_fips_capability_truth_contract tests/test_backend_fips_capability_truth_contract.pas`
    - `./tmp/test_backend_fips_capability_truth/test_backend_fips_capability_truth_contract`
    - `git diff --check`
  - 当前结论：
    - 这批收掉的是一个真实 implementation/capability drift，不是单纯措辞漂移
    - 关键边界不是“Windows 能否检测/遵循 FIPS policy”，而是：
      - 这条线当前没有被 fafafa.ssl 发布成 backend capability/runtime contract
    - 后续继续扫 backend completeness 时，应优先区分：
      - system policy / enterprise helper
      - versus
      - shipped public capability / selector-visible truth
- [completed] Custom cipher capability truth alignment 已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-custom-cipher-capability-truth-alignment.md`
  - 新增 focused contract：
    - `tests/scripts/test_custom_cipher_capability_truth_contract.sh`
    - `tests/test_backend_custom_cipher_capability_truth_contract.pas`
  - 当前已修正的真实 implementation/capability drift：
    - `src/fafafa.ssl.openssl.backed.pas`
      不再无条件发布：
      - `SupportsCustomCipherSuites=True`
    - `OpenSSL` custom-cipher capability 现在统一跟随共享 runtime gate：
      - `SSL_CTX_set_cipher_list`
      - `SSL_CTX_set_ciphersuites`
    - `src/fafafa.ssl.freepascal.lib.pas`
      不再继续误发：
      - `SupportsCustomCipherSuites=True`
    - `src/fafafa.ssl.freepascal.context.pas`
      - `src/fafafa.ssl.winssl.context.pas`
      - `src/fafafa.ssl.mbedtls.context.pas`
      - `src/fafafa.ssl.wolfssl.context.pas`
      的 `SetCipherList` / `SetCipherSuites` 现在统一回到：
      - custom non-default override -> fail-closed `unsupported`
      - empty clear / shipped baseline defaults -> 继续允许作为 compatibility/default-context path
  - 当前已同步收口的 docs/test truth：
    - `docs/BACKEND_CAPABILITY_MATRIX.md`
    - `docs/reference/API_REFERENCE.md`
    - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
    - `docs/reference/WINSSL_PERFORMANCE_TUNING.md`
    - `docs/guides/WINSSL_BEST_PRACTICES.md`
    - `docs/guides/MBEDTLS_USER_GUIDE.md`
    - 以及被旧心智污染的：
      - `tests/test_direct_library_default_config_parity.pas`
      - `tests/mbedtls/test_mbedtls_server_accept_simple.pas`
      - `tests/winssl/test_winssl_context_config.pas`
      - `tests/winssl/test_winssl_context_comprehensive.pas`
      - `tests/unit/test_winssl_comprehensive.pas`
  - focused verification 已通过：
    - `bash -n tests/scripts/test_custom_cipher_capability_truth_contract.sh`
    - `bash tests/scripts/test_custom_cipher_capability_truth_contract.sh`
    - `fpc -B -Fu./src -Fu./tests -FUtmp/test_custom_cipher_truth -FEtmp/test_custom_cipher_truth -otmp/test_custom_cipher_truth/test_backend_custom_cipher_capability_truth_contract tests/test_backend_custom_cipher_capability_truth_contract.pas`
    - `./tmp/test_custom_cipher_truth/test_backend_custom_cipher_capability_truth_contract`
    - `fpc -B -Fu./src -Fu./tests -FUtmp/test_direct_library_default_config_parity -FEtmp/test_direct_library_default_config_parity -otmp/test_direct_library_default_config_parity/test_direct_library_default_config_parity tests/test_direct_library_default_config_parity.pas`
    - `./tmp/test_direct_library_default_config_parity/test_direct_library_default_config_parity`
    - `git diff --check`
  - 当前结论：
    - 这批收掉的是一个真实 implementation/capability drift，不是文档措辞问题
    - 关键新基线不是“所有 cipher setter 都彻底禁掉”，而是：
      - custom non-default override 必须跟 capability/public truth 对齐
      - shipped baseline defaults 继续作为 default-context compatibility path
    - 后续继续扫接口/后端完整性时，应优先找这种：
      - capability 已发布
      - 但 setter/runtime 还在 storage-only / helper-missing / system-policy-only 路径上
- [completed] OpenSSL callback publication runtime gate 已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-openssl-callback-publication-runtime-gate.md`
  - 当前已修正的实现真问题：
    - `src/fafafa.ssl.openssl.backed.pas`
      不再无条件发布：
      - `SupportsCallbacks=True`
    - OpenSSL callback capability 现在统一跟随共享 runtime gate：
      - verify callback helper
      - password callback helper
      - password callback userdata helper
      - info callback helper
    - `src/fafafa.ssl.openssl.context.pas`
      的 verify/password/info setter 现在统一回到：
      - callback surface 不完整时 non-nil fail-closed
      - `nil` clear 继续允许作为 compatibility clear/no-op
  - 当前已补强的 focused contracts：
    - `tests/scripts/test_callback_capability_truth_contract.sh`
    - `tests/scripts/test_callback_setter_fail_closed_contract.sh`
    - `tests/test_backend_callback_capability_truth_contract.pas`
    - `tests/test_backend_callback_setter_fail_closed_contract.pas`
  - focused verification 已通过：
    - `bash -n tests/scripts/test_callback_capability_truth_contract.sh`
    - `bash tests/scripts/test_callback_capability_truth_contract.sh`
    - `bash -n tests/scripts/test_callback_setter_fail_closed_contract.sh`
    - `bash tests/scripts/test_callback_setter_fail_closed_contract.sh`
    - `fpc -B -Fu./src -Fu./tests -FUtmp/test_callback_capability_truth -FEtmp/test_callback_capability_truth -otmp/test_callback_capability_truth/test_backend_callback_capability_truth_contract tests/test_backend_callback_capability_truth_contract.pas`
    - `./tmp/test_callback_capability_truth/test_backend_callback_capability_truth_contract`
    - `fpc -B -Fu./src -Fu./tests -FUtmp/test_callback_setter_fail_closed -FEtmp/test_callback_setter_fail_closed -otmp/test_callback_setter_fail_closed/test_backend_callback_setter_fail_closed_contract tests/test_backend_callback_setter_fail_closed_contract.pas`
    - `./tmp/test_callback_setter_fail_closed/test_backend_callback_setter_fail_closed_contract`
  - 当前结论：
    - 这批收掉的是一个真实 implementation/capability drift，不是文档措辞问题
    - 后续继续做 backend completeness 审查时，应优先查这种：
      - capability bool 已发布
      - 但 setter/runtime 仍依赖未锁定 symbol/helper 的路径
- [completed] Migration guide low-level helper entrypoint truth 已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-migration-guide-lowlevel-helper-entrypoint-truth.md`
  - 收紧 existing contract：
    - `tests/scripts/test_migration_guide_active_truth_contract.sh`
  - 当前已修正的活跃文档：
    - `docs/guides/MIGRATION_GUIDE.md`
  - 当前已收掉的真问题：
    - OpenSSL low-level helper 片段不再继续使用旧：
      - `TSSLFactory.GetLibrary(...)`
    - 迁移指南现在统一回到：
      - `TSSLFactory.GetLibraryInstance(...)`
      即使是在 backend-specific low-level helper 语境里也不再回流旧工厂入口
  - focused verification 已通过：
    - `bash -n tests/scripts/test_migration_guide_active_truth_contract.sh`
    - `bash tests/scripts/test_migration_guide_active_truth_contract.sh`
    - `git diff --check`
  - 当前结论：
    - 这批修掉的是 migration guide 里的单点旧工厂调用残余，不是新的 runtime 缺口
    - 后续继续扫 migration / specialized docs 时，应优先找这种已缩到单个示例片段的旧入口残留
- [completed] Security best practices pinning helper truth 已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-security-best-practices-pinning-helper-truth.md`
  - 新增 focused contract：
    - `tests/scripts/test_security_best_practices_pinning_helper_truth_contract.sh`
  - 当前已修正的活跃文档：
    - `docs/guides/security-best-practices.md`
  - 当前已收掉的真问题：
    - certificate pinning 示例不再继续教授不存在的：
      - `LoadCertificateFromFile(...)`
    - 示例现在明确回到：
      - `LoadCertificateFromPEM(...)`
      - `X509_free(...)`
    - 文档也已明确说明：
      - 这里走的是 OpenSSL raw certificate handle 路径
      - 不是 backend-neutral helper
  - focused verification 已通过：
    - `bash -n tests/scripts/test_security_best_practices_pinning_helper_truth_contract.sh`
    - `bash tests/scripts/test_security_best_practices_pinning_helper_truth_contract.sh`
    - `git diff --check`
  - 当前结论：
    - 这批修掉的是 security specialized guide 中“复制即错”的 helper 名漂移，不是 runtime 缺口
    - 后续继续扫 specialized guides 时，应优先找同类仍在教授不存在 helper/API 名称的片段
- [completed] PKCS12 helper guide active truth 已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-pkcs12-helper-guide-active-truth.md`
  - 新增 focused contract：
    - `tests/scripts/test_pkcs12_helper_guide_active_truth_contract.sh`
  - 当前已修正的活跃文档：
    - `docs/guides/PKCS12_USER_GUIDE.md`
    - `docs/reference/API_REFERENCE.md`
  - 当前已收掉的真问题：
    - `PKCS12_USER_GUIDE` 不再继续教授源码中不存在的：
      - `LoadCertificateFromFile(...)`
      - `LoadPrivateKeyFromFile(...)`
    - PKCS#12 活跃指南现在明确区分：
      - 高入口 helper：`TPKCS12Manager` / `DefaultPKCS12Options`
      - OpenSSL raw API：`fafafa.ssl.openssl.api.pkcs12` / `fafafa.ssl.openssl.api.pem`
    - `API_REFERENCE` 现在已补出 façade 上当前公开的 PKCS#12 helper 入口
  - focused verification 已通过：
    - `bash -n tests/scripts/test_pkcs12_helper_guide_active_truth_contract.sh`
    - `bash tests/scripts/test_pkcs12_helper_guide_active_truth_contract.sh`
    - `git diff --check`
  - 当前结论：
    - 这批修掉的是 PKCS#12 高入口指导仍把调用方带回不存在旧 API 的 docs completeness 问题，不是 runtime 缺口
    - 后续再继续做证书/密钥文档完整性审查时，不应再把 `PKCS12_USER_GUIDE` 当成旧 helper 名称的来源
- [completed] Capability precedence doc truth 已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-capability-precedence-doc-truth.md`
  - 新增 focused contract：
    - `tests/scripts/test_capability_precedence_docs_truth_contract.sh`
  - 当前已修正的高入口 capability 文档：
    - `docs/CAPABILITY_MATRIX_GUIDE.md`
    - `docs/reference/API_REFERENCE.md`
    - `docs/BACKEND_CAPABILITY_MATRIX.md`
  - 当前已收掉的真问题：
    - capability docs 现在明确说明：
      - paired feature 的 `SNISupport` / `ALPNSupport` / `OCSPStaplingSupport` / `CertTransparencySupport` / `SessionTicketsSupport`
        才是当前 truth source
      - legacy `SupportsSNI` / `SupportsALPN` / `SupportsOCSPStapling` / `SupportsCertificateTransparency` / `SupportsSessionTickets`
        只是 compatibility projection
      - `SupportsTLS13` 仍然是 primary bool truth，因为当前没有 `TLS13Support`
    - capability guide / API reference 的高入口示例现在回到：
      - `TSSLFactory.GetLibraryInstance(...)`
    - capability 记录示例中的 `CompatibilityLevel` 类型现在回到源码真相：
      - `Integer`
    - capability guide 的新 backend 示例现在明确：
      - paired feature 先写 `*Support`
      - 再 `NormalizeLegacyCapabilityBooleans(Result);`
  - focused verification 已通过：
    - `bash -n tests/scripts/test_capability_precedence_docs_truth_contract.sh`
    - `bash tests/scripts/test_capability_precedence_docs_truth_contract.sh`
    - `git diff --check`
  - 当前结论：
    - 这批修掉的是 capability 控制面仍在暗示“双主真相”的文档漂移，不是 runtime/backends 缺口
    - 后续若继续扫 capability matrix / selector / serializer，不应再把 active docs 当成 paired features 的 dual-truth 来源
- [completed] Interface audit current truth refresh 已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-interface-audit-current-truth-refresh.md`
  - 新增 focused contract：
    - `tests/scripts/test_interface_audit_current_truth_contract.sh`
  - 当前已修正的权威静态审计输出：
    - `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
  - 当前已收掉的真问题：
    - 审计报告不再继续误写：
      - factory / builder 仍主动把 `ServerName` 写回 context
      - 活跃文档仍承诺 `ISSLServerConnection` 存在
      - `BufferSize` / `HandshakeTimeout` 只是“看起来像 inert 字段”
    - 当前审计基线现在重新回到：
      - 高层 SNI family = `warning/reject/ignore` 的 frozen compatibility surface
      - 活跃 docs 已明确说明当前 public source 尚未声明 `ISSLServerConnection`
      - `TSSLConfig` 仍是 mixed-scope public record，但 `BufferSize` / `HandshakeTimeout` 在 create-path 上是显式 reject
  - focused verification 已通过：
    - `bash -n tests/scripts/test_interface_audit_current_truth_contract.sh`
    - `bash tests/scripts/test_interface_audit_current_truth_contract.sh`
    - `git diff --check`
  - 当前结论：
    - 这批修掉的是“路线判断依然被旧审计结论带偏”的控制面问题，不是 runtime 缺口
    - 后续再讨论接口设计优先级时，不应再把这三条已收口事实当成当前 live blocker
- [completed] Public unit/import guidance truth 已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-public-unit-import-guidance-truth.md`
  - 新增 focused contract：
    - `tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
  - 当前已修正的高入口活跃文档：
    - `docs/guides/USER_GUIDE.md`
    - `docs/guides/WINSSL_QUICKSTART.md`
    - `docs/guides/WINSSL_USER_GUIDE.md`
    - `docs/guides/MBEDTLS_USER_GUIDE.md`
    - `docs/guides/TROUBLESHOOTING.md`
    - `docs/reference/API_REFERENCE.md`
  - 当前已收掉的真问题：
    - 高入口 docs 不再继续教授：
      - `fafafa.ssl.abstract.intf`
      - `fafafa.ssl.abstract.types`
      - 不存在的 `fafafa.ssl.openssl` facade unit
      - 不存在的 `CreateSSLLibrary(...)`
      - 旧枚举名 `sslLibraryWinSSL` / `sslLibraryOpenSSL` / `sslLibraryAutoDetect`
      - 旧上下文枚举名 `sslContextClient`
      - 不存在的 `GetLibraryName`
      - 手动 `LoadOpenSSL` 作为普通应用入口步骤
    - 高入口创建/导入心智现在统一回到：
      - `fafafa.ssl`
      - `TSSLFactory.GetLibraryInstance(...)`
      - `TSSLFactory.IsLibraryAvailable(...)`
      - `sslCtxClient`
      - `LibraryTypeToString(Lib.GetLibraryType)`
    - `API_REFERENCE` 现在明确区分：
      - 高入口 public library-entrypoint
      - backend-specific low-level creators
  - focused verification 已通过：
    - `bash -n tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
    - `bash tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
    - `git diff --check`
  - 当前结论：
    - 这批修掉的是最前排 onboarding/reference 文档把用户带回已删除单元、旧 creator 和错误枚举名的问题
    - 后续如果继续扫 onboarding / troubleshooting / backend guides，不应再把这些 public import / factory 路径当成 current source truth
- [completed] Migration guide active truth 已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-migration-guide-active-truth.md`
  - 新增 focused contract：
    - `tests/scripts/test_migration_guide_active_truth_contract.sh`
  - 当前已修正的高入口活跃文档：
    - `docs/guides/MIGRATION_GUIDE.md`
  - 当前已收掉的真问题：
    - `MIGRATION_GUIDE` 顶部不再停在：
      - `v0.8`
      - `v0.7/v0.8` 作为当前 active 迁移主线
    - 活跃迁移示例不再继续使用：
      - `fafafa.ssl.abstract.intf`
      - 不存在的 `fafafa.ssl.openssl` facade unit
      - backend-specific `CreateOpenSSLLibrary` 作为主迁移入口
    - 迁移主路径现在明确重新回到：
      - `fafafa.ssl`
      - `fafafa.ssl.context.builder`
      - `TSSLFactory`
      - `TSSLConnector`
      - `TSSLStream`
    - client SNI / hostname 当前迁移心智现在明确回到：
      - `TSSLConnector.ConnectSocket(..., ServerName)`
      - 或 `ISSLClientConnection.SetServerName(...)`
    - WinSSL enterprise helper 当前名称不再写旧：
      - `IsFIPSEnabled`
      - `GetTrustedRoots`
      - `GetAllPolicies`
    - OpenSSL low-level error helper 当前不再被误写成 generic public facade API
  - focused verification 已通过：
    - `bash -n tests/scripts/test_migration_guide_active_truth_contract.sh`
    - `bash tests/scripts/test_migration_guide_active_truth_contract.sh`
    - `git diff --check`
  - 当前结论：
    - 这批修掉的是高入口迁移指南把旧版本叙事、旧单元名和旧 helper 教成现行主路径的问题
    - 后续如果继续扫 migration / onboarding 文档，不应再把 `MIGRATION_GUIDE` 当成 `v0.x` 时代的旧入口
- [completed] Active connection API docs truth 已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-active-connection-api-docs-truth.md`
  - 新增 focused contract：
    - `tests/scripts/test_active_connection_api_docs_truth_contract.sh`
  - 当前已修正的高入口活跃文档：
    - `docs/reference/API_DOCUMENTATION.md`
    - `docs/guides/WINSSL_BEST_PRACTICES.md`
    - `docs/guides/WINSSL_USER_GUIDE.md`
  - 当前已收掉的真问题：
    - `API_DOCUMENTATION` 不再把：
      - `ISSLConnection.Connect` 教成 `Connect(host, port)`
      - `CreateConnection` 教成直接接收端口号
      - `Disconnect` / `Connection.GetLastError` / `GetPeerCertificateVerified`
        这类不存在或过时 surface 当成当前 public API
    - `API_DOCUMENTATION` 的 `ISSLConnection` section 现在重新回到 current shipped truth：
      - `Connect: Boolean`
      - `Write(const ABuffer; ACount)`
      - `Read(var ABuffer; ACount)`
      - `WriteString`
      - `ReadString`
    - `WINSSL_BEST_PRACTICES` 的测试最佳实践不再继续教授：
      - `LConn.Connect('example.com', 443)`
      - `LConn.Connect('localhost', 8443)`
    - `WINSSL_USER_GUIDE` 不再把 WinSSL 讲成与其它 backend “完全相同的接口”
      现在明确回到：
      - 共享统一核心 public interface
      - published capability 仍以后端 `ISSLLibrary.GetCapabilities` 为准
  - focused verification 已通过：
    - `bash -n tests/scripts/test_active_connection_api_docs_truth_contract.sh`
    - `bash tests/scripts/test_active_connection_api_docs_truth_contract.sh`
    - `git diff --check`
  - 当前结论：
    - 这批修掉的是 active docs 把旧连接形状和 backend overclaim 继续教给用户的问题
    - 后续如果继续扫 `ISSLConnection` / WinSSL completeness，不应再把这些高入口旧 `Connect(host, port)` 片段当成 current source truth
- [completed] ALPN owner-path active guidance 已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-alpn-owner-path-active-guidance.md`
  - 新增 focused contract：
    - `tests/scripts/test_alpn_owner_path_active_guidance_contract.sh`
  - 当前已修正的活跃入口：
    - `docs/guides/WINSSL_USER_GUIDE.md`
    - `examples/https_server/https_server_alpn.pas`
  - 当前已收掉的真问题：
    - `GetSelectedALPNProtocol` 当前已是 `ISSLConnectionInfo` owner surface 的 deprecated mirror
    - 但活跃 WinSSL 指南和 ALPN server example 之前还把它教成 `ISSLConnection` 普通主路径
    - 活跃入口现在统一回到：
      - guide 文案显式指向 `ISSLConnectionInfo.GetSelectedALPNProtocol`
      - example 先 `Supports(Connection, ISSLConnectionInfo, ...)` 再读取协商结果
  - focused verification 已通过：
    - `bash -n tests/scripts/test_alpn_owner_path_active_guidance_contract.sh`
    - `bash tests/scripts/test_alpn_owner_path_active_guidance_contract.sh`
    - `fpc -B -Fu./src -Fu./examples -FUtmp/example_https_server_alpn -FEtmp/example_https_server_alpn -otmp/example_https_server_alpn/https_server_alpn examples/https_server/https_server_alpn.pas`
    - `git diff --check`
  - 当前结论：
    - 这批修掉的是 active guidance 对 deprecated ALPN mirror 的回流，不是 backend 实现缺口
    - 后续再看 `ISSLConnectionInfo` owner-path completeness 时，不应再把这条 ALPN 活跃示例误导当成未审问题
- [completed] `ReadString` 活跃示例签名真相 已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-readstring-active-example-signature-truth.md`
  - 新增 focused contract：
    - `tests/scripts/test_readstring_active_example_signature_truth_contract.sh`
  - 当前已修正的活跃入口：
    - `docs/reference/API_REFERENCE.md`
    - `docs/guides/USER_GUIDE.md`
    - `docs/guides/MIGRATION_GUIDE.md`
    - `examples/04_https_rest_client.pas`
  - 当前已收掉的真问题：
    - 多份活跃 guide / reference / example 之前还把 `ReadString` 教成“直接返回字符串”的旧签名
    - 当前 shipped source 真相一直是：
      - `function ReadString(out AStr: string): Boolean;`
    - 活跃入口现在统一改成：
      - `if Conn.ReadString(LData) then ...`
  - focused verification 已通过：
    - `bash -n tests/scripts/test_readstring_active_example_signature_truth_contract.sh`
    - `bash tests/scripts/test_readstring_active_example_signature_truth_contract.sh`
    - `fpc -B -Fu./src -Fu./examples -FUtmp/example_04_https_rest_client -FEtmp/example_04_https_rest_client -otmp/example_04_https_rest_client/example_04_https_rest_client examples/04_https_rest_client.pas`
    - `git diff --check`
  - 当前结论：
    - 这批修掉的是高入口用法签名漂移，而不是 runtime bug
    - 后续如果继续扫 `ISSLConnection` / guide completeness，不应再把 `ReadString` 的旧“string-return”用法当成现状
- [completed] `ISSLConnection` convenience surface classification 已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-isslconnection-convenience-surface-classification.md`
  - 新增 focused contract：
    - `tests/scripts/test_isslconnection_convenience_surface_classification_contract.sh`
  - 当前已修正的 source / docs truth：
    - `src/fafafa.ssl.base.pas`
    - `docs/reference/API_REFERENCE.md`
    - `docs/reference/INTERFACE_DESIGN_V2.md`
    - `docs/ARCHITECTURE.md`
    - `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
  - 当前已收掉的真问题：
    - `INTERFACE_DESIGN_V2` 不再把：
      - `ReadString` / `WriteString`
      - `SetTimeout` / `GetTimeout`
      - `SetBlocking` / `GetBlocking`
      误写成“当前源码已移除”
    - `ARCHITECTURE` 的最小 `ISSLConnection` snippet 现在明确标注为 conceptual slice，而不是 current source truth
    - source comments / canonical API doc 现在明确：
      - `ReadString` / `WriteString` = `v1.x` convenience-core 文本 helper
      - timeout/blocking = `v1.x` connection-adjacent convenience surface，推荐 builder-first
    - 设计审计报告不再把这组 convenience 方法和已进入 owner-surface demotion 的 mirror methods 混成同一类“应立即移除”问题
  - focused verification 已通过：
    - `bash -n tests/scripts/test_isslconnection_convenience_surface_classification_contract.sh`
    - `bash tests/scripts/test_isslconnection_convenience_surface_classification_contract.sh`
    - `git diff --check`
  - 当前结论：
    - 这批修掉的是接口路线真相分叉，而不是 backend 实现缺口
    - 后续若继续做 `ISSLConnection` slimming，应把 convenience 方法退出 core 视为独立的 `v2` API surgery，而不是继续误报成“当前实现已经偏离文档”
- [completed] API reference certificate surfaces truth 已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-api-reference-certificate-surfaces-truth.md`
  - 新增 focused contract：
    - `tests/scripts/test_api_reference_certificate_surfaces_truth_contract.sh`
  - 当前已修正的 active canonical doc：
    - `docs/reference/API_REFERENCE.md`
  - 当前已收掉的真问题：
    - `ISSLCertificate` 代码块不再遗漏：
      - `LoadFromMemory`
      - `SaveToStream`
      - `GetInfo`
      - `GetPublicKeyAlgorithm`
      - `GetSignatureAlgorithm`
      - `GetDaysUntilExpiry`
      - `GetSubjectCN`
      - `GetExtension`
      - `GetFingerprint(...)`
      - issuer-link / clone helpers
    - `ISSLCertificate` 的扩展集合类型不再错误写成：
      - `TStringList`
      现在已回到源码真相：
      - `TSSLStringArray`
    - `ISSLCertificateStore` 不再缺失高入口独立小节
  - focused verification 已通过：
    - `bash -n tests/scripts/test_api_reference_certificate_surfaces_truth_contract.sh`
    - `bash tests/scripts/test_api_reference_certificate_surfaces_truth_contract.sh`
    - `git diff --check`
  - 当前结论：
    - `API_REFERENCE` 的证书/证书库高入口 surface 现在重新回到 shipped source truth
    - 后续不应再把证书面 canonical API doc 当成“只有窄化子集”的旧状态
- [completed] API reference library/context surface truth 已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-api-reference-library-context-surface-truth.md`
  - 新增 focused contract：
    - `tests/scripts/test_api_reference_library_context_surface_truth_contract.sh`
  - 当前已修正的 active canonical doc：
    - `docs/reference/API_REFERENCE.md`
  - 当前已收掉的真问题：
    - `ISSLLibrary` 代码块不再遗漏：
      - `SetDefaultConfig`
      - `GetDefaultConfig`
      - `GetStatistics`
      - `ResetStatistics`
    - `ISSLContext` 代码块不再遗漏：
      - `SetPreferredVersion` / `GetPreferredVersion`
      - `LoadCertificatePEM` / `LoadPrivateKeyPEM`
      - `SetSessionCacheSize` / `GetSessionCacheSize`
      - `SetOptions` / `GetOptions`
      - `SetServerName` / `GetServerName`
      - `SetALPNProtocols` / `GetALPNProtocols`
      - `SetCertVerifyFlags` / `GetCertVerifyFlags`
      - `SetPasswordCallback` / `SetInfoCallback`
      - certificate pinning helpers
  - focused verification 已通过：
    - `bash -n tests/scripts/test_api_reference_library_context_surface_truth_contract.sh`
    - `bash tests/scripts/test_api_reference_library_context_surface_truth_contract.sh`
    - `git diff --check`
  - 当前结论：
    - `API_REFERENCE` 的高入口 `ISSLLibrary` / `ISSLContext` 代码块现在重新回到 shipped source truth
    - 后续不应再把这两块旧的精简 code listing 当成当前公开接口面
- [completed] Optional interface capability alignment 已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-optional-interface-capability-alignment.md`
  - 新增 focused contract：
    - `tests/scripts/test_optional_interface_capability_alignment_contract.sh`
  - 当前已修正的实现边界：
    - `src/fafafa.ssl.openssl.context.pas`
    - `src/fafafa.ssl.openssl.connection.pas`
    - `src/fafafa.ssl.openssl.backed.pas`
    - `src/fafafa.ssl.wolfssl.context.pas`
    - `src/fafafa.ssl.wolfssl.lib.pas`
  - 当前已收掉的真问题：
    - OpenSSL base context 不再无条件实现：
      - `ISSLEarlyDataContext`
      - `ISSLServerOCSPStaplingContext`
    - OpenSSL base connection 不再无条件实现：
      - `ISSLEarlyDataConnection`
    - WolfSSL base context 不再无条件实现：
      - `ISSLServerOCSPStaplingContext`
    - OpenSSL / WolfSSL 当前都改成 capability-gated subclass 暴露 optional interface
    - `CreateContext` / `CreateConnection` 路径现在与 `GetCapabilities` 的 optional surface truth 对齐
  - focused verification 已通过：
    - `bash -n tests/scripts/test_optional_interface_capability_alignment_contract.sh`
    - `bash tests/scripts/test_optional_interface_capability_alignment_contract.sh`
    - `python3 scripts/compile_all_modules.py`: `187/187 PASS`
    - `git diff --check`: PASS
  - 当前结论：
    - 这批修掉的是接口设计层的结构性漂移，而不是单个文案或单个 capability 字段
    - builder / factory / source contract 对 optional interface 的公共心智现在重新一致
- [completed] Active release / platform truth sweep 已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-active-release-platform-truth-sweep.md`
  - 新增 focused contract：
    - `tests/scripts/test_active_release_platform_truth_contract.sh`
  - 当前已修正的高入口活跃文档：
    - `docs/RELEASE_NOTES.md`
    - `docs/PLATFORM_SUPPORT.md`
    - `docs/guides/WINSSL_USER_GUIDE.md`
    - `docs/ZERO_DEPENDENCY_DEPLOYMENT.md`
  - 当前已收掉的真问题：
    - `RELEASE_NOTES` 顶部不再把 `v1.0.0` 历史快照冒充当前稳定发布入口
    - `PLATFORM_SUPPORT` 不再保留：
      - `97.5% / 99%+`
      - `macOS 验证中`
      - `WinSSL 100% 完成 / 所有 6 个阶段完成`
      这类旧阶段口径
    - `WINSSL_USER_GUIDE` 不再把 `session resumption / tickets` 写成 `100% 完成 / 完全支持`
    - `ZERO_DEPENDENCY_DEPLOYMENT` 不再把 WinSSL 总体状态写成 `100% 完成，生产就绪`
    - 活跃文档中的 `yourusername` / `your-repo` / `your.email@example.com` 占位入口已清掉
  - focused verification 已通过：
    - `bash -n tests/scripts/test_active_release_platform_truth_contract.sh`
    - `bash tests/scripts/test_active_release_platform_truth_contract.sh`
    - `git diff --check`: PASS
  - 当前结论：
    - 当前 release/platform/WinSSL 高入口文档已经重新锚回：
      - `docs/ROADMAP.md`
      - `docs/test_reports/RELEASE_READINESS_V1.5.0.md`
      - `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
      - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
    - 后续如果继续看 WinSSL，不应再从“100% 完成”这类旧口径出发，而应直接从当前 session/runtime truth 进入
- [completed] Implemented backend future truth sweep 已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-implemented-backend-future-truth-sweep.md`
  - 新增 focused contract：
    - `tests/scripts/test_implemented_backend_future_truth_contract.sh`
  - 当前已修正的活跃文档：
    - `docs/reference/BACKEND_ABSTRACTION_LAYER_DESIGN.md`
    - `docs/guides/USER_GUIDE.md`
    - `docs/MIGRATION_GUIDE_V1.1.md`
    - `docs/ARCHITECTURE.md`
    - `docs/NATIVE_HANDLE_QUICK_REF.md`
  - 当前已收掉的真问题：
    - `FreePascal` 不再被 backend abstraction design 写成 `❌ 计划中`
    - `USER_GUIDE` 不再把 `MbedTLS` 推荐写成“未来”
    - `MIGRATION_GUIDE_V1.1` 不再把 `sslFreePascal` 描述成等待未来发布的 backend
    - `ARCHITECTURE` / `NATIVE_HANDLE_QUICK_REF` 不再保留“纯 Pascal backend 还在未来”的旧示例心智
  - focused verification 已通过：
    - `bash -n tests/scripts/test_implemented_backend_future_truth_contract.sh`
    - `bash tests/scripts/test_implemented_backend_future_truth_contract.sh`
    - `git diff --check`: PASS
  - 当前结论：
    - 这批把“已实现 backend 仍被活跃文档说成未来态”的主要残留收掉了
    - 后续接口/后端 completeness 审查现在能直接建立在当前 backend family truth 上
- [completed] Active root doc link repair 已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-active-root-doc-link-repair.md`
  - 新增 focused contract：
    - `tests/scripts/test_active_root_doc_link_repair_contract.sh`
  - 当前已修正的高入口活跃文档：
    - `docs/PLATFORM_SUPPORT.md`
    - `docs/RELEASE_NOTES.md`
    - `docs/TOOLS.md`
    - `docs/ZERO_DEPENDENCY_DEPLOYMENT.md`
    - `docs/guides/WINSSL_USER_GUIDE.md`
  - 当前已收掉的真问题：
    - 根入口文档不再指向旧的 `QUICKSTART.md` / `GETTING_STARTED.md` / `API_REFERENCE.md` / `TROUBLESHOOTING.md` 裸路径
    - `RELEASE_NOTES` 不再保留旧的 `docs/QuickStart.md` / `docs/API_Reference.md` / `docs/PROJECT_FINAL_SUMMARY.md`
    - `ZERO_DEPENDENCY_DEPLOYMENT` 不再指向 `.claude/plan/WINSSL_COMPLETION_REPORT.md`
    - `WINSSL_USER_GUIDE` 不再保留：
      - `WINSSL_HTTPS_TEST_REPORT.md`
      - `../PHASE2_2_COMPLETION_REPORT.md`
      - `../PHASE2_4_TEST_REPORT.md`
      这些失效入口
  - focused verification 已通过：
    - `bash -n tests/scripts/test_active_root_doc_link_repair_contract.sh`
    - `bash tests/scripts/test_active_root_doc_link_repair_contract.sh`
    - `git diff --check`: PASS
  - 当前结论：
    - 这批把 5 个最容易被先打开的入口文档重新接回当前真实存在的 guides/reference/test_reports 页面
    - 后续 backend/platform/WinSSL 审查不再先被 broken links 绊住
- [completed] Backend doc linkage + enum truth 已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-backend-doc-linkage-and-enum-truth.md`
  - 新增 focused contract：
    - `tests/scripts/test_backend_doc_linkage_and_enum_truth_contract.sh`
  - 当前已修正的活跃真相：
    - `docs/BACKEND_CAPABILITY_MATRIX.md`
      - 不再引用不存在的：
        - `reference/OPENSSL_BACKEND.md`
        - `reference/WINSSL_BACKEND.md`
      - 现已改为 live backend references：
        - `reference/OPENSSL_MODULES.md`
        - `reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
        - `reference/WINSSL_DESIGN.md`
        - `reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
    - `docs/reference/API_REFERENCE.md`
      - `TSSLLibraryType` 示例现已补齐：
        - `sslAutoDetect`
        - `sslOpenSSL`
        - `sslWolfSSL`
        - `sslMbedTLS`
        - `sslWinSSL`
        - `sslFreePascal`
      - 不再把 `sslMbedTLS` 标成“计划中”
    - `src/fafafa.ssl.base.pas`
      - `sslFreePascal` 注释不再保留“未来”表述
  - focused verification 已通过：
    - `bash -n tests/scripts/test_backend_doc_linkage_and_enum_truth_contract.sh`
    - `bash tests/scripts/test_backend_doc_linkage_and_enum_truth_contract.sh`
    - `git diff --check`: PASS
  - 当前结论：
    - 这批修掉的是活跃 backend 文档导航和公共枚举说明的真相漂移
    - 后续不会再被坏链接和过期 enum 注释带偏 backend completeness 审查
- [completed] Backend capability truth tightening 已完成 focused 收口：
  - 新增计划：
    - `docs/plans/2026-05-19-backend-capability-truth-tightening.md`
  - 新增 focused contract：
    - `tests/scripts/test_optional_backends_session_cache_capability_contract.sh`
  - 扩展 focused contract：
    - `tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh`
  - 当前已修正的实现 / 文档真相：
    - `src/fafafa.ssl.mbedtls.lib.pas`
      - `SessionCacheSupport` 现已明确发布为 `sslSupportStable`
    - `src/fafafa.ssl.wolfssl.lib.pas`
      - `SessionCacheSupport` 现已明确发布为 `sslSupportStable`
    - `docs/BACKEND_CAPABILITY_MATRIX.md`
      - `Session Resumption` 快速参考现已收紧到：
        - `FreePascal = ⚠️`
        - `WinSSL = ⚠️`
    - `docs/guides/QUICKSTART.md` / `docs/reference/WINSSL_DESIGN.md`
      - 不再把 WinSSL session resumption 写成已 runtime-proven 的稳定成功/70-90% 性能收益
    - `docs/reference/BACKEND_ABSTRACTION_LAYER_DESIGN.md` / `docs/reference/BACKEND_SELECTOR_DESIGN.md`
      - 不再把 WinSSL `OCSP Stapling` / `Session Ticket` 写成无条件完整支持
  - focused verification 已通过：
    - `bash -n tests/scripts/test_optional_backends_session_cache_capability_contract.sh`
    - `bash tests/scripts/test_optional_backends_session_cache_capability_contract.sh`
    - `bash -n tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh`
    - `bash tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh`
    - `python3 scripts/compile_all_modules.py`: `187/187 PASS`
    - `git diff --check`: PASS
  - 当前结论：
    - 这批修掉的不是单纯文案味道，而是：
      - optional backend `SessionCacheSupport` 发布缺口
      - WinSSL 活跃 capability/docs truth 对后续路线判断的误导
- [completed] WinSSL session-info probe allowlist 已完成本地收口：
  - 新增计划：`docs/plans/2026-05-19-winssl-session-info-probe-allowlist.md`
  - 新增 focused contract：
    - `tests/scripts/test_winssl_session_info_probe_allowlist_contract.sh`
  - 当前 guard 已锁住：
    - 允许的受控 probe site：
      - `src/fafafa.ssl.winssl.connection.pas`
      - `tests/winssl/test_winssl_session_resumption.pas`
    - 明确禁止 residual shim：
      - `src/fafafa.ssl.winssl.session.pas`
  - focused verification 已通过：
    - `bash -n tests/scripts/test_winssl_session_info_probe_allowlist_contract.sh`
    - `bash tests/scripts/test_winssl_session_info_probe_allowlist_contract.sh`
    - `git diff --check`: PASS
  - 当前结论：
    - 以后如果又有新的未隔离 `SECPKG_ATTR_SESSION_INFO` query 混进 repo，会被 source contract 直接打红
- [completed] WinSSL native-probe handle metadata 已完成第一轮 live Windows 取证：
  - manual run `26071754477`，head=`0751afc`
  - `linux-gate`: `success`
  - `macos-gate`: `success`
  - `windows-gate`: `failure`
  - downloaded Windows artifact：
    - `tmp/gh-run-26071754477/windows/winssl_runtime_suite_winssl_handle_metadata_20260519_google.log`
  - live evidence 已明确证明：
    - `backend=winssl`
    - `handle_valid=true`
    - `dwLower/dwUpper` 非零
    - worker 仍在
      - `stage=before_query_context_attributes`
      之后立刻以 `-1073741819` 退出
  - live summary artifact 也已确认：
    - `closure readiness`
      - `linux=PASS`
      - `macos=PASS`
      - `windows=FAIL`
    - `handoff bundle`
      - `handoff_state: NEEDS_GATE_REPAIR`
      - `consistency_status: CONSISTENT`
  - 当前结论：
    - 当前残留已经不再是“句柄也许无效”
    - 而是“在有效 WinSSL context 上查询 `SECPKG_ATTR_SESSION_INFO` 本身就会把 isolated worker 打死”
- [completed] WinSSL session shim safe fallback 已完成本地收口：
  - 新增计划：`docs/plans/2026-05-19-winssl-session-shim-safe-fallback.md`
  - 新增 focused contract：
    - `tests/scripts/test_winssl_session_shim_safe_fallback_contract.sh`
  - `src/fafafa.ssl.winssl.session.pas`
    - compatibility shim 已移除直接 `QueryContextAttributesW(...)` / risky session-info attribute 路径
    - 当前已改回保守 fallback：
      - `Format('winssl-session-%p', [Pointer(AContext)])`
      - `SetSessionMetadata(..., False)`
  - focused verification 已通过：
    - `bash -n tests/scripts/test_winssl_session_shim_safe_fallback_contract.sh`
    - `bash tests/scripts/test_winssl_session_shim_safe_fallback_contract.sh`
    - `bash tests/scripts/test_winssl_session_truth_source_contract.sh`
    - `git diff --check`: PASS
  - 当前结论：
    - `winssl.session.pas` 现在重新符合“compatibility shim only”的定位
    - 不会再绕过当前 WinSSL native-probe quarantine 再私自碰 risky session-info query
- [completed] WinSSL native-probe handle metadata 已完成本地收口：
  - 新增计划：`docs/plans/2026-05-19-winssl-native-probe-handle-metadata.md`
  - 新增 focused contract：
    - `tests/scripts/test_winssl_native_probe_handle_metadata_contract.sh`
  - `tests/winssl/test_winssl_session_resumption.pas`
    - 当前已新增 `BackendTypeText(...)`
    - native probe 在 `before_query_context_attributes` 前现在还会额外输出：
      - `backend`
      - `handle_valid`
      - `dwLower`
      - `dwUpper`
  - focused verification 已通过：
    - `bash -n tests/scripts/test_winssl_native_probe_handle_metadata_contract.sh`
    - `bash tests/scripts/test_winssl_native_probe_handle_metadata_contract.sh`
    - `bash tests/scripts/test_winssl_native_probe_stage_markers_contract.sh`
    - `bash tests/scripts/test_winssl_native_probe_worker_quarantine_contract.sh`
    - `fpc -Twin64 ... tests/winssl/test_winssl_session_resumption.pas`
    - `git diff --check`: PASS
  - 当前结论：
    - 下一轮 Windows artifact 不仅会告诉我们 crash 在 `QueryContextAttributesW(...)` 边界前后
    - 还会直接告诉我们当时的 native handle 是否被 WinSSL 自己视为 valid，以及句柄双字内容长什么样
- [completed] Wave B/B2 closure Windows runtime truth 已完成 live GitHub 复核：
  - manual run `26071188795` 已完成，head=`9a47c33`
  - summary artifact 已确认：
    - `closure readiness`
      - `windows | FAIL | ... suite_end_status=FAIL`
      - `closure_status: IN_PROGRESS`
    - `cross summary`
      - `windows | FAIL`
    - `handoff bundle`
      - `handoff_state: NEEDS_GATE_REPAIR`
      - `consistency_status: CONSISTENT`
  - 当前结论：
    - closure/cross/handoff 四层 truth 现在在真实 GitHub workflow 上已经重新对齐
- [completed] WinSSL native-probe stage markers 已完成第一轮 live Windows 取证：
  - manual run `26071361489`，head=`c99fd07`
  - 当前已下载 Windows artifact：
    - `tmp/gh-run-26071361489/windows/winssl_runtime_suite_winssl_stage_markers_20260519_google.log`
  - 新证据已明确收窄 crash boundary：
    - `stage=before_supports`
    - `stage=after_supports`
    - `stage=before_get_native_handle`
    - `stage=after_get_native_handle handle_nil=false`
    - `stage=before_query_context_attributes`
    - 随后 `native_probe_worker exit_code=-1073741819`
  - 当前结论：
    - crash 现在已经明确不在 `Supports(...)` / `GetNativeHandle` 之前
    - 当前最高价值边界已收窄到 `QueryContextAttributesW(SECPKG_ATTR_SESSION_INFO, ...)` 调用本身
- [completed] WinSSL native-probe stage markers 已完成本地收口：
  - 新增计划：`docs/plans/2026-05-19-winssl-native-probe-stage-markers.md`
  - 新增 focused contract：
    - `tests/scripts/test_winssl_native_probe_stage_markers_contract.sh`
  - `tests/winssl/test_winssl_session_resumption.pas`
    - `TryQueryNativeSessionReuse(...)` 现在已接收显式 `label`
    - 当前 probe body 已补齐阶段性 `native_probe` markers：
      - `stage=before_supports`
      - `stage=after_supports`
      - `stage=before_get_native_handle`
      - `stage=after_get_native_handle`
      - `stage=before_query_context_attributes`
      - `stage=query_failed`
      - `stage=after_query_context_attributes`
      - `stage=exception`
    - 初始握手与 same-context attempt 的 native probe 调用点现在都会把对应 label 传进 helper
  - focused verification 已通过：
    - `bash -n tests/scripts/test_winssl_native_probe_stage_markers_contract.sh`
    - `bash tests/scripts/test_winssl_native_probe_stage_markers_contract.sh`
    - `bash tests/scripts/test_winssl_native_probe_worker_quarantine_contract.sh`
    - `bash tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
    - `fpc -Twin64 ... tests/winssl/test_winssl_session_resumption.pas`
    - `git diff --check`: PASS
  - 当前结论：
    - 下一轮 GitHub Windows native-probe worker 即使继续以 `-1073741819` 退出，`last_marker` 也不应再只停在 `pending=true`
    - 当前最高价值下一步已经重新收敛为：
      - 用 Windows runner 实证 `last_marker` 新落点
      - 再决定下一刀是 owner-surface / native handle / `QueryContextAttributesW` 哪个边界
- [completed] Wave B/B2 closure Windows runtime truth 已完成 focused 收口：
  - 新增计划：`docs/plans/2026-05-19-wave-b-b2-closure-windows-runtime-truth.md`
  - 新增 focused contracts：
    - `tests/scripts/test_wave_b_b2_closure_windows_runtime_fail_contract.sh`
    - `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_windows_runtime_fail_contract.sh`
  - `scripts/check_wave_b_b2_closure_readiness.sh`
    - 当前已新增可选 `--windows-runtime-transcript`
    - 若未显式传入且已提供 `--windows-summary`
      - 会默认跟随 sibling `winssl_runtime_suite_<run_id>.log`
    - runtime transcript 现在只负责在 `suite_end_status=FAIL` 时把 Windows closure state 降成 `FAIL`
    - 不会反向把缺 summary 的场景抬成 `PASS`
  - `scripts/prepare_wave_b_b2_handoff_bundle.sh`
    - 现在会把 Windows sibling runtime transcript 显式透传给 closure checker
  - focused verification 已通过：
    - `bash -n scripts/check_wave_b_b2_closure_readiness.sh`
    - `bash tests/scripts/test_wave_b_b2_closure_windows_runtime_fail_contract.sh`
    - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_windows_runtime_fail_contract.sh`
    - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`
    - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_gate_repair_state_contract.sh`
    - `bash tests/scripts/test_wave_b_b2_closure_next_actions_contract.sh`
    - `bash tests/scripts/test_wave_b_b2_consistency_explicit_windows_runtime_logs_required_contract.sh`
    - `bash tests/scripts/test_wave_b_b2_consistency_windows_runtime_substantive_contract.sh`
  - real artifact re-check 已通过：
    - downloaded run `26070488337` platform artifacts
    - 用 `scripts/prepare_wave_b_b2_handoff_bundle.sh` 重新生成四层报告后，`closure readiness` 已改为：
      - `windows | FAIL | ... suite_end_status=FAIL`
      - `closure_status: IN_PROGRESS`
    - `handoff bundle` 继续保持：
      - `handoff_state: NEEDS_GATE_REPAIR`
      - `consistency_status: CONSISTENT`
  - 当前结论：
    - `cross summary` / `closure readiness` / `evidence consistency` / `handoff bundle`
      对 run `26070488337` 的 Windows runtime failure 已重新对齐
    - macOS failure 仍是同批独立问题，不应与 WinSSL native-probe worker 崩溃混为一条线
- [completed] WinSSL native-probe manual investigation lane 已完成本地收口：
  - 新增计划：`docs/plans/2026-05-19-winssl-native-probe-manual-investigation-lane.md`
  - 新增 focused workflow/source contract：`tests/scripts/test_wave_b_b2_winssl_native_probe_input_contract.sh`
  - `wave-b-b2-manual.yml` / `.github/workflows/wave-b-b2-manual.yml.disabled`
    - 当前已新增可选 `workflow_dispatch` 输入：
      - `winssl_enable_native_probe`
    - Windows `Run broader WinSSL runtime suite` step 现在只会在显式 truthy 输入时注入：
      - `FAFAFA_WINSSL_ENABLE_NATIVE_PROBE=1`
    - 留空或 `false` 时会显式记录：
      - native probe disabled by default
    - 当前仍不会自动注入：
      - `FAFAFA_WINSSL_REQUIRE_NATIVE_REUSE`
  - `.github/README.md`
    - 当前已明确记录 `winssl_enable_native_probe` 是有风险的 Schannel evidence lane，默认关闭
  - focused verification 已通过：
    - `bash -n tests/scripts/test_wave_b_b2_winssl_native_probe_input_contract.sh`
    - `bash tests/scripts/test_wave_b_b2_winssl_native_probe_input_contract.sh`
    - `bash tests/scripts/test_wave_b_b2_winssl_session_host_input_contract.sh`
    - `bash tests/scripts/test_wave_b_b2_strict_input_description_contract.sh`
    - `bash tests/scripts/test_wave_b_b2_optional_runner_artifact_download_workflow_contract.sh`
    - `bash tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
    - `git diff --check`: PASS
  - live GitHub verification 已完成：
    - `git push origin master`: PASS
    - `gh workflow run wave-b-b2-manual.yml -f run_id=winssl_native_probe_20260519_google -f strict_closure=false -f winssl_session_host=www.google.com -f winssl_enable_native_probe=true`: PASS
    - manual run `26068984446`: `FAILURE`
    - GitHub step log 已确认：
      - `Using WinSSL session resumption host override: www.google.com`
      - `Enabling risky WinSSL native probe for Schannel session evidence`
    - downloaded Windows runtime artifact confirms:
      - 失败点仍落在 first public signal 之后
      - 没有任何 `native_probe ...` marker 成功写出
      - `WinSSL Session Resumption Truth` 退出码仍为 `-1073741819`
  - 当前结论：
    - repo 已具备 bounded、可复用、已实跑证明接通的 native-probe manual investigation lane
    - `www.google.com + native_probe=true` 这轮 live run 说明当前 public-handle native probe 在 GitHub Windows runner 上依旧不安全
    - 失败边界与旧证据一致：
      - 初始 public reuse signal 已输出
      - 尚未进入首条 `native_probe` marker
      - 紧接着以 `-1073741819` 退出
    - 对这类 opt-in lane，`wave_b_cross_platform_summary` / `handoff_bundle CLOSED` 只能说明 summary/closure 链存在，不能当作 native-probe 成功证据；真实判断必须看 workflow run conclusion 和 `winssl_runtime_suite_*.log`
- [completed] WinSSL session runtime host-override investigation lane 已完成本地收口：
  - 新增计划：`docs/plans/2026-05-19-winssl-session-runtime-host-override-investigation.md`
  - 新增 focused workflow/source contract：`tests/scripts/test_wave_b_b2_winssl_session_host_input_contract.sh`
  - `wave-b-b2-manual.yml` / `.github/workflows/wave-b-b2-manual.yml.disabled`
    - 当前已新增可选 `workflow_dispatch` 输入：
      - `winssl_session_host`
    - Windows `Run broader WinSSL runtime suite` step 现在只会在输入非空时注入：
      - `FAFAFA_WINSSL_SESSION_HOST`
    - 留空时继续打印并使用测试程序默认 host，不改变既有默认 lane
  - `.github/README.md`
    - 当前已明确记录 `winssl_session_host` 的调查用途与默认空值语义
  - 同批顺手修掉一条真实 workflow contract 漂移：
    - `tests/scripts/test_wave_b_b2_optional_runner_artifact_download_workflow_contract.sh`
      不再错误钉死 `actions/download-artifact@v4`
    - 现在改为锁住 pinned action truth，而不是旧版本标签
  - focused verification 已通过：
    - `bash -n tests/scripts/test_wave_b_b2_winssl_session_host_input_contract.sh`
    - `bash tests/scripts/test_wave_b_b2_winssl_session_host_input_contract.sh`
    - `bash tests/scripts/test_wave_b_b2_strict_input_description_contract.sh`
    - `bash tests/scripts/test_wave_b_b2_optional_runner_artifact_download_workflow_contract.sh`
    - `git diff --check`: PASS
    - `gh auth status`: PASS
  - live GitHub verification 已通过：
    - `git push origin master`: PASS
    - `gh workflow run wave-b-b2-manual.yml -f run_id=winssl_host_probe_20260519_google -f strict_closure=false -f winssl_session_host=www.google.com`: PASS
    - manual run `26068474291`: `SUCCESS`
    - downloaded Windows runtime artifact confirms:
      - `host=www.google.com`
      - `observed_reuse=false`
      - `session_configured=true`
  - 当前结论：
    - repo 已具备 bounded、可复用、已实跑证明接通的 GitHub Windows runner host-override 调查入口
    - 这次非默认 host 调查说明当前 `observed_reuse=false` 并不只是默认 `www.cloudflare.com` 单点现象
    - 如果继续沿 WinSSL session runtime 深挖，下一步更适合继续扩样 host family 或打开 native probe 做更窄的 Schannel 证据，而不是再回头改 workflow plumbing
- [completed] WinSSL session-reuse benchmark truth alignment 已完成 focused 收口：
  - 新增计划：`docs/plans/2026-05-19-winssl-session-reuse-benchmark-truth-alignment.md`
  - 新增 focused source contract：`tests/scripts/test_winssl_session_reuse_benchmark_truth_contract.sh`
  - `tests/winssl/test_winssl_session_reuse_benchmark.pas`
    - 现已改走 `ISSLSessionResumption` owner path
    - 现已区分：
      - `SessionConfiguredCount`
      - `ObservedReuseCount`
    - 现已修掉 benchmark metrics 被整条覆盖的真实逻辑 bug
    - 现已修掉 success-count 为 `0` 时的除零/异常展示风险
  - `tests/winssl/SESSION_REUSE_BENCHMARK_GUIDE.md`
    - 现已对齐当前 conservative WinSSL runtime truth：
      - `observed_reuse=false`
      - `session_configured=true`
    - 不再把 timing delta 或历史 `70-90%` 目标当作 native resumed-handshake 已证实结论
  - focused verification 已通过：
    - `bash -n tests/scripts/test_winssl_session_reuse_benchmark_truth_contract.sh`
    - `bash tests/scripts/test_winssl_session_reuse_benchmark_truth_contract.sh`
    - `fpc -Twin64 ... tests/winssl/test_winssl_session_reuse_benchmark.pas`
    - `git diff --check`: PASS
  - 当前结论：
    - WinSSL benchmark residual lane 已不再继续传播旧 public/core session 语义
    - 如果继续沿 WinSSL session 路线推进，下一刀更适合直接进入 native resumed-handshake / session tickets 行为调查
    - 如果回到更高价值主线，则应继续横向审其它 backend implementation completeness / runtime truth
- [completed] session-resumption guide old-name truth freeze 已完成 focused 收口：
  - 新增计划：`docs/plans/2026-05-19-session-resumption-guide-old-name-freeze.md`
  - 新增 focused source contract：`tests/scripts/test_session_resumption_guide_old_name_truth_contract.sh`
  - `docs/guides/QUICKSTART.md`
    - Session 保存/恢复/复用示例现已统一改走 `ISSLSessionResumption`
  - `docs/guides/TROUBLESHOOTING.md`
    - WinSSL Session 复用排障与性能示例现已不再教学 `IsSessionResumed` / direct `SetSession`
  - `docs/guides/USER_GUIDE.md`
    - 性能优化里的复用检测现已切到 owner path
  - focused verification 已通过：
    - `bash -n tests/scripts/test_session_resumption_guide_old_name_truth_contract.sh`
    - `bash tests/scripts/test_session_resumption_guide_old_name_truth_contract.sh`
    - `git diff --check`: PASS
  - focused residual scan 已确认：
    - active guides 里的旧 session 名称漂移当前已收干净
    - repo 内剩余 `GetSessionID` / `IsSessionResumed` 主要位于：
      - `docs/reference/API_REFERENCE.md` 的历史/兼容性说明
      - `tests/winssl/SESSION_REUSE_BENCHMARK_GUIDE.md` 的 WinSSL 专项 benchmark 文档
      - contract / plan / progress 台账自身
  - 当前结论：
    - ordinary active guides 的 session-resumption truth 已基本对齐
    - 如果继续沿 session-resumption 文档线推进，下一刀更适合切 `tests/winssl/SESSION_REUSE_BENCHMARK_GUIDE.md`
    - 如果回到更高价值主线，则更适合继续 backend completeness / backend-specific runtime truth 审查
- [completed] facade / main-entry truth freeze 已完成 focused 收口：
  - 新增计划：`docs/plans/2026-05-19-facade-main-entry-truth-freeze.md`
  - 新增 focused source contract：`tests/scripts/test_facade_main_entry_truth_contract.sh`
  - `docs/README.md`
    - 快速开始现已切到 `uses fafafa.ssl` 的门面入口
    - 推荐路径现已展示 `TSSLConnector.FromContext(Ctx)`，同时保留 direct `ISSLClientConnection.SetServerName(...)` 真相
  - `src/fafafa.ssl.pas`
    - 头部示例现已切到 facade connector 主路径
  - `src/fafafa.ssl.factory.pas`
    - 头部示例与参数说明现已统一为 `sslCtxClient` / `sslCtxServer`
  - `docs/guides/INTEGRATION_GUIDE.md`
    - 当前已不再教学旧的 `sslClient` 枚举名
  - focused verification 已通过：
    - `bash -n tests/scripts/test_facade_main_entry_truth_contract.sh`
    - `bash tests/scripts/test_facade_main_entry_truth_contract.sh`
    - `git diff --check`: PASS
  - 当前结论：
    - highest-visibility facade/main-entry truth source 已对齐到当前 public 真相
    - 下一刀更适合转向 session-resumption 旧命名文档漂移，而不是再回头重扫 `sslClient` / split-unit main entry
- [completed] `WinSSL` connection peer-certificate issuer-link completeness 已完成 focused 收口：
  - 新增计划：`docs/plans/2026-05-19-winssl-peer-cert-issuer-link.md`
  - 新增 focused runtime test：`tests/winssl/test_winssl_peer_certificate_surface.pas`
  - 新增 Lazarus entry：`tests/winssl/test_winssl_peer_certificate_surface.lpi`
  - `src/fafafa.ssl.winssl.connection.pas`
    - `GetPeerCertificate()` 现在会在可用链中补回 leaf issuer link
    - `GetPeerCertificateChain()` 现在会给 returned chain entries 接上 issuer link
  - `tests/run_winssl_tests.ps1` 现在已接入 `WinSSL Peer Certificate Surface` runtime lane
  - 本地 `Win64 cross-target + wine` 已先 RED 后 GREEN，`tests/contract/test_backend_contract.pas` 继续 green
- [completed] connection-level peer-certificate issuer-link completeness 已完成 cross-backend 收口：
  - 已覆盖：
    - `FreePascal`
    - `OpenSSL`
    - `WolfSSL`
    - `MbedTLS`
    - `WinSSL`
  - `MbedTLS` 新增计划：`docs/plans/2026-05-19-mbedtls-peer-cert-chain-issuer-link.md`
  - `tests/test_mbedtls_connection_peer_certificate_contract.pas`
    - 现在已锁住 leaf+issuer chain materialization 与 leaf issuer-link truth
  - `src/fafafa.ssl.mbedtls.connection.pas`
    - `GetPeerCertificate()` 现在会从 native peer chain materialize leaf，并补回 issuer link
    - `GetPeerCertificateChain()` 不再把 native chain 截断成单个 leaf
    - chain entries 现在会顺序保留 `GetIssuerCertificate()` truth
  - focused verification 已通过：
    - `tests/test_mbedtls_connection_peer_certificate_contract.pas`: `14 passed / 0 failed`
    - `tests/contract/test_backend_contract.pas`: `135 total / 111 passed / 0 failed / 24 skipped`
    - `git diff --check`: PASS
  - 下一刀应转向“各 backend 的 verification / optional surface 还有没有剩余 completeness seam”，而不是重开已关掉的 peer-cert issuer-link lane
- [completed] cross-backend `ISSLCertificate.Clone()` issuer-link completeness 已完成 focused 收口：
  - 新增计划：`docs/plans/2026-05-19-certificate-clone-issuer-link.md`
  - 新增 focused contract：`tests/test_certificate_clone_issuer_link_contract.pas`
  - `src/fafafa.ssl.openssl.certificate.pas`
    - `Clone()` 现在会保留 `FIssuerCert`
  - `src/fafafa.ssl.wolfssl.certificate.pas`
    - `Clone()` 现在会保留 `FIssuerCert`
  - `src/fafafa.ssl.mbedtls.certificate.pas`
    - `Clone()` 现在会保留 `FIssuerCert`
  - `src/fafafa.ssl.winssl.certificate.pas`
    - `Clone()` 现在会保留 `FIssuerCert`
  - 当前 clone truth 已重新对齐到 `FreePascal` 语义参考：
    - clone 后保留 leaf fingerprint truth
    - clone 后保留 `GetIssuerCertificate()` truth
  - focused verification 已通过：
    - Linux focused contract：`16 passed / 0 failed`
    - `Win64 cross-target + wine` focused contract：`8 passed / 0 failed / 3 skipped`
    - `tests/contract/test_backend_contract.pas`：`135 total / 111 passed / 0 failed / 24 skipped`
    - `git diff --check`：PASS
  - 下一刀应继续横向审剩余 certificate-verification / optional surface completeness seam，而不是重开这条 clone issuer-link lane
- [completed] `ISSLCertificateVerification` high-visibility owner path 已完成 focused 收口：
  - 新增计划：`docs/plans/2026-05-19-isslcertificateverification-high-visibility-owner-path.md`
  - `src/fafafa.ssl.connection.builder.pas`
    - client/server handshake failure path 现在优先走 `ISSLCertificateVerification`
  - `src/fafafa.ssl.tls.pas`
    - connector/acceptor handshake failure path 现在优先走 `ISSLCertificateVerification`
  - `docs/guides/OCSP_USAGE_GUIDE.md` / `docs/guides/CT_IMPLEMENTATION_GUIDE.md`
    - 高可见失败示例不再教学 direct core `GetVerifyResultString`
  - `tests/scripts/test_isslcertificateverification_active_guidance_contract.sh`
    - 现在额外锁住 builder / TLS facade / OCSP guide / CT guide 的 owner-path truth
  - focused verification 已通过：
    - `bash tests/scripts/test_isslcertificateverification_active_guidance_contract.sh`
    - `tests/test_connection_builder_hostname_precedence.pas`: `29 passed / 0 failed`
    - `tests/test_tls_connector_hostname_override_precedence.pas`: `6 passed / 0 failed`
    - `tests/contract/test_backend_contract.pas`: `135 total / 111 passed / 0 failed / 24 skipped`
    - `git diff --check`: PASS
  - 下一刀应继续盘点 verify-result mirrors 的 residual runtime/core uses，准备后续 compiler-deprecated 收口，而不是重开这条高可见 owner-path lane
- [completed] `ISSLCertificateVerification` peer-chain issuer-link truth 已进入统一 backend contract：
  - 新增计划：`docs/plans/2026-05-19-certificate-verification-chain-issuer-link-contract.md`
  - `tests/contract/test_backend_contract.pas`
    - `Contract 21` 现在额外锁住：
      - optional/core peer-chain entry 的 `GetIssuerCertificate()` nil/non-nil truth
      - issuer-link 存在时的 issuer cert public identity truth
  - 这次统一 contract 补强后的验证结果仍保持 green：
    - `tests/contract/test_backend_contract.pas`: `135 total / 111 passed / 0 failed / 24 skipped`
    - `git diff --check`: PASS
  - 这说明前面已经修好的 cross-backend issuer-link completeness 现在不再只靠 focused tests 保着，也已经进入 repo-level backend consistency truth
  - 下一刀不应再重开 peer-cert / issuer-link completeness lane，而应回到更大的 verification / optional-surface completeness 审查
- [completed] generic examples / 通用测试示例的 `ISSLCertificateVerification` owner path 已收口：
  - 新增计划：`docs/plans/2026-05-19-isslcertificateverification-generic-examples-owner-path.md`
  - 新增 source contract：`tests/scripts/test_isslcertificateverification_generic_examples_contract.sh`
  - `examples/fafafa.examples.tcp.pas`
    - 新增 `GetCertificateVerificationInfo(...)` 共享 helper
    - 优先走 `ISSLCertificateVerification`，仅在接口不可用时回退 core getters
  - 已切换的 generic examples / tests：
    - `examples/01_tls_client.pas`
    - `examples/example_https_api.pas`
    - `examples/production/https_client_auth.pas`
    - `examples/validation/real_world_test.pas`
    - `tests/examples/test_openssl.pas`
    - `tests/examples/test_real_websites.pas`
    - `tests/examples/test_real_websites_enhanced.pas`
    - `tests/examples/test_real_websites_comprehensive.pas`
    - `tests/connection/test_ssl_client_connection.pas`
  - 这批 target compile 过程中还顺手压出并修掉了两条真实 compile-liveness 问题：
    - `test_real_websites*` 三个程序原本仍是 FPC 不接受的 `try..except..finally` 结构
    - `test_ssl_client_connection.pas` 仍按旧 socket/native-handle API 书写
  - focused verification 已通过：
    - `bash tests/scripts/test_isslcertificateverification_generic_examples_contract.sh`
    - 9 个目标程序 compile 全绿
    - `git diff --check`: PASS
  - 当前结论：
    - generic examples/tests 这条 verify-result guidance lane 现在可以视为关闭
    - 下一刀更适合继续盘点 backend-specific runtime / residual deprecation lane，而不是再回头清 generic examples
- [completed] `ISSLCertificateVerification` residual direct-core surface 已冻结成 allowlist：
  - 新增计划：`docs/plans/2026-05-19-isslcertificateverification-residual-classification-freeze.md`
  - 新增 source contract：`tests/scripts/test_isslcertificateverification_residual_classification_contract.sh`
  - `src/fafafa.ssl.base.pas`
    - `GetVerifyResult` / `GetVerifyResultString` 注释现已明确：
      - preferred-access 是 `ISSLCertificateVerification`
      - `ISSLConnection` core getter 仅为 v1.x compatibility mirror
  - `src/fafafa.ssl.connection.base.pas`
    - 现已写明 shared mirror implementation 的 residual surface truth
  - 当前 allowlist 已锁住：
    - active docs direct-core file set = `0`
    - `examples/` direct-core file set = `examples/fafafa.examples.tcp.pas`
    - `tests/examples/` direct-core file set = `0`
    - `tests/connection/` direct-core file set = `tests/connection/test_ssl_client_connection.pas`
    - `tests/contract/` direct-core file set = `tests/contract/test_backend_contract.pas`
    - backend-specific runtime / contract residual file set = 当前 23 条剩余 proof 文件
  - focused verification 已通过：
    - `bash -n tests/scripts/test_isslcertificateverification_residual_classification_contract.sh`
    - `bash tests/scripts/test_isslcertificateverification_residual_classification_contract.sh`
    - `git diff --check`: PASS
  - 当前结论：
    - `ISSLCertificateVerification` 这条 ordinary guidance / generic examples / residual freeze 路线当前已完整收口
    - 下一刀更适合继续 backend-specific runtime / residual deprecation lane，而不是再回头做 residual archaeology
- [completed] `ISSLCertificateVerification` WinSSL runtime residual trio 已冻结成 intentional proof：
  - 新增计划：`docs/plans/2026-05-19-isslcertificateverification-winssl-runtime-residual-freeze.md`
  - 新增 focused source contract：`tests/scripts/test_isslcertificateverification_winssl_runtime_residual_contract.sh`
  - `tests/winssl/test_winssl_error_mapping_online.pas`
  - `tests/winssl/test_winssl_hostname_mismatch_online.pas`
  - `tests/winssl/test_winssl_revocation_online.pas`
    - direct core `GetVerifyResult` / `GetVerifyResultString` 已显式标记为 `INTENTIONAL_VERIFY_RESULT_CORE_SURFACE`
    - 当前用途被固定为 WinSSL-specific online certificate-error proof
    - `ISSLCertificateVerification` owner-path coverage 已明确由 generic/contract guidance tests 在别处守住
  - 当前 WinSSL direct-core verify-result file set 已锁住为这 3 个文件，未再扩张到其他 `tests/winssl/*`
  - focused verification 已通过：
    - `bash -n tests/scripts/test_isslcertificateverification_winssl_runtime_residual_contract.sh`
    - `bash tests/scripts/test_isslcertificateverification_winssl_runtime_residual_contract.sh`
    - `bash tests/scripts/test_isslcertificateverification_residual_classification_contract.sh`
    - `git diff --check`: PASS
  - 当前结论：
    - WinSSL verify-result runtime residual trio 现在可以视为关闭，不应再作为 generic guidance 漂移反复拉起
    - 下一刀更适合继续 `MbedTLS` residual cluster，而不是重扫 WinSSL trio
- [completed] `ISSLCertificateVerification` MbedTLS residual cluster 已冻结成 backend-specific proof：
  - 新增计划：`docs/plans/2026-05-19-isslcertificateverification-mbedtls-residual-cluster-freeze.md`
  - 新增 focused source contract：`tests/scripts/test_isslcertificateverification_mbedtls_residual_contract.sh`
  - 当前 direct-core verify-result residual file set 已锁住为：
    - `tests/mbedtls/benchmark_handshake_simple.pas`
    - `tests/mbedtls/test_mbedtls_safe.pas`
    - `tests/mbedtls/test_mbedtls_simple_connection.pas`
    - `tests/mbedtls/test_mbedtls_lowlevel.pas`
    - `tests/mbedtls/test_mbedtls_cert_chain.pas`
    - `tests/mbedtls/test_mbedtls_cert_errors.pas`
    - `tests/mbedtls/test_mbedtls_cert_verify_flags.pas`
    - `tests/test_mbedtls_framework.pas`
  - 上述文件现在都已显式标记为 `INTENTIONAL_VERIFY_RESULT_CORE_SURFACE`
  - 当前用途被固定为 MbedTLS-specific benchmark / runtime diagnostics / framework contract proof
  - `ISSLCertificateVerification` owner-path guidance 已明确由 generic/contract 路径在别处守住
  - focused verification 已通过：
    - `bash -n tests/scripts/test_isslcertificateverification_mbedtls_residual_contract.sh`
    - `bash tests/scripts/test_isslcertificateverification_mbedtls_residual_contract.sh`
    - `bash tests/scripts/test_isslcertificateverification_residual_classification_contract.sh`
    - `git diff --check`: PASS
  - 当前结论：
    - MbedTLS verify-result residual cluster 现在可以视为关闭，不应再作为 generic guidance 漂移反复拉起
    - 下一刀更适合继续 root-test / OpenSSL / WolfSSL 剩余 residual subgroup
- [completed] `ISSLCertificateVerification` OpenSSL/WolfSSL OCSP runtime duo 已冻结成 diagnostics proof：
  - 新增计划：`docs/plans/2026-05-19-isslcertificateverification-ocsp-runtime-duo-freeze.md`
  - 新增 focused source contract：`tests/scripts/test_isslcertificateverification_ocsp_runtime_duo_contract.sh`
  - 当前 direct-core verify-result residual duo 已锁住为：
    - `tests/openssl/test_openssl_server_ocsp_stapling_runtime.pas`
    - `tests/wolfssl/test_wolfssl_server_ocsp_stapling_runtime.pas`
  - 两个文件现在都已显式标记为 `INTENTIONAL_VERIFY_RESULT_CORE_SURFACE`
  - 当前用途被固定为 backend-specific server-side OCSP stapling runtime diagnostics
  - `ISSLCertificateVerification` owner-path guidance 已明确由 generic/contract 路径在别处守住
  - focused verification 已通过：
    - `bash -n tests/scripts/test_isslcertificateverification_ocsp_runtime_duo_contract.sh`
    - `bash tests/scripts/test_isslcertificateverification_ocsp_runtime_duo_contract.sh`
    - `bash tests/scripts/test_isslcertificateverification_residual_classification_contract.sh`
    - `git diff --check`: PASS
  - 当前结论：
    - OpenSSL/WolfSSL server-side OCSP runtime duo 现在可以视为关闭，不应再作为 generic guidance 漂移反复拉起
    - 下一刀更适合继续 root-test residual subgroup
- [completed] `ISSLCertificateVerification` root-test residual subgroup 已冻结成 runtime / backend-contract proof：
  - 新增计划：`docs/plans/2026-05-19-isslcertificateverification-root-test-residual-freeze.md`
  - 新增 focused source contract：`tests/scripts/test_isslcertificateverification_root_test_residual_contract.sh`
  - 当前 direct-core verify-result residual file set 已锁住为：
    - `tests/test_freepascal_backend_basic.pas`
    - `tests/test_freepascal_client_cert_verify_flags_runtime.pas`
    - `tests/test_freepascal_client_certificate_flight_requirements.pas`
    - `tests/test_freepascal_client_chain_trust_runtime.pas`
    - `tests/test_freepascal_client_ct_sct_surface.pas`
    - `tests/test_freepascal_client_ocsp_stapling_runtime.pas`
    - `tests/test_freepascal_client_online_ocsp_runtime.pas`
    - `tests/test_freepascal_server_accept_skeleton.pas`
    - `tests/test_mbedtls_framework.pas`
    - `tests/test_openssl_connection_verify_result_contract.pas`
    - `tests/test_wolfssl_framework.pas`
  - 上述文件现在都已显式标记为 `INTENTIONAL_VERIFY_RESULT_CORE_SURFACE`
  - 当前用途被固定为 FreePascal runtime contracts + backend framework / verify-result contracts
  - `ISSLCertificateVerification` owner-path guidance 已明确由 generic/contract 路径在别处守住
  - focused verification 已通过：
    - `bash -n tests/scripts/test_isslcertificateverification_root_test_residual_contract.sh`
    - `bash tests/scripts/test_isslcertificateverification_root_test_residual_contract.sh`
    - `bash tests/scripts/test_isslcertificateverification_residual_classification_contract.sh`
    - `git diff --check`: PASS
  - 当前结论：
    - root-test verify-result residual subgroup 现在可以视为关闭，不应再作为 generic guidance 漂移反复拉起
    - `ISSLCertificateVerification` 当前 residual 面已经基本全部完成 backend-specific / contract-specific 分类冻结
- [completed] GitHub Actions Windows runner 已重新纳入当前 truth surface：
  - `wave-b-b2-manual.yml` 的 live run `26030261335` 已证实 `windows-gate` 三层都能在 GitHub CI 上实际执行
  - 当前 WinSSL lane 不再允许退回“本地没 Windows，只能静态审查”的旧入口
- [completed] Windows runtime evidence strengthening 批次已落地：
  - broader suite 不再只依赖 `Start-Transcript` 壳，而是改为 UTF-8 console capture
  - `tests/run_winssl_tests.ps1` 现在会输出稳定的 `[WINSSL-RUNTIME]` markers
  - `check_wave_b_b2_evidence_consistency.sh` / `prepare_wave_b_b2_handoff_bundle.sh` 不再把 marker-less runtime log 当成合格 evidence
  - `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md` / `tests/windows/VALIDATION_BUNDLE.md` 已同步更新到新的 runtime-evidence 口径
- [completed] `wave-b-b2-manual.yml` live rerun `26031191987` 已验证新的 Windows artifact 证据链：
  - `winssl_runtime_suite_wave_b_b2_20260518_193941_evidence_fix.log` 已直接包含 `[WINSSL-RUNTIME] suite_start / suite_summary / suite_end`
  - `wave_b_b2_evidence_consistency_wave_b_b2_20260518_193941_evidence_fix.md` 已把 `windows_runtime_transcript` 记成 `substantive runtime evidence; suite_end_status=PASS`
  - 当前 Wave B/B2 manual lane 的 Windows runtime evidence gap 已从“artifact 空壳”切换成“substantive proof available”
- [completed] WinSSL / MbedTLS `IsSessionReused` semantic false positive 已完成 focused 收口：
  - 新增 `docs/plans/2026-05-18-session-reused-semantic-truth-audit.md`
  - 新增 `tests/scripts/test_session_reused_semantic_truth_contract.sh`
  - 新增 `tests/test_mbedtls_connection_session_reused_contract.pas`
  - `src/fafafa.ssl.winssl.connection.pas` / `src/fafafa.ssl.mbedtls.connection.pas` 不再把 `SetSession(...)` 直接等价成“当前握手已复用”
  - 当前真相已重新对齐到：`SetSession` 只配置待恢复 session；`IsSessionReused` 只报告 post-handshake 实际结果
- [completed] WinSSL session-resumption runtime proof bridge 已完成本轮 truth-extraction 收口：
  - 新增 `docs/plans/2026-05-18-winssl-session-runtime-proof-bridge.md`
  - canonical `src/fafafa.ssl.winssl.connection.pas` 当前已把 shared `SECPKG_ATTR_SESSION_INFO` probe 撤下，避免 shared handshake path 再次被打崩
  - `TryGetCurrentSessionInfo(...)` 仍保留为后续 dedicated Windows proof lane 的实验入口
  - client `DoConnect(...)` 成功后也会保存 session metadata，不再只有 server path 落 `SaveSessionAfterHandshake`
  - `tests/run_winssl_tests.ps1` 现在已接入 `test_winssl_session_resumption.lpi`
  - broader suite 会把 `[WINSSL-SESSION-RESUME]` 原始观测行提升成 `[WINSSL-RUNTIME] session_resumption ...` evidence markers
  - focused source contracts + Win64 cross-target compile 已通过
  - GitHub Actions live run `26033545656` 已先暴露出一个 workflow-entry 漂移，而不是 runtime 语义失败：
    - `test_winssl_session_resumption.lpi` 仍硬编码 `TargetOS=linux`
    - Windows `Run broader WinSSL runtime suite` 因此把这条 dedicated lane 当成 Linux 项目编译，卡在 compile phase
  - GitHub Actions live rerun `26034303732` 已证明这条 `.lpi` 漂移修复有效：
    - `Run broader WinSSL runtime suite` 的 compile phase 已全部通过
    - 新的 first hard blocker 已收敛到 shared runtime helper `UpdateSessionReuseTruthFromContext(...)`
    - `WinSSL Integration Tests (Multi-Scenario)` / `Backend Comparison Tests` / `WinSSL Session Resumption Truth` / `WinSSL Performance Benchmark` / `WinSSL HTTPS Client` 都在握手后观测 session info 时触发同类 `EAccessViolation`
  - GitHub Actions live rerun `26034948820` 已把这个 Windows crash 进一步压缩到更窄的 shared path：
    - `linux-gate` / `macos-gate` 持续 green，compile phase 继续全部通过
    - `windows-gate` 仍只失败在 `Run broader WinSSL runtime suite`
    - crash 顶点已收敛到 canonical `src/fafafa.ssl.winssl.connection.pas` 里的 `SessionIdBytesToHex(LSessionInfo)` 读取
    - 当前 Windows runner 上可继续相信 `dwFlags and SSL_SESSION_RECONNECT`，但 raw session-id byte buffer 不能再放进共享握手后路径
  - GitHub Actions live rerun `26035941452` 继续把这个问题往真实根因压缩：
    - `windows-gate` 这次已经稳定通过 `Run quick WinSSL smoke` 与 `Run Windows Wave B gate`
    - broader suite compile phase 继续全部通过，旧的 `SessionIdBytesToHex(...)` 崩点也不再出现
    - 但 `Run broader WinSSL runtime suite` 仍在 canonical `UpdateSessionReuseTruthFromContext(...)` 的 line `850` 触发 `EAccessViolation`
    - 这说明当前不只是 raw session-id bytes 不稳，而是整条 `SECPKG_ATTR_SESSION_INFO` shared probe 仍不适合放在 canonical 握手后路径
    - 同一次 rerun 中 `macos-gate` 失败已确认回到了独立的 `run_all_module_tests.sh` lane，不是 WinSSL session-resumption 当前这批的直接回归
  - 当前这批的最小收口是：
    - 保留 `.lpi` target 修复与 project-target guard，不再回头重开旧问题
    - 把 canonical shared path 上的 `SECPKG_ATTR_SESSION_INFO` probe 整体撤下
    - 当前共享真相先回到 `reused=false` + existing fallback session-id generators
    - 仅把 `TryGetCurrentSessionInfo(...)` 保留成后续 dedicated Windows runtime proof lane 的实验入口，而不是继续放在共享握手后路径
  - GitHub Actions live rerun `26037518301` 已完成这条 bridge lane 的最终验收：
    - `linux-gate` / `macos-gate` / `windows-gate` / `summary` 全部 success
    - Windows broader suite `suite_summary passed=7 failed=0 total=7 success_rate=100`
    - `WinSSL Session Resumption Truth` lane 当前真实 runtime 结论已固定为：
      - `host=www.cloudflare.com`
      - `attempts=4`
      - `observed_reuse=false`
      - `require_reuse=false`
      - `session_configured=true`
    - 这说明当前 bridge 已经把“会不会 crash / 会不会误报”这个问题关掉了
    - 当前剩下的不再是 workflow 或 shared-path 安全性，而是“WinSSL backend 是否要继续实现真正的 native resumed handshake”
- [completed] WinSSL native probe quarantine 已完成并得到新的 Windows artifact 证实：
  - 新增计划：`docs/plans/2026-05-18-winssl-native-probe-evidence-lane.md`
  - `tests/winssl/test_winssl_session_resumption.pas` 现在会把 public truth 与 native Schannel observation 分开输出
  - 第一轮已新增：
    - `native_probe label=... available=... reused=...`
    - `native_observed_reuse=...`
    - `native_probe_succeeded=...`
    - `require_native_reuse=...`
  - focused source contract / Win64 cross-target compile / `git diff --check` 已通过
  - 但 GitHub Windows live run `26042437486` 已给出新的更窄真相：
    - `WinSSL Session Resumption Truth` 在首个 public signal 后、第一条 `native_probe` marker 前就以 `exit_code=-1073741819` 退出
    - 这说明当前 public-handle probe 方式在 broader suite 默认开启时并不安全
  - 当前最小安全修法已明确：
    - broader suite 默认 lane 先把 native probe 维持为 `opt-in`
    - 默认记录 `reason=disabled_by_default`
    - 不再回头重开 shared probe / client reconnect truth / capability/docs truth 旧 lane
  - 本地 follow-up 已实现并通过：
    - `FAFAFA_WINSSL_ENABLE_NATIVE_PROBE` 显式 opt-in
    - summary 追加 `native_probe_enabled=...`
    - focused contract / Win64 compile / `git diff --check` 重新转绿
  - GitHub Windows live rerun `26043523820` 已证实 quarantine 生效：
    - `WinSSL Session Resumption Truth` lane 已恢复 PASS
    - runtime artifact 真实写出：
      - `native_probe ... reason=disabled_by_default`
      - `summary ... native_probe_enabled=false native_observed_reuse=false native_probe_succeeded=false`
  - 这条 lane 当前已完成：
    - broader suite 默认 lane 不再被 risky native probe 打崩
    - native probe 明确降格成 opt-in experimental evidence
- [completed] Windows broader suite 的 `integration_multi` 外部 HTTP 状态断言误报已完成收口：
  - 新增计划：`docs/plans/2026-05-18-winssl-integration-multi-http-status-stability.md`
  - GitHub Windows live run `26043523820` 已证明：
    - `api.github.com` 的 TCP/TLS/send/receive/status-line 都 PASS
    - 只有“响应状态码正常 (2xx/3xx)”断言失败
  - 当前最小正确修法已落地并得到新的 live rerun 证实：
    - 状态码改成 `可解析 + 非 5xx`
    - focused contract / Win64 compile / `git diff --check` 已通过
    - GitHub Actions live run `26044471873` 已确认：
      - `windows-gate` PASS
      - broader WinSSL runtime suite 不再因为 `integration_multi` 的 `2xx/3xx` 断言失败而红
  - 这条 lane 当前已完成：
    - Windows broader suite 已恢复 green
    - 当前 repo-level cross-platform failure 已不在 WinSSL Windows 路线
- [completed] macOS OpenSSL loader 的 `OPENSSL_ROOT` 优先级修复已完成实验收口，但已被 live rerun 排除为最终根因：
  - 新增计划：`docs/plans/2026-05-18-macos-openssl-root-loader-priority.md`
  - 本地 focused contract / loader Pascal contracts 均已通过
  - 但新的 live macOS rerun 失败面没有收窄：
    - `Store/TS/CT` 继续 PASS
    - `PEM/EVP/PKCS12/CMS/OCSP` 仍成片失败
  - 当前这条线应保留为“已做过且有价值的 loader hardening”，而不是继续被当成主根因反复拉起
- [completed] macOS loader/symbol probe evidence lane 已完成 live truth 收口，不再是当前 blocker：
  - 新增计划：`docs/plans/2026-05-18-macos-openssl-loader-symbol-probe.md`
  - 当前静态真相已经压清：
    - `TS/CT/Store` 主要走 direct `GetCryptoProcAddress(...)`
    - `EVP/PEM/PKCS12/CMS/OCSP` 主要走 `LoadFunctions(...)` / batch-binding
  - 现有 `wave_b_macos_gate_probe_*.json` 只覆盖环境，不覆盖 loader/symbol 真相
  - 当前批次已落地：
    - `tests/diagnostic/test_macos_openssl_loader_symbol_probe.pas`
    - `scripts/run_macos_openssl_loader_symbol_probe.sh`
    - `scripts/run_wave_b_macos_gate.sh` 新增 `loader-symbol-probe` step
    - `.github/workflows/wave-b-b2-manual.yml` active + disabled template 现会上传新的 probe JSON
    - focused workflow/gate contracts 已通过
    - commit `07e526b` (`ci/macos: add openssl loader symbol probe`) 已推送到 `origin/master`
    - live workflow run `26048015976` 已完成 `success`
    - `wave_b_macos_loader_symbol_probe_wave_b_b2_20260518_macos_loader_symbol_probe_07e526b.json` 已证明：
      - `loader_version_string = OpenSSL 3.6.2 7 Apr 2026`
      - direct symbols 全部为 `true`
      - `evp/pem/pkcs12/cms/ocsp/ts/ct/store` module truth 全部为 `true`
    - 同一 run 的 `linux-gate` / `windows-gate` / `summary` 也全部 `success`
  - 当前结论：
    - 不要再把 macOS loader/path、symbol export、batch-binding 漂移当成当前主线 blocker 重复拉起
- [in_progress] 当前 repo-level 下一步应回到更高价值的 completeness 路线：
  - 继续审查各 backend implementation completeness / optional surface completeness
  - 不再凭环境探测或请求名字符串重开 `OPENSSL_ROOT` / macOS loader 怀疑
  - 若继续深挖 WinSSL，则优先扩展真实 resumed handshake / session tickets / certstore / OCSP / enterprise 等高风险 lane，而不是再重复治理 runtime capture、shared probe crash 或已修掉的 semantic false positive
- [completed] `MbedTLS/WolfSSL` c-library session metadata 与 peer-certificate completeness 已完成 focused 收口：
  - 新增计划：`docs/plans/2026-05-19-clibrary-session-metadata-peer-cert-completeness.md`
  - `src/fafafa.ssl.mbedtls.session.pas`
    - `FromContext(...)` 现在会真实回填 protocol / cipher
    - 对 `mbedtls_ssl_get_peer_cert(...)` 返回的 borrowed cert 走 `DER copy -> owned reload`
    - helper 不足时继续 `fail-closed`
  - `src/fafafa.ssl.wolfssl.certificate.pas`
    - `SaveToDER()` 现在可直接从 native `WOLFSSL_X509` 导出 DER
  - `src/fafafa.ssl.wolfssl.session.pas`
    - `FromConnection(...)` 现在会 materialize peer cert，并在 clone 后保留这条 truth
  - `src/fafafa.ssl.mbedtls.certificate.pas`
    - `Clone()` 不再只复制缓存字段；现在会重新 materialize native cert，避免 clone 成空壳
  - focused verification 已通过：
    - `tests/test_mbedtls_framework.pas`: `116 passed / 0 failed`
    - `tests/test_wolfssl_framework.pas`: `136 passed / 0 failed`
    - `tests/contract/test_backend_contract.pas`: `135 total / 111 passed / 0 failed / 24 skipped`
    - `git diff --check`: PASS
  - 当前结论：
    - session completeness 的主缺口已从 “version/cipher/peer cert 缺失” 收口
    - 下一刀更适合继续横向审其它 backend 的 session/certificate clone semantics，而不是再重开本批
- [completed] `MbedTLS` connection peer-certificate materialization 已完成 focused 收口：
  - 新增计划：`docs/plans/2026-05-19-mbedtls-connection-peer-cert-materialization.md`
  - 新增 focused contract：`tests/test_mbedtls_connection_peer_certificate_contract.pas`
  - `src/fafafa.ssl.mbedtls.connection.pas`
    - `GetPeerCertificate()` 不再直接返回 borrowed cert wrapper
    - `GetPeerCertificateChain()` 的单叶子入口也不再暴露 borrowed handle
    - 两条 surface 现在统一走 `TMbedTLSCertificate.Clone()` materialize owned copy
    - helper 不足时继续 fail-closed
  - focused verification 已通过：
    - `tests/test_mbedtls_connection_peer_certificate_contract.pas`: `8 passed / 0 failed`
    - `tests/test_mbedtls_framework.pas`: `116 passed / 0 failed`
    - `tests/contract/test_backend_contract.pas`: `135 total / 111 passed / 0 failed / 24 skipped`
    - `git diff --check`: PASS
  - 当前结论：
    - `MbedTLS` 连接态 public cert surface 已不再泄漏 backend-internal lifetime 约束
    - 下一刀更适合继续横向审 `WolfSSL` / `OpenSSL` / `MbedTLS` 其它 connection-level completeness seam，而不是再回头重开这条 borrowed-peer-cert 问题
- [completed] `WolfSSL` certificate clone materialization 已完成 focused 收口：
  - 新增计划：`docs/plans/2026-05-19-wolfssl-certificate-clone-materialization.md`
  - `src/fafafa.ssl.wolfssl.certificate.pas`
    - `Clone()` 不再只复制 `FPEMData/FDERData/FInfo`
    - loaded cert 现在统一走 `DER copy -> owned reload`
    - X509 materialization helper 缺失时改为 `fail-closed`
  - `tests/test_wolfssl_framework.pas`
    - 新增 `WolfSSL Certificate Clone Materialization Contract`
    - 锁住 native handle、subject/issuer、fingerprint 与 helper-loss truth
  - focused verification 已通过：
    - `tests/test_wolfssl_framework.pas`: `141 passed / 0 failed`
    - `tests/contract/test_backend_contract.pas`: `135 total / 111 passed / 0 failed / 24 skipped`
    - `git diff --check`: PASS
  - 当前结论：
    - `WolfSSL` loaded certificate 的 public clone surface 已不再退化成 metadata shell
    - 下一刀更适合继续横向审其它 backend 的 certificate clone / connection completeness seam，而不是再重开这条 clone 空壳问题
- [completed] `WolfSSL` connection peer-certificate materialization 已完成 focused 收口：
  - 新增计划：`docs/plans/2026-05-19-wolfssl-connection-peer-cert-materialization.md`
  - 新增 focused contract：`tests/test_wolfssl_connection_peer_certificate_contract.pas`
  - `src/fafafa.ssl.wolfssl.connection.pas`
    - `GetPeerCertificate()` 不再直接返回 `wolfSSL_get_peer_certificate(...)` 的 native wrapper
    - 当前改为 `native X509 -> DER export -> owned reload`
    - copy helper 不足时改为 fail-closed
  - focused verification 已通过：
    - `tests/test_wolfssl_connection_peer_certificate_contract.pas`: `4 passed / 0 failed`
    - `tests/test_wolfssl_framework.pas`: `141 passed / 0 failed`
    - `tests/contract/test_backend_contract.pas`: `135 total / 111 passed / 0 failed / 24 skipped`
    - `git diff --check`: PASS
  - 当前结论：
    - `WolfSSL` 连接态单证书 public surface 已与现有 chain/session materialization truth 对齐
    - 下一刀更适合继续横向审其它 backend 的 connection-level completeness seam，而不是再重开这条单证书 materialization 缺口
- [completed] `FreePascal` peer-certificate issuer link 已完成 focused 收口：
  - 新增计划：`docs/plans/2026-05-19-freepascal-peer-cert-issuer-link.md`
  - `src/fafafa.ssl.freepascal.connection.pas`
    - 构建 `FPeerCertificateChain` 后现在会显式接上相邻 issuer link
    - leaf cert 与 chain leaf 都不再丢失 `GetIssuerCertificate()` truth
  - `tests/test_freepascal_client_peer_certificate_surface.pas`
    - 新增 leaf/chain issuer-link truth 断言
  - focused verification 已通过：
    - `tests/test_freepascal_client_peer_certificate_surface.pas`: PASS
    - `tests/contract/test_backend_contract.pas`: `135 total / 111 passed / 0 failed / 24 skipped`
    - `git diff --check`: PASS
  - 当前结论：
    - `FreePascal` 连接态 peer cert surface 已不再出现“leaf/chain 都有了，但 issuer link 仍为空”的链真相缺口
    - 下一刀更适合横向审其它 backend 是否也存在同类 issuer-link completeness seam
- [completed] `OpenSSL` peer-certificate issuer link 已完成 focused 收口：
  - 新增计划：`docs/plans/2026-05-19-openssl-peer-cert-issuer-link.md`
  - 新增 focused contract：`tests/test_openssl_connection_peer_certificate_surface.pas`
  - `src/fafafa.ssl.openssl.connection.pas`
    - `GetPeerCertificate()` 现在会从 peer chain / verified chain 尝试 materialize issuer link
    - `GetPeerCertificateChain()` 现在会为返回的 chain entries 补 issuer link
    - 现有 safe-degrade 边界保持不变
  - focused verification 已通过：
    - `tests/test_openssl_connection_peer_certificate_surface.pas`: PASS
    - `tests/test_openssl_connection_peer_certificate_contract.pas`: `2 passed / 0 failed`
    - `tests/test_openssl_connection_peer_certificate_chain_contract.pas`: `8 passed / 0 failed`
    - `tests/contract/test_backend_contract.pas`: `135 total / 111 passed / 0 failed / 24 skipped`
    - `git diff --check`: PASS
  - 当前结论：
    - `OpenSSL` 连接态 peer cert surface 已不再出现“leaf/chain 已有，但 issuer link 仍为空”的链真相缺口
    - 下一刀更适合继续横向审剩余 backend 的 issuer-link completeness seam，而不是重开这条 OpenSSL surface
- [completed] `WolfSSL` peer-certificate issuer link 已完成 focused 收口：
  - 新增计划：`docs/plans/2026-05-19-wolfssl-peer-cert-issuer-link.md`
  - 更新 focused surface：`tests/connection/test_wolfssl_client_peer_certificate_surface.pas`
  - `src/fafafa.ssl.wolfssl.connection.pas`
    - `GetPeerCertificate()` 现在会在可用时从 peer chain 补 issuer link
    - `GetPeerCertificateChain()` 现在会为返回的 chain entries 补 issuer link
    - 现有 materialization / safe-degrade 边界保持不变
  - focused verification 已通过：
    - `tests/connection/test_wolfssl_client_peer_certificate_surface.pas`: PASS
    - `tests/test_wolfssl_connection_peer_certificate_contract.pas`: `4 passed / 0 failed`
    - `tests/test_wolfssl_framework.pas`: `141 passed / 0 failed`
    - `tests/contract/test_backend_contract.pas`: `135 total / 111 passed / 0 failed / 24 skipped`
    - `git diff --check`: PASS
  - 当前结论：
    - `WolfSSL` 连接态 peer cert surface 已不再出现“leaf/chain 已有，但 issuer link 仍为空”的链真相缺口
    - 下一刀更适合继续横向审剩余 backend 的 issuer-link completeness seam，而不是重开这条 WolfSSL surface
- [completed] generic session-cache persistence count truth 已完成 focused 修复并形成新基线：
  - 新增计划：`docs/plans/2026-05-19-session-cache-persistence-count-truth.md`
  - 新增 focused test：`tests/test_session_cache_persistence_contract.pas`
  - 修复：`src/fafafa.ssl.session.cache.pas`
    - `SaveToFile(...)` 不再把 `FCache.Count` 直接写进文件头
    - 现在会回填真实写入条目数，避免跳过 invalid/expired session 后把文件结构写坏
  - focused verification 已通过：
    - 新契约先 `RED` 后 `GREEN`
    - `git diff --check` 通过
  - 当前结论：
    - 这条缺口说明“后端实现完整性”之外，generic persistence seam 也需要持续审查
    - 但这次问题已收口，不再把 session-cache 持久化偶发损坏当成未定位噪声
- [completed] `v1.5.0` release / workflow / cross-platform runtime closeout 已经不再是当前主线：
  - 当前默认控制面应保持在 `post-release route selection`
  - 不再围绕 release lane 或旧的 Windows runtime blocker 重复开工
- [completed] 已存在一份较强的静态接口审查基线：
  - `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
  - 但它主要聚焦 public interface 设计，不等于“接口设计 + 各 backend 实现对齐”已被全面验证
- [in_progress] 当前批次已切换到新的 repo-level goal：
  - 先建立新的计划/记录入口
  - 再按“公共接口 -> facade/factory/builder/config -> capability matrix -> backend implementation truth -> focused fix”顺序推进
- [completed] 两份顶层 core test 也已完成非交互收口：
  - `tests/test_exceptions.pas`
  - `tests/test_base_interface_contract.pas`
  - 新增 `tests/scripts/test_top_level_core_tests_noninteractive_contract.sh`
  - 新增 `docs/plans/2026-05-18-noninteractive-top-level-core-tests.md`
  - 当前这两份测试已不再输出“按回车键退出...”或依赖 `ReadLn`
  - repo-wide `ReadLn` 扫描表明剩余命中主要位于 examples / diagnostic / benchmark / WinSSL 专项程序，不属于这批顶层 core automation 收口范围
- [completed] WinSSL 活跃测试程序也已完成非交互收口：
  - `tests/unit/test_winssl_comprehensive.pas`
  - `tests/winssl/test_winssl_context_comprehensive.pas`
  - `tests/winssl/test_winssl_errors_comprehensive.pas`
  - `tests/winssl/test_winssl_monitoring.pas`
  - `tests/winssl/test_winssl_connection_edge_cases.pas`
  - `tests/winssl/test_winssl_certstore.pas`
  - `tests/winssl/test_winssl_session_management.pas`
  - `tests/winssl/test_winssl_library_basic.pas`
  - `tests/winssl/test_winssl_certificate_loading.pas`
  - 新增 `tests/scripts/test_winssl_active_tests_noninteractive_contract.sh`
  - 新增 `docs/plans/2026-05-18-noninteractive-winssl-active-tests.md`
  - `run_winssl_tests.ps1` 的 non-interactive 意图已经与源码重新对齐
  - 剩余 `ReadLn` 命中已主要收缩到 examples / diagnostics / benchmark，而不再是活跃 core/WinSSL 测试主面
- [completed] backend optional public surface 的 focused completion-audit revalidation 已补齐：
  - `tests/contract/test_backend_contract.pas` 当前已实际覆盖：
    - Contract 12: context optional interface alignment
    - Contract 13: context native-handle interface alignment
    - Contract 14: context HTTP hooks interface alignment
    - Contract 15: session native-handle interface alignment
    - Contract 17: certificate-store native-handle interface alignment
    - Contract 18: diagnostics interface alignment
  - 新增 `docs/plans/2026-05-18-backend-optional-surface-completion-audit-revalidation.md`
  - 6 份旧 plan 中原本缺失的 execution result 现已补成 focused revalidation result
  - focused 合同当前结果：
    - `Total Tests: 135`
    - `Passed: 111`
    - `Failed: 0`
    - `Skipped: 24`
  - OpenSSL / WolfSSL / MbedTLS / FreePascal 的上述 optional surface 当前都已有 live contract 证据
  - WinSSL 继续按当前 Linux 主机的既有平台边界保持 skip truth，不误写成已本机证实
- [completed] 第一轮接口/后端真相交叉验证已经完成：
  - 已确认 `ISSLServerConnection` 只存在于活跃文档承诺，不存在于 public source
  - 已确认 context-level `ServerName` 仍由 factory / builder / connection constructors / tests 一起固化
  - 已确认 `BufferSize` / `HandshakeTimeout` 是显式拒绝的 connection-scoped config，不是 silent no-op
  - 已确认 capability dual-truth 仍是系统性结构，不是单 backend 漏洞
- [completed] 当前批次已落一条边界清晰的最小修复：
  - 修正文档中不存在的 `ISSLServerConnection` 承诺
  - 新增 `tests/scripts/test_interface_docs_no_nonexistent_isserverconnection_contract.sh`
- [completed] 第二条边界清晰的 capability 真相修复已经落地：
  - 在 `src/fafafa.ssl.base.pas` 新增 `NormalizeLegacyCapabilityBooleans(...)`
  - OpenSSL / FreePascal / WinSSL / MbedTLS / WolfSSL 的 `GetCapabilities` 统一在返回前用 `*Support` 字段回填 legacy boolean 兼容视图
  - capability focused contracts 已切到 “runtime truth 以 support-level 为准，legacy boolean 只是 compatibility projection”
- [completed] serializer / deserializer / diff 线上的两处具体真 bug 已完成收口：
  - 反序列化现在在检测到 v1.2 `*Support` 字段时，会用 support-level truth 覆盖冲突的 legacy boolean
  - capability diff 不再忽略 `SNISupport` / `ALPNSupport` / `OCSPStaplingSupport` / `CertTransparencySupport` / `SessionTicketsSupport` 以及 support-only 的 v1.2 字段
  - 新增 focused regression 证明红灯已转绿，且旧 round-trip 兼容仍保持
- [completed] `context-level ServerName` 内部 warning quarantine 已按 live 证据收窄：
  - `tests/contract/test_capabilities_contract.pas` 已固定为当前 deprecated warning compile probe
  - `wolfssl` / `mbedtls` / `winssl` 的兼容 fallback 读取点已加局部 warning quarantine
  - 没有改动 factory / builder / runtime compatibility 语义
- [completed] serializer 输出面的 truth projection 已对齐到 v1.2 support-level 真相：
  - 新增 `tests/test_capability_serialization_truth_projection.pas`，直接检查 JSON/XML 输出字符串
  - serializer 现在会在 record 已携带 support-level truth 时，先回填 legacy boolean 再输出
  - 既有 JSON/XML round-trip 兼容保持绿色
- [completed] legacy-only capability round-trip truth 也已补上 focused 收口：
  - 新增 `docs/plans/2026-05-20-capability-support-level-serialization-precedence.md`
  - 新增 `tests/test_capability_serialization_support_level_truth.pas`
  - serializer 不再对 pure legacy-only record 凭空生成 `sniSupport` / `ocspStaplingSupport` / `sessionTicketsSupport` 这类 `none` truth
  - JSON/XML round-trip 现在同时固定两条规则：
    - support-level-aware record 继续显式输出 `*Support` 并以其为真相
    - legacy-only record 保留旧 boolean truth，不再被 synthetic `*Support="none"` 反向抹掉
  - 当前 live backend `GetCapabilities` producer 已统一发布完整 support-level 视图，所以这批不会削弱真实 runtime/export surface；手工混合 record 的彻底无歧义语义仍需未来 presence bits
- [completed] `managed result init safety`
  当前 focused 目标：
  - 收掉 public facade / shared connection base 中带 `string` / 动态数组结果 record 的不安全初始化写法
  - 不改对外行为，只把 shared helper / owner-surface mirror 的初始化收回类型安全路径
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-managed-result-init-safety.md`
  - 新增 focused contract：
    - `tests/scripts/test_managed_result_init_safety_contract.sh`
  - 收口源码：
    - `src/fafafa.ssl.pas`
    - `src/fafafa.ssl.connection.base.pas`
  当前最终收口证据：
  - `CreateDefaultConfig(...)`
    fallback 现在改用
    `Default(TSSLConfig)`
    不再对 managed `TSSLConfig`
    做
    `FillChar(...)`
  - `TBaseSSLConnection`
    当前已改用：
    - `Default(TSSLConnectionInfo)`
    - `Default(TSSLDiagnosticInfo)`
    - `Result := nil`
      作为空 `TBytes` 返回
  - focused compile proof 已显示：
    - 之前的
      `connection.base`
      4 条 managed-result warning
      已消失
    - 当前 grep 输出只剩仓库其它旧 warning
      与测试文件自身 warning
  focused verification 已通过：
  - `bash -n tests/scripts/test_managed_result_init_safety_contract.sh`
  - `bash tests/scripts/test_managed_result_init_safety_contract.sh`
  - `fpc -B ... tests/config/test_default_config.pas`
  - `./tmp/defaultcfg_bin/test_default_config`
  - `fpc -B ... tests/test_connection_builder_hostname_precedence.pas`
  - `./tmp/conninfo_bin/test_connection_builder_hostname_precedence`
  - `git diff --check`
  当前结论：
  - 这批修的是 shared public helper / base-class 实现残口，
    不是抽象层面的“接口看起来不优雅”
  - 同类 managed-result warning 在仓库其它单元仍有存量，
    但这条高可见 public surface 现已先收干净
- [completed] `managed result init safety wave2`
  当前 focused 目标：
  - 把同类 managed `TBytes` result 初始化 warning
    从 public facade / connection base
    继续推进到 shared TLS13 / FreePascal session 实现层
  当前 batch 范围：
  - 新增计划：
    - `docs/plans/2026-05-20-managed-result-init-safety-wave2.md`
  - 新增 focused contract：
    - `tests/scripts/test_managed_result_init_safety_wave2_contract.sh`
  - 收口源码：
    - `src/fafafa.ssl.tls13.wire.pas`
    - `src/fafafa.ssl.freepascal.session.pas`
  当前最终收口证据：
  - `BuildTLSPlaintext(...)`
    当前会先
    `Result := nil`
    再
    `SetLength(...)`
  - `ReadVector16(...)`
    当前会先
    `Result := nil`
    再
    `SetLength(...)`
  - `TFreePascalSession.Serialize(...)`
    当前以
    `Result := nil`
    初始化空 payload
  - focused compile grep 已证明：
    - `tls13.wire`
      本身不再出现先前那条 managed-result warning
    - `freepascal.session`
      本身也不再出现先前两条 managed-result warning
    - grep 输出只剩其它 TLS13 / session 相关单元的旧 warning
  focused verification 已通过：
  - `bash -n tests/scripts/test_managed_result_init_safety_wave2_contract.sh`
  - `bash tests/scripts/test_managed_result_init_safety_wave2_contract.sh`
  - `fpc -B ... tests/test_tls13_foundation.pas`
  - `./tmp/tls13_foundation_bin/test_tls13_foundation`
  - `fpc -B ... tests/test_freepascal_client_session_resumption.pas`
  - `./tmp/fp_session_bin/test_freepascal_client_session_resumption`
  - `git diff --check`
  当前结论：
  - 这批继续收掉的是 shared implementation 的真实 warning 残口，
    不是去做“代码洁癖式”的机械替换
  - TLS13 wire / FreePascal session 这两条高复用实现面
    现在也已经从这类 managed-result 初始化坑里脱身
- [completed] `context-level ServerName` 迁移路线图与兼容锁点地图已固化：
  - 新增 `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - intentional compatibility tests 已统一纳入 `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - 当前已明确下一批应优先做 builder surface narrowing，而不是直接硬删 backend fallback
- [completed] `context-level ServerName` Phase B 的第一刀 builder surface narrowing 已收口：
  - `TSSLContextBuilderImpl.ExportToJSON/INI` 在保留 `server_name` 兼容载荷时，会显式导出 `server_name_mode=deprecated_context_sni`
  - `ImportFromJSON/INI` 继续接受 legacy-only `server_name` 输入，并在回导出时自动补上兼容 marker
  - focused config regressions 证明这是 additive compatibility de-emphasis，不是 runtime 行为删改
- [completed] `context-level ServerName` Phase B 的第二刀 factory/config surface narrowing 已收口：
  - `TSSLFactory.CreateContext(AContextType, ALibType)` 与 `TSSLFactory.CreateContext(const AConfig)` 在 client-side 兼容写入 `TSSLConfig.ServerName` 时，都会发出显式 warning
  - warning 直接点名 `TSSLConfig.ServerName` 是 deprecated context-level SNI compatibility，并把调用方导向 `ISSLClientConnection.SetServerName(...)` / `TSSLConnector.Connect*(..., ServerName)`
  - `src/fafafa.ssl.base.pas` 与 `docs/reference/API_REFERENCE.md` 已把该字段降格成 compatibility-only 入口
  - focused factory regressions 证明当时这次收口没有直接改掉现有兼容写入行为；后续 FreePascal runtime cut 已让该 backend 的 client connection 不再继承
- [completed] `context-level ServerName` Phase C 的第一刀 shared compatibility shim 已收口：
  - 新增 `src/fafafa.ssl.context.compat.pas`
  - OpenSSL / FreePascal / WolfSSL / MbedTLS / WinSSL 的 constructor fallback 已统一改走 `GetContextLevelServerNameCompatibilityValue(...)`
  - direct deprecated `AContext.GetServerName` / `FContext.GetServerName` 读取已从五个 backend 本地构造路径移除
  - focused source contract 与当时的跨 backend fallback runtime regressions 均保持绿色；后续 FreePascal 已先行切到 no-inheritance
- [completed] `context-level ServerName` 的 builder runtime warning 已与 validation / factory 对齐：
  - `TSSLContextBuilderImpl.BuildClient` 会在应用 `WithSNI(...)` 兼容写入前发出显式 warning
  - `TSSLContextBuilderImpl.BuildServer` 会发出显式 warning；当前后续批次已进一步收口为 warning + ignore
  - `docs/reference/API_REFERENCE.md` 已把 `WithSNI(...)` 也降格成 compatibility-only 入口
  - focused builder warning regressions、validation regressions 与 runtime consistency regressions 均保持绿色
- [completed] 第一批明确属于普通 WinSSL 客户端连接流的测试已迁到 per-connection SNI：
  - `tests/winssl/test_winssl_error_mapping_online.pas`
  - `tests/winssl/test_winssl_https_client.pas`
  - `tests/winssl/test_winssl_revocation_online.pas`
  - `tests/winssl/test_winssl_mtls_e2e_local.pas`
  - 这些文件不再通过 context-level `SetServerName(...)` 教客户端连接流
  - focused source contract 绿灯，Win64 交叉编译也已通过
- [completed] 残余 `context-level SetServerName(...)` 模糊测试面已完成分类/收口：
  - `tests/test_tls_connector_early_data_contract.pas` 已显式标记为 `INTENTIONAL_COMPAT`
  - `tests/mbedtls/test_mbedtls_context_contract.pas`
  - `tests/wolfssl/test_wolfssl_context_contract.pas`
  - `tests/winssl/test_winssl_library_basic.pas`
  - `tests/winssl/test_winssl_mtls_skeleton.pas`
    已显式标记为 `INTENTIONAL_API_SURFACE`
  - `tests/winssl/test_winssl_mtls_skeleton.pas` 的真实握手路径已迁到 per-connection SNI
  - focused residual contract 绿灯，Linux-safe / Win64 focused 编译验证已通过
- [completed] 第一条真正的 behavior migration 已经以 server-side builder dead-compat cut 落地：
  - `TSSLContextBuilderImpl.BuildServer` 保留 `WithSNI(...)` compatibility warning，但不再把它写回 built context
  - `ValidateServer` / runtime warning / API note 已同步改成 `BuildServer ignores it and server-side connections ignore it`
  - focused RED -> GREEN：
    - `tests/test_context_builder_server_servername_runtime_consistency.pas`
    - `tests/test_context_builder_server_name_compatibility_warning.pas`
    - `tests/config/test_config_validation.pas`
- [completed] 第一条 client-side fallback behavior migration 已经以 `sslCtxBoth` ambiguity cut 落地：
  - shared compatibility shim 不再把 dual-role `sslCtxBoth` 的 deprecated context-level `ServerName` 继承进新连接
  - `sslCtxBoth` 仍 exposes `ISSLClientConnection`，但调用方若选择 client role，必须显式在 connection 上设置 `ServerName`
  - `tests/test_sslctxboth_client_capability_clarification.pas` 已不再属于 intentional-compat label 集合
  - focused RED -> GREEN：
    - `tests/test_sslctxboth_client_capability_clarification.pas`
    - `tests/test_sslctxboth_roleless_handshake_clarification.pas`
    - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
- [completed] 跨 backend 网络合同已不再把 deprecated context-level SNI 当成普通指导路径：
  - `tests/integration/test_cross_backend_consistency_contract.pas`
  - `tests/integration/test_cross_backend_errors_contract.pas`
    已统一迁到 `CreateConnection(...) -> ISSLClientConnection.SetServerName(...)`
  - 它们已从 `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh` 的 intentional-compat 集合中移除
  - 新增 `tests/scripts/test_cross_backend_network_contracts_no_context_level_sni_guidance.sh`，直接守住“不再教 `Ctx.SetServerName(...)`”
  - focused compile/runtime shape 保持绿色；本机 live network path 仍因 `FAFAFA_RUN_NETWORK_TESTS!=1` 保持 gate skip
- [completed] FreePascal 客户端连接已不再继承 deprecated context-level `ServerName` fallback：
  - `src/fafafa.ssl.freepascal.connection.pas` 的 socket / stream 两个 client 构造器都已移除 shared compat shim 读取
  - `tests/test_freepascal_context_server_name_inheritance.pas` 已翻成 negative regression：builder/direct context path 都不再把 `ServerName` 自动带进新连接
  - 新增 `tests/scripts/test_freepascal_client_connections_no_context_servername_fallback.sh`
  - `tests/test_freepascal_context_server_name_inheritance.pas` 已从 intentional-compat label 集合中移除
  - focused RED -> GREEN：
    - `bash tests/scripts/test_freepascal_client_connections_no_context_servername_fallback.sh`
    - `tests/test_freepascal_context_server_name_inheritance.pas`
    - `tests/test_connection_builder_hostname_precedence.pas`
    - `tests/test_tls_connector_hostname_override_precedence.pas`
- [completed] `TSSLConnectionBuilder` 客户端路径已不再保留 inherited context fallback：
  - `src/fafafa.ssl.connection.builder.pas` 的 `TryBuildClient` 现在在连接支持 `ISSLClientConnection` 且未调用 `WithHostname(...)` 时，会显式 `SetServerName('')`
  - `tests/test_connection_builder_hostname_precedence.pas` 已翻成 no-fallback precedence contract：
    - 未调用 `WithHostname(...)` -> 不再保留 context fallback
    - `WithHostname('conn.example.com')` -> 继续显式覆盖
    - `WithHostname('')` -> 继续显式清空
  - `tests/test_connection_builder_hostname_precedence.pas` 已从 intentional-compat label 集合中移除
  - focused RED -> GREEN：
    - `tests/test_connection_builder_hostname_precedence.pas`
    - `tests/test_tls_connector_hostname_override_precedence.pas`
    - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
- [completed] `TSSLConnector` override precedence 契约已不再依赖 inherited context fallback 输入：
  - `tests/test_tls_connector_hostname_override_precedence.pas` 已移除 mock `Ctx.SetServerName('ctx.example.com')`
  - 新增 `tests/scripts/test_tls_connector_override_no_context_level_sni_guidance.sh`
  - `tests/test_tls_connector_hostname_override_precedence.pas` 已从 intentional-compat label 集合中移除
  - focused 验证：
    - `bash tests/scripts/test_tls_connector_override_no_context_level_sni_guidance.sh`
    - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
    - `tests/test_tls_connector_hostname_override_precedence.pas`
- [completed] `TSSLConnector` early-data 合同已不再依赖 inherited context fallback 输入：
  - `tests/test_tls_connector_early_data_contract.pas` 已移除 mock `Ctx.SetServerName('ctx.example.com')`
  - 新增 `tests/scripts/test_tls_connector_early_data_no_context_level_sni_guidance.sh`
  - focused 验证：
    - `bash tests/scripts/test_tls_connector_early_data_no_context_level_sni_guidance.sh`
    - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
    - `tests/test_tls_connector_early_data_contract.pas`
- [completed] FreePascal-focused client context-ServerName contracts 已与 live runtime truth 重新对齐：
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
  - `tests/test_factory_server_name_scope_clarification.pas`
  - `tests/test_factory_config_server_name_isolation.pas`
    不再错误宣称 FreePascal 新连接会继承 deprecated context-level `ServerName`
  - 它们现在继续覆盖 context state 仍被保留，但 client connection 已明确不再自动继承
  - focused 验证：
    - `tests/test_context_builder_server_servername_runtime_consistency.pas`
    - `tests/test_factory_server_name_scope_clarification.pas`
    - `tests/test_factory_config_server_name_isolation.pas`
    - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
- [completed] Shared client fallback divergence 已完成跨 backend 对齐：
  - `src/fafafa.ssl.context.compat.pas` 现在对任意非空 context 都返回 `''`
  - OpenSSL / WolfSSL / MbedTLS / WinSSL 虽然仍走 shared seam，但新 client connection 不再继承 deprecated context-level `ServerName`
  - FreePascal 继续保持早先的 no-inheritance 规则，且不再依赖 shared helper
  - dedicated cross-backend contract:
    - `tests/test_cross_backend_client_context_server_name_clarification.pas`
  - source contract 已同步到当前真相：
    - `tests/scripts/test_context_server_name_compat_shim_contract.sh`
      现在要求 shared helper 只出现在 OpenSSL / WolfSSL / MbedTLS / WinSSL
      并禁止 FreePascal/helper/backend source 重新引入 direct context getter fallback
- [completed] High-level context `ServerName` write surfaces 已完成 `warning + ignore` 收口：
  - `src/fafafa.ssl.context.builder.pas`
    的 `BuildClient` 不再把 `WithSNI(...)` 写回 built client context
  - `src/fafafa.ssl.factory.pas`
    的 client default-config / one-shot `CreateContext(...)` 路径
    不再把 `TSSLConfig.ServerName` 写回新建 context
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
  - `tests/test_factory_server_name_scope_clarification.pas`
  - `tests/test_factory_config_server_name_isolation.pas`
    已翻成 built context `GetServerName = ''` 的新真相
  - focused 验证：
    - `tests/test_context_builder_server_name_compatibility_warning.pas`
    - `tests/test_factory_server_name_compatibility_warning.pas`
    - `tests/config/test_config_validation.pas`
    - `tests/test_cross_backend_client_context_server_name_clarification.pas`
- [completed] OpenSSL backend-specific direct library default-config path 已与当前高层真相对齐：
  - `src/fafafa.ssl.openssl.backed.pas`
    的 `TOpenSSLLibrary.CreateContext(...)`
    不再把 `FDefaultConfig.ServerName` 写回新建 client context
  - 同一路径在 server context 下若 default-config 带 `ServerName`，现在会 fail-fast reject
  - direct OpenSSL library path 若配置了 log callback，也会发出 compatibility warning
  - focused 验证：
    - `tests/test_openssl_library_default_config_server_name_clarification.pas`
    - `tests/test_cross_backend_client_context_server_name_clarification.pas`
- [completed] final public surface cleanup prep 的第一刀 static classification cleanup 已收口：
  - `tests/test_quick.pas` 不再把 `.WithSNI('example.com')` 当普通 builder smoke 用法
  - `tests/winssl/test_winssl_connection_edge_cases.pas` 不再顺手写无行为意义的 `LConfig.ServerName := ...`
  - 剩余 builder/config compatibility surface 测试现在全部显式带 `INTENTIONAL_COMPAT`
  - 新增 `tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
    守住 deprecated `WithSNI(...)` / `TSSLConfig.ServerName` 只存在于 allowlist compatibility tests
- [completed] final public surface cleanup prep 的第二刀 active direct-context classification cleanup 已收口：
  - active tests 中剩余 real `Ctx.SetServerName(...)` 命中已经全部显式分类
  - 新增 `tests/scripts/test_active_direct_context_servername_surface_classification_contract.sh`
    守住 direct-context `SetServerName(...)` 只存在于 allowlist compatibility / API-surface tests
  - `tests/test_cross_backend_client_context_server_name_clarification.pas`
  - `tests/test_sslctxboth_client_capability_clarification.pas`
  - `tests/test_connection_builder_hostname_precedence.pas`
    现在都显式带 `INTENTIONAL_COMPAT`
- [completed] intentional direct-context compatibility tests 的 local warning quarantine 已补齐：
  - `tests/test_cross_backend_client_context_server_name_clarification.pas`
  - `tests/test_sslctxboth_client_capability_clarification.pas`
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
    现在对刻意保留的 deprecated context getter/setter 做局部 warning suppression
  - focused compile outputs 已不再额外夹带这些 direct-context deprecation 噪音
- [completed] `WithSNI(...)` compiler-level deprecation alignment 已收口：
  - `src/fafafa.ssl.context.builder.pas`
    的 public `ISSLContextBuilder.WithSNI(...)` 与内部 `TSSLContextBuilderImpl.WithSNI(...)`
    declaration 现在都已经是编译期 `deprecated`
  - 新增 `tests/scripts/test_withsni_compiler_deprecated_contract.sh`
    守住源码层 truth，不允许 `WithSNI(...)` 重新退回“只有注释/运行时 warning”的状态
  - 刻意保留 `.WithSNI(...)` 的 compatibility tests 现在都做了局部 warning quarantine，
    避免 focused compile 输出被这条已知 deprecated surface 反复刷屏
- [completed] `TSSLConfig.ServerName` 的 `v1.x` surface truth freeze 已收口：
  - 当前设计决定是不在 `v1.x` 直接移除或改名这个字段，避免破坏现有源码兼容
  - 但 active source/doc truth 现在已经被锁成 compatibility-only：
    - `src/fafafa.ssl.base.pas` 字段注释明确指向 `ISSLClientConnection.SetServerName`
    - generic factory / OpenSSL direct-library warning 明确点名 `TSSLConfig.ServerName`
    - active docs 只允许 `docs/reference/API_REFERENCE.md` 以 compatibility note 形式提及它
  - 新增 `tests/scripts/test_tsslconfig_servername_surface_truth_contract.sh`
    守住这条 `v1.x freeze` truth，不允许它重新漂回普通主路径
- [completed] direct `ISSLContext.SetServerName/GetServerName` 的 `v1.x` surface truth freeze 已收口：
  - 当前设计决定是不在 `v1.x` 直接移除这组 deprecated context API，避免破坏现有源码兼容
  - 但它们现在已经被锁成 deprecated compatibility-only surface：
    - `src/fafafa.ssl.base.pas` 的 deprecation message 统一指向 `ISSLClientConnection.Set/GetServerName`
    - production `src/` 已不再存在真实 direct context caller
    - active docs 不再把 `Ctx.SetServerName(...)` 当普通 client 指导路径
  - 新增 `tests/scripts/test_direct_context_servername_surface_truth_contract.sh`
    守住这条 `v1.x freeze` truth，不允许 direct context guidance 或 production caller 回流
- [completed] `WithSNI(...)` 的 `v1.x` surface truth freeze 已收口：
  - 当前设计决定是不在 `v1.x` 直接移除或改挂这个 fluent method，避免破坏现有源码兼容
  - 但它现在已经被锁成 deprecated compatibility-only fluent surface：
    - `src/fafafa.ssl.context.builder.pas` 保持 compatibility-only comment
    - compiler `deprecated` declaration 已由 dedicated contract 守住
    - active docs 只允许 `docs/reference/API_REFERENCE.md` 提及 `WithSNI(...)`
    - active tests 继续只允许 allowlist compatibility coverage
  - 新增 `tests/scripts/test_withsni_surface_truth_contract.sh`
    守住这条 `v1.x freeze` truth，不允许 `.WithSNI(...)` 重新漂回普通 fluent builder 示例
- [completed] `optional backends certificate stream/memory truth` 已完成 focused 收口：
  - 新 plan：
    - `docs/plans/2026-05-20-optional-backends-certificate-stream-memory-truth.md`
  - 这批收回的是一条真正还没被验证完的 certificate public surface：
    - `LoadFromStream`
    - `LoadFromMemory`
    - `SaveToStream`
  - 当前已确认并修复的真问题：
    - `MbedTLS`
      PEM memory
      需要 content-aware parse
      + null terminator
    - `WolfSSL`
      memory / stream
      之前比 file surface 更窄，
      只接受 DER
    - `WolfSSL`
      在 malformed PEM
      边界上会把
      `EBase64Error`
      向外逃逸，
      而不是
      `False`
      + 空状态
  - 当前 focused proof：
    - `tests/connection/test_wolfssl_metadata_accuracy.pas`
      - PASS
    - `tests/test_wolfssl_framework.pas`
      - PASS
      - `245 passed / 0 failed`
    - `tests/test_mbedtls_framework.pas`
      - PASS
      - `231 passed / 0 failed`
    - `git diff --check`
      - PASS
  - 当前结论：
    - optional backends
      的 certificate stream/memory surface
      已对齐到当前仓库其它 backend 的 content-aware truth：
      - valid PEM memory
        通过
      - `SaveToStream -> LoadFromStream`
        roundtrip
        通过
      - malformed PEM
        继续失败，
        但现在是稳定 fail-closed

## Scope

1. 公共 Pascal surface：
   - `src/fafafa.ssl.base.pas`
   - `src/fafafa.ssl.pas`
2. 高层创建/配置路径：
   - `src/fafafa.ssl.factory.pas`
   - `src/fafafa.ssl.context.builder.pas`
3. capability truth：
   - `docs/BACKEND_CAPABILITY_MATRIX.md`
   - `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md`
   - `src/fafafa.ssl.backend.selector.pas`
   - `src/fafafa.ssl.capability.*`
4. backend 实现：
   - `src/fafafa.ssl.openssl.lib.pas`
   - `src/fafafa.ssl.winssl.lib.pas`
   - `src/fafafa.ssl.freepascal.lib.pas`
   - `src/fafafa.ssl.mbedtls.lib.pas`
   - `src/fafafa.ssl.wolfssl.lib.pas`
5. 验证与合同：
   - `tests/test_capability_matrix_v12.pas`
   - `tests/contract/test_backend_contract.pas`
   - 需要时新增 focused source contract

## Current Queue

1. 进入 final public surface cleanup prep：
   - `TSSLConfig.ServerName` 已冻结为 `v1.x` compatibility-only field
   - direct `ISSLContext.SetServerName/GetServerName` 已冻结为 `v1.x` deprecated compatibility API
   - `WithSNI(...)` 已冻结为 `v1.x` deprecated compatibility-only fluent surface
   - 当前 `context-level SNI` 兼容家族在 `v1.x` 已无新的即时 surface 收口项
2. `TSSLConfig` post-SNI 第一批已经落成 `scope buckets` truth：
   - `docs/plans/2026-05-18-tsslconfig-scope-buckets.md`
   - `src/fafafa.ssl.base.pas` 和 `docs/reference/API_REFERENCE.md` 现在直接写明 mixed-scope buckets：
     - `library-scoped defaults`
     - `context-scoped`
     - `connection-scoped`
     - `compatibility-only`
     - `option-bridge`
   - 新增 `tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
     守住 source/doc/factory/OpenSSL direct-path 的 bucket truth
3. `ISSLLibrary.CreateContext(AType)` 的 direct-library default-config parity 已完成第一轮收口：
   - 新 plan：
     - `docs/plans/2026-05-18-direct-library-default-config-parity.md`
   - 新验证：
     - `tests/test_direct_library_default_config_parity.pas`
     - `tests/scripts/test_direct_library_default_config_parity_contract.sh`
   - 当前已对齐的 context-safe 默认字段：
     - `ProtocolVersions`
     - `PreferredVersion`
     - `VerifyMode`
     - `VerifyDepth`
     - `CipherList`
     - `CipherSuites`
     - `Options`
     - `SessionCacheSize`
     - `SessionTimeout`
     - `SessionCacheMode`
     - `ALPNProtocols`
   - `SetDefaultConfig(...)` 也已在 `freepascal` / `winssl` / `mbedtls` / `wolfssl` library units 中补齐 normalization
4. direct-library `ServerName` compatibility parity 也已完成第一轮收口：
   - 新 plan：
     - `docs/plans/2026-05-18-direct-library-servername-compatibility-parity.md`
   - 新验证：
     - `tests/test_freepascal_library_default_config_server_name_clarification.pas`
     - `tests/scripts/test_direct_library_servername_compatibility_contract.sh`
   - 当前 direct-library path 已对齐：
     - client default-config = warning + ignore
     - server default-config = reject
   - 这条规则现在已在 `openssl` / `freepascal` / `winssl` / `mbedtls` / `wolfssl` library units 上保持同一条 source truth
5. direct-library `early-data / replay-store` parity 也已完成第一轮收口：
   - 新 plan：
     - `docs/plans/2026-05-18-direct-library-early-data-replay-store-parity.md`
   - 新验证：
     - `tests/test_direct_library_early_data_replay_store_parity.pas`
     - `tests/scripts/test_direct_library_early_data_replay_store_parity_contract.sh`
   - 当前 direct-library path 已对齐：
     - `ClientEarlyDataEnabled`
     - `ServerEarlyDataPolicy`
     - `ServerMaxEarlyDataSize`
     - `ServerEarlyDataReplayStoreFile`
     - `ServerEarlyDataReplayStoreDirectory`
   - replay-store 语义现在也与 factory/context path 同步：
     - client path = reject
     - server file/directory = mutually exclusive
     - backend 不实现 installer seam = fail-fast
   - 这条规则现在已通过 shared helper 固定在
     `openssl` / `freepascal` / `winssl` / `mbedtls` / `wolfssl`
     的 library `CreateContext(AType)` 路径上
6. direct-library special-case parity 当前已全部收口，下一条不该再回到这条线：
   - 这类问题已经不需要和 `ISSLConnection` 大手术混成一批
7. 在 direct-library special-case parity 收口后，再决定 broader interface debt 的后续路线：
   - 是否继续推进 `TSSLConfig` option-bridge freeze / slimming
   - 还是进入 `ISSLConnection` 核心 surface slimming roadmap
8. 若未来要让 serializer 对“纯 legacy-only in-memory record”也具备完全无歧义的 projection，需要先为 capability model 补 presence/truth 元信息；当前批次不在无信号状态下瞎猜。
9. `TSSLConfig option-bridge default truth parity` 当前也已完成第一轮收口：
   - 新 plan：
     - `docs/plans/2026-05-18-tsslconfig-option-bridge-default-truth-parity.md`
   - 新验证：
     - `tests/test_tsslconfig_option_bridge_default_truth.pas`
     - `tests/scripts/test_tsslconfig_option_bridge_default_truth_contract.sh`
   - 当前已对齐的 fresh default-config surfaces：
     - factory-held `ISSLLibrary.GetDefaultConfig(...)`
     - `CreateDefaultConfig(...)`
     - `Lib.SetDefaultConfig(Lib.GetDefaultConfig)` round-trip
   - 当前已确认的真实根因：
     - `factory` 对真实 backend 仍走 raw registered-class instantiation
     - 这条路径会丢失 backend constructor 内部建立的 `FDefaultConfig` 真相
     - 因而问题不只是 “constructor normalization 不够”，而是 “生产实例化路径本身不保真”
   - 当前修法：
     - `TSSLFactory` 增加 explicit creator-function registration path
     - `openssl` / `freepascal` / `winssl` / `mbedtls` / `wolfssl`
       真实 backend 注册统一改走 `Create*SSLLibrary(...)`
   - 下一条相关路线不该再回到这个 fresh default-config surface：
     - 若继续推进，应讨论 `Options vs legacy booleans` 的 broader precedence/slimming 规则
     - 而不是重新怀疑 `CreateDefaultConfig(...)` 单点
10. `TSSLConfig option-bridge precedence freeze` 当前也已完成第一轮收口：
    - 新 plan：
      - `docs/plans/2026-05-18-tsslconfig-option-bridge-precedence-freeze.md`
    - 新验证：
      - `tests/test_tsslconfig_option_bridge_precedence_freeze.pas`
      - `tests/scripts/test_tsslconfig_option_bridge_precedence_freeze_contract.sh`
    - 当前已冻结的 `v1.x` truth：
      - legacy booleans 仍是 compatibility write surface
      - 当 `Options` 与 legacy booleans 冲突时，legacy booleans 赢
      - normalization 会先把 legacy booleans 写进 `Options`
      - 再把最终 `Options` truth 回投到 legacy booleans
    - 当前 production proof 已覆盖：
      - `TSSLFactory.NormalizeConfig(...)`
      - `TSSLFactory.CreateContext(const AConfig)`
      - `ISSLLibrary.SetDefaultConfig(...)` / `ISSLLibrary.CreateContext(AType)`
    - 下一条相关路线不该再回到“冲突输入到底谁赢”的讨论：
      - 若继续推进，应进入真正的 `TSSLConfig` public-surface slimming / migration 设计
      - 而不是再把 precedence 当成未定规则
11. `TSSLConfig option-bridge surface truth freeze` 当前也已完成第一轮收口：
    - 新 plan：
      - `docs/plans/2026-05-18-tsslconfig-option-bridge-surface-truth-freeze.md`
    - 新验证：
      - `tests/scripts/test_tsslconfig_option_bridge_surface_truth_contract.sh`
      - `tests/test_tsslconfig_option_bridge_default_truth.pas`
      - `tests/test_tsslconfig_option_bridge_precedence_freeze.pas`
      - `tests/security/test_session_security.pas`
    - 当前已冻结的 `v1.x` public truth：
      - `EnableCompression` / `EnableSessionTickets` / `EnableOCSPStapling`
        是 compatibility-only option-bridge booleans
      - 新代码应优先直接写 `Options`
      - 仍需覆盖这些字段的测试必须显式标记为 compatibility coverage
      - 非 compatibility 活跃测试不应再把它们当主写入口
    - 当前 focused proof 已覆盖：
      - source comment / API reference wording
      - dedicated compatibility tests label truth
      - active session-security coverage 改走 context `SetOptions(...)` / `GetOptions(...)`
    - 下一条相关路线不该再回到“这些字段是不是普通主路径”的讨论：
      - 若继续推进，应进入真正的 `TSSLConfig` slimming / migration design
      - 而不是重复补 public wording 或兼容测试标签
12. `TSSLConfig active guidance cleanup` 当前也已完成第一轮收口：
    - 新 plan：
      - `docs/plans/2026-05-18-tsslconfig-active-guidance-cleanup.md`
    - 新验证：
      - `tests/scripts/test_tsslconfig_active_guidance_truth_contract.sh`
      - `examples/example_factory_usage.pas` focused compile
    - 当前已收口的 active guidance 漂移：
      - 活跃 example 不再把 `BufferSize` / `HandshakeTimeout` 教成 factory/config 主路径
      - `docs/reference/ARCHITECTURE.md` 不再描述过时的伪 `TSSLConfig` 结构
      - `tests/examples/test_lib_core_functionality.pas` 的 direct context `SetServerName(...)` example-surface coverage 继续显式带 `INTENTIONAL_API_SURFACE`
    - 下一条相关路线不该再回到高可见度 guidance cleanup：
      - 若继续推进，应进入真正的 `TSSLConfig` public-surface slimming / migration design
      - 而不是继续修 example/reference 漂移
13. `TSSLConfig public-surface slimming roadmap` 当前也已完成第一轮收口：
    - 新 plan：
      - `docs/plans/2026-05-18-tsslconfig-public-surface-slimming-roadmap.md`
    - 新验证：
      - `tests/scripts/test_tsslconfig_migration_targets_contract.sh`
    - 当前已固定的字段级迁移决策：
      - `LogLevel` / `LogCallback` -> library defaults surface
      - `HandshakeTimeout` / `BufferSize` -> connection / transport surface
      - `ServerName` -> per-connection SNI surface
      - `EnableCompression` / `EnableSessionTickets` / `EnableOCSPStapling` -> `Options` / `WithOption(...)`
      - context-safe 字段继续留在 `TSSLConfig` 主路径
    - 下一条相关路线不该再回到“先补一份 migration map”：
      - 若继续推进，应在上述 buckets 中挑第一条最小实现切片
      - 当前最优先候选是 `LogLevel` / `LogCallback` 的 library-default detachment
14. `TSSLConfig logging surface truth freeze` 当前也已完成第一轮收口：
    - 新 plan：
      - `docs/plans/2026-05-18-tsslconfig-logging-surface-truth-freeze.md`
    - 新验证：
      - `tests/scripts/test_tsslconfig_logging_surface_truth_contract.sh`
      - `tests/test_factory_logging_scope_clarification.pas`
      - `tests/config/test_default_config.pas`
    - 当前已收口的真实 drift：
      - `docs/guides/USER_GUIDE.md`
      - `docs/guides/TROUBLESHOOTING.md`
        不再把“只调用 `ISSLLibrary.SetLogCallback(...)`”教成足以看到 `sslLogInfo` / `sslLogDebug` 输出的完整配置
      - `docs/reference/API_REFERENCE.md`
      - `docs/reference/ARCHITECTURE.md`
        现在明确拆开：
        - `LogLevel` 走 `ISSLLibrary.GetDefaultConfig(...)` / `SetDefaultConfig(...)`
        - `LogCallback` 走 `ISSLLibrary.SetLogCallback(...)`
        - fresh/request config 仍回到 `sslLogError` + `nil` baseline
    - 当前 focused proof 已覆盖：
      - 新 docs contract 先 RED 后 GREEN，直接证明活跃 guidance 曾经和 runtime truth 冲突
      - 既有 Pascal logging 回归继续保持绿色，说明这次收口只修 guidance truth，没有扰动 runtime/source contract
    - 下一条相关路线不该再回到 logging guidance 漂移：
      - 若继续沿 `TSSLConfig` buckets 推进，应优先寻找新的 live bug 信号
      - 不要再把 `LogLevel` / `LogCallback` 的 active docs truth 当成未收口问题反复拉起
15. `direct-library connection-scope clarification` 当前也已完成第一轮收口：
    - 新 plan：
      - `docs/plans/2026-05-18-direct-library-connection-scope-clarification.md`
    - 新验证：
      - `tests/scripts/test_direct_library_connection_scope_clarification_contract.sh`
      - `tests/test_freepascal_library_default_config_connection_scope_clarification.pas`
      - `tests/test_factory_connection_scope_clarification.pas`
    - 当前已收口的真实 drift：
      - `ISSLLibrary.SetDefaultConfig(...)` 之前可以保存自定义 `HandshakeTimeout` / `BufferSize`
      - 五个 backend 的 `CreateContext(AType)` 又不会消费这两个 connection-scoped 字段
      - 因而 direct-library path 曾经留下了“default-config 可写、CreateContext 静默忽略”的假可用入口
    - 当前修法：
      - 在 `src/fafafa.ssl.context.config.pas` 新增 shared `ValidateDirectLibraryConnectionScope(...)`
      - `openssl` / `freepascal` / `winssl` / `mbedtls` / `wolfssl`
        的 library `CreateContext(AType)` 统一接入这条 helper
      - `docs/reference/API_REFERENCE.md`
      - `docs/reference/ARCHITECTURE.md`
        也同步改成 direct-library path reject 这两个字段
    - 当前 focused proof 已覆盖：
      - 新 contract 先 RED 后 GREEN，直接证明 docs/source 曾经没有把 direct-library connection-scope truth 说清楚
      - 新 FreePascal direct-library runtime test 先 RED 后 GREEN，直接证明生产路径从 silent accept 变成 fail-fast reject
      - 既有 factory connection-scope 回归继续绿色，说明 shared helper 没扰动原有 factory truth
    - 下一条相关路线不该再回到 direct-library `HandshakeTimeout` / `BufferSize` 漂移：
      - 后续应继续找新的 live interface/implementation gap
      - 不要再把 direct-library connection-scope 静默忽略当成未收口问题反复拉起
16. `library-default LogCallback detachment` 当前也已完成第一轮收口：
    - 新 plan：
      - `docs/plans/2026-05-18-library-default-logcallback-detachment.md`
    - 新验证：
      - `tests/scripts/test_library_default_logcallback_detachment_contract.sh`
      - `tests/test_factory_logging_scope_clarification.pas`
      - `tests/test_freepascal_library_default_config_server_name_clarification.pas`
      - `tests/test_openssl_library_default_config_server_name_clarification.pas`
      - `tests/config/test_default_config.pas`
      - `tests/scripts/test_tsslconfig_logging_surface_truth_contract.sh`
      - `tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
      - `tests/scripts/test_tsslconfig_migration_targets_contract.sh`
    - 当前已收口的真实 drift：
      - public truth 已经把 callback owner 收到 `ISSLLibrary.SetLogCallback(...)`
      - 但五个 backend 的 `SetDefaultConfig(...)` 之前仍会直接把 `LConfig.LogCallback` 装进 runtime `FLogCallback`
      - 结果就是 `LogCallback` 同时挂在 default-config path 和 dedicated setter path 上，owner 不单一
    - 当前修法：
      - `openssl` / `freepascal` / `winssl` / `mbedtls` / `wolfssl`
        的 `SetDefaultConfig(...)` 现在只继续更新 `LogLevel` 和其他 default-config 字段
      - runtime callback 改为只由 `SetLogCallback(...)` 维护
      - `GetDefaultConfig(...)` 仍然镜像当前 callback 真相，但 `SetDefaultConfig(...)` 不再安装或替换它
      - `docs/reference/API_REFERENCE.md`
      - `docs/reference/ARCHITECTURE.md`
      - `src/fafafa.ssl.base.pas`
        也同步写明这条 detachment truth
    - 当前 focused proof 已覆盖：
      - 新 source contract 先 RED 后 GREEN，直接证明 5 个 backend 曾经都还让 `SetDefaultConfig(...)` 安装 callback
      - 强化后的 logging runtime 回归先 RED 后 GREEN，直接证明：
        - `SetDefaultConfig(LogCallback)` 不再安装 callback
        - `SetLogCallback(...)` 仍是唯一 owner
        - 后续 `SetDefaultConfig(LogLevel)` 不会顺手清掉已安装 callback
      - 受影响的 direct-library `ServerName` warning 测试继续绿色，说明这次 detachment 没把已有 warning/reject 路线带歪
      - default-config / docs / scope-bucket / migration-targets focused contracts 继续绿色
    - 下一条相关路线不该再回到 `LogCallback` owner 模糊地带：
      - `LogLevel` / `LogCallback` 这条线当前已从 docs freeze 进入 runtime/source truth
      - 后续应继续找新的 live interface/implementation gap，而不是再把 callback default-config owner 当成未收口问题反复拉起
17. `noninteractive core compat tests` 当前也已完成第一轮收口：
    - 新 plan：
      - `docs/plans/2026-05-18-noninteractive-core-compat-tests.md`
    - 新验证：
      - `tests/test_factory_logic.pas`
      - `tests/test_data_structures.pas`
    - 当前已收口的真实问题：
      - 这两份核心 `TSSLConfig` record-shape / compatibility 测试此前虽然能跑通，
        但末尾仍保留：
        - `WriteLn('按回车键退出...')`
        - `ReadLn`
      - 结果就是它们继续表现得像“手工演示程序”，而不是直接适合自动化执行的测试
    - 当前修法：
      - 移除两份文件末尾的交互式退出逻辑
      - 头部 `INTENTIONAL_COMPAT` 注释同步补清：
        - deprecated `ServerName`
        - option-bridge booleans
        - mixed-scope record-shape fields（`BufferSize` / `HandshakeTimeout`）
    - 当前 focused proof 已覆盖：
      - 修复前 direct run 输出会以“按回车键退出...”收尾
      - 修复后两份测试都可直接 `timeout 2 ./...` 跑完，且输出不再留下交互式退出尾巴
    - 下一条相关路线不该再回到这两份 core test 的交互尾巴：
      - 它们当前已可作为自动化测试程序直接执行
      - 后续应继续找新的 live interface/implementation gap，而不是再把这两份文件的手工退出逻辑当成未收口问题反复拉起
18. `top-level core tests noninteractive` 当前也已完成第一轮收口：
    - 新 plan：
      - `docs/plans/2026-05-18-noninteractive-top-level-core-tests.md`
    - 新验证：
      - `tests/scripts/test_top_level_core_tests_noninteractive_contract.sh`
      - `tests/test_exceptions.pas`
      - `tests/test_base_interface_contract.pas`
    - 当前已收口的真实问题：
      - 这两份顶层 core test 在当前 headless shell 下虽然会因 stdin EOF 直接退出，
        但源码末尾仍保留：
        - `WriteLn('按回车键退出...')`
        - `ReadLn`
      - 结果就是自动化输出会持续带着手工演示尾巴，且退出行为依赖运行方式
    - 当前修法：
      - 移除两份文件末尾的交互式退出逻辑
      - 新增 focused shell contract，禁止这两份文件重新带回交互尾巴
    - 当前 focused proof 已覆盖：
      - 新合同先 RED，直接命中 `tests/test_exceptions.pas` 的残余 `ReadLn`
      - 修复后新合同 GREEN
      - 两份测试都可直接 `timeout 2 ./...` 跑完，且输出尾部只保留测试总结
    - 下一条相关路线不该再回到这两份顶层 core test 的交互尾巴：
      - 这条线现在已经有 source contract 护栏
      - 若继续清理 `ReadLn` 残留，应优先按 `top-level test -> WinSSL specialized test -> examples/diagnostics` 分层，而不是重新混做一批
19. `WinSSL active tests noninteractive` 当前也已完成第一轮收口：
    - 新 plan：
      - `docs/plans/2026-05-18-noninteractive-winssl-active-tests.md`
    - 新验证：
      - `tests/scripts/test_winssl_active_tests_noninteractive_contract.sh`
      - `run_winssl_tests.ps1`
      - `tests/unit/test_winssl_comprehensive.pas`
      - `tests/winssl/test_winssl_context_comprehensive.pas`
      - `tests/winssl/test_winssl_errors_comprehensive.pas`
      - `tests/winssl/test_winssl_monitoring.pas`
      - `tests/winssl/test_winssl_connection_edge_cases.pas`
      - `tests/winssl/test_winssl_certstore.pas`
      - `tests/winssl/test_winssl_session_management.pas`
      - `tests/winssl/test_winssl_library_basic.pas`
      - `tests/winssl/test_winssl_certificate_loading.pas`
    - 当前已收口的真实问题：
      - 这批文件虽然属于活跃 WinSSL 测试程序，并且仍被脚本/验证清单引用，
        但源码末尾仍保留：
        - `WriteLn('按回车键退出...')`
        - `WriteLn('Press Enter to exit...')`
        - `ReadLn`
      - 其中 `run_winssl_tests.ps1` 甚至明确把 `tests/unit/test_winssl_comprehensive.pas`
        归类为 `Minimal, non-network, non-interactive tests`
    - 当前修法：
      - 移除这批 WinSSL 活跃测试程序的交互式退出逻辑
      - 新增 focused source contract，禁止这些文件重新带回交互尾巴
      - 不混入 examples / diagnostics / benchmark
    - 当前 focused proof 已覆盖：
      - 新合同先 RED，直接命中 `tests/unit/test_winssl_comprehensive.pas`
      - 修复后新合同 GREEN
      - `tests/unit/test_winssl_comprehensive.pas` 的 Linux 非 Windows 分支可直接编译运行，输出不再带手工退出提示
      - `tests/unit/test_winssl_comprehensive.pas`
      - `tests/winssl/test_winssl_session_management.pas`
        的 Win64 交叉编译都已通过，说明这次尾部清理没有破坏 Windows 语法面
    - 下一条相关路线不该再回到 WinSSL 活跃测试程序的交互尾巴：
      - 这条线现在已有 focused contract 护栏
      - 若继续清理 `ReadLn` 残留，只应处理 examples / diagnostics / benchmark 等明确非活跃测试面
      - 更高优先级则应回到 broader interface debt，而不是继续沉在已收口的 active test prompt cleanup
20. `backend optional-surface completion-audit revalidation` 当前也已完成第一轮收口：
    - 新 plan：
      - `docs/plans/2026-05-18-backend-optional-surface-completion-audit-revalidation.md`
    - 新验证：
      - `tests/contract/test_backend_contract.pas`
      - `docs/plans/2026-05-04-backend-context-optional-interface-completion-audit.md`
      - `docs/plans/2026-05-04-backend-context-native-handle-completion-audit.md`
      - `docs/plans/2026-05-04-backend-http-hooks-interface-completion-audit.md`
      - `docs/plans/2026-05-04-backend-session-native-handle-completion-audit.md`
      - `docs/plans/2026-05-04-backend-certificate-store-native-handle-completion-audit.md`
      - `docs/plans/2026-05-04-backend-diagnostics-interface-completion-audit.md`
    - 当前已收口的真实问题：
      - 上述 6 份 plan 文档虽然对应的 contract 已经实际存在于 `tests/contract/test_backend_contract.pas`
      - 但文档本身仍缺 execution result，容易让后续会话误判这些 optional public surface 还没真的验证过
    - 当前修法：
      - focused 重新编译并运行 `tests/contract/test_backend_contract.pas`
      - 把 contracts 12-18 的现状证据回写到缺结果的 plan 文档
      - 明确标成 `Focused Revalidation Result (2026-05-18)`，不虚报未重跑的重门禁
    - 当前 focused proof 已覆盖：
      - `tests/contract/test_backend_contract.pas` 当前结果：
        - `Total Tests: 135`
        - `Passed: 111`
        - `Failed: 0`
        - `Skipped: 24`
      - OpenSSL / WolfSSL / MbedTLS / FreePascal 的 context optional/native-handle、HTTP hooks、session native-handle、certificate-store native-handle、diagnostics surface 全部 PASS
      - WinSSL 继续按 Linux 主机平台边界 SKIP；`Contract 15` 也继续明确 session truth 需要 dedicated Windows batch
    - 下一条相关路线不该再回到“这些 optional surface 可能还没验证过”的怀疑：
      - 当前缺口已经从“缺 contract/缺结果”收成“已有 focused live proof”
    - 更高优先级应回到 broader interface debt：
        - `TSSLConfig` public-surface slimming 后续
        - `ISSLConnection` 核心 surface slimming / completion audit
21. `ISSLConnection surface truth freeze` 现在应作为当前默认主线：
    - 新 plan：
      - `docs/plans/2026-05-18-isslconnection-surface-truth-freeze.md`
    - 当前已确认的工作流偏差：
      - `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md` 仍把 `TSSLConfig` 写成默认 immediate next step
      - 但仓库当前更急的误导源其实是 `docs/reference/API_REFERENCE.md`
        中 `ISSLConnection` / `ISSLSession` active docs 与源码真相漂移
    - 当前批的目标：
      - 先冻结活跃文档真相，不直接修改 public signature
      - 把 `ISSLConnection` 的 compatibility-core mirrors 与 optional owner 说明写清楚
      - 新增 focused contract，阻止旧方法名再次回流到 active docs
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslconnection_surface_truth_contract.sh`
      - `bash tests/scripts/test_isslconnection_surface_truth_contract.sh`
      - `git diff --check`
      - 当前结果均为 PASS，说明这批已经在文档/contract 层完成收口
    - 当前批收口后，下一步才适合从稳定真相上选择第一条真正的 slimming slice：
      - `ISSLConnection` compatibility-core slimming
      - 或回到 `TSSLConfig` 的更小实现切片
22. `backend connection-surface completion-audit revalidation` 当前也应补齐：
    - 新 plan：
      - `docs/plans/2026-05-18-backend-connection-surface-completion-audit-revalidation.md`
    - 当前重新核对后确认的事实：
      - `ISSLConnectionInfo` / `ISSLSessionResumption` / `ISSLCertificateVerification`
        这些连接层 optional surface 已经有 execution result
      - 真正缺当前 execution receipt 的，是另外 3 份仍直接落在 `ISSLConnection` 主面上的旧计划：
        - `docs/plans/2026-05-04-backend-client-connection-sni-interface-alignment.md`
        - `docs/plans/2026-05-04-backend-connection-native-handle-interface-alignment.md`
        - `docs/plans/2026-05-04-backend-ocsp-connection-interface-alignment.md`
    - 当前修法：
      - focused 重新编译并运行 `tests/contract/test_backend_contract.pas`
      - 仅把 Contracts 8 / 10 / 11 的当前 live 结果回写到上述 3 份 plan
      - 不混入新的生产代码变更，也不虚报未重跑的重门禁
    - 当前 focused proof：
      - `tests/contract/test_backend_contract.pas` 当前结果仍为：
        - `Total Tests: 135`
        - `Passed: 111`
        - `Failed: 0`
        - `Skipped: 24`
      - `Contract 8`：
        - OpenSSL / WolfSSL / MbedTLS / FreePascal PASS
        - WinSSL SKIP
      - `Contract 10`：
        - OpenSSL / WolfSSL / FreePascal non-stub PASS
        - MbedTLS absent PASS
        - WinSSL SKIP
      - `Contract 11`：
        - OpenSSL / WolfSSL / MbedTLS native-handle PASS
        - FreePascal absent PASS
        - WinSSL SKIP
    - 当前批收口后，连接层历史 execution receipt 的主要缺口将被清空
    - 下一条应优先进入真正的 `ISSLConnection` slimming，而不是继续补旧计划结果
23. `ISSLConnectionInfo mirror demotion / migration-map` 现在应作为下一条 design 主线：
    - 新 plan：
      - `docs/plans/2026-05-18-isslconnectioninfo-mirror-demotion-migration-map.md`
    - 当前重新核对后确认的设计 drift：
      - `docs/reference/INTERFACE_DESIGN_V2.md` 仍漏掉 `ISSLConnectionInfo`
      - 仍保留 `ISSLAdvanced` 这个当前无实际落点的空壳名
      - `TBaseSSLConnection` 示例没列出 `ISSLConnectionInfo`
      - 迁移对照表把 `GetConnectionInfo` 错归给 `ISSLDiagnostics`
      - 还过早把 `GetStateString` / `GetContext` / `GetSelectedALPNProtocol` 直接写死到其它路线
    - 当前修法：
      - 在 `INTERFACE_DESIGN_V2.md` 中补出 `ISSLConnectionInfo`
      - 把 `GetConnectionInfo` / `GetContext` / `GetSelectedALPNProtocol` / `GetStateString`
        的 Stage-A demotion target 统一写成 `ISSLConnectionInfo`
      - 新增 focused contract，禁止错误 owner / `ISSLAdvanced` 回流
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslconnectioninfo_migration_targets_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_migration_targets_contract.sh`
      - `git diff --check`
      - 新 contract 当前 PASS
    - 当前批收口后，`ISSLConnection` 真正剩下的问题会更聚焦到 source-facing slimming prep
24. `ISSLConnectionInfo active guidance de-emphasis` 现在应作为紧随其后的用户面收口：
    - 新 plan：
      - `docs/plans/2026-05-18-isslconnectioninfo-active-guidance-deemphasis.md`
    - 当前 active-doc drift：
      - `API_REFERENCE.md` 仍直接示例 `LConn.GetConnectionInfo` / `LConn.GetSelectedALPNProtocol` / `LConn.GetStateString`
      - `INTEGRATION_GUIDE.md` 也仍把 `Conn.GetSelectedALPNProtocol` / `Conn.GetStateString` 当推荐排错路径
    - 当前修法：
      - 把这组用户可见示例改成先 `Supports(..., ISSLConnectionInfo, ...)`
      - 新增 focused contract，防止 active guidance 回流到 direct core mirror teaching
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslconnectioninfo_active_guidance_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_active_guidance_contract.sh`
      - `git diff --check`
      - 新 contract 当前 PASS
    - 当前批收口后，用户可见路径会开始和 `ISSLConnectionInfo` 的 Stage-A demotion map 真正同向
25. `ISSLConnectionInfo source classification freeze` 现在应作为 source-facing slimming prep：
    - 新 plan：
      - `docs/plans/2026-05-18-isslconnectioninfo-source-classification-freeze.md`
    - 当前 source-facing 缺口：
      - 设计文档和 active docs 已经写明 Stage-A demotion map
      - 但 `src/fafafa.ssl.base.pas` / `src/fafafa.ssl.connection.base.pas` 还没明确写出
        这 4 个 mirrors 当前是 `compatibility-core duplicates`
    - 当前修法：
      - 在 source comments 中补出 `GetConnectionInfo` / `GetContext` /
        `GetSelectedALPNProtocol` / `GetStateString` 的 Stage-A classification note
      - 新增 focused source contract，防止 source-facing truth 再次回流丢失
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslconnectioninfo_source_classification_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_source_classification_contract.sh`
      - `git diff --check`
      - 新 contract 当前 PASS
    - 当前批收口后，`ISSLConnection` 主线会更接近第一条真正的实现切片
26. `GetContext active guidance de-emphasis` 现在应作为第一条 mirror-specific route selection prep：
    - 新 plan：
      - `docs/plans/2026-05-18-getcontext-active-guidance-deemphasis.md`
    - 当前 residual drift：
      - `docs/CAPABILITY_MATRIX_GUIDE.md` 仍直接示例 `Conn.GetContext.GetLibrary.GetCapabilities`
      - `API_REFERENCE.md` 的优先路径说明还没把 `GetContext` 明确并入 `ISSLConnectionInfo` first guidance
    - 当前修法：
      - 把 capability 示例改成先 `Supports(..., ISSLConnectionInfo, ConnInfo)` 再用 `ConnInfo.GetContext`
      - 新增 focused contract，防止活跃文档把 core `GetContext` 教回推荐路径
      - 在路线图中把 `GetContext` 固定成当前第一优先 mirror
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslconnectioninfo_getcontext_active_guidance_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getcontext_active_guidance_contract.sh`
      - `git diff --check`
      - 新 contract 当前 PASS
    - 当前批收口后，下一刀就可以直接进入 `GetContext` 的 source/class split feasibility
27. `GetContext contract owner primacy` 现在应作为第一条测试层真实收窄：
    - 新 plan：
      - `docs/plans/2026-05-18-getcontext-contract-owner-primacy.md`
    - 当前 residual coupling：
      - `tests/contract/test_backend_contract.pas` 仍把 `ISSLConnection.GetContext` 和
        `ISSLConnectionInfo.GetContext` 写成并列 owner
      - 失败文案也仍然是双 owner 叙事，不利于后续真正讨论 `GetContext` 离开 core 的路线
    - 当前修法：
      - 先验证 `ISSLConnectionInfo.GetContext` 与创建 context type 一致
      - 再把 `ISSLConnection.GetContext` 降为 mirror-equality proof
      - 新增 focused source guard，防止 contract 语义回流
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslconnectioninfo_getcontext_contract_owner_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getcontext_contract_owner_contract.sh`
      - `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
      - `git diff --check`
      - focused contract 当前 PASS
    - 当前批收口后，下一刀就可以直接进入 `GetContext` 的更强 feasibility / deprecation 讨论
28. `GetContext source/class split feasibility freeze` 现在应作为第一条实现切片前的 allowlist 固化：
    - 新 plan：
      - `docs/plans/2026-05-18-getcontext-source-class-split-feasibility-freeze.md`
    - 当前 remaining surface：
      - 生产源码里只剩接口声明与 `TBaseSSLConnection.GetContext` 共享实现
      - 活跃文档只剩 `ConnInfo.GetContext`
      - direct core `LConn.GetContext` 只剩 `tests/contract/test_backend_contract.pas` 的 mirror proof
    - 当前修法：
      - 在 source comments 中补 `GetContext` 的 preferred-access / owner / mirror 语义
      - 新增 focused allowlist contract，守住当前 remaining live surface
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslconnectioninfo_getcontext_source_class_split_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getcontext_source_class_split_contract.sh`
      - `git diff --check`
      - 新 contract 当前 PASS
    - 当前批收口后，`GetContext` 就不再需要继续做 evidence cleanup，可以决定是进入 public deprecation wording 还是切到下一条 mirror
29. `GetStateString active test de-emphasis` 现在应作为下一条 mirror 的第一刀：
    - 新 plan：
      - `docs/plans/2026-05-18-getstatestring-active-test-deemphasis.md`
    - 当前 high-value residual：
      - `tests/connection/test_connection_basic.pas` 仍直接调用 `LConnection.GetStateString`
      - `tests/integration/test_real_https_connection.pas` 仍把 `Conn.GetStateString` 用作普通握手失败输出
    - 当前修法：
      - 把 generic/integration 测试切到 `ISSLConnectionInfo.GetStateString`
      - 新增 focused contract，防止普通测试路径把 direct core `GetStateString` 教回去
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslconnectioninfo_getstatestring_active_test_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getstatestring_active_test_contract.sh`
      - `mkdir -p tmp/test_connection_basic && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_basic -FEtmp/test_connection_basic -otmp/test_connection_basic/test_connection_basic tests/connection/test_connection_basic.pas && ./tmp/test_connection_basic/test_connection_basic`
      - `mkdir -p tmp/test_real_https_connection && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_real_https_connection -FEtmp/test_real_https_connection -otmp/test_real_https_connection/test_real_https_connection tests/integration/test_real_https_connection.pas && ./tmp/test_real_https_connection/test_real_https_connection`
      - `git diff --check`
      - focused contract 当前 PASS
    - 当前批收口后，下一刀就可以决定是收 residual runtime uses，还是切到 `GetSelectedALPNProtocol`
30. `GetStateString residual classification freeze` 现在应作为 active-test 之后的 allowlist 固化：
    - 新 plan：
      - `docs/plans/2026-05-18-getstatestring-residual-classification-freeze.md`
    - 当前 remaining surface：
      - direct core `GetStateString` 已从 ordinary docs/tests 退出
      - 当前 residual 只剩 backend contract mirror proof 与 OpenSSL / WolfSSL backend-specific runtime files
    - 当前修法：
      - 在 source comments 中补 `GetStateString` 的 preferred-access / owner / residual-surface 说明
      - 新增 focused allowlist contract，守住 direct core residual file set
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslconnectioninfo_getstatestring_residual_classification_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getstatestring_residual_classification_contract.sh`
      - `git diff --check`
      - 新 contract 当前 PASS
    - 当前批收口后，`GetStateString` 就不再需要继续做 evidence cleanup，可以决定是进入更强 deprecation wording 还是切到 `GetSelectedALPNProtocol`
31. `GetSelectedALPNProtocol active test de-emphasis` 现在应作为下一条 mirror 的第一刀：
    - 新 plan：
      - `docs/plans/2026-05-18-getselectedalpn-active-test-deemphasis.md`
    - 当前 high-value residual：
      - `tests/integration/test_real_https_connection.pas` 仍直接调用 `Conn.GetSelectedALPNProtocol`
      - `tests/integration/test_cross_backend_consistency_contract.pas` 仍把 `Conn.GetSelectedALPNProtocol` 当归一化 ALPN 探测输出
    - 当前修法：
      - 在这两个 ordinary integration/contract 文件里补 `ISSLConnectionInfo`-first helper
      - 新增 focused contract，防止普通测试路径把 direct core `GetSelectedALPNProtocol` 教回去
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslconnectioninfo_getselectedalpn_active_test_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getselectedalpn_active_test_contract.sh`
      - `mkdir -p tmp/test_real_https_connection && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_real_https_connection -FEtmp/test_real_https_connection -otmp/test_real_https_connection/test_real_https_connection tests/integration/test_real_https_connection.pas && ./tmp/test_real_https_connection/test_real_https_connection`
      - `mkdir -p tmp/test_cross_backend_consistency_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_consistency_contract -FEtmp/test_cross_backend_consistency_contract -otmp/test_cross_backend_consistency_contract/test_cross_backend_consistency_contract tests/integration/test_cross_backend_consistency_contract.pas && ./tmp/test_cross_backend_consistency_contract/test_cross_backend_consistency_contract`
      - `git diff --check`
      - focused contract 当前 PASS
    - 当前批收口后，下一刀就可以决定是收 residual runtime uses，还是进入更强 client-owner / deprecation wording 讨论
32. `GetSelectedALPNProtocol residual classification freeze` 现在应作为 active-test 之后的 allowlist 固化：
    - 新 plan：
      - `docs/plans/2026-05-18-getselectedalpn-residual-classification-freeze.md`
    - 当前 remaining surface：
      - direct core `GetSelectedALPNProtocol` 已从 ordinary docs/tests 退出
      - 当前 residual 只剩 backend contract mirror proof、MbedTLS backend-specific runtime test 与 WinSSL backend-specific runtime tests
    - 当前修法：
      - 在 source comments 中补 `GetSelectedALPNProtocol` 的 preferred-access / owner / residual-surface 说明
      - 新增 focused allowlist contract，守住 direct core residual file set
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslconnectioninfo_getselectedalpn_residual_classification_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getselectedalpn_residual_classification_contract.sh`
      - `git diff --check`
      - 新 contract 当前 PASS
    - 当前批收口后，`GetSelectedALPNProtocol` 就不再需要继续做 evidence cleanup，可以决定是进入更强 client-owner / deprecation wording，还是切到 `GetConnectionInfo`
33. `GetConnectionInfo residual classification freeze` 现在应作为这组 mirrors 的最后一条 allowlist 固化：
    - 新 plan：
      - `docs/plans/2026-05-18-getconnectioninfo-residual-classification-freeze.md`
    - 当前 remaining surface：
      - direct core `GetConnectionInfo` 已从 active docs 与 ordinary tests 退出
      - 当前 residual 只剩 backend contract mirror proof、OpenSSL backend-specific connection-info contract test 与 WinSSL backend-specific runtime/edge-case tests
    - 当前修法：
      - 在 source comments 中补 `GetConnectionInfo` 的 preferred-access / owner / residual-surface 说明
      - 新增 focused allowlist contract，守住 direct core residual file set
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
      - `git diff --check`
      - 新 contract 当前 PASS
    - 当前批收口后，`GetConnectionInfo` 也不再需要继续做 evidence cleanup，`ISSLConnectionInfo` 这 4 条 Stage-A mirror 路线将全部进入 post-freeze 决策阶段
34. `GetConnectionInfo base enrichment from residual audit` 已完成并应作为当前默认下一步的完成记录保留：
    - 新 plan：
      - `docs/plans/2026-05-18-getconnectioninfo-base-enrichment-from-residual-audit.md`
    - 当前已确认的共享层 completeness 修复：
      - `TBaseSSLConnection.GetConnectionInfo` 现在会统一补齐 `ServerName`
      - `SessionId` 现在会在 `FConnected or FHandshakeComplete` 且后端可返回当前 session 时补齐
      - OpenSSL / FreePascal / MbedTLS / WolfSSL / WinSSL 已通过 `DoGetConnectionInfoServerName` hook 暴露各自连接对象持有的 `FServerName`
    - 当前根因与实现约束：
      - 不应在 `TBaseSSLConnection.GetConnectionInfo` 对 `Self` 走 `Supports(Self, ISSLClientConnection, ...)`
      - 具体类直接以 object ref 使用时，这种临时 interface ref 在 `TInterfacedObject` 路径上可能触发错误的自释放
      - 因此本批使用 protected virtual hook，而不是 shared base 里的 interface cast
    - 当前 focused proof 已覆盖：
      - `tests/test_connection_builder_hostname_precedence.pas`
      - `tests/test_openssl_connection_info_cipher_contract.pas`
      - `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` 线上的高优先级下一步不再是 residual archaeology，而是剩余 completeness debt：
      - `PeerCertificate`
      - `CipherSuiteId` / `KeyExchange` / `Cipher` / `Hash` / `KeySize` / `MacSize`
      - 更强 owner / deprecation wording route
35. `GetConnectionInfo` shared `PeerCertificate` enrichment 已完成并应作为当前 implementation-completeness 主线的继续收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-getconnectioninfo-peercertificate-base-enrichment.md`
    - 当前已确认的共享层 completeness 修复：
      - `TBaseSSLConnection.GetConnectionInfo` 现在会在连接可暴露当前对端证书时统一补齐 `PeerCertificate`
      - OpenSSL / WinSSL / FreePascal / MbedTLS / WolfSSL 的既有 `DoGetPeerCertificate` / `ISSLCertificate.GetInfo` 能力现在都能被共享层折进 `TSSLConnectionInfo`
    - 当前 focused proof 已覆盖：
      - `tests/test_connection_builder_hostname_precedence.pas`
      - `tests/test_openssl_connection_info_cipher_contract.pas`
      - `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` 线上真正剩下的 completeness debt 已进一步收缩到：
      - `CipherSuiteId`
      - `KeyExchange`
      - `Cipher`
      - `Hash`
      - `KeySize`
      - `MacSize`
      - 更强 owner / deprecation wording route
36. `GetConnectionInfo` crypto detail name-derived first slice 已完成并应作为当前 shared/detail 分层路线的完成记录保留：
    - 新 plan：
      - `docs/plans/2026-05-18-getconnectioninfo-crypto-detail-name-derived-first-slice.md`
    - 当前已确认的共享层 completeness 修复：
      - shared `GetConnectionInfo` 现在会基于 negotiated `CipherSuite` 名称 best-effort 推导：
        - `Cipher`
        - `Hash`
        - `KeySize`
      - 当 cipher-suite name 显式携带 legacy key-exchange 前缀时，也会 best-effort 推导：
        - `KeyExchange`
    - 当前 static audit 结论：
      - `CipherSuiteId` / `MacSize` 仍主要属于 backend/platform-specific detail
      - `Cipher` / `Hash` / `KeySize` 更适合先走 shared name-derived normalization
      - WinSSL 继续保留自己的 override，不依赖 shared parser
    - 当前 focused proof 已覆盖：
      - `tests/test_connection_builder_hostname_precedence.pas`
      - `tests/test_openssl_connection_info_cipher_contract.pas`
      - `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` implementation-completeness 主线已进一步收缩到：
      - `CipherSuiteId`
      - `MacSize`
      - 无法只靠名字稳定推导的更细平台差异
      - 更强 owner / deprecation wording route
37. `GetConnectionInfo` `CipherSuiteId` first slice 已完成并应作为当前 implementation-completeness 主线的继续收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-getconnectioninfo-ciphersuiteid-first-slice.md`
    - 当前已确认的 shared + backend truth：
      - shared `GetConnectionInfo` 现在会对标准 TLS 1.3 cipher-suite name best-effort 推导：
        - `CipherSuiteId`
      - OpenSSL `GetConnectionInfo` 现在会优先走：
        - `SSL_CIPHER_get_protocol_id`
      - 若该 helper 不可用，则会回退：
        - `SSL_CIPHER_get_id and $FFFF`
    - 当前 focused proof 已覆盖：
      - `tests/test_connection_builder_hostname_precedence.pas`
      - `tests/test_openssl_connection_info_cipher_contract.pas`
      - `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` implementation-completeness 主线已进一步收缩到：
      - `MacSize`
      - 无法只靠名字或统一 low-level helper 稳定归一的更细平台差异
      - 更强 owner / deprecation wording route
38. WinSSL `GetConnectionInfo` cipher truth correction 已完成并应作为当前 WinSSL-specific 审查纠偏记录保留：
    - 新 plan：
      - `docs/plans/2026-05-18-winssl-connectioninfo-cipher-truth-correction.md`
    - 当前已确认的 WinSSL truth:
      - `SecPkgContext_ConnectionInfo.aiCipher`
        - 只是算法级字段
        - 不应直接写入 `CipherSuiteId`
      - WinSSL `CipherSuiteId` 现在会优先走：
        - `SECPKG_ATTR_CIPHER_INFO`
        - `dwCipherSuite`
      - 当 Schannel 可返回真实 suite name 时：
        - `DoGetCipherName` / `GetConnectionInfo.CipherSuite` 会优先对齐该 truth
    - 当前 focused proof 已覆盖：
      - `bash tests/scripts/test_winssl_connectioninfo_cipher_truth_contract.sh`
      - `tests/test_connection_builder_hostname_precedence.pas`
      - `tests/test_openssl_connection_info_cipher_contract.pas`
      - GitHub Actions `Wave B B2 Manual Gate (Template)` run `26019296095`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` implementation-completeness 主线更准确地收缩到：
      - `MacSize`
      - 无法只靠名字或统一 low-level helper 稳定归一的更细平台差异
      - 更强 owner / deprecation wording route
39. `GetConnectionInfo` `MacSize` semantics matrix 已完成并应作为当前 implementation-completeness 主线的下一条 bounded 收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-getconnectioninfo-macsize-semantics-matrix.md`
    - 当前已确认的 shared + backend truth：
      - shared `GetConnectionInfo` 现在会对可识别 AEAD suite name best-effort 推导：
        - `...GCM` / `...POLY1305` / `...OCB` / `...CCM` -> `MacSize = 16`
        - `...CCM_8` -> `MacSize = 8`
      - OpenSSL / FreePascal / MbedTLS / WolfSSL 当前都已通过 shared path 吃到这组统一 truth
      - WinSSL `GetConnectionInfo` 现在会先走 inherited shared path
      - WinSSL 只有在 shared path 仍未给出稳定值时，才回退：
        - `ConnInfo.dwHashStrength div 8`
    - 当前 focused proof 已覆盖：
      - `bash tests/scripts/test_winssl_connectioninfo_cipher_truth_contract.sh`
      - `bash tests/scripts/test_winssl_connectioninfo_macsize_semantics_contract.sh`
      - `tests/test_connection_builder_hostname_precedence.pas`
      - `tests/test_openssl_connection_info_cipher_contract.pas`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` implementation-completeness 主线已进一步收缩到：
      - legacy non-AEAD `MacSize` 是否值得补更强 low-level truth
      - 无法只靠 shared suite-name 路径稳定归一的更细平台差异
      - 更强 owner / deprecation wording route
40. `OpenSSL GetConnectionInfo legacy MacSize truth` 已完成并应作为当前 implementation-completeness 主线的进一步收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-openssl-connectioninfo-macsize-legacy-truth-feasibility.md`
    - 当前已确认的 OpenSSL truth：
      - `TOpenSSLConnection.GetConnectionInfo` 现在在 shared path 已无 `MacSize` 且 cipher 明确 non-AEAD 时，会使用：
        - `SSL_CIPHER_get_digest_nid`
        - `EVP_get_digestbynid`
        - `EVP_MD_size`
      - AEAD cipher 继续保持 shared `MacSize` owner truth，不会被 digest size 覆盖
      - `api.ssl` 与 `api.evp` 的 active export/binding chain 现在已经补齐：
        - `SSL_CIPHER_is_aead`
        - `SSL_CIPHER_get_digest_nid`
        - `EVP_get_digestbynid`
    - 当前 focused proof 已覆盖：
      - `bash tests/scripts/test_openssl_connectioninfo_macsize_truth_contract.sh`
      - `tests/test_openssl_connection_info_cipher_contract.pas`
      - `tests/test_connection_builder_hostname_precedence.pas`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` implementation-completeness 主线已进一步收缩到：
      - WinSSL / MbedTLS / WolfSSL 是否存在值得接入的更强 legacy `MacSize` truth
      - 无法只靠 shared or current low-level helpers 稳定归一的更细平台差异
      - 更强 owner / deprecation wording route
41. `WolfSSL GetConnectionInfo legacy MacSize truth` 已完成并应作为当前 implementation-completeness 主线的进一步收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-wolfssl-connectioninfo-macsize-legacy-truth-feasibility.md`
    - 当前已确认的 WolfSSL truth：
      - `TWolfSSLConnection.GetConnectionInfo` 现在会先走 inherited shared path
      - 仅当 shared path 仍未给出 `MacSize` 时，才回退：
        - `wolfSSL_GetHmacSize(FWolfSSL)`
      - shared AEAD `MacSize` 继续保持 owner truth，不会被 backend helper 覆盖
      - `wolfssl.api` 的 active export/binding chain 现在已经补齐：
        - `wolfSSL_GetHmacSize`
    - 当前 focused proof 已覆盖：
      - `bash tests/scripts/test_wolfssl_connectioninfo_macsize_truth_contract.sh`
      - `tests/test_wolfssl_connection_info_macsize_contract.pas`
      - `tests/test_connection_builder_hostname_precedence.pas`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` implementation-completeness 主线已进一步收缩到：
      - MbedTLS 是否存在值得接入的更强 legacy `MacSize` truth
      - 若收益不高，是否切回更强 owner / deprecation wording route
42. `MbedTLS GetConnectionInfo ciphersuite truth` 已完成并应作为当前 implementation-completeness 主线的进一步收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-mbedtls-connectioninfo-ciphersuite-truth-feasibility.md`
    - 当前已确认的 MbedTLS truth：
      - `TMbedTLSConnection.GetConnectionInfo` 现在会优先走：
        - `mbedtls_ssl_get_ciphersuite_id_from_ssl`
      - direct helper 不可用时，会回退到：
        - `mbedtls_ssl_get_ciphersuite`
        - `mbedtls_ssl_get_ciphersuite_id`
      - ciphersuite info 现在会补齐：
        - `CipherSuiteId`
        - `KeySize`
        - legacy/non-AEAD `MacSize`
      - shared AEAD `MacSize` 继续保持 owner truth，不会被 digest size 覆盖
      - shared parser 现在也额外接受：
        - `TLS-RSA-...`
        - `AES-128[-GCM]`
        - `AES-256[-GCM]`
      - `mbedtls.base` 的 `MBEDTLS_MD_SHA1` / `MBEDTLS_MD_RIPEMD160` 常量真相也已修正
    - 当前 focused proof 已覆盖：
      - `bash tests/scripts/test_mbedtls_connectioninfo_ciphersuite_truth_contract.sh`
      - `tests/test_mbedtls_connection_info_ciphersuite_contract.pas`
      - `tests/test_connection_builder_hostname_precedence.pas`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` implementation-completeness 主线已进一步收缩到：
      - 是否需要对这条 route 做一次 completion audit
      - FreePascal 是否还有必须单独补的 low-level truth
      - 若没有新的高价值实现缺口，是否切回更强 owner / deprecation wording route
43. `FreePascal GetConnectionInfo completion audit` 已完成并应作为当前 implementation-completeness 主线的正式收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-freepascal-getconnectioninfo-completion-audit.md`
    - 当前已确认的 FreePascal truth：
      - `TFreePascalConnection` 没有 dedicated `GetConnectionInfo` override
      - 当前 backend 只额外提供：
        - `DoGetConnectionInfoServerName`
      - client / server TLS 1.3 runtime path 都会把 negotiated suite truth 写成：
        - `FCipherName := TLS13CipherSuiteToString(...)`
      - session / resumption path 继续保留：
        - `FCipherSuite: Word`
      - shared `GetConnectionInfo` 已能对这组标准 suite-name truth 补齐：
        - `CipherSuiteId`
        - `Hash`
        - `KeySize`
        - `MacSize`
    - 当前 focused proof 已覆盖：
      - `bash tests/scripts/test_freepascal_connectioninfo_completion_contract.sh`
      - `tests/test_freepascal_server_accept_skeleton.pas`
      - `tests/test_freepascal_client_session_resumption.pas`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` implementation-completeness 主线已可视为基本完成：
      - 不再默认继续往 backend 里盲补 low-level helper
      - 下一步应先切回 route-level completion audit / next-route selection
      - 默认主线回到更强 owner / deprecation wording route
44. `GetConnectionInfo contract owner primacy` 已完成并应作为当前 owner/mirror route 的正式收紧保留：
    - 新 plan：
      - `docs/plans/2026-05-18-getconnectioninfo-contract-owner-primacy.md`
    - 当前已确认的 route truth：
      - `Contract 19` 现在先验证：
        - `ISSLConnectionInfo.GetConnectionInfo`
      - 再验证：
        - `ISSLConnection.GetConnectionInfo`
          只是 v1.x compatibility-core mirror
      - 新 completeness / proof tests 已不再默认走 direct core getter：
        - FreePascal server / session-resumption proof
        - OpenSSL cipher contract
        - WolfSSL MacSize contract
        - MbedTLS ciphersuite contract
        - shared builder proof
      - residual direct-core `GetConnectionInfo` surface 现在只剩 5 个命中：
        - `tests/contract/test_backend_contract.pas`
        - `tests/winssl/test_winssl_connection_info.pas`
        - `tests/winssl/test_winssl_connection_edge_cases.pas`
    - 当前 focused proof 已覆盖：
      - `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_contract_owner_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
      - `tests/contract/test_backend_contract.pas`
      - `tests/test_connection_builder_hostname_precedence.pas`
      - `tests/test_freepascal_server_accept_skeleton.pas`
      - `tests/test_freepascal_client_session_resumption.pas`
      - `tests/test_mbedtls_connection_info_ciphersuite_contract.pas`
      - `tests/test_openssl_connection_info_cipher_contract.pas`
      - `tests/test_wolfssl_connection_info_macsize_contract.pas`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` route 的默认下一步应为：
      - 更强 owner / deprecation wording route
      - 或判定剩余 WinSSL direct-core tests 是否属于 intentional core-surface proof
      - 不再继续把普通 completeness proof 留在 direct core getter 上
45. `GetConnectionInfo` WinSSL direct-core classification 已完成并应作为当前 residual route 的最终定性保留：
    - 新 plan：
      - `docs/plans/2026-05-18-getconnectioninfo-winssl-direct-core-classification.md`
    - 当前已确认的 route truth：
      - WinSSL residual direct-core `GetConnectionInfo` file set 已稳定收缩到：
        - `tests/winssl/test_winssl_connection_info.pas`
        - `tests/winssl/test_winssl_connection_edge_cases.pas`
      - 它们当前都已显式标记为：
        - `INTENTIONAL_CORE_SURFACE`
      - 这说明剩余 WinSSL direct-core 面属于 intentional core-surface proof，
        不是遗漏迁移的普通 completeness test
    - 当前 focused proof 已覆盖：
      - `bash tests/scripts/test_getconnectioninfo_winssl_direct_core_classification_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` route 的默认下一步应为：
      - 更强 owner / deprecation wording route
      - 不再继续停留在 residual classification 清扫
46. `GetConnectionInfo` public wording de-emphasis 已完成并应作为当前 source/doc owner truth 对齐的正式收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-getconnectioninfo-public-wording-deemphasis.md`
    - 当前已确认的 route truth：
      - `src/fafafa.ssl.base.pas`
        现在明确写出：
        - 默认 owner 为 `ISSLConnectionInfo.GetConnectionInfo`
        - `ISSLConnection.GetConnectionInfo` 仅兼容保留，不再作为新代码 primary entry
      - `docs/reference/API_REFERENCE.md`
        现在在声明、示例、结构说明三处统一同一叙事
      - `docs/reference/INTERFACE_DESIGN_V2.md`
        不再只写“仍然存在”，而是明确把 `GetConnectionInfo` 视为 compatibility mirror
    - 当前 focused proof 已覆盖：
      - `bash tests/scripts/test_getconnectioninfo_public_wording_deemphasis_contract.sh`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` route 的默认下一步应为：
      - 第一条真正的 public slimming slice feasibility selection
      - 不再重复做 wording / residual classification 清扫
47. `GetConnectionInfo` compiler deprecation alignment 已完成并应作为当前第一条 public slimming slice 的正式收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-getconnectioninfo-compiler-deprecation-alignment.md`
    - 当前已确认的 route truth：
      - `src/fafafa.ssl.base.pas`
        现在把 `ISSLConnection.GetConnectionInfo` 声明成：
        - `deprecated 'Use ISSLConnectionInfo.GetConnectionInfo'`
      - `docs/reference/API_REFERENCE.md`
        与 `docs/reference/INTERFACE_DESIGN_V2.md`
        现在都明确记录：
        - core getter 仅兼容保留
        - 当前源码声明已经是编译期 deprecated
      - residual intentional direct-core tests 已带局部 warning quarantine：
        - `tests/contract/test_backend_contract.pas`
        - `tests/winssl/test_winssl_connection_info.pas`
        - `tests/winssl/test_winssl_connection_edge_cases.pas`
    - 当前 focused proof 已覆盖：
      - `bash tests/scripts/test_getconnectioninfo_compiler_deprecated_contract.sh`
      - `bash tests/scripts/test_getconnectioninfo_public_wording_deemphasis_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
      - `tests/contract/test_backend_contract.pas`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` route 的默认下一步应为：
      - 切到下一条 mirror 的 feasibility / slimming 选择
      - 不再重复做这条 getter 的 wording / deprecation 清扫
48. `GetContext` compiler deprecation alignment 已完成并应作为当前第一条 public slimming slice 的正式收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-getcontext-compiler-deprecation-alignment.md`
    - 当前已确认的 route truth：
      - `src/fafafa.ssl.base.pas`
        现在把 `ISSLConnection.GetContext` 声明成：
        - `deprecated 'Use ISSLConnectionInfo.GetContext'`
      - `docs/reference/API_REFERENCE.md`
        与 `docs/reference/INTERFACE_DESIGN_V2.md`
        现在都明确记录：
        - core getter 仅兼容保留
        - 当前源码声明已经是编译期 deprecated
      - residual direct-core mirror proof 已带局部 warning quarantine：
        - `tests/contract/test_backend_contract.pas`
    - 当前 focused proof 已覆盖：
      - `bash tests/scripts/test_getcontext_compiler_deprecated_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getcontext_active_guidance_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getcontext_contract_owner_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getcontext_source_class_split_contract.sh`
      - `tests/contract/test_backend_contract.pas`
      - `git diff --check`
    - 当前批收口后，`GetContext` route 的默认下一步应为：
      - 切到下一条 mirror 的 feasibility / slimming 选择
      - 不再重复做这条 getter 的 wording / deprecation 清扫
49. `GetStateString` compiler deprecation alignment 已完成并应作为当前下一条 public slimming slice 的正式收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-getstatestring-compiler-deprecation-alignment.md`
    - 当前已确认的 route truth：
      - `src/fafafa.ssl.base.pas`
        现在把 `ISSLConnection.GetStateString` 声明成：
        - `deprecated 'Use ISSLConnectionInfo.GetStateString'`
      - `docs/reference/API_REFERENCE.md`
        与 `docs/reference/INTERFACE_DESIGN_V2.md`
        现在都明确记录：
        - core getter 仅兼容保留
        - 当前源码声明已经是编译期 deprecated
      - residual direct-core proofs 已带局部 warning quarantine：
        - `tests/contract/test_backend_contract.pas`
        - `tests/openssl/test_openssl_server_ocsp_stapling_runtime.pas`
        - `tests/wolfssl/test_wolfssl_server_ocsp_stapling_runtime.pas`
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_getstatestring_compiler_deprecated_contract.sh`
      - `bash tests/scripts/test_getstatestring_compiler_deprecated_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getstatestring_active_test_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getstatestring_residual_classification_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_active_guidance_contract.sh`
      - `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
      - `git diff --check`
    - 当前批收口后，`GetStateString` route 的默认下一步应为：
      - 切到下一条 mirror 的 feasibility / slimming 选择
      - 不再重复做这条 getter 的 wording / deprecation 清扫
50. `GetSelectedALPNProtocol` compiler deprecation alignment 已完成并应作为当前下一条 public slimming slice 的正式收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-getselectedalpn-compiler-deprecation-alignment.md`
    - 当前已确认的 route truth：
      - `src/fafafa.ssl.base.pas`
        现在把 `ISSLConnection.GetSelectedALPNProtocol` 声明成：
        - `deprecated 'Use ISSLConnectionInfo.GetSelectedALPNProtocol'`
      - `docs/reference/API_REFERENCE.md`
        与 `docs/reference/INTERFACE_DESIGN_V2.md`
        现在都明确记录：
        - core getter 仅兼容保留
        - 当前源码声明已经是编译期 deprecated
      - residual direct-core proofs 已带局部 warning quarantine：
        - `tests/contract/test_backend_contract.pas`
        - `tests/mbedtls/test_mbedtls_alpn.pas`
        - `tests/winssl/test_winssl_alpn_sni.pas`
        - `tests/winssl/test_winssl_connection_edge_cases.pas`
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_getselectedalpn_compiler_deprecated_contract.sh`
      - `bash tests/scripts/test_getselectedalpn_compiler_deprecated_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getselectedalpn_active_test_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getselectedalpn_residual_classification_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_active_guidance_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_migration_targets_contract.sh`
      - `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
      - `git diff --check`
    - 当前批收口后，`GetSelectedALPNProtocol` route 的默认下一步应为：
      - 从 mirrors wording/compiler 治理线切回 interface-design completeness / implementation-completeness 主线
      - 不再重复做这条 getter 的 wording / deprecation 清扫
51. `ISSLDiagnostics` active-guidance de-emphasis 已完成并应作为当前 optional-owner surface 的下一条普通路径收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-issldiagnostics-active-guidance-deemphasis.md`
    - 当前已确认的 route truth：
      - `docs/reference/API_REFERENCE.md`
        的普通 diagnostics examples 现在统一优先走：
        - `ISSLDiagnostics.IsHealthy`
        - `ISSLDiagnostics.GetHealthStatus`
        - `ISSLDiagnostics.GetPerformanceMetrics`
        - `ISSLDiagnostics.GetDiagnosticInfo`
      - `tests/test_sslctxboth_roleless_handshake_clarification.pas`
        现在先验证 `Supports(LConn, ISSLDiagnostics, LDiag)`，再读取 diagnostics owner path
      - WinSSL diagnostics runtime tests 继续保留为 backend-specific residual proof，不属于这批收口范围
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_issldiagnostics_active_guidance_contract.sh`
      - `bash tests/scripts/test_issldiagnostics_active_guidance_contract.sh`
      - `mkdir -p tmp/test_sslctxboth_roleless_handshake_clarification && fpc -B -Fu./src -Fu./tests -FUtmp/test_sslctxboth_roleless_handshake_clarification -FEtmp/test_sslctxboth_roleless_handshake_clarification -otmp/test_sslctxboth_roleless_handshake_clarification/test_sslctxboth_roleless_handshake_clarification tests/test_sslctxboth_roleless_handshake_clarification.pas && ./tmp/test_sslctxboth_roleless_handshake_clarification/test_sslctxboth_roleless_handshake_clarification`
      - `git diff --check`
    - 当前批收口后默认下一步应为：
      - 继续盘点下一个 ordinary guidance 仍偏 core 的 optional-owner surface
      - 或切回更大的 interface-design completeness 选择
52. `ISSLCertificateVerification` active-guidance de-emphasis 已完成并应作为当前 optional-owner surface 的下一条普通路径收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-isslcertificateverification-active-guidance-deemphasis.md`
    - 当前已确认的 route truth：
      - `docs/INTEGRATION_GUIDE.md`
        的握手失败示例与排错条目现在统一优先走：
        - `ISSLCertificateVerification.GetVerifyResult`
        - `ISSLCertificateVerification.GetVerifyResultString`
      - `docs/reference/API_DOCUMENTATION.md`
        的 CT 示例失败路径现在也统一优先走：
        - `ISSLCertificateVerification.GetVerifyResultString`
      - `tests/integration/test_cross_backend_consistency_contract.pas`
        与 `tests/integration/test_cross_backend_errors_contract.pas`
        现在都通过 helper 改走 `ISSLCertificateVerification` owner path
      - backend-specific certificate-verification runtime tests 继续保留为 residual proof，不属于这批收口范围
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslcertificateverification_active_guidance_contract.sh`
      - `bash tests/scripts/test_isslcertificateverification_active_guidance_contract.sh`
      - `mkdir -p tmp/test_cross_backend_consistency_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_consistency_contract -FEtmp/test_cross_backend_consistency_contract -otmp/test_cross_backend_consistency_contract/test_cross_backend_consistency_contract tests/integration/test_cross_backend_consistency_contract.pas && ./tmp/test_cross_backend_consistency_contract/test_cross_backend_consistency_contract`
      - `mkdir -p tmp/test_cross_backend_errors_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_errors_contract -FEtmp/test_cross_backend_errors_contract -otmp/test_cross_backend_errors_contract/test_cross_backend_errors_contract tests/integration/test_cross_backend_errors_contract.pas && ./tmp/test_cross_backend_errors_contract/test_cross_backend_errors_contract`
      - `git diff --check`
    - 当前批收口后默认下一步应为：
      - 继续盘点下一个 ordinary guidance 仍偏 core 的 optional-owner surface
      - 或切回更大的 interface-design completeness 选择
53. `ISSLSessionResumption` active-guidance de-emphasis 已完成并应作为当前 optional-owner surface 的下一条普通路径收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-isslsessionresumption-active-guidance-deemphasis.md`
    - 当前已确认的 route truth：
      - `docs/reference/API_REFERENCE.md`
        的 session-resumption / WinSSL session 示例现在统一优先走：
        - `ISSLSessionResumption.GetSession`
        - `ISSLSessionResumption.SetSession`
        - `ISSLSessionResumption.IsSessionReused`
      - `docs/reference/API_DOCUMENTATION.md`
        的会话缓存 / 性能问题示例现在先 capability-gate：
        - `Supports(Connection, ISSLSessionResumption, SessionResumption)`
      - `docs/INTEGRATION_GUIDE.md`
        的 resumed-session + early-data 例子现在先验证：
        - `Supports(InitialStream.Connection, ISSLSessionResumption, Resumption)`
      - `tests/integration/test_e2e_scenarios.pas`
        不再把 `Conn1.GetSession / Conn2.SetSession / Conn2.IsSessionReused`
        当普通读取/写入路径
      - backend-specific session runtime / benchmark proof 继续保留为 residual proof，不属于这批收口范围
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslsessionresumption_active_guidance_contract.sh`
      - `bash tests/scripts/test_isslsessionresumption_active_guidance_contract.sh`
      - `mkdir -p tmp/test_e2e_scenarios && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_e2e_scenarios -FEtmp/test_e2e_scenarios -otmp/test_e2e_scenarios/test_e2e_scenarios tests/integration/test_e2e_scenarios.pas && ./tmp/test_e2e_scenarios/test_e2e_scenarios`
      - `git diff --check`
    - 当前批收口后默认下一步应为：
      - 优先盘点 `ISSLOCSPStapling` ordinary guidance 是否仍在 direct core `GetOCSP*` 路径上漂移
      - 不再重复拉起 session-resumption active-guidance 清扫
54. `ISSLOCSPStapling` active-guidance de-emphasis 已完成并应作为当前 optional-owner surface 的下一条普通路径收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-isslocspstapling-active-guidance-deemphasis.md`
    - 当前已确认的 route truth：
      - `docs/reference/API_DOCUMENTATION.md`
        的 ordinary OCSP method examples 现在统一优先走：
        - `ISSLOCSPStapling.GetOCSPStaplingEnabled`
        - `ISSLOCSPStapling.GetOCSPResponse`
        - `ISSLOCSPStapling.IsOCSPResponseVerified`
        - `ISSLOCSPStapling.GetOCSPResponseStatus`
      - 同一文档现在明确把：
        - `Connection.GetOCSP*`
        标成 compatibility-core mirrors，而不是新代码推荐路径
      - backend-specific OCSP runtime / contract proof 继续保留为 residual proof，不属于这批收口范围
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslocspstapling_active_guidance_contract.sh`
      - `bash tests/scripts/test_isslocspstapling_active_guidance_contract.sh`
      - `git diff --check`
    - 当前批收口后默认下一步应为：
      - 不再重复拉起 optional-owner ordinary-guidance 清扫
      - 切回更大的 interface-design completeness / implementation-completeness 审查

55. `WinSSL session capability/docs truth alignment` 已完成并应作为当前 WinSSL session-resumption lane 的最新 public truth 基线保留：
    - 新 plan：
      - `docs/plans/2026-05-18-winssl-session-capability-truth-alignment.md`
    - 当前已确认的 route truth：
      - `src/fafafa.ssl.winssl.lib.pas`
        现在继续保留：
        - `SessionCacheSupport := sslSupportStable`
        - `SupportsSessionTickets := True`
        但已经把：
        - `SessionTicketsSupport`
          收紧到 `sslSupportExperimental`
        - `KnownIssues`
          显式写入当前 dedicated Windows runtime truth：
          - `observed_reuse=false`
          - `session_configured=true`
      - `docs/reference/API_REFERENCE.md`
      - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
      - `docs/reference/WINSSL_PERFORMANCE_TUNING.md`
      - `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
        现在都已统一收紧到：
        - public surface 存在
        - shared crash 已关闭
        - native resumed-handshake 仍未被当前 GitHub Windows proof 证实
      - WinSSL performance/session 示例也已经统一优先走：
        - `ISSLSessionResumption.GetSession`
        - `ISSLSessionResumption.SetSession`
        - `ISSLSessionResumption.IsSessionReused`
        不再混回 direct core `GetSession` / `SetSession` / `IsSessionResumed`
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_winssl_capability_source_contract.sh`
      - `bash tests/scripts/test_winssl_capability_source_contract.sh`
      - `bash -n tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh`
      - `bash tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh`
      - `bash -n tests/scripts/test_isslsessionresumption_active_guidance_contract.sh`
      - `bash tests/scripts/test_isslsessionresumption_active_guidance_contract.sh`
      - `mkdir -p tmp/winssl_session_capability_truth_win64 && fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/winssl_session_capability_truth_win64 -FEtmp/winssl_session_capability_truth_win64 -otmp/winssl_session_capability_truth_win64/test_winssl_session_resumption.exe tests/winssl/test_winssl_session_resumption.pas`
      - `git diff --check`
    - 当前批收口后默认下一步应为：
      - 不再重开 capability/docs truth alignment 或 shared-crash proof lane
      - 直接进入 WinSSL backend native resumed-handshake / session tickets 行为调查
      - 或切回更大的 backend implementation completeness 横向审查
56. `WinSSL session cache runtime flag alignment` 已完成并应作为当前 WinSSL context-level session-control truth 基线保留：
    - 新 plan：
      - `docs/plans/2026-05-18-winssl-session-cache-runtime-flag-alignment.md`
    - 当前已确认的 route truth：
      - `src/fafafa.ssl.winssl.context.pas`
        当前 context-level `CredHandle` 仍是 WinSSL reconnect/runtime 的 canonical carrier
      - `SetSessionCacheMode(...)`
        不再只是改 `FSessionCacheEnabled`，现在会显式触发 `FCredentialsNeedRebuild := True`
      - `SetOptions(...)`
        不再只是改 `FOptions`，现在会在 session/ticket-related option 变化后显式触发 credential rebuild
      - `EnsureCredentialsAcquired`
        现在会在 server-side disable truth 下使用 `SCH_CRED_DISABLE_RECONNECTS`
      - client-side reconnect truth 当前重新收紧为：
        - same `target name`
        - same context-level `credential handle`
      - 这说明 WinSSL 的 `session cache / session tickets` context surface 已不再只是 Pascal-level bookkeeping，而是开始真实影响 Schannel credential acquisition
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_winssl_session_cache_runtime_flag_contract.sh`
      - `bash tests/scripts/test_winssl_session_cache_runtime_flag_contract.sh`
      - `mkdir -p tmp/winssl_session_cache_runtime_flag_win64 && fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/winssl_session_cache_runtime_flag_win64 -FEtmp/winssl_session_cache_runtime_flag_win64 -otmp/winssl_session_cache_runtime_flag_win64/test_winssl_session_resumption.exe tests/winssl/test_winssl_session_resumption.pas`
      - `git diff --check`
    - 当前批收口后默认下一步应为：
      - 在这个新的 context/runtime 基线上继续追 native resumed-handshake 观测
      - 优先调查为什么 current Windows proof 仍停在 `observed_reuse=false`
      - 不再把 session cache / ticket option runtime wiring 当成未知缺口重复拉起
58. `WinSSL client reconnect truth alignment` 已完成并应作为当前 WinSSL native resumed-handshake 调查的最新上游基线保留：
    - 新 plan：
      - `docs/plans/2026-05-18-winssl-client-reconnect-truth-alignment.md`
    - 当前已确认的 route truth：
      - `SCH_CRED_DISABLE_RECONNECTS` 在 `SCHANNEL_CRED` 上当前只保留 server-side truth，不再直接挂到 client credential path
      - client-side Schannel reconnect/cache lookup 当前更准确的 canonical truth 是：
        - same `target name`
        - same context-level `credential handle`
        - same process / logon session
      - `ISSLSessionResumption.SetSession(...)` 在 WinSSL 上当前更接近 compatibility metadata surface，而不是 native session-handle injection 点
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_winssl_session_cache_runtime_flag_contract.sh`
      - `bash tests/scripts/test_winssl_session_cache_runtime_flag_contract.sh`
      - `mkdir -p tmp/winssl_client_reconnect_truth_win64 && fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/winssl_client_reconnect_truth_win64 -FEtmp/winssl_client_reconnect_truth_win64 -otmp/winssl_client_reconnect_truth_win64/test_winssl_session_resumption.exe tests/winssl/test_winssl_session_resumption.pas`
      - `git diff --check`
    - 当前批收口后默认下一步应为：
      - 继续在“same target name + same credential handle”这个真实模型上调查 Windows runtime 为何仍然 `observed_reuse=false`
      - 不再把 `SetSession(...)` 当成 WinSSL native reconnect 的直接注入点
      - 不再把 server-only `SCH_CRED_DISABLE_RECONNECTS` 错挂回 client path
      - 最新 follow-up 已切到 `docs/plans/2026-05-18-winssl-native-probe-evidence-lane.md`：
        - dedicated proof 程序现在会单独记录 `native_probe` markers
        - summary 会分开记录 `observed_reuse` 与 `native_observed_reuse`
        - GitHub Windows live run `26042437486` 已证明这条 probe 默认开启会触发 `-1073741819`
        - 因而当前默认 broader suite lane 需要先把 native probe 降为 opt-in evidence
57. `WinSSL session serialization roundtrip alignment` 已完成并应作为当前 WinSSL session-object completeness 基线保留：
    - 新 plan：
      - `docs/plans/2026-05-18-winssl-session-serialization-roundtrip-alignment.md`
    - 当前已确认的 route truth：
      - `src/fafafa.ssl.winssl.connection.pas` 中的 `TWinSSLSession`
        现在不再只是：
        - `Serialize -> FSessionData`
        - `Deserialize -> FSessionData := AData`
        这种空壳实现
      - `TWinSSLSession` 现在已经具备：
        - `BuildSerializedSessionData`
        - `TryLoadSerializedSessionData`
        两个 helper，用于 round-trip：
        - `ID`
        - `creation time`
        - `timeout`
        - `protocol`
        - `cipher`
        - `resumed flag`
      - `SetSessionMetadata(...)` 与 `SetTimeout(...)`
        现在也会同步刷新 serialized payload，不再让 `Serialize` 吐出 stale bytes
      - 这说明 WinSSL `ISSLSession` 的 serialization surface 现在至少对自身 metadata 自洽；
        但它仍不等于 native resumed-handshake 已经能靠 serialized payload 直接恢复
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_winssl_session_serialization_roundtrip_contract.sh`
      - `bash tests/scripts/test_winssl_session_serialization_roundtrip_contract.sh`
      - `mkdir -p tmp/test_session_metadata_win64 && fpc -Twin64 -Fu./src -Fu./tests -FUtmp/test_session_metadata_win64 -FEtmp/test_session_metadata_win64 -otmp/test_session_metadata_win64/test_session_metadata.exe tests/winssl/test_session_metadata.pas`
      - `git diff --check`
    - 当前批收口后默认下一步应为：
      - 不再把 WinSSL session serialization surface 当成“基本空壳”重复拉起
      - 继续回到 native resumed-handshake / Windows runtime 观测主线
      - 或转向其他 backend 的 session object completeness 横向审查
59. `MbedTLS/WolfSSL c-library session serialization truth` 已完成 focused 收口，并应作为当前 session-object completeness 新基线保留：
    - 新 plan：
      - `docs/plans/2026-05-19-clibrary-session-serialization-truth-alignment.md`
    - 当前已确认的 route truth：
      - `src/fafafa.ssl.mbedtls.api.pas`
        已正式绑定：
        - `mbedtls_ssl_session_load`
        - `mbedtls_ssl_session_save`
      - `src/fafafa.ssl.mbedtls.session.pas`
        不再把 `Deserialize(...)` 实现成“只缓存传入字节”
      - `TMbedTLSSession.Deserialize(...)`
        在 helper 缺失时现在明确 `fail-closed`
      - `TMbedTLSSession.Serialize(...)`
        现在优先通过 native helper 生成真实 payload，而不是回放 stale cached bytes
      - `src/fafafa.ssl.wolfssl.session.pas`
        在 `wolfSSL_d2i_SSL_SESSION` 缺失时也改为 `fail-closed`
      - 这说明当前 c-library backend session surface 的最小真相已经重新对齐为：
        - 有 native helper 才承认 deserialize/serialize
        - 没有 helper 时公开返回失败，而不是制造“假成功”
    - 当前 focused proof 已覆盖：
      - `mkdir -p tmp/test_mbedtls_framework_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_mbedtls_framework_units -FEtmp/test_mbedtls_framework_units -otmp/test_mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas && ./tmp/test_mbedtls_framework_units/test_mbedtls_framework`
      - `mkdir -p tmp/test_wolfssl_framework_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_framework_units -FEtmp/test_wolfssl_framework_units -otmp/test_wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas && ./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
      - `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
      - `git diff --check`
    - 当前批收口后默认下一步应为：
      - 横向继续审 `Clone()` / metadata/native-handle ownership 语义
      - 不再把 MbedTLS/WolfSSL session serialization surface 当成“helper 缺失也能成功”的未定位问题重复拉起
60. `MbedTLS/WolfSSL c-library session clone truth` 已完成 focused 收口，并应作为当前 session-object completeness 新基线保留：
    - 新 plan：
      - `docs/plans/2026-05-19-clibrary-session-clone-truth-alignment.md`
    - 当前已确认的 route truth：
      - `TMbedTLSSession.Clone()`
        不再把 valid session 克隆成 `FSession=nil` 的 metadata shell
      - `TWolfSSLSession.Clone()`
        现在也会保留 valid/resumable/native-handle truth
      - `TWolfSSLSession.Serialize()`
        当前优先输出 native `i2d` bytes，而不是先回放 stale cached bytes
      - 这说明当前 c-library backend session clone surface 的最小真相已经重新对齐为：
        - clone 后仍保留可用 session object
        - valid session 不会因为 clone 而被降级成 invalid shell
    - 当前 focused proof 已覆盖：
      - `mkdir -p tmp/test_mbedtls_framework_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_mbedtls_framework_units -FEtmp/test_mbedtls_framework_units -otmp/test_mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas && ./tmp/test_mbedtls_framework_units/test_mbedtls_framework`
      - `mkdir -p tmp/test_wolfssl_framework_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_framework_units -FEtmp/test_wolfssl_framework_units -otmp/test_wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas && ./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
      - `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
      - `git diff --check`
    - 当前批收口后默认下一步应为：
      - 横向继续审 `FromContext/FromConnection` ownership 与 source-lifetime 边界
      - 不再把 MbedTLS/WolfSSL session clone surface 当成“valid clone 会失效”的未定位问题重复拉起
61. `WolfSSL session source-lifetime truth` 已完成 focused 收口，并应作为当前 session-extraction 新基线保留：
    - 新 plan：
      - `docs/plans/2026-05-19-wolfssl-session-source-lifetime-truth-alignment.md`
    - 当前已确认的 route truth：
      - `OpenSSL.DoGetSession()`
        当前仍通过 `SSL_get1_session` secure ownership
      - `MbedTLS.FromContext()`
        当前仍通过 `mbedtls_ssl_get_session` 复制到独立 session 存储
      - `WolfSSL.FromConnection()`
        之前是直接包 `wolfSSL_get_session()` 返回的 borrowed handle
      - `TWolfSSLSession.FromConnection()`
        现在会先 secure ownership：
        - 优先 `wolfSSL_SESSION_dup`
        - 否则退到 `i2d/d2i` duplication
        - 如果 ownership 无法保障则 `fail-closed`
      - 这说明当前真正存在 lifetime 漂移的点已经从“泛化怀疑 c-library session 提取”收缩成“WolfSSL 已修，OpenSSL/MbedTLS 当前无同类硬缺口”
    - 当前 focused proof 已覆盖：
      - `mkdir -p tmp/test_wolfssl_framework_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_framework_units -FEtmp/test_wolfssl_framework_units -otmp/test_wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas && ./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
      - `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
      - `git diff --check`
    - 当前批收口后默认下一步应为：
      - 横向继续审 `GetPeerCertificate` / metadata extraction completeness
      - 不再把 WolfSSL source-session lifetime gap 当成未定位问题重复拉起
62. `WolfSSL certificate clone materialization` 已完成 focused 收口，并应作为当前 certificate-object completeness 新基线保留：
    - 新 plan：
      - `docs/plans/2026-05-19-wolfssl-certificate-clone-materialization.md`
    - 当前已确认的 route truth：
      - `TWolfSSLCertificate.Clone()`
        之前只复制：
        - `FPEMData`
        - `FDERData`
        - `FInfo`
      - 但不会重新 materialize `FX509`
      - 结果 loaded cert clone 后曾出现：
        - native handle 丢失
        - `GetSubject` / `GetIssuer` 退化成 shell truth
        - fingerprint 仍可能继续来自缓存 DER
      - 当前修复后：
        - clone 会优先拿可用 DER
        - 再 `LoadFromDER(...)` 重建 owned native cert
        - helper 不足时 `fail-closed`
      - 这说明当前 `WolfSSL` certificate clone surface 的最小真相已经重新对齐为：
        - loaded certificate clone 后仍保留可用 native X509
        - public metadata truth 不再因为 clone 而退化
    - 当前 focused proof 已覆盖：
      - `mkdir -p tmp/test_wolfssl_framework_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_framework_units -FEtmp/test_wolfssl_framework_units -otmp/test_wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas && ./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
      - `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
      - `git diff --check`
    - 当前批收口后默认下一步应为：
      - 横向继续审其它 backend 的 certificate clone / connection completeness seam
      - 不再把 WolfSSL loaded-certificate clone shell gap 当成未定位问题重复拉起
63. `WolfSSL connection peer-certificate materialization` 已完成 focused 收口，并应作为当前 connection-level completeness 新基线保留：
    - 新 plan：
      - `docs/plans/2026-05-19-wolfssl-connection-peer-cert-materialization.md`
    - 当前已确认的 route truth：
      - `/usr/include/wolfssl/test.h` 的官方示例对 `wolfSSL_get_peer_certificate(ssl)` 会在使用后显式 `wolfSSL_FreeX509(peer)`
      - 这说明当前问题不在“连接内部 borrowed 指针会立即悬空”
      - 真正的缺口在于：
        - `TWolfSSLConnection.GetPeerCertificate()`
          之前直接返回 native wrapper
        - 但同一 backend 的：
          - `GetPeerCertificateChain()`
          - `TWolfSSLSession.FromConnection()`
          - `TWolfSSLCertificate.Clone()`
          都已经走 owned/materialized truth
      - 当前修复后：
        - `GetPeerCertificate()` 统一改为 `native X509 -> DER export -> owned reload`
        - 返回 cert 不再 alias source native handle
        - copy helper 不足时 `fail-closed`
      - 这说明当前 `WolfSSL` connection single-cert surface 的最小真相已经重新对齐为：
        - public peer cert object 持有自有 native cert
        - helper-loss 时不再继续吐出假完整 wrapper
    - 当前 focused proof 已覆盖：
      - `mkdir -p tmp/test_wolfssl_connection_peer_certificate_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/test_wolfssl_connection_peer_certificate_contract_units -FEtmp/test_wolfssl_connection_peer_certificate_contract_units -otmp/test_wolfssl_connection_peer_certificate_contract_units/test_wolfssl_connection_peer_certificate_contract tests/test_wolfssl_connection_peer_certificate_contract.pas && ./tmp/test_wolfssl_connection_peer_certificate_contract_units/test_wolfssl_connection_peer_certificate_contract`
      - `mkdir -p tmp/test_wolfssl_framework_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_framework_units -FEtmp/test_wolfssl_framework_units -otmp/test_wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas && ./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
      - `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
      - `git diff --check`
    - 当前批收口后默认下一步应为：
      - 横向继续审其它 backend 的 connection-level certificate ownership/completeness seam
      - 不再把 WolfSSL connection single-cert materialization gap 当成未定位问题重复拉起
64. `FreePascal peer-certificate issuer link` 已完成 focused 收口，并应作为当前 connection-level chain-truth 新基线保留：
    - 新 plan：
      - `docs/plans/2026-05-19-freepascal-peer-cert-issuer-link.md`
    - 当前已确认的 route truth：
      - `ISSLCertificate` 公共接口明确暴露：
        - `SetIssuerCertificate(...)`
        - `GetIssuerCertificate(...)`
      - `TFreePascalConnection` 之前虽然已经构建了：
        - `FPeerCertificateChain`
        - `FPeerCertificate := FPeerCertificateChain[0]`
      - 但没有把 chain 相邻证书之间的 issuer link 接起来
      - 所以曾出现：
        - `GetPeerCertificate()` 返回 leaf cert
        - `GetPeerCertificateChain()` 返回完整 chain
        - 但 leaf 上的 `GetIssuerCertificate()` 仍为空
      - 当前修复后：
        - 构建 `FPeerCertificateChain` 后会显式把 `chain[i].issuer = chain[i+1]`
        - 最后一个 cert 的 issuer link 归零
      - 这说明当前 `FreePascal` connection-level peer cert truth 已重新对齐为：
        - public leaf cert 可以沿 issuer link 继续追到 chain issuer
        - chain leaf 也保留同一条 issuer-link truth
    - 当前 focused proof 已覆盖：
      - `mkdir -p tmp/test_freepascal_client_peer_certificate_surface_units && fpc -B -Fu./src -Fu./tests -FUtmp/test_freepascal_client_peer_certificate_surface_units -FEtmp/test_freepascal_client_peer_certificate_surface_units -otmp/test_freepascal_client_peer_certificate_surface_units/test_freepascal_client_peer_certificate_surface tests/test_freepascal_client_peer_certificate_surface.pas && ./tmp/test_freepascal_client_peer_certificate_surface_units/test_freepascal_client_peer_certificate_surface`
      - `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
      - `git diff --check`
    - 当前批收口后默认下一步应为：
      - 横向审其它 backend 是否仍缺 issuer-link completeness
      - 不再把 FreePascal peer-cert issuer-link gap 当成未定位问题重复拉起
65. `GetVerifyResult` / `GetVerifyResultString` compiler deprecation alignment 已完成 focused 收口，并应作为当前 verify-result route 的最终 compatibility-only closeout 保留：
   - 新 plan：
     - `docs/plans/2026-05-19-getverifyresult-compiler-deprecation-alignment.md`
   - 当前已确认的 route truth：
     - `src/fafafa.ssl.base.pas`
       - `ISSLConnection.GetVerifyResult`
       - `ISSLConnection.GetVerifyResultString`
       当前都已进入 compiler `deprecated`
     - `docs/reference/API_REFERENCE.md`
       - 现在用 `ISSLCertificateVerification owner surface` 记录推荐入口
       - 不再在活跃文档里留下会撞到 residual grep 的 `TypeName.GetVerifyResult*` 字面
     - 当前阻塞根因已被确认不是实现回归，而是：
       - API reference 的点号写法与 residual-classification contract 的 direct-core grep 规则相撞
       - 最小正确修法只是收紧文档 wording 与 focused contract 对齐
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_getverifyresult_compiler_deprecated_contract.sh`
     - `bash tests/scripts/test_getverifyresult_compiler_deprecated_contract.sh`
     - `bash tests/scripts/test_isslcertificateverification_residual_classification_contract.sh`
     - `bash tests/scripts/test_isslcertificateverification_generic_examples_contract.sh`
     - `mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence`
     - `mkdir -p tmp/test_tls_connector_hostname_override_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_tls_connector_hostname_override_precedence -FEtmp/test_tls_connector_hostname_override_precedence -otmp/test_tls_connector_hostname_override_precedence/test_tls_connector_hostname_override_precedence tests/test_tls_connector_hostname_override_precedence.pas && ./tmp/test_tls_connector_hostname_override_precedence/test_tls_connector_hostname_override_precedence`
     - `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 把 `GetVerifyResult*` 这条 verify-result residual archaeology 视为阶段性关闭
     - 重新把注意力切回更大的接口设计 / 各 backend completeness 审查
     - 不再把同一类 verify-result wording / grep 误报当成新的实现问题反复拉起
66. `native-handle / owner-surface truth` 已完成 focused 收口，并应作为当前 interface-design completeness 的 canonical truth 保留：
   - 新 plan：
     - `docs/plans/2026-05-19-native-handle-owner-surface-truth-freeze.md`
   - 当前已确认的 route truth：
     - `src/fafafa.ssl.base.pas`
       - `GetNativeHandle` 当前 owner 是 `ISSLNativeHandleAccess`
       - 它不属于 `ISSLContext` / `ISSLConnection` core surface
     - `docs/reference/API_REFERENCE.md`
       - 之前还把 `GetNativeHandle` 列在 `ISSLContext` code listing 里
     - `docs/reference/INTERFACE_DESIGN_V2.md`
       - 之前还把 `GetNativeHandle` 画进 `ISSLConnection` core
       - 并把 `GetSelectedALPNProtocol` 错画进 `ISSLClientConnection`
     - `tests/connection/test_ssl_connection_local.pas`
       - 真实编译 RED 也已证明 generic smoke 还在按旧 core 假设读 `Connection.GetNativeHandle`
       - 同文件还在普通路径上直读 deprecated `GetConnectionInfo`
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_native_handle_owner_surface_truth_contract.sh`
     - `bash tests/scripts/test_native_handle_owner_surface_truth_contract.sh`
     - `mkdir -p tmp/test_ssl_connection_local_units && fpc -B -Fu./src -Fu./tests -FUtmp/test_ssl_connection_local_units -FEtmp/test_ssl_connection_local_units -otmp/test_ssl_connection_local_units/test_ssl_connection_local tests/connection/test_ssl_connection_local.pas && ./tmp/test_ssl_connection_local_units/test_ssl_connection_local`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 不再把 `GetNativeHandle` owner surface 当成文档/测试层的未定真相
     - 继续回到更大的 interface-design / backend completeness 审查
     - 优先找下一条“活跃 canonical docs / 活跃 generic tests / backend truth”仍互相打架的接口面
67. `Wave B/B2 opt-in runtime failure truth` 已完成 focused 收口，并应作为当前 Windows workflow truth 的最新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-wave-b-b2-opt-in-runtime-failure-truth.md`
   - 当前已确认的 route truth：
     - live GitHub run `26068984446` 中，Windows broader runtime transcript 已明确给出 `suite_end_status=FAIL`
     - 旧版 `generate_wave_b_cross_platform_summary.sh` 只消费 Windows summary，因此会把同批 broader runtime failure 继续写成 `windows PASS`
     - 旧版 `prepare_wave_b_b2_handoff_bundle.sh` 也会在这种情况下继续给出 `handoff_state: CLOSED`
     - 这条问题首先是 workflow/report truth 漏洞，而不是 WinSSL shared implementation 本身已经被修好或应当在同一批里重开
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_wave_b_cross_platform_summary_windows_runtime_fail_contract.sh`
     - `bash tests/scripts/test_wave_b_cross_platform_summary_windows_runtime_fail_contract.sh`
     - `bash -n tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_runtime_fail_contract.sh`
     - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_runtime_fail_contract.sh`
     - `bash tests/scripts/test_wave_b_cross_platform_summary_next_actions_contract.sh`
     - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_gate_repair_state_contract.sh`
     - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`
     - `bash tests/scripts/test_wave_b_b2_consistency_windows_runtime_substantive_contract.sh`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 不再把 `windows PASS` / `handoff_state: CLOSED` 当成 opt-in WinSSL runtime 已通过的可信信号
     - 继续回到 WinSSL-specific native-probe runtime fail seam，直接定位为何 opt-in runtime 在首个 public signal 后以 `-1073741819` 退出
     - 若还要补 workflow truth，下一刀更适合单独审 `check_wave_b_b2_evidence_consistency.sh` 的 next-actions wording，而不是把它和这批混修

## Verification Discipline

- 默认先做静态审查与 focused contract，不重跑整条重型门禁。
- 只有当修复影响行为语义时，才补最小 Pascal/脚本合同验证。
- 每完成一个可闭环小批次，都要同步：
  - `docs/plans/...`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Risks

- 接口设计问题很多是“结构性债务”，不一定适合一批次全部动生产代码。
- capability truth 问题容易横跨文档、selector、serializer、backend source，多处同修但必须保持最小改动。
- 旧 release/runtime 历史记录很多，必须防止这轮再次被历史 closeout 信息带偏。

## Exit Criteria

- 至少形成一份新的综合审查 plan，明确记录范围、证据源、发现与后续队列。
- 至少完成一轮“公共接口 + 各 backend capability/实现”的横向验证。
- 若发现高价值且边界清晰的问题，则完成最小修复与 focused 验证。
- 给出可复用结论：哪些是已确认问题，哪些是设计债，哪些是下一批应继续推进的最优路径。

68. `OpenSSL CT capability truth` 回漂已完成 focused 收口，并应作为当前 capability/public-surface 审查的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-openssl-ct-capability-truth-retightening.md`
   - 当前已确认的 route truth：
     - 默认 `OpenSSL` backend 没有发布 `ISSLCertificateTransparency` / `ISSLCertificateTransparencyValidation` connection surface
     - 之前的真实漂移不是“默认初始化就报错”，而是：
       - 只要 `osmCT` 被其他路径标记成 loaded
       - `src/fafafa.ssl.openssl.backed.pas` 就会把低层 CT binding readiness 错当成 public capability / feature truth
     - 这会直接误导：
       - `IsFeatureSupported(sslFeatCertificateTransparency)`
       - `SupportsCertificateTransparency`
       - `CertTransparencySupport`
       - 以及依赖这些字段的 selector / caller 判断
   - 当前最小正确修法已落地：
     - 不扩写 `TOpenSSLConnection`
     - 不新增 OpenSSL CT optional interface
     - 只把 OpenSSL CT public capability 收紧回：
       - `sslFeatCertificateTransparency = False`
       - `SupportsCertificateTransparency = False`
       - `CertTransparencySupport = sslSupportNone`
     - 并把 `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md` 的 CT 行改成“底层 API 可用性”而非“默认 capability 直接映射”
   - 当前 focused proof 已覆盖：
     - `mkdir -p tmp/test_openssl_features_units && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_features_units -FEtmp/test_openssl_features_units -otmp/test_openssl_features_units/test_openssl_features tests/openssl/test_openssl_features.pas && ./tmp/test_openssl_features_units/test_openssl_features`
     - `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
     - `python3 scripts/compile_all_modules.py`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 继续找下一条“低层 binding readiness 被误抬成 public capability truth”的 backend drift
     - 不再把 OpenSSL CT 这条线按“默认 capability 看起来没问题所以无需处理”重新拉起
69. `hardware-key capability truth` 已完成 focused 收口，并应作为当前 selector/capability 审查的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-hardware-key-capability-truth-tightening.md`
   - 当前已确认的 route truth：
     - `src/fafafa.ssl.openssl.context.pas`
       - 已存在 shipped `LoadPrivateKeyFromPKCS11(...)` 路径
       - `TPKCS11BackendFactory.CreateBackend(btAuto)` 仍是当前真实 PKCS#11 loader bridge
     - `src/fafafa.ssl.openssl.backed.pas`
       - 之前把 `SupportsTPM` 直接写成 `True`
       - 但当前仓库并没有 shipped TPM public/runtime path
     - `src/fafafa.ssl.winssl.lib.pas`
       - 之前把 `SupportsPKCS11` / `SupportsTPM` 都直接写成 `True`
       - 但当前 WinSSL backend 只有系统证书存储 / PFX / DER 等已发布 surface，没有 shipped PKCS#11 URI / TPM loading/runtime path
     - `src/fafafa.ssl.backend.selector.pas`
       - 会直接消费 `SupportsPKCS11` / `SupportsTPM` 做 required-match 与 platform-score 判断
       - 所以前述 capability 假阳性不是“文档味道”，而是会把 auto backend selection 带偏的真实实现问题
     - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
       - 之前还把“智能卡 / TPM”写成已支持
   - 当前最小正确修法已落地：
     - 保留 OpenSSL 已 shipped 的 PKCS#11 capability truth
     - 只把 OpenSSL `SupportsTPM` 收紧回 `False`
     - 只把 WinSSL `SupportsPKCS11` / `SupportsTPM` 收紧回 `False`
     - 同步把 WinSSL active capability doc 改成“当前 capability 不发布”叙事
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_hardware_key_capability_truth_contract.sh`
     - `bash tests/scripts/test_hardware_key_capability_truth_contract.sh`
     - `mkdir -p tmp/test_openssl_features_units && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_features_units -FEtmp/test_openssl_features_units -otmp/test_openssl_features_units/test_openssl_features tests/openssl/test_openssl_features.pas && ./tmp/test_openssl_features_units/test_openssl_features`
     - `mkdir -p tmp/test_auto_backend_tpm_truth_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_auto_backend_tpm_truth_units -FEtmp/test_auto_backend_tpm_truth_units -otmp/test_auto_backend_tpm_truth_units/test_auto_backend_tpm_truth_contract tests/test_auto_backend_tpm_capability_truth_contract.pas && ./tmp/test_auto_backend_tpm_truth_units/test_auto_backend_tpm_truth_contract`
     - `python3 scripts/compile_all_modules.py`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 继续找下一条“平台潜在能力 / 低层 helper 可用性被误抬成 public capability truth”的 backend drift
     - 优先复审 `OpenSSL SupportsPKCS11` 是否还需要更细的 runtime-readiness gate，而不是重开已关闭的 TPM / WinSSL hardware-key 假阳性路线
70. `OpenSSL PKCS#11 capability runtime truth` 已完成 focused 收口，并应作为当前 capability/public-surface 审查的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-openssl-pkcs11-capability-runtime-truth.md`
   - 当前已确认的 route truth：
     - `src/fafafa.ssl.openssl.context.pas`
       - 继续保留 shipped `LoadPrivateKeyFromPKCS11(...)` 路径
     - `src/fafafa.ssl.pkcs11.backend.pas`
       - `TPKCS11BackendFactory.IsBackendAvailable(btAuto)` 已经提供现成的 runtime readiness truth
       - 当前 auto truth 由两组 surface 共同决定：
         - Provider:
           - `OSSL_PROVIDER_load`
           - `OSSL_STORE_open`
           - `OSSL_STORE_expect`
         - ENGINE:
           - `ENGINE_by_id`
           - `ENGINE_init`
           - `ENGINE_load_private_key`
     - `src/fafafa.ssl.openssl.backed.pas`
       - 之前仍把 `SupportsPKCS11` 硬编码成 `True`
       - 这会把“仓库里有 shipped loader path”误抬成“当前运行时一定具备 PKCS#11 backend readiness”
   - 当前最小正确修法已落地：
     - 不新增 PKCS#11 实现
     - 不改 builder / selector API
     - 只把 OpenSSL `SupportsPKCS11` 改为跟随：
       - `TPKCS11BackendFactory.IsBackendAvailable(btAuto)`
     - 同步把 active capability doc 改成 runtime-readiness 口径
   - 当前 focused proof 已覆盖：
     - `mkdir -p tmp/test_openssl_features_units && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_features_units -FEtmp/test_openssl_features_units -otmp/test_openssl_features_units/test_openssl_features tests/openssl/test_openssl_features.pas && ./tmp/test_openssl_features_units/test_openssl_features`
     - `python3 scripts/compile_all_modules.py`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 继续找下一条“低层 binding/helper readiness 被误抬成 public capability truth”的 backend drift
     - 优先看其它 backend / feature rows 是否还存在“helper exists => capability true”的残余点
71. `hardware-key shell contract runtime truth` 已完成 focused 收口，并应作为当前 tests/docs completeness 的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-hardware-key-contract-runtime-truth-resync.md`
   - 当前已确认的工作流偏差：
     - `tests/scripts/test_hardware_key_capability_truth_contract.sh`
       在上一批源码 truth 已收紧后，仍要求：
       - `Result.SupportsPKCS11 := True;`
     - 这会把旧的静态 capability 口径重新当成正确答案，导致合同自己落后于当前实现
   - 当前最小正确修法已落地：
     - 保留 OpenSSL shipped `LoadPrivateKeyFromPKCS11(...)` / backend-factory path 守护
     - 改为要求：
       - `LPKCS11Ready := TPKCS11BackendFactory.IsBackendAvailable(btAuto);`
       - `Result.SupportsPKCS11 := LPKCS11Ready;`
     - 明确禁止旧的：
       - `Result.SupportsPKCS11 := True;`
     - 同步把 OpenSSL active capability doc 的 runtime-readiness wording 纳入合同守护
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_hardware_key_capability_truth_contract.sh`
     - `bash tests/scripts/test_hardware_key_capability_truth_contract.sh`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 继续找其它“合同/文档仍锚在旧 capability truth，但源码已切到 runtime-aware truth”的残余点
     - 再决定是否继续深挖新的 backend capability drift
72. `active capability docs runtime truth` 已完成 focused 收口，并应作为当前 docs completeness 的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-active-capability-docs-runtime-truth-sweep.md`
   - 当前已确认的 active-doc drift：
     - `docs/MIGRATION_GUIDE_V1.1.md`
       - 仍把：
         - `WinSSL PKCS#11 = ✅`
         - `WinSSL TPM = ✅`
         - `OpenSSL FIPS = ✅`
         当成当前 capability truth
     - `docs/BACKEND_SELECTION_GUIDE.md`
       - OpenSSL 评分示例仍把：
         - `SupportsPKCS11: Yes`
         写成 unconditional truth
     - `docs/CAPABILITY_MATRIX_GUIDE.md`
       - Windows 推荐示例仍要求：
         - `SupportsSystemCertStore and SupportsTPM`
       - 这已经不符合当前 WinSSL capability truth
   - 当前最小正确修法已落地：
     - 不改生产代码
     - 只把上述 3 份 active docs 重新锚回：
       - OpenSSL `PKCS#11` runtime-aware truth
       - WinSSL `PKCS11/TPM` 非发布 truth
       - OpenSSL 默认构建 `FIPS = False`
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_active_capability_docs_runtime_truth_contract.sh`
     - `bash tests/scripts/test_active_capability_docs_runtime_truth_contract.sh`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 继续找其它 active docs / examples 是否仍把 runtime-aware capability 写成 unconditional truth
     - 优先复审 builder/selector 入口文档里的环境假设
73. `auto-backend PKCS#11 capability truth` 已完成 focused 收口，并应作为当前 selector/builder completeness 的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-auto-backend-pkcs11-capability-truth-contract.md`
   - 当前已确认的 proof gap：
     - 上一轮已经收口：
       - OpenSSL `SupportsPKCS11` runtime-aware source truth
       - `hardware-key` shell contract
     - 但 selector / builder 下游当前只有：
       - `RequireTPM` focused contract
     - 还没有：
       - `RequirePKCS11Support` focused runtime-aware downstream contract
   - 当前最小正确修法已落地：
     - 不改 selector 算法
     - 不改 builder 行为
     - 只新增一条 focused contract：
       - 若当前任一已注册 backend 发布 `SupportsPKCS11=True`，auto-backend selection 必须成功
       - 否则必须失败并返回 `No suitable SSL backend found for requirements`
   - 当前 focused proof 已覆盖：
     - `mkdir -p tmp/test_auto_backend_pkcs11_truth_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_auto_backend_pkcs11_truth_units -FEtmp/test_auto_backend_pkcs11_truth_units -otmp/test_auto_backend_pkcs11_truth_units/test_auto_backend_pkcs11_capability_truth_contract tests/test_auto_backend_pkcs11_capability_truth_contract.pas && ./tmp/test_auto_backend_pkcs11_truth_units/test_auto_backend_pkcs11_capability_truth_contract`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 继续找其它“source truth 已 runtime-aware，但 downstream proof 还缺位”的 builder/selector 残余点
     - 优先审 `RequirePKCS11Support` 相关文档/示例是否仍把本机 harness 现状误写成通用结论
74. `active FIPS docs truth` 已完成 focused 收口，并应作为当前 docs completeness 的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-active-fips-docs-truth-sweep.md`
   - 当前已确认的 active-doc drift：
     - `docs/reference/BACKEND_ABSTRACTION_LAYER_DESIGN.md`
       - 仍把 `OpenSSL FIPS = ✅` 写成当前 capability truth
     - `docs/reference/BACKEND_SELECTOR_DESIGN.md`
       - 仍把 `OpenSSL FIPS = ✅` 写成 selector 设计层默认真相
     - `docs/PLATFORM_SUPPORT.md`
       - 仍把 OpenSSL / WinSSL 对比写成两边都“FIPS 模式支持”
   - 当前最小正确修法已落地：
     - 不改生产代码
     - 只把上述 3 份 active docs 重新锚回：
       - OpenSSL 默认构建 `SupportsFIPSMode = False`
       - WinSSL 当前 `SupportsFIPSMode = True`
       - OpenSSL 若要进入 FIPS 路线，需要专门模块/构建
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_active_fips_docs_truth_contract.sh`
     - `bash tests/scripts/test_active_fips_docs_truth_contract.sh`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 继续找其它 active reference/platform docs 是否仍把 OpenSSL 默认构建写成已发布 FIPS capability
     - 或继续回到 builder/selector/implementation completeness 的下一个 focused proof gap
75. `backend selection guide runtime truth` 已完成 focused 收口，并应作为当前 builder/selector docs completeness 的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-backend-selection-guide-runtime-truth-sweep.md`
   - 当前已确认的 active-guide drift：
     - `WithSecurityFirst`
       - 只写安全优先快捷方式，没有说明它不等于默认 FIPS 路线
     - `RequirePKCS11Support`
       - 只写“要求支持 PKCS#11”，没有说明这取决于当前已发布 capability，且可能失败
     - “政府/金融系统”场景
       - 直接把 `FIPS + PKCS#11` 组合成当前示例
       - 但没有说明当前默认 shipped backends 不保证自动满足这条路线
   - 当前最小正确修法已落地：
     - 不改生产代码
     - 只把 `docs/BACKEND_SELECTION_GUIDE.md` 重新锚回：
       - `WithSecurityFirst` 不等于默认 FIPS
       - `RequirePKCS11Support` = runtime-aware requirement
       - `FIPS + PKCS#11` 场景 = 需求表达，不是当前默认部署必然成功
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_backend_selection_guide_runtime_truth_contract.sh`
     - `bash tests/scripts/test_backend_selection_guide_runtime_truth_contract.sh`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 继续找 builder/selector 入口 docs/examples 是否还把“需求表达”误写成“当前默认环境必然满足”
     - 或回到 selector/implementation 的下一个 downstream proof gap
76. `security-first FIPS independence contract` 已完成 focused 收口，并应作为当前 interface/backend completeness 的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-security-first-fips-independence-contract.md`
   - 当前已确认的真实 proof gap：
     - active guide 虽然已经写明：
       - `WithSecurityFirst` 不等于默认 FIPS
     - 但 builder / selector 层还缺少可执行证明来说明：
       - `CreateSecurityFirstRequirements` 默认不会设置 `PreferFIPSCompliant=True`
       - `WithSecurityFirst` 在存在 FIPS-capable backend 时，也不会把它当成默认偏好
   - 当前最小正确修法已落地：
     - 不改生产 selector / builder 代码
     - 只新增一条 environment-independent mock contract：
       - 默认 security-first 选择 non-FIPS backend
       - 只有显式打开 `PreferFIPSCompliant` 后，选择结果才切到 FIPS backend
       - `WithSecurityFirst` builder 默认构建出的 context 仍来自 non-FIPS backend
   - 当前 focused proof 已覆盖：
     - `mkdir -p tmp/test_security_first_fips_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_security_first_fips_units -FEtmp/test_security_first_fips_units -otmp/test_security_first_fips_units/test_security_first_fips_independence_contract tests/test_security_first_fips_independence_contract.pas && ./tmp/test_security_first_fips_units/test_security_first_fips_independence_contract`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 继续找其它“guide truth 已修正，但 behavior proof 仍未闭环”的 builder / selector / facade 入口
     - 优先复审：
       - deprecated context-level SNI 是否仍由高层入口默认传播
       - capability dual-truth 在 serializer / selector / docs 之间是否还有残余漂移
77. `ISSLOCSPStapling residual classification freeze` 已完成并应作为当前 backend-specific OCSP residual truth 的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-isslocspstapling-residual-classification-freeze.md`
   - 当前已确认的 residual truth：
     - ordinary docs 已不再把：
       - `Connection.GetOCSP*`
         当作新代码推荐路径
     - 当前 direct-core `GetOCSP*` residual file set 已稳定收缩到：
       - `tests/mbedtls/test_mbedtls_ocsp_capability.pas`
       - `tests/openssl/test_ocsp_connection_verification_regression.pas`
       - `tests/test_openssl_connection_ocsp_storectx_issuer_contract.pas`
       - `tests/test_wolfssl_ocsp_stapling_contract.pas`
     - 这 4 个 residual files 的性质都更接近 backend-specific runtime / contract proof，而不是 ordinary guidance 漂移
   - 当前最小正确修法已落地：
     - 不改 public signature
     - 不改 backend runtime 行为
     - 只补：
       - source owner / compatibility note
       - API reference compatibility note
       - residual-file `INTENTIONAL_OCSP_CORE_SURFACE` 标注
       - focused allowlist contract
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_isslocspstapling_residual_classification_contract.sh`
     - `bash tests/scripts/test_isslocspstapling_residual_classification_contract.sh`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 不再重复拉起 OCSP residual archaeology
     - 继续切回更大的 backend implementation-completeness 审查
78. `client-side OCSP optional interface capability alignment` 已完成并应作为当前 public-path optional-interface truth 的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-client-ocsp-optional-interface-capability-alignment.md`
   - 当前已确认的结构性 drift：
     - `tests/contract/test_backend_contract.pas` 的 `Contract 10` 早就要求：
       - `OCSPStaplingSupport<>None` 时，client connection 必须暴露 `ISSLOCSPStapling`
       - `OCSPStaplingSupport=None` 时，client connection 不应暴露 `ISSLOCSPStapling`
     - 但 `TOpenSSLConnection` / `TWolfSSLConnection` 之前仍直接实现：
       - `ISSLOCSPStapling`
     - 这意味着 capability 若在特定 runtime 下回到 `none`，public `CreateConnection(...)` 仍可能把 connection 误暴露成 OCSP-capable
   - 当前最小正确修法已落地：
     - 不改 OCSP runtime 逻辑
     - 只把 public connection creation path 改成 capability-aware subclass matrix：
       - `base`
       - `ocsp`
       - `early-data`
       - `early-data + ocsp`
     - 并把现有 focused source contract 扩到 client-side OCSP connection gating
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_optional_interface_capability_alignment_contract.sh`
     - `bash tests/scripts/test_optional_interface_capability_alignment_contract.sh`
     - `mkdir -p tmp/test_backend_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_backend_contract -FEtmp/test_backend_contract -otmp/test_backend_contract/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/test_backend_contract/test_backend_contract`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 不再重复拉起 client-side OCSP optional-interface matrix drift
     - 继续切回更大的 backend implementation-completeness 审查
79. `SupportsCallbacks capability truth audit` 已完成 focused 收口，并应作为当前 callback capability/source truth 的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-supportscallbacks-capability-truth-audit.md`
   - 当前已确认的 callback capability truth：
     - `OpenSSL`
       - `SupportsCallbacks=True`
       - verify/password/info callback 都有真实 runtime wiring
     - `WinSSL`
       - verify/info callback 在 connection/runtime path 被真实消费
       - capability 之前未显式发布，属于 source truth drift
     - `FreePascal`
       - verify/password/info 目前只有 setter / field 存储
       - 没有真实 runtime use-site
       - 之前 `SupportsCallbacks=True` 属于误发布
     - `WolfSSL` / `MbedTLS`
       - 当前也属于 setter-only / storage-only
       - 在没有真实 runtime wiring 前不应发布 `SupportsCallbacks=True`
   - 当前最小正确修法已落地：
     - 不改 callback API 设计
     - 不重写 runtime callback 行为
     - 只做 capability truth 对齐：
       - `WinSSL` 显式发布 `SupportsCallbacks=True`
       - `FreePascal` 改回 `SupportsCallbacks=False`
       - `WolfSSL` / `MbedTLS` 显式固定 `SupportsCallbacks=False`
       - `TSSLBackendCapabilities.SupportsCallbacks` 注释补充为“至少一条 callback 具备真实 runtime wiring”
     - 并新增：
       - source-truth shell contract
       - backend capability runtime truth contract
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_callback_capability_truth_contract.sh`
     - `bash tests/scripts/test_callback_capability_truth_contract.sh`
     - `mkdir -p tmp/test_callback_capability_truth && fpc -B -Fu./src -Fu./tests -FUtmp/test_callback_capability_truth -FEtmp/test_callback_capability_truth -otmp/test_callback_capability_truth/test_backend_callback_capability_truth_contract tests/test_backend_callback_capability_truth_contract.pas && ./tmp/test_callback_capability_truth/test_backend_callback_capability_truth_contract`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 不再重复拉起 `SupportsCallbacks` capability 真值本身
     - 继续审查：
       - `SupportsCallbacks=False` 的 backend 是否应该对 `SetVerifyCallback` / `SetPasswordCallback` / `SetInfoCallback` fail-closed
       - 或至少补齐 active docs / API reference，对 setter-only compatibility surface 给出明确 guidance
80. `callback setter fail-closed alignment` 已完成 focused 收口，并应作为当前 callback setter/runtime semantics 的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-callback-setter-fail-closed-alignment.md`
   - 当前已确认的真实 drift：
     - 前一批虽然已经把：
       - `FreePascal`
       - `WolfSSL`
       - `MbedTLS`
       的 `SupportsCallbacks` capability 收回到 `False`
     - 但这 3 个 backend 的：
       - `SetVerifyCallback`
       - `SetPasswordCallback`
       - `SetInfoCallback`
       仍然只是 silent setter / field store
     - 这会让 caller 继续误以为“虽然 capability 不发布，但接口至少还能安全配置”
     - 同时 `docs/reference/API_REFERENCE.md` 的 callback type signatures 也还停留在旧接口形态
   - 当前最小正确修法已落地：
     - 不改 `OpenSSL` / `WinSSL` 已发布 callback runtime path
     - 不重做 callback runtime 设计
     - 只把 `SupportsCallbacks=False` backend 的 setter 语义收紧为：
       - non-nil 赋值 -> fail-closed `unsupported`
       - `nil` -> 允许清除 / 保持默认行为
     - 并把：
       - `base` interface docs
       - `API_REFERENCE` callback gating note
       - `API_REFERENCE` callback type signatures
       写回当前源码真相
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_callback_setter_fail_closed_contract.sh`
     - `bash tests/scripts/test_callback_setter_fail_closed_contract.sh`
     - `mkdir -p tmp/test_callback_setter_fail_closed && fpc -B -Fu./src -Fu./tests -FUtmp/test_callback_setter_fail_closed -FEtmp/test_callback_setter_fail_closed -otmp/test_callback_setter_fail_closed/test_backend_callback_setter_fail_closed_contract tests/test_backend_callback_setter_fail_closed_contract.pas && ./tmp/test_callback_setter_fail_closed/test_backend_callback_setter_fail_closed_contract`
     - `bash tests/scripts/test_callback_capability_truth_contract.sh`
     - `bash tests/scripts/test_api_reference_library_context_surface_truth_contract.sh`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 不再重复拉起 false-backend callback setter silent-store drift
     - 继续审查：
       - `WinSSL` 的 callback surface 是否只是 verify/info partial runtime，而 `Password callback` 仍未接线
       - 现有单一 `SupportsCallbacks` bool 是否需要继续细化成 per-callback truth，或至少补 active docs 说明 partial runtime coverage
81. `WinSSL password callback partial-publication alignment` 已完成 focused 收口，并应作为当前 WinSSL callback granularity truth 的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-winssl-password-callback-publication-alignment.md`
   - 当前已确认的 WinSSL callback truth：
     - verify callback
       - 有真实 runtime use-site
     - info callback
       - 有真实 runtime use-site
     - password callback
       - 没有 runtime use-site
       - 没有 access seam
       - 之前只是 silent setter / field store
   - 当前最小正确修法已落地：
     - 不改 `WinSSL` verify/info callback path
     - 不改 `SupportsCallbacks` bool 结构
     - 只把 `WinSSL` password callback 收紧为：
       - non-nil 赋值 -> fail-closed `unsupported`
       - `nil` -> clear / no-op
     - 并同步：
       - `test_winssl_comprehensive` 的 Windows 预期
       - callback setter runtime contract 的 WinSSL 特例矩阵
       - `API_REFERENCE`
       - `WINSSL_DESIGN`
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_winssl_password_callback_partial_publication_contract.sh`
     - `bash tests/scripts/test_winssl_password_callback_partial_publication_contract.sh`
     - `mkdir -p tmp/test_callback_setter_fail_closed && fpc -B -Fu./src -Fu./tests -FUtmp/test_callback_setter_fail_closed -FEtmp/test_callback_setter_fail_closed -otmp/test_callback_setter_fail_closed/test_backend_callback_setter_fail_closed_contract tests/test_backend_callback_setter_fail_closed_contract.pas && ./tmp/test_callback_setter_fail_closed/test_backend_callback_setter_fail_closed_contract`
     - `bash tests/scripts/test_callback_setter_fail_closed_contract.sh`
     - `bash tests/scripts/test_callback_capability_truth_contract.sh`
     - `bash tests/scripts/test_api_reference_library_context_surface_truth_contract.sh`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 不再把 `WinSSL` password callback 当作已发布 callback surface
     - 继续审查：
       - 单一 `SupportsCallbacks` bool 是否需要进一步拆成 per-callback capability
       - 或先做 active docs / capability matrix，把 callback publication granularity 系统化写清
82. `callback publication matrix truth` 已完成 focused 收口，并应作为当前 active callback matrix docs 的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-callback-publication-matrix-truth.md`
   - 当前已确认的 docs drift：
     - `API_REFERENCE` 已经写明：
       - callback gating note
       - `WinSSL` partial callback publication
     - 但 active capability matrix docs 还缺：
       - `docs/BACKEND_CAPABILITY_MATRIX.md` 的 callback publication row
       - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md` 的 coarse bool / partial-publication note
   - 当前最小正确修法已落地：
     - 不改生产代码
     - 不重新设计 capability 结构
     - 只把 callback publication truth 写回 active matrix docs：
       - backend quick-reference row
       - callback row semantics note
       - WinSSL backend matrix partial-publication row
       - WinSSL coarse-grained `SupportsCallbacks=True` note
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_callback_publication_matrix_truth_contract.sh`
     - `bash tests/scripts/test_callback_publication_matrix_truth_contract.sh`
     - `bash tests/scripts/test_callback_capability_truth_contract.sh`
     - `bash tests/scripts/test_winssl_password_callback_partial_publication_contract.sh`
     - `bash tests/scripts/test_api_reference_library_context_surface_truth_contract.sh`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 不再重复拉起 active callback matrix docs drift
     - 继续审查：
       - 是否还存在其它 active guide / reference 页面把 `SupportsCallbacks=True` 误读成“所有 callback 种类都已发布”
       - 以及单一 bool capability 是否最终需要拆解成 finer-grained publication surface
83. `password-protected key capability truth` 已完成 focused 收口，并应作为当前 private-key password capability 的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-password-protected-key-capability-truth.md`
   - 当前已确认的真实 drift：
     - `FreePascal` / `WolfSSL` 此前都把 `SupportsPasswordProtectedKeys` 发布为 `True`
     - 但当前实现并没有真正消费：
       - `LoadPrivateKey(..., APassword)`
       - `LoadPrivateKeyPEM(..., APassword)`
       的 non-empty password path
     - `FreePascal` 甚至直接以 `if APassword <> '' then;` 静默吞掉参数
     - `WolfSSL` 也没有 shipped password bridge，且还留有“密码回调需要单独设置”的旧注释
   - 当前最小正确修法已落地：
     - 不补做 `FreePascal` / `WolfSSL` 的 encrypted private-key runtime
     - 只把：
       - `src/fafafa.ssl.freepascal.lib.pas`
       - `src/fafafa.ssl.wolfssl.lib.pas`
       的 `SupportsPasswordProtectedKeys` 收回到 `False`
     - 并让 `FreePascal` / `WolfSSL` 的：
       - file
       - stream
       - PEM
       三条 private-key load path 在收到 non-empty `APassword` 时 fail-closed 为 `unsupported`
     - 同时同步：
       - `src/fafafa.ssl.base.pas`
       - `docs/BACKEND_CAPABILITY_MATRIX.md`
       - `docs/reference/API_REFERENCE.md`
       - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
       - `docs/reference/WINSSL_DESIGN.md`
       说明当前 WinSSL 仍只是 coarse-grained `True`：
       - password-protected PFX/P12 import path 已发布
       - PEM private-key password path 仍 unsupported
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_password_protected_key_capability_truth_contract.sh`
     - `bash tests/scripts/test_password_protected_key_capability_truth_contract.sh`
     - `mkdir -p tmp/test_password_protected_key_capability_truth && fpc -B -Fu./src -Fu./tests -FUtmp/test_password_protected_key_capability_truth -FEtmp/test_password_protected_key_capability_truth -otmp/test_password_protected_key_capability_truth/test_backend_password_protected_key_capability_truth_contract tests/test_backend_password_protected_key_capability_truth_contract.pas && ./tmp/test_password_protected_key_capability_truth/test_backend_password_protected_key_capability_truth_contract`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 不再重复拉起 `FreePascal` / `WolfSSL` password-protected key capability 假阳性
     - 继续审查：
       - 是否还有其它 coarse-grained capability 在某个 backend 上只发布了 partial surface，却在 active docs / source comments 里说得过宽
       - 或是否需要把 `SupportsPasswordProtectedKeys` 最终细化成更明确的 per-format / per-path capability
84. `WinSSL private-key format truth` 已完成 focused 收口，并应作为当前 WinSSL key-format capability 的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-winssl-private-key-format-truth.md`
   - 当前已确认的真实 drift：
     - `WinSSL` 之前仍把：
       - `SupportsDERPrivateKey`
       - `SupportsPKCS8PrivateKey`
       发布为 `True`
     - 但现有 `LoadPrivateKey*` 实际只发布 `PFX/P12` bundle import path
     - 同时 `TWinSSLContext.LoadPrivateKey(AStream, APassword)` 在 non-PFX 输入上还存在 silent-success 漏口：
       - else 分支错误写成 `if AStream = nil then raise ...`
       - 结果是普通 PEM/DER 私钥流可能既不加载，也不 fail-fast
   - 当前最小正确修法已落地：
     - 不补做 WinSSL 的 bare DER / PKCS#8 private-key import
     - 只把：
       - `src/fafafa.ssl.winssl.lib.pas`
       的 `SupportsDERPrivateKey` / `SupportsPKCS8PrivateKey` 收回到 `False`
     - 保留：
       - `SupportsPKCS12=True`
       - `SupportsPasswordProtectedKeys=True`
       但明确它们只代表当前 `PFX/P12` import path
     - 并把 `TWinSSLContext.LoadPrivateKey(AStream, APassword)` 修成：
       - `nil` stream -> invalid param
       - non-PFX input -> fail-closed `unsupported`
     - 同时同步：
       - `docs/reference/API_REFERENCE.md`
       - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
       - `docs/reference/WINSSL_DESIGN.md`
       - `docs/guides/WINSSL_QUICKSTART.md`
       - `docs/guides/WINSSL_BEST_PRACTICES.md`
       - `docs/guides/WINSSL_USER_GUIDE.md`
       把 WinSSL 专属示例和说明收回到真实 `PFX/P12` 路径
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_winssl_private_key_format_truth_contract.sh`
     - `bash tests/scripts/test_winssl_private_key_format_truth_contract.sh`
     - `mkdir -p tmp/test_winssl_private_key_format_truth && fpc -B -Fu./src -Fu./tests -FUtmp/test_winssl_private_key_format_truth -FEtmp/test_winssl_private_key_format_truth -otmp/test_winssl_private_key_format_truth/test_winssl_private_key_format_truth_contract tests/test_winssl_private_key_format_truth_contract.pas && ./tmp/test_winssl_private_key_format_truth/test_winssl_private_key_format_truth_contract`
     - `bash tests/scripts/test_password_protected_key_capability_truth_contract.sh`
     - `bash tests/scripts/test_winssl_capability_source_contract.sh`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 不再把 WinSSL 的 bare DER / PKCS#8 private-key path 当作已发布 capability
     - 继续审查：
       - 是否还有其它 backend 在 `SupportsDERPrivateKey` / `SupportsPKCS8PrivateKey` / `SupportsPKCS12` 上也存在 partial-publication truth
       - 以及 active global docs 是否需要把 key-format capability matrix 系统化写清
85. `optional backends PKCS12 capability truth` 已完成 focused 收口，并应作为当前 PKCS#12 backend truth 的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-optional-backends-pkcs12-capability-truth.md`
   - 当前已确认的真实 drift：
     - `MbedTLS` / `WolfSSL` 此前都把：
       - `SupportsPKCS12`
       发布为 `True`
     - 但当前 shipped context path 只覆盖：
       - `LoadCertificate*`
       - `LoadPrivateKey*`
       的 PEM / DER / PKCS#8 路径
     - 当前看不到任何 public：
       - PKCS#12 create
       - PKCS#12 parse
       - PFX/P12 bundle import
       surface
     - active docs 还存在全局口径冲突：
       - `docs/guides/FAQ.md` 仍写“PKCS#12 支持计划中”
       - `docs/guides/PKCS12_USER_GUIDE.md` 则写“通过 OpenSSL 后端提供完整支持”
   - 当前最小正确修法已落地：
     - 不补做 `MbedTLS` / `WolfSSL` 的 PKCS#12 runtime
     - 只把：
       - `src/fafafa.ssl.mbedtls.lib.pas`
       - `src/fafafa.ssl.wolfssl.lib.pas`
       的 `SupportsPKCS12` 收回到 `False`
     - 并同步全局文档口径：
       - `docs/BACKEND_CAPABILITY_MATRIX.md`
       - `docs/guides/FAQ.md`
       - `docs/guides/PKCS12_USER_GUIDE.md`
       - `docs/reference/API_REFERENCE.md`
       统一回到：
       - `OpenSSL` = 完整 PKCS#12 helper/API
       - `WinSSL` = PFX/P12 bundle import partial path
       - `FreePascal` / `MbedTLS` / `WolfSSL` = 当前不发布 PKCS#12 bundle surface
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_optional_backends_pkcs12_capability_truth_contract.sh`
     - `bash tests/scripts/test_optional_backends_pkcs12_capability_truth_contract.sh`
     - `mkdir -p tmp/test_optional_backends_pkcs12_capability_truth && fpc -B -Fu./src -Fu./tests -FUtmp/test_optional_backends_pkcs12_capability_truth -FEtmp/test_optional_backends_pkcs12_capability_truth -otmp/test_optional_backends_pkcs12_capability_truth/test_optional_backends_pkcs12_capability_truth_contract tests/test_optional_backends_pkcs12_capability_truth_contract.pas && ./tmp/test_optional_backends_pkcs12_capability_truth/test_optional_backends_pkcs12_capability_truth_contract`
     - `bash tests/scripts/test_password_protected_key_capability_truth_contract.sh`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 不再把 `MbedTLS` / `WolfSSL` 的 `SupportsPKCS12` 当作已发布 capability
     - 继续审查：
       - 是否还有其它 coarse-grained capability 在 global docs / matrix 里被写成“全 backend 通用支持”
       - 以及 `SupportsPKCS12=True` 是否还需要在更多 active docs 中显式区分：
         - OpenSSL helper/API
         - WinSSL PFX/P12 import
86. `MbedTLS active docs capability truth` 已完成 focused 收口，并应作为当前 MbedTLS 高入口文档的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-mbedtls-active-docs-capability-truth.md`
   - 当前已确认的真实 drift：
     - `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
       之前仍把：
       - `0-RTT`
       - `证书固定`
       - `自定义 I/O`
       讲得比当前 published surface 更宽
     - `docs/guides/MBEDTLS_USER_GUIDE.md`
       之前仍保留大量过时 API 名称与旧签名：
       - `LoadCertificateFromFile`
       - `LoadPrivateKeyFromFile`
       - `LoadCAFromFile`
       - `Connection.SetHostname`
       - `Connection.Connect(host, port)`
       - `ReadAll`
       - `GetCipherSuite`
       - `GetLastError: string`
     - 同时还把 MbedTLS 说成与其它 backend “完全相同的接口”，并把 callback / FIPS / 0-RTT truth 讲宽
   - 当前最小正确修法已落地：
     - 不补做新的 MbedTLS runtime 能力
     - 只把：
       - `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
       - `docs/guides/MBEDTLS_USER_GUIDE.md`
       收回到当前 public API / capability truth
     - 同步后的当前心智为：
       - `SupportsCallbacks=False`
       - `SupportsPKCS12=False`
       - `SupportsFIPSMode=False`
       - `0-RTT` current public capability = none
       - 证书固定走 context pinning API，不是 callback surface
       - transport public surface 只发布 socket / stream `CreateConnection(...)`
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_mbedtls_active_docs_capability_truth_contract.sh`
     - `bash tests/scripts/test_mbedtls_active_docs_capability_truth_contract.sh`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 不再把旧的 MbedTLS 指南/矩阵当成 current source truth
     - 继续审查：
       - 其它 backend 专属 active guide/reference 是否也残留同类“旧方法名 + 过宽 capability 叙事”
       - 以及还有哪些高入口文档仍把 backend-specific truth 写成“统一等价接口”
87. `API inventory / PKCS11 high-entry doc truth` 已完成 focused 收口，并应作为当前高入口参考页的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-api-inventory-pkcs11-high-entry-doc-truth.md`
   - 当前已确认的真实 drift：
     - `docs/reference/API_INVENTORY.md`
       - 仍停在 2026-01-31 的 phase snapshot 叙事
       - 仍只列 `OpenSSL` / `WinSSL` context/connection family
       - 仍把 `GetOCSPStaplingEnabled` / `GetOCSPResponse` / `IsOCSPResponseVerified` / `GetOCSPResponseStatus` 写成“待实现”
       - 仍把 `PKCS#11` / `OCSP Stapling` 写成“下一步计划”
     - `docs/guides/PKCS11_USER_GUIDE.md`
       - 虽然 builder 示例已更新
       - 但高层叙事还没有把当前 published path 明确锚到 `OpenSSL` backend
       - 也没有把 `SupportsPKCS11` 的 runtime-aware truth 作为主叙事
     - `docs/reference/PKCS11_ARCHITECTURE.md`
       - 仍缺少“其它 backend 当前 `SupportsPKCS11=False`”的显式边界
       - `TOpenSSLContext.LoadPrivateKeyFromPKCS11(...)` 示例签名也还残留旧形态
   - 当前最小正确修法已落地：
     - 不改生产源码
     - 只把高入口参考页重新锚回当前 source/runtime truth：
       - `API_INVENTORY.md`
         - 改成 current public-surface index
         - 去掉历史 phase snapshot / 测试统计 / 性能数字 / next-step 待办
         - 明确多 backend context / connection / certificate / store / session family
         - 明确 OCSP compatibility methods 已 shipped，owner truth 在 `ISSLOCSPStapling`
       - `PKCS11_USER_GUIDE.md`
         - 明确当前 published PKCS#11 private-key path 只在 `OpenSSL` backend 暴露
         - 明确 capability truth 跟随 `TPKCS11BackendFactory.IsBackendAvailable(btAuto)`
         - 明确其它 backend 当前不发布 `SupportsPKCS11`
       - `PKCS11_ARCHITECTURE.md`
         - 明确当前 published path = OpenSSL backend integration
         - 修正 `LoadPrivateKeyFromPKCS11(const AURI: string; const APIN: string)` 签名示例
         - 补齐 runtime-aware capability / non-OpenSSL boundary
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_api_inventory_pkcs11_high_entry_truth_contract.sh`
     - `bash tests/scripts/test_api_inventory_pkcs11_high_entry_truth_contract.sh`
     - `bash tests/scripts/test_pkcs11_docs_builder_guidance_contract.sh`
     - `npx prettier --write docs/reference/API_INVENTORY.md docs/guides/PKCS11_USER_GUIDE.md docs/reference/PKCS11_ARCHITECTURE.md`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 不再把 `API_INVENTORY` 或 `PKCS11` 专题页当成历史 phase snapshot
     - 继续审查：
       - 其它高入口 reference / guide 页面是否仍把 backend-specific truth 写成统一等价接口
       - 以及还有哪些入口页仍保留“阶段报告式”快照内容而不是 current source truth
88. `WinSSL quickstart runtime truth` 已完成 focused 收口，并应作为当前 WinSSL 高入口 quickstart 的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-winssl-quickstart-runtime-truth.md`
   - 当前已确认的真实 drift：
     - `docs/guides/WINSSL_QUICKSTART.md`
       - 仍把：
         - `Ctx.SetVerifyMode([sslVerifyPeer])`
         - `Ctx.SetVerifyMode([sslVerifyPeer, sslVerifyFailIfNoPeerCert])`
         - `Ctx.LoadCAFile('custom-ca.crt')`
         讲成“待实现”
       - 仍使用旧语法：
         - `Ctx.SetVerifyMode(sslVerifyPeer);`
         - `Ctx.SetVerifyMode(sslVerifyPeer or sslVerifyFailIfNoPeerCert);`
       - 故障排查里仍写：
         - “证书验证失败（未实现时使用手动模式）”
       - SNI 调试示例仍使用 deprecated：
         - `Ctx.GetServerName`
       - 同一页 FAQ 却已经承认：
         - 自动证书验证已实现
         - 双向 TLS 已支持
     - 这不是单点措辞问题，而是同一高入口 quickstart 内部自己和自己矛盾
   - 当前最小正确修法已落地：
     - 不改 WinSSL 生产实现
     - 只把 `docs/guides/WINSSL_QUICKSTART.md` 重新锚回当前 runtime/source truth：
       - `SetVerifyMode([])` = 测试环境 verify-none
       - `SetVerifyMode([sslVerifyPeer])` = 当前生产推荐
       - `SetVerifyMode([sslVerifyPeer, sslVerifyFailIfNoPeerCert])` = 当前 mTLS verify policy
       - `LoadCAFile('custom-ca.crt')` = 当前已发布 CA load path
       - troubleshooting 改成当前验证/mTLS 失败语义
       - SNI 调试示例改成 per-connection `ISSLClientConnection.GetServerName`
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_winssl_quickstart_runtime_truth_contract.sh`
     - `bash tests/scripts/test_winssl_quickstart_runtime_truth_contract.sh`
     - `bash tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
     - `bash tests/scripts/test_winssl_private_key_format_truth_contract.sh`
     - `npx prettier --write docs/guides/WINSSL_QUICKSTART.md`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 不再把 `WINSSL_QUICKSTART` 当作旧阶段状态页
     - 继续审查：
       - 其它 backend quickstart / high-entry guide 是否也残留“已实现能力仍写待实现”或旧接口语法
       - 尤其优先看还保留 phase snapshot / 总测试数 / 完成度口径的 specialized guides
89. `Security guide HSM/password-key truth` 已完成 focused 收口，并应作为当前安全指南密钥管理段落的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-security-guide-hsm-password-truth.md`
   - 当前已确认的真实 drift：
     - `docs/guides/SECURITY_GUIDE.md`
       - 仍示范不存在的：
         - `LoadPKCS11Engine(...)`
         - `LoadKeyFromHSM(...)`
         - `LContext.SetPrivateKey(...)`
       - 仍把：
         - `LContext.LoadPrivateKey('server.key', 'strong-password')`
         当作 generic truth
       - 但没有交代：
         - 先检查 `SupportsPasswordProtectedKeys`
         - `WinSSL` 当前只有 password-protected PFX/P12 path
         - `FreePascal` / `WolfSSL` 当前 non-empty `APassword` 会 fail-closed
       - 也没有交代：
         - 当前 published HSM / PKCS#11 path 只在 `OpenSSL` backend 暴露
         - `SupportsPKCS11=True` 依赖 runtime-ready Provider / ENGINE path
   - 当前最小正确修法已落地：
     - 不改生产实现
     - 只把 `SECURITY_GUIDE` 重新锚回当前 public API / capability truth：
       - 密码保护私钥示例改成先检查 `SupportsPasswordProtectedKeys`
       - 明确 `WinSSL` / `FreePascal` / `WolfSSL` 的边界
       - HSM 示例改成当前真实 published path：
         - `OpenSSL` backend
         - `LLib.GetCapabilities.SupportsPKCS11`
         - `LoadPrivateKey('pkcs11:...')`
       - 同步链接到专门的 `PKCS11_USER_GUIDE`
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_security_guide_hsm_password_truth_contract.sh`
     - `bash tests/scripts/test_security_guide_hsm_password_truth_contract.sh`
     - `bash tests/scripts/test_api_inventory_pkcs11_high_entry_truth_contract.sh`
     - `npx prettier --write docs/guides/SECURITY_GUIDE.md`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 不再让 `SECURITY_GUIDE` 把不存在的 HSM helper 当作 public API
     - 继续审查：
       - 其它 specialized guides 是否还把 backend-specific helper/API 冒充成 generic public path
       - 以及哪些指南仍保留“总测试数 / 通过率 / Phase 完成度”式快照内容
90. `specialized guide historical test snapshot cleanup` 已完成 focused 收口，并应作为当前 specialized guides 文档口径的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-specialized-guide-historical-test-snapshot-cleanup.md`
   - 当前已确认的真实 drift：
     - `docs/guides/CMS_USER_GUIDE.md`
       - 仍把：
         - `43/43`
         - `20/20`
         - `100.0%`
         - `总测试数`
         - `预期输出`
         这类历史测试快照直接写在当前正文里
       - 还保留按时间线记录的旧通过率更新日志
     - `docs/guides/PKCS12_USER_GUIDE.md`
       - 仍把：
         - `34/34`
         - `100.0%`
         - `总测试数`
         - `预期输出`
         直接写成当前 helper/API 指南 truth
       - 还保留旧的测试通过率/阶段性更新日志
   - 当前最小正确修法已落地：
     - 不改生产实现
     - 只把 `CMS_USER_GUIDE` / `PKCS12_USER_GUIDE` 的正文口径改成：
       - 保留当前 surface 边界
       - 保留可执行测试命令
       - 保留使用示例
       - 去掉硬编码历史统计与 captured output 块
       - 用“成功标准 + 以当前运行结果为准”的方式描述验证
       - 把更新日志收成维护说明，不再把旧通过率写成当前正文 truth
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_specialized_guides_historical_test_snapshot_contract.sh`
     - `bash tests/scripts/test_specialized_guides_historical_test_snapshot_contract.sh`
     - `npx prettier --write docs/guides/CMS_USER_GUIDE.md docs/guides/PKCS12_USER_GUIDE.md`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 不再把 specialized guides 当作固定测试统计面板
     - 继续审查：
       - 其它 guide/reference 是否还残留相同的“历史快照混入当前正文 truth”问题
       - 尤其优先看仍保留 phase 完成度 / 性能基准截图 / 通过率段落的文档
91. `PKCS7 guide status/performance truth` 已完成 focused 收口，并应作为当前 PKCS7 specialized guide 的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-pkcs7-guide-status-performance-truth.md`
   - 当前已确认的真实 drift：
     - `docs/guides/PKCS7_USER_GUIDE.md`
       - 仍把：
         - `Production Ready (100% 测试通过)`
         - 固定 `2 ms` 签名/加密/解密数字
         - 固定 `500 ops/s`
         - 固定 `158/158`
         直接写成当前正文 truth
       - 同时没有交代：
         - 当前指南只覆盖 `OpenSSL` backend PKCS7 surface
         - 当前 public 入口既有 helper，也有 raw API
         - `PKCS7` 当前没有一对一 capability 字段
   - 当前最小正确修法已落地：
     - 不改生产实现
     - 只把 `PKCS7_USER_GUIDE` 重新锚回当前 public/source truth：
       - 明确 `OpenSSL` backend raw API + helper surface
       - 明确 `SignData` / `VerifySignedData` / `EncryptData` / `DecryptData`
       - 明确 `LoadPKCS7Functions` + `osmPKCS7` + focused tests 的支持判定口径
       - 保留 BIO ownership 规则
       - 用“验证入口 + 成功标准 + 以当前运行结果为准”替换固定状态/性能/通过率快照
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_pkcs7_guide_status_performance_truth_contract.sh`
     - `bash tests/scripts/test_pkcs7_guide_status_performance_truth_contract.sh`
     - `npx prettier --write docs/guides/PKCS7_USER_GUIDE.md`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 优先继续审查高入口 truth drift 页面：
       - `docs/guides/WINSSL_USER_GUIDE.md`
       - `docs/guides/WINSSL_QUICKSTART.md`
       - `docs/guides/QUICKSTART_30SEC.md`
       - `docs/guides/5_MINUTE_QUICKSTART.md`
       - `docs/reference/ARCHITECTURE.md`
     - 性能类文档如 `PERFORMANCE_GUIDE` / `PERFORMANCE_OPTIMIZATION_GUIDE` 暂排在这些高入口页之后
92. `WinSSL user guide performance/runtime truth` 已完成 focused 收口，并应作为当前 WinSSL 高入口 guide 的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-winssl-user-guide-performance-runtime-truth.md`
   - 当前已确认的真实 drift：
     - `docs/guides/WINSSL_USER_GUIDE.md`
       - 仍把：
         - `436.94 ms`
         - `204.52 ms`
         - `2.41 conn/s`
         - `100%`
         - `30/30 成功`
         直接写成当前性能/稳定性正文 truth
       - 同时没有把：
         - `WINSSL_BACKEND_STATUS_REPORT`
         - `tests/windows/VALIDATION_BUNDLE.md`
         - `.github/workflows/wave-b-b2-manual.yml` 的 `windows-gate`
         作为当前 runtime baseline 入口讲清楚
   - 当前最小正确修法已落地：
     - 不改生产实现
     - 只把 `WINSSL_USER_GUIDE` 的性能段落重新锚回当前 runtime truth：
       - 明确固定 latency / rate / success-rate 只是历史运行快照
       - 明确当前 baseline 应看状态报告、validation bundle、`windows-gate`
       - 明确成功标准是 fresh artifact / summary / session truth 对齐
       - 保留调优文档链接，但不再把 benchmark snapshot 写成 capability truth
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_winssl_user_guide_performance_truth_contract.sh`
     - `bash tests/scripts/test_winssl_user_guide_performance_truth_contract.sh`
     - `bash tests/scripts/test_active_release_platform_truth_contract.sh`
     - `bash tests/scripts/test_active_connection_api_docs_truth_contract.sh`
     - `npx prettier --write docs/guides/WINSSL_USER_GUIDE.md`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 优先继续审查：
       - `docs/guides/WINSSL_QUICKSTART.md`
         - 仍保留 `WinSSL 后端 100% 完成（所有 6 个阶段）`
         - FAQ 里仍有 `Phase 5 完成` 这类阶段快照口径
       - `docs/guides/QUICKSTART_30SEC.md`
       - `docs/guides/5_MINUTE_QUICKSTART.md`
       - `docs/reference/ARCHITECTURE.md`
     - 性能类文档仍排在这些高入口页之后
93. `WinSSL quickstart status/phase truth` 已完成 focused 收口，并应作为当前 WinSSL first-contact quickstart 的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-winssl-quickstart-status-phase-truth.md`
   - 当前已确认的真实 drift：
     - `docs/guides/WINSSL_QUICKSTART.md`
       - FAQ 仍把：
         - `WinSSL 已完整实现服务器模式（Phase 5 完成）`
         - `WinSSL 已实现完整的自动证书验证（Phase 1 完成）`
         直接写成当前结论
       - 性能段仍把：
         - `~150ms`
         - `~160ms`
         - `~80 MB/s`
         - `~85 MB/s`
         写成 quickstart 参考表
       - 使用建议里仍把：
         - `需要服务器模式（当前）`
         - `需要完整证书验证（当前）`
         推给 OpenSSL
       - 页尾仍保留：
         - `WinSSL 后端 100% 完成（所有 6 个阶段）`
   - 当前最小正确修法已落地：
     - 不改生产实现
     - 只把 `WINSSL_QUICKSTART` 重新锚回当前 public/runtime truth：
       - 顶部增加当前口径说明
       - FAQ 的 server/verify 回到当前 public surface + 状态报告边界
       - 性能段改成 runtime baseline / benchmark 说明，不再保留固定跑数
       - 使用建议改成“跨平台 server/runtime 路径 / caller-provided server OCSP stapling / 更深 session runtime 证明”
       - 页尾状态改成当前零依赖客户端 baseline + experimental session truth
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_winssl_quickstart_status_phase_truth_contract.sh`
     - `bash tests/scripts/test_winssl_quickstart_status_phase_truth_contract.sh`
     - `bash tests/scripts/test_winssl_quickstart_runtime_truth_contract.sh`
     - `bash tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
     - `npx prettier --write docs/guides/WINSSL_QUICKSTART.md`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 优先继续审查：
       - `docs/guides/QUICKSTART_30SEC.md`
         - 仍保留 captured `预期输出`
       - `docs/guides/5_MINUTE_QUICKSTART.md`
         - 仍保留多处 captured `预期输出`
       - `docs/reference/ARCHITECTURE.md`
         - 仍保留 `WinSSL ... 100% 完成`
     - 性能类文档仍排在这些高入口页之后
94. `high-entry quickstarts captured-output truth` 已完成 focused 收口，并应作为当前通用 quickstart 入口文档的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-high-entry-quickstarts-captured-output-truth.md`
   - 当前已确认的真实 drift：
     - `docs/guides/QUICKSTART_30SEC.md`
       - 仍把固定 `预期输出`
       - 固定 OpenSSL 版本字符串
       - 固定 TLS 版本/密码套件示例
       直接写成 quickstart 正文 truth
     - `docs/guides/5_MINUTE_QUICKSTART.md`
       - 仍把多段 captured `预期输出`
       - 固定 OpenSSL 版本 / backend 版本
       - 固定 HTTP 响应预览
       直接写成 quickstart 正文 truth
       - 还保留错误 clone 地址：
         - `https://github.com/your-org/fafafa.ssl.git`
   - 当前最小正确修法已落地：
     - 不改生产实现
     - 只把两份 quickstart 重新锚回当前可执行入口 truth：
       - 保留当前编译/运行命令
       - 用“成功标准 + 以当前运行结果为准”替代 captured output
       - 把 5 分钟 quickstart 的 clone URL 改成当前仓库地址
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_high_entry_quickstarts_captured_output_truth_contract.sh`
     - `bash tests/scripts/test_high_entry_quickstarts_captured_output_truth_contract.sh`
     - `npx prettier --write docs/guides/QUICKSTART_30SEC.md docs/guides/5_MINUTE_QUICKSTART.md`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 优先继续审查：
       - `docs/reference/ARCHITECTURE.md`
         - 仍保留 `WinSSL ... 100% 完成`
         - 还保留 `OpenSSL ... 生产就绪` 这类阶段化 status wording
     - 性能类文档继续排在这条高入口参考页之后
95. `architecture backend-status truth` 已完成 focused 收口，并应作为当前架构参考页的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-architecture-backend-status-truth.md`
   - 当前已确认的真实 drift：
     - `docs/reference/ARCHITECTURE.md`
       - backend 状态表仍把：
         - `OpenSSL ... ✅ 生产就绪`
         - `WinSSL ... 100% 完成`
         写成当前 truth
       - 但这页本身已经承认：
         - 当前执行顺序和阶段判断应看 `docs/ROADMAP.md`
   - 当前最小正确修法已落地：
     - 不改生产实现
     - 只把 `ARCHITECTURE` 的 backend 状态表改回当前架构页口径：
       - `OpenSSL` = 当前默认 active backend
       - `WinSSL` = Windows 零依赖客户端 baseline 已验证；更细 runtime truth 见状态报告
       - 并在表前显式声明 shipped/runtime truth source 不以本表的完成度措辞为准
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_architecture_backend_status_truth_contract.sh`
     - `bash tests/scripts/test_architecture_backend_status_truth_contract.sh`
     - `npx prettier --write docs/reference/ARCHITECTURE.md`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 当前高入口 docs truth 主线已从：
       - WinSSL quickstart / user guide
       - 通用 quickstart
       - PKCS7 / CMS / PKCS12 guide
       - architecture backend status
       基本收口
     - 后续优先队列转向：
       - `docs/guides/PERFORMANCE_GUIDE.md`
       - `docs/guides/PERFORMANCE_OPTIMIZATION_GUIDE.md`
       - 以及其它仍保留 phase/baseline/benchmark 快照的历史型文档
96. `performance guides benchmark truth` 已完成 focused 收口，并应作为当前性能文档主入口的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-performance-guides-benchmark-truth.md`
   - 当前已确认的真实 drift：
     - `docs/guides/PERFORMANCE_GUIDE.md`
       - 仍把：
         - `Phase B 优化成果`
         - 固定 `ops/s`
         - 固定 `ms`
         - 固定 `目标值`
         - 固定 `完成 Phase C`
         直接写成当前正文 truth
       - 还把 `benchmark_aesgcm_pool` 与默认 Phase 2 baseline lane 混成一个口径
     - `docs/guides/PERFORMANCE_OPTIMIZATION_GUIDE.md`
       - 仍把：
         - `3.7ms`
         - `1160ms`
         - `181ms`
         - `6.4 倍`
         - `完美支持`
         这类某次 TLS 运行快照写成当前结论
       - 同时仍在示例里教：
         - `ISSLConnection.GetSession`
         - `ISSLConnection.SetSession`
         - `ISSLConnection.IsSessionReused`
         - `ISSLConnection.GetPerformanceMetrics`
         但这些 core mirror 当前都已不是 active owner path
   - 当前最小正确修法已落地：
     - 不改生产实现
     - 只把两份性能文档重新锚回当前 benchmark/source truth：
       - 明确 `scripts/run_phase2_performance_baseline.sh`
       - 明确 `tests/benchmarks/run_all_benchmarks.sh`
       - 明确 `tests/benchmarks/baselines/*.json`
       - 明确“成功标准 + 环境记录 + 以当前运行结果为准”
       - 把 TLS 性能示例切回：
         - `ISSLSessionResumption`
         - `ISSLDiagnostics`
       - 把 `benchmark_aesgcm_pool` 降回 manual/auxiliary lane，不再冒充默认 shipped baseline
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_performance_guides_benchmark_truth_contract.sh`
     - `bash tests/scripts/test_performance_guides_benchmark_truth_contract.sh`
     - `bash tests/scripts/test_active_docs_no_ci_pipeline_contract.sh`
     - `npx prettier --write docs/guides/PERFORMANCE_GUIDE.md docs/guides/PERFORMANCE_OPTIMIZATION_GUIDE.md`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 继续审查其它历史型/专项型文档是否还保留：
       - 固定 benchmark snapshot
       - 固定 phase 完成度
       - direct-core compatibility mirror 示例
     - 但不再回头重开已经收口的高入口 docs truth 页面
97. `active owner-path docs alignment` 已完成 focused 收口，并应作为当前活跃文档 owner-path guidance 的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-active-owner-path-docs-alignment.md`
   - 当前已确认的真实 drift：
     - `docs/reference/API_REFERENCE.md`
       - `TSSLHealthStatus` / `TSSLPerformanceMetrics` / `TSSLDiagnosticInfo`
         的说明仍写成：
         - `通过 ISSLConnection.GetHealthStatus 获取...`
         - `通过 ISSLConnection.GetPerformanceMetrics 获取...`
         - `通过 ISSLConnection.GetDiagnosticInfo 获取...`
       - 这会和同段里已有的 deprecated/owner-path 说明自相矛盾
     - `docs/guides/WINSSL_BEST_PRACTICES.md`
       - 仍示范：
         - `LConn1.GetSession`
         - `LConn.SetSession`
     - `docs/guides/PERFORMANCE_PROFILING_GUIDE.md`
       - 仍示范：
         - `Conn1.GetSession`
         - `Conn2.SetSession`
     - `docs/reference/WINSSL_DESIGN.md`
       - warmup 伪代码仍写：
         - `FSessionManager.AddSession(LHost, LConn.GetSession);`
   - 当前最小正确修法已落地：
     - 不改生产实现
     - 只把这 4 份活跃文档统一切回：
       - `ISSLDiagnostics`
       - `ISSLSessionResumption`
     - 并新增 focused contract 冻结这组 owner-path guidance
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_active_owner_path_docs_alignment_contract.sh`
     - `bash tests/scripts/test_active_owner_path_docs_alignment_contract.sh`
     - `npx prettier --write docs/reference/API_REFERENCE.md docs/guides/WINSSL_BEST_PRACTICES.md docs/guides/PERFORMANCE_PROFILING_GUIDE.md docs/reference/WINSSL_DESIGN.md`
     - `git diff --check`
     - `rg -l '\b(?:Conn|LConn|Conn1|Conn2|Connection|Stream\.Connection)\.(?:GetSession|SetSession|IsSessionReused|GetPerformanceMetrics|GetHealthStatus|GetDiagnosticInfo|IsHealthy)\b' docs/guides docs/reference --glob '!docs/archive/**' --glob '!docs/plans/**' | sort`
   - 当前批收口后的新剩余面：
     - 活跃 `docs/guides` / `docs/reference` 已不再残留 direct-core 连接调用示例
     - 这条线现在只剩：
       - `PERFORMANCE_OPTIMIZATION_GUIDE.md`
         对 direct-core 名字的“解释性提及”，但它已经明确说明这些只是 compatibility mirror，不属于 owner-path drift
   - 当前批收口后默认下一步应为：
     - 继续回到“接口设计 + 各 backend 实现完整性”主轴
     - 优先查：
       - capability matrix / KnownIssues / backend contract 之间是否还有实现或发布边界不一致
       - 活跃 reference/guides 是否还残留固定 capability 结论或 backend-specific old truth
98. `P2 minimum API matrix CT truth` 已完成 focused 收口，并应作为当前 P2 最低 API 矩阵 CT 映射口径的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-p2-minimum-api-matrix-ct-truth.md`
   - 当前已确认的真实 drift：
     - `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md`
       - 顶部结论仍写：
         - `TSSLBackendCapabilities 已能直接表达 PKCS12 / CT`
       - 但 CT 行和特别说明同时又明确：
         - `无默认直接字段映射`
         - `SupportsCertificateTransparency` / `CertTransparencySupport`
           不应当作这组底层 API 的直接映射
   - 当前最小正确修法已落地：
     - 不改生产实现
     - 只把这页的顶部结论改回当前 capability/public truth
     - 并新增 focused contract 冻结 CT 映射口径
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_p2_minimum_api_matrix_ct_truth_contract.sh`
     - `bash tests/scripts/test_p2_minimum_api_matrix_ct_truth_contract.sh`
     - `npx prettier --write docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 继续按 capability truth 主线审查：
       - 其它 matrix / KnownIssues / API reference 是否还有“顶部结论”和具体字段口径打架
       - backend capability 发布面是否还存在 coarse-grained flag 与具体 runtime/public surface 不一致
99. `WinSSL session cache semantic boundary` 已完成 focused 收口，并应作为当前 WinSSL capability/source/runtime 边界的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-winssl-session-cache-semantic-boundary.md`
   - 当前已确认的真实 drift：
     - `docs/reference/API_REFERENCE.md`
       - `TSSLBackendCapabilities` 代码块之前没有完整列出：
         - `SessionCacheSupport`
       - 读取优先级说明也漏掉了：
         - `SessionCacheSupport`
       - 这会把 active interface truth 写成“只看 `SessionTicketsSupport`”，但没有把
         context-level session cache/control surface 单独发布出来
     - `src/fafafa.ssl.base.pas`
       - `SessionCacheSupport` 注释之前只写“会话缓存支持级别”
       - 没有说明它不等于已观测到 resumed handshake
     - `src/fafafa.ssl.winssl.lib.pas`
       - `Result.SessionCacheSupport := sslSupportStable`
         之前缺少紧邻语义注释
       - 容易让后续审查把这个 `stable` 直接误读成 dedicated Windows runtime proof
   - 当前最小正确修法已落地：
     - 不改 WinSSL runtime/handshake 实现
     - 只把 source comment / API reference / WinSSL active docs 明确收紧到：
       - `SessionCacheSupport=sslSupportStable`
         在 WinSSL 上表示 context-level session cache/control surface 已发布且已接线
       - 这不等于当前已经 runtime-proven 的 resumed handshake
       - 当前 dedicated Windows truth 仍看：
         - `observed_reuse=false`
         - `session_configured=true`
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_winssl_session_cache_semantic_boundary_contract.sh`
     - `bash tests/scripts/test_winssl_session_cache_semantic_boundary_contract.sh`
     - `bash tests/scripts/test_winssl_session_cache_runtime_flag_contract.sh`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 不再重开“`SessionCacheSupport=stable` 是否天然等于 resumed-handshake proof”这条线
     - 继续回到更值钱的 WinSSL runtime 端调查：
       - 为什么 same `target name` + same `credential handle` 仍然停在 `observed_reuse=false`
     - 或继续横向审查其它 backend capability/support-level 字段是否还有类似的语义漂移
100. `WinSSL session evidence model truth` 已完成 repo-side focused 收口，并应作为当前 WinSSL runtime 证据链的新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-winssl-session-evidence-model-truth.md`
   - 当前已确认的真实 drift：
     - `src/fafafa.ssl.winssl.connection.pas`
       - `UpdateSessionReuseTruthFromContext(...)`
         当前明确保持：
         - `ASessionId := ''`
         - `FSessionReused := False`
       - 原因不是“已经安全证明 Schannel 不会复用”
       - 而是 canonical shared path 继续撤下 live `SECPKG_ATTR_SESSION_INFO` probe，以避免 GitHub Windows 上的 AV
     - `tests/winssl/test_winssl_session_resumption.pas`
       - summary 虽然已经同时输出：
         - `observed_reuse`
         - `native_observed_reuse`
         - `native_probe_succeeded`
       - 但没有一条稳定 marker 明说当前 evidence model
     - `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md`
     - `tests/windows/VALIDATION_BUNDLE.md`
     - `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
     - `docs/reference/API_REFERENCE.md`
     - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
     - `docs/guides/WINSSL_USER_GUIDE.md`
       - 之前都还容易把：
         - `observed_reuse=false`
         当成“是否真的观测到 resumed handshake”的唯一结论
       - 没有把：
         - shared/public conservative truth
         - opt-in isolated native probe truth
         这两层证据明确拆开
   - 当前最小正确修法已落地：
     - 不改 WinSSL runtime/handshake 实现
     - 让 dedicated proof program 额外输出稳定 marker：
       - `evidence_model public_reuse_truth=conservative_shared_path native_probe_truth=isolated_worker_opt_in`
     - 把 Windows checklist / bundle / status report / WinSSL 高入口说明统一收紧到：
       - `observed_reuse` = shared/public conservative truth
       - `native_observed_reuse` / `native_probe_succeeded` = deeper opt-in native evidence
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_winssl_session_evidence_model_truth_contract.sh`
     - `bash tests/scripts/test_winssl_session_evidence_model_truth_contract.sh`
     - `bash tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
     - `git diff --check`
   - 当前 live follow-up 已拿到最新结果：
     - 新的 GitHub Windows manual lane：
       - run `26104446972`
     - 当前已确认的 fresh runtime evidence：
       - broader suite 的 session-resumption lane 在启用 native probe 后失败
       - `native_probe_worker exit_code=-1073741819`
       - last marker 停在：
         - `native_probe label=initial_handshake stage=before_query_context_attributes`
       - summary 仍是：
         - `observed_reuse=false`
         - `native_probe_enabled=true`
         - `native_observed_reuse=false`
         - `native_probe_succeeded=false`
         - `session_configured=true`
     - 这说明当前更值钱的问题已经继续收窄成：
       - isolated worker / `SECPKG_ATTR_SESSION_INFO` probe 自身仍不安全
       - 而不是 workflow 没跑起来，也不是 broader/shared lane marker 丢失
   - 当前批收口后默认下一步应为：
     - 不再把问题描述成“WinSSL session truth 还不够清楚”
     - 直接静态审查并缩小：
       - isolated worker / `SECPKG_ATTR_SESSION_INFO` probe 的 ABI / lifetime / buffer safety 边界
     - 若能定位 Pascal 绑定或调用约束缺口，就开下一批 source-side 修复
     - 若仍无安全修法，再考虑把 native probe lane 明确降级成更强的 quarantined investigation path
101. `WinSSL native probe safe query path` 已完成 repo-side focused 收口，并应作为当前 isolated native-probe lane 的最新 source-side 基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-winssl-native-probe-safe-query-path.md`
   - 当前已确认的真实缺口：
     - 最新 Windows native-probe run `26104446972`
       已经把 fresh crash boundary 收窄到：
       - `native_probe label=initial_handshake stage=before_query_context_attributes`
       - `native_probe_worker exit_code=-1073741819`
     - 当前 repo source 在 dedicated proof 程序里仍然直接调用：
       - `QueryContextAttributesW(LCtxtHandle, SECPKG_ATTR_SESSION_INFO, @LSessionInfo)`
     - 这意味着 isolated worker 还没有利用官方可选的：
       - `QueryContextAttributesExW(..., cbBuffer)`
       这条更明确的 sized-buffer 查询路径
   - 当前最小正确修法已落地：
     - 不改 canonical shared/public path
     - 只把 `tests/winssl/test_winssl_session_resumption.pas`
       的 native probe 收紧到：
       - 优先动态解析并调用
         - `QueryContextAttributesExW(..., SizeOf(SecPkgContext_SessionInfo))`
       - 若入口不存在，再回退：
         - `QueryContextAttributesW(...)`
       - 同时新增：
         - `stage=query_api api=query_context_attributes_exw|query_context_attributesw`
         evidence marker
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_winssl_native_probe_safe_query_contract.sh`
     - `bash tests/scripts/test_winssl_native_probe_safe_query_contract.sh`
     - `bash tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
     - `bash tests/scripts/test_winssl_session_info_probe_allowlist_contract.sh`
     - `bash tests/scripts/test_winssl_native_probe_stage_markers_contract.sh`
     - `bash tests/scripts/test_winssl_native_probe_handle_metadata_contract.sh`
     - `mkdir -p tmp/winssl_native_probe_safe_query_win64 && fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/winssl_native_probe_safe_query_win64 -FEtmp/winssl_native_probe_safe_query_win64 -otmp/winssl_native_probe_safe_query_win64/test_winssl_session_resumption.exe tests/winssl/test_winssl_session_resumption.pas`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 推送后重新发起一条 `winssl_enable_native_probe=true` 的 Windows manual lane
     - 优先验证这次 `ExW 优先 + W 回退` 是否能把：
       - `native_probe_worker exit_code=-1073741819`
       从 `before_query_context_attributes` 这条边界上拉开
     - 若仍 crash，再继续追：
       - `SECPKG_ATTR_SESSION_INFO` 的 attribute binding / lifetime / provider behavior
102. `WinSSL native probe resolver diagnostics` 已完成 repo-side focused 收口，并应作为当前 `QueryContextAttributesEx*` 解析调查的最新基线保留：
   - 新 plan：
     - `docs/plans/2026-05-19-winssl-native-probe-resolver-diagnostics.md`
   - 当前 fresh runtime evidence：
     - run `26106025515`
       在带上 `ExW 优先 + W 回退` 补丁后仍失败于 wider suite
     - 但关键新事实已经从 log 里显式暴露出来：
       - `stage=query_api api=query_context_attributesw`
       - 说明本次 Windows runner 上 `QueryContextAttributesEx*` 根本没有解析成功
       - crash 仍然停在：
         - `native_probe_worker exit_code=-1073741819`
         - last marker:
           - `stage=query_api api=query_context_attributesw`
   - 当前最小正确修法已落地：
     - 不重开 probe 行为本身
     - 只把 resolver 收紧为：
       - 候选模块/符号遍历
         - `secur32.dll`:
           - `QueryContextAttributesExW`
           - `QueryContextAttributesExA`
           - `QueryContextAttributesEx`
         - `sspicli.dll`:
           - `QueryContextAttributesExW`
           - `QueryContextAttributesExA`
           - `QueryContextAttributesEx`
       - 显式 `PAnsiChar(...)` 调用 `GetProcAddress`
       - 新增 resolver diagnostic marker：
         - `stage=query_resolver module=... symbol=... resolved=...`
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_winssl_native_probe_resolver_diagnostics_contract.sh`
     - `bash tests/scripts/test_winssl_native_probe_resolver_diagnostics_contract.sh`
     - `bash -n tests/scripts/test_winssl_native_probe_safe_query_contract.sh`
     - `bash tests/scripts/test_winssl_native_probe_safe_query_contract.sh`
     - `bash tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
     - `bash tests/scripts/test_winssl_session_info_probe_allowlist_contract.sh`
     - `bash tests/scripts/test_winssl_native_probe_stage_markers_contract.sh`
     - `bash tests/scripts/test_winssl_native_probe_handle_metadata_contract.sh`
     - `mkdir -p tmp/winssl_native_probe_resolver_diag_win64 && fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/winssl_native_probe_resolver_diag_win64 -FEtmp/winssl_native_probe_resolver_diag_win64 -otmp/winssl_native_probe_resolver_diag_win64/test_winssl_session_resumption.exe tests/winssl/test_winssl_session_resumption.pas`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 推送后重新发起 native-probe Windows manual lane
     - 先看 resolver marker：
       - 是哪个 `module/symbol` 被成功解析
       - 还是全部失败
     - 如果全部失败，再把问题继续收窄到：
       - runner 平台缺少导出
       - 或 API 名字/模块 reality 与文档不一致
103. `WinSSL native probe control query boundary` 现在应作为 `ExW 已成功解析但调用仍 crash` 之后的下一条最小调查批次：
   - 新 plan：
     - `docs/plans/2026-05-19-winssl-native-probe-control-query-boundary.md`
   - 当前 fresh runtime evidence：
     - run `26107307586`
       已经明确不是 resolver miss，而是：
       - `stage=query_resolver module=sspicli.dll symbol=QueryContextAttributesExW resolved=true`
       - `stage=query_api api=query_context_attributes_exw`
       - `native_probe_worker exit_code=-1073741819`
   - 当前最小正确修法应先做对照控制，而不是继续盲改 `ExW`：
     - 在相同 extracted native handle 上先跑
       - `QueryContextAttributesW(..., SECPKG_ATTR_CONNECTION_INFO, ...)`
     - 新增 marker：
       - `stage=before_control_query`
       - `stage=after_control_query`
       - `stage=control_query_failed`
   - 这批的调查价值：
     - 若 control query 也崩：
       - 更偏向 handle path / context lifetime 问题
     - 若 control query 先过而 session-info probe 仍崩：
       - 更偏向 `SECPKG_ATTR_SESSION_INFO` 的 attribute-specific provider/runtime boundary
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_winssl_native_probe_control_query_contract.sh`
     - `bash tests/scripts/test_winssl_native_probe_control_query_contract.sh`
     - `bash tests/scripts/test_winssl_native_probe_resolver_diagnostics_contract.sh`
     - `bash tests/scripts/test_winssl_native_probe_safe_query_contract.sh`
     - `bash tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
     - `bash tests/scripts/test_winssl_native_probe_stage_markers_contract.sh`
     - `bash tests/scripts/test_winssl_native_probe_handle_metadata_contract.sh`
     - `bash tests/scripts/test_winssl_session_info_probe_allowlist_contract.sh`
     - `mkdir -p tmp/winssl_native_probe_control_query_win64 && fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/winssl_native_probe_control_query_win64 -FEtmp/winssl_native_probe_control_query_win64 -otmp/winssl_native_probe_control_query_win64/test_winssl_session_resumption.exe tests/winssl/test_winssl_session_resumption.pas`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 推送后重新发起 native-probe Windows manual lane
     - 优先看 control-query marker：
       - 是否到达 `after_control_query`
       - 是否停在 `before_control_query`
       - 或返回 `control_query_failed`
     - 若 control query 先过，再继续看 session-info probe 是否仍停在：
       - `stage=query_api api=query_context_attributes_exw`
104. `WinSSL native probe worker evidence-only` 现在应作为这条 attribute-specific crash 已经被充分证明后的默认收口批次：
   - 新 plan：
     - `docs/plans/2026-05-19-winssl-native-probe-worker-evidence-only.md`
   - 当前 fresh runtime evidence：
     - run `26108237632`
       已明确：
       - `before_control_query`
       - `after_control_query status=0x0`
       - `query_resolver module=sspicli.dll symbol=QueryContextAttributesExW resolved=true`
       - 最后仍停在：
         - `stage=query_api api=query_context_attributes_exw`
         - `native_probe_worker exit_code=-1073741819`
   - 当前语义判断：
     - handle path 已被 control query 证明可用
     - 崩溃点已收窄为：
       - `SECPKG_ATTR_SESSION_INFO` 的 attribute-specific provider/runtime boundary
   - 当前最小正确修法：
     - 默认只把 worker 非零退出记为 evidence
     - 仅 `FAFAFA_WINSSL_REQUIRE_NATIVE_REUSE=1` 时继续严格失败
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_winssl_native_probe_worker_evidence_only_contract.sh`
     - `bash tests/scripts/test_winssl_native_probe_worker_evidence_only_contract.sh`
     - `bash tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
     - `bash tests/scripts/test_winssl_native_probe_control_query_contract.sh`
     - `bash tests/scripts/test_winssl_native_probe_safe_query_contract.sh`
     - `bash tests/scripts/test_winssl_native_probe_resolver_diagnostics_contract.sh`
     - `bash tests/scripts/test_winssl_native_probe_handle_metadata_contract.sh`
     - `mkdir -p tmp/winssl_native_probe_worker_evidence_only_win64 && fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/winssl_native_probe_worker_evidence_only_win64 -FEtmp/winssl_native_probe_worker_evidence_only_win64 -otmp/winssl_native_probe_worker_evidence_only_win64/test_winssl_session_resumption.exe tests/winssl/test_winssl_session_resumption.pas`
     - `git diff --check`
   - 当前批收口后默认下一步应为：
     - 推送后重新发起 native-probe Windows manual lane
     - 验证在默认 `require_native_reuse=false` 下：
       - Windows quick smoke 仍 PASS
       - Windows Wave B gate 仍 PASS
       - broader WinSSL runtime suite 由 FAIL 转为 PASS
       - native probe marker 仍完整保留在 transcript 中
   - 最新 runtime verification：
     - run `26108902159`
       已经完成上述验证：
       - Windows quick smoke = PASS
       - Windows Wave B gate = PASS
       - broader WinSSL runtime suite = PASS
     - 因此 WinSSL native probe 主线当前应视为：
       - Windows mainline unblocked
       - remaining failure moved off this lane and back to macOS-specific gate work
105. `MbedTLS async capability doc truth alignment` 这批现在应作为一个已验证通过、等待提交的最小收口批次保留：
   - 新 plan：
     - `docs/plans/2026-05-20-mbedtls-async-capability-doc-truth-alignment.md`
   - 当前 source / API truth：
     - `src/fafafa.ssl.base.pas`
       已发布：
       - `WantRead`
       - `WantWrite`
     - `src/fafafa.ssl.mbedtls.connection.pas`
       当前明确以：
       - `MBEDTLS_ERR_SSL_WANT_READ`
       - `MBEDTLS_ERR_SSL_WANT_WRITE`
       驱动：
       - `DoWantRead`
       - `DoWantWrite`
     - `tests/test_mbedtls_framework.pas`
       已冻结：
       - `ERR_SSL_WANT_READ -> sslErrWantRead`
       - `ERR_SSL_WANT_WRITE -> sslErrWantWrite`
   - 当前最小修正：
     - 把 `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
       中
       - `| 异步操作 | ⚠️ 部分 | 非阻塞 I/O |`
       收紧为
       - `当前 public surface 通过 WantRead / WantWrite 暴露非阻塞重试语义；没有 dedicated async callback / job public capability`
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_mbedtls_async_capability_doc_truth_contract.sh`
     - `bash tests/scripts/test_mbedtls_async_capability_doc_truth_contract.sh`
     - `git diff --check`
   - 当前外部流程状态：
     - GitHub Actions `WinSSL Runtime Gate`
       run `26130501368`
       已 `success`
     - 说明 Windows / WinSSL runtime proof
       现在已经有自动 lane 承接，
       不必再把这条线当成“只能静态审查”的阻塞理由
   - 当前批收口后默认下一步应为：
     - 继续同一组 residual doc-truth 审查，
       优先处理：
       - `MbedTLS Ed25519`
       - `WinSSL Windows 7 SP1` 平台支持表述
106. `MbedTLS Ed25519 capability doc truth alignment` 这批现在应作为继续压缩 residual doc-truth 队列的最新收口批次保留：
   - 新 plan：
     - `docs/plans/2026-05-20-mbedtls-ed25519-capability-doc-truth-alignment.md`
   - 当前 source / public-surface truth：
     - `src/fafafa.ssl.mbedtls.lib.pas`
       当前 capability record 只发布：
       - `sslKexRSA`
       - `sslKexDHE_RSA`
       - `sslKexECDHE_RSA`
       - `sslKexECDHE_ECDSA`
     - `src/fafafa.ssl.mbedtls.certificate.pas`
       当前证书算法 metadata 仍返回默认值：
       - `GetPublicKeyAlgorithm -> 'RSA'`
       - `GetSignatureAlgorithm -> 'SHA256withRSA'`
     - `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
       之前却仍写：
       - `| Ed25519 | ⚠️ 部分 | MbedTLS 3.x |`
   - 当前最小修正：
     - 把 `Ed25519`
       这一行收紧为：
       - `❌ 当前 capability 不发布`
       - 并明确：
         - 当前 backend 没有 published `Ed25519`-specific capability / metadata surface
         - 不要把上游 MbedTLS 3.x 理论能力当成 fafafa.ssl 当前 backend truth
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_mbedtls_ed25519_capability_doc_truth_contract.sh`
     - `bash tests/scripts/test_mbedtls_ed25519_capability_doc_truth_contract.sh`
     - `git diff --check`
   - 当前外部流程状态：
     - GitHub Actions `CI`
       run `26130974672`
       已 `success`
     - GitHub Actions `WinSSL Runtime Gate`
       run `26130501368`
       已 `success`
   - 当前路线图进度判断：
     - Windows / WinSSL 自动 runtime 验证已闭环到主线 workflow
     - Linux / FreePascal 当前主 CI 也保持绿色
     - repo 当前最高价值路径继续回到：
       - residual capability / doc drift 收口
       - 更大的 interface-design / backend completeness 审查
   - 当前批收口后默认下一步应为：
     - 继续同一组 residual doc-truth 审查：
       - `WinSSL Windows 7 SP1` 平台支持表述
     - 然后切回更大的 completeness 主线：
       - `ISSLConnection`
       - `TSSLConfig`
       - `ISSLServerConnection`
107. `WinSSL platform support doc truth alignment` 这批现在应作为把 `Windows 7 SP1 / 1903+` 平台口径完全收回到 source truth 的最新收口批次保留：
   - 新 plan：
     - `docs/plans/2026-05-20-winssl-platform-support-doc-truth-alignment.md`
   - 当前 source truth：
     - `src/fafafa.ssl.winssl.lib.pas`
       当前明确：
       - `Initialize`: `Windows Vista+`
       - `TLS 1.1 / 1.2`: `Windows 7+`
       - `TLS 1.3`: `Windows 10 Build 18362+`
   - 当前 active-doc drift：
     - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
       之前仍把：
       - `Windows 7 SP1`
         写成：
         - `⚠️ 部分 | 需更新`
       - `Windows Server 2019`
         写成：
         - `TLS 1.3 = ⚠️`
     - `docs/PLATFORM_SUPPORT.md`
       之前仍写：
       - `Windows 10 20348+ 或 Windows 11`
     - `docs/ZERO_DEPENDENCY_DEPLOYMENT.md`
       之前仍按：
       - `20348`
         切 Windows 10 的 TLS 1.3 门槛
   - 当前最小修正：
     - 把 WinSSL dedicated matrix 收紧为：
       - `Windows 7 SP1 = ✅ 支持 / TLS 1.0/1.1/1.2`
       - `Windows Server 2019 = ✅ 支持 / TLS 1.2`
     - 把活跃文档里的 `20348+`
       统一改回：
       - `18362`
       - `Windows 10 1903+`
   - 当前 focused proof 已覆盖：
     - `bash -n tests/scripts/test_winssl_platform_support_doc_truth_contract.sh`
     - `bash tests/scripts/test_winssl_platform_support_doc_truth_contract.sh`
     - `git diff --check`
   - 当前外部流程状态：
     - GitHub Actions `CI`
       run `26131189318`
       已 `success`
     - GitHub Actions `WinSSL Runtime Gate`
       run `26130501368`
       已 `success`
   - 当前路线图进度判断：
   - 当前主要 WinSSL 平台支持口径 drift 已收口
   - 近期最高价值的 residual doc-truth 队列已进一步缩短
   - 下一步默认应切回更大的 completeness 主线：
     - `ISSLConnection`
     - `TSSLConfig`
     - `ISSLServerConnection`
108. `capability support-level serialization precedence` 这批用于把 capability dual-truth 的最后一条 legacy-only round-trip 漂移口正式钉死：
   - 新 plan：
     - `docs/plans/2026-05-20-capability-support-level-serialization-precedence.md`
   - 当前静态问题：
     - `src/fafafa.ssl.capability.serializer.pas`
       之前无条件导出：
       - `sniSupport`
       - `ocspStaplingSupport`
       - `sessionTicketsSupport`
       - 以及其它 `*Support`
     - 这会让 pure legacy-only in-memory record
       在 JSON/XML 输出时凭空带上
       `none`
       级别 truth，
       随后在反序列化路径上把：
       - `SupportsSNI=True`
       - `SupportsOCSPStapling=True`
       - `SupportsSessionTickets=False`
       这类旧布尔真相反向覆盖掉
   - 当前最小修法：
     - serializer 先准备本地副本
     - 只有 record 已携带 support-level truth 时，
       才继续显式导出 `*Support` 视图
     - pure legacy-only record
       保持只导出 legacy boolean，
       不再被 synthetic `none` 污染 round-trip
   - 当前 focused proof：
     - `fpc -B -Fu./src -Fu./tests -otmp/test_capability_serialization_support_level_truth tests/test_capability_serialization_support_level_truth.pas`
     - `./tmp/test_capability_serialization_support_level_truth`
     - `mkdir -p tmp/cap_roundtrip`
     - `fpc -B -Fu./src -Fu./tests -FUtmp/cap_roundtrip -FEtmp/cap_roundtrip -otest_capability_deserialization_roundtrip tests/test_capability_deserialization_roundtrip.pas`
     - `./tmp/cap_roundtrip/test_capability_deserialization_roundtrip`
   - 当前结论：
     - support-level-aware record
       继续显式输出 `*Support`
       并保持它是 round-trip 真相源
     - legacy-only record
       现在也能保持原有布尔真相
     - 由于 live backend producer
       已统一发布完整 support-level matrix，
       这批收口不会打穿当前 runtime/export 面
   - 当前批收口后的默认下一步：
     - 回到
       `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
       的剩余大项
     - 优先继续：
       - `ISSLConnection` slimming
       - `TSSLConfig` scope splitting
       - facade/export historical path classification
109. `managed result init safety` 这批用于把 public facade / shared connection base 里的 managed-result 初始化坑收成当前真实安全边界：
   - 新 plan：
     - `docs/plans/2026-05-20-managed-result-init-safety.md`
   - 当前新发现：
     - `src/fafafa.ssl.pas`
       的
       `CreateDefaultConfig(...)`
       fallback
       仍对 managed `TSSLConfig`
       使用
       `FillChar(Result, SizeOf(Result), 0);`
     - `src/fafafa.ssl.connection.base.pas`
       里的：
       - `GetConnectionInfo`
       - `GetDiagnosticInfo`
       - `DoGetOCSPResponse`
       - `DoGetSignedCertificateTimestampList`
       仍会触发
       `managed type result variable does not seem to be initialized`
       warning
   - 当前最小修法：
     - `TSSLConfig` / `TSSLConnectionInfo` / `TSSLDiagnosticInfo`
       改为
       `Default(...)`
     - 空 `TBytes`
       返回改为
       `Result := nil`
   - 当前 focused proof：
     - `bash tests/scripts/test_managed_result_init_safety_contract.sh`
     - `fpc ... tests/config/test_default_config.pas`
     - `./tmp/defaultcfg_bin/test_default_config`
     - `fpc ... tests/test_connection_builder_hostname_precedence.pas`
     - `./tmp/conninfo_bin/test_connection_builder_hostname_precedence`
   - 当前结论：
     - shared public surface 上这组 managed-result 初始化坑已经收口
     - 上一批 push 的主线 CI
       `26161586399`
       也已 `success`
   - 当前批收口后的默认下一步：
     - 回到更高层的接口路线：
       - `ISSLConnection` v1.x compatibility-core 剩余面是否还有未分类 generic 入口
       - 或继续盘点高可见 public 单元里其它 managed-result warning 是否也落在 shared/helper 路径上
110. `managed result init safety wave2` 这批用于继续把同类 warning 从 shared TLS13 / session 基础件里收掉：
   - 新 plan：
     - `docs/plans/2026-05-20-managed-result-init-safety-wave2.md`
   - 当前新发现：
     - `src/fafafa.ssl.tls13.wire.pas`
       的
       `BuildTLSPlaintext(...)`
       会在未显式初始化
       `Result: TBytes`
       前直接
       `SetLength(...)`
     - `src/fafafa.ssl.freepascal.session.pas`
       的：
       - `ReadVector16(...)`
       - `TFreePascalSession.Serialize(...)`
       也存在同类写法
   - 当前最小修法：
     - 这些 shared helper / session path
       统一先
       `Result := nil`
       再进入长度分配或 append 逻辑
   - 当前 focused proof：
     - `bash tests/scripts/test_managed_result_init_safety_wave2_contract.sh`
     - `fpc ... tests/test_tls13_foundation.pas`
     - `./tmp/tls13_foundation_bin/test_tls13_foundation`
     - `fpc ... tests/test_freepascal_client_session_resumption.pas`
     - `./tmp/fp_session_bin/test_freepascal_client_session_resumption`
   - 当前结论：
     - `tls13.wire`
       与
       `freepascal.session`
       这三条 managed-result warning 已收口
     - 上一批 managed-result-init push 后的主 CI
       `26163273748`
       现已完整 `success`：
       - `Code Quality (Light)` success
       - `Minimal Gate (Linux)` success
       - `FreePascal TLS 1.3 Completeness` success
   - 当前批收口后的默认下一步：
     - 继续优先清 shared/public implementation
       里同类真实 warning 残口
     - 若这条线收益开始下降，再切回
       `ISSLConnection` remaining generic entry classification
111. `managed result init safety wave3` 这批用于继续把同类 warning 从 shared TLS13 primitives / constant-time helper 里收掉：
   - 新 plan：
     - `docs/plans/2026-05-20-managed-result-init-safety-wave3.md`
   - 当前新发现：
     - `src/fafafa.ssl.tls13.primitives.pas`
       里的：
       - `CopyBytes(...)`
       - `ConcatBytes(...)`
       - `BuildTLS13HKDFLabel(...)`
       - `HKDF_Expand_SHA256(...)`
       - `HKDF_Expand_SHA384(...)`
       都还存在
       managed `TBytes` result
       在显式初始化前直接
       `SetLength(...)`
       或
       `SetLength(Result, 0)`
       的写法
     - `src/fafafa.ssl.crypto.constant_time.pas`
       的
       `TConstantTime.Select(...)`
       也存在同类写法
   - 当前最小修法：
     - 这些 shared helper
       统一先
       `Result := nil`
       再进入长度分配
       或 append 逻辑
     - 零长度返回分支
       不再通过未初始化 result 上的
       `SetLength(Result, 0)`
       兜底
   - 当前 focused proof：
     - `bash -n tests/scripts/test_managed_result_init_safety_wave3_contract.sh`
     - `bash tests/scripts/test_managed_result_init_safety_wave3_contract.sh`
     - `fpc ... tests/test_tls13_foundation.pas`
     - `./tmp/tls13_foundation_bin/test_tls13_foundation`
     - `fpc ... tests/unit/test_constant_time.pas`
     - `./tmp/constant_time_bin/test_constant_time`
     - `python3 scripts/compile_all_modules.py | rg -n "tls13\\.primitives|crypto\\.constant_time|Warning:"`
   - 当前结论：
     - `tls13.primitives`
       与
       `crypto.constant_time`
       这批 managed-result warning
       已从 focused compile / broader compile grep 里收口
     - `test_constant_time`
       的
       `Select`
       功能断言保持绿色，
       但整套测试仍带一个旧的统计型 timing gate：
       - 基于
         `GetTickCount64`
       - 要求
         `< 5%`
         方差
       这在快机器上会伪失败，
       属于独立的测试稳定性问题，
       不是这批初始化修复带来的行为回归
   - 当前批收口后的默认下一步：
     - 若继续沿 warning ROI 前进，
       优先看：
       - `fafafa.ssl.tls13.keyschedule.pas(228,19)`
       - `fafafa.ssl.tls13.clienthello.pas`
         剩余那组 managed-result warning
     - 若要切回更高层 completeness 主线，
       则回到：
       - `ISSLConnection`
       - `TSSLConfig`
       - `ISSLServerConnection`
112. `managed result init safety wave4` 这批用于继续把同类 warning 从 shared TLS13 key-schedule / ClientHello builder 里收掉：
   - 新 plan：
     - `docs/plans/2026-05-20-managed-result-init-safety-wave4.md`
   - 当前新发现：
     - `src/fafafa.ssl.tls13.keyschedule.pas`
       里的：
       - `HashTranscriptForSuite(...)`
       - `HKDFExtractForSuite(...)`
       - `HKDFExpandLabelForSuite(...)`
       - `TLS13ComputePSKBinderForCipherSuite(...)`
       还留着同家族的空 `TBytes` result 初始化写法
     - `src/fafafa.ssl.tls13.clienthello.pas`
       里的：
       - `BuildExtensionServerName(...)`
       - `BuildExtensionALPN(...)`
       - `BuildExtensionPreSharedKey(...)`
       - `BuildTLS13ClientHelloBody(...)`
       - `BuildTLS13ClientHelloBodyWithPSKCore(...)`
       - `BuildTLS13ClientHelloHandshake(...)`
       - `BuildTLS13ClientHelloHandshakeWithPSK(...)`
       - `BuildTLS13ClientHelloHandshakeWithComputedPSKBinder(...)`
       也还在通过
       `SetLength(Result, 0)`
       或未显式初始化 result 后直接进入 append 路径
   - 当前最小修法：
     - 这些 TLS13 shared helper / builder
       统一先
       `Result := nil`
     - unsupported/invalid/empty 的 fast path
       直接保留空 `nil` 结果并
       `Exit`
     - 删除目标函数里对 result 的
       `SetLength(Result, 0)`
       兜底
   - 当前 focused proof：
     - `bash -n tests/scripts/test_managed_result_init_safety_wave4_contract.sh`
     - `bash tests/scripts/test_managed_result_init_safety_wave4_contract.sh`
     - `fpc ... tests/test_tls13_foundation.pas`
     - `./tmp/tls13_foundation_bin/test_tls13_foundation`
     - `fpc ... tests/test_tls13_resumption.pas`
     - `./tmp/tls13_resumption_bin/test_tls13_resumption`
     - `fpc ... tests/test_tls13_foundation.pas 2>&1 | rg "tls13\\.keyschedule|tls13\\.clienthello|Warning: Function result variable of a managed type does not seem to be initialized"`
   - 当前结论：
     - `tls13.keyschedule`
       与
       `tls13.clienthello`
       这批 managed-result warning
       已从 focused compile 中收口
     - 普通 ClientHello、
       PSK ClientHello、
       binder transcript /
       resumption 基础回归
       都保持绿色
     - 当前 compile 剩余 warning
       已经移到下一层：
       - `fafafa.ssl.tls13.appschedule.pas`
       - `fafafa.ssl.tls13.serverhello.pas`
       - `tests/test_tls13_resumption.pas`
         自己的 helper
   - 当前批收口后的默认下一步：
     - 若继续沿 warning ROI 前进，
       优先看：
       - `fafafa.ssl.tls13.appschedule.pas`
       - `fafafa.ssl.tls13.serverhello.pas`
     - 若切回更高层 completeness 主线，
       则回到：
       - `ISSLConnection`
       - `TSSLConfig`
       - `ISSLServerConnection`
113. `managed result init safety wave5` 这批用于继续把同类 warning 从 shared TLS13 application-schedule / ServerHello builder 里收掉，并顺手清理同批测试 helper：
   - 新 plan：
     - `docs/plans/2026-05-20-managed-result-init-safety-wave5.md`
   - 当前新发现：
     - `src/fafafa.ssl.tls13.appschedule.pas`
       里的：
       - `TLS13ComputeResumptionMasterSecretFromTranscriptHash(...)`
       - `TLS13DeriveResumptionPSKFromTranscriptHash(...)`
       仍对 managed `TBytes` result
       直接
       `SetLength(Result, 0)`
     - 同单元里的：
       - `HashTranscriptForSuite(...)`
       - `HKDFExtractForSuite(...)`
       - `HKDFExpandLabelForSuite(...)`
       也还保留同家族 empty-result 兜底
     - `src/fafafa.ssl.tls13.serverhello.pas`
       里的：
       - `BuildExtensionHeader(...)`
       - `BuildTLS13ServerHelloBody(...)`
       - `BuildTLS13ServerHelloHandshake(...)`
       - `BuildTLS13ServerHelloHandshakeWithSelectedPSK(...)`
       也还在通过
       `SetLength(Result, 0)`
       或未显式初始化 result 后直接进入 append 路径
     - `tests/test_tls13_resumption.pas`
       的
       `HexToBytes(...)`
       也属于同家族测试 helper warning
   - 当前最小修法：
     - 这些生产 helper / builder
       统一先
       `Result := nil`
     - unsupported / invalid / empty 的 fast path
       直接保留空 `nil` 结果并
       `Exit`
     - 删掉目标函数里对 result 的
       `SetLength(Result, 0)`
       兜底
     - `test_tls13_resumption`
       的
       `HexToBytes(...)`
       同步改成
       `Result := nil`
       后再分配
   - 当前 focused proof：
     - `bash -n tests/scripts/test_managed_result_init_safety_wave5_contract.sh`
     - `bash tests/scripts/test_managed_result_init_safety_wave5_contract.sh`
     - `fpc ... tests/test_tls13_appschedule.pas`
     - `./tmp/tls13_appschedule_bin/test_tls13_appschedule`
     - `fpc ... tests/test_tls13_serverhello_builder.pas`
     - `./tmp/tls13_serverhello_bin/test_tls13_serverhello_builder`
     - `fpc ... tests/test_tls13_resumption.pas`
     - `./tmp/tls13_resumption_bin/test_tls13_resumption`
     - `fpc ... tests/test_tls13_resumption.pas 2>&1 | rg "tls13\\.appschedule|tls13\\.serverhello|test_tls13_resumption|Warning: Function result variable of a managed type does not seem to be initialized"`
     - `python3 scripts/compile_all_modules.py 2>&1 | rg "Warning: Function result variable of a managed type does not seem to be initialized"`
   - 当前结论：
     - `tls13.appschedule`
       与
       `tls13.serverhello`
       这批 managed-result warning
       已从 focused compile 中收口
     - `tests/test_tls13_resumption.pas`
       自己的 helper warning
       也一起收口
     - `test_tls13_appschedule`
       /
       `test_tls13_serverhello_builder`
       /
       `test_tls13_resumption`
       都保持绿色
     - broader grep
       没有再匹配到
       `Function result variable of a managed type does not seem to be initialized`
       这类 warning；
       说明这条
       managed-result warning
       主线当前已经基本收口
   - 当前批收口后的默认下一步：
     - warning 清扫主线可暂时降级，
       优先切回更高层 completeness 主线：
       - `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
         里的
         `ISSLConnection`
         /
         `TSSLConfig`
         /
         `ISSLServerConnection`
         残口
     - 若继续做静态编译 hygiene，
       下一类更真实的 residual
       是：
       - implicit string conversion warnings
         （`crypto.hash` / `crypto.constant_time`）
       - `test_constant_time`
         的 timing-flaky gate
114. `capability public truth freeze` 这批用于把公开 capability 入口还残留的 legacy-bool-first 叙事正式收口：
   - 新 plan：
     - `docs/plans/2026-05-20-capability-public-truth-freeze.md`
   - 当前静态问题：
     - `src/fafafa.ssl.base.pas`
       里的
       `TSSLBackendCapabilities`
       record
       虽然已有
       `NormalizeLegacyCapabilityBooleans(...)`
       helper 注释，
       但 record 自身还没把 paired feature truth model 直接写出来
     - `docs/BACKEND_CAPABILITY_MATRIX.md`
       仍有几处活跃入口叙事
       先用 legacy bool 解释当前 truth：
       - `SupportsALPN=True` / `SupportsSNI=True`
       - `SupportsOCSPStapling=False`
       - `SupportsCertificateTransparency=False`
     - `docs/MIGRATION_GUIDE_V1.1.md`
       的迁移示例仍在用：
       - `TSSLFactory.GetLibrary(...)`
       - `Caps.SupportsALPN`
   - 当前最小修法：
     - 在
       `TSSLBackendCapabilities`
       record
       顶部直接声明：
       - paired feature
         以
         `*Support`
         为主真相
       - legacy
         `Supports*`
         只是由
         `NormalizeLegacyCapabilityBooleans(...)`
         回填的 compatibility projection
       - `SupportsTLS13`
         仍是当前唯一明确保留的主 bool truth
     - 把
       `BACKEND_CAPABILITY_MATRIX`
       的
       `FreePascal ALPN/SNI`
       /
       `WinSSL OCSP`
       /
       `OpenSSL CT`
       说明改成 support-level-first
     - 把
       `MIGRATION_GUIDE_V1.1`
       的能力示例改成：
       - `GetLibraryInstance(...)`
       - `ALPNSupport`
       - `SNISupport`
       - paired feature 读法说明
     - 扩
       `tests/scripts/test_capability_precedence_docs_truth_contract.sh`
       去锁住上述入口
   - 当前 focused proof：
     - `bash -n tests/scripts/test_capability_precedence_docs_truth_contract.sh`
     - `bash tests/scripts/test_capability_precedence_docs_truth_contract.sh`
     - `git diff --check`
   - 当前结论：
     - 这批不需要重开 runtime / backend producer 实现，
       因为真正剩余的 drift
       已经收窄成
       public source/doc narration
     - capability dual-truth
       在 runtime/source/serializer/diff
       层面的主真相已经基本收口；
       当前更值钱的是把入口话术一起锁死
     - 当前本地 focused proof
       已完成，
       并已形成提交：
       - `fe435c4`
         `docs(capability): freeze public support-level truth`
   - 当前批收口后的默认下一步：
     - 当前上一批
       `2582cac`
       的远端
       `TLS13 Signer Gate`
       /
       `CI`
       已确认转绿
     - 当前新 push
       `fe435c4`
       已触发：
       - `26167070948`
         `CI`
         `in_progress`
     - 若这轮新 CI 转绿，
       则继续回到：
       - `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
         里的
         `ISSLConnection`
         /
         `TSSLConfig`
         /
         `ISSLServerConnection`
         completeness 主线
     - 若远端红灯，
       只修红灯，
       不再扩 capability 范围
115. `facade capability/native-handle export closure` 这批用于修复主门面 `fafafa.ssl` 的一个真实 public compile gap：
   - 新 plan：
     - `docs/plans/2026-05-20-facade-capability-native-handle-export-closure.md`
   - 当前新发现：
     - `src/fafafa.ssl.pas`
       顶部注释仍写着
       “导出所有公共接口和类型”
     - 但只
       `uses fafafa.ssl`
       再写 capability / native-handle 基础调用时，
       当前会直接编译失败：
       - `TSSLBackendCapabilities`
       - `TSSLBackendImplType`
       - `TSSLFeatureSupportLevel`
       - `ISSLNativeHandleAccess`
       - `IsFeatureStable(...)`
       - `GetCapabilitiesDescription(...)`
       - 以及 capability helper 依赖的 enum values
         如：
         `sslCipherAES256GCM`
         /
         `sslHashSHA256`
         /
         `sslKexECDHE_RSA`
     - 这说明主门面对这组已发布 public surface
       仍存在真实出口缺口，
       不只是文档叙事问题
   - 当前最小修法：
     - 在
       `src/fafafa.ssl.pas`
       补齐 capability / native-handle 相关：
       - type re-export
       - interface re-export
       - enum value const re-export
       - helper function forwarding
     - 覆盖至少这几类：
       - `TSSLBackendCapabilities`
       - `TSSLBackendImplType`
       - `TSSLFeatureSupportLevel`
       - `TSSLFeature`
       - `TSSLFeatures`
       - `TSSLCipherSupport`
       - `TSSLHashSupport`
       - `TSSLKeyExchangeSupport`
       - `ISSLNativeHandleAccess`
       - capability helper functions
     - 在
       `API_REFERENCE`
       补一句：
       - 主门面
         `fafafa.ssl`
         也 re-export
         capability / native-handle public surface
     - 用 compile-based focused contract 锁住：
       - source re-export truth
       - `uses fafafa.ssl`
         的最小 capability/native-handle probe
         可编译并运行
   - 当前 focused proof：
     - `bash -n tests/scripts/test_facade_capability_native_handle_export_contract.sh`
     - `bash tests/scripts/test_facade_capability_native_handle_export_contract.sh`
     - `bash tests/scripts/test_facade_optional_owner_surface_export_contract.sh`
     - `bash tests/scripts/test_facade_main_entry_truth_contract.sh`
     - `git diff --check`
   - 当前结论：
     - 这条缺口已经从“门面自称导出全部 public surface”
       收口到真实 compile proof
     - 最新两轮远端 CI：
       - `26167070948`
       - `26167259858`
       也都已转绿，
       当前可以继续从接口完整性主线前进，
       不需要再被上一批 gate 卡住
   - 当前批收口后的默认下一步：
     - 继续回到
       `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
       的 completeness 主线
     - 优先再找
       “已有 public surface 仍未真正从主门面闭合”
       或
       “源码/文档已说明但缺 compile/runtime contract”
       的残口
116. `facade certificate supporting-type export closure` 这批用于修复主门面 `fafafa.ssl` 的下一条真实 public compile gap：
   - 新 plan：
     - `docs/plans/2026-05-20-facade-certificate-supporting-type-export-closure.md`
   - 当前新发现：
     - `src/fafafa.ssl.pas`
       顶部注释仍写着
       “导出所有公共接口和类型”
     - 但只
       `uses fafafa.ssl`
       再声明当前证书 public surface 常用 supporting types 时，
       仍会直接编译失败：
       - `TSSLStringArray`
       - `TSSLCertVerifyResult`
     - 这两种类型并不是边缘内部类型，
       而是当前 shipped surface 已直接使用的公共签名：
       - `ISSLCertificate.GetSubjectAltNames`
       - `ISSLCertificate.GetKeyUsage`
       - `ISSLCertificate.GetExtendedKeyUsage`
       - `ISSLCertificate.VerifyEx(...)`
     - `docs/reference/API_REFERENCE.md`
       也已经把这两种类型作为 canonical truth
       直接展示出来
   - 当前最小修法：
     - 在
       `src/fafafa.ssl.pas`
       补齐：
       - `TSSLStringArray`
       - `TSSLCertVerifyResult`
       的 type re-export
     - 在
       `API_REFERENCE`
       补一句：
       - 主门面
         `fafafa.ssl`
         也 re-export
         证书 public surface 常用 supporting types
     - 用 compile-based focused contract 锁住：
       - source re-export truth
       - `uses fafafa.ssl`
         的最小 supporting-type probe
         可编译并运行
   - 当前 focused proof：
     - `bash -n tests/scripts/test_facade_certificate_supporting_types_export_contract.sh`
     - `bash tests/scripts/test_facade_certificate_supporting_types_export_contract.sh`
     - `git diff --check`
   - 当前最终收口证据：
     - focused contract
       首轮 RED
       直接打出：
       - `main facade must re-export TSSLStringArray`
     - 这说明缺口不是文档抽象判断，
       而是 facade-only compile proof
       当场失败
     - 最小修复后：
       - `src/fafafa.ssl.pas`
         已补齐：
         - `TSSLStringArray`
         - `TSSLCertVerifyResult`
       - `docs/reference/API_REFERENCE.md`
         已补一条主门面 supporting-type 覆盖说明
     - 相邻门面 contract
       继续全绿：
       - `test_facade_optional_owner_surface_export_contract.sh`
       - `test_facade_capability_native_handle_export_contract.sh`
   - focused verification 已通过：
     - `bash -n tests/scripts/test_facade_certificate_supporting_types_export_contract.sh`
     - `bash tests/scripts/test_facade_certificate_supporting_types_export_contract.sh`
     - `bash tests/scripts/test_facade_optional_owner_surface_export_contract.sh`
     - `bash tests/scripts/test_facade_capability_native_handle_export_contract.sh`
     - `git diff --check`
   - 当前结论：
     - 这批继续证明：
       `fafafa.ssl`
       主门面的完整性问题
       不是一次性收完的，
       而是需要 compile-based probing
       去逐条把 supporting types /
       owner surface /
       capability surface
       闭合
     - 但到这一批为止，
       证书 public surface 里最常用的 supporting-type compile gap
       已经收口
   - 当前总路线图进度：
     - `接口设计`
       主线已经从纯静态批评
       进入
       “活跃 public surface
       是否真的 compile-closed”
       的实证阶段
     - `后端实现`
       当前不需要为这条问题改 backend runtime，
       说明这次 residual
       确实在 facade/interface completeness
       而不是 producer side
     - `测试与文档`
       本批继续补齐：
       - focused compile contract
       - canonical API reference truth
       - docs/plans + working-memory 台账
   - 当前批收口后的默认下一步：
     - 继续回到
       `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
       的 completeness 主线
     - 优先再找：
       - 其它已发布 public type / interface
         是否仍未真正从主门面闭合
       - 或 source / canonical docs
         已说明存在，
         但还缺 focused compile/runtime contract
         的残口
117. `certificate verifyhostname fixture parity` 这批用于把 `ISSLCertificate.VerifyHostname(...)` 的高风险 fixture proof 补齐到当前主要 backend：
   - 新 plan：
     - `docs/plans/2026-05-20-certificate-verifyhostname-fixture-parity.md`
   - 当前新发现：
     - `FreePascal`
       已经有 focused fixture proof：
       - `san_cn_conflict_cert.pem`
       - `san_wildcard_cert.pem`
     - 但
       `MbedTLS` /
       `WolfSSL` /
       `OpenSSL` /
       `WinSSL`
       还缺同级证明，
       通用 contract
       只覆盖
       `san-test.pem`
       的基础 SAN/IP 命中
     - `MbedTLS` /
       `WolfSSL`
       的 `VerifyHostname(...)`
       真实存在两条实现缺陷：
       - SAN 不匹配时仍错误回退到 CN
       - wildcard SAN
         会被
         `IsValidHostname(...)`
         预过滤掉
     - `WinSSL`
       的实现模式同样共享这两个风险，
       而且现有
       `test_winssl_certificate_san.pas`
       还额外有：
       - 夹具路径和 runtime script 工作目录不一致
       - `.lpi`
         仍硬编码
         `TargetOS=linux`
   - 当前最小修法：
     - 在
       `tests/test_mbedtls_framework.pas`
       和
       `tests/test_wolfssl_framework.pas`
       加入：
       - SAN-vs-CN precedence
       - wildcard 单层匹配
       focused fixture assertions
     - 新增
       `tests/openssl/test_openssl_certificate_hostname_contract.pas`
       作为 OpenSSL 最小 focused contract
     - 在
       `src/fafafa.ssl.mbedtls.certificate.pas`
       `src/fafafa.ssl.wolfssl.certificate.pas`
       `src/fafafa.ssl.winssl.certificate.pas`
       收紧同一组语义：
       - 只有当证书没有 relevant SAN 时才允许 CN fallback
       - wildcard SAN
         允许进入 hostname pattern matching
     - 把
       `tests/winssl/test_winssl_certificate_san.pas`
       接到真实 runtime lane：
       - repo-relative fixture path resolution
       - `.lpi`
         去掉错误的
         `TargetOS=linux`
       - `tests/run_winssl_tests.ps1`
         纳入执行列表
   - 当前 focused proof：
     - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_mbedtls_framework_units -FEtmp/test_mbedtls_framework_units -otmp/test_mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas`
     - `./tmp/test_mbedtls_framework_units/test_mbedtls_framework`
     - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_framework_units -FEtmp/test_wolfssl_framework_units -otmp/test_wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas`
     - `./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
     - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_openssl_hostname_units -FEtmp/test_openssl_hostname_bin -otmp/test_openssl_hostname_bin/test_openssl_certificate_hostname_contract tests/openssl/test_openssl_certificate_hostname_contract.pas`
     - `./tmp/test_openssl_hostname_bin/test_openssl_certificate_hostname_contract`
     - `git diff --check`
   - 当前最终收口证据：
     - `MbedTLS` 首轮 RED：
       - `SAN-vs-CN fixture prioritizes SAN over CN`
       - `Wildcard SAN fixture matches single-label subdomain`
     - `WolfSSL` 首轮 RED：
       - 同上两条
     - 这证明缺口不是“还缺测试而已”，
       而是共享实现逻辑确实偏离
       `FreePascal/OpenSSL`
       truth
     - 最小修复后：
       - `MbedTLS focused framework test`
         `221 passed / 0 failed`
       - `WolfSSL focused framework test`
         `237 passed / 0 failed`
       - `OpenSSL focused hostname contract`
         PASS
     - 上一笔提交
       `a6c52d5`
       的 GitHub Actions
       `26170282078`
       现已全部绿色：
       - `Code Quality (Light)`
       - `Minimal Gate (Linux)`
       - `FreePascal TLS 1.3 Completeness`
   - 当前结论：
     - 这批把
       `VerifyHostname`
       从
       “部分 backend 有基础 SAN proof”
       推进到
       “主要 backend 对高风险 fixture 具备 focused parity evidence”
     - 同时也把
       `WinSSL`
       这份 SAN 测试从孤立文件
       推进到了可由 Windows CI 接手的 runtime lane
   - 当前总路线图进度：
     - `接口设计`
       继续朝
       compile/runtime contract 闭合
       推进，
       不是只看 signature 是否存在
     - `后端实现`
       本批确实修到了
       三个 backend 的 shared semantic residual
     - `测试与文档`
       新增：
       - OpenSSL focused contract
       - optional backend fixture parity proof
       - WinSSL runtime-lane test wiring
   - 当前批收口后的默认下一步：
     - 当前已追加最终外部证据：
       - `WinSSL Runtime Gate`
         `26172089572`
         已全部绿色
     - 因而下一刀不再停留在
       `VerifyHostname`
       Windows runtime proof，
       而应继续收紧
       活跃文档 / 活跃测试
       是否仍按旧 public API
       心智教学
118. `certificate public SAN array semantics truth` 这批用于把活跃 public guide 与代表性 OpenSSL 证书测试重新对齐到当前 `TSSLStringArray` source truth：
   - 新 plan：
     - `docs/plans/2026-05-20-certificate-public-san-array-semantics-truth.md`
   - 当前新发现：
     - 当前 source truth 已经明确：
       - `TSSLCertificateInfo.SubjectAltNames`
         是 `TSSLStringArray`
       - `ISSLCertificate.GetSubjectAltNames`
         / `GetKeyUsage`
         / `GetExtendedKeyUsage`
         都返回 `TSSLStringArray`
     - 但活跃面仍有两处明显漂移：
       - `docs/guides/TROUBLESHOOTING.md`
         还在教学：
         - `LAltNames.Count`
         - `LAltNames.Free`
       - `tests/certificate/test_certificate_unit.pas`
         还把：
         - `GetSubjectAltNames`
         - `GetKeyUsage`
         - `GetExtendedKeyUsage`
         当成 `TStringList`
         使用
     - 这不是静态洁癖，
       而是当前就能打出的真实 RED：
       - `test_certificate_unit.pas`
         编译直接报
         `got "TSSLStringArray" expected "TStringList"`
   - 当前最小修法：
     - 在
       `TROUBLESHOOTING`
       把 `GetSubjectAltNames`
       示例切回：
       - `Length(LAltNames)`
       - `High(LAltNames)`
       array 语义
     - 在
       `tests/certificate/test_certificate_unit.pas`
       用：
       - `TSSLStringArray`
       - `ArrayContains(...)`
       - `Length(...)`
       替换旧
       `TStringList`
       / `Count`
       / `IndexOf`
       / `Free`
       心智
     - 同时把同文件里
       空证书日期旧断言
       一起收紧到当前 truth：
       - 未知日期 `(0,0)`
         是允许的
     - 新增 focused source contract：
       - `tests/scripts/test_certificate_public_san_array_semantics_contract.sh`
   - 当前 focused proof：
     - `bash -n tests/scripts/test_certificate_public_san_array_semantics_contract.sh`
     - `bash tests/scripts/test_certificate_public_san_array_semantics_contract.sh`
     - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_certificate_unit_units -FEtmp/test_certificate_unit_bin -otmp/test_certificate_unit_bin/test_certificate_unit tests/certificate/test_certificate_unit.pas`
     - `./tmp/test_certificate_unit_bin/test_certificate_unit`
     - `git diff --check`
   - 当前最终收口证据：
     - 首轮 RED：
       - `TROUBLESHOOTING.md`
         仍命中：
         - `LAltNames.Count`
         - `LAltNames.Free`
       - `test_certificate_unit.pas`
         编译失败：
         - `got "TSSLStringArray" expected "TStringList"`
         - 共 9 处错误
     - 最小修复后：
       - focused shell contract
         PASS
       - `test_certificate_unit`
         先从“编不过”
         推进到“能编能跑但有 1 条旧断言失败”
       - 收紧同文件
         空证书日期断言后：
         - `47 passed / 0 failed`
     - 这说明这批不是只改注释，
       而是把一份真实坏掉的 active test
       修回当前 public API truth
   - 当前结论：
     - 这批把
       `ISSLCertificate`
       证书扩展 array semantics
       从
       “源码/API reference 正确，
       但活跃 guide/test 仍在回退旧心智”
       收紧成
       source / guide / representative test
       三者一致
   - 当前总路线图进度：
     - `接口设计`
       继续从
       “签名存在”
       推进到
       “调用方式与心智模型也一致”
     - `后端实现`
       本批不改生产实现，
       说明这次 residual
       的确落在 public API truth sync
     - `测试与文档`
       新增：
       - focused source contract
       - 重新可编译运行的代表性 OpenSSL 证书测试
       - 活跃 troubleshooting guide truth sync
   - 当前批收口后的默认下一步：
     - 提交并推送本批
     - 继续沿
       `ISSLCertificate`
       / `ISSLCertificateStore`
       public-surface completeness
       查下一条
       活跃 doc/test/example residual
119. `troubleshooting store public API truth` 这批用于把活跃排障文档里的 `ISSLCertificateStore` 示例重新对齐到当前 public store surface：
   - 新 plan：
     - `docs/plans/2026-05-20-troubleshooting-store-public-api-truth.md`
   - 当前新发现：
     - `docs/guides/TROUBLESHOOTING.md`
       里仍有一段：
       - `LStore := LLib.CreateCertificateStore;`
       - `LStore.Open(SSL_STORE_ROOT);`
     - 但当前 shipped
       `ISSLCertificateStore`
       公共接口只暴露：
       - `LoadFromFile`
       - `LoadFromPath`
       - `LoadSystemStore`
       - `AddCertificate`
       - `FindBy...`
     - `Open(...)`
       与
       `SSL_STORE_ROOT`
       只是
       `fafafa.ssl.winssl.certstore`
       concrete helper，
       不是 generic public store flow
   - 当前最小修法：
     - 保留 OS 工具
       `certutil`
       的持久导入建议
     - 把文档里的代码示例
       改成：
       - `LoadSystemStore`
       - `AddCertificate`
       这条跨后端 public store flow
     - 显式补一句语义说明：
       - 这段代码只影响
         当前进程里注入的验证 store
       - 若要持久写入 Windows
         系统存储，
         继续使用
         `certutil`
         或 WinSSL
         专用 helper
     - 新增 focused contract：
       - `tests/scripts/test_troubleshooting_store_public_api_truth_contract.sh`
   - 当前 focused proof：
     - `bash -n tests/scripts/test_troubleshooting_store_public_api_truth_contract.sh`
     - `bash tests/scripts/test_troubleshooting_store_public_api_truth_contract.sh`
     - `git diff --check`
   - 当前最终收口证据：
     - 首轮 RED：
       - `TROUBLESHOOTING.md`
         当前仍命中：
         - `LStore.Open(SSL_STORE_ROOT);`
         - `SSL_STORE_ROOT`
     - 最小修复后：
       - guide
         改回
         `LoadSystemStore`
         / `AddCertificate`
         public flow
       - 并补出：
         - “只影响当前进程验证 store”
           这条边界说明
       - focused shell contract
         PASS
   - 当前结论：
     - 这批把
       generic public store flow
       与
       WinSSL concrete store helper flow
       在活跃排障文档里重新分层清楚，
       避免继续传播
       `Open(SSL_STORE_ROOT)`
       的错误 public 心智
   - 当前总路线图进度：
     - `接口设计`
       继续从
       “签名存在”
       推进到
       “活跃排障与用法教学也不再混 concrete-only 能力”
     - `后端实现`
       本批仍不改 runtime，
       说明这次 residual
       依旧落在
       public surface truth sync
     - `测试与文档`
       新增：
       - focused troubleshooting contract
       - 活跃 store guide truth sync
   - 当前批收口后的默认下一步：
     - 先提交并推送本批
     - 再继续扫
       活跃 guide/example/test
       里是否还有
       `ISSLCertificateStore`
       public / concrete
       混用残留
120. `winssl store active docs truth` 这批用于把活跃 WinSSL 文档里的证书存储示例重新对齐到当前 helper/public/concrete 分层真相：
   - 新 plan：
     - `docs/plans/2026-05-21-winssl-store-active-docs-truth.md`
   - 当前新发现：
     - `docs/guides/WINSSL_BEST_PRACTICES.md`
       仍在对
       `ISSLCertificateStore`
       变量调用：
       - `LStore.Open(SSL_STORE_MY);`
     - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
       仍在示例里使用：
       - 不存在的类名
         `TWinSSLCertStore`
       - 非 public surface
         `Store.Certificates`
       - 非 public getter 形态
         `Cert.Subject`
   - 当前源码真相：
     - `OpenSystemStore(...)`
       helper
       返回
       `ISSLCertificateStore`
     - public 枚举路径是：
       - `GetCount`
       - `GetCertificate`
       - `GetSubject`
     - `Open` / `Close` / `IsOpen` / `GetAllCertificates`
       只属于
       `TWinSSLCertificateStore`
       concrete class
   - 当前最小修法：
     - `WINSSL_BEST_PRACTICES`
       改成：
       - `OpenSystemStore(SSL_STORE_MY)`
     - `WINSSL_BACKEND_CAPABILITY_MATRIX`
       改成：
       - `OpenSystemStore(SSL_STORE_MY)`
       - `GetCount`
       - `GetCertificate`
       - `GetSubject`
     - 新增 focused contract：
       - `tests/scripts/test_winssl_store_active_docs_truth_contract.sh`
   - 当前 focused proof：
     - `bash -n tests/scripts/test_winssl_store_active_docs_truth_contract.sh`
     - `bash tests/scripts/test_winssl_store_active_docs_truth_contract.sh`
     - `git diff --check`
   - 当前最终收口证据：
     - focused contract
       PASS
     - 两处 active WinSSL 文档示例
       已不再教：
       - `TWinSSLCertStore`
       - `ISSLCertificateStore.Open(...)`
       - `Store.Certificates`
       - `Cert.Subject`
   - 当前结论：
     - 这批把
       WinSSL helper
       /
       concrete class
       /
       public interface
       三层边界重新写清楚，
       避免用户继续按活跃文档抄出编译级错误
   - 当前总路线图进度：
     - `接口设计`
       继续从
       “签名存在”
       推进到
       “backend-specific helper
       与 public interface
       的边界也不再误教”
     - `后端实现`
       本批不改 runtime，
       说明这次 residual
       仍落在 active-doc truth sync
     - `测试与文档`
       新增：
       - focused WinSSL store docs contract
       - 两处活跃 WinSSL 文档 truth 修复
   - 当前批收口后的默认下一步：
     - 提交并推送本批
     - 继续沿
       `ISSLCertificate`
       /
       `ISSLCertificateStore`
       public-surface completeness
       查下一条活跃 doc/test/example residual
121. `optional backends certificate store fingerprint query parity` 这批用于把 `MbedTLS` / `WolfSSL` 的 `FindByFingerprint` 收紧到与其他 backend 一致的 normalized query truth：
   - 新 plan：
     - `docs/plans/2026-05-21-optional-backends-certificate-store-fingerprint-query-parity.md`
   - 当前新发现：
     - `TMbedTLSCertificateStore.FindByFingerprint`
       仍在拿
       `GetFingerprintSHA1`
       /
       `GetFingerprintSHA256`
       和输入做 raw-string compare
     - `TWolfSSLCertificateStore.FindByFingerprint`
       也还是同样的 raw-string compare
     - 但：
       - `OpenSSL`
       - `FreePascal`
       - `WinSSL`
       已经都支持
       去分隔符
       /
       大小写归一化
   - 当前源码真相：
     - `MbedTLS`
       /
       `WolfSSL`
       两边都已经有：
       - `NormalizeMbedTLSCertFingerprint(...)`
       - `NormalizeWolfCertFingerprint(...)`
     - 同一组 helper
       已经用于：
       - `Contains`
       - `RemoveCertificate`
       - chain de-dup
     - 所以
       `FindByFingerprint`
       的 raw-string compare
       是 public query residual，
       不是 helper 缺失
   - 当前最小修法：
     - 先在：
       - `tests/test_mbedtls_framework.pas`
       - `tests/test_wolfssl_framework.pas`
       - `tests/test_freepascal_backend_basic.pas`
       补
       normalized fingerprint query contract
     - 再把
       `TMbedTLSCertificateStore.FindByFingerprint`
       /
       `TWolfSSLCertificateStore.FindByFingerprint`
       改成统一走现有 normalize helper
   - 当前 focused proof：
     - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_mbedtls_framework_units -FEtmp/test_mbedtls_framework_units -otmp/test_mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas`
     - `./tmp/test_mbedtls_framework_units/test_mbedtls_framework`
     - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_framework_units -FEtmp/test_wolfssl_framework_units -otmp/test_wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas`
     - `./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
     - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_backend_basic_units -FEtmp/test_freepascal_backend_basic_units -otmp/test_freepascal_backend_basic_units/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas`
     - `./tmp/test_freepascal_backend_basic_units/test_freepascal_backend_basic`
     - `git diff --check`
   - 当前最终收口证据：
     - 首轮 RED：
       - `MbedTLS`
         只在
         `FindByFingerprint supports normalized query variant`
         失败
       - `WolfSSL`
         只在
         `FindByFingerprint supports normalized query variant`
         失败
       - `FreePascal`
         控制组继续通过
     - 最小修复后：
       - `MbedTLS Framework Test Summary`
         `233 passed / 0 failed`
       - `WolfSSL Framework Test Summary`
         `247 passed / 0 failed`
       - `FreePascal backend basic checks passed`
   - 当前结论：
     - 这批证明
       `ISSLCertificateStore.FindByFingerprint`
       在 optional backends
       上确实还留着真实实现缺口，
       现在已重新对齐到跨 backend 的 normalized query truth
   - 当前总路线图进度：
     - `接口设计`
       继续从
       “签名一致”
       推进到
       “查询语义也一致”
     - `后端实现`
       本批直接落在
       `MbedTLS`
       /
       `WolfSSL`
       实现收口
     - `测试与文档`
       新增：
       - fingerprint query parity plan
       - 三个 focused runtime/assertion proof
   - 当前批收口后的默认下一步：
     - 提交并推送本批
     - 继续沿
       `ISSLCertificate`
       /
       `ISSLCertificateStore`
       public-surface completeness
       查下一条真正的 backend implementation residual
122. `OpenSSL/WinSSL certificate store serial query parity` 这批用于把 `FindBySerialNumber` 在两个主 backend 上收紧到 shared normalized hex truth，并补掉 OpenSSL serial getter 的隐藏 readiness residual：
   - 新 plan：
     - `docs/plans/2026-05-21-openssl-winssl-certificate-store-serial-query-parity.md`
   - 当前新发现：
     - `TOpenSSLCertificateStore`
       仍把 serial
       按
       `UpperCase(...)`
       建索引和查询
     - `TWinSSLCertificateStore`
       仍对
       `Cert.GetSerialNumber`
       和输入做 raw compare
     - 首轮
       `OpenSSL`
       focused RED
       还补出了更深一层的真问题：
       `TOpenSSLCertificate.GetSerialNumber`
       在 native serial helper
       尚未 ready
       时会直接退出，
       导致 fallback
       实际上走不到
   - 当前源码真相：
     - `OpenSSL`
       store 的 serial index/query
       建在
       `Cert.GetSerialNumber`
       之上
     - 所以如果 getter
       自己先空掉，
       store normalize
       也无法成立
   - 当前最小修法：
     - 给
       `OpenSSL`
       /
       `WinSSL`
       store
       补 shared 风格的
       serial normalize helper
     - 给
       `TOpenSSLCertificate.SaveToDER`
       补 lazy-load
     - 给
       `TOpenSSLCertificate.GetSerialNumber`
       改成：
       - 先 lazy-load native helper
       - native path
         失败时
         回退到
         DER / PEM 导出
         + `TX509Certificate`
         parser
   - 当前 focused proof：
     - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_openssl_certstore_serial_query_contract_units -FEtmp/test_openssl_certstore_serial_query_contract_units -otmp/test_openssl_certstore_serial_query_contract_units/test_openssl_certstore_serial_query_contract tests/openssl/test_openssl_certstore_serial_query_contract.pas`
     - `./tmp/test_openssl_certstore_serial_query_contract_units/test_openssl_certstore_serial_query_contract`
     - `git diff --check`
     - `WinSSL`
       runtime proof
       继续看
       GitHub Windows CI
   - 当前最终收口证据：
     - 首轮 RED：
       - `Fixture exposes serial number`
       - `FindBySerialNumber supports normalized serial query variant`
     - 修复后：
       - `OpenSSL` focused contract
         `9 passed / 0 failed`
       - `git diff --check`
         通过
   - 当前结论：
     - 这批再次证明，
       backend implementation residual
       真实存在于：
       - query normalize
       - getter readiness
       两层
     - 当前本地
       `OpenSSL`
       已闭环；
       `WinSSL`
       runtime truth
       等 push 后
       Windows CI
       最终确认
   - 当前批收口后的默认下一步：
     - 提交并推送本批
     - 看
       `Windows`
       runtime suite
       是否接受新的
       `FindBySerialNumber`
       memory-store contract
123. `OpenSSL/WinSSL certificate store fingerprint query parity` 这批用于把 `FindByFingerprint` 在两个主 backend 上收紧到与 `FreePascal` 一致的 normalized hex truth：
   - 新 plan：
     - `docs/plans/2026-05-21-openssl-winssl-certificate-store-fingerprint-query-parity.md`
   - 当前新发现：
     - `TOpenSSLCertificateStore`
       当前 fingerprint index/query
       只去掉 `:`
     - `TWinSSLCertificateStore`
       当前 fingerprint compare
       也只去掉 `:`
     - 这意味着：
       lower-case + `:`
       可能已经能命中，
       但
       `AA-BB-CC`
       /
       首尾空白
       这类展示格式
       仍没有真正对齐
       `FreePascal`
       基线
   - 当前源码真相：
     - `FreePascal.NormalizeFingerprint(...)`
       已统一去掉：
       - `:`
       - `-`
       - 空格
     - `OpenSSL`
       /
       `WinSSL`
       还没有收回到 shared hex normalize helper
   - 当前最小修法：
     - `OpenSSL`
       fingerprint index/query
       改复用
       `NormalizeCertificateStoreHex(...)`
     - `WinSSL`
       fingerprint compare
       也改复用
       `NormalizeCertificateStoreHex(...)`
     - 在
       `OpenSSL`
       /
       `WinSSL`
       focused tests
       都补
       `-`
       +
       空白 variant
   - 当前 focused proof：
     - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_openssl_certstore_fingerprint_query_contract_units -FEtmp/test_openssl_certstore_fingerprint_query_contract_units -otmp/test_openssl_certstore_fingerprint_query_contract_units/test_openssl_certstore_fingerprint_query_contract tests/openssl/test_openssl_certstore_fingerprint_query_contract.pas`
     - `./tmp/test_openssl_certstore_fingerprint_query_contract_units/test_openssl_certstore_fingerprint_query_contract`
     - `git diff --check`
     - `WinSSL`
       runtime proof
       继续看
       GitHub Windows CI
   - 当前最终收口证据：
     - 首轮 RED：
       - `FindByFingerprint supports normalized fingerprint query variant`
     - 修复后：
       - `OpenSSL` focused contract
         `9 passed / 0 failed`
       - `git diff --check`
         通过
   - 当前结论：
     - 这批证明
       `FindByFingerprint`
       在两个主 backend
       上仍有真实 query-normalization residual，
       不是文档假设问题
     - 当前本地
       `OpenSSL`
       已闭环；
       `WinSSL`
       runtime truth
       继续由
       Windows CI
       最终确认
   - 当前总路线图进度：
     - `接口设计`
       已从
       “有这个方法”
       推到
       “查询语义跨 backend 真一致”
     - `后端实现`
       当前正在收最后几条
       certstore query family
       的主 backend residual
     - `测试和文档`
       继续跟着每条 focused contract
       同步闭环
   - 当前批收口后的默认下一步：
     - 提交并推送本批
     - 看
       `Windows`
       runtime suite
       是否接受新的
       fingerprint query
       memory-store contract

### 2026-05-21 Ed25519 证书算法元数据真相收口

- 当前远端基线已补齐：
  - `CI`
    run
    `26179499027`
    = success
  - `WinSSL Runtime Gate`
    run
    `26179498925`
    = success
- 当前新收口的不是旧文档措辞问题，
  而是纯 Pascal
  `TX509Certificate`
  在
  `Ed25519`
  证书上仍有真实 parser residual：
  - `Algorithm.Name`
    暴露 OID
  - `KeyType`
    仍是
    `Unknown`
  - `KeySize`
    仍是
    `0`
  - `SignatureAlgorithm.Name`
    也仍是 OID
- 最小正确修法：
  - `ASN.1`
    OID 表补
    `Ed25519`
    /
    `Ed448`
  - `ParsePublicKeyInfo(...)`
    补
    `Ed25519`
    /
    `Ed448`
    key type / size truth
  - `MbedTLS`
    dedicated matrix
    改成区分：
    - handshake capability 未发布
    - certificate metadata truth 已发布
- 当前 focused proof：
  - `tests/test_x509_ed25519_algorithm_truth.pas`
    `7 passed / 0 failed`
  - `tests/test_cert_utils_ed25519_contract.pas`
    `24 passed / 0 failed`
  - `tests/scripts/test_mbedtls_ed25519_capability_doc_truth_contract.sh`
    PASS
  - `git diff --check`
    PASS
- 当前批收口后的默认下一步：
  - 提交并推送本批
  - 看新的
    GitHub Actions
    是否接受这条
    parser/doc truth
    修正

### 2026-05-21 主 backend Ed25519 证书算法真相收口

- [completed] 当前新的 residual
  已从 shared parser
  收窄到两个主 backend
  的证书算法 getter：
  - `OpenSSL`
    `GetPublicKeyAlgorithm`
    /
    `GetInfo.PublicKeyAlgorithm`
    在
    `Ed25519`
    证书上仍是
    `Unknown`
  - `WinSSL`
    `GetPublicKeyAlgorithm`
    /
    `GetSignatureAlgorithm`
    仍发布裸 OID
- [completed] 最小正确修法已经落地：
  - `src/fafafa.ssl.openssl.certificate.pas`
    补
    `EVP_PKEY_ED25519`
    /
    `EVP_PKEY_ED448`
    名称映射
  - `src/fafafa.ssl.winssl.certificate.pas`
    把算法 OID
    收口到
    `OIDToName(...)`
    truth，
    未知时才回退原始 OID
- [completed] focused proof
  已经补齐到
  “Linux 本地 + Windows CI 待接管”
  的闭环状态：
  - 新增
    `docs/plans/2026-05-21-main-backends-ed25519-certificate-algorithm-truth.md`
  - 新增
    `tests/openssl/test_openssl_ed25519_certificate_algorithm_truth.pas`
    并已打出：
    - RED：
      `GetPublicKeyAlgorithm = Unknown`
      /
      `GetInfo.PublicKeyAlgorithm = Unknown`
    - GREEN：
      `9 passed / 0 failed`
  - `tests/winssl/test_winssl_unit_comprehensive.pas`
    已补入生成式
    `Ed25519`
    runtime assertion，
    push 后由
    `WinSSL Runtime Gate`
    最终确认
- [completed] 当前批收口后的默认下一步：
  - `git diff --check`
  - 简短 review
  - commit / push
  - 看新的
    `CI`
    /
    `WinSSL Runtime Gate`
    是否接受这条主 backend 修正

### 2026-05-21 WinSSL certificate publickey contract 对齐

- [completed] 当前新的 residual
  已明确落在
  `WinSSL` 主 backend
  的 certificate public surface：
  - 仓库现行 contract
    已经是
    `GetPublicKey = GetPublicKeyAlgorithm`
  - `OpenSSL`
    /
    `FreePascal`
    /
    `MbedTLS`
    /
    `WolfSSL`
    都已经按这条最小语义收口
  - 但
    `TWinSSLCertificate.GetPublicKey`
    仍单独返回
    `SubjectPublicKeyInfo`
    PEM 字符串
- [completed] 当前 focused RED
  已通过本地静态 contract
  稳定坐实：
  - 新增
    `docs/plans/2026-05-21-winssl-certificate-publickey-contract-alignment.md`
  - 新增
    `tests/scripts/test_winssl_certificate_publickey_contract.sh`
  - 首轮失败直接打在：
    - `WinSSL GetPublicKey is not aligned to GetPublicKeyAlgorithm contract`
- [completed] 最小正确修法已经落地：
  - `src/fafafa.ssl.winssl.certificate.pas`
    的
    `GetPublicKey`
    已收口到
    `GetPublicKeyAlgorithm`
  - `tests/winssl/test_winssl_unit_comprehensive.pas`
    已补入
    `GetPublicKey`
    /
    `GetPublicKeyAlgorithm`
    对齐断言
- [completed] 当前 focused proof：
  - `bash tests/scripts/test_winssl_certificate_publickey_contract.sh`
    PASS
  - `git diff --check`
    PASS
  - push 后由
    `WinSSL Runtime Gate`
    最终确认这条 runtime truth
