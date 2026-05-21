# Findings - Interface Design And Backend Implementation Verification

## 2026-05-21

- builder merge verify semantics
  这一刀确认了
  builder verify
  线不只是
  factory/config
  与
  validation
  有裂缝，
  `Merge(...)`
  之前也还保留着
  同类问题

- 当前
  `ExportToJSON(...)`
  明明总会携带：
  - `verify_modes`
  但
  `Merge(...)`
  却只在
  `LVerify.Count > 0`
  时才覆盖目标
  `FVerifyMode`

- 这意味着：
  source builder
  即便已经显式表达：
  - `verify_modes = []`
  merge 后 target
  仍会保留旧的
  `[sslVerifyPeer]`

- 这个问题之所以重要，
  是因为它不是 isolated bug，
  而是正好卡在
  builder verify
  语义对齐链的中间：
  - import
    能导入
    `[]`
  - validation
    现在也能把
    `[]`
    识别成
    no-verify
  - 但 merge
    却还会吞掉
    这个空集合

- 修复后，
  `Merge(...)`
  现在只要看到
  `verify_modes`
  字段存在，
  就会按 source snapshot
  覆盖目标 verify mode，
  包括：
  - 空数组 `[]`

- 这使得 builder
  在
  import / merge / validation
  三个关键面上，
  对
  `VerifyMode = []`
  的解释终于进一步收敛成一致

- builder verify validation
  这条线又暴露出一条
  runtime / validation
  不一致的问题：
  当前 runtime 上，
  `[]`
  与
  `[sslVerifyNone]`
  都会关闭 peer verification，
  但 validation
  之前只把
  `[sslVerifyNone]`
  认成
  “禁用验证”

- 更关键的是，
  这不是纯理论分歧，
  因为 builder 的
  `ImportFromJSON(...)`
  /
  `ImportFromINI(...)`
  都可以把
  `verify_modes`
  导入成：
  - `[]`

- 这会导致：
  导入后的 builder
  在 runtime 上
  已经是
  no-verify，
  但
  `ValidateClient`
  /
  `ValidateServer`
  却不会发出当前应有的
  insecure warning

- 修复后，
  builder validation
  现在不再盯着
  `sslVerifyNone`
  这个单一枚举值，
  而是按
  “是否启用了
  `sslVerifyPeer`”
  来判断当前 verify 是否真的开启

- 这使得：
  - `WithVerifyNone`
    继续保持 warning
  - JSON / INI 导入出的
    `verify_modes = []`
    也终于和 runtime 真相一致地
    得到 warning

- `VerifyMode = []`
  这条线最终暴露出的
  不是单纯文档残留，
  而是一个真实的
  interface/implementation
  语义 bug

- 当前各 backend/context
  默认 verify 基线
  普遍仍是：
  - `[sslVerifyPeer]`

- 但 factory /
  library-default
  context creation path
  之前普遍都写成：
  - `if LConfig.VerifyMode <> [] then`
    `Result.SetVerifyMode(LConfig.VerifyMode);`

- 这会导致：
  当调用方通过
  one-shot config
  或
  library default config
  明确写：
  - `VerifyMode := []`
  创建路径会把它误读成
  “未设置”
  而不是
  “显式禁用验证”

- 当前 public reality
  却已经广泛把
  `[]`
  当成 direct-context
  的禁用验证语义：
  - 大量 tests
  - 多份活跃文档
  - 多个 backend guide
  都如此使用

- 因而此前的状态
  实际是在同一公共接口族里
  制造了一条危险分叉：
  - direct context path:
    `[]`
    表示
    no-verify
  - factory / default-config creation path:
    `[]`
    却被吞掉，
    退回默认
    `[sslVerifyPeer]`

- 修复后，
  这条空集合语义
  已在
  `TSSLFactory`
  与各 backend
  `ISSLLibrary.CreateContext(...)`
  默认配置路径上
  收回一致：
  - `VerifyMode := []`
    会真实落成
    `GetVerifyMode = []`

- 这批也顺手把
  `TVerificationMode`
  的路线判断
  往前推进了一步：
  当前更急的不是
  先补 typed verify builder seam，
  而是先把现有
  `TSSLVerifyModes`
  / empty-set
  语义在各创建路径上
  完整收正

- `TSessionCacheMode`
  这条线经过 current source truth
  复核后，
  当前也不像
  `TTimeoutDuration`
  那样存在一条明显的
  “高频 public path
  只差 typed overload”
  的 adoption seam

- 当前 source truth
  很清楚：
  - `ISSLContext.SetSessionCacheMode(...)`
    仍接收
    `Boolean`
  - `TSessionCacheMode`
    /
    `scm_*`
    虽然仍被主门面 re-export，
    但当前 source
    没有把它们桥接到
    context / builder
    入口

- 这意味着：
  不能因为 facade
  还导出了
  `TSessionCacheMode`
  就默认我们下一步该去补一个
  builder/context typed seam；
  它当前更像
  safety/policy type
  而不是当前直接 runtime 参数

- 当前真实问题
  不是实现缺口，
  而是活跃中文 FAQ
  还残留一条
  会把人带偏的旧写法：
  - `LContext.SetSessionCacheMode(sslSessCacheClient);`

- 修复后，
  中文 FAQ
  已重新收敛成当前 public truth：
  - `LContext.SetSessionCacheMode(True);`
  - 并明确写出：
    `TSessionCacheMode`
    /
    `scm_*`
    当前更适合作为
    调用方自己的
    policy wrapper，
    而不是当前直接传给
    context
    的参数类型

- 这批再次强化了总路线：
  对
  `TVerificationMode`
  /
  `TSessionCacheMode`
  这类 safety surface，
  先做 source-truth 分型，
  再决定是
  active-doc truth alignment
  还是
  真正的 API adoption

- `TSSLContextBuilder`
  这条线当前又确认出一类
  很值得优先消灭的路线偏移：
  不是实现缺口，
  而是活跃文档还在教学
  一批源码里并不存在的
  builder fluent method

- 当前 source truth
  已经很明确：
  - `WithSessionCache`
    只有
    `Boolean`
    开关入口
  - session cache size
    的公开入口
    仍是
    `ISSLContext.SetSessionCacheSize(...)`
  - builder 并不存在：
    - `WithStrongCipherSuites`
    - `WithPerfectForwardSecrecy`
    - `WithSessionTickets`
    - `WithoutVerifyPeer`
    - `WithSSL3`
    - `WithTLS10`

- 当前真实问题
  不是要立刻补这些 fluent API，
  而是两份 active guide
  仍在把它们讲成
  “好像现在就能用”的 public surface：
  - `docs/guides/PERFORMANCE_PROFILING_GUIDE.md`
    把
    `WithSessionCache`
    写成了
    size/count overload
  - `docs/guides/security-best-practices.md`
    继续教学多条
    builder 假接口

- 这类 drift
  的风险比普通笔误更高，
  因为它会直接误导我们后续的
  interface-design / type-safety
  路线判断：
  看起来像是
  “只差补一个 overload”，
  实际上 current source
  根本没有那条高入口 seam

- 修复后，
  active builder guidance
  已重新收敛成当前 shipped truth：
  - modern cipher/profile baseline
    优先走
    `WithSafeDefaults`
  - session tickets
    当前走
    `WithOption(ssoEnableSessionTickets)`
  - 禁用验证的危险示例
    当前应写成
    `WithVerifyNone`
  - 弱协议反例
    当前应通过
    `WithProtocols([...])`
    表达

- 这批也补清楚了一条
  很重要的语义边界：
  启用 session cache / tickets
  只是准备上下文能力；
  如果调用方需要显式保存或注入
  session candidate，
  仍应走
  `ISSLSessionResumption.GetSession / SetSession`
 ；
  不能把
  “开了 cache”
  自动等同于
  “已经观测到 resumed handshake”

- `TBufferSize`
  这条线经过 current source truth
  复核后，
  当前并没有暴露出一个像
  `TTimeoutDuration`
  那样的
  “高入口 typed adoption
  还没落到真实 builder/facade path”
  缺口

- 相反，
  repo 当前已经有相当硬的真相锚点：
  - `TSSLConfig.BufferSize`
    source comment
    明确是
    connection-scoped buffering hint
  - factory /
    direct-library
    path
    对自定义
    `BufferSize`
    都会显式 reject
  - 当前推荐入口
    也已经收敛成：
    外围
    socket / stream / transport / app-level
    buffering policy

- 这意味着：
  如果继续把
  `TBufferSize`
  当成
  “下一条待补进 context builder / factory 的 typed public seam”，
  反而会把项目往错误方向推，
  等于在当前 public truth 之外再发明一个新入口

- 当前真实问题
  不是实现缺口，
  而是
  `docs/guides/MIGRATION_GUIDE_PHASE_2.4.md`
  这份仍会被读到的历史迁移指南
  还残留两种误导：
  - 把
    `TBufferSize`
    混进一个看似当前库内存在的
    `ConfigureSSLConnection(...)`
    统一入口
  - 在组合示意里继续写
    `SetBuffer(...)`

- 修复后，
  这条线的 repo truth
  重新收敛成：
  - `TBufferSize`
    仍是 facade 可见的类型安全单位类型
  - 但它当前更适合
    调用方自己的
    transport / buffer policy helper
  - 不是当前
    TLS context / factory / direct-library
    的 buffer-size public seam

- 这批也给后续路线一个很重要的校正：
  不能只因为某个 safety type
  已经被 main facade re-export，
  就默认它一定还存在一条待补进高入口的实现主线；
  还要先核对
  当前 runtime / factory / builder
  是否真的存在那条可接入路径

- `TTimeoutDuration`
  在 timeout 这条线上，
  当前又暴露出一条更深一层的 adoption gap：
  虽然
  `TSSLConnector`
  /
  `TSSLAcceptor`
  /
  `ISSLConnectionBuilder`
  已经补上了 typed overload，
  但
  `TSSLContextBuilder`
  之前仍只公开：
  - `WithSessionTimeout(ASeconds: Integer)`

- 这意味着：
  safety type
  还没有真正进入
  context 配置这条最常见 fluent path，
  而活跃 builder 示例也仍在继续教学：
  - `.WithSessionTimeout(7200)`

- 当前 source truth
  还进一步确认了：
  这条接口并不是握手 timeout，
  而是
  context-scoped session lifetime，
  当前真实底层仍然是：
  - `TSSLConfig.SessionTimeout: Integer`
  - `ISSLContext.SetSessionTimeout(Integer)`
  - 单位：秒

- 因此这批最稳的最小修法
  不是重构底层存储，
  而是在 builder 高入口做 bridge：
  - `WithSessionTimeout(const ATimeout: TTimeoutDuration)`
  同时保留旧：
  - `WithSessionTimeout(ASeconds: Integer)`

- 这条 bridge
  也必须显式锁住几个容易含糊的边界：
  - `Infinite`
    不能被解释成 session lifetime
  - 非整秒 duration
    不能被静默截断
  - 超出当前
    `Integer`
    秒范围的 duration
    不能被静默溢出

- 修复后，
  context builder
  已经真实补上：
  - `WithSessionTimeout(const ATimeout: TTimeoutDuration)`
  并且活跃 builder 示例
  已开始采用：
  - `TTimeoutDuration.Minutes(120)`

- focused contract
  当前已经同时证明：
  - typed duration
    真会落成
    `120`
    秒
  - legacy integer overload
    仍继续保持可用
  - `1500ms`
    会以
    `ESSLInvalidArgument`
    明确 reject
  - `Infinite`
    会以
    `ESSLInvalidArgument`
    明确 reject

- `TTimeoutDuration`
  之前也有一条和证书 key-config
  很像的 adoption gap：
  虽然
  `fafafa.ssl`
  主门面已经 re-export 了
  `TTimeoutDuration`，
  而且历史迁移文档也已经讲过
  “毫秒/秒混淆”，
  但当前最常见的 TLS 高入口
  仍只暴露：
  - `TSSLConnector.WithTimeout(AMs: Integer)`
  - `TSSLAcceptor.WithTimeout(AMs: Integer)`
  - `ISSLConnectionBuilder.WithTimeout(AMs: Integer)`

- 这意味着：
  type-safety
  在 timeout 这条线上，
  仍没有真正进入
  用户最可能直接调用的
  connector / acceptor / builder fluent path

- 当前最稳的最小修法
  也不是重构底层
  `ISSLConnection.SetTimeout(Integer)`
  真相，
  而是先在高入口做 bridge：
  - `WithTimeout(const ATimeout: TTimeoutDuration)`
  并保留旧
  `Integer`
  overload

- 这条 bridge
  还暴露出一条必须显式处理的边界：
  `TTimeoutDuration`
  的内部存储是
  `Int64`
  毫秒，
  而 connector / builder
  当前真实存储仍是
  `Integer`

- 因此如果不加 guard，
  极大 duration
  会静默溢出。
  当前更稳的真相是：
  - `Infinite` -> `-1`
  - 可表示范围内的 duration
    -> `Integer` 毫秒
  - 超范围
    -> `ESSLInvalidArgument`

- 修复后，
  timeout type-safety
  已经真实进入：
  - `TSSLConnector`
  - `TSSLAcceptor`
  - `ISSLConnectionBuilder`
  同时活跃文档 / compileable examples
  也开始采用：
  - `TTimeoutDuration.Seconds(15)`

- focused contract
  当前已经证明：
  - connector typed timeout
    真会落成
    `15000`
  - acceptor typed timeout
    真会落成
    `20000`
  - builder typed timeout
    真会落成
    `12000`
  - legacy integer overload
    仍继续保持可用

- `type-safety`
  目前还有一条很真实的 adoption gap：
  虽然
  `fafafa.ssl`
  主门面已经 re-export 了
  `TKeySize`
  /
  `TEllipticCurve`
  等 non-generic safety surface，
  但
  `src/fafafa.ssl.cert.builder.pas`
  之前仍只公开：
  - `WithRSAKey(ABits: Integer = 2048)`
  - `WithECDSAKey(const ACurve: string = 'prime256v1')`

- 这意味着：
  safety surface
  还没有真正进入
  用户最容易直接调用的
  证书生成路径；
  它更多仍停留在：
  - safety unit
  - facade truth
  - 单独测试

- 当前最稳的最小修法
  不是立刻大改
  `TCertGenOptions`
  的内部表示，
  而是先在 builder 上做 bridge：
  - `WithRSAKey(const ASize: TKeySize)`
  - `WithECDSAKey(ACurve: TEllipticCurve)`
  同时保留旧 overload
  以维持兼容

- `src/fafafa.ssl.cert.utils.pas`
  的当前实现真相也确认了：
  `GenerateECKey(const ACurve: string)`
  吃的是
  OpenSSL curve token，
  例如：
  - `prime256v1`
  - `secp384r1`
  - `secp521r1`
  - `brainpoolP256r1`

- 因此不能把
  `EllipticCurveToString(...)`
  直接复用进 builder bridge，
  因为它返回的是展示名：
  - `P-256`
  - `P-384`
  - `P-521`
  - `X25519`
  - `Brainpool P-256`

- 当前 ECDSA cert key path
  还必须明确 reject：
  - `ec_X25519`
  - `ec_X448`
  因为它们是 ECDH-only curve，
  不是当前 certificate builder
  的 signing curve truth

- 修复后，
  builder public surface
  已经真实补上：
  - `WithRSAKey(const ASize: TKeySize)`
  - `WithECDSAKey(ACurve: TEllipticCurve)`
  并且高层真实路径
  `TCertificate`
  /
  `TSSLQuick`
  也开始采用这组 overload

- focused contract
  现在已经同时证明三件事：
  - typed RSA self-signed path
    可编译、可运行
  - typed ECDSA self-signed path
    可编译、可运行
  - `WithECDSAKey(ec_X25519)`
    会以
    `ESSLInvalidArgument`
    明确 fail，
    不再把错误曲线伪装成可用配置

- `docs/reference/API_REFERENCE.md`
  里的
  `TSSLErrorCode`
  代码块
  当前也存在明显文档真相漂移：
  它还在发布一套更早的旧名字，
  而源码当前真实枚举已经换成：
  - `sslErrMemory`
  - `sslErrInvalidParam`
  - `sslErrProtocol`
  - `sslErrHandshake`
  - `sslErrCertificate`
  - `sslErrConnection`
  - `sslErrUnsupported`
  等完整新集合

- 这类问题的风险很直接：
  它不会让生产实现立刻坏掉，
  但会让活跃 canonical doc
  持续把调用方、
  审查与后续测试
  带回旧 public truth

- 当前这批收口后，
  `API_REFERENCE`
  已经不再继续发布这些过时名字：
  - `sslErrInvalidParameter`
  - `sslErrOutOfMemory`
  - `sslErrConnectionClosed`
  - `sslErrHandshakeFailed`
  - `sslErrCertificateVerifyFailed`
  - `sslErrCipherNotSupported`
  - `sslErrProtocolNotSupported`

- 这也说明我们现在的“完整度”推进已经更稳：
  不只是修 contract / test，
  而是把
  source
  /
  focused proof
  /
  active API doc
  三者重新压回同一套 error-code truth

- `tests/contract/test_error_mapping_contract.pas`
  当前又暴露出一条更硬的
  fresh RED：
  它不是 runtime fail，
  而是直接编译失败，
  因为还在引用已经不存在的
  `sslErrOK`

- 当前 `TSSLErrorCode`
  的 no-error 真相
  是：
  - `sslErrNone`
  不是
  - `sslErrOK`

- 这说明：
  error-mapping contract
  已经从当前 public truth
  漂开了一步，
  不能再被当成
  “现成可用的 focused proof”

- 同时，
  这条 contract
  原本也没有显式走
  backend registration path；
  即便只把枚举名改对，
  也仍可能退化成
  “编过了，但实际全 skip”

- 修复后，
  当前 Linux host
  上这条 contract
  已经真实执行：
  - `OpenSSL`
  - `FreePascal`
  并得到：
  - `10 passed`
  - `0 failed`
  - `3 skipped`

- 这批说明我们现在推进的
  “测试完整”
  不是只加更多测试文件，
  而是把已有 contract
  持续拉回当前 API truth
  和真实 runtime execution path

- `tests/test_optional_backends_pkcs12_capability_truth_contract.pas`
  里又暴露出一个典型的
  focused runtime coverage hole：
  它明明调用了
  `CheckBackendCapability(sslFreePascal, False);`
  但自身没有
  `uses fafafa.ssl.freepascal.lib`

- 因为
  `TSSLFactory.IsLibraryAvailable(...)`
  先看 registration map，
  而
  `FreePascal`
  backend 的注册发生在
  `fafafa.ssl.freepascal.lib`
  的 initialization 里，
  所以这条测试之前实际上把
  `FreePascal`
  跑成了：
  - `[SKIP] FreePascal Native backend not available on this platform`

- 这不是
  `FreePascal SupportsPKCS12`
  真相不清楚，
  而是 focused runtime proof
  自己没有把主线 backend
  真正注册进来

- 修复后，
  当前 Linux host
  上的 runtime proof
  已经直接变成：
  - `FreePascal Native SupportsPKCS12 = False`
  - `OpenSSL SupportsPKCS12 = True`
  - `MbedTLS / WolfSSL SupportsPKCS12 = False`
  - `WinSSL`
    因平台原因继续 skip

- 这批和前面的 shared capability / FreePascal coverage hardening
  属于同一类高价值工作：
  不是继续猜实现，
  而是让
  “各 backend 的 focused/shared proof
  真的覆盖到该 backend”
  这件事
  更完整

- `tests/test_capability_matrix_v12.pas`
  这次又打出了一条真正有价值的
  fresh RED：
  初始把
  `OpenSSL SessionCacheSupport`
  写成固定
  `stable`
  后，
  shared regression
  在当前 Linux host
  立即暴露出：
  - `SessionCacheSupport=sslSupportNone`

- 这条红线说明：
  `OpenSSL`
  的 session-cache
  不能再被我们想当然地当成
  “和 session tickets 一样稳定发布”；
  它当前仍然受
  runtime helper surface
  门控

- 因此这批最终收口出的更准真相是：
  - `OpenSSL`
    - `SessionTicketsSupport=stable`
    - `SupportsPKCS12=True`
    - `SupportsCustomCipherSuites=True`
    - `SupportsCallbacks=True`
  - `OpenSSL SessionCacheSupport`
    不在 shared regression
    里被写死成固定级别，
    而是被提升成
    `IsFeatureSupported(sslFeatSessionCache)`
    与
    `SessionCacheSupport`
    的 shared parity contract
  - `FreePascal`
    - `SessionCacheSupport=experimental`
    - `ZeroRTTSupport=experimental`

- 这批说明我们当前路线没有迷失：
  shared audit entrypoint
  继续往前推时，
  已经能直接暴露出
  “接口/能力发布真相
  是否被误写成固定结论”
  这一层问题，
  而不是只在 focused contracts
  里局部成立

- `OpenSSL custom-cipher / callback / PKCS12`
  这三条 published/runtime truth
  现在又多了一层 shared coverage，
  同时 focused proof
  继续保持绿色：
  - `SupportsCustomCipherSuites=True`
  - `SupportsCallbacks=True`
  - `SupportsPKCS12=True`

- `tests/test_capability_matrix_v12.pas`
  现在又推进了一层：
  它已经不再只是
  capability snapshot printer，
  而是对
  `OpenSSL`
  /
  `FreePascal`
  当前主线 truth
  有明确硬断言的 shared regression

- 这批 focused hardening
  锁住了两类最关键的 shared truth：
  1. paired support-level feature
     的 legacy bool projection
     必须一致：
     - `SupportsSNI`
     - `SupportsALPN`
     - `SupportsOCSPStapling`
     - `SupportsCertificateTransparency`
     - `SupportsSessionTickets`
  2. backend-specific capability publication truth：
     - `OpenSSL`
       - `sslImplCLibrary`
       - `RequiresExternalLibrary=True`
       - `TLS13 / SNI / ALPN / OCSP = stable`
       - `CT = unpublished`
     - `FreePascal`
       - `sslImplNative`
       - `RequiresExternalLibrary=False`
       - `SNI / ALPN / OCSP / CT / SessionTickets / EarlyData = experimental`
       - `PKCS12 / password-protected keys / custom cipher suites / callbacks = unpublished`

- 运行结果继续说明：
  当前 shared regression
  并没有打出新的
  `OpenSSL`
  或
  `FreePascal`
  capability drift；
  它补上的是真正的
  “shared audit 能否第一时间报警”
  这一层能力

- 这意味着后面如果：
  - `OpenSSL`
    的 CT publication
    又被错误抬高
  - `FreePascal`
    的 experimental / unpublished truth
    被无意改写
  - support-level / legacy bool projection
    再次分叉
  这条 shared regression
  会更早直接 fail，
  不需要等更窄的 contract
  或人工读输出才发现

- `tests/test_capability_matrix_v12.pas`
  当前刚补上一个
  明确的审查缺口：
  这条 shared
  capability-matrix regression
  之前只跑
  `OpenSSL / WolfSSL / MbedTLS / WinSSL`，
  漏掉了当前产品主线 backend
  `FreePascal`

- 这批 focused 修复后，
  结论很清楚：
  问题是
  verification coverage hole，
  不是
  `FreePascal`
  fresh capability drift

- static contract
  `tests/scripts/test_capability_matrix_v12_freepascal_coverage_contract.sh`
  现在把这条 truth
  直接钉死成：
  - `test_capability_matrix_v12`
    必须覆盖：
    - `OpenSSL`
    - `FreePascal`
    - `WolfSSL`
    - `MbedTLS`
    - `WinSSL`

- 当前 Linux host
  上的 focused runtime proof
  也直接说明：
  - `OpenSSL`
    executed
  - `FreePascal`
    executed
  - `WolfSSL / MbedTLS / WinSSL`
    继续按 backend-not-available skip
  - summary:
    - `Backends executed: 2`
    - `Contract checks: 6`
    - `Contract failures: 0`

- 这意味着：
  后续如果
  `FreePascal`
  capability truth
  再发生 drift，
  这条 shared regression
  已经更容易第一时间把它打出来；
  我们不需要再靠单独记忆去提醒
  “别忘了 pure Pascal backend”

- `WinSSL`
  这条线现在又多了一层关键证据：
  当前远端 head
  `80b3500`
  已有 fresh
  Windows runtime proof，
  不再只是引用
  2026-05-19
  那批更早的 run

- auto lane
  `winssl-tests.yml`
  在 run
  `26193849105`
  上直接证明了：
  - quick smoke = PASS
  - Wave B Windows gate = PASS
  - broader WinSSL runtime suite = PASS
  - evidence artifact 可下载且内容不是空壳

- 这批最重要的语义结论
  不是“WinSSL 一切都已 production-complete”，
  而是：
  当前 published/runtime truth
  在 Windows runner 上
  又被重新证实了一次，
  尤其是 broader suite
  里的 session lane
  继续保持保守口径：
  - `observed_reuse=false`
  - `session_configured=true`
  - `native_probe_enabled=false`
  所以现在可以更有把握地说：
  - 当前文档里对 WinSSL session 的保守表述
    是真实 Windows evidence 支撑的
  - 但这条 fresh proof
    仍然不能把 wording 升级成
    “observed resumed-handshake success”

- 这也把
  `WinSSL`
  从
  “当前 Linux 下只能静态审查”
  明显推进了一步：
  - 本地改动 → push
  - 手动派发最窄 Windows lane
  - 下载 artifact
  - 读取 runtime markers
  现在已经是一条可重复的闭环

- 新的 workflow-surface 风险也暴露出来了：
  当前 run annotation
  提示
  `windows-latest`
  将在
  2026-06-15
  前后被重定向到
  `windows-2025-vs2026`
  这不是当前失败，
  但应当作为后续
  GitHub Actions truth audit
  的候选项记录下来

- `OpenSSL/MbedTLS/WolfSSL`
  这三条 C-library backend
  在
  `TSSLConfig`
  mixed-scope 线上，
  当前又补齐了两组 runtime parity proof：
  - `LogLevel / LogCallback`
    library-default ownership truth
  - `HandshakeTimeout / BufferSize`
    direct-library reject truth

- 这批最重要的结论是：
  当前缺口确实主要是
  `proof gap`
  而不是新的
  implementation drift。
  focused runtime tests
  直接证明了：
  - `SetDefaultConfig(...)`
    只更新
    `LogLevel`
    与 default-config snapshot
  - `SetDefaultConfig(LogCallback)`
    不会安装 runtime callback
  - `SetLogCallback(...)`
    继续拥有 runtime callback 的唯一 owner truth
  - `Lib.CreateContext(sslCtxClient)`
    会 reject 自定义
    `HandshakeTimeout / BufferSize`
  - request-safe defaults
    仍能成功建 context

- 这意味着
  `TSSLConfig`
  的两条最容易跨层混淆的线，
  现在已经从：
  - source comments
  - docs
  - focused shell contracts
  - factory proofs
  - `FreePascal` direct-library proof
  继续扩到：
  - `OpenSSL`
  - `MbedTLS`
  - `WolfSSL`
    direct-library runtime parity

- 当前还没有 fresh 证据表明
  `OpenSSL/MbedTLS/WolfSSL`
  在这两组字段上存在实际实现分叉。
  所以这批之后，
  如果再回到
  `TSSLConfig`
  mixed-scope 线，
  更该优先查的是：
  - `WinSSL`
    当前平台外的 proof handoff
  - 其他 context-safe 字段
    有没有还没补 runtime parity 的 backend gap
  而不是重新怀疑
  `LogLevel / LogCallback`
  /
  `HandshakeTimeout / BufferSize`
  这两组已收口的 truth

- 这批也再次证明：
  focused shell contract
  最适合守
  backend 覆盖面
  与核心语义；
  runtime test
  最适合守
  owner / reject / dispatch
  这类行为级 truth。
  两者结合，
  才能避免我们以后既重复考古源码，
  又重复从零跑整套重型门禁

- `TSSLConfig.ServerName`
  这条 direct-library compatibility 线，
  当前真正的缺口
  不是实现分叉，
  而是 backend proof
  不对称：
  - `OpenSSL`
  - `FreePascal`
    已有 runtime proof
  - `MbedTLS`
  - `WolfSSL`
    之前更多还是源码 truth

- 补上
  `MbedTLS/WolfSSL`
  direct-library default-config
  focused runtime proof 后，
  当前 durable 结论已经更完整：
  - client default-config `ServerName`
    = warning + ignore
  - server default-config `ServerName`
    = reject
  - empty client default-config
    = no warning
  这三条语义
  现在不再只是
  `OpenSSL/FreePascal`
  的局部事实

- `WinSSL`
  这轮仍保持静态 contract 级证明，
  但至少 direct-library source truth
  已和其他 backend
  一起被放进同一条 focused shell contract

- 这批还有一个工作流层面的结论：
  focused shell contract
  不应把 runtime test
  的 helper 写法钉死。
  初版 contract
  因为要求精确出现
  `TSSLFactory.IsLibraryAvailable(sslMbedTLS)`
  这类实现细节而误打红；
  调整后只守：
  - backend 覆盖事实
  - optional-backend skip 语义
  - warning/reject 核心断言
  这样更耐重构，
  也更符合
  “记录真相而不是记录写法”

- 在
  `session semantics residual sweep`
  里，
  当前最值钱的残留
  已经不是
  API 文档，
  而是普通活跃 examples
  仍在把读者带回
  compatibility-core mirrors

- 这批 examples residual
  暴露出的不只是
  owner-path drift，
  还有一条真正的示例逻辑 bug：
  - `examples/session_resumption_example.pas`
    之前把所有
    `SessionReused=False`
    的后续连接
    都打印成
    `首次握手`
  - 统计里也会把它们重新塞回
    first-handshake bucket
  - 结果就是：
    一旦 warm resume 没命中，
    示例输出和性能统计
    都会继续误导

- 所以这批最小正确动作
  不是只改措辞，
  而是同时收两层 truth：
  - 普通活跃 examples
    统一切回
    `ISSLSessionResumption`
    owner path
  - `session_resumption_example`
    运行输出和统计逻辑
    也开始区分：
    - first handshake
    - observed reuse hit
    - warm miss

- 这条线收完之后，
  session semantics
  在当前活跃入口上的一致性
  明显更高了：
  - API/reference
  - active guides
  - generic examples
  - high-entry user guide
  现在都更少把
  `SetSession(...)`
  /
  `candidate session`
  混写成
  `observed resumed handshake`

- focused residual grep
  也进一步证明：
  当前活跃
  `docs/examples/tests`
  里的 direct-core session 命中
  已基本只剩：
  - intentional proof files
  - 历史 / 当前 plan docs
  这说明
  `ISSLSessionResumption`
  这条 ordinary guidance
  已经不是“局部看起来收口”，
  而是在高入口层面真实收得比较干净

- 上一批
  `MbedTLS`
  /
  `WolfSSL`
  owner-truth proof
  跑完后，
  当前最明显的 residual
  已经不是生产实现，
  而是 active docs
  仍把
  generic session-resumption
  流程写得过强

- `MbedTLS`
  当前源码 truth
  比文档保守得多：
  - `src/fafafa.ssl.mbedtls.connection.pas`
    里
    `DoSetSession(...)`
    只会重置
    `FSessionReused := False`
    然后调用
    `mbedtls_ssl_set_session(...)`
  - `DoIsSessionReused`
    直接返回
    `FSessionReused`
  - 但当前 source 里
    没有和
    `OpenSSL`
    /
    `WolfSSL`
    那种 native reused getter
    对应的翻真路径

- 所以对
  `MbedTLS`
  而言，
  当前最稳妥的 public truth
  应该是：
  - 已发布
    `GetSession / SetSession`
    与
    serialize / deserialize / cache candidate path
  - 但这条线只表示
    configured session
  - 不能被写成
    observed resumed handshake
    已有通用 runtime proof

- 这批 focused docs contract
  的价值很高：
  - 它把
    top-level matrix
    /
    dedicated MbedTLS matrix
    /
    MbedTLS guide
    /
    API reference
    /
    API documentation
    /
    performance guides
    的关键措辞
    全部冻到了同一条 source truth 上
  - 以后即使某个高入口文档
    又飘回
    “会话复用成功”
    这种写法，
    也会被 focused shell contract
    直接打回来

- `SetSession(...)`
  /
  `IsSessionReused`
  这条线在
  `MbedTLS/WolfSSL`
  上继续往下挖之后，
  暴露出的不是 session object
  自己的 metadata 问题了，
  而是连接 owner path
  还缺少一层更具体的可复用 proof：
  - 真实
    `Deserialize(...)`
    出来的 session
    再注入连接时，
    语义会不会继续说真话

- `MbedTLS`
  当前 local-header truth
  已经很清楚：
  - 有
    `mbedtls_ssl_set_session(...)`
  - 有
    `mbedtls_ssl_get_session(...)`
  - 有
    `mbedtls_ssl_session_load/save(...)`
  - 但当前 public helper surface
    没有像
    `SSL_session_reused(...)`
    /
    `wolfSSL_session_reused(...)`
    那样的直接 reused getter

- 这意味着
  `MbedTLS`
  连接侧当前最稳妥的 durable truth
  不是“已经有 post-handshake observed reuse proof”，
  而是：
  - 真实 deserialized native session
    可以被注入连接
  - 但在当前 shipped source / local headers
    范围里，
    不能把这一步自动解释成
    observed resumed handshake

- `WolfSSL`
  这边则正好相反：
  - session class
    已经能通过
    `Deserialize(...)`
    拿回真实 native handle
  - 连接侧也已经有 native
    `wolfSSL_session_reused(...)`
    getter
  - 所以当前最值得锁住的 truth
    不是
    “能不能 SetSession”
    本身，
    而是：
    - owner
      `ISSLSessionResumption.SetSession(...)`
      注入的还是不是这个 deserialized native handle
    - owner
      `IsSessionReused`
      继续读的是 native observed truth，
      而不是
      `session configured`

- focused proof 跑完后，
  这条线现在的 durable 结论是：
  - `MbedTLS`
    deserialized session injection
    已被证明不会制造
    false positive reuse
  - `WolfSSL`
    deserialized session injection
    已被证明能走通 owner path，
    且
    `ISSLConnectionInfo.GetConnectionInfo.IsResumed`
    继续镜像
    `ISSLSessionResumption.IsSessionReused`
    的 native truth

- 这批还有一个对工作流很重要的结论：
  - runtime residual proof 文件集合不应该轻易扩散
  - 新增
    `WolfSSL`
    focused contract
    可以完全走 owner path
  - 所以不需要把 direct-core residual 白名单继续扩大
  - 也避免我们以后又从“哪些 direct core 调用只是 intentional proof”
    这个老治理问题反复拉起

- `MbedTLS/WolfSSL`
  session metadata
  这条线继续往下挖之后，
  暴露出的不是
  `Deserialize(...)`
  成不成功，
  而是：
  session object
  自己会不会继续伪造
  `ID`
  /
  `creation time`
  /
  `timeout`
  /
  `cipher`

- 这批 durable truth 是：
  - `TMbedTLSSession`
    不该再对 native session
    回退到
    GUID
    /
    `Now`
    /
    `TLS1.2`
    /
    empty cipher
  - `TWolfSSLSession`
    也不该再对 native session
    回退到
    GUID
    /
    `Now`
    /
    field-only timeout
    /
    `unknown`

- `WolfSSL`
  这里还补了一条很容易反复踩的 ABI truth：
  - 当前 Linux/CI target
    上 session getter 的时间/超时
    应按
    `clong`
  - 所以：
    - `src/fafafa.ssl.wolfssl.session.pas`
      需要显式引入
      `ctypes`
    - focused tests
      里的
      `StubWolfSSLSessionGetTime`
      /
      `StubWolfSSLSessionGetTimeout`
      也必须跟着改成
      `clong`

- 更关键的是，
  前一批引入 metadata envelope 之后，
  本批 native getter 一旦让
  raw `Deserialize(...)`
  也重新拿回真实
  `protocol/cipher/id/time/timeout`，
  旧的 session-class 断言：
  - “serialize 后必须原样回吐 raw native bytes”
  就不再是真 contract

- 这条线现在真正应该被锁住的是：
  - session 在拿回 native metadata truth 之后，
    `Serialize(...)`
    产出的是
    non-empty
    metadata-complete snapshot
  - 而 public contract
    真正关心的是：
    reload 之后
    metadata 还能不能保持真值，
    而不是 payload
    是否仍逐字节等于旧 raw bytes

- 这也把后续
  `SetSession(...) -> IsSessionReused`
  的准备条件补齐了：
  - 之后如果 reuse proof 失败，
    我们就不需要再怀疑
    session object
    本身是不是还在伪造基础 metadata

- `MbedTLS`
  /
  `WolfSSL`
  的 session roundtrip
  到这一步暴露出的
  已经不是
  “能不能 deserialize 成功”，
  而是
  “deserialize 成功之后 public metadata
  还剩不剩真值”

- 两边之前都已经在 live extraction 路径上
  拿到了真实
  `protocol/cipher`
  - `TMbedTLSSession.FromContext(...)`
    会从
    `mbedtls_ssl_get_version`
    /
    `mbedtls_ssl_get_ciphersuite`
    回填 metadata
  - `TWolfSSLSession.FromConnection(...)`
    会从
    `wolfSSL_get_version`
    /
    `wolfSSL_CIPHER_get_name`
    回填 metadata

- 但旧实现一旦进入
  `Serialize -> Deserialize`
  路径，
  这条 truth
  会立即退化：
  - `MbedTLS`
    退回
    `TLS1.2 + empty cipher`
  - `WolfSSL`
    退回
    `unknown + unknown`

- 这说明
  `ISSLSession`
  在当前仓库里的真实 contract
  不能只看：
  - native handle 有没有回来
  还要看：
  - public metadata
    有没有继续可用

- 这批的最小正确收法
  不是去发明新的
  native getter，
  而是承认当前 backend truth：
  - raw native serialized bytes
    并不稳定携带当前 public metadata
  - 但库内部完全可以在
    public `Serialize(...)`
    surface
    上给 metadata-complete session
    增加 envelope

- 更重要的是，
  这条 envelope 不能破坏旧 payload 兼容：
  - 已有 raw native bytes
    仍要能被
    `Deserialize(...)`
    接受
  - 只有 metadata-complete session
    才值得输出 envelope；
    否则会把旧 focused contract
    也一并打断

- 因而本批 durable truth 是：
  - `TMbedTLSSession.Serialize(...)`
    仅在
    `FCipherName <> ''`
    时输出 envelope
  - `TWolfSSLSession.Serialize(...)`
    仅在
    `FCipherName <> 'unknown'`
    时输出 envelope
  - `Deserialize(...)`
    若识别到 envelope，
    就恢复：
    - `session id`
    - `creation time`
    - `timeout`
    - `protocol`
    - `cipher`
  - 若不是 envelope，
    则继续按旧 raw payload
    语义回退

- 这也把
  `Clone()`
  的真实保证进一步收紧了：
  - 之前我们只收了
    native handle
    不能丢
  - 现在连
    反序列化后的
    `protocol/cipher`
    truth
    也已被 focused tests
    锁住

- `API_DOCUMENTATION`
  顶部 quickstart
  之前还有一层很容易被忽视的 live drift：
  - 它已经不再写错
    `Connect`
    /
    `CreateConnection`
    形状
  - 但因为没有主路径分类，
    读者仍会自然把它读成：
    “这就是当前默认 TLS bootstrap 入口”

- 这类 drift
  跟 section-level mirror drift
  很像，
  但位置更危险：
  - 它不在某个 getter 条目里，
    而是在整页最顶部的
    `5 分钟上手`
  - 所以只要不显式标注，
    之前已经在
    `README`
    /
    `GETTING_STARTED`
    收住的主入口心智，
    还是会被这页重新带偏

- 这再次说明：
  高入口文档的风险
  不只在代码示例是否调用了正确 API，
  还在于它有没有说清楚
  “为什么这里故意走低层入口”

- 这批收口后的 durable truth 是：
  - `README`
    /
    `GETTING_STARTED`
    继续负责 ordinary bootstrap path
  - `API_DOCUMENTATION`
    开头 quickstart
    现在明确只是：
    - active API reference
    - direct `ISSLConnection`
      / owner-surface reference
  - 之所以仍直接使用
    `CreateConnection(...)`
    是因为本页后续要继续展开
    `ISSLOCSPStapling`
    /
    `ISSLCertificateVerification`
    等连接侧 owner surface

- `docs/reference/API_DOCUMENTATION.md`
  在
  `ISSLConnection`
  这层此前已经不再大面积写错签名，
  但还残留一个更隐蔽的 active drift：
  - `README`
    /
    `ARCHITECTURE`
    已把它标成
    slice
  - `API_DOCUMENTATION`
    却仍像在发布完整 shipped source truth

- 这会造成一种“入口级双真相”：
  - landing / architecture 文档教的是
    slice + full reference 回跳
  - active API docs
    却还让读者自然理解成：
    “这就是当前
    `ISSLConnection`
    的完整公开面”

- `GetOCSP*`
  这组 surface
  则暴露出另一种同页漂移：
  - 示例与 prose
    已经 owner-first
    走
    `ISSLOCSPStapling`
  - 但条目层级仍用
    `##### GetOCSP...`
    直接挂在
    `ISSLConnection`
    下
  - 如果没有 section 级 classification，
    读者仍会把它读成核心主接口而不是 compatibility mirror

- 这再次说明：
  active docs
  的风险不只在签名和示例，
  还在 section-level framing；
  只守 snippet truth
  不守 classification truth，
  后续仍会反复从入口语义里把旧心智拉回来

- 这批收口后的 durable truth 是：
  - `API_DOCUMENTATION`
    的
    `ISSLConnection`
    小节现在明确只是当前常用连接方法切片
  - 完整 shipped source truth
    应回到：
    - `src/fafafa.ssl.base.pas`
    - `docs/reference/API_REFERENCE.md`
  - `GetOCSP*`
    在
    `ISSLConnection`
    上仍保留，
    但在 active reference 里已经被明确标记为：
    - compatibility-core mirrors
  - 新代码优先通过
    `ISSLOCSPStapling`
    获取 stapling state / response / verify status / status string

- `docs/reference/API_DOCUMENTATION.md`
  在证书验证这条线上此前存在一类典型的“同页双真相”问题：
  - CT 示例已经使用
    `Supports(Conn, ISSLCertificateVerification, CertVerify)`
  - 但错误处理 / 故障排查片段
    还在直调
    `Connection.GetVerifyResult`
    /
    `Connection.GetVerifyResultString`

- 这说明
  `ISSLCertificateVerification`
  这条 owner-path
  虽然已经在更高层指南和部分 reference 示例里落地，
  但只要某个活跃 reference 页面没被 focused contract
  明确守住，
  它就仍可能局部回退到 direct-core mirror 叙事

- 更关键的 workflow 根因是：
  `tests/scripts/test_active_connection_api_docs_truth_contract.sh`
  此前还在显式要求：
  - `if Connection.GetVerifyResult <> 0 then`
  - `WriteLn('证书验证失败: ', Connection.GetVerifyResultString);`
  这会继续把旧路线包装成“current truth”

- 因而这批再次证明：
  对已经进入 Stage-A owner demotion
  的 surface，
  真正要防反复拉起，
  不能只改文档正文，
  必须同步修 focused contract；
  否则就会出现：
  - 一部分文档已经教 owner path
  - 另一部分文档和合同还在保 direct-core mirror

- 这批收口后的 durable truth 是：
  - 活跃文档的新代码示例
    应优先通过
    `ISSLCertificateVerification`
    获取：
    - `GetVerifyResult`
    - `GetVerifyResultString`
  - `ISSLConnection`
    上的同名方法
    仍是 compatibility mirror，
    但不再适合继续作为 active guidance 主路径

- `docs/README.md`
  里的
  `ISSLConnection`
  摘要此前虽然没有直接写错签名，
  但仍存在高价值 active drift：
  - 它列的是框架集成最小关注面
  - 却没有显式说明：
    这不是完整 shipped source truth
  - 对新读者来说，
    这会自然滑向
    “`ISSLConnection` 当前就只有这些方法”
    的错误心智

- 更关键的是，
  现有
  `tests/scripts/test_isslconnection_convenience_surface_classification_contract.sh`
  此前只守：
  - `API_REFERENCE`
  - `INTERFACE_DESIGN_V2`
  - `ARCHITECTURE`
  - 设计审计报告
  但没有守住
  `docs/README.md`
  这层最高可见入口

- 因而这批的 workflow 收口点是：
  - `README` 这类 landing doc
    即使不是 canonical full reference，
    也必须被纳入 focused contract
  - 否则就会再次出现：
    - canonical docs 已经说清楚
    - 入口文档却仍在继续发布模糊心智

- 这批收口后的 durable truth 是：
  - `docs/README.md`
    里的
    `ISSLConnection`
    代码块现在明确只是：
    - 面向框架集成的最小关注面
  - 完整 shipped source truth
    仍以：
    - `src/fafafa.ssl.base.pas`
    - `docs/reference/API_REFERENCE.md`
    为准
  - `Close`
    /
    `DoHandshake`
    /
    `ReadString`
    /
    `WriteString`
    /
    timeout/blocking
    这类 connection-adjacent / compatibility-core 方法
    不会再被首页摘要静默“消失”

- `docs/guides/MBEDTLS_USER_GUIDE.md`
  里还残留着一条真实的 active API drift：
  - 示例在
    `Connection.Connect`
    失败后调用
    `Connection.GetLastErrorString`
  - 但当前 shipped source
    只在
    `ISSLLibrary`
    上发布
    `GetLastError`
    /
    `GetLastErrorString`

- 同一文件的接口摘要也在继续伪造
  `ISSLConnection`
  surface：
  - 把
    `GetProtocolVersion`
    写成
    `string`
  - 把
    `GetLastErrorString`
    写成 connection-level 方法
  - 而当前源码真相实际是：
    - `GetProtocolVersion: TSSLProtocolVersion`
    - `GetError(ARet: Integer): TSSLErrorCode`
    - 没有
      `ISSLConnection.GetLastErrorString`

- 更关键的 workflow 发现是：
  这条 drift
  之所以没有被后续审查持续暴露，
  不是因为缺测试，
  而是因为现有
  `tests/scripts/test_mbedtls_active_docs_capability_truth_contract.sh`
  正在把旧文档当成正确答案：
  - 它此前明确要求
    `WriteLn('连接失败: ', Connection.GetLastErrorString);`
  - 也没有防止
    `function GetProtocolVersion: string;`
    这类错误摘要继续存在

- 因而这批最值钱的修法不是只改文档，
  而是先把 focused contract
  改成当前源码真相并拿到 RED，
  再最小修正文档；
  这样才能把“文档错了但测试仍绿”的 workflow 反向锁定一起消掉

- 这批收口后的 durable truth 是：
  - backend raw guide
    可以继续展示
    `CreateConnection(...)`
    /
    `ReadString`
    /
    `WriteString`
    这类当前 shipped convenience surface
  - 但错误获取必须回到
    `ISSLLibrary.GetLastError`
    /
    `ISSLLibrary.GetLastErrorString`
  - 接口摘要如果只列常用片段，
    也必须显式标注：
    不是完整
    `v1.5.0`
    source mirror

- shared
  `TX509Certificate`
  在
  `subjectAltName`
  的
  `iPAddress`
  解析里此前只处理了
  `Length=4`
  的 IPv4，
  没有处理
  `Length=16`
  的 IPv6；
  这意味着 parser-backed truth owner
  本身就会在 rich SAN fixture
  上丢掉 IPv6 SAN

- 因而这条线的正确修法顺序必须是：
  - 先修 shared parser
    的 IPv6 SAN truth
  - 再让
    `OpenSSL`
    /
    `WinSSL`
    getter 与
    `GetInfo`
    回收到 parser-backed snapshot
  - 否则如果先把
    `WinSSL SAN`
    切到 parser-first，
    反而会把原先 native path
    已能读出的 IPv6
    也一起丢掉

- `OpenSSL.GetInfo`
  此前虽然 getter
  已基本向 shared parser
  对齐，
  但
  snapshot
  仍是半套 native 投影：
  - `PublicKeySize = 0`
  - `PathLength`
    /
    `PathLenConstraint`
    /
    `KeyUsage`
    /
    `SubjectAltNames`
    也没有统一绑定到同一份 parser snapshot

- `WinSSL`
  这条线暴露出的关键设计问题
  不是“少几个 native case”
  这么简单，
  而是它把同一个
  `ISSLCertificate`
  surface
  发布成了另一套 contract：
  - SAN native path
    只发
    `DNS/IP`
  - EKU native path
    发
    `OID + friendly name`
  - `GetInfo`
    只填
    `SubjectAltNames`
    而不填
    `PublicKeySize`
    /
    `PathLength`
    /
    `PathLenConstraint`
    /
    `KeyUsage`

- 这批收口后得到的 durable truth 是：
  - shared parser
    现在负责
    IPv4 / IPv6 / email / URI
    SAN
    的统一纯值语义
  - `OpenSSL`
    /
    `WinSSL`
    在能加载 parser 时，
    应优先把 getter
    与
    `GetInfo`
    一起投影到同一份 parser snapshot
  - native helper
    只作为 fallback，
    不能再继续发布另一套 pretty-text
    /
    OID+friendly-name
    contract

- `docs/reference/ARCHITECTURE.md`
  之前也存在文档漂移：
  - 仍写着
    `OpenSSL`
    会回退到
    `X509V3_EXT_print`
    文本解析
  - 仍把
    `WinSSL`
    现状描述成已完整枚举
    `RFC822/URL`
    并与
    `OpenSSL`
    一致
  - 实际源码 truth
    直到这批收口前并非如此

- 首次 push
  后的
  `WinSSL Runtime Gate`
  又补出了一条 workflow truth：
  仅靠本地 source contract
  还不够覆盖
  Windows quick-smoke
  的跨单元编译面

- 更具体地说：
  - 远端 quick smoke
    不是行为失败，
    而是在编译
    `test_winssl_certificate_loading`
    时直接报：
    `Identifier not found "X509KeyUsageToStrings"`
  - 这说明
    `WinSSL`
    这次 parser-first 收口
    还需要把
    key-usage string projection helper
    一并补齐，
    否则
    Linux 侧静态 contract
    无法发现这个缺口

- 因而这批又留下一个 durable workflow 改进点：
  - `WinSSL`
    相关 getter
    一旦改成 parser-backed helper
    组合，
    不能只靠 source grep contract；
  - 真正的闭环
    仍需要看
    GitHub Windows quick smoke
    是否能把整个单元图编译过去

- follow-up
  补回
  `X509KeyUsageToStrings`
  后，
  第二轮远端 gate
  `26185903650`
  已经把：
  - `quick smoke`
  - `Wave B`
  - `broader runtime suite`
  全部跑绿

- 这进一步证明：
  - 本批 parser-backed metadata
    收口本身没有破坏
    WinSSL
    运行时行为
  - 首次失败的根因
    确实只是
    Windows-only compile hole，
    不是 metadata 设计方向错误

- 主 backend 的
  `ISSLCertificate.GetExtension`
  此前并不一致：
  - `FreePascal`
    /
    `MbedTLS`
    /
    `WolfSSL`
    走的是 parser-backed
    `hex-or-name`
    truth
  - `OpenSSL`
    返回的是
    `X509V3_EXT_print(...)`
    pretty text
  - `WinSSL`
    返回的是带 `:`
    的原始 hex

- 这说明
  `GetExtension`
  不是 isolated helper，
  而是一个已经被不同 backend 当成 public contract 暴露出去的 surface；
  所以只要还有一个主 backend 偏离，
  调用方就会在同一个
  `ISSLCertificate`
  API
  上遇到不同语义

- `OpenSSL`
  这条线还暴露出一个更隐蔽的设计问题：
  `GetSubjectAltNames`
  /
  `GetKeyUsage`
  /
  `GetExtendedKeyUsage`
  以及 strict-chain EKU gate
  曾经部分依赖
  `GetExtension`
  的 pretty-text fallback

- 因而正确修法不是单点替换返回值，
  而是把这些 fallback
  一并收回到 parser-backed SAN/KU/EKU truth，
  否则 native helper 缺失时会出现新的行为回退

- focused contract test
  也补出了一条 durable workflow truth：
  预期 parser truth
  应该通过公开
  `CreateFreePascalSSLLibrary.CreateCertificate`
  surface
  获取，
  不该直接实例化实现段里的
  `TFreePascalCertificate`
  - 这既避免编译失败，
    也避免后续 focused proof
    误依赖内部类可见性

## 2026-05-20

- `WinSSL certificate.Verify`
  /
  `VerifyEx`
  这轮被进一步证实的根因
  不是 policy flag 本身，
  而是
  `ACAStore`
  只被当成了
  `CertGetCertificateChain(..., hAdditionalStore, ...)`
  的附加 store

- 在
  `CERT_CHAIN_POLICY_BASE`
  这条 cert-level 路径上，
  这意味着：
  - store 里的 CA
    可以参与建链
  - 但不会自动成为 trusted root
  - 所以
    `expired-signer.pem + ca_cert.pem`
    这组夹具
    会先暴露
    `CERT_E_UNTRUSTEDROOT`
    而不是 expiry

- 因而
  `CurrentUser\\ROOT`
  workaround
  虽然能让 focused test
  暂时跑通，
  但它掩盖了真正的 backend 实现缺口：
  WinSSL cert-level public surface
  没有把 custom store 兑现成真实 trust source

- 这批最小正确修法是：
  - 给 WinSSL 补上
    `CERT_CHAIN_ENGINE_CONFIG`
    /
    `CertCreateCertificateChainEngine`
    /
    `CertFreeCertificateChainEngine`
    绑定
  - 在 custom store 存在时，
    为
    `Verify`
    /
    `VerifyEx`
    创建专用 chain engine
  - 通过
    `hExclusiveRoot`
    把该 store
    作为 trust anchor，
    再通过
    `cAdditionalStore`
    让同一个 store
    继续参与建链

- 这样收口后，
  focused WinSSL expiry 契约
  可以重新回到纯 memory-store fixture，
  同时也把
  `Verify`
  与
  `VerifyEx`
  在 custom store 语义上的分叉
  一起堵住

- 最新 Windows run
  `26152137388`
  又补出了一条更窄的 runtime truth：
  trust-root 方向本身已经成立，
  但当前 custom chain-engine helper
  还有一个生命周期 bug

- 证据是：
  - 第一次
    `VerifyEx(..., [], ...)`
    已经正确得到
    `Certificate has expired`
  - 说明：
    - custom store
      已经不再被误判成
      `CERT_E_UNTRUSTEDROOT`
    - `hExclusiveRoot`
      这条 trust 修法
      方向是对的
  - 但第二次
    `VerifyEx(..., [sslCertVerifyIgnoreExpiry], ...)`
    紧接着抛出
    `EAccessViolation`

- 结合当前实现，
  最可信的根因是：
  `CreateChainEngineForStore(...)`
  把
  `CERT_CHAIN_ENGINE_CONFIG.rghAdditionalStore`
  指向了 helper 栈上的临时数组
  - `CertCreateCertificateChainEngine(...)`
    返回后，
    这个地址就不再稳定
  - 后续建链再触发读取时，
    就可能直接访问无效内存

- 因而这条 lane 的更正做法不是撤回 custom trust engine，
  而是把职责拆清：
  - chain engine
    只承载
    `hExclusiveRoot`
    trust-anchor 语义
  - 建链所需的同一个 store
    改为在每次
    `CertGetCertificateChain(...)`
    调用时
    通过
    `hAdditionalStore`
    显式传入

- 再下一轮 Windows run
  `26152785337`
  进一步证伪了上一个怀疑：
  helper-local
  `rghAdditionalStore`
  的生命周期洞
  确实值得修，
  但不是当前最后一个 blocker

- 证据是：
  - 去掉这个洞之后，
    最新 runtime 失败位置
    完全没变
  - 仍然是：
    - baseline
      `VerifyEx(..., [], ...)`
      正确返回
      `Certificate has expired`
    - 一进入
      `VerifyEx(..., [sslCertVerifyIgnoreExpiry], ...)`
      就
      `EAccessViolation`

- 这说明当前真正不稳定的
  很可能不是 custom trust 语义，
  而是
  WinSSL cert-level
  `CERT_CHAIN_POLICY_BASE`
  在
  nonzero `CERT_CHAIN_POLICY_PARA.dwFlags`
  下的 native policy-flag path
  本身

- 这条判断还和更早的另一条证据互相印证：
  - 之前 generated self-signed lane
    一进入
    `sslCertVerifyAllowSelfSigned`
    也曾打出
    `EAccessViolation`
  - 两次共同点
    都是
    “非零 cert-level policy exception flag”

- 所以这条线更稳的最终修法
  不是继续把 public exception flags
  直接压给 Win32 policy，
  而是：
  - 保留 zero-flag native baseline
  - 让 native chain/policy
    先返回真实的
    `CERT_E_EXPIRED`
    /
    `CERT_E_UNTRUSTEDROOT`
  - 再在
    `ISSLCertificate.VerifyEx`
    的 public-contract 层
    做窄范围 override

- 这样做的意义是：
  - custom trust engine
    继续保留，
    不撤回已证明正确的方向
  - 同时把
    `IgnoreExpiry`
    /
    `AllowSelfSigned`
    的最终稳定性
    从不可靠的 native flag lane
    收回到我们自己可验证的 public surface

- 最新 Windows run
  `26153510516`
  又给了我们一条更细的残差信号：
  - 第一条
    `VerifyEx(..., [], ...)`
    已稳定返回
    `Certificate has expired`
  - 说明：
    - custom trust engine
      和
      zero-flag native baseline
      这两个方向没有再偏
  - 但第二条
    `VerifyEx(..., [sslCertVerifyIgnoreExpiry], ...)`
    仍抛
    `EAccessViolation`

- 由于这次崩溃发生在
  “进入 public-contract override 成功路径” 的第一枪，
  当前最小可信 follow-up
  不是再回退设计，
  而是先消掉两个会掩盖真实 fault boundary 的因素：
  - `WinSSL VerifyEx`
    success override
    路径里的
    `Format(...0x%x...)`
  - focused WinSSL test
    自己的
    `FormatVerifyState(...)`

- `src/fafafa.ssl.tls13.primitives.pas`
  与
  `src/fafafa.ssl.crypto.constant_time.pas`
  里这批 warning
  和前两波是同一类根因：
  - managed `TBytes` function result
    在首次
    `SetLength(Result, ...)`
    前没有显式初始化
  - 或零长度分支还在用
    `SetLength(Result, 0)`
    兜底

- 这条线的最小正确修法仍然一致：
  - 在函数入口先
    `Result := nil`
  - 再进行
    `SetLength(...)`
    或 append
  - 零长度直接
    `Exit`
    或保持
    `Result := nil`
    即可

- 这批修完后，
  `python3 scripts/compile_all_modules.py | rg -n "tls13\\.primitives|crypto\\.constant_time|Warning:"`
  的 focused grep
  只剩：
  - `[21/186] 编译 fafafa.ssl.tls13.primitives.pas... ✓ 成功`
  - `[114/186] 编译 fafafa.ssl.crypto.constant_time.pas... ✓ 成功`
  没再伴随这两个单元自己的
  managed-result warning

- `tests/unit/test_constant_time.pas`
  当前不是稳定的回归门：
  - 功能断言
    包括
    `TConstantTime.Select`

## 2026-05-21

- `OpenSSL/WinSSL`
  这条
  `FindBySerialNumber`
  parity
  线，
  首轮看上去像是
  store query
  没做 normalized compare，
  但真正的 OpenSSL RED
  证明问题更深：
  `LCert.GetSerialNumber`
  自己就可能先返回空串

- 进一步缩边界后，
  当前最可信的根因是：
  `TOpenSSLCertificate.GetSerialNumber`
  把 native serial helper
  当成“已经 ready”
  的前提
  - 一旦
    `X509_get_serialNumber`
    /
    `ASN1_INTEGER_to_BN`
    /
    `BN_bn2hex`
    里有任一项
    尚未加载，
    它会直接退出
  - 这样后面的
    pure-Pascal
    `TX509Certificate`
    fallback
    虽然写在源码里，
    但实际上永远走不到

- 同时，
  `SaveToDER`
  也还依赖
  export helper
  事先已经 ready
  - 这意味着：
    即便想回退到
    DER parser
    取 serial，
    也会先被导出层卡死

- 所以这批最小正确修法
  不是只改
  `FindBySerialNumber`
  的比较逻辑，
  而是两层一起收：
  - `OpenSSL`
    /
    `WinSSL`
    store
    统一对 serial query
    做 normalized hex compare
  - `OpenSSL certificate`
    自己补上：
    - native helper lazy-load
    - `SaveToDER`
      lazy-load
    - native path
      失败时
      回退到
      DER / PEM
      + `TX509Certificate`
      parser

- 当前本地证据已经说明
  这条修法方向成立：
  - `OpenSSL`
    focused contract
    重新转绿：
    `9 passed / 0 failed`
  - 这说明：
    - fixture serial
      已不再空掉
    - normalized serial query
      现在可以命中
      同一张证书

- 因而这批结论可以记录为：
  `ISSLCertificateStore.FindBySerialNumber`
  在主 backend 上
  的 residual
  确实是实现层问题，
  不只是测试空白或文档漂移

- 当前剩余唯一未完成 proof
  不是 Linux/OpenSSL，
  而是
  `WinSSL`
  runtime
  需要 push 后
  由
  GitHub Windows CI
  最终确认

- 紧接着往下扫时，
  又抓到了一条同 family 的主 backend residual：
  `OpenSSL`
  /
  `WinSSL`
  的
  `FindByFingerprint`
  目前虽然不再是完全 raw compare，
  但仍只去掉了 `:`

- 这和当前仓库更稳的基线
  已经不一致：
  - `FreePascal.NormalizeFingerprint(...)`
    会统一去掉：
    - `:`
    - `-`
    - 空格
  - optional backend
    之前也已经围绕 normalized fingerprint truth
    做过 focused contract

- 所以这条线的真实问题
  不是“有没有大小写归一化”，
  而是：
  `OpenSSL`
  /
  `WinSSL`
  仍没把 fingerprint query
  真正收回到 compact hex truth

- 首轮
  `OpenSSL`
  focused RED
  也非常干净：
  - getter 正常
  - add/store 正常
  - 只在
    `FindByFingerprint supports normalized fingerprint query variant`
    失败
  - 失败输入正是：
    lower-case
    +
    `-`
    +
    首尾空白
    的 variant

- 因而这批最小正确修法
  不是重构 store design，
  而是直接把：
  - `TOpenSSLCertificateStore`
    的 fingerprint index/query
  - `TWinSSLCertificateStore`
    的 fingerprint compare
  统一改成复用
  `NormalizeCertificateStoreHex(...)`

- 当前本地证据已经说明
  这条修法方向成立：
  - `OpenSSL`
    focused contract
    重新转绿：
    `9 passed / 0 failed`
  - 说明：
    `FindByFingerprint`
    现在已经能正确吃掉
    `-`
    /
    空白
    格式的 query variant

- 因而当前路线图上
  `ISSLCertificateStore`
  query family
  的主 backend residual
  又少了一条：
  现在还未最终 runtime 盖章的
  主要只剩
  `WinSSL`
  push 后
  的 Windows CI 结果
    都是绿色
  - 失败的是
    `Timing variance is acceptable`
    这条统计型检查
  - 它使用
    `GetTickCount64`
    和
    `MAX_DEVIATION = 0.05`
    的粗粒度组合，
    在快机器上会因为大量
    `0ms/1ms`
    采样而放大成伪失败

- 因而当前更准确的 next queue 是：
  - 如果继续沿 shared-helper warning 高 ROI 收口，
    优先转到：
    - `fafafa.ssl.tls13.keyschedule.pas(228,19)`
    - `fafafa.ssl.tls13.clienthello.pas`
      剩余那组 managed-result warning
  - 如果要收测试稳定性债，
    就把
    `test_constant_time`
    的 timing gate
    独立成一个测试工程问题，
    不要和实现层 warning 修复混在一起

- `src/fafafa.ssl.tls13.keyschedule.pas`
  与
  `src/fafafa.ssl.tls13.clienthello.pas`
  这一批和前几波仍然是同一家族问题：
  - empty managed `TBytes` result
    在 unsupported / invalid / builder 入口
    仍靠
    `SetLength(Result, 0)`
    兜底
  - 或直接在没有显式初始化 result 的情况下进入 append 路径

- 这批修法的边界仍然很稳：
  - 不碰 TLS 1.3 transcript / binder / ClientHello 语义
  - 只把目标函数收回到
    `Result := nil`
    作为空结果起点
  - 然后继续原有 append / `Exit(...)` 路径

- `tests/test_tls13_foundation.pas`
  与
  `tests/test_tls13_resumption.pas`
  是这批最合适的真实验证面：
  - `foundation`
    覆盖普通 ClientHello record / handshake 组包
  - `resumption`
    覆盖 PSK ClientHello、
    computed binder、
    binder transcript rebuild、
    early-data ordering

- 这批收口后，
  focused compile grep
  `fpc ... tests/test_tls13_foundation.pas 2>&1 | rg "tls13\\.keyschedule|tls13\\.clienthello|Warning: Function result variable of a managed type does not seem to be initialized"`
  只剩：
  - `Compiling ./src/fafafa.ssl.tls13.clienthello.pas`
  - `Compiling ./src/fafafa.ssl.tls13.keyschedule.pas`
  不再伴随这两个单元自己的
  managed-result warning

- 当前更准确的 next queue 进一步收窄成：
  - shared TLS13 warning 路线的下一批：
    - `fafafa.ssl.tls13.appschedule.pas`
    - `fafafa.ssl.tls13.serverhello.pas`
  - `tests/test_tls13_resumption.pas`
    自身的 managed-result warning
    属于测试 helper 级别，
    可以放在生产单元 warning 收口之后处理

- `src/fafafa.ssl.tls13.appschedule.pas`
  与
  `src/fafafa.ssl.tls13.serverhello.pas`
  这一批仍然和前几波是同一家族问题：
  - empty managed `TBytes` result
    在 resumption secret 派生 / ServerHello builder 入口
    仍靠
    `SetLength(Result, 0)`
    兜底
  - 或直接在没有显式初始化 result 的情况下进入 append 路径

- `tests/test_tls13_resumption.pas`
  的
  `HexToBytes(...)`
  也是同家族测试 helper：
  - 它不是生产实现残口
  - 但和这批 production compile 紧邻，
    顺手收掉能让 focused compile 更干净

- 这批修法的边界仍然很稳：
  - 不碰 TLS 1.3 resumption master secret /
    resumption PSK /
    ServerHello 语义
  - 只把目标函数收回到
    `Result := nil`
    作为空结果起点
  - 然后继续原有 append /
    `Exit(...)`
    路径

- `tests/test_tls13_appschedule.pas`
  /
  `tests/test_tls13_serverhello_builder.pas`
  /
  `tests/test_tls13_resumption.pas`
  是这批最合适的真实验证面：
  - `appschedule`
    直接覆盖 application secret derivation
  - `serverhello_builder`
    直接覆盖 ServerHello handshake / record 组包
  - `resumption`
    继续覆盖 selected PSK / resumption lane

- 这批收口后，
  focused compile grep
  `fpc ... tests/test_tls13_resumption.pas 2>&1 | rg "tls13\\.appschedule|tls13\\.serverhello|test_tls13_resumption|Warning: Function result variable of a managed type does not seem to be initialized"`
  只剩：
  - `Compiling tests/test_tls13_resumption.pas`
  - `Compiling ./src/fafafa.ssl.tls13.appschedule.pas`
  - `Compiling ./src/fafafa.ssl.tls13.serverhello.pas`
  - `Linking tmp/tls13_resumption_bin/test_tls13_resumption`
  不再伴随这三个源自己的
  managed-result warning

- 更重要的是，
  `python3 scripts/compile_all_modules.py 2>&1 | rg "Warning: Function result variable of a managed type does not seem to be initialized"`
  没再匹配到任何结果；
  这说明当前 repo 里这条
  managed-result warning
  主线已经基本被我们收口了

- 因而当前更准确的 next queue
  不再是继续追这同一类 warning，
  而是应切回更高层 completeness 主线：
  - `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
    中剩余的
    `ISSLConnection`
    /
    `TSSLConfig`
    /
    `ISSLServerConnection`
    设计与实现残口
  - 以及独立的测试稳定性 / 其它 warning 家族：
    - implicit string conversion
    - `test_constant_time`
      timing-flaky gate
    格式化

- 与此同时，
  focused test
  现在会输出
  `VerifyEx start/end`
  阶段 trace；
  所以下一轮 Windows CI
  即使仍失败，
  也能直接回答：
  - 崩在
    `VerifyEx`
    函数体内部
  - 还是崩在
    函数返回后的
    结果字符串渲染

- 下一轮远端证明又立刻补出一条非运行时事实：
  - commit
    `406179f`
    对应的
    `WinSSL Runtime Gate`
    `26158271807`
    并没有继续跑到
    `VerifyEx`
    focused runtime
  - 它先在
    `Run quick WinSSL smoke`
    编译阶段失败

- 失败根因已经明确：
  - 不是
    `Format(...)`
    或 trace
    自己
  - 而是我把
    WinSSL
    布尔链
    改成了
    `and then`
  - 当前仓库的
    FPC / ObjFPC
    编译器在这条语法上直接报错：
    `Illegal expression`
    /
    `"THEN" expected but "(" found`

- 所以这个回归的收口方式也很清楚：
  - 保留本轮真正有价值的东西：
    - override-path
      去
      `Format(...)`
    - focused trace
  - 只撤回
    `and then`
    这处不可编译改动

- 新 run
  `26158902571`
  把这条线继续收窄了，而且这次证据更硬：
  - `quick smoke`
    和
    `Wave B gate`
    都恢复为
    PASS
  - 失败重新回到
    `WinSSL Certificate VerifyEx Flag Parity`
    focused runtime

- 更关键的是，
  新增的
  `VerifyEx start/end`
  trace
  直接证伪了
  “外层结果格式化触发 AV”
  这个怀疑：
  - 第一条
    `expired/no-flags/initial`
    能看到完整的
    start/end
  - 第二条
    `expired/ignore-expiry`
    在打出
    start
    之前的准备后，
    直接抛
    `EAccessViolation`
  - 没有任何
    `VerifyEx end`
    记录

- 这意味着：
  - 外层 test helper
    `FormatVerifyState(...)`
    已经不是 fault boundary
  - 问题仍然在
    `TWinSSLCertificate.VerifyEx`
    自己
  - 而且高度集中在
    override success
    这条内部路径

- 当前最小继续收口动作因此不是改测试，
  而是进一步去掉
  override success
  路径里的字符串写入：
  - 不再给
    `DetailedInfo`
    赋值
  - override 成功时也先不写额外 success message
  - 只保留
    `Success/ErrorCode/ChainStatus`
    这三个最小状态写入

- 再看一遍最新 trace 和 focused test 源码后，
  当前最可信根因已经变了：
  - 不是
    `VerifyEx`
    override success
    路径里的字符串写入
  - 而是
    `tests/winssl/test_winssl_cert_verify_ex.pas`
    自己把
    `TWinSSLCertificateStore`
    当类实例持有，
    再临时转换成
    `ISSLCertificateStore`
    传参

- 这是一个典型的
  `TInterfacedObject`
  生命周期陷阱：
  - 当对象只被类引用持有时，
    临时接口参数的
    `_Release`
    可能把对象销毁
  - 之后类变量仍指向已析构对象
  - 下一次把它再转成接口，
    在真正进入被调函数之前
    就可能直接
    `EAccessViolation`

- 这条根因和 Windows runtime 证据逐项吻合：
  - 第一条
    `VerifyEx(..., [], ...)`
    正常返回
  - 第二条
    `expired/ignore-expiry`
    连
    `VerifyEx start`
    都没有打印
  - 所以崩溃点其实在
    helper
    入参求值 / store
    接口转换阶段，
    不在
    `TWinSSLCertificate.VerifyEx`
    函数体内部

- 当前最小正确修法因此是：
  - focused WinSSL test
    改成始终用
    `ISSLCertificateStore`
    接口持有
    memory-backed store
  - 先把测试本身的生命周期洞修掉，
    再看 WinSSL 实现是否还有真实残余

- 最新 Windows 证明
  `26159931322`
  已经把这条根因完全坐实：
  - `WinSSL Runtime Gate`
    全绿
  - `quick smoke`
    /
    `Wave B gate`
    /
    `broader runtime suite`
    全部通过
  - 所以之前 lingering 的
    `EAccessViolation`
    不是 production
    `WinSSL VerifyEx`
    的剩余实现洞，
    而是 focused test 的接口持有错误

- 这条结论对总路线图的意义很大：
  - WinSSL cert-level
    custom-store trust
    + published
    `VerifyEx`
    flag parity
    现在已经有真实
    Windows runtime proof
  - `windows/WinSSL`
    这条长期 blocker
    从“实现未证实”
    收口成了
    “focused lane 已闭环”
  - 后续优先级应回到：
    - public interface design
      剩余漂移
    - 其余 backend
      completeness gaps
    - 测试/文档
      coverage debt
- `OpenSSL certificate.VerifyEx`
  这轮被打出来的真实问题
  不是
  `OCSP`
  /
  `CRL`
  路径，
  而是
  `sslCertVerifyStrictChain`
  在 certificate surface 上被静默忽略

- 这和
  `OpenSSLContext.SetCertVerifyFlags(...)`
  里的
  `X509_V_FLAG_X509_STRICT`
  不是一回事：
  - context 级别的 verify flags
    主要约束
    full connection/runtime path
  - `ISSLCertificate.VerifyEx`
    自己仍然需要兑现
    published flag
    语义

- 新增的 OpenSSL focused RED
  证明了：
  - `tests/certificate/test_certs/signer_cert.pem`
    用真实
    `ca_cert.pem`
    验证时
    默认路径成功
  - 但打开
    `sslCertVerifyStrictChain`
    后，
    旧实现仍然成功

- 这轮顺手还确认了一个
  OpenSSL-specific nuance：
  “缺少
  `extendedKeyUsage`
  扩展”
  不能被简单等同成
  “允许
  `serverAuth`
  ”

- 所以最小正确修法
  不是去重写 OpenSSL verify core，
  而是：
  - 保持
    `X509_verify_cert`
    作为基础链验证真相
  - 在 success path
    上额外要求：
    - EKU 扩展必须显式存在
    - 且必须包含
      `serverAuth`
  - 否则对
    `sslCertVerifyStrictChain`
    fail-closed
- `OpenSSL certificate.VerifyEx`
  在
  `sslCertVerifyIgnoreExpiry`
  /
  `sslCertVerifyAllowSelfSigned`
  这条 lane 上，
  当前真正的实现真相
  被这次 focused contract 重新校正了：
  - 先前把
    `IgnoreExpiry`
    从
    `X509_STORE_set_flags`
    挪到
    `X509_STORE_CTX`
    参数后，
    在当前 Linux/OpenSSL 运行时上
    已经足够阻止同一个 store
    的后续调用被污染
  - 所以“还需要继续深挖 OpenSSL X509 param binding 才能止血”
    这个怀疑，
    在这次重跑中被证伪

- 这次新补的 companion RED
  打出来的真 residual
  其实是：
  `sslCertVerifyAllowSelfSigned`
  根本没有兑现 public truth
  - 旧实现尝试用
    `X509_V_FLAG_PARTIAL_CHAIN`
    近似
  - 但对
    self-signed leaf + empty store
    仍然失败

- 因而这条 lane 的最小正确修法是：
  - 保留
    `IgnoreExpiry`
    走 per-call verify-param
  - 不再把
    `AllowSelfSigned`
    绑定到
    `PARTIAL_CHAIN`
    这种不精确 native 近似
  - 仅在：
    - leaf 证书确认 self-signed
    - 且
      `X509_verify_cert`
      的失败属于
      self-signed / trust failure
      （如
      `DEPTH_ZERO_SELF_SIGNED_CERT`
      /
      `SELF_SIGNED_CERT_IN_CHAIN`
      /
      `UNABLE_TO_GET_ISSUER_CERT_LOCALLY`
      /
      `UNABLE_TO_VERIFY_LEAF_SIGNATURE`）
    时，
    对当前调用做窄范围 success override

- 这样收口后，
  `OpenSSL certificate.VerifyEx`
  在这两个 exception flags 上
  终于与其它 backend
  更接近同一条 public truth：
  - `IgnoreExpiry`
    真能放行过期证书，
    且不污染后续调用
  - `AllowSelfSigned`
    真能放行 self-signed leaf，
    且不污染后续调用
- `FreePascal certificate.VerifyEx`
  这次 focused RED
  把两条之前容易被 runtime/connection 主线掩盖的
  cert-level residual
  明确打出来了：
  - `sslCertVerifyAllowSelfSigned`
    在 cert-level
    `VerifyEx`
    上原本完全没有兑现
  - `sslCertVerifyCheckOCSP`
    原本也没有
    fail-closed
    分支，
    属于典型 round-trip

- 这说明：
  - `FreePascal connection`
    runtime path
    已经覆盖 hostname / revocation material
    等行为，
    不能反推
    `ISSLCertificate.VerifyEx`
    cert-level surface
    也已经完整
  - 同一个 backend
    在
    connection path
    与
    cert-level path
    上
    可能长期存在不同步的 published-flag truth

- 所以这批最小正确修法是：
  - 保留现有
    store verification
    /
    expiry
    /
    strict-chain
    路径
  - 仅对
    self-signed leaf
    + `AllowSelfSigned`
    做窄范围 success override
  - 把
    `CheckOCSP`
    收紧成与其它 backend
    一致的 fail-closed

- 同一轮静态审查还确认了下一个高价值 residual：
  `WinSSL certificate.VerifyEx`
  当前 source
  仍明显缺：
  - `IgnoreExpiry`
  - `AllowSelfSigned`
  - `StrictChain`
  的 cert-level 兑现分支；
  这条线应作为下一批默认入口保留
- `WolfSSL`
  当前
  `Verify`
  /
  `VerifyEx`
  原实现里最硬的 bug
  不是“结果字段不够丰富”，
  而是
  `Verify`
  只要
  `GetIssuer = CACert.GetSubject`
  就直接返回
  `True`

- 这意味着：
  - 只要给它一个
    subject 相同
    但密钥错误的
    CA
  - 旧实现也会把它误判为验证成功
  - 属于实打实的
    false positive

- 新增的
  `tests/certs/ca-subject-imposter.pem`
  把这条 bug
  固化成了可重复的 RED，
  避免下次又回到
  “看起来像链上了”
  的弱判断

- `MbedTLS`
  这批真正被打出来的
  backend gap
  不只是
  `VerifyEx`
  结果字段简陋；
  更底层的是
  `TMbedTLSCertificateStore.AddCertificate`
  之前没有把新增证书
  同步进
  native
  `mbedtls_x509_crt`
  CA chain

- 所以旧行为会出现：
  - query / fingerprint / build-chain
    看上去都能找到证书
  - 但
    `Verify`
    /
    `VerifyEx`
    走 native verify 时
    仍然像“空 trust store”

- 这说明
  optional backend
  证书存储
  之前存在
  “public list truth”
  和
  “native verification truth”
  分裂

- 这批新测试还顺手暴露出一个
  shared safety smell：
  多个 backend 的
  `VerifyEx`
  之前都用
  `FillChar`
  初始化
  `TSSLCertVerifyResult`
  这种带
  `string`
  字段的 record

- 本轮已经顺手把
  `OpenSSL`
  /
  `WinSSL`
  /
  `FreePascal`
  /
  `MbedTLS`
  /
  `WolfSSL`
  这几处初始化
  全部改成
  显式字段重置，
  避免继续传播这类 unsafe pattern

- `WolfSSL`
  这一批更稳的修法
  不是硬拉
  不存在的
  store-context
  绑定，
  而是：
  - 复用当前仓库已有的
    `TX509Certificate`
    解析真相
  - 结合
    `tls13.servercertverify`
    里的 RSA / ECDSA
    纯 Pascal 签名校验
  - 先把
    假成功
    收紧成
    最小可信验证

- 最终收口后的 live truth：
  - `WolfSSL Verify`
    不再用
    issuer/subject
    文本命中
    代替签名校验
  - `WolfSSL VerifyEx`
    现在会：
    - 构链
    - 校验 validity
    - 校验 issuer 签名
    - 检查 strict-chain serverAuth
    - 对 revocation / CRL / OCSP
      fail-closed
    - 填写
      `ErrorCode`
      /
      `ChainStatus`
      /
      `RevocationStatus`
      /
      `DetailedInfo`
  - `MbedTLS VerifyEx`
    现在会：
    - 对 nil store / invalid handle
      给出明确错误
    - 在 success path
      填充
      `DetailedInfo`
    - 对 revocation / CRL / OCSP
      fail-closed
    - 对 strict-chain
      检查
      `serverAuth`

- 上一批
  `verification truth`
  收口后，
  `MbedTLS VerifyEx`
  的真实 residual
  不是整个 verify pipeline 失效，
  而是两个已经发布的 exception flags
  还停留在
  “API 接受但 live 结果不变”：
  - `sslCertVerifyIgnoreExpiry`
  - `sslCertVerifyAllowSelfSigned`

- 本机
  `mbedtls_x509_crt_verify`
  已经通过 verify bits
  给出了足够细粒度的 native truth：
  - `MBEDTLS_X509_BADCERT_EXPIRED`
  - `MBEDTLS_X509_BADCERT_FUTURE`
  - `MBEDTLS_X509_BADCERT_NOT_TRUSTED`

- 所以这批最小正确修法
  不是重写整个
  `MbedTLS`
  verify path，
  而是：
  - native verify
    仍然先做
    authoritative chain check
  - 仅在调用方显式请求时，
    对对应 failure bits
    做有边界的掩码放行

- 这样能保住两个关键边界：
  - 不会为了支持
    `IgnoreExpiry`
    /
    `AllowSelfSigned`
    又把其它 trust / chain 错误一起放掉
  - success path
    仍然来自
    native verify truth，
    不是另起一套弱语义分支

- 新增的
  `tests/certs/expired-signer.pem`
  把
  expiry 例外路径
  固定成了稳定 RED；
  同时
  `version1-cert.pem`
  继续作为 self-signed control fixture，
  让
  `WolfSSL`
  这组测试
  成为可靠的对照组
- `gh run view 26143487129`
  最终
  `conclusion=success`，
  说明上一批
  certificate version truth
  的 push
  没有引入
  Linux CI 回归；
  这轮可以直接继续往下切
  下一条 residual surface

- 这次时间面审查里，
  最初怀疑的
  `WolfSSL`
  DER/native
  路径丢失 validity
  并没有在 focused RED
  里复现；
  它反而成了更好的
  control group

- 真正红灯的是
  `MbedTLS`
  空证书状态：
  - `GetNotBefore`
    伪造
    `Now - 365`
  - `GetNotAfter`
    伪造
    `Now + 365`
  - `GetDaysUntilExpiry`
    也因此继续产出
    非零假数据

- 这类问题
  不是普通 fallback
  “不够优雅”；
  它会把
  unknown certificate state
  错报成
  有明确 validity window，
  属于 public truth
  被默认值壳污染

- `TMbedTLSCertificate`
  当前其实已经具备
  更稳的数据源：
  - `TryLoadX509Parser(...)`
  - `SaveToDER`
  - `TX509Certificate.Validity`

- 所以这批最小正确修法
  不是继续强依赖
  `mbedtls_x509_crt_info(...)`
  的文本切片，
  而是：
  - 时间 getter
    优先复用 parser truth
  - parser / fallback
    都拿不到数据时
    返回
    `0`
  - `IsExpired`
    /
    `GetDaysUntilExpiry`
    对 unknown time
    fail-closed

- `MbedTLS`
  当前
  `GetVersion`
  的真实残留
  不是没有数据源，
  而是实现仍固定返回
  `3`

- 这条问题之前一直没被打成
  真正 RED，
  主要是因为仓库里
  没有现成非 v3 fixture，
  不是因为 optional backend
  没法表达版本真相

- 用最小 OpenSSL config
  配合
  `openssl req -new -x509 -x509v1`
  可以稳定生成
  真实
  X.509 v1
  证书；
  这比 mock / field patch
  更接近 public runtime truth

- 还顺手暴露了一个
  repo workflow 细节：
  `tests/certificate/test_certs/`
  下新增文件
  会被
  `.gitignore`
  里的
  `tests/**/test_*`
  模式吞掉，
  因为目录名
  `test_certs`
  本身命中规则

- 所以这批更稳的夹具落点
  不是继续往
  `tests/certificate/test_certs/`
  塞新文件，
  而是放到
  `tests/certs/`
  这类已跟踪路径，
  避免后续每次都要
  `git add -f`

- 最终收口方式是：
  - 用真实 v1 fixture
    证明
    `WolfSSL`
    已经能发布正确版本真相
  - 再把
    `TMbedTLSCertificate.GetVersion`
    改成 parser-backed truth
    而不是继续默认 `3`

- `WinSSL`
  当前 public
  `GetSubject`
  /
  `GetIssuer`
  的真正漂移点
  不是
  certstore query
  本身，
  而是 getter
  仍停在
  `CERT_NAME_SIMPLE_DISPLAY_TYPE`

- 这会导致：
  - public getter
    暴露的
    仍偏向
    display name
  - 但同仓库里
    `FindBySubject`
    /
    `FindByIssuer`
    已经基于
    full DN
    做匹配

- 所以这里更稳的修复
  不是继续补
  query-side
  normalization，
  而是让 public getter
  直接回到
  native
  `CERT_INFO.Subject/Issuer`
  的
  `CertNameToStrW(..., CERT_X500_NAME_STR or CERT_NAME_STR_COMMA_FLAG, ...)`
  full-DN path

- 这也说明
  `WinSSL certstore`
  之前的 distinct-issuer runtime closeout
  并没有覆盖
  public getter truth；
  需要把确定性 fixture
  的 getter 断言
  并入
  `tests/winssl/test_winssl_certstore.pas`
  才能避免之后再从静态审查重拉

- `WinSSL`
  当前
  `BuildCertificateChain`
  还有一条更隐蔽但更硬的
  runtime 风险：
  它把
  `ISSLCertificate`
  先转成
  raw pointer
  存进 `TList`，
  再在后面转回 interface

- 这会绕开
  interface refcount；
  一旦循环里
  `ChainCert`
  被下一次赋值覆盖，
  较早的链元素
  就可能先释放，
  留下悬空引用

- 所以这条问题
  不是简单的
  “数组转换风格不优雅”，
  而是
  WinSSL chain result
  在多元素链上
  存在 use-after-free
  级别风险

- 另外，
  `tests/winssl/test_winssl_certstore.pas`
  目前还有一个
  workflow 真相问题：
  即使有
  `Assert` 失败，
  程序结尾
  也不会
  `Halt(1)`

- 这意味着：
  - Windows runtime suite
    即使跑到了失败断言
  - `run_winssl_tests.ps1`
    仍可能因为
    进程退出码是 `0`
    而把该测试记成 PASS

- 所以这一批
  除了补 WinSSL
  partial/full chain
  contract 本身，
  还必须顺手修掉
  certstore test harness
  的 failure propagation；
  否则新增 contract
  也不具备真正的
  CI 证明力

- 这次远端红灯还顺手证实了
  一个 WinSSL
  backend completeness gap：
  `TWinSSLCertificate.LoadFromFile`
  当前只把文件内容
  当 DER 走，
  不会在失败后
  fallback 到 PEM

- 但从整个仓库
  current public truth
  看，
  `*.pem` 证书文件
  一直都是正常 caller input；
  所以这里更合理的修复
  不是把测试改回
  手工读文本再 `LoadFromPEM`，
  而是把 WinSSL 的
  file-loading surface
  补齐到
  DER + PEM

- 继续深挖同一段远端日志后，
  更直接的失败边界
  也被钉住了：
  `TestDeterministicDNQueryContract`
  在
  `tests/winssl`
  工作目录下
  仍然拿
  repo-root
  相对路径去读 fixture

- 也就是说，
  上一轮红灯
  实际同时暴露了两件事：
  - WinSSL `LoadFromFile`
    的 PEM fallback
    还不够完整
  - WinSSL certstore test
    自己的 fixture-path
    也没有对 runtime
    工作目录做收口

- `OpenSSL`
  当前
  `BuildCertificateChain`
  不是单纯
  “少查了一次 issuer”，
  而是把
  整个 store
  都塞进了
  shared verifier
  的 trusted store

- 这会和
  `TSSLCertificateChainVerifier.IsRootCertificate`
  当前语义直接冲突：
  - 只要
    `FTrustedStore.Contains(CurrentCert)`
    为真，
    就把它当成 root / trust anchor

- 对 OpenSSL store 来说，
  intermediate
  也同样在
  `Self`
  里；
  所以当链走到
  intermediate
  那一跳时，
  即使 store
  里还持有真正的
  self-signed root，
  也会被提前截断

- 这类 drift
  不会被
  “store 只有 intermediate”
  的最小链表象揭露，
  因为那种情况下
  `leaf -> intermediate`
  恰好也是允许的结果；
  真正能戳穿它的 contract
  是：
  - store 同时持有
    `intermediate + root`
  - 期望
    `BuildCertificateChain`
    返回
    `leaf -> intermediate -> root`

- 所以这批修复
  不能只改
  `FindIssuer`
  或
  `Contains`
  的某个局部判断，
  而要在
  OpenSSL certstore
  调用 shared verifier
  之前，
  先把
  “谁是 trust anchor”
  和
  “谁只是 intermediate”
  分层表达出来

- 对 OpenSSL backend
  更稳的接法不是
  “把 native store
  整体映射成 trusted store”，
  而是：
  - self-signed certs
    充当 trust anchors
  - non-self-signed certs
    充当 intermediate pool
  这样 shared verifier
  的现有终止逻辑
  才不会误把
  intermediate
  当作链终点

- 这也补齐了一个更稳定的
  public contract：
  `BuildCertificateChain`
  应尽量沿 issuer path
  往上拼接到
  self-signed root；
  找不到下一跳时，
  才退化为
  partial chain

- generic
  `TSSLCertificateChainVerifier`
  这次暴露的是
  更底层的一类 shared drift：
  不是某个 backend
  单独的 store query
  没对齐，
  而是 shared
  chain verifier
  自己对 trusted store
  把 issuer lookup
  走成了
  `FindByIssuer`

- 正确语义应该是：
  - 拿当前证书的
    `Issuer`
  - 去找
    “谁的 subject
      等于这个 issuer”
  所以 trusted store
  这里应该走
  `FindBySubject`

- 这类 bug
  很容易被
  “root 自签名证书
  issuer = subject”
  掩盖，
  因为到了 root
  那一跳它恰好又能命中

- 真正能把它戳穿的
  focused contract
  是：
  - trusted store
    只持有
    non-self-signed
    intermediate anchor
  - 这时如果查错方向，
    `BuildChain`
    就会直接找不到 issuer

- WinSSL certstore
  那条 Windows lane
  这次也已经通过远端 CI
  完整证明：
  真问题就是 test drift，
  不是 runtime 实现缺方法

- 这次
  `WinSSL Runtime Gate`
  的红灯
  进一步证明了一个
  很典型的项目停滞陷阱：
  不是 backend 真缺功能，
  而是
  Windows-only 测试文件
  还在调用旧 concrete API
  却把变量类型写成了
  shared interface

- 也就是说，
  `test_winssl_certstore.pas`
  这次暴露的问题，
  本质不是
  `TWinSSLCertificateStore`
  少了
  `Open` / `Close` / `IsOpen`
  等方法，
  而是
  test harness
  把
  `ISSLCertificateStore`
  和
  `TWinSSLCertificateStore`
  的边界又混回去了

- 这同样说明：
  “当前接口设计是否有问题”
  不只是看源码声明，
  还要看各 backend 自己的 runtime test
  有没有继续拿旧 concrete surface
  冒充 shared contract

- `TWinSSLCertificateStore.Create(const AStoreName: string)`
  真实语义是：
  - `AStoreName <> ''`
    时立即打开对应系统 store
  所以旧测试里
  `Create('MY')`
  后断言
  `not IsOpen`
  也是错误历史真相

- `BuildCertificateChain`
  这条 public surface
  继续往下审，
  真正的新问题不是
  “返回数组就算完成”，
  而是
  certificate
  自己已经持有的
  `issuer-link truth`
  有没有真的被 chain builder 消费

- `FreePascal`
  之前已经把这条 truth
  打通了：
  - 先读 `GetIssuerCertificate()`
  - 再 fallback 到 store lookup
  - 追加前做 object / fingerprint 去重

- `MbedTLS` /
  `WolfSSL`
  之前都还只是：
  - append current
  - `FindBySubject(GetIssuer)`
  - max depth break
  这意味着：
  - earlier peer-cert /
    clone issuer-link 修复
    不能传导到 certstore
  - public `ISSLCertificate`
    明明公开了 issuer-link，
    但 optional backends
    的 chain builder
    实际上视而不见

- 这次还暴露出一个
  很容易骗过审查的测试问题：
  `FreePascal`
  旧的 chain-dedup 用例
  用的是 self-signed 证书，
  所以在
  `IsSelfSigned`
  处就提前结束，
  并没有真的覆盖
  non-self-signed issuer-link path

- 所以更稳的 shared contract
  应该是：
  - 如果 leaf cert
    已经携带显式 issuer-link
  - 即使 store
    里没有 issuer
  - `BuildCertificateChain`
    也应该返回
    leaf -> issuer
    的最小链

- 一旦开始消费显式 issuer-link，
  loop suppression
  就不能再省略；
  否则 chain builder
  很容易在 clone /
  cycle 场景下
  只靠深度上限硬停

- 当 `ISSLCertificateStore`
  的 query family
  收口以后，
  下一层很容易被忽略但同样属于
  shared public contract
  的问题，
  就是：
  clone / duplicate / remove
  到底按对象身份，
  还是按 certificate truth

- `MbedTLS`
  这次证明了一个很典型的实现残缺：
  query semantics
  已经对齐了，
  但 store ownership semantics
  还停在最原始的
  `IndexOf(ACert)`
  级别

- 对调用方来说，
  `ISSLCertificateStore`
  如果已经把
  `Contains`
  / duplicate reject
  / `RemoveCertificate`
  建立在 fingerprint truth 上，
  那么 clone
  就不该再被当作另一张证书；
  否则“同一证书”这个 public 概念
  在不同 backend 上
  还是会裂开

- optional backend 审查
  也再次说明，
  “接口设计看起来已经完整”
  并不等于
  “生命周期 / ownership semantics
  也已经完整”

- 这次远端 `WinSSL Runtime Gate`
  的失败同样给了一个重要提醒：
  workflow 新接入一个测试后，
  红灯不一定是实现错，
  也可能是
  test project file
  自己保留了错误 target truth

- `test_winssl_certstore.lpi`
  这次不是业务逻辑失败，
  而是硬编码了
  `TargetOS=linux`，
  导致 Windows runner 上的
  `lazbuild`
  试图编 Linux target，
  直接在编译前就摔倒

- 所以
  “让 Windows CI 真正替代本地 Windows 条件”
  的前提，
  不只是把测试接进 workflow，
  还要保证对应 `.lpi`
  的 target truth
  不再藏着历史平台残留

- 当我们把 optional backends
  的 query family
  收口完之后，
  真正浮出来的
  下一个 shared 设计债
  不是更多 getter，
  而是
  `ISSLCertificateStore`
  的 DN query contract

- 当前仓库最稳的 public 语义
  不是“只允许 full DN exact match”，
  而是：
  - normalized DN query
  - partial DN fragment lookup
  - empty query fail-closed
  因为这才与：
  - system-store 搜索习惯
  - 现有 store smoke
  - optional backends
    已落地语义
  更一致

- 但实现层如果直接只做
  substring，
  会把内部
  `FindBySubject(LCurrent.GetIssuer)`
  这类 full-DN path
  放宽得太粗；
  所以
  exact-first + substring fallback
  是更稳的实现细节

- `signer_cert.pem`
  这批再次证明是对的夹具，
  但也暴露出一个
  很容易误判的问题：
  当前各 backend
  吐出的 DN 顺序
  是
  `CN -> O -> L -> ST -> C`，
  不是一开始拍脑袋假设的
  `O -> CN`
  所以 query contract
  必须强调
  “支持 partial fragment”
  而不是把测试写成
  对某种 DN 序列化顺序的死绑定

- `OpenSSL`
  这批真正缺的
  不是 substring 能力，
  而是
  subject / issuer cache
  没有缓存 normalized DN truth
  所以
  query 只是 upper-case
  仍然不够

- `WinSSL`
  这批也再次证明，
  不能把 public contract
  完全外包给
  `CertFindCertificateInStore`
  这种 backend-native 搜索语义；
  共享接口层
  需要自己对查询做归一化，
  然后基于缓存对象
  发布 repo 级一致结果

- 这意味着
  `ISSLCertificateStore`
  的“完整实现”
  不能只看：
  - 能不能打开系统存储
  - 能不能按 native API 查
  还要看：
  - shared interface
    对同一类人类输入
    是否在主要 backend 上
    给出一致 truth

- optional backends
  的 certificate store query family
  继续往下收口后，
  `FindByIssuer`
  也暴露出
  和前两条一样的
  “原始字符串比较”问题

- 这次如果继续用
  self-signed fixture，
  很容易把
  `subject` / `issuer`
  取错字段
  也测不出来；
  所以用
  `signer_cert.pem`
  这种 distinct-issuer fixture
  是必要的

- `MbedTLS` / `WolfSSL`
  这批都证明：
  当前 `FindByIssuer`
  的真实缺口
  不是“找不到证书 API”，
  而是
  issuer query
  还没有进入 normalized text truth

- 到这一批为止，
  optional backends
  的 store query family
  已经形成一条更清晰的内部路线：
  - `FindBySubject`
  - `FindBySerialNumber`
  - `FindByIssuer`
  都应该至少先摆脱
  原始展示字符串比较

- 但这次也同时暴露出
  更上层的接口设计债：
  `FindByIssuer`
  在全仓库范围内
  仍然没有一个统一 canonical contract
  - `FreePascal`
    更像 exact match
  - `OpenSSL` / `WinSSL`
    更像 substring match
  - optional backends
    现在先被收口到
    与自己当前 query family
    更一致的 normalized substring truth

- 也就是说，
  这批修的是
  “optional backends 不该再掉队”的实现问题，
  不是
  “全仓库 issuer-search 语义
  已经最终统一”

- optional backends
  的 certificate completeness
  继续往下收口后，
  public getter
  之外的另一层真实残缺
  已经浮出来了：
  store query semantics
  也会直接分叉

- `FreePascal`
  其实已经给出了
  更稳的 store query contract：
  - `FindBySubject`
    允许大小写 / 分隔符空格归一化
  - `FindBySerialNumber`
    允许 `AA:BB` / `aabb` / 带空格
    这类展示格式差异

- 但 optional backends
  在这层长期落后：
  - `MbedTLS`
    subject / serial
    都还是原始字符串比较
  - `WolfSSL`
    只补了 subject，
    serial 仍是原始字符串比较

- 这类问题看起来不像
  getter AV
  那么“炸裂”，
  但它会把
  同一张证书
  在不同 backend
  上的 lookup truth
  直接分裂成
  “能查到 / 查不到”

- 也就是说，
  `ISSLCertificateStore`
  的完整性
  不能只看：
  - 能不能加证书
  - 能不能删证书
  - 指纹查找通不通
  还要看：
  - 面向人类输入的
    subject / serial 查询
    是否有基础归一化能力

- 这一批也再次证明，
  optional backend
  最高价值的收口路线
  不是再补一堆新 native helper，
  而是优先把
  仓库内部已经存在的
  shared/public contract
  对齐过来

- 直到这一批之前，
  `FreePascal`
  已经有的 normalized query truth
  其实还没有真正推广到
  `MbedTLS` / `WolfSSL`
  这说明
  “接口设计已存在”
  并不等于
  “各 backend 实现已经完整”

- optional backends
  的 certificate completeness
  继续往下挖后，
  最危险的残缺
  不再是“空串”
  或“默认算法名”，
  而是
  identity getter
  已经可能直接发布错误 truth
  甚至触发异常

- `TWolfSSLCertificate.GetSerialNumber`
  这次暴露出来的
  不是简单格式问题，
  而是把 serial 对象指针
  当成 public serial value；
  在 focused contract 下
  这会进一步表现成
  `EAccessViolation`
  风险

- 这说明，
  optional backend 审查
  不能只盯
  “返回值看起来是不是空/默认”，
  还要盯：
  - getter 是否安全
  - getter 是否发布真实 X.509 truth

- `TX509Certificate`
  这批再次证明
  不只是 metadata helper，
  它已经足够作为
  optional backends
  的 certificate identity truth owner：
  - `Subject.ToString`
  - `Issuer.ToString`
  - `Subject.CommonName`
  - `SerialNumberAsHex`

- 所以
  `MbedTLS` / `WolfSSL`
  上
  `GetSubject` /
  `GetIssuer` /
  `GetSerialNumber`
  的正确路线
  不是继续各自维护
  native 文本切片/one-line helper 语义，
  而是优先复用同一条 parser truth path，
  native 路径只做 fallback

- `GetVersion`
  虽然仍是
  certificate identity surface
  的残余疑点，
  但当前仓库夹具全是 `Version: 3`；
  在没有非 v3 fixture 前，
  继续做这条线很容易变成
  没有有效 RED 的“实现猜测”

- 到这一批为止，
  optional backends
  的 certificate surface
  已连续收口四层高价值缺口：
  - algorithm metadata
  - public surface
  - extension metadata
  - identity getters
  这比继续回头讨论 release 流程
  更贴近当前总 goal
  里的
  “接口设计完整 / 各 backend 实现完整”

- 在 optional backends
  证书实现里，
  算法 metadata
  和扩展 metadata
  收口之后，
  还残留一层更基础的
  public surface 空壳：
  - `GetPublicKey = ''`
  - `GetExtension = ''`

- 这批确认了
  `GetPublicKey`
  在当前仓库里的真实 contract
  不是“完整公钥导出”，
  而是
  与 `OpenSSL` /
  `FreePascal`
  一致的最小语义：
  - 返回算法标识字符串

- 所以 optional backends
  的正确补法
  不是额外补 native EVP / PEM API，
  而是先把
  已有 contract
  补齐到一致

- `GetExtension`
  这层同样不需要
  新 native binding
  才能收口，
  因为
  `TX509Certificate.Extensions`
  已经保留了：
  - `OID`
  - `Name`
  - `Value`

- optional backends
  证书审查的稳定模式
  进一步清晰了：
  - 优先把
    `TX509Certificate`
    当作 parser truth owner
  - 先收掉
    public surface
    上的空壳/默认壳
  - 只有在 parser truth
    本身不存在时
    才考虑追加 native binding

- 本批之后，
  `MbedTLS` / `WolfSSL`
  在 certificate public surface
  上又少了两处
  “接口存在但加载后仍为空”
  的残缺

- 上一批把
  算法元数据默认壳
  收掉之后，
  optional backends
  证书 surface
  的下一条高价值缺口
  不是新的算法名，
  而是
  “扩展类元数据
  在 getter / `GetInfo`
  之间继续分裂”

- 当前最直接的残缺是：
  - `TMbedTLSCertificate.IsCA`
    仍固定 `False`
  - `TWolfSSLCertificate.GetKeyUsage`
    / `GetExtendedKeyUsage`
    仍固定空数组
  - 两边 `GetInfo`
    都没有完整填：
    - `PublicKeySize`
    - `IsCA`
    - `SubjectAltNames`
    - `KeyUsage`

- 这比单个 getter 更危险，
  因为调用方通常会把
  `GetInfo`
  当作快照 truth；
  如果 snapshot 缺字段，
  上层逻辑就会在
  “getter 有值 / snapshot 没值”
  之间继续漂移

- 这批也说明，
  `TX509Certificate`
  不只是算法名 fallback 工具，
  它其实已经是 optional backends
  最稳的扩展 truth owner：
  - `BasicConstraints`
  - `SubjectAltNames`
  - `KeyUsage`
  - `ExtKeyUsage`
  - `PublicKeyInfo.KeySize`
  都已经在那里统一解析好了

- `MbedTLS`
  这批真正暴露出来的
  不是 parser 解析能力不足，
  而是证书对象
  在多次 `LoadFromFile(...)`
  之间
  没有清理旧的
  `FDERData` /
  `FPEMData`
  缓存

- 这意味着，
  之前某些“看起来像 extension parser 不支持”
  的现象，
  实际上只是
  第二张证书开始
  还在读取第一张证书的 cached snapshot

- 所以 optional backends
  证书审查里
  不能只看单次加载后的 truth，
  还必须看：
  - 同一对象反复 load
    不同 cert
    时
    snapshot 有没有跟着切换

- 这批收口后，
  `GetInfo`
  不再只是“部分字段随缘可用”的半快照，
  而开始真正接近
  一个可供上层直接消费的
  structured certificate snapshot

- `CI` run `26131410258`
  现已完整 `success`，
  说明上一批
  `WinSSL` / workflow truth
  收口已经正式落地；
  当前不该再把控制面状态当作主阻塞

- 下一条更贴近
  “接口设计完整 / 各 backend 实现完整”
  的真实缺口是：
  `MbedTLS` / `WolfSSL`
  证书算法元数据
  仍保留
  `RSA` /
  `SHA256withRSA`
  默认壳

- 这类问题比文档漂移更直接，
  因为它已经落在
  `ISSLCertificate`
  的实际 published getter surface 上：
  - `GetPublicKeyAlgorithm`
  - `GetSignatureAlgorithm`
  - `GetInfo`
    中对应字段

- 现有仓库里其实已经有更稳的修复路径，
  不需要继续扩 native binding：
  - `TFreePascalCertificate`
    已复用
    `TX509Certificate`
    解析
    `PublicKeyInfo.Algorithm.Name`
    和
    `SignatureAlgorithm.Name`
  - optional backends
    直接复用同一路径
    就能把这条真相补齐

- 现有 framework tests
  把默认值当成真相
  本身也是 drift：
  - 当测试继续断言
    `RSA`
    / `SHA256withRSA`
    时，
    它会把后来的人持续带离
    “真实证书元数据应该被发布”
    这条主线

- 这批 RED/GREEN 也再次证明：
  对 optional backends 的完整性修复，
  最稳的路线
  不是继续追逐
  各家 native helper
  的局部 binding，
  而是优先复用仓库里
  已经稳定存在的
  pure-Pascal `TX509Certificate`
  truth path

- 这样做的好处是：
  - `MbedTLS` / `WolfSSL`
    不需要再分别维护
    一套算法名映射
  - `FreePascal` / optional backends
    最少可以共享
    同一份
    OID -> Name
    解析真相
  - framework tests
    也能围绕同一类
    ECDSA fixture
    写成可复用 contract

- `WinSSL`
  这次最高价值缺口
  已经从
  “某一行文档写错”
  转成了
  “自动验证控制面缺一条 Windows lane”

- 也就是说，
  当仓库已经有：
  - quick smoke
  - Windows Wave B gate
  - broader WinSSL suite
  这些真实 runtime 脚本时，
  下一条正确路线
  就不该继续把它们
  只留在
  `workflow_dispatch`
  手动链路里

- 否则会持续出现一种误导：
  - `ci.yml`
    全绿
  - 但 WinSSL
    实际并没有自动 runner 证明
  这会让“测试完整性”
  看起来比真实情况更高

- 所以 workflow 本身也属于
  “接口 / 实现 / 测试 / 文档”
  这条总路线的一部分：
  - 如果能力真值只能靠人手工点 workflow 才会被验证，
    那么 testing completeness
    仍然是不完整的

- 这批也说明，
  自动化 lane
  最好不要重新发明新脚本。
  更稳的做法是：
  - 把已经在 manual lane
    多次跑过的真实证据链
    原样提到自动 workflow
  - 再用 focused contract
    锁住它
  这样比重新设计一条“更轻”的 Windows workflow
  更不容易把 runtime proof 稀释成
  “仅文件存在”或
  “仅摘要存在”

- `WinSSL TLS 1.3`
  这次暴露的不是 docs drift，
  而是
  同一 backend
  内部两个 truth source
  已经直接分叉：
  - `GetCapabilities.SupportsTLS13`
  - `IsProtocolSupported(sslProtocolTLS13)`

- 这类问题比单文件文档漂移更危险，
  因为它会让：
  - backend selector
  - capability-based branching
  - runtime feature checks
  在同一运行环境里
  得到互相冲突的结论

- 也就是说，
  backend 审查
  不能只看
  docs/source
  是否一致，
  还要看
  source 内部
  的多个 published truth channel
  是否一致：
  - bool capability field
  - runtime probe API
  - 测试叙事

- `SupportsTLS13`
  和
  `IsProtocolSupported(sslProtocolTLS13)`
  这种组合
  尤其关键，
  因为顶层文档已经明确：
  - `SupportsTLS13`
    就是当前
    `TLS 1.3`
    的主 bool truth
  如果 runtime protocol probe
  继续用另一套门槛，
  那么调用方即使完全按文档使用，
  也会得到自相矛盾结果

- 这批也说明，
  测试文案本身
  也是开发路线的一部分。
  当
  `test_winssl_unit_comprehensive.pas`
  还在说
  `Windows 11`
  才支持
  `TLS 1.3`
  时，
  它会把后来的人继续带回旧认知，
  即使 capability docs
  和一半 source
  已经走到
  `Windows 10 1903+`

- `MbedTLS TLS 1.3`
  这次暴露的是
  dedicated backend page
  的一种更隐蔽 drift：
  - 不是把
    `False`
    写成
    `True`
  - 而是把
    “取决于 runtime/version gate 的条件能力”
    扁平化成了
    “当前 backend 无条件 `✅`”

- source 这次其实非常明确：
  - `MBEDTLS_MIN_VERSION = 3.0.0`
  - `HasTLS13 := VersionNumber >= MBEDTLS_MIN_VERSION`
  - `IsProtocolSupported(sslProtocolTLS13) := HasTLS13`
  - `SupportsTLS13 := HasTLS13`
  也就是说，
  `TLS 1.3`
  在当前 MbedTLS backend
  里并不是
  unconditional capability，
  而是
  runtime-detected capability

- 所以当 active docs
  写：
  - `MbedTLS 3.x 支持`
  时，
  如果没有继续说明
  “当前 published truth 取决于运行时检测”，
  读者就会自然把它理解成：
  - 只要你选了
    `sslMbedTLS`
  - `TLS 1.3`
    就一定是
    `✅`

- 这说明，
  对 capability 审查来说，
  不能只盯：
  - `支持`
  - `不支持`
  - `部分支持`
  这种三态字面值，
  还要盯：
  - 文档有没有把条件门槛
    展平成无条件承诺

- 也就是说，
  `runtime capability gate`
  本身就是
  published surface
  的一部分。
  一旦 active docs
  把这个 gate 抹平，
  后续路线就会被误导成：
  - “是不是 canonical matrix 太保守”
  而不是
  - “这条 backend capability
    本来就依赖 runtime truth”

- `MbedTLS`
  这次协议表说明了：
  dedicated backend page
  不只是会在“高级能力族”上漂，
  也会在最基础的 protocol table
  上继续保留旧时代认知

- source 已经明确给出：
  - `TLS 1.0 / 1.1 = False`
  - `DTLS 1.0 / 1.2 = False`
  - `MinTLSVersion = TLS 1.2`
  这种情况下，
  active docs
  就不能再写：
  - `⚠️ 可选`
  - `✅ 支持`

- 也就是说，
  “上游库理论上可能支持 / 某些编译配置可能存在”
  并不等于
  `fafafa.ssl`
  当前 runtime path
  已发布这条 capability

- 所以协议表和能力表的审查原则
  其实和前几批一样：
  - 先看当前 source/runtime path
  - 再决定 active docs
    能不能继续保留
    “可选” / “支持” / 平台表
  - 不能因为它看起来是基础协议行，
    就默认它一定没漂

- `WinSSL`
  这次 DTLS 行暴露的是
  dedicated backend page
  的另一种典型 drift：
  - 不是“平台潜力 vs 当前 capability”混写
  - 而是旧平台版本表
    直接残留在活跃专页里，
    让读者以为当前库层仍发布对应能力

- 当 source 已经给出
  `SupportsDTLS=False`
  这种全局结论时，
  active dedicated page
  就不应该继续维持：
  - 某些 Windows 版本 `✅`
  - 某些版本 `⚠️`
  这种旧平台矩阵

- 这说明，
  对 backend docs 来说，
  “按操作系统版本列平台潜力”
  和
  “按当前库层 published capability 列真值”
  也必须分开。
  一旦前者继续留在活跃能力表里，
  就会直接覆盖掉 source 已经明确给出的
  `False/None`
  capability truth

- 所以高风险专页审查
  不只是盯：
  - `⚠️ 部分`
  - `需手动实现`
  也要盯：
  - 看起来很完整的旧平台兼容表
  因为它们同样可能已经脱离当前 shipped surface

- `MbedTLS`
  这次暴露的是一种
  比“写错支持状态”更容易误导路线的表述：
  - 文档把
    “调用方可在库外自己实现某种 workflow”
    和
    “当前 backend 已经发布对应 capability”
    混成了一个
    `⚠️ 部分`
    结论

- 对 backend matrix 来说，
  `需手动实现`
  这种说法必须非常谨慎。
  如果当前 source / tests / builder
  都还在表达：
  - capability = none
  - optional interface absent
  - builder fail-fast
  那么 dedicated page
  就不能再写成
  “部分支持”

- MbedTLS 这批很有代表性：
  - source 里：
    - `sslFeatOCSPStapling=False`
    - `OCSPStaplingSupport=sslSupportNone`
    - `KnownIssues` 继续说
      `OCSP stapling` 当前不支持
  - tests 里：
    - `ISSLServerOCSPStaplingContext`
      不暴露
    - unsupported backend
      不应暴露
      `ISSLOCSPStapling`
  - 但 dedicated page
    却还留着
    `OCSP | ⚠️ 部分 | 需手动实现`

- 这说明，
  当某条能力压根没有 shipped public surface 时，
  正确的文档写法应该是：
  - 先明确
    `当前 capability 不发布`
  - 再补一句：
    如果你要做类似 workflow，
    只能在
    `fafafa.ssl`
    已发布 surface 之外，
    由应用层自己实现

- 也就是说，
  “应用层能不能自己做”
  是 integration possibility，
  不是 backend capability classification。
  这两者一旦混写，
  很容易把后续实现路线带偏成
  “是不是少接了一层 glue”
  而不是
  “当前根本没发布这条 surface”

- 这批说明，
  design/reference 文档
  本身也可能成为
  “开发路线漂移源”，
  即使：
  - source truth 是对的
  - canonical matrix 是对的
  - active guides 也已经基本对齐

- `BACKEND_ABSTRACTION_LAYER_DESIGN`
  这次最危险的点
  不是某一行 capability 写错，
  而是它继续把
  `FreePascal`
  放在
  `Future`
  位置，
  这会直接把读者的架构心智
  拉回旧阶段

- `BACKEND_SELECTOR_DESIGN`
  暴露的是另一类更隐蔽的 drift：
  - 文档在描述一个
    “看起来合理”
    但当前 source 里并不存在的
    public API 家族
  - 例如：
    - `TBackendSelector`
    - `TBackendSelectionResult`
    - `WithAutoBackend`
    - `WithPreferredBackend`
    - `WithFallbackBackend`
    - `WithAllowPartialMatch`
    - dedicated selector env vars

- 这类 drift
  比普通文案错误更伤路线，
  因为它不只是说错现状，
  还会让后续开发/补测/补文档
  围绕一个并不存在的接口层
  继续做决策

- selector 这一支现在更稳的文档策略是：
  - design doc 只说明：
    - 抽象关系
    - 真实入口
    - capability 映射原则
  - backend 细粒度能力真值
    统一下钻到
    canonical matrix
    和 dedicated backend pages
  - 不再在 design doc
    里复制一张容易过期的大表

- 这批也再次证明，
  focused contract 不应该只盯“哪些句子必须出现”，
  还要盯
  “哪些旧草案 surface 必须消失”。
  对设计文档尤其如此，
  因为旧 API 名字
  一旦继续留在活跃文档里，
  就会被误读成
  “还没实现完”
  而不是
  “当前根本没发布”

- `WinSSL`
  这次虽然没有新实现变更，
  但 design doc 里仍需要显式保留一条原则：
  - 平台潜力
    不等于
    当前已发布 capability
  - 所以
    `OCSPStaplingSupport=sslSupportNone`
    /
    `EarlyDataSupport=sslSupportNone`
    这类真相
    不能在 selector / abstraction 设计层
    被“系统可能支持”
    这种表述冲淡

- 这次补出来的不是新的实现问题，
  而是一类“验证空洞”：
  - server-side optional surface
    的 active docs
    之前并没有一条统一 focused contract
    去同时盯：
    - `API_REFERENCE`
    - top-level matrix
    - dedicated backend pages
    - `EARLY_DATA_GUIDE`
    - `OCSP_USAGE_GUIDE`

- 这意味着即使 source / runtime truth 现在看起来一致，
  后面也很容易再次发生：
  - 某个 dedicated backend page 漂了
  - 某个 guide 沿用旧的 capability 叙事
  - 但因为没有一条 cross-file focused contract，
    漂移要等到人工复查时才会再次出现

- 这批之后我们多了一条更适合当前阶段的审查策略：
  - 对已经连续出现过 doc drift 的能力族，
    不要只修当前那一个文件
  - 要补一条能跨：
    - main reference
    - matrix
    - dedicated backend pages
    - active guides
    的 focused contract
  - 这样后续再漂时，
    可以第一时间知道是“某个入口又单独跑偏了”

- 新 contract 第一次报红也提醒了一点 workflow 经验：
  - 有时候新批次的第一处红灯，
    不是产品 drift，
    而是 contract 自己的 quoting / pattern 写错
  - 这时候先修 contract 让它稳定跑完，
    才能知道真实审查结果到底是什么

- WinSSL 这次又验证了一条很重要的能力矩阵审查规则：
  - top-level matrix 说对了
  - source capability 也说对了
  - 不代表 dedicated backend page
    就一定还跟着对

- `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
  这次的 drift 很典型：
  - 说明列里已经承认
    `fafafa.ssl`
    不暴露
    `ISSLServerOCSPStaplingContext` /
    `ISSLEarlyDataContext`
  - 但“支持状态”那一列仍写
    `⚠️ 部分`
  - 这种写法会把“底层平台潜力”
    误读成“当前库层 shipped capability”

- 对 backend matrix 来说，
  “状态列”优先级比说明列更高：
  - 读者通常先看左边的支持状态，再决定是否继续下钻
  - 所以如果状态列写 `⚠️ 部分`，
    即使说明列补一句
    “封装层不暴露”
    也已经太晚了

- 这也进一步说明，
  我们后面做 backend 完整性审查时，
  不能只看 source 和总矩阵：
  - dedicated backend pages
    本身也是 capability truth source
  - 尤其要盯这种
    “平台潜力 vs 当前 public surface”
    是否被混成同一个状态词

- FreePascal early-data 这次暴露的不是实现没跟上，
  而是 durable-default 实现先落地后，
  active docs / focused contract 没有一起追上：
  - `TFreePascalContext`
    server 默认 ledger
    已经是
    `TFreePascalDefaultPersistentEarlyDataReplayLedger`
  - `KnownIssues`
    runtime truth
    也已经是
    `local persistent ... fail-closed`
  - 但活跃文档里仍有地方继续教学
    `in-memory single-process anti-replay ledger`

- 这说明“旧 focused contract 仍然是绿的 / 还没被看见”
  不等于它锁住的是当前真相。
  这次旧 contract 本身已经变成 drift source：
  - 它不只是没覆盖新真相
  - 而是在主动要求 README 保留 retired wording

- 对这种“实现先变，docs/contract 迟到”的批次，
  最值钱的不是继续补实现证据，
  而是把四层 truth 一起重连：
  - source constructor truth
  - runtime capability truth
  - active docs truth
  - focused contract truth

- `API_REFERENCE.md` 这次还暴露了另一种更隐蔽的活跃文档问题：
  - 同一节前面已经写
    默认 shipped path
    改成了持久化 replay-store
  - 结尾却又写
    “不代表默认路径已经改成持久化”
  - 这种“同页自相矛盾”比单纯漏写更危险，
    因为它会让调用方以为 durable-default 仍然只是 opt-in 讨论

- `INTEGRATION_GUIDE` / `security-best-practices`
  这次也说明：
  - 活跃 guide
    不只是“能跑的示例集合”
  - 它们本身也在对外定义 capability 心智模型
  - 所以当 capability 剩余 caveat 改了，
    这些 guide 里的一句旧边界描述
    也算真实接口设计漂移，
    不是可忽略的文案尾巴

- `src/fafafa.ssl.pas` 这次暴露的是一种很容易被“源码里明明 `uses fafafa.ssl.base` 了”
  这种错觉掩盖掉的门面缺口：
  - 活跃文档把 `uses fafafa.ssl;`
    当成主入口
  - 主单元头部注释也写着“导出所有公共接口和类型”
  - 但真正的 alias 集却没有把一整组 live optional owner surfaces
    和 supporting types 挂出来

- 这说明“主门面已经 uses 了 base/factory/tls 单元”
  不等于外部调用方就能直接看到那些符号。
  对 Pascal 门面而言，
  真正决定 public surface 的仍然是：
  - interface section 里的显式 alias / declaration
  - 而不是门面内部自己依赖了哪些 unit

- 这批里最容易漏掉的不是接口本身，
  而是 supporting types：
  - `TSSLHealthStatus`
  - `TSSLPerformanceMetrics`
  - `TSSLDiagnosticInfo`
  - `TSSLCertificateArray`
  如果只补
  `ISSLDiagnostics` /
  `ISSLCertificateVerification`
  这些 interface 名字，
  facade 仍然会停在“表面可见，实际不完整”的状态

- 这也补出了一条更稳的 façade contract 写法：
  - 对“主入口是否完整”这类问题，
    最好的 focused compile proof
    不是去碰 runtime 字段或具体行为
  - 而是只验证：
    `uses fafafa.ssl;`
    能否独立解析这批 alias/type
  - 这样既能精准钉 public completeness，
    又不会把 product truth 和测试代码自己的实现细节混在一起

- `API_REFERENCE.md` 这次暴露的是另一类比“错误文案”更隐蔽的 completeness gap：
  - 活跃 guide / `API_DOCUMENTATION.md` 已经在教学
    `ISSLConnectionInfo` / `ISSLDiagnostics` /
    `ISSLSessionResumption` / `ISSLCertificateVerification` /
    `ISSLOCSPStapling`
  - source / facade 也已经公开了
    `ISSLHttpHooksAccess` /
    `ISSLServerOCSPStaplingContext` /
    `ISSLEarlyDataContext` /
    `ISSLEarlyDataConnection`
  - 但 canonical `API_REFERENCE.md`
    却主要只把 `ISSLNativeHandleAccess`
    显式列出来

- 这种缺口的危险不在于“用户完全看不到功能”，
  而在于：
  - 用户会从主参考拿到一张不完整的 API 地图
  - 然后被迫去 secondary docs 或源码里猜
    哪些 interface 是真实 shipped surface，
    哪些只是设计讨论

- 对 server-side 叙事尤其如此：
  - `ARCHITECTURE` / `INTERFACE_DESIGN_V2`
    已经说明当前没有
    `ISSLServerConnection`
  - 但如果 canonical API reference
    不同时写出
    `ISSLServerOCSPStaplingContext`
    和
    `ISSLEarlyDataContext`
    这类 server-side context surfaces，
    读者还是很难形成“当前 server 能力到底挂在哪”的完整心智

- 所以这批最重要的不是“再补几段接口代码块”本身，
  而是把三层真相重新对齐：
  - source / facade 已导出的 optional interfaces
  - active guides 已在教学的 owner surfaces
  - canonical `API_REFERENCE.md`
    的 shipped API 地图

- `GetPeerCertificateChain` 之前正好卡在一个比
  `GetVerifyResult*`
  更容易让人误判的半收口状态：
  - `ISSLCertificateVerification`
    早就已经暴露了这条 surface
  - `TBaseSSLConnection`
    里的 residual note
    也已经把 ordinary docs/tests 判断成 owner-path
  - 但 `ISSLConnection.GetPeerCertificateChain`
    自己仍是普通 public declaration，
    所以编译器、API 参考、以及一部分普通示例
    还在继续把它当默认入口

- 这批最值钱的修法不是单纯再补一句文档，
  而是把两类入口彻底分开：
  - 普通文档 / 普通示例：
    统一切到
    `ISSLCertificateVerification.GetPeerCertificateChain`
  - backend/runtime/contract residual proof：
    允许继续保留 direct-core mirror，
    但显式标成 intentional deprecated compatibility usage

- 这也说明一个更一般化的审查规则：
  - 如果 owner-path 已存在，
    但普通 docs/example 仍继续直接教学 core getter，
    那么“source comment 已说明 owner”
    还不够
  - 真正会改变使用心智的，
    是：
    - ordinary guidance 切换
    - compiler-deprecated 声明
    - residual allowlist 冻结
    三者一起收口

- Windows 静态 residual 文件这次也再次说明：
  - 当当前 host 不能直接编译某个 backend lane 时，
    最稳的静态收口方式不是“假设本地 warning 会被局部 `PUSH/POP` 覆盖”，
    而是直接在 intentional residual 文件顶层留下
    file-scoped deprecation quarantine
  - 这样静态 contract
    就能在 Linux host 上明确冻结这层意图

- 这批还补了一条小 workflow 经验：
  - `rg` 精确匹配 markdown 列表项时，
    如果 pattern 以 `- ` 开头，
    记得加 `--`
  - 否则 contract 自己会先因为参数解析失败而报假红

- OCSP 这组 core surface 之前停在一个“半收口”状态：
  - active docs 已经转向 `ISSLOCSPStapling`
  - source comments 也已经把 core `GetOCSP*` 标成 compatibility-core mirrors
  - residual direct-core tests 也已经缩到 4 个 intentional backend/runtime proofs
  - 但 public declaration 本身还没有进入 compiler-deprecated

- 这意味着之前的真相其实还缺最后一层：
  - 文档说“仅兼容保留”
  - 注释说“owner 是 `ISSLOCSPStapling`”
  - 但源码声明本身还像普通 core shipped API
  - 这种 source/declaration 级别的松动，会继续把 `ISSLConnection` 误读成合理的 OCSP 首选入口

- 这批之后，OCSP 这组 surface 才真正进入和 session / diagnostics / connection-info 相同的路线：
  - `GetOCSPStaplingEnabled`
  - `GetOCSPResponse`
  - `IsOCSPResponseVerified`
  - `GetOCSPResponseStatus`
  都已经成为 compiler-deprecated compatibility-core mirrors
  - owner path 统一明确为 `ISSLOCSPStapling`

- 这里还有一个 workflow 经验也值得记下来：
  - 当一条老 contract 锁的是“兼容保留”旧文案，
    而我们把它推进到“compiler-deprecated compatibility mirror”时，
    最容易卡住的不是源码，而是旧 contract 自己成了旧真相
  - 这次 `test_isslocspstapling_residual_classification_contract.sh`
    就属于这种情况
  - 以后看到“新 contract 绿、旧 contract 红”的场景，要先判断是不是旧 contract 本身需要升级，而不是急着回滚源码

- 另一个小经验是：
  - Pascal 的 `{$PUSH}` / `{$POP}` 是编译期指令，不是运行时分支语义
  - 不能把 `{$POP}` 放进 `if ... Exit` 分支里指望只走一条路径
  - 这次 OpenSSL regression test 的第一次编译失败
    `A POP without a preceding PUSH`
    就是这个典型坑

- 从主线路线图看，这批的意义不只是“又补了一组 deprecation”：
  - 它证明 `ISSLConnection` slimming 已经不再只是 freeze / docs clarification
  - 我们现在已经在对 core 表面做真实、可验证、可渐进的 source-level 收窄
  - 下一批可以继续按这个模式挑剩余 core residual，
    而不是重新回到大而散的 interface 讨论

- capability 双真相这条线现在可以再向前收一层：
  - 之前我们已经收掉了：
    - selector / builder 的 support-level 消费
    - serializer / deserializer precedence
    - diff support-level truth
    - backend 返回前统一 `NormalizeLegacyCapabilityBooleans(Result);`
  - 但 live backend source 里仍保留
    “先手工写 legacy bool，再写 `*Support`，最后再 normalize”的 producer 形态
  - 这会继续把 legacy bool 暗示成主发布口，而不只是 compatibility projection

- 新增的 focused shell contract 先红后绿，把这个 residual 直接钉成了真实 source 问题：
  - 初始失败点就是：
    - `src/fafafa.ssl.openssl.backed.pas`
      仍有 `Result.SupportsSNI := LSNIReady;`
  - 说明这条线不是“我们已经在逻辑上解决了，只剩文档描述没同步”，
    而是 producer source 形态本身确实还没收口

- 这批之后，五个 live backend 的 paired capability producer
  已经统一回到同一个规则：
  - backend 只发布：
    - `SNISupport`
    - `ALPNSupport`
    - `OCSPStaplingSupport`
    - `CertTransparencySupport`
    - `SessionTicketsSupport`
  - compatibility bool：
    - `SupportsSNI`
    - `SupportsALPN`
    - `SupportsOCSPStapling`
    - `SupportsCertificateTransparency`
    - `SupportsSessionTickets`
    只再由 shared
    `NormalizeLegacyCapabilityBooleans(...)`
    投影

- 这一点很重要，因为它把“support-level 为 runtime/source truth”
  从：
  - 文档规则
  - serializer/diff 规则
  - focused runtime contract
  进一步推进成了 live backend producer 的真实源码形态

- cross-backend runtime contract 继续全绿也说明：
  - 这批没有引入新的 capability 行为回归
  - `OpenSSL / WolfSSL / MbedTLS / FreePascal Native`
    仍保持：
    - support-level truth 存在
    - legacy bool 与 support-level compatibility projection 一致
  - `Windows Schannel`
    在当前 Linux host 上仍只能做静态/source 审查，
    不属于本轮回归

- capability producer 这条线到这里可以认为“当前一段闭环”：
  - 若后面再回到 capability 主线，
    更值得看的不会是 paired legacy bool 归一化本身，
    而是：
    - 还有没有 support-level-only 字段缺乏一致 consumer proof
    - 或者 backend public surface / runtime semantics 自己是否还存在更大的结构债

## 2026-05-19

- `26048015976` 和 `26108902159` 的对照把一个很重要的 triage 规则钉死了：
  - 这次 macOS 红面不是“这个 runner 从来没支持过这些 OpenSSL 模块”
  - 而是：
    - 同一类 `OpenSSL 3.6.2 7 Apr 2026`
    - direct symbol 仍然可见
    - 但 `EVP/PEM/PKCS12/CMS/OCSP` 这批 wrapper/module truth 发生了回归
  - 所以后续不该再从 path/root 层面重跑旧调查，而要直接盯 batch-loader lane 自己

- 这次对照还暴露了一个比“依赖闭环”更尖锐的模式：
  - `TS/CT/Store` 这类 direct assignment 路线在 probe 里继续是绿的
  - 红掉的是 `LoadFunctions(...)` / batch-binding 路线
  - 说明最先该加固的不是文档叙事，而是 batch loader 自身的 durable diagnostics 和 binding-table 存储形态

- `LoadOpenSSLPEM(...)` 之前把模块 ready 判定绑在
  - `PEM_read_bio_X509`
  - `PEM_write_bio_X509`
  这会把“写路径 helper 缺口”误升级成整个 PEM 模块失败。
  对当前项目里大量真正使用的证书/私钥导入路径来说，更合理的 minimal ready surface
  应该先回到：
  - `PEM_read_bio_X509`
  - `PEM_read_bio_PrivateKey`

- `TOpenSSLLoader.LoadFunctions(...)` 之前缺少 durable per-call diagnostics，
  导致我们只能看到：
  - direct symbol 是真
  - wrapper/module 结果是假
  但看不到中间到底是：
  - required symbol 缺失
  - 还是 batch-binding 自己的命中数异常
  给 loader 增加最近一次 `loaded_count + missing_required_bindings`
  是后续避免反复拉起这条线的关键记录层。

- 本机 focused probe 也帮这次批量修法补了一条很值钱的 baseline：
  - `evp = 98`
  - `pem = 60`
  - `pkcs12 = 37`
  - `cms = 86`
  - `ocsp = 67`
  - 且 `missing_required_bindings = ""`
  以后如果 GitHub macOS lane 再掉，只要比对这几个 count 和 missing list，
  就能更快判断是：
  - symbol/export 真的少了
  - required gating 过严
  - 还是 batch-binding 命中数整体异常

- `PERFORMANCE_GUIDE` / `PERFORMANCE_OPTIMIZATION_GUIDE` 这类历史型性能文档的 drift，
  其实不只是“数字过期”这么简单：
  - 一层是固定 benchmark / phase / threshold snapshot 被写成 current truth
  - 另一层是它们还会反向把旧的 direct-core session/diagnostics mirror 重新教给新读者
  - 所以这批最值钱的修法不是单纯删数字，而是同时把 benchmark 真相源和 owner-path
    设计一起重新钉牢

- 当前更 durable 的性能 truth 已经很清楚：
  - `scripts/run_phase2_performance_baseline.sh`
  - `tests/benchmarks/run_all_benchmarks.sh`
  - `tests/benchmarks/baselines/*.json`
  - `docs/test_reports/PHASE2_PERFORMANCE_METRICS_TEMPLATE.md`
  - 也就是说，文档应该教“去哪里跑、怎么比较、如何记录环境”，而不是继续背诵某次
    `ops/s` / `ms` / `P99` / `倍率`

- `benchmark_aesgcm_pool` 这类专项 benchmark 需要和默认 Phase 2 baseline lane 分开：
  - 它可以继续作为辅助/手工 lane 存在
  - 但不应该再被写成默认 shipped baseline 的一部分
  - 这个边界如果不写清楚，后面就很容易把单点实验结果误升格成项目主结论

- 性能文档现在也必须和接口设计真相保持一致：
  - `ISSLConnection.GetSession` / `SetSession` / `IsSessionReused`
    在 active guidance 里不该再当主入口
  - `ISSLConnection.GetPerformanceMetrics`
    也不该继续当默认性能采样入口
  - 正确 owner path 是：
    - `ISSLSessionResumption`
    - `ISSLDiagnostics`

- 这批还暴露了一个 workflow 级别的小经验：
  - 对 markdown 正文做 contract 时，整句 literal 匹配很容易被 prettier 自动换行误伤
  - 这类规则更稳的写法是：
    - 关键短语 fragments
    - 或跨换行的语义匹配
  - 记住这一点可以减少之后重复返工 contract 本身

- 文档 owner-path drift 现在已经很明确地分成了两类：
  - 一类是“解释性提及” core mirror 名字，但同时明确说它是 deprecated/compatibility
  - 另一类是“教学示例真的在调用 direct-core”
  - 真正需要优先收口的是第二类，因为它会直接把读者带回旧接口使用方式

- 这次第 97 批确认了一个很重要的事实：
  - `API_REFERENCE` 里最危险的不是大段代码示例
  - 反而是 record 小节前的说明 bullet
  - 因为读者会把“通过 ISSLConnection.GetHealthStatus 获取...”这种描述当成权威入口，
    即使下面已经写了 deprecated/owner-path 说明，也还是会形成自相矛盾心智

- `WINSSL_BEST_PRACTICES` / `PERFORMANCE_PROFILING_GUIDE` / `WINSSL_DESIGN`
  这三页说明了另一个模式：
  - 一旦某条旧示例进入“最佳实践 / profiling / design pseudocode”层，
  - 它的伤害往往比普通 FAQ 更大，
  - 因为读者更容易把它当成“推荐实现模式”而不是“历史遗留 API”

- 当前活跃文档 owner-path 面现在已经基本清空：
  - 我们重新扫描 `docs/guides` / `docs/reference`
  - 已经没有 direct-core `GetSession` / `SetSession` / `IsSessionReused` /
    `GetHealthStatus` / `GetPerformanceMetrics` / `GetDiagnosticInfo` 调用示例残留
  - 剩下的 direct-core 名字主要是解释性提及或 compatibility/deprecation 声明，
    不再属于 active guidance drift

- `P2_MINIMUM_API_CAPABILITY_MATRIX.md` 这次暴露的是另一类更隐蔽的 capability drift：
  - 不是表格行本身错
  - 而是顶部“先看结论”还保留旧判断
  - 结果就会出现“摘要说 CT 有直接字段映射，但 CT 行和特别说明又说没有”的
    自相矛盾

- 这种“摘要层 truth 漂移”风险很高：
  - 很多人只看顶部 bullet，不会继续读到表格细节和特别说明
  - 所以 capability 文档里，顶部 summary 反而要比具体行更保守
  - 后续扫 capability matrix / KnownIssues 时，也要优先盯 summary / 结论段

- `BACKEND_CAPABILITY_MATRIX.md` 这次又证明了一个更具体的模式：
  - 顶层 quick reference 不只是会和细节段落打架
  - 它甚至可能直接违背它自己刚写下来的 precedence 规则
  - 这次最典型的 3 处就是：
    - `WinSSL TLS 1.3`
      - source 明确受 Windows / Schannel 版本门控
      - quick reference 却先写成了无条件 `✅`
    - `WinSSL PSK`
      - WinSSL 专项矩阵已明确 `❌ 不支持`
      - quick reference 却先写成了 `⚠️`
    - `FreePascal ALPN / SNI`
      - 顶层文档自己声明这两行按 `*Support` 字段解读
      - source 当前却仍是 `sslSupportExperimental`
      - quick reference 却先写成了稳定 `✅`

- 这说明 capability 审查里最危险的不一定是“大段说明文字”，
  反而可能是首页那几个看起来最省事的单元格：
  - 因为读者最容易只扫一眼 quick reference 就形成心智模型
  - 一旦 quick reference 比 source/backend truth 更激进，后面再多详细解释都很难补救

- `BACKEND_CAPABILITY_MATRIX.md` 的后半段又暴露了同一种风险的另一个变体：
  - quick reference 收紧之后，根入口仍可能在“性能对比”和“选择建议”里继续偷跑
  - 也就是：
    - 用固定 benchmark 相对值表假装当前长期 truth
    - 用 blanket recommendation 掩盖 backend-specific caveat

- 这里最危险的不是“数字旧了”本身，而是它会直接改写产品路线判断：
  - 读者会因为一张 `1.2x / 0.8x` 表，就误以为 backend 性能排序是稳定常量
  - 也会因为“Windows 应用推荐 WinSSL”一句话，就忽略：
    - Early Data
    - caller-provided server OCSP stapling
    - session resumption / tickets runtime truth
    - custom cipher / PKCS#11 / 完整 PKCS#12 helper
    这些真正会改变技术选型的边界

- 所以 capability/doc 审查到这个阶段，顶层入口的修法应该统一成一个原则：
  - 根入口只负责给出当前可验证口径和分流路径
  - 不能继续自己携带“固定性能榜单”或“无条件优胜者”叙事
  - 真正的性能 truth 要回到 benchmark runner / baseline / fresh output
  - 真正的选型建议要回到 capability-aware recommendation

- `BACKEND_CAPABILITY_MATRIX.md` 底部的 `版本历史` 也属于同一种 summary drift：
  - 它表面上只是历史说明
  - 但如果根入口直接从 `v1.4.1` / `v1.4.0` 开始列，就会把读者带回
    “这页自己已经覆盖当前 release truth”的错觉

- 这再次说明一个路线图级原则：
  - capability milestone
  - release truth
  - runtime truth
  这三层不能混在同一段口径里
  - 否则人会把“某版本曾经加过什么功能”
    误判成“当前稳定版已经完整验证并正式发布了什么”

- 根入口最稳的写法应该是：
  - 先指向当前 stable/release-control 权威入口
  - 再把旧里程碑降级成历史附录
  - 这样 capability matrix 才不会变成一张混合了
    当前路线图 + 历史公告 + 旧 milestone 的误导页

- WinSSL session 这条线暴露的是另一种更“像实现问题”的语义漂移：
  - public interface 上有 `ISSLSessionResumption.SetSession(...)`
  - 高入口示例也在调用它
  - 但 shared client handshake 路径里并没有把 caller-supplied session
    当成 native handle 注入 `InitializeSecurityContextW`
  - 当前 reconnect 仍主要依赖 Schannel automatic cache key：
    - `target name`
    - `credential handle`

- 这意味着这里最需要防的误读不是“接口不存在”，而是：
  - 调用方会把 `SetSession(...)` 误读成 OpenSSL 风格的显式 session restore
  - 然后把“接口存在”误判成“调用方可控的 resumed-handshake 语义已完整”

- 这也帮助我们把“实现完整度”路线再分细一层：
  - 一层是 public surface 是否存在
  - 一层是 public surface 的 runtime semantics 是否真的和调用方心智匹配
  - WinSSL 当前在 session 这条线上更接近：
    - public surface 已存在
    - 但 caller-driven restore semantics 仍未达到 OpenSSL 式直觉语义

- 所以当前最稳的修法不是先假装把实现说圆，
  而是先把 source/high-entry docs 的 semantic boundary 钉牢：
  - `SetSession(...)` = compatibility metadata surface
  - real reconnect truth = Schannel automatic cache key + fresh runtime transcript

- session-resumption residual 这条线现在终于可以稳定冻结了：
  - `tests/contract/test_backend_contract.pas`
    是 compatibility mirror proof
  - `tests/test_mbedtls_connection_session_reused_contract.pas`
  - `tests/test_openssl_connection_session_reused_contract.pas`
    是 backend semantic truth proof
  - 这 3 份继续保留 direct-core，不再属于 ordinary owner-path drift

- `tests/winssl/test_session_save_logic.pas`
  之前之所以总被 residual 扫描带出来，并不是 public interface truth 真的有问题，
  而是 mock helper 自己用了一个看起来像 public surface 的 `GetSession` 命名。
  把它改成 `GetSavedSession` 之后，噪音 residual 就能真正从扫描结果里消失。

- `src/fafafa.ssl.connection.base.pas`
  的 residual note 也需要跟着说真话：
  - 之前写成 `backend-specific runtime residuals`
    会继续暗示这批文件像是“还没迁完”
  - 但当前更准确的 truth 是：
    `backend-specific semantic truth proofs`
  - 这类措辞收紧对后续防止重复审查很重要，因为它决定大家会不会继续把 intentional proof 当成 drift

- `tests/test_freepascal_tls13_early_data.pas`
  这块最大的 ordinary runtime residual 证明了一个更稳的迁移策略：
  - 对超大测试文件，最小正确修法不是到处散落 `Supports(...)` 局部变量
  - 而是在 helper 区收一个统一入口：
    - `RequireSessionResumption(...)`
    - `AssertSessionReused(...)`
  - 这样既能把 direct-core `GetSession` / `SetSession` / `IsSessionReused`
    一次性迁走，也不会让文件继续长出一堆重复样板

- 这批 focused RED -> GREEN 也再次证明：
  - 红灯一开始确实来自 ordinary runtime file 里还活着的 direct-core session mirrors
  - 真正迁完后，大测试重新编译运行仍然全绿
  - 所以这次收掉的是 runtime owner-path truth 漂移，不是行为逻辑调整

- session-resumption runtime residual 现在已经进一步压缩到更清晰的一层：
  - `tests/contract/test_backend_contract.pas`
    是 intentional compatibility mirror proof
  - `tests/test_mbedtls_connection_session_reused_contract.pas`
  - `tests/test_openssl_connection_session_reused_contract.pas`
    更像 backend semantic truth proof，需要单独决定是否保留 direct-core
  - `tests/winssl/test_session_save_logic.pas`
    更像 mock/save helper residual
  - 这意味着 ordinary runtime lane 基本已经收口，不应该再把
    `tests/test_freepascal_tls13_early_data.pas`
    反复拉起

- session-resumption 这条线继续往下压后，又暴露了一个更值钱的事实：
  - 不只是 ordinary runtime tests 在直接走 core mirror
  - `src/fafafa.ssl.connection.builder.pas`
  - `src/fafafa.ssl.tls.pas`
    这两条普通生产路径也还在直接调用 `ISSLConnection.SetSession`
  - 所以上一批 compiler `deprecated` 收口不是终点，它实际上帮我们把生产代码里
    还活着的旧调用面也抖了出来

## 2026-05-20

- helper surface 这次又暴露了一个不同于“接口不存在/文档写错”的模式：
  - exported API 继续 shipped 并不等于它还是当前主入口
  - 如果权威 API 文档不主动分级，
    调用方就会把 `仍导出` 误解成 `仍推荐`

- 对 `fafafa.ssl` 当前最该分开的 helper 至少有三层：
  - TLS bootstrap main entry
    - `TSSLFactory.GetLibraryInstance(...)`
    - `TSSLConnector` / `TSSLAcceptor` / `TSSLStream`
  - facade convenience helpers
    - `CreateDefaultConfig`
    - `TSSLHelper`
    - `QuickServer`
    - `CreateOCSPClient`
    - `CreateCRLManager`
  - backend-specific / legacy convenience wrappers
    - 例如 WinSSL enterprise 的
      `IsFIPSModeEnabled(...)`
      `GetEnterpriseTrustedRoots(...)`

- 这次 `WinSSL 企业工具` 小节说明了一条很重要的文档原则：
  - 即使旧全局函数还活着，
  - canonical docs 也不该再把它们摆在主入口代码块里，
  - 否则它们就会重新压过真正的当前 helper 主路径
    `TSSLEnterpriseConfig.IsFIPSEnabled / GetTrustedRoots / GetAllPolicies`

- 所以对这类长期兼容项目，最稳的写法不是“把旧 surface 从文档里抹掉”，
  而是：
  - 保留 shipped 事实
  - 但显式降级成 convenience / compatibility wrapper
  - 同时把主入口路径钉到 canonical guide/reference 里

- `INTEGRATION_GUIDE` 这次证明了一个很典型的文档/工作流问题：
  - 漂移不一定来自“同一页内部有错”
  - 也可能来自“同一个主题有两份 active 文件，同时被不同 contract 当真”
  - 这种双路径真相比单页措辞错误更危险，因为它会让不同批次各自修自己的那一份，
    最后越修越分叉

- 对当前 repo 来说，`docs/INTEGRATION_GUIDE.md` 才是权威入口：
  - `docs/README.md`
  - `docs/DOCUMENTATION_INDEX.md`
  - 多条 owner-path active-guidance contract
  都已经把根目录版本当作 canonical source
  - `docs/guides/INTEGRATION_GUIDE.md` 更像后来长出的漂移副本

- 所以这次最值钱的修法不是“把两份都修一遍”，而是：
  - 删除 shadow copy
  - 把 active contracts 统一收回到 root canonical path
  - 顺手把 canonical guide 的 import truth 也收回 `uses fafafa.ssl;`
  这样后续再修 connection-info / cert-verify / session-resumption guidance 时，
  就不会再出现“只修到另一份文档”的假收口

- 这条经验也直接反推到整体路线图：
  - 对 `fafafa.ssl` 这类接口/文档并行演进项目，
  - “唯一权威路径”本身就是 correctness 要件，不是文档整理偏好
  - 以后只要出现同主题双路径 active docs，就应优先把它当 correctness bug 收口

- `26110676557` 的最终结果把这条 macOS 线彻底钉死了：
  - 不是“本地修了，CI 未必真好”
  - 而是 GitHub 三平台 gate 已经一起全绿：
    - `linux-gate = success`
    - `macos-gate = success`
    - `windows-gate = success`
    - `summary = success`
  - 所以这次 batch 可以作为真正 closeout，而不是半收口

- macOS artifact 也证明这次修法命中的就是回归本体，而不是偶然绕过：
  - same `OpenSSL 3.6.2 7 Apr 2026`
  - `direct_symbols` 全 true
  - `evp/pem/pkcs12/cms/ocsp` 又恢复全绿
  - 且 CI probe 现在能稳定给出 `load_functions_loaded_count +
    missing_required_bindings`，后续再掉线时不必回到旧日志猜谜

- 这次 GitHub run 还顺带给出一条重要的 workflow 结论：
  - 不该再把 macOS batch-loader 回归和旧 WinSSL/native-probe 线搅在一起
  - 因为 `windows-gate` 在同一 run 里也已经成功
  - 后续平台问题应继续按 artifact/contract 各自收口，不要再跨线串案

- 当前“接口设计 + 各 backend 实现一致性”总 goal 的下一条高价值工作，
  不再是平台 runtime triage，而是继续处理 active public surface debt：
  - `TSSLConfig` mixed-scope public record
  - facade/quick-entry 推荐入口与 compatibility 路径分层
  也就是优先防止调用方继续被活跃入口误导，而不是重复验证已收口的平台门禁

- 当前 durable truth 已进一步收口为：
  - ordinary runtime tests 现在优先通过 `ISSLSessionResumption`
  - `TSSLConnectionBuilder.WithSession(...)` 与 `TSSLConnector.WithSession(...)`
    的真实应用路径，也都已改成 owner path
  - `tests/test_tls_connector_early_data_contract.pas`
    重新证明了 connector 仍保持：
    - `session -> servername -> earlydata -> connect`
    的应用顺序

- 最新 residual snapshot 现在已经足够小，可以作为下一阶段路线图：
  - 真正还留着 direct-core session mirrors 的主要是：
    - `tests/contract/test_backend_contract.pas`
    - `tests/test_freepascal_tls13_early_data.pas`
    - `tests/test_mbedtls_connection_session_reused_contract.pas`
    - `tests/test_openssl_connection_session_reused_contract.pas`
    - `tests/winssl/test_winssl_session_resumption.pas`
  - `tests/winssl/test_session_save_logic.pas`
    里虽然还有 `GetSession` 名字，但它是 mock/save-logic helper，不是公共接口
    owner-path 漂移的主战场

- 这也把下一步工作变得更清楚了：
  - `tests/test_freepascal_tls13_early_data.pas`
    是剩余 ordinary runtime owner-path migration 的最大块
  - `mbedtls/openssl` 这两份 `session_reused_contract`
    更像 backend semantic truth proof，是否保留 direct-core mirror 需要单独判定
  - `WinSSL` 那条则更接近 runtime proof residual，不应和普通文档/示例路径混在一起

- session-resumption 这组方法此前也存在和 diagnostics 很像的中间态：
  - active docs/tests 已经默认走 `ISSLSessionResumption`
  - 但 `ISSLConnection.GetSession` / `SetSession` / `IsSessionReused`
    还没有进入 compiler-level `deprecated`
  - 这会让核心 `ISSLConnection` 在 source/doc 上继续像主入口，而不是 compatibility mirror

- 当前 durable truth 已经收口为：
  - `GetSession` / `SetSession` / `IsSessionReused`
    在核心 `ISSLConnection` 上现在统一只是 compatibility mirror
  - 这 3 个方法的源码声明已经进入编译期 `deprecated`
  - ordinary docs/tests 默认应走 `ISSLSessionResumption` owner path
  - `tests/contract/test_backend_contract.pas`
    继续保留 direct-core session mirror proof

- 这批 focused proof 也把“现在还剩什么”分得更清楚了：
  - compiler-surface truth 缺口已经收掉
  - 但 runtime/semantic 测试中仍有一批 direct-core session 调用残留
  - 这些残留主要分布在：
    - `tests/test_freepascal_client_session_resumption.pas`
    - `tests/test_freepascal_server_session_resumption.pas`
    - `tests/test_freepascal_tls13_early_data.pas`
    - 以及少数 builder / mock / backend-specific semantic contracts
  - 所以下一条高价值路线不再是继续补 `deprecated`，而是：
    - 把 ordinary runtime tests 进一步迁到 `ISSLSessionResumption`
    - 把真正需要验证 compatibility-core mirror 的测试明确收窄成 residual set

- focused backend contract 重新编译后，`Session-resumption interface alignment`
  在 `OpenSSL` / `WolfSSL` / `MbedTLS` / `FreePascal` 依旧保持全绿。
  这说明我们这批修的是 compiler-surface/design truth，不是引入 runtime 回归。

- diagnostics 这组方法此前确实还卡在一个容易误导新代码的中间态：
  - `ISSLDiagnostics` owner path 虽然已经是 active docs/tests 的默认入口
  - 但 `ISSLConnection.GetHealthStatus` / `IsHealthy` / `GetDiagnosticInfo` /
    `GetPerformanceMetrics` 还没有进入 compiler-level `deprecated`
  - 这会让 core surface 在 source/doc 上继续看起来像主入口，而不是 compatibility mirror

- 当前 durable truth 已经收口为：
  - `GetHealthStatus` / `IsHealthy` / `GetDiagnosticInfo` / `GetPerformanceMetrics`
    在核心 `ISSLConnection` 上现在统一只是 compatibility mirror
  - 这 4 个 getter 的源码声明已经进入编译期 `deprecated`
  - ordinary docs/tests 默认应走 `ISSLDiagnostics` owner path
  - direct-core diagnostics 只保留：
    - `tests/contract/test_backend_contract.pas` 的 cross-backend mirror proof
    - `tests/winssl/test_winssl_connection_edge_cases.pas`
    - `tests/winssl/test_winssl_monitoring.pas`
      这两份 WinSSL runtime residual proofs

- `tests/winssl/test_winssl_session_resumption.pas` 之前继续用 `AConn.GetPerformanceMetrics`
  是这条 residual 集合里最不该继续活着的一处：
  - 它不是 contract mirror proof
  - 也不是 WinSSL diagnostics runtime residual 的专门验证
  - 所以这次正确的最小修法就是把它切回 `ISSLDiagnostics` owner path

- 这批 focused proof 也说明 diagnostics 这条线当前不需要再反复拉起“大审查”：
  - 新 shell contract 已锁住：
    - source/compiler `deprecated`
    - docs truth
    - residual direct-core allowlist
    - `session_resumption` 不再偷走 direct-core metrics path
  - `tests/contract/test_backend_contract.pas`
    focused 编译运行后，`OpenSSL` / `WolfSSL` / `MbedTLS` / `FreePascal`
    的 diagnostics / connection-info / session-resumption / cert-verify
    contract 都继续保持全绿
  - `Windows Schannel` 在 Linux 上依旧是 platform skip；这不再构成 blocker，
    因为该 runtime 证据链已经由 GitHub Windows CI 闭环

- 第三轮 Windows CI (`26093405878`) 证明这条 callback runtime proof 链已经真正闭环：
  - `windows-gate` / `linux-gate` / `macos-gate` / `summary` 全部 `success`
  - 成功 artifact `wave-b-windows-winssl_callback_markers_fix2_20260519_191025`
    内现在可以直接 grep 到：
    - `[WINSSL-RUNTIME] callback_surface verify=pass password=unsupported info=pass`
    - `[WINSSL-RUNTIME] suite_summary passed=8 failed=0 total=8 success_rate=100`
    - `[WINSSL-RUNTIME] suite_end status=PASS`

- 这也把整个问题链最终分成了两个已经验证完成的子结论：
  - 第一层 root cause：
    - marker 提取链之前抓错了 Windows runtime truth source
  - 第二层 follow-up root cause：
    - Windows comprehensive test 对 password callback fail-closed 文案判断过窄
  - 这两层都已经被修正并经过真实 Windows artifact 证明

- 这批之后应保留的 durable truth 是：
  - WinSSL partial callback publication 的 Windows runtime artifact 现在有稳定 marker，可直接回答：
    - verify callback 已发布
    - info callback 已发布
    - password callback 当前仍为 unsupported / not published
  - 后续再看这条链，不需要重新读源码推理，只要优先检查成功 artifact 里的：
    - `callback_surface`
    - `suite_summary`
    - `suite_end`

- 第二轮 Windows CI (`26092828923`) 把这条链又往前推了一步：
  - artifact 里已经明确出现了：
    - `[WINSSL-RUNTIME] callback_surface verify=pass password=unsupported info=pass`
  - 这说明我们上一轮修的核心目标其实已经达成：
    - callback marker 不再是 `missing/missing/missing`
    - Windows runtime transcript 现在能直接写出 callback granularity truth

- 当前剩下的失败也因此被准确缩小成了测试断言问题，而不是 library 实现问题：
  - `WinSSL Unit Tests (Comprehensive)` 失败点是：
    - `[Callback Configuration] Password callback unsupported as expected: FAIL`
  - 真实异常文案是：
    - `Password callback is not published by the current WinSSL backend runtime. The current WinSSL callback surface only publishes verify/info paths.`
  - 这条 message 本质上就是当前已发布 truth 的 fail-closed 表述：
    - password callback 未发布
    - verify/info 才是 published path
  - 但我们刚加到 `tests/winssl/test_winssl_unit_comprehensive.pas`
    的 Windows callback 测试只把包含：
    - `unsupported`
    的 message 视作成功

- 所以下一步的最小正确修法不是再碰 callback marker，也不是再改 WinSSL context：
  - 只需要把 Windows comprehensive 测试的 password callback 断言放宽到接受当前真实 fail-closed message
  - 这类 message 至少应接受：
    - `unsupported`
    - 或
    - `not published`
  - 因为后者已经明确表达了同一条 public truth：
    - password callback 不是当前已发布的 WinSSL runtime path

- 继续往 WinSSL runtime completeness 收口时，先用 GitHub Windows artifact 把 callback proof gap 反证实了：
  - workflow `26092105397`
    的 `winssl_runtime_suite_winssl_callback_markers_20260519_184245.log`
    已经写出：
    - `[WINSSL-RUNTIME] callback_surface verify=missing password=missing info=missing`
  - 所以问题不是 marker 没写出来，而是 marker 背后的 truth source 本来就抓错了

- 当前真正的 root cause 也因此比原先判断更具体：
  - broader suite 运行的是：
    - `test_winssl_unit_comprehensive.lpi`
  - 这份 LPI 实际对应：
    - `tests/winssl/test_winssl_unit_comprehensive.pas`
  - 但我们之前拿来当 callback truth source 的是：
    - `tests/unit/test_winssl_comprehensive.pas`
  - 也就是说，脚本不是“没把正确输出写进 artifact”，而是“从一开始就在等一份根本不会被当前 Windows suite 运行的输出”

- 这条问题仍然不是新的 library product bug，而是典型的验证链错位：
  - library callback publication / unsupported truth 本身没变
  - 出错的是 Windows runtime proof 把错误的测试源当成了证据来源
  - 如果不改这里，workflow 再绿也只会稳定产出 `missing/missing/missing`

- 当前最小正确修法因此也跟着变了：
  - 保留 `tests/run_winssl_tests.ps1`
    对 `test_winssl_unit_comprehensive.lpi` 的 marker 汇总
  - 但必须先让实际被 broader suite 运行的：
    - `tests/winssl/test_winssl_unit_comprehensive.pas`
    输出稳定 callback truth：
    - `Verify callback set`
    - `Password callback unsupported as expected`
    - `Info callback set`
  - 然后 artifact 才有可能收敛到：
    - `[WINSSL-RUNTIME] callback_surface verify=pass password=unsupported info=pass`

- 继续沿着“capability 先说支持、实现/控制面其实没发布”的线往下压后，又确认了一条更隐蔽但影响 selector/security score 的问题：
  - `src/fafafa.ssl.winssl.lib.pas`
    仍在发布：
    - `SupportsFIPSMode=True`
  - 但当前源码里与 WinSSL FIPS 最接近的实现主要是：
    - `src/fafafa.ssl.winssl.enterprise.pas`
      - `IsFIPSModeEnabled`
      - `TSSLEnterpriseConfig.IsFIPSEnabled`
  - 这条线体现的是：
    - Windows FIPS policy / 注册表 / enterprise helper 检测
    - 不是 fafafa.ssl 已发布的 backend runtime/control surface

- 这条漂移比普通文档错误更危险，因为它会直接进入公共打分和选择路径：
  - `src/fafafa.ssl.backend.selector.pas`
    会把：
    - `SupportsFIPSMode`
    用在 `PreferFIPSCompliant` 平台打分
    和推荐原因输出里
  - `src/fafafa.ssl.base.pas`
    还会把：
    - `SupportsFIPSMode`
    计入 security score
  - 如果 WinSSL 在这里只是“能检测系统 policy”，却先对外发成 `True`
  - 那 selector / score / docs 就会一起把 helper 当成 shipped capability

- 当前最小正确修法因此不是删掉 WinSSL enterprise helper，而是把边界重新说真：
  - `SupportsFIPSMode` 回到：
    - `False`
  - `fafafa.ssl.winssl.enterprise`
    继续保留：
    - FIPS policy 检测
    - enterprise roots / GPO helper
  - 活跃文档统一改成：
    - 可检测/遵循 Windows FIPS policy
    - 但这不等于 `ISSLLibrary.GetCapabilities.SupportsFIPSMode=True`

- 这批还再次证明了一条后续审查准则：
  - “平台有潜在能力 / 系统可检测某策略”
    不等于
  - “fafafa.ssl 当前 backend 已发布 capability”
  - 对 WinSSL 这种 OS-native backend，最容易出错的正是这类：
    - system-managed policy
    - 被误投影成 coarse public capability bool

- 继续从 callback/runtime drift 往下压后，又确认了一条同样属于“capability 先说支持、实现其实没接通”的问题族：
  - `SupportsCustomCipherSuites`
  - `SetCipherList(...)`
  - `SetCipherSuites(...)`

- 这条问题的关键不只是：
  - `FreePascal` 之前把 `SupportsCustomCipherSuites` 发成了 `True`
  - 而是 coarse-grained custom-cipher truth 在多个 backend 上都不一致：
  - `OpenSSL` 虽然有 runtime apply，但仍依赖：
    - `SSL_CTX_set_cipher_list`
    - `SSL_CTX_set_ciphersuites`
    helper 是否真的存在
  - `FreePascal` / `WinSSL` / `MbedTLS` / `WolfSSL`
    则都只是 storage-only / system-policy-only 路径，custom non-default override 没有真实 runtime wiring

- 如果继续保持旧语义，风险非常实际：
  - 调用方先看到：
    - `ISSLLibrary.GetCapabilities.SupportsCustomCipherSuites=True`
  - 然后在 custom override 时要么 silent ignore，要么 storage-only，要么 helper 不全时悄悄降级
  - 这等于把“能力是否真发布”变成了 runtime 猜谜

- 这批里最重要的 nuance 不是“一刀切禁掉所有 setter”，而是把：
  - shipped baseline defaults
  - caller 自定义 override
  真正分开

- 当前最小正确修法因此是：
  - `OpenSSL` 新增 shared runtime gate：
    - `OpenSSLPublishedCustomCipherSurfaceReady`
  - `OpenSSL` 只有在 TLS 1.2 / TLS 1.3 两条 helper 都齐时，才发布：
    - `SupportsCustomCipherSuites=True`
  - `FreePascal` / `WinSSL` / `MbedTLS` / `WolfSSL`
    当前统一发布：
    - `SupportsCustomCipherSuites=False`
  - 各 backend setter 统一回到：
    - custom non-default override -> fail-closed `unsupported`
    - empty clear / shipped baseline defaults -> 继续允许作为 compatibility/default-context path

- 这里保留 shipped baseline defaults 是有必要的：
  - 否则 factory / direct-library / builder 的默认安全基线会被这批修法误伤
  - 但保留 baseline defaults 不等于继续对外承诺：
    - “支持 caller 自定义 cipher override”

- focused runtime proof 也已经补上，避免以后又回到静态猜测：
  - `OpenSSL` 正常 helper 完整时：
    - `SupportsCustomCipherSuites=True`
    - custom non-default override 可用
  - 动态去掉：
    - `SSL_CTX_set_ciphersuites`
    后：
    - `SupportsCustomCipherSuites` 会回落为 `False`
    - custom non-default TLS 1.2 / TLS 1.3 override 都会 fail-closed
    - shipped baseline defaults 仍然可走 compatibility path
  - `FreePascal` / `MbedTLS` / `WolfSSL`
    当前 runtime proof 也已压实：
    - capability false
    - custom non-default override rejected
    - baseline defaults retained

- 这批收口后的新基线应明确保留：
  - `SupportsCustomCipherSuites` 不再是 coarse bool 撒谎源
  - default-context shipped baseline 与 custom override 已被区分
  - WinSSL / MbedTLS 专项文档和旧测试也不再继续误教：
    - `SetCipherList(...)`
    - `SetCipherSuites(...)`
    作为当前已发布的 backend tuning 手段
- 继续从文档收尾切回 implementation completeness 后，当前最有价值的新问题已经从“表述漂移”升级成了真正的 capability/runtime 撒谎：
  - `src/fafafa.ssl.openssl.backed.pas`
    之前无条件发布：
    - `Result.SupportsCallbacks := True;`
  - 但 `src/fafafa.ssl.openssl.context.pas`
    的 verify/password/info callback setter 仍然依赖 runtime symbol/helper 是否真的存在

- 这条漂移的关键不只是“某个符号可能缺失”，而是 coarse `SupportsCallbacks` bool 当前并没有 per-callback 粒度：
  - 调用方只能先看：
    - `ISSLLibrary.GetCapabilities.SupportsCallbacks`
  - 如果这里先说 `True`
  - 然后在 non-nil callback assignment 时再抛 `unsupported`
  - 那就等于 capability 对外先撒谎，再在 setter 层补刀

- 这批重新核对后还额外压实了一条容易漏掉的细节：
  - password callback 不只依赖：
    - `SSL_CTX_set_default_passwd_cb`
  - 还依赖：
    - `SSL_CTX_set_default_passwd_cb_userdata`
  - 否则 `PasswordCallbackThunk` 拿不到 `Self`
  - 所以“password callback helper 存在但 userdata helper 丢了”也必须视为 callback surface 不完整

- 因而当前最小正确修法不是引入新的 per-callback capability 字段，而是先把 OpenSSL 的 coarse published truth 收紧到 fail-closed：
  - 新增共享 helper：
    - `OpenSSLPublishedContextCallbackSurfaceReady`
  - `GetCapabilities.SupportsCallbacks`
    现在直接跟随这条 helper
  - `SetVerifyCallback` / `SetPasswordCallback` / `SetInfoCallback`
    对 non-nil assignment 统一先检查 published callback surface
  - callback surface 不完整时：
    - non-nil 统一 `unsupported`
    - `nil` clear 继续允许作为 compatibility clear/no-op

- focused runtime proof 也已经补上，避免以后只靠静态 grep 重新争论：
  - 动态把 `SSL_CTX_set_default_passwd_cb_userdata` 置空后：
    - `OpenSSL SupportsCallbacks` 会回落为 `False`
    - verify/password/info 三个 non-nil setter 都会 fail-closed
    - `nil` clear 仍然可用

- 这批收口后的新基线应明确保留：
  - `OpenSSL` callback publication 不再是 unconditional truth
  - coarse capability 与 setter/runtime 语义重新对齐
  - 后续如果继续做 callback / capability completeness，应优先审查：
    - 还有哪些 coarse bool 仍然没有把“required helper set”锁进 published truth
- 在前面连续收掉 specialized guides 的旧 helper 名之后，active docs 扫描结果已经明显收窄：
  - 当前只剩 `docs/guides/MIGRATION_GUIDE.md`
    里 OpenSSL low-level helper 片段还保留了一处：
    - `TSSLFactory.GetLibrary(sslOpenSSL)`

- 这条残余虽然只是单点，但性质和前几批完全一致：
  - 它会把读者重新带回旧工厂调用名
  - 而当前仓库的高入口 public library-entrypoint 已经明确统一为：
    - `TSSLFactory.GetLibraryInstance(...)`

- 这次的关键点在于：
  - 该片段是在讲：
    - `GetFriendlyErrorMessage(...)`
    - `GetOpenSSLErrorCategory(...)`
    这种 OpenSSL-specific low-level helper
  - 但即使示例处在 low-level helper 语境里，也没有必要再回流旧 `GetLibrary(...)`
  - 换句话说：
    - “backend-specific low-level helper”
      不等于
    - “可以继续使用旧工厂入口”

- 当前最小正确修法因此非常干净：
  - 不改整份 migration guide 其它段落
  - 不新开一套并行 contract
  - 只把现有 `test_migration_guide_active_truth_contract.sh` 收紧一条：
    - 该 low-level helper 片段必须使用 `GetLibraryInstance(...)`
    - 并且不再允许 `GetLibrary(...)`

- 这批收口后的新基线应明确保留：
  - `MIGRATION_GUIDE` 的高入口工厂心智现在进一步完整
  - active docs 扫描也证明我们已经开始进入：
    - “每次只剩单个示例残余”的收尾阶段
  - 后续若继续做文档完整性扫尾，应优先复查：
    - 还有哪些 backend-specific snippet 虽然大方向对了，但还藏着旧 creator / old factory 名称
- specialized guide 继续往下扫后，又确认了一条非常典型的“backend scope 虽然大致对了，但具体示例仍然复制即错”的残余：
  - `docs/guides/security-best-practices.md`
    的 certificate pinning 示例还在使用：
    - `LoadCertificateFromFile(...)`
  - 这个名字当前在源码里并不存在

- 当前这段示例真正走的是 OpenSSL raw certificate handle 路径：
  - `src/fafafa.ssl.cert.pinning.pas`
    的 `TPinValidator.ExtractPublicKeyHash(...)`
    直接接收：
    - `PX509`
  - `src/fafafa.ssl.openssl.api.pem.pas`
    当前提供的真实文件 helper 是：
    - `LoadCertificateFromPEM(...)`
  - 既然示例直接持有 `PX509`，就还应该显式：
    - `X509_free(...)`

- 这类问题的风险同样很实际：
  - 调用方如果直接照抄 `LoadCertificateFromFile(...)`
    会立刻撞上不存在的 helper
  - 就算自己猜到要切到 low-level `PX509` 路径，旧示例也没有释放句柄，会顺手把资源管理一起教错

- 当前最小正确修法因此仍然是纯控制面收口：
  - 不改 pinning runtime
  - 只把 guide 示例改回：
    - `LoadCertificateFromPEM(...)`
    - `X509_free(...)`
  - 并显式写明：
    - 这里是 OpenSSL raw certificate handle 路径
    - 不是 backend-neutral helper

- 这批收口后的新基线应明确保留：
  - 我们现在不仅在收 capability / factory / migration 这些高入口文档
  - 也已经开始逐页拔 specialized guide 里的“复制即错”残余
  - 后续若继续做文档完整性扫尾，应优先复查：
    - 其它 certificate/security/pinning 相关指南里是否还有同类旧 helper 名
- 在 capability / key-format 口径逐步收口后，又暴露出一条更前排的 docs completeness 残余：
  - `docs/guides/PKCS12_USER_GUIDE.md`
    之前虽然已经说明自己是 OpenSSL backend scoped
    但示例里仍在使用：
    - `LoadCertificateFromFile(...)`
    - `LoadPrivateKeyFromFile(...)`
  - 这两个名字当前在源码里并不存在，不是“旧但兼容保留”，而是会直接把读者带到死路

- 当前源码里真正存在的 PKCS#12 入口分成两层：
  - façade / helper 层：
    - `src/fafafa.ssl.pas`
      已导出：
      - `TPKCS12Manager`
      - `TPKCS12Options`
      - `DefaultPKCS12Options`
  - OpenSSL raw helper 层：
    - `src/fafafa.ssl.openssl.api.pem.pas`
      当前有：
      - `LoadCertificateFromPEM(...)`
      - `LoadPrivateKeyFromPEM(...)`
    - `src/fafafa.ssl.openssl.api.pkcs12.pas`
      当前有 raw `PKCS12_create` / `PKCS12_parse` / `d2i/i2d_PKCS12_bio`

- 这类问题的风险非常实际：
  - `PKCS12_USER_GUIDE` 会让调用方以为：
    - 自己应该先找某个通用 `LoadCertificateFromFile` / `LoadPrivateKeyFromFile`
  - 但当前 façade 真正给的高入口路径其实是：
    - `TPKCS12Manager`
    - `DefaultPKCS12Options`
  - 如果文档不把 helper/raw 边界说清楚，后续做 PKCS#12 导入导出时还会反复走错层级

- 当前最小正确修法因此是纯控制面收口：
  - `PKCS12_USER_GUIDE` 顶部显式区分：
    - 高入口 helper：`fafafa.ssl` / `TPKCS12Manager` / `DefaultPKCS12Options`
    - OpenSSL raw API：`fafafa.ssl.openssl.api.pkcs12` + `fafafa.ssl.openssl.api.pem`
  - 高入口示例改成：
    - `TPKCS12Manager.CreatePKCS12ToFile(...)`
    - `TPKCS12Manager.LoadFromPKCS12File(...)`
  - raw API 示例改成：
    - `LoadCertificateFromPEM(...)`
    - `LoadPrivateKeyFromPEM(...)`
  - `API_REFERENCE` 补出 façade 上实际公开的 PKCS#12 helper 参考段落

- 这批收口后的新基线应明确保留：
  - PKCS#12 文档不再只是“backend truth 对了”，而是连具体可复制示例也重新回到了当前代码入口
  - 后续若继续做证书/密钥路线的完整性审查，应优先复查：
    - 其它 specialized guide 是否也还残留同类“backend scope 已改对，但具体 helper 名还停在旧时代”的问题
- capability dual-truth 这条线继续往下压后，当前剩得最明显的已不再是 runtime/serializer/diff 本身，而是高入口 capability 文档还没有把“谁才是真相源”讲清楚：
  - `src/fafafa.ssl.base.pas` 已经把：
    - `NormalizeLegacyCapabilityBooleans(...)`
    - `@note runtime truth 以 support-level 字段为准；legacy boolean 仅作兼容派生`
    固定成 source truth
  - `src/fafafa.ssl.capability.serializer.pas` 已经把：
    - v1.2 `*Support` 出现时优先回填 legacy boolean
    固定成序列化/反序列化 truth
  - `src/fafafa.ssl.capability.diff.pas` 已经把：
    - `v1.2 support-level 为真相，legacy boolean 仅作兼容回退`
    固定成 diff truth
  - 但 `docs/CAPABILITY_MATRIX_GUIDE.md` / `docs/reference/API_REFERENCE.md` 之前仍只是并排列出两套字段，没有把 precedence 讲透

- 这类问题的风险不只是“文档解释不够完整”，而是会持续把后续审查重新带回已经收掉的双真相歧义：
  - 新读者会误以为：
    - `SupportsSNI` 和 `SNISupport`
    - `SupportsALPN` 和 `ALPNSupport`
    是并列 primary truth
  - 这会直接冲淡前面已经通过 runtime contract 固定下来的 support-level-first 规则
  - 也会让 capability table / selector / optional-interface alignment 的后续讨论反复绕回旧心智

- 当前最小正确修法因此非常清楚，而且只需要动控制面：
  - `CAPABILITY_MATRIX_GUIDE` 明确写出：
    - paired feature 的 `*Support` 是 truth source
    - legacy `Supports*` 只是 compatibility projection
    - `SupportsTLS13` 仍是 primary bool truth，因为当前没有 `TLS13Support`
  - `API_REFERENCE` 在 `TSSLBackendCapabilities` 小节重复同一条 precedence 规则
  - `BACKEND_CAPABILITY_MATRIX` 在表格前补一条简短口径说明，避免读者把表格误读成 legacy bool 表

- 在同一批 capability docs sweep 里，还顺手压实了几处同页相邻漂移：
  - `CAPABILITY_MATRIX_GUIDE` 多处高入口示例仍用 `TSSLFactory.GetLibrary(...)`
    现在统一回到：
    - `TSSLFactory.GetLibraryInstance(...)`
  - `CAPABILITY_MATRIX_GUIDE` / `API_REFERENCE` 里的 `CompatibilityLevel` 之前仍写成：
    - `Byte`
    但当前源码真相是：
    - `Integer`
  - capability guide 的“如何为新后端实现能力矩阵”示例之前仍更像 legacy-bool-first 心智
    现在明确回到：
    - paired feature 先写 `*Support`
    - 再 `NormalizeLegacyCapabilityBooleans(Result);`

- 当前这批收口后的新基线应明确保留：
  - active capability docs 已经重新与 source/runtime precedence 对齐
  - 后续若继续扫 capability 体系，应优先找：
    - 真正尚未统一的 publication granularity
    - coarse-grained bool 是否还需要继续细分
  - 不应再把 active docs 本身当成 paired capability dual-truth 的主要来源
- 继续往“接口设计 / 开发路线 / 审计结论是否还讲真话”这条线上压时，当前最有价值的新问题不是 runtime，而是静态权威审计报告本身已经落后于最近几轮真实收口：
  - `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
    仍然把下面三条写成当前 live drift：
    - factory / builder 仍主动把 `ServerName` 写回 context
    - 活跃文档还承诺 `ISSLServerConnection` 存在
    - `BufferSize` / `HandshakeTimeout` 只是“看起来像 inert 字段”

- 重新核对当前 source / active-doc truth 后，这三条都已经不是原来的状态：
  - `TSSLFactory.CreateContext(...)` 对 `TSSLConfig.ServerName` 现在是 warning + ignore
  - `TSSLContextBuilder.WithSNI(...)` 已经是 compile-time deprecated compatibility-only surface，`BuildClient` / `BuildServer` 都是 warning + ignore
  - 各 direct-library `CreateContext(...)` 当前也已经统一成：
    - server-side reject
    - client-side warning + ignore
  - `docs/ARCHITECTURE.md` / `docs/reference/INTERFACE_DESIGN_V2.md` 现在都已显式说明当前 public Pascal source 尚未声明 `ISSLServerConnection`
  - `TSSLConfig.BufferSize` / `HandshakeTimeout` 当前在 factory / direct-library create-path 上是显式 reject，不是 silent inert

- 这条问题的风险非常实际：
  - 旧审计如果继续当成路线图锚点，会把已经冻结的 compatibility baggage 误判成当前 live blocker
  - 也会让我们反复把精力投回已经收掉的 drift，而不是继续看真正剩余的接口设计问题

- 因而当前最小正确修法不是改 runtime，而是刷新这份审计报告的事实层：
  - 把 context-level SNI 从“仍在传播”改成“已冻结但仍是 public baggage”
  - 把 `ISSLServerConnection` 从“活跃文档失真”改成“当前 docs 已说清楚，但 server-side 对称扩展仍缺位”
  - 把 `TSSLConfig` 从“部分字段疑似 inert”改成“mixed-scope 仍是问题，但部分边界已显式 reject/warn”

- 当前这批收口后的新基线应明确保留：
  - `INTERFACE_DESIGN_AUDIT_V1.5.0.md` 现在重新回到当前源码/活跃文档真相
  - 这意味着“路线判断控制面”比上一轮更干净了
  - 后续继续做接口设计优先级时，应优先盯：
    - `ISSLConnection` core slimming / owner-surface demotion
    - `TSSLConfig` mixed-scope public surface surgery
    - capability matrix dual-truth cleanup
    - facade main entry slimming
- 继续沿着“高入口文档是否还在教授旧 public entrypoint”往下压时，这次压实的是另一组更前排、也更容易让用户第一步就走错的导入/创建路径漂移：
  - `docs/guides/USER_GUIDE.md`
  - `docs/guides/WINSSL_QUICKSTART.md`
  - `docs/guides/WINSSL_USER_GUIDE.md`
  - `docs/guides/MBEDTLS_USER_GUIDE.md`
  - `docs/guides/TROUBLESHOOTING.md`
  - `docs/reference/API_REFERENCE.md`
  仍在混用：
  - `fafafa.ssl.abstract.intf`
  - `fafafa.ssl.abstract.types`
  - 不存在的 `fafafa.ssl.openssl` facade unit
  - 不存在的 `CreateSSLLibrary(...)`
  - 旧枚举名 `sslLibraryWinSSL` / `sslLibraryOpenSSL` / `sslLibraryAutoDetect`
  - 旧上下文枚举名 `sslContextClient`
  - 不存在的 `GetLibraryName`
  - 手动 `LoadOpenSSL` 作为普通应用入口

- 这类问题的风险不是“文档有点旧”，而是会直接把新用户送进编译失败或错误心智：
  - 直接复制 `uses fafafa.ssl.abstract.intf` / `fafafa.ssl.openssl` 会命中已删除或不存在的单元
  - 复制 `CreateSSLLibrary(...)` / `sslLibraryWinSSL` 会命中当前 shipped source 不存在的 creator/枚举
  - 把 `LoadOpenSSL` 当成通用入口又会把高入口用户带进不该先碰的底层 loader 语境

- 这条线当前最小正确修法也很明确：
  - 高入口示例统一回到：
    - `fafafa.ssl`
    - `TSSLFactory.GetLibraryInstance(...)`
    - `TSSLFactory.IsLibraryAvailable(...)`
    - `sslCtxClient`
    - `LibraryTypeToString(Lib.GetLibraryType)`
  - `API_REFERENCE` 明确区分：
    - 当前 public library-entrypoint
    - backend-specific low-level creators
  - `TROUBLESHOOTING` 只保留统一工厂入口排障，不再继续教授手动底层 loader

- 在这批 source-backed sweep 里，还顺手压实了几处与本批同文件相邻、会让示例本身失真的 drift：
  - `USER_GUIDE` 的证书 SAN 示例当前应使用 `TSSLStringArray`，不是 `TStringList`
  - `USER_GUIDE` / `TROUBLESHOOTING` 中 `TSSLEnterpriseConfig` 的类方法当前是：
    - `IsFIPSEnabled`
    - `GetTrustedRoots`
    - `GetAllPolicies`
  - `API_REFERENCE` 的 WinSSL 错误辅助函数当前是：
    - `GetFriendlyErrorMessageCN`
    - `GetFriendlyErrorMessageEN`

- 当前这批收口后的新基线应明确保留：
  - 高入口导入/creator/path 已重新锚回当前 facade/factory truth
  - 新增 contract 还顺手修掉了自身的跨行 `rg` 噪音，后续重复验证不会再反复吐 multiline 警告
  - 后续如果继续扫 API/reference completeness，应优先信源码和当前 facade，而不是更早一轮的说明性笔记
- 继续沿着“高入口文档是否仍在教授旧 public entrypoint”这条线往下压时，`MIGRATION_GUIDE` 暴露的是另一类更重的漂移：
  - 它不只是某几个代码片段旧，而是整份迁移主线仍停在 `v0.7 / v0.8`
  - 顶部版本、迁移叙事、helper 命名、单元引用一起落在旧时期心智上

- 当前压实的具体问题非常明确：
  - 顶部仍写：
    - `v0.8`
  - 仍把：
    - `v0.7 → v0.8`
    - `v0.6 → v0.7`
    当成当前 active migration 章节
  - 迁移代码仍使用已经不存在的：
    - `fafafa.ssl.abstract.intf`
    - `fafafa.ssl.openssl` facade unit
  - WinSSL 企业 helper 仍写成旧名称：
    - `IsFipsModeEnabled`
    - `GetEnterpriseTrustedRoots`
    - `GetGroupPolicies`
  - OpenSSL 低层 error helper 仍被混成 generic 迁移 API

- 这类问题的风险不只是“文档旧”，而是会直接把迁移用户带进错误入口：
  - 新用户会先学到不存在的单元名
  - 也会误以为 backend-specific helper 是跨 backend 统一 contract
  - 更糟的是，整个 guide 会继续把 `v0.x` 历史快照包装成当前 `v1.5.0` 的迁移主路径

- 这条线当前最小正确修法不是继续在旧结构上补注释，而是直接把 active migration guide 重写回当前主路径：
  - 以：
    - `src/fafafa.ssl.base.pas`
    - `src/fafafa.ssl.pas`
    - `src/fafafa.ssl.tls.pas`
    - `docs/reference/API_REFERENCE.md`
    作为当前 truth anchor
  - 迁移示例回到：
    - `fafafa.ssl`
    - `fafafa.ssl.context.builder`
    - `TSSLConnector`
    - `TSSLStream`
  - client SNI/hostname 明确回到：
    - `TSSLConnector.ConnectSocket(..., ServerName)`
    - 或 `ISSLClientConnection.SetServerName(...)`
  - WinSSL enterprise / OpenSSL low-level helper 的边界重新分类清楚

- 当前这批收口后的新基线应明确保留：
  - `MIGRATION_GUIDE` 已不再是 `v0.x` 历史快照伪装成 active guide
  - 当前迁移入口已经重新锚回当前公开门面与连接语义
  - 后续如果继续做 onboarding / migration 审查，不需要再把这份 guide 当成未收口的旧版本主入口

- 继续沿着“高入口 active docs 是否还在教授旧 public surface”这条线往下压时，又压实了一批直接影响上手用户的连接 API 漂移：
  - `docs/reference/API_DOCUMENTATION.md`
    还停在旧的连接时代：
    - `ISSLConnection.Connect(host, port)`
    - `CreateConnection(443)`
    - `Disconnect`
    - `Write(string)` / `Read(TBytes)` 旧签名
    - `Connection.GetLastError`
    - `Connection.GetPeerCertificateVerified`
  - `docs/guides/WINSSL_BEST_PRACTICES.md`
    还在测试最佳实践里继续教授：
    - `LConn.Connect('example.com', 443)`
    - `LConn.Connect('localhost', 8443)`
  - `docs/guides/WINSSL_USER_GUIDE.md`
    还把 WinSSL 讲成“与 OpenSSL 后端完全相同的接口”

- 这批问题的风险不只是文案过时，而是会直接把调用方带回不存在或已变形的 public surface：
  - 新用户照着 `API_DOCUMENTATION` 抄代码，会得到与当前 shipped source 不匹配的连接初始化、I/O 和错误处理写法
  - WinSSL 指南里的“完全同构”说法，又会掩盖前面已经连续收口过的 backend-specific capability truth
    - callbacks
    - DER/PKCS8 私钥导入
    - PKCS#12 helper 范围

- 这条线当前最小正确修法也很明确：
  - 不改 runtime
  - 不扩到更大范围的 guide 全量重写
  - 只把高入口文档拉回当前 source truth：
    - transport 先建立，再 `CreateConnection(Socket/Stream)`
    - client SNI 走 `ISSLClientConnection.SetServerName(...)`
    - `ISSLConnection.Connect` 当前是无参握手入口
    - 文本 I/O 走 `ReadString` / `WriteString`
    - 证书验证问题走 `GetVerifyResult` / `GetVerifyResultString`
  - 再用 focused contract 守住：
    - 旧 `Connect(host, port)` 片段
    - 旧 `Disconnect` / `Connection.GetLastError`
    - WinSSL “完全相同的接口” overclaim

- 当前这批收口后的新基线应明确保留：
  - `API_DOCUMENTATION` 已经重新回到 current `ISSLConnection` / `ISSLClientConnection` 真相
  - `WINSSL_BEST_PRACTICES` 不再把旧连接调用形状当作推荐路径
  - `WINSSL_USER_GUIDE` 现在明确：
    - 核心 public interface 一致
    - 但 published capability 仍然是 backend-specific truth

- 在修完 `ReadString` 活跃示例签名漂移之后，继续扫 `ISSLConnection` / owner-surface 残口时，又压实了另一条高入口指导面回流：
  - `GetSelectedALPNProtocol` 当前在源码里已经是：
    - `deprecated 'Use ISSLConnectionInfo.GetSelectedALPNProtocol'`
  - 但活跃指导面仍有两处把它教成 `ISSLConnection` 普通主路径：
    - `docs/guides/WINSSL_USER_GUIDE.md`
    - `examples/https_server/https_server_alpn.pas`

- 这条问题的风险并不只是文案不够新，而是会把已经 demote 的 mirror surface 又带回主路径：
  - guide 会让读者觉得“ALPN 协商结果就该直接从 connection 取”
  - example 会把这种旧路径固化成可复制代码
  - 结果会冲淡前面已经收口的 `ISSLConnectionInfo` owner-surface 真相

- 当前这批的最小正确修法同样很清楚：
  - 不改 ALPN runtime/backends
  - 不改 public signature
  - 只把活跃 guide/example 统一回 owner path：
    - guide 明确指向 `ISSLConnectionInfo.GetSelectedALPNProtocol`
    - example 先 `Supports(Connection, ISSLConnectionInfo, ...)`
      再读取协商结果

- 这批收口后的新基线应明确保留：
  - `GetSelectedALPNProtocol` 在 `ISSLConnection` 上当前只应被视为 compatibility-core mirror
  - 活跃 guide/example 不再应把它教成普通主路径
  - 后续若继续做 `ISSLConnectionInfo` / owner-surface completeness 审查，可以把这条 ALPN 活跃示例误导视为已收口问题

- 在 `ISSLConnection` convenience surface 路线真相收口之后，继续扫活跃 guide/reference/example 时，又压实了一条更具体的用法级漂移：
  - 多份活跃入口还在把 `ReadString` 当成“直接返回字符串”的旧签名
  - 但当前 shipped source 真相一直是：
    - `function ReadString(out AStr: string): Boolean;`

- 这条问题不是轻微文案差异，而是会直接把调用方带到错误 API 形状：
  - `docs/reference/API_REFERENCE.md`
  - `docs/guides/USER_GUIDE.md`
  - `docs/guides/MIGRATION_GUIDE.md`
  - `examples/04_https_rest_client.pas`
  都存在同类旧用法残留

- 这类漂移的风险非常实际：
  - 它会让用户照抄后得到与当前 public signature 不匹配的代码
  - 也会让前面刚收口的 `ISSLConnection` convenience truth 看起来仍然“不稳定”
  - 因为问题发生在高入口 reference/guide/example，上手用户最容易先踩到

- 当前这批最小正确修法很明确：
  - 不改 `ReadString` runtime 实现
  - 不改 `ISSLConnection` public signature
  - 只把活跃入口统一回 source truth：
    - `if Conn.ReadString(LData) then ...`
  - 再用 focused contract 守住不要回流旧签名示例

- 这批收口后的新基线应明确保留：
  - `ReadString` 当前仍是 `out` 参数 + `Boolean` 返回值的 convenience-core 文本 helper
  - 活跃 docs/examples 不再应把它教成“直接返回字符串”的函数
  - 后续若继续做 `ISSLConnection` / docs completeness 审查，可以把这类 `ReadString` 签名误导视为已收口问题

- 继续沿着最早的 `ISSLConnection 太胖` 设计审计往下压时，这次真正暴露出来的不是“马上要拆接口”的实现 bug，而是路线真相已经分叉：
  - `src/fafafa.ssl.base.pas` 仍正式保留：
    - `ReadString` / `WriteString`
    - `SetTimeout` / `GetTimeout`
    - `SetBlocking` / `GetBlocking`
  - `src/fafafa.ssl.connection.builder.pas` 仍直接通过 `AConnection.SetTimeout(...)` / `SetBlocking(...)` 应用构建期设置
  - `docs/reference/API_REFERENCE.md` 也一直把这组方法作为 shipped source truth 列出
  - 但 `docs/reference/INTERFACE_DESIGN_V2.md` 却把它们写成 `**移除**`

- 这类问题的风险不在 runtime，而在后续路线被文档自己带偏：
  - 会把“未来想做的最小 core”误读成“当前源码已经完成的收口”
  - 会让后续审查者不断重开同一条 convenience/core 线路，误以为实现还没跟上
  - 也会把真正已经进入 owner-surface demotion 的 mirrors，和仍被 builder/guides 使用的 convenience 方法混成一类

- 这批重新压实后的当前 truth 应明确分成两类：
  - 已进入 owner-surface demotion / compile-time deprecated 的 mirrors：
    - `GetConnectionInfo`
    - `GetContext`
    - `GetSelectedALPNProtocol`
    - `GetStateString`
    - `GetVerifyResult`
    - `GetVerifyResultString`
  - 仍然 shipped 且有活跃调用面的 convenience-core / connection-adjacent surface：
    - `ReadString` / `WriteString`
    - `SetTimeout` / `GetTimeout`
    - `SetBlocking` / `GetBlocking`

- 因而这条线当前最小正确修法不是 public API surgery，而是先把 classification truth 钉牢：
  - source comments 说明推荐路径
  - `API_REFERENCE` 说明 builder-first / transport-first 的当前心智
  - `INTERFACE_DESIGN_V2` 明确这是 v2 目标，而不是 current source mirror
  - `ARCHITECTURE` / `INTERFACE_DESIGN_AUDIT_V1.5.0` 不再把 convenience 方法误报成“当前已被移除”

- 这批收口后的新基线应明确保留：
  - `ReadString` / `WriteString` 当前是 `v1.x` convenience-core 文本 helper
  - timeout / blocking 当前是 `v1.x` connection-adjacent convenience surface，推荐 builder-first，但连接侧 override 仍是 shipped truth
  - 如果未来真要让这组方法退出 `ISSLConnection` core，应该新开独立 `v2` API surgery 批次，而不是继续把它当成“当前实现缺口”

- 沿着刚收口的 `API_REFERENCE` high-entry surface 继续往下审时，又压实了同类但更严重的一条缺口：
  - `ISSLCertificate` 主代码块不只是缺少零星方法，而是仍停留在旧的窄化证书 surface
  - `ISSLCertificateStore` 甚至连独立高入口小节都没有

- 这条问题的风险非常直接：
  - 证书 / 证书库本来就是 SSL 公共接口里最基础的一组对象
  - 如果 canonical API 文档把这组 surface 讲得比源码更小，调用方会误以为：
    - 没有 `LoadFromMemory` / `SaveToStream`
    - 没有 `GetInfo` / `GetFingerprint(...)`
    - 没有 issuer-link / clone
    - 甚至没有一套明确的 `ISSLCertificateStore` API 入口

- 当前压实的具体 drift 包括：
  - `ISSLCertificate` 代码块之前遗漏：
    - `LoadFromMemory`
    - `SaveToStream`
    - `GetInfo`
    - `GetPublicKeyAlgorithm`
    - `GetSignatureAlgorithm`
    - `GetDaysUntilExpiry`
    - `GetSubjectCN`
    - `GetExtension`
    - `GetFingerprint(AHashType: TSSLHash)`
    - `SetIssuerCertificate`
    - `GetIssuerCertificate`
    - `Clone`
  - 扩展集合类型也还停在旧心智：
    - `TStringList`
    - 而当前源码真相已经是：
      - `TSSLStringArray`
  - `ISSLCertificateStore` 当前在 `API_REFERENCE` 里缺少独立 section

- 这条线的最小正确修法同样很明确：
  - 不改 public Pascal source
  - 不把 scope 扩到 runtime certificate verification / backend implementation
  - 只把 `API_REFERENCE` 的证书相关高入口 surface 拉回 current source truth
  - 再用 focused contract 守住以后不要重新回漂成“窄化子集 + 缺失小节”

- 当前这批收口后的新基线应明确保留：
  - `ISSLCertificate` 已不再被 active canonical doc 写小
  - `ISSLCertificateStore` 已拥有正式高入口 section
  - 后续若继续做 interface-design 审查，不需要再怀疑 `API_REFERENCE` 的 certificate/store surface 是否还是旧状态

- 继续按“活跃 canonical docs / 活跃 generic tests / source truth”这条线往下审时，又压实了一条很适合当前阶段收口的高入口 drift：
  - `docs/reference/API_REFERENCE.md` 的 `ISSLLibrary` / `ISSLContext` 主代码块
  - 还停留在旧的精简接口面
  - 漏掉了一批当前 `src/fafafa.ssl.base.pas` 已经明确 shipping 的 public methods

- 这类问题虽然不碰 runtime，但风险一点都不轻：
  - `API_REFERENCE` 本来就是当前最容易被当作权威入口打开的 active canonical doc
  - 一旦它把代码块写得比源码更窄，调用方就会学到错误的 surface boundary
  - 后续会把真实的 mixed-scope / compatibility 设计债误判成“源码里根本没有这些入口”

- 当前压实的缺口很具体，不是泛泛的“文档有点旧”：
  - `ISSLLibrary` 代码块之前遗漏：
    - `SetDefaultConfig`
    - `GetDefaultConfig`
    - `GetStatistics`
    - `ResetStatistics`
  - `ISSLContext` 代码块之前遗漏：
    - `SetPreferredVersion` / `GetPreferredVersion`
    - `LoadCertificatePEM` / `LoadPrivateKeyPEM`
    - `SetSessionCacheSize` / `GetSessionCacheSize`
    - `SetOptions` / `GetOptions`
    - `SetServerName` / `GetServerName`
    - `SetALPNProtocols` / `GetALPNProtocols`
    - `SetCertVerifyFlags` / `GetCertVerifyFlags`
    - `SetPasswordCallback` / `SetInfoCallback`
    - certificate pinning helpers

- 这条线的最小正确修法也很清楚：
  - 不改 public Pascal source
  - 不重开 broader `TSSLConfig` slimming / `ISSLConnection` surgery
  - 只把 `API_REFERENCE` 的两段代码块恢复成 current source-truth view
  - 再用 focused contract 守住以后别回漂成“旧精简子集”

- 当前这批收口后的新基线应明确保留：
  - `API_REFERENCE` 的 `ISSLLibrary` / `ISSLContext` 代码块不再只是演示性子集
  - 它们现在应被当作当前 shipped public surface 的高入口视图
  - 后续若继续做 interface-design 审查，可以把这两块视为已收口，不必重复拉起

- 继续做 backend interface/completeness 静态审查时，挖出了一条比 capability wording 更深的结构性问题：
  - `tests/contract/test_backend_contract.pas` 的公共心智已经明确要求：
    - `EarlyDataSupport = none` 时，不应暴露 `ISSLEarlyDataContext / ISSLEarlyDataConnection`
    - `OCSPStaplingSupport = none` 时，不应暴露 `ISSLServerOCSPStaplingContext`
  - 但源码层面并不完全满足：
    - OpenSSL base context 之前无条件实现 `ISSLEarlyDataContext` 与 `ISSLServerOCSPStaplingContext`
    - OpenSSL base connection 之前无条件实现 `ISSLEarlyDataConnection`
    - WolfSSL base context 之前无条件实现 `ISSLServerOCSPStaplingContext`

- 这类问题的风险比普通 capability 字段漂移更高：
  - builder / factory / helper / caller 会把 `Supports(...)` 当作 public truth
  - 一旦 capability 是 `none`，但 interface 仍暴露，就会出现：
    - source contract 说“不应该有”
    - 调用方却还能拿到接口
  - 这会直接污染接口设计完整性的判断，而不只是文案偏差

- 这一轮还顺手确认了一个 FPC 层面的实现约束，必须记录下来避免下次重复踩坑：
  - `GetInterface` 不是当前这里可 override 的收口点
  - 试图在这些类上直接 `override GetInterface(...)` 会被编译器打回
  - 因而在这个仓库里，针对 optional interface 的稳定收口方案应优先使用：
    - capability-gated subclass
    - `CreateContext` / `CreateConnection` 选择不同具体类
  - 而不是继续尝试靠 `GetInterface`/分发层拦截

- 最小正确修法因此变得很清楚：
  - 不改 runtime capability 实现本身
  - 不重写现有 early-data / OCSP 方法体
  - 只把 base class 与 optional interface 解耦
  - 然后用 capability-gated subclass 重新把 public surface 接回去

- 当前收口后的新基线应明确保留：
  - OpenSSL：
    - `TOpenSSLContext` 只保留 core context / native-handle / HTTP hooks
    - `TOpenSSLEarlyDataContext`
    - `TOpenSSLServerOCSPContext`
    - `TOpenSSLAdvancedContext`
      负责按 capability 组合暴露 optional interface
    - `TOpenSSLEarlyDataConnection`
      只在 parent context 仍暴露 `ISSLEarlyDataContext` 时才创建
  - WolfSSL：
    - `TWolfSSLContext` 不再无条件实现 server OCSP stapling interface
    - `TWolfSSLOCSPStaplingContext`
    - `TWolfSSLEarlyDataContext`
    - `TWolfSSLAdvancedContext`
      负责按 capability 组合暴露 optional interface

- 这意味着后续如果再做 interface/completeness 审查，一个很重要的原则已经被正式钉住：
  - `GetCapabilities`、`Supports(...)`、`CreateContext/CreateConnection` 的具体类选择，必须是同一套 truth
  - 不能允许 capability 与 public optional interface 再各说各话

- 再次核对当前权威源后，上一轮候选清单里有一部分已经过时：
  - `docs/test_reports/RELEASE_READINESS_V1.5.0.md` 已明确说明：
    - `v1.5.0` 已发布
    - cross-platform runtime workflow 已绿
  - 所以这轮不能再把平台总口径简单改成“都还没完成”，而是要区分：
    - 发布链已经闭环
    - WinSSL 的 session resumption / tickets 仍不是“完整 runtime-proven”

- 这轮最值得收掉的高入口漂移，不是实现缺口，而是“当前权威入口和当前能力边界被旧叙事盖住”：
  - `RELEASE_NOTES.md`
    - 顶部仍把 `v1.0.0 / 99.5% / Production Ready` 当作当前主叙事
  - `PLATFORM_SUPPORT.md`
    - 同时残留两类相反方向的漂移：
      - Windows/WinSSL 被写得过满
      - macOS 又仍被写成“验证中”
  - `WINSSL_USER_GUIDE.md` / `ZERO_DEPENDENCY_DEPLOYMENT.md`
    - 继续把 WinSSL 写成 `100% 完成`
    - 继续把 session resumption / tickets 讲成稳定成功能力

- 这说明“活跃文档真相”不能只看一种漂移：
  - 过强表述会误导下一步路线，以为能力已经 runtime-complete
  - 过弱表述也会误导下一步路线，以为平台发布链还没闭环
  - 二者都需要重新锚到同一组 authority docs

- 当前最有效的修法不是重写整页历史内容，而是把高入口文档重新分层：
  - 当前稳定 truth 放到文档顶部
  - 旧 `v1.0.0` 内容降级成显式历史快照
  - WinSSL 细能力边界直接锚到：
    - `WINSSL_BACKEND_STATUS_REPORT`
    - `WINSSL_BACKEND_CAPABILITY_MATRIX`

- 这轮还顺手暴露了一类常被忽略、但实际会直接误导用户的真问题：
  - 活跃文档仍残留 `yourusername` / `your-repo` / `your.email@example.com`

- 沿着上一批的 key-format / password-protected truth 继续压时，这次又挖出一条很典型的 coarse-grained capability 假阳性：
  - `src/fafafa.ssl.mbedtls.lib.pas`
  - `src/fafafa.ssl.wolfssl.lib.pas`
  之前都仍发布：
    - `SupportsPKCS12 := True`
  - 但当前 shipped context surface 只看到：
    - `LoadCertificate*`
    - `LoadPrivateKey*`
    的 PEM / DER / PKCS#8 路径
  - 并没有任何 public PKCS#12 create / parse / import surface

- 这条问题不只是 capability 字段写宽了，还会把全局文档心智一起带偏：
  - `docs/guides/FAQ.md`
    - 仍写“PKCS#12 支持计划中”
  - `docs/guides/PKCS12_USER_GUIDE.md`
    - 又写“通过 OpenSSL 后端提供完整支持”
  - 如果不把 backend 粒度说清，调用方既可能误以为：
    - 所有 backend 都支持 PKCS#12
  - 也可能误以为：
    - 当前版本完全不支持 PKCS#12

- 这一批压实后的当前 truth 应明确分成三档：
  - `OpenSSL`
    - `SupportsPKCS12=True`
    - 当前发布完整 PKCS#12 helper/API surface：
      - create
      - parse
      - BIO I/O
  - `WinSSL`
    - `SupportsPKCS12=True`
    - 当前只代表：
      - `PFX/P12` certificate/private-key bundle import
    - 不等于拥有 OpenSSL 风格的 PKCS#12 helper/API
  - `FreePascal` / `MbedTLS` / `WolfSSL`
    - `SupportsPKCS12=False`
    - 当前没有 shipped PKCS#12 bundle create / parse / import surface

- 因而这条线的最小正确修法也很清楚：
  - 不补做 `MbedTLS` / `WolfSSL` PKCS#12 runtime
  - 不把 scope 扩到 OpenSSL PKCS#12 helper 设计
  - 只把 optional backends 的 `SupportsPKCS12` 收回到 `False`
  - 再把 active global docs 统一回 backend-specific truth

- 这批收口后的新基线应明确保留：
  - `SupportsPKCS12=True`
    - 不能再被误读成“所有 backend 都有同等 PKCS#12 能力”
  - 当前 `OpenSSL` 与 `WinSSL` 虽然都为 `True`，但代表的是不同粒度的 published surface
  - 后续若继续做 key-format / bundle capability 审查，应优先检查：
    - coarse-grained bool 是否掩盖了 backend-specific surface 差异

- 沿着 capability / active guidance 真相继续往下压时，这次 `MbedTLS` 暴露出来的主要问题已经不是源码 capability 本身，而是高入口文档比当前 public API 和 published surface 更宽：
  - `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
    - 之前仍把：
      - `0-RTT`
      - `证书固定`
      - `自定义 I/O`
      写成“部分支持 / 通过回调 / 回调函数”
  - 但当前源码真相是：
    - `SupportsCallbacks=False`
    - `TMbedTLSContext.SetVerifyCallback/SetPasswordCallback/SetInfoCallback`
      对 non-nil assignment 都会 fail-closed `unsupported`
    - `MbedTLS` 当前不暴露：
      - `ISSLEarlyDataContext`
      - `ISSLEarlyDataConnection`
    - 证书固定走的是：
      - `AddCertificatePin`
      - `AddCertificatePinBase64`
      - `SetCertificatePinningEnabled`
    - transport 连接面对调用方只发布：
      - `CreateConnection(ASocket)`
      - `CreateConnection(AStream)`
      而不是 caller-supplied custom I/O callback seam

- `docs/guides/MBEDTLS_USER_GUIDE.md` 的漂移更直接，会让调用方照抄就撞到旧接口：
  - 它之前仍保留：
    - `LoadCertificateFromFile`
    - `LoadPrivateKeyFromFile`
    - `LoadCAFromFile`
    - `Connection.SetHostname`
    - `Connection.Connect(host, port)`
    - `ReadAll`
    - `GetCipherSuite`
    - `GetLastError: string`
  - 这些都已经不是当前 `ISSLContext` / `ISSLConnection` 的 shipped source truth

- 这条问题的风险不只是“文档有点旧”，而是会把两层真相一起带偏：
  - 一层是接口名和签名本身已经过时
  - 另一层是 backend-specific capability 被文档讲成了“完全相同的接口”
  - 结果会让后续审查反复误判：
    - 以为 MbedTLS 已发布 callback / 0-RTT / custom I/O surface
    - 或以为 user-guide 里的旧方法名仍是当前 source truth

- 这批重新压实后的当前 truth 应明确保留：
  - `MbedTLS` 与其它 backend 共享统一核心接口，但 published capability 明显 backend-specific
  - 当前：
    - `SupportsCallbacks=False`
    - `SupportsPKCS12=False`
    - `SupportsFIPSMode=False`
    - 0-RTT current public capability = none
  - MbedTLS 高入口 guide/reference 应该优先教授：
    - 当前 `CreateContext(sslCtxClient)` 形状
    - `LoadCertificate` / `LoadPrivateKey` / `LoadCAFile`
    - `ISSLClientConnection.SetServerName`
    - `ReadString(out ...)`
    - `GetCipherName`
    - `GetLastErrorString`
  - 它们不是无害占位，而是错误导航入口

- 本批收口后的新基线应明确保留：
  - 当前 stable release truth = `v1.5.0` 已发布
  - 当前平台发布 truth = Linux / macOS / Windows 都已有发布链证据
  - 当前 WinSSL backend truth = 零依赖客户端 baseline 已验证，但 `session resumption / tickets` 继续按 experimental public surface 理解
  - 当前 WinSSL session 摘要 = `observed_reuse=false` / `session_configured=true`

- 在收掉根入口 broken links 之后，下一层明显的路线图级误导其实不是坏链接，而是“未来态残留”：
  - 几份活跃文档虽然能打开，但仍把已经存在的 backend 写成“计划中/未来”
  - 这会比单纯 broken link 更隐蔽，因为阅读者会以为这是当前权威事实

- 这批 focused sweep 明确暴露了 3 类 stale truth：
  - `docs/reference/BACKEND_ABSTRACTION_LAYER_DESIGN.md`
    - 仍把 `FreePascal` 写成 `❌ 计划中`
  - `docs/guides/USER_GUIDE.md`
    - 仍把 `MbedTLS` 推荐挂着 `(未来)`
  - `docs/MIGRATION_GUIDE_V1.1.md` / `docs/ARCHITECTURE.md` / `docs/NATIVE_HANDLE_QUICK_REF.md`
    - 仍把“纯 Pascal backend”讲成将来态，而不是当前已经存在的 `sslFreePascal`

- 这类问题的真正风险在于它会扭曲我们对接口设计边界的理解：
  - `GetNativeHandle` optional-boundary 的价值，会被误解成“为了将来某个 backend 提前设计”
  - 而不是“当前 `sslFreePascal` 已经在使用这条边界”

- 当前这批收口后的新基线应明确保留：
  - `sslFreePascal` 是当前已实现、已进入工厂/能力矩阵/测试体系的 backend family
  - `sslMbedTLS` 也已是当前可用 backend，不应继续被用户导向文档写成 future-only
  - optional native-handle 设计的现实受益者之一就是当前 `sslFreePascal`

- 在继续扫描活跃文档入口时，又确认了一条会明显拖慢后续审查节奏的现实问题：
  - 仓库里不只是有“个别 stale wording”，而是存在一批真实 broken links
  - 而且它们集中出现在最容易被先点开的根入口文档：
    - `PLATFORM_SUPPORT`
    - `RELEASE_NOTES`
    - `TOOLS`
    - `ZERO_DEPENDENCY_DEPLOYMENT`
    - `guides/WINSSL_USER_GUIDE`

- 这类问题的危害很直接：
  - 它会把继续做 backend/platform/WinSSL 审查的人先导向旧路径、占位路径或已经消失的 phase 报告
  - 结果不是“阅读体验差一点”，而是后续每次都要先重新辨认真正的权威入口

- 当前这批 focused 收口后，入口真相已经重新建立：
  - `PLATFORM_SUPPORT` 统一回到当前活跃的：
    - `docs/guides/*`
    - `docs/reference/API_REFERENCE.md`
  - `RELEASE_NOTES` 的文档入口不再指向大小写错误或已消失的 `docs/*` 旧名
  - `ZERO_DEPENDENCY_DEPLOYMENT` 和 `WINSSL_USER_GUIDE`
    - 已从历史 phase reports / `.claude/plan` / 消失的 WinSSL test report
      切回当前有效的：
      - `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
      - `docs/reference/WINSSL_DESIGN.md`
      - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
      - `docs/reference/WINSSL_PERFORMANCE_TUNING.md`

- 这一轮也顺带说明了下一阶段文档审查的一个方法论：
  - 不要只 grep “计划中/未来” 这种字面漂移
  - 还要把“高入口文档是否仍能把人导向真实存在、当前活跃的 truth source”当成第一类质量问题

- 继续做 backend completeness 审查时，又发现一条很容易把后续工作流带偏的活跃文档裂缝：
  - `docs/BACKEND_CAPABILITY_MATRIX.md` 底部还在链接：
  - `reference/OPENSSL_BACKEND.md`
    - `reference/WINSSL_BACKEND.md`
  - 但这两个文件在当前仓库里根本不存在

- 这类问题的风险不只是“点开 404”：
  - 主能力矩阵本来就是现在最容易被继续拿来做 backend 决策入口的文档之一
  - 如果它还把人导向不存在或历史想象中的 backend 文档，
  - 后续审查就会不断从错误入口重新拉起

- 同一轮又顺手挖出另一条更基础的 enum truth 漂移：
  - `docs/reference/API_REFERENCE.md` 的 `TSSLLibraryType` 示例只剩：
    - `sslOpenSSL`
    - `sslWinSSL`
    - `sslMbedTLS // 计划中`
  - 这和当前源码真相已经明显打架：
    - `sslAutoDetect`
    - `sslOpenSSL`
    - `sslWolfSSL`
    - `sslMbedTLS`
    - `sslWinSSL`
    - `sslFreePascal`

- 更关键的是，源码自己也还残留一条过期心智模型：
  - `src/fafafa.ssl.base.pas` 里 `sslFreePascal` 注释仍写成“纯 FreePascal 实现（未来）”
  - 但当前仓库里 `src/fafafa.ssl.freepascal.*.pas` 已经是活跃实现，不应继续被 enum 注释说成“未来”

- 这批最小正确修法因此非常窄：
  - 不重写整个文档索引
  - 不改 backend 行为
  - 只把：
    - 主能力矩阵 backend links
    - `API_REFERENCE` 的 `TSSLLibraryType` 示例
    - `src/fafafa.ssl.base.pas` 的 `sslFreePascal` 注释
    收回当前真实状态

- 当前收口后的新基线应明确保留：
  - 顶层 backend 能力入口只能指向仓库里真实存在的活跃文档
  - `TSSLLibraryType` 的公开示例必须与源码完整枚举同步
  - `sslFreePascal` 不能再被任何活跃源码/文档表述为“未来态”

- 继续沿“接口设计 + 各 backend completeness”做横向静态审查时，挖出来一条比文档漂移更实的能力发布裂缝：
  - `TMbedTLSLibrary.IsFeatureSupported(sslFeatSessionCache)` 早就返回 `True`
  - `TWolfSSLLibrary.IsFeatureSupported(sslFeatSessionCache)` 也早就返回 `True`
  - 但两边的 `GetCapabilities` 却都没有给 `SessionCacheSupport` 赋值

- 这条裂缝的风险并不只停留在“字段没填完整”：
  - `src/fafafa.ssl.backend.selector.pas` 对 `sslFeatSessionCache` 的必需特性判断，看的正是 `SessionCacheSupport <> sslSupportNone`
  - 所以如果继续放着不修：
    - backend 自己说“支持 session cache”
    - selector 却可能把它当“不支持”
  - 这会直接污染“能力选择器 / capability matrix / 后端文档”三条线的共同真相

- 当前最小正确修法也因此非常明确：
  - 不重开 `mbedtls` / `wolfssl` 的 session 实现线
  - 只把当前 source 已经公开宣称的 `session cache` truth 显式发布到 `GetCapabilities`
  - 让 `IsFeatureSupported` 与 `SessionCacheSupport` 重新一致

- 同一轮审查里，WinSSL 的活跃文档也暴露出另一类真正会误导开发路线的漂移：
  - `docs/BACKEND_CAPABILITY_MATRIX.md` 还把 `Session Resumption` 对所有 backend 一律写成 `✅`
  - `docs/guides/QUICKSTART.md` 还在写 WinSSL `70-90%` 性能收益和“复用成功”口径
  - `docs/reference/WINSSL_DESIGN.md` 还把 `QueryContextAttributes(SECPKG_ATTR_SESSION_INFO)` 写成 shared session flow 的正常一步
  - `docs/reference/BACKEND_ABSTRACTION_LAYER_DESIGN.md` / `docs/reference/BACKEND_SELECTOR_DESIGN.md`
    还把 WinSSL `OCSP Stapling` / `Session Ticket` 画成无条件完整支持

- 这些漂移的共同问题，不是“历史文档老一点”这么简单，而是它们会直接扭曲我们对下一步路线的判断：
  - 会让人误以为 WinSSL session resumption 已经 runtime-proven
  - 会让人误以为 WinSSL 的 `Session Ticket` / `OCSP Stapling` 已经可被 selector 当作稳定需求
  - 也会掩盖 native `SECPKG_ATTR_SESSION_INFO` probe 当前仍应只停留在
    `opt-in isolated worker / experimental evidence lane`

- 因而这批 focused 收口后的新基线应明确保留：
  - `MbedTLS / WolfSSL`
    - `SessionCacheSupport` 现已与 source feature truth 对齐
  - `FreePascal`
    - quick matrix 上的 `Session Resumption` 不应再被写成 `✅`，因为 `SessionTicketsSupport / SessionCacheSupport` 仍是 `experimental`
  - `WinSSL`
    - 当前 public truth 仍是 `observed_reuse=false / session_configured=true`
    - 活跃文档不应再把它表述成稳定 runtime 闭环或通用性能结论

- 这次 `winssl.session.pas` 漂移暴露出来之后，最该补的并不是更多解释，而是 repo 级 allowlist guard：
  - 因为只要 `SECPKG_ATTR_SESSION_INFO` 还可以在别的 WinSSL 文件里悄悄出现
  - 我们就可能不断重复抓到“新的未隔离 probe 残留”

- 现在这条 guard 已经补上：
  - 允许的受控 site 只剩：
    - canonical helper in `winssl.connection.pas`
    - dedicated proof in `test_winssl_session_resumption.pas`
  - `winssl.session.pas` 已被明确排除在 allowlist 之外

- 这让当前 WinSSL session-info probe 的仓库边界终于形成闭环：
  - shared path：保守 truth
  - compatibility shim：保守 fallback
  - dedicated proof：opt-in isolated worker
  - repo guard：禁止新的未隔离 probe 再偷偷长出来

- `26071754477` 现在给了这条 WinSSL 线目前最强的一组 live 证据：
  - `backend=winssl`
  - `handle_valid=true`
  - `dwLower/dwUpper` 非零
  - 仍然在 `stage=before_query_context_attributes` 之后立即 crash

- 这意味着我们已经可以把先前那种“也许 native handle 本身就是坏的”怀疑基本降到次要位置了：
  - 同一 worker 里：
    - `Supports(...)` 已通过
    - `GetNativeHandle` 已通过
    - `handle_valid=true`
    - 早先的 `ValidateReuseTruth(...)` 还成功跑过 `GetConnectionInfo` / `GetPerformanceMetrics`
  - 于是当前最合理的主结论已经很集中：
    - 触发 crash 的关键点就是 `SECPKG_ATTR_SESSION_INFO` 这条 query 本身
    - 而不是 generic WinSSL context 已经彻底失效

- 这轮还顺手把顶层路线图又往前推进了一点：
  - `26071754477` 的 `macos-gate` 这次已经是 `success`
  - 所以当前 Wave B/B2 手动 gate 的主要残留已经进一步收缩成 Windows native-probe 这一条
  - top-level reports 也与此重新对齐：
    - `linux=PASS`
    - `macos=PASS`
    - `windows=FAIL`

- 继续沿 WinSSL native-probe 做静态复核时，又挖出一条真正该立即收掉的实现漂移：
  - `src/fafafa.ssl.winssl.session.pas` 文档和 earlier plan 都把自己定位成 compatibility shim
  - 但这个 shim 里竟然还保留着一条未隔离的直接 `QueryContextAttributesW(...)` session-info probe

- 这条路径的问题不在“是否当前有人用到”，而在它和 repo 已经明确确立的 canonical truth 直接冲突：
  - 当前 risky session-info probe 只能存在于：
    - opt-in
    - isolated worker
    - experimental evidence lane
  - shim 如果继续私自做同一类 query，就等于给外部调用者留了一个未 quarantine 的绕行入口

- 因而这批最小正确动作也非常明确：
  - 不碰 shared handshake
  - 不重开 WinSSL probe 实现层
  - 只把 `winssl.session.pas` 拉回真正的 conservative compatibility shim：
    - pointer-based fallback session id
    - `reused=false`

- focused 结果说明这条静态风险已经被彻底拿掉：
  - 新增 safe-fallback contract 先 RED，直接命中 shim 内还存在的 `QueryContextAttributesW`
  - 修复后转 GREEN
  - 既有 `test_winssl_session_truth_source_contract.sh` 也继续 GREEN

- 这样一来，当前 repo 内关于 WinSSL session-info probe 的边界就重新干净了：
  - canonical shared path：保守 truth，不直接 probe
  - dedicated proof lane：opt-in isolated worker
  - compatibility shim：保守 fallback，不再私自 probe

- `26071188795` 的真实 summary artifact 现在已经把 closure-truth 这条流程修复彻底坐实了：
  - `closure readiness` 不再把 Windows 写成 `PASS`
  - `cross summary` / `handoff bundle` / `closure readiness`
    在 live GitHub run 上已经对齐
  - 所以 Wave B/B2 报告链这一刀，现在可以从“怀疑修好”升级成“真实 runner 上已证实修好”

- `26071361489` 的 Windows artifact 又把 WinSSL native-probe 这条技术线向前推进了一大步：
  - 当前 worker 已明确走过：
    - `stage=before_supports`
    - `stage=after_supports`
    - `stage=before_get_native_handle`
    - `stage=after_get_native_handle handle_nil=false`
    - `stage=before_query_context_attributes`
  - 然后才以 `exit_code=-1073741819` 退出

- 这条结果的含义已经非常强：
  - crash 不在 owner-surface `Supports(...)` 之前
  - crash 也不在 `GetNativeHandle` 之前
  - 当前第一嫌疑点已经收缩到：
    - `QueryContextAttributesW(SECPKG_ATTR_SESSION_INFO, ...)`
    - 或它刚被调用时依赖的句柄内容

- 但当前 `handle_nil=false` 仍然只是“指针地址非空”，不足以说明 `CtxtHandle` 内容本身有效：
  - `TWinSSLConnection.DoGetNativeHandle` 返回的是 `@FCtxtHandle`
  - `ISSLNativeHandleAccess` 还同时提供：
    - `GetBackendType`
    - `IsNativeHandleValid`
  - 所以下一批最小正确动作不是马上猜测 Schannel bug，而是把这层 metadata 一起打进 worker artifact

- 这也正是刚完成的 `handle metadata` 本地批次要解决的问题：
  - probe helper 现在会在 `before_query_context_attributes` 前额外输出：
    - `backend`
    - `handle_valid`
    - `dwLower`
    - `dwUpper`
  - 因而下一轮 Windows artifact 将能直接回答：
    - 是“WinSSL 自己也认为句柄有效，但调用 `SECPKG_ATTR_SESSION_INFO` 仍会崩”
    - 还是“句柄内容本身已经可疑”

- 在 closure truth 收口后，当前 WinSSL native-probe 这条线最高价值的残留已经进一步缩成“probe body 内部没有阶段性 marker”：
  - `pending=true` 只能证明已经进入 isolated worker
  - 但它无法区分 crash 是发生在：
    - `Supports(...)`
    - `GetNativeHandle`
    - 还是 `QueryContextAttributesW(...)`

- 因而这批最小正确动作也非常窄：
  - 不动 `src/fafafa.ssl.winssl.connection.pas`
  - 不改共享 reconnect/session-info 逻辑
  - 只在 `tests/winssl/test_winssl_session_resumption.pas` 的 `TryQueryNativeSessionReuse(...)` 内补齐 stage markers

- 这批本地收口后，当前 native probe 的 evidence 形状已经显著变强：
  - helper 现在显式接收 `label`
  - probe body 已经能区分：
    - `before_supports`
    - `after_supports`
    - `before_get_native_handle`
    - `after_get_native_handle`
    - `before_query_context_attributes`
    - `query_failed`
    - `after_query_context_attributes`
    - `exception`
  - 所以下一轮 Windows artifact 的 `last_marker` 一旦更新，就会直接指出 crash 的精确边界

- focused 结果说明这批边界没有带坏现有 worker quarantine truth：
  - 新增 stage-marker contract 先 RED 在 helper 还没有 label/stage markers
  - 修复后转 GREEN
  - 既有：
    - worker quarantine contract
    - session-resumption runtime-truth contract
    - Win64 cross compile
    都继续 GREEN

- 因而当前最合理的后续顺序已经很明确：
  1. 用 GitHub Windows runner 消费这批新 markers
  2. 看 `last_marker` 停在哪一层
  3. 再决定是否要把后续修复收缩到：
     - owner-surface boundary
     - native handle cast
     - `QueryContextAttributesW(...)` 调用边界

- 最新一轮 WinSSL native-probe worker 隔离 run `26070488337` 暴露的最高价值 residual，不再是 WinSSL 实现本身，而是 Wave B/B2 顶层 report chain 里 `closure_readiness` 的 truth 漂移：
  - `cross summary` 已能把
    - `windows summary overall=PASS`
    - `winssl_runtime_suite_<run_id>.log suite_end_status=FAIL`
    综合成 `windows | FAIL`
  - `handoff bundle` 也已落到 `NEEDS_GATE_REPAIR`
  - 但同一批 `closure_readiness` 之前仍写成 `windows | PASS`

- 这说明当前 repo 内部对同一条 Windows evidence 已出现“三层认为失败、一层仍声称通过”的裂缝：
  - 它不会掩盖 handoff 最终状态
  - 但会继续误导后续人工阅读 artifact，以为 closure 层已经闭环
  - 所以下一步最小正确动作不是继续深挖 WinSSL probe 实现，而是先把 closure 这层 truth 补齐

- 这批最小修法也因此非常窄：
  - 不改 `src/fafafa.ssl.winssl.connection.pas`
  - 不继续扩 native-probe body markers
  - 只让 `check_wave_b_b2_closure_readiness.sh` 接受 Windows runtime transcript，并在 `suite_end_status=FAIL` 时把 Windows state 降成 `FAIL`
  - 同时让 `prepare_wave_b_b2_handoff_bundle.sh` 显式透传 sibling transcript，避免 closure 层再次靠隐式路径漂移

- focused 结果说明这条流程裂缝已经被稳定锁住：
  - 新增 closure-focused RED 合同先直接命中：
    - `check_wave_b_b2_closure_readiness.sh`
    - `prepare_wave_b_b2_handoff_bundle.sh` 生成出来的 closure report
  - 修复后两条新合同转 GREEN
  - 既有：
    - `handoff bundle windows companion path`
    - `gate repair state`
    - `closure next actions`
    - `consistency explicit windows runtime logs`
    - `consistency runtime substantive`
    这些邻近合同也继续 GREEN

- 用真实 artifacts 复算 run `26070488337` 后，当前 canonical truth 已经明确：
  - `closure readiness`
    - `windows | FAIL | summary parsed; runtime_transcript: ... suite_end_status=FAIL`
    - `closure_status: IN_PROGRESS`
  - `handoff bundle`
    - `handoff_state: NEEDS_GATE_REPAIR`
    - `consistency_status: CONSISTENT`
  - 因而这批之后，四层报告终于不再彼此打架

- 这也让接下来的优先级重新收敛：
  - Wave B/B2 workflow truth 这一刀现在可以暂时视为闭环
  - 下一个真正值得继续深挖的技术批次，才是 WinSSL native-probe child 内部更细的 crash markers
  - 另外，run `26070488337` 的 macOS `FAIL` 仍是独立 residual，不应拿来解释 Windows native-probe worker 的 `-1073741819`

- host-override lane live 证明 `www.google.com` 仍然是 `observed_reuse=false / session_configured=true` 之后，下一步最值钱的就不再是继续改 workflow plumbing，而是把已有的 native-probe evidence 能力提升成 manual workflow 的显式 opt-in 调查入口。

- 这个缺口的现状也已经确认清楚：
  - `tests/winssl/test_winssl_session_resumption.pas` 早已支持 `FAFAFA_WINSSL_ENABLE_NATIVE_PROBE`
  - `tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh` 已经锁住：
    - native probe 必须保持 opt-in
    - broader suite 默认 lane 必须 disabled by default
  - 但 `wave-b-b2-manual.yml` 之前并没有把这个 opt-in 暴露成 `workflow_dispatch` 输入

- 因而这批最小正确动作不是碰生产实现，而是把 native-probe 证据能力提升成 manual workflow 的一等入口：
  - 增加 `winssl_enable_native_probe`
  - broader runtime step 只在 truthy 输入时设置 `FAFAFA_WINSSL_ENABLE_NATIVE_PROBE=1`
  - 显式记录：
    - enabled 时是 risky Schannel evidence lane
    - disabled 时仍走安全默认路径
  - 同时保持 workflow 不自动注入 `FAFAFA_WINSSL_REQUIRE_NATIVE_REUSE`

- focused 结果说明这批边界已经正确收住：
  - 新增 native-probe input contract 先 RED 在缺失输入
  - workflow / README 修复后转 GREEN
  - host-override contract、strict input description contract、artifact-download contract 都继续 GREEN
  - `test_winssl_session_resumption_runtime_truth_contract.sh` 继续 GREEN，说明这批没有破坏既有 opt-in truth

- 因而这批本地收口后的下一步非常明确：
  - push 到 GitHub
  - 派发一轮 `winssl_enable_native_probe=true` 的 Windows manual run
  - 让 artifact 直接回答：
    - native probe 有没有真正执行
    - 如果执行了，返回了什么 `SECPKG_ATTR_SESSION_INFO` truth
    - 如果没执行成功，失败点是否仍和旧的 public-handle crash 模式一致

- live dispatch 结果已经把这条 lane 从“能配”推进到“已实跑复现旧边界”：
  - pushed head: `ce602cb`
  - manual run: `26068984446`
  - URL: `https://github.com/dtamade/fafafa.ssl/actions/runs/26068984446`
  - GitHub step log 明确确认了这次输入真的生效：
    - `Using WinSSL session resumption host override: www.google.com`
    - `Enabling risky WinSSL native probe for Schannel session evidence`

- 但 Windows runtime transcript 的结果同样很明确：
  - 只来得及写出第一条 public signal：
    - `signal label=initial_handshake reused=false info_resumed=false perf_reused=false`
  - 还没来得及写任何 `native_probe ...` marker
  - 紧接着 `WinSSL Session Resumption Truth` 就以 `exit_code=-1073741819` 失败

- 这说明当前事实已经进一步收窄了：
  - host override 通道本身没问题
  - native probe opt-in 通道本身也没问题
  - 真正的问题仍然是 public-handle `SECPKG_ATTR_SESSION_INFO` probe 在 GitHub Windows runner 上的执行边界
  - 而且这次即使切到 `www.google.com`，失败形态也没有变化

- 因而现在不该再继续怀疑 host plumbing 或 workflow input wiring，而应把后续调查收缩到更窄的实现层问题：
  - 是否需要一个更安全的 WinSSL-specific probe seam
  - 是否应该绕开当前 `ISSLNativeHandleAccess.GetNativeHandle -> QueryContextAttributesW(...)` 路径

- 这轮 live run 还额外暴露出一条“读报告方式”的真相：
  - `wave_b_cross_platform_summary` 仍会显示 `windows | PASS`
  - `wave_b_b2_handoff_bundle` 仍可能是 `CLOSED`
  - `wave_b_b2_evidence_consistency` 也可能保持 `CONSISTENT`
  - 因为它们当前只把 Windows runtime transcript 视为“存在且含 suite_end marker 的 substantive evidence”，不会把 opt-in lane 的 `suite_end_status=FAIL` 自动抬成 top-level platform failure

- 所以对于这类 opt-in/risky investigation lane，权威证据顺序必须明确改成：
  1. GitHub run conclusion
  2. `winssl_runtime_suite_<run_id>.log`
  3. 然后才是 cross summary / consistency / handoff bundle

- 下一刀最自然的高价值方向因此有两个，但不该混在同一批：
  - A. 做更安全的 WinSSL-specific native probe seam
  - B. 单独收口 Wave B/B2 handoff reports 对 opt-in runtime-failure 的 truth presentation

- 当前 WinSSL session-resumption 这条线，普通 guide / benchmark wording 已经不是主问题；真正还会阻碍后续判断的一层，是 GitHub Actions manual lane 里还没有一个 repo 内建的“换 host 做真实调查”入口。

- 这个缺口的性质也已经确认清楚：
  - `tests/winssl/test_winssl_session_resumption.pas` 其实早就支持 `FAFAFA_WINSSL_SESSION_HOST`
  - `tests/run_winssl_tests.ps1` 也不会覆盖这个变量
  - 缺的是：
    - `wave-b-b2-manual.yml` 没有 `workflow_dispatch` 输入
    - `.github/README.md` 没有把这条调查通道记录成正式 workflow truth

- 因而这批最小正确动作不是改 WinSSL 生产实现，而是把 host-override 调查 lane 提升成 workflow 的一等入口：
  - 增加可选 `winssl_session_host`
  - Windows broader runtime step 只在输入非空时才注入 `FAFAFA_WINSSL_SESSION_HOST`
  - 默认留空时继续使用测试程序当前默认 host，不把手动调查 lane 变成默认风险面

- 这批 focused contract 也顺手压出一条真实 residual：
  - `tests/scripts/test_wave_b_b2_optional_runner_artifact_download_workflow_contract.sh`
    仍钉死 `actions/download-artifact@v4`
  - 但当前 workflow 真相已经是 pinned `download-artifact` v7
  - 这属于验证面本身的漂移，而不是 runtime 行为问题

- 所以这次除了补新 host-override contract，也顺手把旧 artifact-download contract 收回到 action-pinned truth，避免未来每次碰 workflow 都被假红噪音打断。

- focused 结果说明这批边界已经正确收住：
  - 新增 host-override contract 先 RED 在缺失输入
  - workflow / README 修复后转 GREEN
  - strict input description contract 继续 GREEN
  - artifact-download contract 在 truth 对齐后恢复 GREEN
  - `gh auth status` 继续 PASS，说明后续可以直接 dispatch GitHub runner 做真实 host 调查

- live dispatch 结果也已经把这条 lane 从“能配”推进到“已实跑”：
  - pushed head: `81eebb1`
  - manual run: `26068474291`
  - URL: `https://github.com/dtamade/fafafa.ssl/actions/runs/26068474291`
  - Windows runtime artifact 中明确出现：
    - `summary host=www.google.com`
    - `observed_reuse=false`
    - `session_configured=true`

- 这条 live 结果的含义很直接：
  - 新增 workflow 输入和 env 注入链路是通的
  - 非默认 host `www.google.com` 也没有把当前 WinSSL/Schannel reconnect 证据翻成 `observed_reuse=true`
  - 因而当前现象已经不再只是“默认 Cloudflare host 的偶然值”，而更像：
    - broader host-family behavior
    - 或 WinSSL/Schannel reconnect/native probe 语义本身

- 所以下一步如果继续沿这条线深入，不该再回头猜 workflow 或默认 host plumbing，而应：
  - 扩一两个不同 TLS/server family 的 public host 做 bounded 对照
  - 或单独打开 native probe lane，调查 `SECPKG_ATTR_SESSION_INFO` / ticket/reconnect 真相

- WinSSL session-resumption 这条线在 active guides 收口后，剩下最像“还会继续误导后续实现判断”的 residual，并不是普通文档，而是专项 benchmark 程序与 benchmark guide：
  - `tests/winssl/test_winssl_session_reuse_benchmark.pas`
  - `tests/winssl/SESSION_REUSE_BENCHMARK_GUIDE.md`

- 这批 residual 的问题也不只是 wording：
  - benchmark 程序还直接使用 `LConn.GetSession / SetSession / IsSessionReused`
  - benchmark guide 仍承诺 `70-90%` / “快速握手”
  - benchmark 程序本身还存在一条真实逻辑 bug：
    - `RunSessionReuseBenchmark` 先拿无 session 结果
    - 紧接着又整条覆盖成 with-session 结果
    - comparison report 实际拿不到完整双侧 metrics

- 因而这批最小正确动作也不是继续谈 WinSSL native 实现，而是先把 benchmark truth/harness 自身收口：
  - 程序切到 `ISSLSessionResumption`
  - 分开记录：
    - `session_configured`
    - `observed_reuse`
  - 指南明确写回当前 dedicated Windows CI truth：
    - `observed_reuse=false`
    - `session_configured=true`
  - 同时修掉 metrics 覆盖和除零风险

- focused 结果说明这批边界已经正确收住：
  - 新 contract 先 RED 在旧的高复用/高收益承诺
  - 修复后 contract 转绿
  - Win64 cross-target compile 继续 PASS
  - `git diff --check` 继续 PASS

- 这也让 WinSSL session 路线当前的状态更清楚了：
  - 普通 docs/guides truth 已经收口
  - benchmark residual truth 也不再继续夸大 native reuse 现状
  - 真正剩下的高风险问题已经继续收缩到：
    - native resumed-handshake / session tickets 行为本身
    - 而不是 owner-path guidance、benchmark wording 或 harness 统计 bug

- 因而下一刀若继续沿 WinSSL session 路线推进，就不应再回头清 guide/benchmark wording，而应直接进入 native resumed-handshake / session tickets 行为调查。

- `ISSLSessionResumption` 之前虽然已经在 `API_REFERENCE` / `API_DOCUMENTATION` / `INTEGRATION_GUIDE` / generic E2E 场景收过一轮 ordinary guidance，但这次重新审 active guides 仍压出了一组高可见漏网：
  - `docs/guides/QUICKSTART.md`
  - `docs/guides/TROUBLESHOOTING.md`
  - `docs/guides/USER_GUIDE.md`

- 这组漏网点的共同特征也很明确：
  - 不是实现缺口
  - 不是 backend contract 缺口
  - 而是高可见 guide 仍在教学旧的 `GetSessionID` / `IsSessionResumed` / direct connection-core `GetSession` / `SetSession`

- 因而这批最小正确动作不是改生产逻辑，而是做一轮 session-resumption guide old-name truth freeze：
  - 新增 focused source contract，锁住这 3 份 guide 不再回退到旧名字与 direct connection-core 路径
  - `QUICKSTART` 的 Session 保存/恢复/复用示例切回 `Supports(..., ISSLSessionResumption, ...)`
  - `TROUBLESHOOTING` 的 WinSSL 复用排障 / 性能示例切回 owner path
  - `USER_GUIDE` 的性能优化示例也不再继续教 `IsSessionResumed`

- focused 结果说明这批边界已经正确收住：
  - 新 contract 直接 PASS
  - `git diff --check` 继续 PASS
  - 这说明当前剩余问题确实只是 guide truth 漂移，而不是更深层实现反弹

- 进一步的 focused residual scan 也把这条线当前剩余面压得很窄：
  - active guides 里的 `GetSessionID` / `IsSessionResumed` 已清空
  - repo 内剩余旧名字主要只在：
    - `docs/reference/API_REFERENCE.md` 的历史/兼容性说明
    - `tests/winssl/SESSION_REUSE_BENCHMARK_GUIDE.md` 的 WinSSL 专项 benchmark 文档
    - 以及 contract / plan / progress 等台账文件

- 因而 session-resumption 这条 ordinary-guide 路线现在可以视为基本关闭：
  - 不应再把 `QUICKSTART` / `TROUBLESHOOTING` / `USER_GUIDE` 当成未收口问题反复拉起
  - 如果继续沿这条路线收尾，下一刀最自然的是 WinSSL benchmark guide
  - 如果回到更高价值主线，则应继续 backend completeness / backend-specific runtime truth 审查

- 当前最容易把新读者直接带回旧 public surface 的，并不是 runtime 实现，而是 highest-visibility main-entry truth source：
  - `docs/README.md`
  - `src/fafafa.ssl.pas`
  - `src/fafafa.ssl.factory.pas`
  - `docs/guides/INTEGRATION_GUIDE.md`

- 这批文件之前的漂移形态也已经明确：
  - `README` 仍教 `uses fafafa.ssl.factory, fafafa.ssl.base;`
  - `README` / `INTEGRATION_GUIDE` / `factory` 注释仍使用旧的 `sslClient`
  - `src/fafafa.ssl.pas` 顶部示例还停留在 context-only 路径，没有体现当前 facade connector 主入口

- 因而这批最小正确动作不是改实现逻辑，而是做 facade / main-entry truth freeze：
  - 新增 focused source contract，锁住这 4 个高可见入口文件不再回退到 `sslClient` / `sslServer`
  - `README` 切到 `uses fafafa.ssl` + `TSSLConnector.FromContext(...)`
  - direct 路径继续明确 `ISSLClientConnection.SetServerName(...)` 是连接级 SNI/hostname 真相
  - `factory` 注释与 `INTEGRATION_GUIDE` 统一对齐到 `sslCtxClient` / `sslCtxServer`

- focused 结果也说明这批边界已经正确收住：
  - 新 contract 先 RED 在 `docs/README.md`
  - 修复后 contract 转绿
  - `git diff --check` 继续 PASS

- 这样一来，当前 public 最显眼的一层入口真相已经统一：
  - main facade entry = `uses fafafa.ssl`
  - recommended client path = `TSSLConnector.FromContext(...)`
  - direct per-connection SNI truth = `ISSLClientConnection.SetServerName(...)`
  - context enum truth = `sslCtxClient` / `sslCtxServer`

- 因而下一刀不应再回头重扫 `sslClient` / split-unit main-entry 漂移，而更适合转去 session-resumption 旧命名文档组：
  - `docs/guides/QUICKSTART.md`
  - `docs/guides/TROUBLESHOOTING.md`
  - `docs/guides/USER_GUIDE.md`

- 当前剩余的 root-level verify-result 命中也已经被证实不是“普通入口漏改”，而是一组 runtime / backend-contract residual subgroup：
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

- 这组文件虽然都在 `tests/*.pas` 根层，但实际语义已经很清楚：
  - FreePascal runtime contracts
  - OpenSSL / WolfSSL / MbedTLS backend framework or verify-result contracts
  - 所以最小正确动作同样不是改 owner path，而是做 root-test residual subgroup freeze

- 这批最小安全收口也因此落在“写明保留原因 + 锁住 file set”：
  - 在当前尚未标注的 root-test residual 文件里补 `INTENTIONAL_VERIFY_RESULT_CORE_SURFACE`
  - 新增 focused source contract，锁住当前 `tests/*.pas` verify-result residual file set
  - 并继续要求每个文件保留各自预期的 verify-result coverage

- 到这里，`ISSLCertificateVerification` 这条 residual 路线已经基本完成了剩余面分类：
  - WinSSL runtime trio
  - MbedTLS residual cluster
  - OpenSSL/WolfSSL OCSP runtime duo
  - root-test runtime / backend-contract subgroup
  - 后续更应该把注意力从“verify-result residual archaeology”切回更大的接口设计 / backend completeness 审查

- `OpenSSL` / `WolfSSL` 这边剩余的 verify-result 命中并没有散开，而是已经压成一对很窄的 server-side OCSP stapling runtime diagnostics：
  - `tests/openssl/test_openssl_server_ocsp_stapling_runtime.pas`
  - `tests/wolfssl/test_wolfssl_server_ocsp_stapling_runtime.pas`

- 这对 residual duo 的性质也很明确：
  - 它们都属于 backend-specific server-side stapling diagnostics
  - 不是 ordinary docs、generic examples 或 generic tests
  - 所以最小正确动作同样不是改 owner path，而是做 diagnostics subgroup freeze

- 这批收口因此也落在“写明保留原因 + 锁住 duo 文件集”：
  - 两个文件都补 `INTENTIONAL_VERIFY_RESULT_CORE_SURFACE`
  - 新增 focused source contract，锁住当前 `tests/openssl` / `tests/wolfssl` 的 verify-result residual 面恰好只等于这两个文件
  - 同时区分它们各自应继续保留的 diagnostics 覆盖：
    - OpenSSL: `GetVerifyResultString`
    - WolfSSL: `GetVerifyResult` + `GetVerifyResultString`

- 到这里，`ISSLCertificateVerification` 残余面已经进一步缩成：
  - 已冻结的 WinSSL runtime trio
  - 已冻结的 MbedTLS residual cluster
  - 已冻结的 OpenSSL/WolfSSL OCSP runtime duo
  - 剩下主要是 root-test residual subgroup，更适合作为下一刀继续收

- `MbedTLS` 这组 verify-result residual 命中和 WinSSL trio 不同，不是 3 个文件，而是一个完整 backend-specific cluster：
  - `tests/mbedtls/benchmark_handshake_simple.pas`
  - `tests/mbedtls/test_mbedtls_safe.pas`
  - `tests/mbedtls/test_mbedtls_simple_connection.pas`
  - `tests/mbedtls/test_mbedtls_lowlevel.pas`
  - `tests/mbedtls/test_mbedtls_cert_chain.pas`
  - `tests/mbedtls/test_mbedtls_cert_errors.pas`
  - `tests/mbedtls/test_mbedtls_cert_verify_flags.pas`
  - `tests/test_mbedtls_framework.pas`

- 这批文件的共同点也已经很清楚：
  - 它们都属于 backend-specific benchmark / runtime diagnostics / framework contract
  - 它们不是 ordinary docs，也不是 generic examples，更不是 generic tests
  - 所以这里最小正确动作不是把它们再改成 owner path，而是像 WinSSL 一样做 residual subgroup freeze

- 这批最小安全收口也因此落在“写明保留原因 + 锁住文件集”，而不是“改行为”：
  - 在 8 个文件中统一补 `INTENTIONAL_VERIFY_RESULT_CORE_SURFACE`
  - 新增 focused source contract，锁住这 8 个文件就是当前全部 MbedTLS verify-result residual cluster
  - 同时要求这些文件继续保留各自预期的 direct core verify-result coverage

- 这样一来，`ISSLCertificateVerification` 这条线现在已经不仅完成 broad residual allowlist，还进一步把两个 backend-specific subgroup 固化了：
  - WinSSL online certificate-error trio
  - MbedTLS benchmark / runtime / framework cluster
  - 下一刀更适合继续 root-test / OpenSSL / WolfSSL 剩余 subgroup，而不是再重扫这两组 backend residual

- `ISSLCertificateVerification` 的 broad residual allowlist 虽然已经把 `tests/winssl/` 收缩到只剩 3 个 `GetVerifyResult*` 命中，但如果不把这 3 个文件再单独标成 intentional proof，它们看起来仍会像 accidental drift：
  - `tests/winssl/test_winssl_error_mapping_online.pas`
  - `tests/winssl/test_winssl_hostname_mismatch_online.pas`
  - `tests/winssl/test_winssl_revocation_online.pas`

- 这 3 个文件的真实定位也已经进一步说清楚了：
  - 它们不是 ordinary docs、也不是 generic examples、更不是 generic tests
  - 它们都是 WinSSL-specific online certificate-error runtime proof
  - 它们保留 direct core `GetVerifyResult` / `GetVerifyResultString` 的意义，是继续盯住 compatibility mirror 在在线错误映射场景下的 runtime truth

- 因而这批最小安全收口并不是“再做一次 owner-path 改写”，而是把 residual 用意固定下来：
  - 在 3 个文件的 direct core 读取点前补 `INTENTIONAL_VERIFY_RESULT_CORE_SURFACE`
  - 新增 focused source contract，锁住 `tests/winssl/` 当前 direct-core verify-result file set 恰好只等于这 3 个文件
  - 并且每个文件都必须继续带意图注释、继续覆盖 `GetVerifyResult` 与 `GetVerifyResultString`

- 这样一来，`ISSLCertificateVerification` 这条线就不仅有 broad residual allowlist，也有 WinSSL residual subgroup 的专门 freeze：
  - 后续不应再把这 3 个 WinSSL 文件当作 generic guidance 漂移反复拉起
  - 下一刀更适合直接转向 `MbedTLS` residual cluster

- generic examples 收口后，`ISSLCertificateVerification` 这条线当前真正剩下的已经不是“还有没有普通入口直读 core”，而是 residual direct-core file set 还没有被正式冻结：
  - active docs 已经是 owner path
  - generic examples / `tests/examples` 已经是 owner path
  - 但如果不把 residual allowlist 固化下来，后续完全可能又有新的 direct-core `GetVerifyResult` / `GetVerifyResultString` 文件悄悄混进来

- 这批最小安全收口因此很明确，并已落地：
  - 在 `src/fafafa.ssl.base.pas` 给 `GetVerifyResult` / `GetVerifyResultString` 补 preferred-access 与 owner note
  - 在 `src/fafafa.ssl.connection.base.pas` 明确 shared mirror implementation 的 residual-surface truth
  - 新增 focused source contract，把当前 direct-core surface freeze 成稳定 allowlist

- 这次 allowlist 也把 residual 分类真正说清楚了：
  - active docs direct-core file set = `0`
  - `examples/` 只剩 `examples/fafafa.examples.tcp.pas` 共享 helper fallback
  - `tests/examples/` direct-core file set = `0`
  - `tests/connection/` 只剩 `tests/connection/test_ssl_client_connection.pas` 本地 helper fallback
  - `tests/contract/` 只剩 `tests/contract/test_backend_contract.pas` 的 optional/core mirror proof
  - 其余 direct-core 命中全部属于 backend-specific runtime / contract residual proof

- 这也说明当前 `ISSLCertificateVerification` 路线的状态已经和之前 `GetStateString` / `GetSelectedALPNProtocol` 很接近：
  - ordinary guidance 已经切完
  - generic examples 已经切完
  - residual surface 也已经 freeze
  - 后续再重复扫同一批 `GetVerifyResult*` 命中的收益已经很低

- 因而这条 certificate-verification lane 现在可以视为阶段性关闭：
  - 后续不应再把“ordinary/generic 路径是否还在教 direct core verify getters”当成未完成问题反复拉起
  - 下一刀更适合继续 backend-specific runtime / residual deprecation lane，或者切回更大的 interface-design / backend completeness seam

- 在继续追 `verify-result mirrors` 的残余入口时，这次最值得优先收的已经不是 docs，也不是 high-visibility facade，而是 generic examples / 通用测试示例：
  - `examples/01_tls_client.pas`
  - `examples/example_https_api.pas`
  - `examples/production/https_client_auth.pas`
  - `examples/validation/real_world_test.pas`
  - `tests/examples/test_openssl.pas`
  - `tests/examples/test_real_websites*.pas`
  - `tests/connection/test_ssl_client_connection.pas`
  - 这些文件都更像“开发者会直接照着写”的入口，比 backend-specific runtime test 更容易把 verify-result core getters 再教回去

- 这批最小安全收口也已经落地：
  - 在 `examples/fafafa.examples.tcp.pas` 增加共享 `GetCertificateVerificationInfo(...)`
  - helper 优先走 `ISSLCertificateVerification`
  - 只有 optional owner interface 不可用时，才回退 core `GetVerifyResult` / `GetVerifyResultString`
  - `tests/connection/test_ssl_client_connection.pas` 因为不依赖 examples helper，所以保留同名本地 helper

- 这次 target compile 还顺手压出了两个之前没被正式记下来的 compile-liveness 真相：
  - `tests/examples/test_real_websites.pas`、`test_real_websites_enhanced.pas`、`test_real_websites_comprehensive.pas`
    原本都还写着 FPC 不接受的 `try..except..finally` 结构
  - `tests/connection/test_ssl_client_connection.pas`
    也还停留在旧的 `ssockets` / native-handle API 预期：
    - 把 `TInetSocket.Connect` 当返回布尔值
    - 把 `TSocketStream.Create` 当作接收整个 socket 对象
    - 把 native handle 当 `ISSLConnection` 核心方法而不是 helper 获取

- 这也说明当前问题不只是“guidance 顺序不优雅”，而是：
  - generic examples/tests 如果不重新编译验证，连 compile-liveness 都不一定还成立
  - 所以这批必须同时做 owner-path 收口和目标编译，而不能只做 source grep

  - focused 结果说明这批边界已经正确收住：
  - source contract 已 green
  - 9 个目标程序 compile 全绿
  - 现在 `examples` / `tests/examples` / `tests/connection` 下的 direct verify-result 命中已只剩 helper 本身

- 因而 generic examples / tests 这条 verify-result guidance lane 现在可以视为关闭：
  - 后续不应再把“generic examples 还在直读 core verify getters”当作未完成问题反复拉起
  - 下一刀更适合继续盘点 backend-specific runtime / residual deprecation lane

- `tests/contract/test_backend_contract.pas` 的 `Contract 21` 之前虽然已经锁住：
  - `GetVerifyResult`
  - `GetVerifyResultString`
  - peer-chain length / nilness / subject / issuer / serial
  - 但它还没有真正把 `GetPeerCertificateChain()[i].GetIssuerCertificate()` 这层 issuer-link truth 纳入统一 backend contract

- 这个缺口的重要性不在于“又找到一个新 bug”，而在于：
  - 我们前面已经分别在 `FreePascal` / `OpenSSL` / `WolfSSL` / `MbedTLS` / `WinSSL` 修掉过 peer-cert issuer-link completeness
  - 但如果统一 backend contract 不锁这层 truth，后续这些修复仍主要依赖 focused tests 存活
  - 一旦有人改 optional/core wiring，repo-level contract 并不会第一时间报警

- 这批最小安全收口因此很明确，并已落地：
  - 在 `Contract 21` 的 chain loop 里追加 issuer-link nil/non-nil 对齐断言
  - issuer-link 存在时，再比较 issuer cert 的 public identity（subject / issuer / serial）
  - 这让 `ISSLCertificateVerification.GetPeerCertificateChain()` 与 core getter 的对齐范围，从“entry 表层字段一致”扩大到“entry 间 link truth 也一致”

- 更关键的是，这次 contract 补强后没有炸出新的 backend 红点：
  - `tests/contract/test_backend_contract.pas` 继续 green：`135 total / 111 passed / 0 failed / 24 skipped`
  - 这说明前面分 backend 收掉的 issuer-link completeness 并不是局部测试偶然转绿
  - 它们现在已经通过了统一 optional/core alignment contract 的回归

- 因而 peer-cert / certificate-verification issuer-link 这条 lane 现在可以视为真正关闭：
  - 后续不应再把“issuer-link truth 有没有进入统一 backend contract”当成未完成问题反复拉起
  - 下一刀更适合回到更大的 verification / optional surface completeness 审查，或者继续盘点 verify-result mirrors 的 residual runtime/core uses

- `ISSLCertificateVerification` 这条线虽然前面已经完成了 ordinary docs/tests 的 owner-path de-emphasis，但这次又压出一条更贴近真实入口的残余：
  - `src/fafafa.ssl.connection.builder.pas` 仍在 client/server handshake failure path 直接读 core `GetVerifyResult / GetVerifyResultString`
  - `src/fafafa.ssl.tls.pas` 的 connector/acceptor 也还是同样的 direct core 读取
  - `docs/guides/OCSP_USAGE_GUIDE.md` / `docs/guides/CT_IMPLEMENTATION_GUIDE.md` 这两份高可见指南也还在教学 direct core verify-result mirrors

- 这说明当前缺的已经不是 owner truth 有没有，而是高可见入口有没有真正切过去：
  - contract 21 早就证明 `ISSLCertificateVerification` owner interface 存在且与 core 自洽
  - 但如果 builder / facade / 高可见指南仍回头读 core，后续就没法自然进入 compiler-deprecated 收口

- 因而这批最小安全修法也很明确，并已落地：
  - builder / TLS facade 新增本地 owner-path helper
  - 在 `ISSLCertificateVerification` 可用时，优先读 owner `GetVerifyResult / GetVerifyResultString`
  - 只有 owner interface 不可用时，才回退到现有 core mirror
  - OCSP / CT 高可见指南示例也同步改成先 capability-gate `ISSLCertificateVerification`

- 这批 focused contract 还顺手压出一个有价值的小真相：
  - source contract 若直接用子串匹配 `VerifyRes := ...`
  - 会把 helper 里的 `AVerifyRes := ...` 误判成 direct core 旧路径
  - 当前已把 shell contract 收紧到真正的 token-boundary 匹配，避免后续再被这种误报反复拉起

- focused 回归结果说明这次修法边界正确：
  - `tests/scripts/test_isslcertificateverification_active_guidance_contract.sh` 已覆盖 builder / TLS facade / OCSP guide / CT guide，并转绿
  - `tests/test_connection_builder_hostname_precedence.pas`: `29 passed / 0 failed`
  - `tests/test_tls_connector_hostname_override_precedence.pas`: `6 passed / 0 failed`
  - `tests/contract/test_backend_contract.pas` 继续 green：`135 total / 111 passed / 0 failed / 24 skipped`
  - 这说明这次不是“为 owner path 重写行为逻辑”，而是单纯把高可见默认入口对齐到既有 owner surface

- 当前这条 high-visibility owner-path lane 已可以视为关闭：
  - 后续继续审查时，不应再把 builder / TLS facade / CT/OCSP guide 当成 certificate-verification 的 direct-core 漂移点反复拉起
  - 下一刀更适合继续盘点 verify-result mirrors 的 residual runtime/core uses，准备进入 compiler-deprecated 收口

- `MbedTLS` 连接态 peer-certificate public surface 这次被证实还留着一条更窄但更真实的 completeness seam：
  - 之前这条线虽然已经修过 borrowed-cert materialization
  - 但 `DoGetPeerCertificateChain()` 仍然只返回一个 leaf clone
  - `DoGetPeerCertificate()` 与 returned chain leaf 因而都拿不到 `GetIssuerCertificate()` truth
  - 这说明问题不只是“link 没补”，而是 public chain truth 直接被截断了

- 这条问题也已经通过 focused contract 的明确 RED 锁实，而不是阅读代码猜测：
  - `GetPeerCertificate should preserve issuer link`
  - `GetPeerCertificateChain should expose the peer leaf and issuer`
  - `GetPeerCertificateChain leaf should preserve issuer link`
  - 这几条都在同一个本地 Linux focused run 上先红了

- 更关键的是，这次 static/runtime truth 已经压出真正原因：
  - 系统头文件里的 `mbedtls_x509_crt` 本来就带 `next` 链
  - 但我们当前 Pascal connection layer 根本没有走这条 native chain link
  - 所以 current public surface 才会退化成“只拿 leaf”

- 因而这批最小安全修法也已经明确并落地：
  - connection layer 新增 native peer-chain materialization helper
  - 在支持的 64-bit MbedTLS 3.x ABI 上读取 `mbedtls_x509_crt.next`
  - 逐个 cert materialize owned copies，并按顺序补回 issuer link
  - 既有 fail-closed 边界保持不变：copy helper 不可用时，`GetPeerCertificate()` 返回 `nil`，chain 返回空数组

- focused 回归结果说明这次修法边界正确：
  - `tests/test_mbedtls_connection_peer_certificate_contract.pas` 已从 `7 failed` 转成 `14 passed / 0 failed`
  - `tests/contract/test_backend_contract.pas` 继续 green：`135 total / 111 passed / 0 failed / 24 skipped`
  - 这说明这次不是“为了补 MbedTLS chain 引入新的 materialization 回归”，而是单纯补齐了 connection-level chain truth

- 当前 peer-certificate issuer-link 这条横向 completeness lane 现在已经可以视为全部关闭：
  - `FreePascal` / `OpenSSL` / `WolfSSL` / `MbedTLS` / `WinSSL` 都已有 focused evidence
  - 下一刀更适合继续横向审剩余 verification / optional surface completeness seam
  - 不应再把 `MbedTLS` peer chain truncation 当成未定位问题反复拉起

- 这次新的 focused contract 又压出了一条跨 backend 的 certificate-object completeness 漏口：
  - `OpenSSL` / `WolfSSL` / `MbedTLS` / `WinSSL` 的 `ISSLCertificate.Clone()` 此前都能保留 leaf 本体
  - 但 clone 后会把 `GetIssuerCertificate()` 这层 link truth 丢掉
  - `FreePascal` 已经是正确语义参考：`Clone()` 会继续保留 `FIssuerCert`

- 这条问题已经通过本地可重复的双平台 focused 证据锁实，而不是静态猜测：
  - Linux focused contract 先红在：
    - `OpenSSL: clone should preserve issuer link`
    - `WolfSSL: clone should preserve issuer link`
    - `MbedTLS: clone should preserve issuer link`
  - `Win64 cross-target + wine` focused contract 先红在：
    - `WinSSL: clone should preserve issuer link`

- 这也把问题边界说得很清楚：
  - 不是 leaf materialization 又坏了
  - 不是 fingerprint / DER / PEM copy 退化了
  - 而是 clone 路径没有把已有的 `FIssuerCert` 一起带过去

- 因而这批最小安全修法也很直接，并已落地：
  - `OpenSSL` clone 在 retained wrapper 建好后补回 `FIssuerCert`
  - `WolfSSL` / `MbedTLS` clone 在 materialized copy 成功后补回 `FIssuerCert`
  - `WinSSL` clone 在 duplicated cert context wrapper 建好后补回 `FIssuerCert`
  - 全部对齐 `FreePascal` 现有 clone 语义，不额外发明新的 deep-clone 政策

- focused 回归结果说明这次修法边界正确：
  - Linux focused contract 已从 RED 转 GREEN：`16 passed / 0 failed`
  - `Win64 cross-target + wine` focused contract 已从 RED 转 GREEN：`8 passed / 0 failed / 3 skipped`
  - `tests/contract/test_backend_contract.pas` 继续 green：`135 total / 111 passed / 0 failed / 24 skipped`
  - 这说明这次不是“为了补 clone link 改坏 certificate/session surface”，而是单纯补齐了 clone semantics

- 当前这条 clone issuer-link lane 已经可以视为关闭：
  - 后续继续审查时，不应再把它当成未定位问题反复拉起
  - 下一刀更适合继续横向审剩余 certificate-verification / optional surface completeness seam
- `WinSSL` 连接态 peer-certificate public surface 这次也被证实存在同类 completeness 缺口：
  - `DoGetPeerCertificate()` 之前只把 remote leaf context 包成 `ISSLCertificate`
  - `DoGetPeerCertificateChain()` 之前只把 `CertGetCertificateChain(...)` 产物 materialize 成 cert array
  - 但两条路径此前都没有把 `ISSLCertificate.GetIssuerCertificate()` 链接起来

- 这次不是只靠静态猜测，而是走通了本地可重复的 WinSSL runtime RED：
  - 先确认本机有 `Win64` cross-target 与 `wine`
  - 新增 focused test `tests/winssl/test_winssl_peer_certificate_surface.pas`
  - `FAFAFA_RUN_NETWORK_TESTS=1 FAFAFA_WINSSL_PEER_CERT_HOST=api.github.com wine ...`
    第一处失败直接落在：
    - `peer leaf certificate should preserve issuer link`
    - `peer chain leaf entry should preserve issuer link`

- 这也把 WinSSL 这条线的性质说清楚了：
  - 当前不是 session/runtime capture/Windows workflow 老问题
  - 握手、leaf 暴露、chain 暴露都已经成立
  - 真正缺的是 public `issuer-link truth`

- 这批最小安全修法也已经明确并落地：
  - 在 `src/fafafa.ssl.winssl.connection.pas` 增加本地 issuer-lookup/link helper
  - `GetPeerCertificate()` 现在会从 returned chain 中补 leaf issuer link
  - `GetPeerCertificateChain()` 现在会给 returned chain entries 按 subject/issuer truth 接上 issuer link
  - `tests/run_winssl_tests.ps1` 现在会把这条 surface 测试纳入 broader WinSSL runtime suite，避免后续再脱离主证据链

- focused 回归结果说明这次修法边界正确：
  - 本地 Win64 runtime focused test 已从 RED 转 GREEN
  - `tests/contract/test_backend_contract.pas` 继续 green
  - 这说明这次不是“为了补 issuer link 改坏别的 surface”，而是单纯补齐了 WinSSL public cert surface

- `WolfSSL` 连接态 peer-certificate public surface 这次也被证实存在一条真实 completeness 缺口：
  - `DoGetPeerCertificate()` 之前只把 native peer cert materialize 成 owned copy
  - `DoGetPeerCertificateChain()` 之前只把 native chain materialize 成 owned cert array
  - 但两条路径此前都没有把 `ISSLCertificate.GetIssuerCertificate()` 链接起来

- 新增 focused RED 后，第一处失败直接落在：
  - `WolfSSL peer leaf certificate should preserve issuer link`
  - 这说明问题不是“只缺测试”，而是 public chain truth 真少了一层

- 这批最小安全修法也已经明确并落地：
  - 在 `src/fafafa.ssl.wolfssl.connection.pas` 增加本地 materialize/link helper
  - `GetPeerCertificate()` 现在会在 chain 可用时给 leaf cert 补 issuer link
  - `GetPeerCertificateChain()` 现在会用 subject/issuer truth 给 returned chain entries 接上 issuer link
  - 既有 materialization / safe-degrade contract 没有被放宽或改口

- 这也把 `WolfSSL` 这条线的性质进一步说清楚了：
  - `GetPeerCertificate()` 的 owned-copy materialization 问题前一批已经修掉
  - 当前剩下的不是 lifetime，而是 public `issuer-link truth`
  - 所以这批是 connection-level chain completeness 收口，而不是重新修 clone/ownership

- focused 回归结果说明这次修法边界正确：
  - `tests/test_wolfssl_connection_peer_certificate_contract.pas` 继续 green
  - `tests/test_wolfssl_framework.pas` 继续 green
  - `tests/contract/test_backend_contract.pas` 继续 green
  - 这说明这次不是“为了补 issuer link 换来新的 materialization 回归”，而是单纯补全了 WolfSSL public cert surface

- `OpenSSL` 连接态 peer-certificate public surface 这次被证实存在一条真实 completeness 缺口：
  - `DoGetPeerCertificate()` 只包 `SSL_get_peer_certificate(...)` 返回的 leaf wrapper
  - `DoGetPeerCertificateChain()` 只包 `SSL_get_peer_cert_chain(...)` 返回的 chain wrappers
  - 但两条路径此前都没有把 `ISSLCertificate.GetIssuerCertificate()` 链接起来

- 新增 focused RED `tests/test_openssl_connection_peer_certificate_surface.pas` 后，第一处失败直接落在：
  - `OpenSSL peer leaf certificate should preserve issuer link`
  - 这说明问题不是“测试猜测”，而是 public chain truth 真缺了一层

- 这批最小安全修法也已经明确并落地：
  - 在 `src/fafafa.ssl.openssl.connection.pas` 增加 retained-certificate helper
  - `GetPeerCertificate()` 现在会从 peer chain 优先、verified chain 次级地补 issuer link
  - `GetPeerCertificateChain()` 现在会用现有 `FindIssuerX509InChain(...)` 给 returned chain entries 接上 issuer link
  - 既有 safe-degrade contract 没有被放宽或改口

- 这也把 `OpenSSL` 这条线的性质说清楚了：
  - 它不是 `MbedTLS` / `WolfSSL` 那种 borrowed-lifetime materialization 问题
  - `SSL_get_peer_certificate(...)` 本身已经给 leaf 做了安全 ownership
  - 真正缺的是 public `issuer-link truth`

- 这次 focused contract 还暴露出一个值得记下来的测试夹具细节：
  - 首轮 `GetPeerCertificateChain()` 返回空数组不是产品逻辑回归，而是 harness 没桥接 `sk_X509_num/value`
  - 在部分 OpenSSL 构建上，typed `sk_X509_*` 仍可能退回到 generic `OPENSSL_sk_*` 路径
  - focused test 现在已显式桥接这层 ABI 真相，避免未来把夹具问题误判成实现回归

- focused 回归结果也说明这次修法边界正确：
  - `tests/test_openssl_connection_peer_certificate_contract.pas` 继续 green
  - `tests/test_openssl_connection_peer_certificate_chain_contract.pas` 继续 green
  - `tests/contract/test_backend_contract.pas` 继续 green
  - 所以这次不是“用更激进的 chain 逻辑换来新回归”，而是单纯补全了 OpenSSL public cert surface

## 2026-05-18

- GitHub Actions live run `26030261335` 已给出一个很重要的新事实：
  - broader WinSSL suite 并不是“根本没跑”
  - Windows job console log 里实际有 6 个 suite 的编译/运行/汇总输出
  - 弱的是 artifact，不是执行本身

- 新的 live rerun `26031191987` 也已经把这条怀疑真正打实：
  - 新 artifact `winssl_runtime_suite_wave_b_b2_20260518_193941_evidence_fix.log` 直接保存了 broader suite 的编译、逐项执行、汇总和 `[WINSSL-RUNTIME]` markers
  - summary job 生成的 `wave_b_b2_evidence_consistency_...md` 已把 Windows runtime log 记成 `substantive runtime evidence; suite_end_status=PASS`
  - 这说明本批修复已经把“CI 控制台真跑但 artifact 丢真相”的流程缺口真正堵上了

- 当前 WinSSL / MbedTLS session-resumption lane 又暴露出一条更像真 bug 的语义偏差：
  - `src/fafafa.ssl.winssl.connection.pas` 之前在 `DoSetSession(...)` 里执行 `FCurrentSession := ASession` 后立刻把 `FSessionReused := True`
  - `src/fafafa.ssl.mbedtls.connection.pas` 之前在 `mbedtls_ssl_set_session(...) = 0` 成功后，也立刻把 `FSessionReused := True`
  - 但 `docs/reference/API_REFERENCE.md` 与通用 E2E 场景都把 `IsSessionReused` 定义成“握手后是否实际命中了恢复路径”，不是“是否曾配置 session”

- 交叉对照进一步证明这不是设计口味问题，而是实现真相漂移：
  - `OpenSSL` 的 `DoIsSessionReused` 继续直接读 `SSL_session_reused`
  - `WolfSSL` 的 `DoIsSessionReused` 继续直接读 `wolfSSL_session_reused`
  - `FreePascal` 的 `DoSetSession(...)` 会先清空 `FSessionReused`，只在真实恢复路径命中后再翻成 `True`
  - `tests/winssl/test_winssl_session_resumption.pas` 也明确写着 `true resumption尚未接入`

- 因而本批最小安全修法已经明确：
  - `SetSession(...)` 只能表示“配置了待恢复 session”
  - `IsSessionReused` / `GetConnectionInfo.IsResumed` 只能表示“当前握手的实际结果”
  - 在真正的 WinSSL Schannel resumption runtime proof 做出来之前，至少不能再让 `DoSetSession(...)` 提前误报 `True`

- 新增的 focused contracts 也把这个结论钉成了可重复证据，而不只是人工阅读判断：
  - `tests/scripts/test_session_reused_semantic_truth_contract.sh` 先以源码合同形式直接抓到 WinSSL / MbedTLS 的 preclaim 行为
  - `tests/test_mbedtls_connection_session_reused_contract.pas` 用 fake `mbedtls_ssl_set_session(...) = 0` 成功返回，先红后绿地证明：
    - “native helper 成功执行”
    - 不等于 “当前握手已经 resumed”

- 这也把 WinSSL session-resumption lane 的剩余真问题压缩得更清楚了：
  - workflow capture / artifact evidence：已闭环
  - `IsSessionReused` semantic false positive：已修复
  - 真正剩下的高风险问题：Windows 上实际 resumed handshake / session tickets 行为是否成立，以及如何给出 live runtime proof

- 在继续往前推 WinSSL runtime proof 时，又发现了一条更底层的实现缺口：
  - canonical `src/fafafa.ssl.winssl.connection.pas` 虽然维护 `FSessionReused` / `FCurrentSession`
  - 但 client `DoConnect(...)` 成功后此前并不会调用 `SaveSessionAfterHandshake`
  - 因此 client path 上的 session metadata 主要依赖 `DoGetSession()` 事后临时拼装，不是真正的 post-handshake 保存

- 还有一条更关键的 source drift 也被证实了：
  - `src/fafafa.ssl.winssl.session.pas` 这个兼容 shim 里早就有 `QueryContextAttributesW(..., SECPKG_ATTR_SESSION_INFO, ...)`
  - 但 canonical `src/fafafa.ssl.winssl.connection.pas` 却没有把它用于 `FSessionReused` / saved session metadata
  - 这让设计文档里“WinSSL session truth 来自 Schannel session info”的说法和真实实现再次脱节

- 当前批次因此把 WinSSL session truth 重新压回了 canonical 实现：
  - `src/fafafa.ssl.winssl.base.pas` 现在显式发布 `SSL_SESSION_RECONNECT = 1`
  - `src/fafafa.ssl.winssl.connection.pas` 新增 current-session-info helper，直接读取 `SECPKG_ATTR_SESSION_INFO`
  - `FSessionReused` 现在来源于 `dwFlags and SSL_SESSION_RECONNECT`
  - `SaveSessionAfterHandshake(...)` 会把真实 resumed flag 写进 `TWinSSLSession.SetSessionMetadata(...)`

- broader WinSSL runtime suite 之前也确实没有真正覆盖 dedicated session-resumption proof lane：
  - `tests/run_winssl_tests.ps1` 原先只跑 comprehensive / integration / backend-comparison / performance / handshake-debug / https-client
  - `test_winssl_session_resumption.lpi` 虽然存在，但没有被 broader suite 触发
  - 这意味着 checklist/bundle 里虽然说“高风险区域要单独盯 session resumption”，但活跃 CI/手动主路径还没有一条 dedicated proof

- 这批已经把 broader proof surface 向前推进了一步，但仍没有假装“WinSSL runtime proof 已经完成”：
  - `tests/winssl/test_winssl_session_resumption.pas` 现在聚焦同一 context repeated handshake 的 reuse truth
  - 它会同时检查：
    - `ISSLSessionResumption.IsSessionReused`
    - core `ISSLConnection.IsSessionReused`
    - `GetConnectionInfo.IsResumed`
    - `GetPerformanceMetrics.SessionReused`
  - 并输出稳定的 `[WINSSL-SESSION-RESUME] ...` markers
  - `tests/run_winssl_tests.ps1` 会把这些观测提升成 `[WINSSL-RUNTIME] session_resumption ...` markers
  - 但是否稳定观测到 `observed_reuse=true`，仍然必须由 GitHub Windows runner 的 live artifact 给出最终结论

- GitHub Actions live run `26033545656` 进一步把当前 first hard blocker 压缩成了一条纯 workflow-entry 漂移，而不是新的 WinSSL runtime 语义失败：
  - `windows-gate` 失败在 `Run broader WinSSL runtime suite`
  - 失败原因不是 `observed_reuse=false`，也不是 owner/core/info/perf consistency 断言
  - 而是 `test_winssl_session_resumption.lpi` 自己仍硬编码了 `TargetOS=linux`
  - Windows runner 因而以 `-Tlinux` 编译这条新 lane，并在 `Can't find unit system` 处直接终止

- 这次 live run 也顺手暴露了一个流程层漏口：
  - 仓库里已有 `tests/scripts/test_winssl_windows_runtime_project_target_contract.sh`
  - 它本来就是用来防止 WinSSL runtime-entry `.lpi` 再次漂回非 Windows target
  - 但新加的 `test_winssl_session_resumption.lpi` 没有被纳入 guard 列表
  - 所以本地 focused contracts 全绿时，这个新文件仍然可以悄悄绕过旧 guard 进入 CI

- 因而当前最小正确修法不是继续猜 runtime 行为，而是先把 workflow-entry 真相补齐：
  - 去掉 `test_winssl_session_resumption.lpi` 的硬编码 Linux target
  - 把 `test_winssl_windows_runtime_project_target_contract.sh` 扩到该文件
  - 重新触发 `wave-b-b2-manual.yml`，再看 Windows live artifact 给出的真正 runtime 结论

- 同一 run 的 `macos-gate` 失败面已确认是另一条既有 OpenSSL 模块测试 lane：
  - `scripts/run_all_module_tests.sh` 结果为 `17` 个测试中 `8` 通过、`9` 失败、通过率 `47.1%`
  - 它与本批新增的 WinSSL session-resumption lane 无直接耦合
  - 因而本批不把 macOS module failures 误记成“WinSSL runtime proof 回归”

- 这次本地复核也再次确认了 Windows runtime truth source 的边界：
  - 去掉 `.lpi` 里的硬编码 Linux target 后，Linux 宿主上的 `lazbuild tests/winssl/test_winssl_session_resumption.lpi` 会自然落到 `unit Windows` 缺失
  - 这说明当前 `.lpi` 已不再偷带错误平台目标，而是回到了“宿主是谁就按谁编”的正确形态
  - 这也再次证明 dedicated WinSSL session-resumption lane 的最终验收必须继续看 GitHub Windows runner，而不是把 Linux 本地 Lazarus 结果误当成目标真相

- GitHub Actions live rerun `26034303732` 已把上一个问题真正关掉：
  - `test_winssl_session_resumption.lpi` 在 Windows broader suite 的 compile phase 这次已经通过
  - 所以 `.lpi` target drift 的修复是有效的，不再是当前 blocker

- 同一个 rerun 也把新的 shared runtime defect 压得很集中：
  - `WinSSL Integration Tests (Multi-Scenario)`
  - `WinSSL Session Resumption Truth`
  - `WinSSL Performance Benchmark`
  - `WinSSL HTTPS Client`
  都在握手后落到 `src/fafafa.ssl.winssl.connection.pas` 的
  `UpdateSessionReuseTruthFromContext(...)` 并触发 `EAccessViolation`

- 这说明当前真正的问题不是“某个专项测试写错”，而是新接入的 session-info observation 破坏了共享握手后路径：
  - `SaveSessionAfterHandshake(...)` 现在在 client/generic handshake 成功后都会调用
  - `UpdateSessionReuseTruthFromContext(...)` 也会被普通 `GetSession()` 路径复用
  - 因而只要这条 helper 不安全，就会把多条 WinSSL broader-suite runtime path 一起打崩

- 当前最小正确修法因此也很明确：
  - `SECPKG_ATTR_SESSION_INFO` 继续保留为 truth source
  - 但 `TryGetCurrentSessionInfo(...)` 必须把异常吞掉并返回 `False`
  - `UpdateSessionReuseTruthFromContext(...)` 必须降成 best-effort：
    - 能读到就更新 `FSessionReused` / `session_id`
    - 读不到或异常就回落成 `session_id=''` / `FSessionReused=False`
    - 绝不允许 session-info observation 破坏已成功的握手路径

- 同一个 rerun 还有一条重要的范围收敛事实：
  - `macos-gate` 在 `26034303732` 已经转绿
  - 所以当前 workflow 唯一剩余 blocker 是 Windows broader suite 的 shared session-info `AV`

- GitHub Actions live rerun `26034948820` 又把这条 Windows blocker 往下压了一层：
  - `linux-gate` / `macos-gate` 继续保持 green
  - broader suite 的所有 compile phase 继续通过
  - 共享 crash 顶点不再只是泛指 `UpdateSessionReuseTruthFromContext(...)`
  - 而是明确落在 canonical `src/fafafa.ssl.winssl.connection.pas` 的 `SessionIdBytesToHex(LSessionInfo)` 读取

- 这给了当前实现边界一个更具体、也更可靠的真相：
  - `SECPKG_ATTR_SESSION_INFO.dwFlags` 仍可作为 `IsSessionReused` 的 Schannel runtime truth
  - 但同一结构里的 raw session-id byte buffer 在 GitHub Windows runner 上并不稳定
  - 所以 canonical shared connection flow 不能再把 “读到了 session info” 自动延伸成 “可以安全读取 raw session-id bytes”

- 因而当前这批最小正确修法也进一步收紧了：
  - 保留 `TryGetCurrentSessionInfo(...)` / `UpdateSessionReuseTruthFromContext(...)` 的 best-effort 边界
  - 继续用 `dwFlags and SSL_SESSION_RECONNECT` 写入 `FSessionReused`
  - 不再在共享路径里做 `SessionIdBytesToHex(LSessionInfo)`
  - `ASessionId` 留空，继续走现有 fallback：
    - `SaveSessionAfterHandshake(...)` 的 `Format('winssl-session-%p', ...)`
    - `DoGetSession()` 里的 timestamp-based fallback

- GitHub Actions live rerun `26035941452` 证明上面的收口还不够：
  - `windows-gate` 已经稳定通过 `Run quick WinSSL smoke` 与 `Run Windows Wave B gate`
  - dedicated session-resumption lane 的 compile phase 也继续通过
  - 但 broader suite 仍在 `UpdateSessionReuseTruthFromContext(...)` 的 line `850` 触发 `EAccessViolation`
  - 也就是：删掉 raw session-id byte 读取，只是把 crash 从 helper 里更早的读点推到了更后的写回点

- 这让当前根因判断再次收缩了一层：
  - 问题不再只是 `SessionIdBytesToHex(...)`
  - 而是 canonical shared path 上整条 `SECPKG_ATTR_SESSION_INFO` probe 仍然不安全
  - 更直白地说：在当前 binding / GitHub Windows runner 组合下，`QueryContextAttributesW(..., SECPKG_ATTR_SESSION_INFO, ...)` 这件事本身还不能放进共享握手后路径

- 因而第三层收口也已经明确，不应该继续在线上重复“更温柔地读取同一 probe”：
  - canonical shared handshake path 当前必须完全停用 live `SECPKG_ATTR_SESSION_INFO`
  - 共享真相退回到 `FSessionReused=False` 与现有 fallback session-id 生成逻辑
  - `TryGetCurrentSessionInfo(...)` 最多保留成后续 dedicated Windows proof lane 的实验入口，而不是继续影响普通连接流

- 同一个 rerun 还确认了一件范围控制上的事：
  - `macos-gate` 的失败回到了独立的 `run_all_module_tests.sh` module lane
  - 它不改变当前 WinSSL shared-crash 的根因判断，只是提示另有一条平台回归需要单独排队

- GitHub Actions live rerun `26037518301` 终于把这条 bridge lane 真正跑通了：
  - `linux-gate` / `macos-gate` / `windows-gate` / `summary` 全部 success
  - `windows-gate` 的 broader suite 已经 7/7 PASS，不再有 shared session-info crash
  - 这说明“停用 canonical shared path 的 live session-info probe”是当前正确而且足够的收口

- 同一份 Windows runtime artifact 也给出了当前最重要的产品真相：
  - `WinSSL Session Resumption Truth` 当前是稳定通过的
  - 但 marker 明确记录：
    - `attempts=4`
    - `observed_reuse=false`
    - `require_reuse=false`
    - `session_configured=true`
  - 也就是说：当前 backend 已能稳定表达“配置了 session，但没有在这条 CI runtime 里观测到真实 resumed handshake”

- 这让后续方向变得非常清楚：
  - “不会 crash、不会误报、证据链完整” 这一层已经完成
  - 若产品目标是让 WinSSL 真正命中 resumed handshake，下一条工作就不该再碰 workflow/contract capture
  - 而应直接进入 WinSSL backend native resumption implementation / platform-behavior investigation

- 根因已被压缩到 evidence capture 层，而不是 WinSSL 实现层：
  - workflow 用 `Start-Transcript` 包住父 PowerShell
  - broader suite 则在子 `pwsh -File tests/run_winssl_tests.ps1` 里执行
  - 结果是 GitHub Actions 控制台看得到子进程输出，但上传下来的 `winssl_runtime_suite_*.log` 只剩 transcript 开头/结尾壳

- 之前的 handoff/consistency 链条也确实有一个设计缺口：
  - `check_wave_b_b2_evidence_consistency.sh` 对 `windows_runtime_transcript` 只做 presence check
  - 这会把“文件存在但没有实质 runtime 内容”的 artifact 也记成 `CONSISTENT`
  - 因而旧的 `CLOSED` / `CONSISTENT` 结论证据强度偏弱，不足以直接当作 WinSSL runtime proof

- 当前这批修复的正确边界已经明确，不需要误伤 WinSSL 实现本身：
  - workflow 改成 UTF-8 console capture 落盘
  - `tests/run_winssl_tests.ps1` 输出稳定的 ASCII `[WINSSL-RUNTIME]` markers
  - consistency / handoff 报告改成检查 `suite_start` / `suite_summary` / `suite_end`
  - 这样下一次 CI 只要 artifact 仍然是空壳，就会被脚本直接判成 `INCONSISTENT`

- 这批收口后的真实结论也因此改变了：
  - Wave B/B2 manual lane 上的 Windows runtime evidence blocker 已经不再是 capture/handoff
  - 剩余真正值得继续投入的，是更深的 backend/runtime completeness 本身

- 本轮新的审查目标不是 release / workflow / runtime closeout，而是：
  - 公共接口设计是否已经失真
  - facade / factory / builder / config 是否把旧语义继续传播进新入口
  - 各 backend implementation 与 capability truth 是否仍然一致

- 当前已知静态审查基线来自 `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`：
  - `ISSLConnection` 过胖
  - context-level SNI 已 deprecated，但高层入口仍在写入
  - 文档承诺 `ISSLServerConnection`，源码中没有
  - `TSSLConfig` 混合跨层配置
  - capability matrix 仍有布尔字段与 support-level 双真相
  - `fafafa.ssl` 门面仍同时导出多条历史路径

- 但这份旧 audit 还没有回答两个更关键的问题：
  - 上述设计问题是否已经在 backend 实现、selector、serializer、capability contracts 中演变成真实行为漂移
  - 是否存在能用 focused contract 直接钉住并修掉的“接口设计影响实现真相”的问题

- 本轮因此采用“两段式审查”：
  - 第一段先做横向验证，确认 public surface / docs / backend truth 是否一致
  - 第二段只修高价值且边界清晰的问题，避免把结构性设计债误当成一批次即可完成的大重构

- 第一批 live 代码交叉验证当时确认：context-level `ServerName` 不只是“deprecated 但没人用了”的旧接口，而是仍然被实现主动传播：
  - `TSSLFactory.CreateContext(...)` 与 `TSSLContextBuilderImpl.Build*` 仍在对 context 调 `SetServerName(...)`
  - 当时 OpenSSL / WinSSL / FreePascal / MbedTLS / WolfSSL 的 connection 构造器都会把 context-level `ServerName` 带进连接实例
  - 现有测试 `tests/test_factory_server_name_scope_clarification.pas` 与 `tests/test_sslctxboth_client_capability_clarification.pas` 当时也把这种 fallback 继承锁成预期
  - 这说明 SNI 问题已经从“接口设计异味”演变成了“实现层 + 合同层一起固化的历史语义”

- `TSSLConfig.BufferSize` / `HandshakeTimeout` 当前更像“跨层暴露但带显式路障”的设计债，而不是隐藏 bug：
  - `src/fafafa.ssl.factory.pas` 已把它们判定为 connection-scoped / non-context-scoped，并在 request/default 路径上拒绝自定义值
  - 现有 `tests/test_factory_connection_scope_clarification.pas` 已把这条语义锁住
  - 所以问题不在“字段 silently ignored”，而在“公共 record 继续挂着跨层字段，增加理解负担”

- capability 体系的“双真相”问题仍然存在，而且已经扩散到 serializer / diff / selector / contracts：
  - backend `GetCapabilities` 仍同时发布 `SupportsSNI` / `SupportsOCSPStapling` / `SupportsSessionTickets` 与对应的 `*Support`
  - `src/fafafa.ssl.backend.selector.pas` 选特性时优先信 `*Support`
  - `src/fafafa.ssl.capability.serializer.pas` 与 `src/fafafa.ssl.capability.diff.pas` 又同时序列化/反序列化/比较两套字段
  - `tests/contract/test_backend_contract.pas` 对 optional interface 对齐仍主要依赖旧布尔字段（例如 `SupportsSNI`）
  - 也就是说，当前不是某一个 backend 的单点错误，而是 capability model 还没有完成“旧布尔兼容字段从属化”

- 当前最适合本批修的，不是直接拔掉 context-level SNI 旧语义：
  - 因为这会同时打到 factory、builder、各 backend connection 构造器和多份既有合同
  - 这是明确的下一阶段设计迁移，不是“单批次最小修复”
  - 本批更适合先修文档/合同真相，避免源码没有的接口继续被公开文档承诺

- 文档层的一个真实错误已经在本批收口：
  - `docs/ARCHITECTURE.md` 与 `docs/reference/INTERFACE_DESIGN_V2.md` 之前把 `ISSLServerConnection` 画进了活跃接口层次
  - live source 并没有任何 `ISSLServerConnection` 声明
  - 当前修法是把活跃文档改回“当前只公开 `ISSLClientConnection`，服务端特性主要走 optional context interfaces”
  - 并新增 `tests/scripts/test_interface_docs_no_nonexistent_isserverconnection_contract.sh`，防止这类承诺漂移再次回流

- focused 验证结果在当时进一步支持了当前路线判断：
  - `tests/test_factory_connection_scope_clarification.pas` PASS，证明 `BufferSize` / `HandshakeTimeout` 是显式 scope gate，而不是静默失效
  - `tests/test_factory_server_name_scope_clarification.pas` PASS，证明 client-side context `ServerName` 当时仍被正式支持为兼容路径
  - `tests/test_sslctxboth_client_capability_clarification.pas` PASS，证明多 backend 连接构造器当时仍主动继承 context-level `ServerName` fallback
  - 因此“删除 context-level SNI fallback”必须被当作一次兼容性迁移，而不是局部 bugfix

- capability 双真相的 runtime 半边已经可以安全收口，而且应该先收 runtime、后碰 serializer：
  - `src/fafafa.ssl.base.pas` 新增 `NormalizeLegacyCapabilityBooleans(...)`
  - 它统一把 `SupportsSNI` / `SupportsALPN` / `SupportsOCSPStapling` / `SupportsCertificateTransparency` / `SupportsSessionTickets` 视为对应 `*Support <> sslSupportNone` 的兼容投影
  - OpenSSL / FreePascal / WinSSL / MbedTLS / WolfSSL 的 `GetCapabilities` 现在都在返回前走同一条归一化路径
  - 这样 runtime live truth 不再分散在各 backend 自己手填的 legacy boolean 上

- capability focused contracts 也已经切换到 support-level truth：
  - `tests/contract/test_capabilities_contract.pas` 对 major backend 的 SNI / ALPN 改为检查 `SNISupport` / `ALPNSupport <> None`
  - 同时新增 bool/support-level 一致性断言，直接钉住兼容投影必须同步
  - `tests/contract/test_backend_contract.pas` 对 SNI / CT / OCSP optional interface alignment 也改为信 `*Support <> None`
  - 这说明“runtime truth 以 support-level 为准”已经不只是设计意见，而是被合同固定下来的行为规范

- 但 capability 双真相还没有全系统收完：
  - serializer / deserializer / diff 仍然同时 round-trip 和比较两套字段
  - 下一批应该设计“旧输入兼容、内部真相单一”的规则，而不是现在就删除 legacy boolean 字段

- serializer / deserializer / diff 线上的 live 问题已经被精确缩小到两处，而且都不是假问题：
  - `JSONToCapabilities(...)` / `XMLToCapabilities(...)` 原先会并列接受 legacy boolean 与 `*Support`，但对冲突输入没有裁决规则
  - `CompareCapabilities(...)` 原先几乎完全忽略 v1.2 support-level 差异，只看 legacy boolean，因此会漏掉 `experimental -> stable` 这类真实 capability 变化
  - 这两个问题都已经由新的 focused regression 先打出红灯

- 当前修法明确了 capability compatibility boundary：
  - 对反序列化输入：
    - 如果 payload 里出现了某个 `*Support` 字段，就以它为真相，并回填对应 legacy boolean
    - 如果 payload 只有旧 boolean，没有 `*Support`，则继续保留旧输入兼容，不擅自猜测 support-level
  - 对 capability diff：
    - paired feature 先比较 `*Support`
    - support-level-only 的 `SessionCacheSupport` / `ZeroRTTSupport` / `EarlyDataSupport` / `RenegotiationSupport` / `PostHandshakeAuthSupport` 也开始进入 diff
    - 只有在没有 support-level truth 可用时，legacy boolean 才作为 diff fallback

- 这也带来一个清晰的剩余边界：
  - 对“手工构造但内部已经不一致”的 `TSSLBackendCapabilities`，`CapabilitiesToJSON/XML` 目前仍偏向原样输出
  - 这不是 runtime live backend 的当前缺口，也不是反序列化/比对链路的 blocker
  - 但如果后续要把 capability model 彻底收成单真相，还需要决定 serializer 输出面是否做额外的受控归一化

- 继续沿着 SNI 主线深挖后，warning 治理这条线也已经有了新的 live 结论：
  - 旧计划 `docs/plans/2026-05-13-internal-context-servername-warning-quarantine.md` 点名的 `factory` / `builder` / `openssl.connection` / `openssl.backed` 不再是当前 compile 噪音主来源
  - `tests/test_builder_integration.pas` 也不再适合作为 warning contract 入口，因为它已经不能稳定暴露 `ISSLContext.Get/SetServerName` deprecated warning
  - 当前真正能稳定打出 warning 的 live probe 是 `tests/contract/test_capabilities_contract.pas`

- 这次 live probe 精确表明：
  - `wolfssl.connection` 两个构造器里的 context fallback 读取仍会触发 `ISSLContext.GetServerName` deprecated warning
  - `mbedtls.connection` 的 SNI 回填路径也会触发同类 warning
  - `WinSSL` 在当前 Linux compile path 里不直接暴露 warning，但源码里同样存在两处 direct `AContext.GetServerName` 兼容读取

- 因此 warning 治理的最小安全修法已经明确：
  - 不改兼容行为
  - 不碰 factory / builder
  - 只在 `wolfssl` / `mbedtls` / `winssl` 这些内部兼容读取点加局部 `{$PUSH}{$WARN 6058 off}{$WARN SYMBOL_DEPRECATED OFF}` / `{$POP}` quarantine
  - focused shell contract 也改成盯 `test_capabilities_contract` 的 compile log，并对 `WinSSL` 补静态 source guard

- 继续往下验证后，serializer 输出面也被证明确实存在 live truth 漂移：
  - `CapabilitiesToJSON(...)` / `CapabilitiesToXML(...)` 原先直接输出 `ACaps.Supports*`
  - 这会让一个已经携带 v1.2 support-level truth 的 record 仍然导出自相矛盾的 payload，例如：
    - `supportsSNI=false`
    - `sniSupport="stable"`
  - 这说明“support-level 为真相”的收口如果不延伸到 serializer，外部数据面仍会重新泄漏历史双真相

- 当前最小安全修法也已经明确并落地：
  - serializer 先复制一份本地 record
  - 仅当该 record 已携带任意 support-level truth 时，才用 `NormalizeLegacyCapabilityBooleans(...)` 回填 legacy boolean 输出视图
  - 这样可以修掉 v1.2-aware record 的外部输出漂移，同时不去瞎猜纯 legacy-only in-memory record 的 `none` 是否只是默认值

- 继续顺着 serializer 线往下查，又补出了一条更具体的 legacy-only round-trip 漂移：
  - `CapabilitiesToJSON(...)` / `CapabilitiesToXML(...)`
    之前还会无条件导出：
    - `sniSupport`
    - `ocspStaplingSupport`
    - `sessionTicketsSupport`
    - 以及其它 `*Support`
  - 对 pure legacy-only record 来说，
    这些字段的 `none`
    不是输入里真的存在的 truth，
    只是 record 默认值
  - 但一旦输出后，
    反序列化 precedence
    又会把它们当成显式 support-level truth，
    反向覆盖掉原始 legacy boolean

- 这说明 capability dual-truth 的 residual
  不只存在于“support-level-aware record 导出自相矛盾”
  这一种形态，
  还存在于“serializer 凭空合成 `none` truth”
  这条 legacy-only lane

- 这批最小正确修法是：
  - 对 pure legacy-only record，
    serializer 不再导出 synthetic `*Support`
  - 只有 record 已经进入 support-level-aware lane 时，
    才继续显式导出 support-level 视图
  - 这样：
    - support-level-aware record
      仍保持 `*Support` 为真相源
    - legacy-only record
      round-trip 后继续保留旧布尔真相

- 之所以这个修法现在是安全的，
  是因为 live backend `GetCapabilities`
  producer 已经统一发布完整 support-level matrix：
  - 当前 runtime/export 主路径天然属于 support-level-aware lane
  - 这次收掉的是
    public serializer
    对 legacy-only / compatibility record
    的 synthetic truth 漂移，
    不是在削弱已建立的 runtime truth

- 这条线剩下的真正结构性残口也更清楚了：
  - 手工构造的 mixed in-memory record
    如果一部分字段是 support-level-aware，
    另一部分字段仍只靠 legacy boolean，
    目前 record 模型仍没有 presence bits
    区分：
    - `none` 是默认未设置
    - 还是显式不支持
  - 这个问题不能继续靠 serializer 猜；
    若未来要彻底消灭歧义，
    需要 capability model 本身补 truth/presence 元信息

- 这次 focused compile 还顺手暴露了同文件另一类值得立刻收掉的静态风险：
  - `JSONToCapabilities(...)`
    /
    `XMLToCapabilities(...)`
    之前对
    `TSSLBackendCapabilities`
    这种带 managed field 的 result record
    直接做
    `FillChar(Result, SizeOf(Result), 0);`
  - 这正是 FPC 会给出
    “managed type result variable does not seem to be initialized”
    警告的那类写法
  - 当前已改成
    `Result := Default(TSSLBackendCapabilities);`
    把初始化收回类型安全路径
  - 这不是行为面大改，
    但它消掉了本单元两条真实静态警告，
    也避免以后再在 serializer/deserializer
    上留下 managed-record 初始化隐患

- 继续沿 high-visibility public surface 往下查后，又补出了一类同源实现残口：
  - `src/fafafa.ssl.pas`
    的
    `CreateDefaultConfig(...)`
    fallback
    还在对
    `TSSLConfig`
    直接做
    `FillChar(Result, SizeOf(Result), 0);`
  - `src/fafafa.ssl.connection.base.pas`
    的 shared getter 里，
    `GetConnectionInfo`
    /
    `GetDiagnosticInfo`
    以及空
    `TBytes`
    默认返回
    仍会打出
    `managed type result variable does not seem to be initialized`
    warning

- 这条线比普通 warning 更值得优先收，
  因为它们都位于：
  - public facade helper
  - shared connection base owner/mirror surface
  属于高复用、高可见的公共实现层

- 当前最小正确修法也很明确：
  - 对带 `string` / 动态数组成员的 result record
    统一改用
    `Default(...)`
  - 对空 `TBytes`
    返回统一改用
    `Result := nil`
  - 不改任何 public 行为，
    只把初始化路径收回 Pascal managed-type 的安全语义

- focused compile proof 也说明这不是“顺手换个写法”：
  - `connection.base`
    之前那 4 条 managed-result warning
    现在已经消失
  - `test_default_config`
    /
    `test_connection_builder_hostname_precedence`
    运行结果继续全绿
  - 所以这批收掉的是 shared public implementation 残口，
    不是把旧 warning 藏起来

- 顺着这条 managed-result safety 线继续往下查后，又补出了第二组高复用实现残口：
  - `BuildTLSPlaintext(...)`
    是 FreePascal/TLS13/runtime tests
    共用的 shared wire helper
  - `ReadVector16(...)`
    与
    `TFreePascalSession.Serialize(...)`
    是 FreePascal session resumption / early-data
    基础路径的一部分
  - 它们之前都在 `Result: TBytes`
    未显式初始化时直接
    `SetLength(...)`
    或
    `SetLength(Result, 0)`

- 这说明 managed-result 问题并不只停留在 facade/base-class：
  - shared transport/wire helper
  - shared session persistence helper
  也有同类历史写法

- 当前修法继续保持最小：
  - 不重写逻辑
  - 不改 wire/session 语义
  - 只把空 `TBytes`
    初始化统一改成
    `Result := nil`

- focused proof 说明这是“真实 warning 消失 + 行为保持”的收口：
  - `tests/test_tls13_foundation.pas`
    继续通过，
    说明 TLS record builder / parser 没被打穿
  - `tests/test_freepascal_client_session_resumption.pas`
    继续通过，
    说明 FreePascal session serialize / resumption 语义保持不变
  - compile grep 里，
    `tls13.wire`
    和
    `freepascal.session`
    已不再打出原来的 managed-result warning

- 这也把剩余边界说得更清楚了：
  - 现在已经解决的是“v1.2-aware record 导出不应自相矛盾”
  - 尚未、也不能在本批假装解决的是“纯 legacy-only record 在缺少 presence bit 时，是否应该把 `none` 当作显式不支持”
  - 如果将来想彻底消灭这类歧义，需要 capability model 自身增加 presence/truth 元信息，而不是继续在 serializer 里猜

- 在继续推进前，`context-level ServerName` 这条主线的路线图也已经被压实成当前可执行真相：
  - 高层写入面：
    - factory client path 仍会把 `TSSLConfig.ServerName` 写回 context
    - factory server path 已经禁止 `ServerName`
    - builder `BuildClient` 仍会保留 `WithSNI(...) -> context.SetServerName(...)` 的 client-side 兼容写入
    - builder `BuildServer` 现在只保留 warning / compatibility metadata，不再把 `WithSNI(...)` 写回 built context
    - connector 已经是正确方向，直接把 hostname 写到 `ISSLClientConnection.SetServerName(...)`
  - backend 继承面：
    - OpenSSL / FreePascal / WolfSSL / MbedTLS / WinSSL 五个 connection constructor 仍会从 context fallback 读取 `GetServerName`
  - 合同锁点：
    - precedence / inheritance / cross-backend consistency / error normalization 这些测试都仍在有意保留旧兼容语义

- 这意味着最合理的迁移顺序不是“先删 backend fallback”，而是：
  - 先收高层 surface，减少继续写入 deprecated context-level SNI 的入口
  - 再把 backend constructor 里的 fallback 提取成共享 compatibility shim
  - 最后才考虑真正删除历史继承语义

- 兼容测试的“显式标签”也需要当成一等资产维护：
  - 这次映射证明不只最初那几份测试在锁兼容语义，`test_context_builder_server_servername_runtime_consistency.pas` 与 `test_sslctxboth_client_capability_clarification.pas` 也同样在锁住旧 fallback
  - 已经把这些文件纳入 `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - 以后做迁移时，谁要改掉这些行为，就必须先面对这些标签与合同，而不是“顺手改了再说”

- `context-level ServerName` 的 builder surface 第一刀已经证明可以安全缩窄：
  - `ExportToJSON(...)` / `ExportToINI(...)` 现在在保留 `server_name` 的同时，额外导出 `server_name_mode=deprecated_context_sni`
  - `ImportFromJSON(...)` / `ImportFromINI(...)` 显式接受这个 marker 但不让它改变 runtime state
  - legacy-only `server_name` JSON/INI 载荷在 re-export 时会被自动升级成“带兼容 marker 的旧语义”

- 这条线也顺手暴露了一个测试工作流细节：
  - builder JSON 导出使用 `FormatJSON`
  - 对这种输出做 substring 硬匹配会把空格/换行格式误判成行为回归
  - 新 focused test 已改为解析 JSON 字段值本身，避免后续在格式噪音上反复红灯

- `context-level ServerName` 的 factory/config 第二刀也已经证明可以安全收口：
  - `TSSLFactory.CreateContext(AContextType, ALibType)` 与 `TSSLFactory.CreateContext(const AConfig)` 在 client-side 兼容写入 `TSSLConfig.ServerName` 时，不再静默
  - 当前会通过 `TSecurityLog.Warning('Factory', ...)` 显式提示：
    - `TSSLConfig.ServerName` 只是 deprecated context-level SNI compatibility
    - 推荐主路径是 `ISSLClientConnection.SetServerName(...)` / `TSSLConnector.Connect*(..., ServerName)`
  - default-config path 与 one-shot config path 都已被 focused tests 钉住

- 这一步也顺手把 public-facing truth 对齐到了一个更一致的状态：
  - `src/fafafa.ssl.base.pas` 的 `TSSLConfig.ServerName` 字段注释已经明确写成 compatibility-only
  - `docs/reference/API_REFERENCE.md` 也新增了 client SNI compatibility note
  - active docs contract 继续绿色，说明这次文档补充没有把旧 context-level SNI 路线重新教回去

- 因此 `context-level ServerName` 的高层写入面已经不再是“静默主路径”：
  - builder import/export 会加 marker
  - factory/config runtime path 会发 warning
  - 剩余的真正主问题已经转移到 backend constructor fallback 仍分散在五个实现里

- `context-level ServerName` Phase C 的第一刀已经把“分散在五个 backend 的 direct deprecated read”收成了共享 seam：
  - 新增 `src/fafafa.ssl.context.compat.pas`
  - helper 统一封装：
    - client-role gate
    - deprecated `ISSLContext.GetServerName` 读取
    - local warning suppression
  - 这使得 deprecated context fallback 的控制面重新回到一处，而不是继续散落在 OpenSSL / FreePascal / WolfSSL / MbedTLS / WinSSL 五份构造器里

- 这次 shared shim 提取刻意保持 backend side effect 不变：
  - OpenSSL / MbedTLS 继续走 `SetServerName(...)`
  - FreePascal / WolfSSL / WinSSL 继续走字段赋值路径
  - 因此这批是 seam consolidation，不是 behavior migration

- focused source contract 证明 Phase C 第一刀已经真正落地，而不是只在文档里说说：
  - `tests/scripts/test_context_server_name_compat_shim_contract.sh`
    - 要求 shared helper 存在
    - 要求五个 backend 都调用 shared helper
    - 要求五个 backend 不再直接读取 `AContext.GetServerName` / `FContext.GetServerName`

- runtime regression 在那一批也证明 shared shim 没有误伤当时的兼容真相：
  - `tests/test_sslctxboth_client_capability_clarification.pas` 继续绿色
  - `tests/test_factory_server_name_scope_clarification.pas` 继续绿色
  - 这说明在那一时点，“context -> connection 的 fallback 仍存在” 与 “deprecated read 已被集中治理” 这两件事可以同时成立

- 因而 SNI 主线的剩余问题已经再次前移：
  - backend constructor 不再是散点收口对象
  - 下一批应该讨论的是 public/high-level surface cleanup，而不是重新逐个 backend 找 direct fallback read

- 继续往前收后，builder surface 的一个真实缺口也已经被证实并修掉：
  - `ValidateClient` / `ValidateServer` 早就会对 `WithSNI(...)` 发 deprecated compatibility warning
  - 但真实 `BuildClient` / `BuildServer` 路径此前仍然会静默应用它
  - 这造成 builder 的 validation truth 与 runtime truth 不对齐，也让 `WithSNI(...)` 看起来仍像正常主路径

- 当前修法把 builder 的 public/runtime/validation 三层重新对齐到了同一套术语：
  - `BuildClient` 现在会显式 warning：
    - `WithSNI` 是 deprecated context-level SNI compatibility
    - 推荐迁移到 `TSSLConnectionBuilder.WithHostname(...)` / `ISSLClientConnection.SetServerName(...)` / `TSSLConnector.Connect*(..., ServerName)`
  - `BuildServer` 现在会显式 warning：
    - `WithSNI` 只是 deprecated context-level ServerName compatibility
    - server-side connections ignore it
  - `ValidateClient` / `ValidateServer` 也同步沿用这条术语线，不再只在 validation 里单独说一套

- 这使得 `context-level ServerName` 的高层 surface 已经基本不再“静默”：
  - builder import/export 会打 compatibility marker
  - builder runtime path 会发 warning
  - factory/config runtime path 会发 warning
  - backend constructor fallback 已收成 shared shim

- 因而真正剩下的已经不是“哪里还在默默保留旧语义”，而是“哪一组 intentional compatibility tests 先改，才能开始第一条真实 behavior migration RED”

- 第一批普通 WinSSL 客户端连接流的分类与迁移也已经有了实锤：
  - `tests/winssl/test_winssl_error_mapping_online.pas`
  - `tests/winssl/test_winssl_https_client.pas`
  - `tests/winssl/test_winssl_revocation_online.pas`
  - `tests/winssl/test_winssl_mtls_e2e_local.pas`
  - 这些文件都属于真实客户端连接/握手/验证流，不是 intentional compatibility，也不是 context API-surface coverage

- 这四个文件当前已经统一改成：
  - 先 `CreateConnection(...)`
  - 再拿 `ISSLClientConnection`
  - 然后在 `Connect` / `DoHandshake` 前设置 `ServerName`
  - 因此这些文件不再继续把 deprecated context-level SNI 当成正常客户端流的指导方式

- 验证证据也说明这不是纸面改动：
  - focused shell contract 已经证明这四个文件不再含 `Ctx/Context/LCtx/LContext.SetServerName(...)`
  - 本地 Linux 直接编 `test_winssl_https_client.pas` 仍会因为 `fafafa.ssl.winssl.lib` 依赖 `Windows` 单元而失败，这不是本批引入的新问题，而是该测试本身的平台边界
  - 改走 `fpc -Twin64` 后，这四个文件的 Win64 交叉编译都成功完成

- 这轮收口之后，剩余活跃 context-level `SetServerName(...)` 命中已经更接近“故意保留”的集合：
  - connector / precedence / cross-backend compatibility tests
  - backend context contracts / framework tests
  - WinSSL comprehensive / library-basic / skeleton 这类更偏 API-surface 或未完成分类的文件
  - 剩下真正还像普通客户端流的主要残留，已经缩到 `test_winssl_mtls_skeleton.pas` 的握手路径这类更小的面

- 这批 residual 分类/收口之后，上述“更小的面”也已经被真正消化掉：
  - `tests/test_tls_connector_early_data_contract.pas`
    - 已补 `INTENTIONAL_COMPAT`，明确它故意从 inherited context fallback 起步
  - `tests/mbedtls/test_mbedtls_context_contract.pas`
  - `tests/wolfssl/test_wolfssl_context_contract.pas`
  - `tests/winssl/test_winssl_library_basic.pas`
    - 已补 `INTENTIONAL_API_SURFACE`，明确它们是在覆盖 deprecated context setter/getter surface
  - `tests/winssl/test_winssl_mtls_skeleton.pas`
    - 配置段 `SetServerName('test.example.com')` 已补 `INTENTIONAL_API_SURFACE`
    - 真实 `TestMTLSHandshake` 路径已改成 `CreateConnection(...) -> ISSLClientConnection.SetServerName(ServerHost) -> DoHandshake`

- focused contract 与编译证据共同说明：当前剩余活跃 context-level `SetServerName(...)` 命中已经基本不再混着普通客户端流指导语义，而主要是 intentional compatibility / API-surface coverage

- 因而 SNI 主线的下一步已经可以正式前移到：
  - 选择第一组要改写的 client-side intentional-compat tests
  - 定义第一条 client-side fallback behavior migration RED
  - 而不是继续做 residual 分类考古或重复处理 server-only dead compatibility

- 第一条真正的 behavior migration 已经不再停留在路线图上：
  - `BuildServer.WithSNI(...)` 现在只会发 warning，并明确说明 `BuildServer ignores it and server-side connections ignore it`
  - built server context 不再保留这份 client-only `ServerName`
  - 这说明迁移主线已经可以从“先清 dead compatibility”继续推进到 client-side fallback 真正收缩

- 第一条 client-side fallback behavior migration 也已经有了一个很小但真实的落点：
  - `sslCtxBoth` 既然已经在握手层要求显式选择 role，就不该再静默继承 deprecated context-level client SNI fallback
  - shared compatibility shim 现在会对 `sslCtxBoth` 直接返回空字符串
  - 因而 dual-role context 仍保持 client-capable connection surface，但 inherited `ServerName` fallback 已经不再自动生效

- 这让剩余 client-side 迁移面再次收窄：
  - `sslCtxBoth` 不再需要挂在 intentional compatibility label 集合里
  - 下一步真正要碰的就是 `sslCtxClient` direct / builder / factory 这组还在显式锁 inherited fallback 的测试与路径

- cross-backend consistency / errors 这两份网络合同此前也被错误混进了 intentional compatibility 视角：
  - 它们真正要证明的是跨 backend 的结果一致性 / 错误归一化
  - 不是真正要保护 deprecated context-level SNI fallback
  - 因而把它们继续留在 intentional-compat label 集合里，只会让后续 `sslCtxClient` 迁移继续被假锁点拖慢

- 这两份合同现在已经统一迁到 per-connection SNI：
  - `tests/integration/test_cross_backend_consistency_contract.pas`
  - `tests/integration/test_cross_backend_errors_contract.pas`
  - 路径都是 `CreateConnection(...) -> ISSLClientConnection.SetServerName(...) -> Connect`
  - 连 `HTTP:80` 的握手失败分支也同步改掉了 context-level setter

- 这也让 intentional compatibility label 集合再次缩小到真正还在锁 inherited fallback 或兼容语义的文件：
  - `tests/test_connection_builder_hostname_precedence.pas`
  - `tests/test_tls_connector_hostname_override_precedence.pas`
  - `tests/test_freepascal_context_server_name_inheritance.pas`
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`

- focused source contract 与 integration compile/runtime shape 共同说明：
  - cross-backend 网络合同已经不再教 deprecated context-level SNI
  - 但它们在当前 host 上的 live network execution 仍受 `FAFAFA_RUN_NETWORK_TESTS!=1` gate 保护
  - 因此这批证明的是“合同语义与编译/runtime shape 已对齐”，不是重新做一次外网联机证明

- 在 cross-backend 网络合同收口完成的那个时点，下一条最自然的 `sslCtxClient` behavior migration RED 已经更清楚：
  - 首选应转向 `tests/test_freepascal_context_server_name_inheritance.pas`
  - 因为它比 precedence/override 类测试更直接地锁住 inherited context fallback 本体

- 这条 dedicated FreePascal runtime fallback 现在已经被真正切掉：
  - `src/fafafa.ssl.freepascal.connection.pas` 的 socket / stream client 构造器都不再读取 `GetContextLevelServerNameCompatibilityValue(AContext)`
  - 新建 FreePascal client connection 的 `ServerName` 默认回到空字符串
  - 调用方如果要走 FreePascal client path，必须显式在 connection 上 `SetServerName(...)`

- 这批证明了“builder/factory 仍写 deprecated context-level state”与“某个 backend 已不再消费这份 state”可以同时成立：
  - `TSSLContextBuilder.BuildClient.WithSNI(...)` 仍会发 compatibility warning
  - direct context `SetServerName(...)` 也仍然是 deprecated surface
  - 但 FreePascal runtime 已不再把这份 state 静默带进新连接

- 因而 intentional compatibility label 集合再次收窄：
  - 现在真正还在锁 inherited fallback / compatibility precedence 的只剩：
    - `tests/test_connection_builder_hostname_precedence.pas`
    - `tests/test_tls_connector_hostname_override_precedence.pas`
    - `tests/test_context_builder_server_servername_runtime_consistency.pas`
  - `tests/test_freepascal_context_server_name_inheritance.pas` 已经从“保留兼容”翻成“禁止再继承”

- 邻接 focused evidence 也说明这刀没有误伤下一层计划中的 mock precedence contracts：
  - `tests/test_connection_builder_hostname_precedence.pas` 继续绿色
  - `tests/test_tls_connector_hostname_override_precedence.pas` 继续绿色
  - 这意味着下一刀可以更直接地瞄准 `tests/test_connection_builder_hostname_precedence.pas`

- 所以当前最合理的下一条 `sslCtxClient` behavior migration RED 已经前移：
  - 不再是 `tests/test_freepascal_context_server_name_inheritance.pas`
  - 而是 `tests/test_connection_builder_hostname_precedence.pas`
  - 因为它现在成了剩余 intentional client-side inherited fallback 中最直接的下层契约

- 这条 `TSSLConnectionBuilder` mock precedence 契约现在也已经从“保留 fallback”翻成了“默认清空 fallback”：
  - `src/fafafa.ssl.connection.builder.pas` 的 `TryBuildClient` 在连接支持 `ISSLClientConnection` 时，会始终接管 per-connection hostname state
  - 若调用方没有 `WithHostname(...)`，builder 会显式写入空字符串，而不是继续保留 inherited context fallback
  - 若调用方显式 `WithHostname('conn.example.com')` 或 `WithHostname('')`，仍分别保留 override / clear 语义

- 这说明 `TSSLConnectionBuilder` 现在已经和更早之前收紧的 FreePascal runtime 一样，站到了“explicit per-connection hostname”这边：
  - builder 不再是 context-level SNI fallback 的隐式透传通道
  - connector mock precedence 成了剩余更靠上的 intentional 输入面

- focused evidence 也说明这刀是纯 builder 语义收口，而不是误伤 connector：
  - `tests/test_connection_builder_hostname_precedence.pas` RED -> GREEN
  - `tests/test_tls_connector_hostname_override_precedence.pas` 继续绿色
  - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh` 继续绿色

- 因而当前剩余最直接的 client-side intentional compatibility surface 再次收窄：
  - `tests/test_tls_connector_hostname_override_precedence.pas`
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
  - 以及单独分类管理的 `tests/test_tls_connector_early_data_contract.pas`

- 所以下一条最合理的 `sslCtxClient` behavior migration RED 已再次前移：
  - 首选应转向 `tests/test_tls_connector_hostname_override_precedence.pas`
  - 然后再评估 `tests/test_tls_connector_early_data_contract.pas` 是否还需要继续以 inherited context fallback 作为 intentional 输入

- 这条 connector override precedence 契约现在也已经脱离了 inherited context fallback 输入：
  - `tests/test_tls_connector_hostname_override_precedence.pas` 不再需要先做 `Ctx.SetServerName('ctx.example.com')`
  - 它真正锁住的只是：
    - 非空 override 仍然胜出
    - 空 override 仍然保持空字符串
  - 这说明 connector 本身作为高层门面，已经可以完全独立地证明自己的 per-connection hostname override 语义

- focused evidence 也说明这批只是测试/合同真相同步，没有新的生产实现变更：
  - `bash tests/scripts/test_tls_connector_override_no_context_level_sni_guidance.sh` PASS
  - `tests/test_tls_connector_hostname_override_precedence.pas` PASS
  - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh` PASS

- 因而当前剩余最直接的 connector-side intentional compatibility 输入再次收窄：
  - `tests/test_tls_connector_early_data_contract.pas`
  - 以及服务端兼容语义的 `tests/test_context_builder_server_servername_runtime_consistency.pas`

- 所以下一条最合理的 `sslCtxClient` behavior migration RED 已再次前移：
  - 首选应转向 `tests/test_tls_connector_early_data_contract.pas`
  - 再决定 server-side compatibility control case 何时从当前 intentional 集合中拆开

- 这条 connector early-data contract 现在也已经脱离了 inherited context fallback 输入：
  - `tests/test_tls_connector_early_data_contract.pas` 不再需要先做 `Ctx.SetServerName('ctx.example.com')`
  - 它真正锁住的是：
    - session 先被应用
    - 显式 server name 被写到连接
    - early data 在 connect 前排队
    - unsupported early-data 路径继续返回既有错误语义
  - 这说明 connector 的 early-data convenience surface 和 override precedence 一样，都已经可以完全独立地证明自己的 per-connection hostname 语义

- focused evidence 同样说明这批只是测试/合同真相同步，没有新的生产实现变更：
  - `bash tests/scripts/test_tls_connector_early_data_no_context_level_sni_guidance.sh` PASS
  - `tests/test_tls_connector_early_data_contract.pas` PASS
  - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh` PASS

- 因而当前剩余显式 intentional compatibility label 集合已经收窄到最后一个服务端控制案例：
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`

- 所以下一条最合理的 bounded review 已再次前移：
  - 首选应转向 `tests/test_context_builder_server_servername_runtime_consistency.pas`
  - 再决定 public compatibility surface 还能保留到什么边界

- 但对这条 server-side control case 做 live focused retest 后，先暴露出来的反而是三份 FreePascal-focused contracts 已经失真：
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
  - `tests/test_factory_server_name_scope_clarification.pas`
  - `tests/test_factory_config_server_name_isolation.pas`
  - 它们都还在教 “deprecated context-level ServerName 会被 FreePascal 新连接继承”

- 这与当前 live runtime truth 冲突：
  - `src/fafafa.ssl.freepascal.connection.pas` 之前已经切掉 socket / stream client constructor 的 inherited context fallback
  - 所以当前 FreePascal 真相是：
    - context state 仍会保留
    - client connection 不再自动继承

- 这说明当前最先需要修的不是 direct server-context control case 本身，而是把这些 focused contracts 拉回真相：
  - 让它们继续覆盖 deprecated context state 是否还存在
  - 但停止错误宣称 FreePascal connection 仍会继承旧 fallback

- 这也重新排序了下一批主线：
  - 真正剩下的高价值实现问题已经前移到 shared shim 的其余四个 backend：
    - OpenSSL
    - WolfSSL
    - MbedTLS
    - WinSSL
  - 需要决定它们是否也应统一切到 no-inheritance

- dedicated cross-backend RED 已经把这条 shared shim 分歧真正钉死：
  - `tests/test_cross_backend_client_context_server_name_clarification.pas`
    初始明确证明：
    - FreePascal 新 client connection 已经是空 `ServerName`
    - OpenSSL / WolfSSL / MbedTLS 仍会继承 `"client.example.com"`
    - WinSSL 在当前 Linux host 上因为 backend unavailable 被跳过，但源码仍走 shared helper
  - 所以这不是文档误判，而是当时真实存在的跨 backend runtime 分歧

- 当前这条分歧已经被最小实现改动收掉：
  - `src/fafafa.ssl.context.compat.pas`
    不再读取 deprecated context-level `GetServerName`
  - helper 现在保留为 shared seam，但对任意非空 context 一律返回 `''`
  - 这让 OpenSSL / WolfSSL / MbedTLS / WinSSL 与 FreePascal 统一进入 no-inheritance 规则

- 这也暴露出一个工作流层面的真实教训：
  - `tests/scripts/test_context_server_name_compat_shim_contract.sh`
    曾经正确地守住 “五个 backend 都走 shared helper”
  - 但在 FreePascal 先行切到 no-inheritance 之后，它变成了过时契约，开始错误阻塞当前批次
  - 当前已把它改回当前真相：
    - shared helper 只要求出现在 OpenSSL / WolfSSL / MbedTLS / WinSSL
    - FreePascal 明确禁止再走 helper
    - helper 与所有 backend 都禁止直接读 `(AContext|FContext).GetServerName`

- 因而 `context-level ServerName` 主线的阻塞点再次前移：
  - “shared client fallback divergence” 已不再是未决问题
  - 当前剩下的更尖锐问题回到了最后一个 direct server-context legacy-state control case：
    - 高层 builder / factory 是否还要继续保留 context state 可见性
    - 即便这份 state 已经不再对任何新 client connection 产生 inherited fallback

- 这条最后一个 direct server-context legacy-state control case 现在也已经完成收口：
  - `src/fafafa.ssl.context.builder.pas`
    的 `BuildClient` 不再把 `WithSNI(...)` 写回 built client context
  - `src/fafafa.ssl.factory.pas`
    的 client default-config / one-shot `CreateContext(...)` 路径
    不再把 `TSSLConfig.ServerName` 写回新建 context
  - warning 文案也同步切成：
    - `BuildClient ignores it...`
    - `CreateContext ignores it for new contexts...`

- 这意味着 deprecated context-level `ServerName` 已不再通过任何高层新建入口流入新的 context state：
  - builder 高层入口不会再保留它
  - factory 高层入口不会再保留它
  - 所有 client backend 也早已不再把它继承进新 client connection
  - 当前剩下的最后 compatibility surface，已经只剩 direct `ISSLContext.SetServerName/GetServerName` 本身和显式 API-surface coverage

- focused evidence 也说明这不是文案改动，而是真正的行为收口：
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
    PASS (`6 passed, 0 failed`)
  - `tests/test_factory_server_name_scope_clarification.pas`
    PASS (`6 passed, 0 failed`)
  - `tests/test_factory_config_server_name_isolation.pas`
    PASS (`6 passed, 0 failed`)
  - `tests/test_factory_server_name_compatibility_warning.pas`
    PASS (`16 passed, 0 failed`)
  - `tests/config/test_config_validation.pas`
    PASS (`53 passed, 0 failed`)
  - `tests/test_cross_backend_client_context_server_name_clarification.pas`
    PASS (`20 passed, 0 failed, 1 skipped`)

- 因而当前下一条最有价值的路线已经不再是“builder / factory 还要不要继续保留 context state”：
  - 这个问题已经被代码和 focused regressions 一起回答为“不再保留”
  - 现在真正该进入的是 final public surface cleanup prep：
    - `TSSLConfig.ServerName` 是否继续保留当前字段位置
    - `WithSNI(...)` 是否继续保留当前命名/入口
    - direct `ISSLContext.SetServerName/GetServerName` 这条最后 compatibility surface 未来如何降级/替代

- 继续往下摸 public surface 时，又暴露出一个此前没被 focused 合同覆盖的漏口：
  - `src/fafafa.ssl.openssl.backed.pas`
    的 `TOpenSSLLibrary.CreateContext(...)`
    仍会把 `FDefaultConfig.ServerName` 写回新建 context
  - 这不是 generic factory 残留，而是 backend-specific direct library 入口自己的漂移
  - 其余 `freepascal` / `mbedtls` / `wolfssl` / `winssl` library `CreateContext(...)` 当前并没有这条 `ServerName` 注入逻辑

- 这意味着当时的“high-level write surfaces 已全部收口”还差最后一块：
  - generic factory 已经是 `warning + ignore`
  - builder 已经是 `warning + ignore`
  - 但 direct OpenSSL library default-config path 还停留在 “默默写回 context”

- 当前这条 OpenSSL direct-library 漏口也已经被收掉：
  - `TOpenSSLLibrary.CreateContext(sslCtxClient)`
    - 不再把 `FDefaultConfig.ServerName` 写回 built context
    - 若配置了 library log callback，会发出明确 compatibility warning
  - `TOpenSSLLibrary.CreateContext(sslCtxServer)`
    - 若 default-config 带 `ServerName`，现在会 fail-fast 抛 `ESSLConfigurationException`
  - server misuse 的 reject 也已经前移到真正的 fail-fast：
    - 不再先创建 context 再抛错

- focused evidence 说明这次不是只改了 OpenSSL 文案，而是补上了一个真实未覆盖实现面：
  - 新增 `tests/test_openssl_library_default_config_server_name_clarification.pas`
    - RED (`3 passed, 8 failed`) -> GREEN (`13 passed, 0 failed`)
    - 直接钉住 client ignore+warning、server reject、no-ServerName quiet
  - 邻接 retest：
    - `tests/test_cross_backend_client_context_server_name_clarification.pas`
      PASS (`20 passed, 0 failed, 1 skipped`)
    - 说明这次 direct library 对齐没有碰坏当前 cross-backend no-inheritance 真相

- 因而当前路线图又收紧了一层：
  - “high-level write surfaces” 现在不仅包括 builder / generic factory
  - 也包括 direct OpenSSL library default-config path，且这几条都已经不再把 deprecated `ServerName` 流入新 context
  - 剩下的 public-surface 主问题，确实只剩最后的 compatibility API 形状，而不是还有某个 backend-specific 高层入口继续偷写旧 state

- 当主线进入 final public surface cleanup prep 后，新的高价值问题已经不是 backend 行为，而是工作流漂移：
  - `tests/test_quick.pas` 这种普通 smoke 还在顺手示范 `.WithSNI('example.com')`
  - `tests/winssl/test_winssl_connection_edge_cases.pas` 这种普通 edge-case 也还在顺手写 `LConfig.ServerName := ...`
  - 这些命中不再承担 compatibility 行为断言，却会持续把 deprecated builder/config surface 伪装成正常主路径

- 因而 public-surface prep 的第一刀应该是“先做静态分类，再谈 API 形状”：
  - 普通测试里的旧入口示范要迁掉
  - 真正还需要保留 `WithSNI(...)` / `TSSLConfig.ServerName` 的文件，要显式标成 `INTENTIONAL_COMPAT`
  - 否则每次重新审查时，都会被这些普通测试文本重新拉回“是不是还有 runtime 主路径”的旧问题

- 新增的 `tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh` 已经把这条工作流护栏固化：
  - allowlist compatibility tests 必须带 `INTENTIONAL_COMPAT`
  - active `tests/*.pas` 里若重新出现 `.WithSNI(...)` 或 builder-config `ServerName :=`，会直接红灯

- 这也把“下一步该做什么”压缩得更清楚：
  - 现在已经不需要继续排 ordinary test guidance
  - 下一步可以直接讨论最终 API 形状：
    - `TSSLConfig.ServerName`
    - `WithSNI(...)`
    - direct `ISSLContext.SetServerName/GetServerName`

- 继续把 active direct-context `SetServerName(...)` 命中全盘出来后，又确认了一个此前缺少 repo-level 合同的空档：
  - `tests/test_cross_backend_client_context_server_name_clarification.pas`
  - `tests/test_sslctxboth_client_capability_clarification.pas`
  - `tests/test_connection_builder_hostname_precedence.pas`
    这些文件实际上都在故意保留 direct-context legacy input
  - 但在本批之前，它们还没有像 WinSSL comprehensive / backend framework / diagnostic/security files 那样，被统一纳入 active-surface 分类合同

- 这意味着当时 direct context surface 还不算真正“收口”：
  - 旧命中虽然多数已经带局部注释
  - 但 repo 还没有一个 focused truth 能回答：
    - “active tests 里到底哪些 direct-context `SetServerName(...)` 是允许的？”
    - “它们是 compatibility 还是 API-surface coverage？”

- 新增的 `tests/scripts/test_active_direct_context_servername_surface_classification_contract.sh` 已经把这个空档补上：
  - 它枚举 active tests 里所有 real direct-context `SetServerName(...)` 文件
  - 每个文件都必须带正确标签：
    - `INTENTIONAL_COMPAT`
    - 或 `INTENTIONAL_API_SURFACE`
  - allowlist 外若重新出现 direct context setter，会直接红灯

- 因而到当前为止，public compatibility surface 的测试面已经分成两层稳定护栏：
  - builder/config compatibility-only surface：
    - `tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
  - direct-context compatibility/API-surface：
    - `tests/scripts/test_active_direct_context_servername_surface_classification_contract.sh`

- 再往前一步后，还确认了一条很实用的工作流细节：
  - 即便 active direct-context hits 已被分类，如果 intentional compatibility tests 不做局部 warning quarantine
  - focused compile 时仍会冒出我们明知故意保留的 deprecated getter/setter warning
  - 这会让后续验证输出继续混着“已知旧 API 噪音”和“真正新的实现 warning”

- 当前已经把这批 intentional compatibility tests 的 direct-context getter/setter 都包进局部 suppression：
  - `tests/test_cross_backend_client_context_server_name_clarification.pas`
  - `tests/test_sslctxboth_client_capability_clarification.pas`
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
  - 结果是 focused compile 里不再反复提示这几处已知 intentional deprecated surface

- 这也让后续审查信号更干净：
  - 剩下的 compile warnings 更接近真正值得继续治理的实现/类型问题
  - 而不是被我们有意保留的 compatibility API 使用反复刷屏

- 这进一步确认了路线已经真正前移：
  - 现在不再需要继续做测试面排污或分类普查
  - 下一步的最高价值工作已经纯粹是最终 API 形状决策，而不是再找“还有没有哪个文件偷偷示范旧入口”

- `WithSNI(...)` 在这之前还留着最后一个源码层 truth 漏口：
  - runtime warning、validation wording、API 文档都已经把它降格成 compatibility-only
  - 但 public declaration 自身还不是 compiler `deprecated`
  - 这会让源码使用者在编译期看不到和文档/runtime 一致的信号

- 当前这条漏口也已经被最小收口：
  - `ISSLContextBuilder.WithSNI(...)`
  - `TSSLContextBuilderImpl.WithSNI(...)`
    都已挂上同一条 compiler `deprecated` message：
    `Use per-connection hostname via TSSLConnectionBuilder.WithHostname or ISSLClientConnection.SetServerName`
  - 新增 `tests/scripts/test_withsni_compiler_deprecated_contract.sh`
    直接守住 declaration-level truth

- 这次修法也顺手确认了一条工作流事实：
  - intentional compatibility tests 继续保留 `.WithSNI(...)` 是合理的
  - 但如果不做局部 warning quarantine，focused compile 输出会继续混入我们已知接受的 compiler deprecation 噪音
  - 因而当前对 `.WithSNI(...)` 的剩余测试使用，应一律视为“故意覆盖 compatibility surface”，不是普通 fluent builder 示例

- 因此 `WithSNI(...)` 这半边已经不再属于“语义还没收实”的范围：
  - 它现在同时具备：
    - compatibility-only 文档定位
    - runtime warning + ignore
    - compiler-level deprecated truth
    - explicit compatibility-test classification
  - 剩下真正未决的，只是最终 public surface 是否继续保留它当前的命名/挂载位置

- 对 `TSSLConfig.ServerName` 的最新静态审查进一步说明：
  - 当前已经不存在“还有某条高层 runtime path 会偷偷消费它”的实现漏口
  - generic factory、OpenSSL direct-library、ordinary tests、active docs guidance 都已经被收干净
  - 真正剩下的问题不是行为真相，而是要不要在 `v1.x` 直接改掉它的字段位置/命名

- 当前最稳妥的 `v1.x` 设计决定已经明确：
  - 不在当前版本线直接移除或改名 `TSSLConfig.ServerName`
  - 保持 source compatibility
  - 但把它冻结成一个“仅剩 compatibility truth 的 record field”

- 这条 `v1.x freeze` 现在也不再只是口头结论：
  - `src/fafafa.ssl.base.pas` 字段注释明确指向 per-connection `ISSLClientConnection.SetServerName`
  - `src/fafafa.ssl.factory.pas` 与 `src/fafafa.ssl.openssl.backed.pas` 的 warning wording 都继续点名 `TSSLConfig.ServerName`
  - `docs/reference/API_REFERENCE.md` 现在不只在顶层 compatibility note 说明它，还在 `Use TSSLConfig with TSSLFactory.CreateContext(...)` 段落旁边明确写出 client-side warning + ignore truth
  - 新增 `tests/scripts/test_tsslconfig_servername_surface_truth_contract.sh`
    把 source comment、warning wording、以及 active docs confinement 一起钉住

- 因而 `TSSLConfig.ServerName` 这条线对当前主路线的意义已经变化：
  - 它不再是“下一刀要不要删/改”的首要候选
  - 它已经被降成 `v1.x` compatibility-only frozen surface
  - 下一步真正该继续收口的，已经前移到 direct `ISSLContext.SetServerName/GetServerName` 这组最后的 context-level compatibility API

- 对 direct `ISSLContext.SetServerName/GetServerName` 的最新静态审查也已经给出同样清晰的结论：
  - 这组 API 仍然存在于 public interface，并由各 backend context 实现
  - 但 production `src/` 已经不再存在真实 direct context caller
  - active docs 也不再把 `Ctx.SetServerName(...)` 当普通 client 流指导路径

- 因而当前最稳妥的 `v1.x` 设计决定也已经明确：
  - 不在当前版本线直接移除这组 deprecated context API
  - 保持 source compatibility
  - 但把它们冻结成“deprecated but still present”的 compatibility-only context surface

- 这条 direct-context `v1.x freeze` 现在已经被 source/doc contract 固化：
  - `src/fafafa.ssl.base.pas` 的 deprecation message 继续统一指向 `ISSLClientConnection.Set/GetServerName`
  - 新增 `tests/scripts/test_direct_context_servername_surface_truth_contract.sh`
    钉住：
    - deprecated declaration message
    - production source 无 direct context caller
    - active docs 无 direct context setter guidance
  - 既有 `tests/scripts/test_active_direct_context_servername_surface_classification_contract.sh`
    与 `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
    则继续守住 active tests 的 intentional coverage 边界

- 这也让主路线再次前移：
  - `TSSLConfig.ServerName` 已 frozen
  - direct context API 已 frozen
  - 当前最后仍值得继续讨论 public shape 的，只剩 `WithSNI(...)` 的命名/挂载位置

- `WithSNI(...)` 的最终静态审查结果也已经足够清楚：
  - source 中只剩两处 declaration + 一处 implementation 命中
  - active docs 现在也只剩 `docs/reference/API_REFERENCE.md` 以 compatibility note 形式提及它
  - active tests 里的 `.WithSNI(...)` 命中已经被 `tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh` 限制在 allowlist compatibility coverage

- 因而当前最稳妥的 `v1.x` 设计决定同样已经明确：
  - 不在当前版本线直接移除或改挂 `WithSNI(...)`
  - 保持 source compatibility
  - 但把它冻结成“deprecated but still present”的 compatibility-only fluent surface

- 这条 `WithSNI` `v1.x freeze` 现在已经被 source/doc/test contract 固化：
  - `tests/scripts/test_withsni_compiler_deprecated_contract.sh`
    守住 compiler deprecation truth
  - 新增 `tests/scripts/test_withsni_surface_truth_contract.sh`
    守住：
    - source compatibility comment
    - active docs confinement
    - source hits 不得扩散出当前 declaration/implementation 边界
  - 既有 `tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
    继续守住 active tests allowlist

- 这意味着当前版本线里的整个 `context-level SNI` compatibility family 已经全部 frozen：
  - `TSSLConfig.ServerName`
  - direct `ISSLContext.SetServerName/GetServerName`
  - `TSSLContextBuilder.WithSNI(...)`

- 因而下一批最值得做的事不再是继续绕着 SNI 旧兼容语义打转，而是回到更大的 interface-design debt：
  - `TSSLConfig` 跨层字段拆分 / slimming
  - `ISSLConnection` 核心 surface slimming

- 对 post-SNI 路线的最新筛选也已经有了更清楚的优先级：
  - `TSSLConfig` 路线已经积累了足够多的 scope truth：
    - `BufferSize` / `HandshakeTimeout` = connection-scoped
    - `LogLevel` / `LogCallback` = library-scoped
    - 多个 option-style 字段仍承担 compatibility bridge
  - 这意味着它更适合先做“field buckets + slimming roadmap”的 bounded batch

- 相比之下，`ISSLConnection` 核心 surface slimming 现在仍然更像下一阶段的大手术：
  - public interface 影响面更广
  - 会直接打到各 backend connection 实现与大量 tests/helpers
  - 如果马上动手，风险明显高于先做 `TSSLConfig` 路线

- 因而当前最值得执行的 post-SNI 第一条主线是：
  - 先把 `TSSLConfig` 做成明确分桶的跨层字段 roadmap
  - 再决定是否以及如何进入 `ISSLConnection` core surface slimming

- `TSSLConfig` 的 mixed-scope truth 现在已经可以稳定分成 5 个 buckets：
  - `library-scoped defaults`
    - `LogLevel`
    - `LogCallback`
  - `context-scoped`
    - `SessionCacheSize`
    - `SessionTimeout`
    - `ALPNProtocols`
    - `ClientEarlyDataEnabled`
    - `ServerEarlyDataPolicy`
    - `ServerMaxEarlyDataSize`
    - `ServerEarlyDataReplayStoreFile`
    - `ServerEarlyDataReplayStoreDirectory`
  - `connection-scoped`
    - `HandshakeTimeout`
    - `BufferSize`
  - `compatibility-only`
    - `ServerName`
  - `option-bridge`
    - `EnableCompression`
    - `EnableSessionTickets`
    - `EnableOCSPStapling`

- 这份 bucket truth 现在已经落进 durable source/doc surface：
  - `src/fafafa.ssl.base.pas`
    关键 mixed-scope 字段注释不再是泛泛中文描述，而是直接带 scope truth
  - `docs/reference/API_REFERENCE.md`
    新增 `TSSLConfig Scope Buckets` 段，避免后续审查再反复翻 factory/backend source
  - `tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
    用 focused contract 固定 source/doc/factory/OpenSSL direct-path 的 truth

- 当前这一轮静态横查还额外暴露出一个更实质的 backend parity risk：
  - `ISSLLibrary.CreateContext(AType)` 的 default-config 套用在各 backend 间并不一致
  - OpenSSL direct-library path：
    - 在 `TOpenSSLLibrary.CreateContext` 中显式应用：
      - `SessionCacheSize`
      - `SessionTimeout`
      - `ALPNProtocols`
    - 也显式处理 deprecated `ServerName` 的 warning/reject truth
  - WinSSL direct-library path：
    - 当前只看到 `Options` 被显式套用
  - FreePascal / MbedTLS / WolfSSL direct-library path：
    - 当前静态上只看到直接创建 context
    - 没看到 parallel 的 default-config replay/apply block

- 由于这些 backend 同时都满足下面两点，这个差异目前更像真实实现缺口，而不是“字段本来没打算支持”：
  - library side 都持有 `FDefaultConfig`
  - context side 都公开并维护：
    - `SessionCacheSize`
    - `SessionTimeout`
    - `ALPNProtocols`

- 因而当前最优先的下一批不该直接跳去 `ISSLConnection` slimming，而应先做：
  - `direct-library default-config parity audit/fix`
  - 先把 `ISSLLibrary.CreateContext(AType)` 在各 backend 的 default-config 套用 truth 守住
  - 再继续 broader `TSSLConfig` slimming 或 `ISSLConnection` surgery

- `direct-library default-config parity audit/fix` 现在已经完成第一轮收口：
  - runtime RED 已真实出现：
    - `tests/test_direct_library_default_config_parity.pas`
      在修复前证明 FreePascal direct-library `CreateContext(sslCtxClient)` 没有套用：
      - `ProtocolVersions`
      - `VerifyMode`
      - `VerifyDepth`
      - `CipherList`
      - `CipherSuites`
      - `SessionCacheSize`
      - `SessionTimeout`
      - `ALPNProtocols`
      - option-bridge normalized `Options`
  - source RED 也真实出现：
    - `tests/scripts/test_direct_library_default_config_parity_contract.sh`
      在修复前证明 `freepascal` library unit 甚至还没有先 normalize `SetDefaultConfig(...)`

- 当前已修正的实现 truth：
  - `TFreePascalSSLLibrary`
  - `TWinSSLLibrary`
  - `TMbedTLSLibrary`
  - `TWolfSSLLibrary`
  - 以上 4 个 backend library units 现在都已：
    - 在 `SetDefaultConfig(...)` 里先调用 `TSSLFactory.NormalizeConfig(...)`
    - 在 direct-library `CreateContext(AType)` 里显式套用 context-safe 默认字段

- 这轮 direct-library parity 现在覆盖的字段是：
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

- 这轮没有混入的未收口点，也因此成为下一优先级：
  - `ServerName` compatibility warning/reject parity
    - 目前 OpenSSL direct-library path 仍然更完整
  - early-data / replay-store direct-library parity
    - 当前仍主要以 factory/context path 为真相源

- 因而当前更准确的 next queue 是：
  - 先继续做 direct-library special-case parity
  - 再决定是否进入 broader `TSSLConfig` slimming 或 `ISSLConnection` surgery

- `direct-library ServerName compatibility parity` 现在也已经完成第一轮收口：
  - RED 证据：
    - `tests/scripts/test_direct_library_servername_compatibility_contract.sh`
      初次运行即证明 `freepascal` library unit 还没有：
      - server reject
      - client warning + ignore
    - `tests/test_freepascal_library_default_config_server_name_clarification.pas`
      初次运行即证明 FreePascal direct-library path 当前只是静默忽略 client `ServerName`，server 也不 reject

- 当前已修正的实现 truth：
  - `TFreePascalSSLLibrary`
  - `TWinSSLLibrary`
  - `TMbedTLSLibrary`
  - `TWolfSSLLibrary`
  - 以上 4 个 backend library units 现在都已对齐 OpenSSL 的这条专门兼容语义：
    - client default-config = warning + ignore
    - server default-config = reject

- 这一轮 direct-library `ServerName` parity 没有再回退去“恢复 context-level SNI 正常主路径”：
  - context 上的 `GetServerName = ''` 仍保持 compatibility-only truth
  - warning message 仍明确要求迁移到：
    - `ISSLClientConnection.SetServerName`
    - `TSSLConnector.Connect*(..., ServerName)`

- 因而当前 direct-library special-case parity 的剩余重点已经进一步缩窄为：
  - early-data / replay-store direct-library parity
  - 这应当是下一条高价值、边界依然清楚的小批次

- `direct-library early-data / replay-store parity` 现在也已经完成第一轮收口：
  - RED 证据：
    - `tests/scripts/test_direct_library_early_data_replay_store_parity_contract.sh`
      初次运行即证明 5 个 backend library path 还没统一接 replay-store scope 校验与 early-data/replay-store apply helper
    - `tests/test_direct_library_early_data_replay_store_parity.pas`
      初次运行即证明 FreePascal direct-library path 当前还没有：
      - 应用 `ClientEarlyDataEnabled`
      - 应用 `ServerEarlyDataPolicy` / `ServerMaxEarlyDataSize`
      - 安装 replay-store file / directory
      - 拒绝 client replay-store config
      - 拒绝 conflicting replay-store file + directory

- 这轮 production fix 刻意没有把逻辑再复制进 5 份 backend：
  - 新增 `src/fafafa.ssl.context.config.pas`
  - 先把 replay-store client/server scope 校验、early-data context apply、replay-store installer apply 收成 shared internal helper
  - 再让 `TOpenSSLLibrary` / `TFreePascalSSLLibrary` / `TWinSSLLibrary` / `TMbedTLSLibrary` / `TWolfSSLLibrary`
    的 `CreateContext(AType)` 一起接回这条 helper

- 这也把 direct-library path 和 factory/context path 的关系重新说清楚了：
  - client path：
    - `ClientEarlyDataEnabled` 若 backend 暴露 `ISSLEarlyDataContext`，就会应用
    - replay-store file / directory 继续 fail-fast reject
  - server path：
    - `ServerEarlyDataPolicy` / `ServerMaxEarlyDataSize` 若 backend 暴露 `ISSLEarlyDataContext`，就会应用
    - replay-store file / directory 保持 mutually exclusive
    - 若 backend 不实现 installer seam，则保持 fail-fast，而不是静默忽略

- 当前 FreePascal runtime 已给出完整的 live proof：
  - direct-library client context 会正确反映 `ClientEarlyDataEnabled`
  - direct-library server context 会正确反映 `ServerEarlyDataPolicy` / `ServerMaxEarlyDataSize`
  - replay-store file / directory 都会真实 materialize 到配置路径
  - cross-context replay rejection 继续成立
  - client replay-store config 与 conflicting file+directory 都会抛出 `ESSLConfigurationException`

- 因而 direct-library special-case parity 当前已经全部收口：
  - `default-config`
  - deprecated `ServerName`
  - `early-data / replay-store`
  - 下一条路线不该再回到 “继续补 direct-library 小口子”，而应回到 broader interface debt 的选择：
    - `TSSLConfig` option-bridge freeze / slimming
    - 或 `ISSLConnection` 核心 surface slimming roadmap

- `TSSLConfig option-bridge default truth parity` 这轮也已经被进一步缩到一个更准确的根因：
  - 初看像是：
    - `CreateDefaultConfig(...)` 单点丢了 `EnableSessionTickets`
  - 但继续把测试缩细后确认：
    - direct `CreateFreePascalSSLLibrary` 路径是对的
    - `Lib.SetDefaultConfig(Lib.GetDefaultConfig)` direct-library round-trip 也是对的
    - 真正错误的是 factory-held backend instance 的 `GetDefaultConfig(...)`
  - 这说明问题不只在 `NormalizeConfig(...)` 或某个 public helper，而在“生产实例化路径是否真的保留了 backend constructor truth”

- 新增的 runtime narrowing 已经把这条根因钉实：
  - `TSSLFactory.GetLibrary(sslFreePascal).GetDefaultConfig`
    在修复前就已经丢了 `EnableSessionTickets = True`
  - `TSSLFactory.GetLibrary(sslAutoDetect).GetDefaultConfig`
    在 `SetDefaultLibrary(sslFreePascal)` 后同样丢失
  - 因而 `CreateDefaultConfig(...)` 的失败只是 downstream symptom，不是 upstream source

- 本批最终确认的实现问题是：
  - `factory` 对真实 backend 仍主要依赖 raw registered-class instantiation
  - 这条 path 不足以保住 backend constructor 内建立的 `FDefaultConfig` 真相
  - 所以即便 source 里各 backend constructor 已经补了：
    - `TSSLFactory.NormalizeConfig(FDefaultConfig)`
    - FreePascal `EnableSessionTickets := True`
    - 生产实例化得到的 library defaults 仍可能失真

- 当前修法没有去赌 Pascal metaclass semantics，也没有再把默认配置逻辑复制一份到 factory：
  - `TSSLFactory` 新增 explicit creator-function registration path
  - `TSSLLibraryRegistration` 现在允许直接存 `CreateFunc`
  - `CreateLibraryInstance(...)` 优先走 `CreateFunc`
  - `openssl` / `freepascal` / `winssl` / `mbedtls` / `wolfssl`
    的真实 backend 注册统一改成 `@Create*SSLLibrary`
  - 这样 factory-held instance 与 direct-library instance 回到了同一条 backend-owned constructor truth

- 这也纠正了原本 plan 里的一个误导点：
  - “多个 backend library constructor 仍是未归一化 mixed truth” 只说对了一半
  - 更准确的说法应是：
    - constructor normalization 的确必要
    - 但如果生产实例化路径不走 backend creator truth，fresh default-config surface 依然会漂移

- 当前 focused evidence 已经闭环：
  - `tests/test_tsslconfig_option_bridge_default_truth.pas`
    - PASS
    - 直接覆盖：
      - direct library default-config truth
      - factory-held `GetDefaultConfig(...)`
      - auto-detect `GetDefaultConfig(...)`
      - `CreateDefaultConfig(...)`
  - `tests/config/test_default_config.pas`
    - PASS
    - 说明既有 `CreateDefaultConfig(...)` baseline 没被这次 creator-path fix 打穿
  - `tests/scripts/test_tsslconfig_option_bridge_default_truth_contract.sh`
    - PASS
    - 现在同时守住：
      - constructor normalization
      - backend 注册必须走 explicit creator function

- 因而这条线的 next queue 也变得更清楚：
  - 不需要再反复怀疑 factory-held default-config 是否 stale
  - 之后若继续推进，应讨论：
    - `Options vs legacy booleans` 冲突优先级是否要进一步单真相化
    - `TSSLConfig` option-bridge surface 是否要继续 freeze/slim

- `TSSLConfig option-bridge precedence` 这轮也已经从“源码隐含行为”升级成了明确 contract：
  - 当前 `v1.x` truth 不是 “`Options` 总是权威输入”
  - 也不是 “legacy booleans 已经只剩只读投影”
  - 更准确的 truth 是：
    - legacy booleans 仍是兼容写入口
    - 当调用方传入冲突的 `Options` 和 legacy booleans 时，legacy booleans 赢
    - normalization 先把 legacy booleans 写入相关 option bit
    - 再把最终 `Options` truth 回投到这三个 compatibility booleans

- 这条结论不是纸面推理，而是现在已经有 focused production evidence：
  - `TSSLFactory.NormalizeConfig(...)`
    - 直接覆盖 conflict input
  - `TSSLFactory.CreateContext(const AConfig)`
    - 证明 one-shot factory path 跟随同一条 precedence truth
  - `ISSLLibrary.SetDefaultConfig(...)` / `ISSLLibrary.CreateContext(AType)`
    - 证明 direct-library path 也跟随同一条 precedence truth

- 继续做 source search 后，这条线还多确认了一个关键背景：
  - production code 里，真正会写这三个 legacy booleans 的地方已经非常集中：
    - backend default-config constructors
    - `CreateDefaultConfig(...)`
    - `TSSLFactory.NormalizeConfig(...)`
  - builder/import-export/config snapshot 等活跃高层 surface 实际上主要围绕 `Options`
  - 这意味着当前“legacy booleans 仍是 compatibility write surface”这件事，边界已经足够清晰，不再是散落在各处的隐藏入口

- 因而这轮之后，`Options vs legacy booleans` 不再是一个“未定义设计问题”，而是一个“已冻结的 `v1.x` compatibility contract”：
  - 现在该问的已经不是：
    - 冲突时到底谁赢？
  - 而是：
    - 未来要不要把这组 legacy booleans 继续缩成更窄的 compatibility-only surface
    - 若要缩，应该走什么非破坏性的 migration path

- 这也让总体路线图更清楚了一步：
  - `TSSLConfig` 这条主线当前已完成：
    - scope buckets
    - fresh default-config truth parity
    - conflict precedence freeze
  - 下一条更值得开的批次，不再是继续补 “option-bridge 真相”
  - 而是：
    - `TSSLConfig` public-surface slimming / migration roadmap
    - 或再往后才考虑 `ISSLConnection` 核心 surface slimming

- `TSSLConfig option-bridge surface` 这轮继续往前收后，public truth 也终于不再停留在“行为已经冻结，但表达还松”的状态：
  - `src/fafafa.ssl.base.pas` 现在明确把
    - `EnableCompression`
    - `EnableSessionTickets`
    - `EnableOCSPStapling`
    定义为 compatibility-only option-bridge flags，并直接提示新代码优先写 `Options`
  - `docs/reference/API_REFERENCE.md` 也同步改成同一套 public-facing truth：
    - 这三个字段是历史 compatibility 写入口
    - factory / direct-library default-config path 会先把它们折叠进 `Options`
    - fresh default-config surfaces 返回时也必须保持 boolean 与最终 `Options` 真相一致

- 这轮同时暴露了一个比“文案松”更具体的问题：
  - `tests/security/test_session_security.pas` 原本不是 compatibility coverage，却还在通过 `EnableSessionTickets := ...` 驱动语义
  - 这会继续把 legacy boolean 教成普通主路径，也会和已经冻结的 “legacy boolean 优先于冲突 `Options`” 规则纠缠在一起
  - 当前已经把这条活跃安全测试改成直接覆盖 context `SetOptions(...)` / `GetOptions(...)` 主路径

- 与此对应，仍然故意覆盖 compatibility surface 的测试也已经显式化：
  - `tests/test_factory_logic.pas`
  - `tests/test_data_structures.pas`
  - `tests/test_tsslconfig_option_bridge_default_truth.pas`
  - `tests/test_tsslconfig_option_bridge_precedence_freeze.pas`
  - `tests/test_direct_library_default_config_parity.pas`
  - 它们现在都明确说明自己是在保留 option-bridge compatibility coverage，而不是继续把这组字段当普通推荐 API

- 这轮还有一个工作流层面的 live 教训值得保留：
  - 当 public wording 被收紧后，旧 contract 脚本会先因为盯旧文案而报假红灯
  - 这次 `test_tsslconfig_scope_bucket_truth_contract.sh`
    / `test_tsslconfig_option_bridge_default_truth_contract.sh`
    / `test_tsslconfig_option_bridge_precedence_freeze_contract.sh`
    都一起对齐到了新的 wording truth
  - 这比“重新跑更多重型验证”更有价值，因为它直接消掉了后续重复拉起的噪音源

- 因而当前 `TSSLConfig` 这条线的 next queue 已经进一步收敛：
  - 不需要再反复补
    - option-bridge precedence wording
    - fresh default-config wording
    - compatibility labels
  - 真正值得开的下一批，应是：
    - `TSSLConfig` public-surface slimming / migration design
    - 明确哪些 compatibility-only 字段在 `v2` 继续保留、改挂、还是迁到更窄入口

- 在 option-bridge surface 收紧之后，活跃指导面里又暴露出两条更直接的漂移：
  - `examples/example_factory_usage.pas`
    - 还在通过 `Config.BufferSize := ...` / `Config.HandshakeTimeout := ...`
      演示 `TSSLFactory.CreateContext(...)` 的配置写法
    - 但这两个字段当前早已被 factory 明确判定为 connection-scoped / transport-adjacent，不属于 context/factory 主路径
  - `docs/reference/ARCHITECTURE.md`
    - 还保留一段过时的伪 `TSSLConfig` 结构
    - 字段名例如 `DefaultLibraryType` / `ProtocolVersion` / `CertificatePath` / `ReadTimeout` / `WriteTimeout`
      已与当前 public source 明显脱节

- 这说明 `TSSLConfig` 当前不只存在“内部设计债”，还存在“高可见度用户入口仍在教旧模型”的问题：
  - 如果不先收掉这些 example/reference 漂移，后续即使开始做 slimming design，用户也还会继续从活跃入口学到 mixed-scope 旧写法

- 当前修法刻意保持在 guidance 层，不去碰 runtime：
  - `examples/example_factory_usage.pas`
    - 移除 `BufferSize` / `HandshakeTimeout` 的错误示例
    - 明确把 timeout 导向 `TSSLConnector.WithTimeout` / `ISSLConnection.SetTimeout`
    - 明确把 buffering 导向外围 socket / stream / transport 配置
  - `docs/reference/ARCHITECTURE.md`
    - 把“伪 record 结构”改成当前真实 scope buckets：
      - library-scoped defaults
      - context-scoped
      - connection-scoped
      - compatibility-only

- 与此同时，example-surface 上故意保留的 direct context API coverage 仍然保持显式分类：
  - `tests/examples/test_lib_core_functionality.pas`
    - 继续保留 `INTENTIONAL_API_SURFACE`
    - 这说明我们这轮修的是“活跃用户指导面”，不是把所有 direct context API 命中都误当成 bug

- 这轮的 focused evidence 也足够干净：
  - `tests/scripts/test_tsslconfig_active_guidance_truth_contract.sh`
    - PASS
    - 守住：
      - 活跃 example 不得再教 `BufferSize` / `HandshakeTimeout` factory/config 写法
      - 活跃 architecture reference 不得再回到过时伪结构
      - example-surface 的 direct context API coverage 继续显式带标签
  - `examples/example_factory_usage.pas`
    - focused compile PASS
    - 说明这次 guidance cleanup 没把示例代码本身改坏

- 因而现在 `TSSLConfig` 这条线又进一步少掉了一个常见重复入口：
  - 后续不该再回到
    - “example 里还在教错字段”
    - “architecture reference 里还是旧 record”
  - 真正值得开的下一批，已经更明确地只剩：
    - `TSSLConfig` public-surface slimming / migration design

- 这轮已经把 `TSSLConfig` 的 slimming / migration design 从“抽象建议”推进成了字段级决策：
  - `docs/reference/API_REFERENCE.md`
    - 新增 `TSSLConfig Migration Targets`
    - 把 mixed-scope / compatibility 字段逐一映射到当前推荐入口与 `v2` 方向
  - `docs/plans/2026-05-18-tsslconfig-public-surface-slimming-roadmap.md`
    - 把这份 map 落成了可执行 roadmap，而不是只留一句“以后再 slimming”

- 这份 migration matrix 当前已经明确了 4 条后续实现主线：
  - `LogLevel` / `LogCallback`
    - library defaults surface
  - `HandshakeTimeout` / `BufferSize`
    - connection / transport surface
  - `ServerName`
    - per-connection SNI surface
  - `EnableCompression` / `EnableSessionTickets` / `EnableOCSPStapling`
    - `Options` / builder `WithOption(...)`

- 这一步的价值在于：
  - 后续不需要每次先重做“这些字段该迁去哪”的分析
  - 真正的实现批次可以直接从这 4 条线里挑最小切片

- 当前最适合率先进入实现的，不再是 `ServerName` 或 option-bridge：
  - 这两条线虽然已经冻结 truth，但兼容历史更重
  - 更稳的第一刀应是 `LogLevel` / `LogCallback`：
    - 它们已经被 factory request path 明确拒绝
    - 替代入口也已经稳定存在于 `ISSLLibrary` defaults surface
    - 因而最适合作为 `TSSLConfig` slimming 的第一条真正实现切片

- 顺着这条 logging detachment 继续往下看后，这轮又挖出了一个真正会误导调用方的 active-doc bug：
  - `docs/guides/USER_GUIDE.md`
  - `docs/guides/TROUBLESHOOTING.md`
    都曾经只演示 `ISSLLibrary.SetLogCallback(...)`
    然后立刻调用 `LLib.Log(sslLogInfo, ...)`
  - 但当前 runtime truth 早已固定为：
    - default `LogLevel = sslLogError`
    - backend `Log(...)` 只有在 `ALevel <= configured LogLevel` 时才会 dispatch
  - 因而这些 snippet 不是“讲得不完整”，而是“照着写也看不到示例里的 info/debug 输出”

- 这也进一步澄清了 `LogLevel` / `LogCallback` 这两个字段在 public surface 上最容易失真的点：
  - `LogCallback` 的 owner 不等于 `LogLevel` 的 owner
  - 更准确的说法是：
    - `LogLevel` 通过 `ISSLLibrary.GetDefaultConfig(...)` / `SetDefaultConfig(...)` 调整
    - `LogCallback` 通过 `ISSLLibrary.SetLogCallback(...)` 安装
    - `CreateDefaultConfig(...)` / factory request path 继续回到 request-safe baseline
  - 之前 reference/guides 把这两个动作混成一句“设置日志回调”，正是 drift 的来源

- 当前修法刻意保持在 truth freeze 层，不动 runtime：
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/ARCHITECTURE.md`
    现在都明确拆开了 logging level 与 callback 的入口
  - `docs/guides/USER_GUIDE.md`
  - `docs/guides/TROUBLESHOOTING.md`
    在演示 `sslLogInfo` / `sslLogDebug` 前，都会先通过 default-config path 抬高 library default `LogLevel`
  - 新增 `tests/scripts/test_tsslconfig_logging_surface_truth_contract.sh`
    防止 active docs 再退回“只设 callback 就应该看到 info/debug”这条假指导

- 这轮的 focused evidence 也证明这不是一次“为了文档而猜实现”的收口：
  - 新 docs contract 首次运行 RED，直接暴露 API/reference/guides 还没把 logging level 入口说清楚
  - 修正后同一条 contract GREEN
  - `tests/test_factory_logging_scope_clarification.pas`
    继续证明：
    - request path 拒绝 `LogLevel` / `LogCallback`
    - library default snapshot / dispatch truth 保持不变
  - `tests/config/test_default_config.pas`
    继续证明：
    - `CreateDefaultConfig(...)` 仍然强制返回 `sslLogError` + `nil`

- 因而 `LogLevel` / `LogCallback` 这条线当前在 `v1.x` 下已经足够清晰：
  - runtime/source truth 已稳
  - active docs truth 已稳
  - 后续不该再把 logging guidance 漂移当成未验证区域反复拉起

- 沿着 `TSSLConfig` mixed-scope buckets 继续往下查后，这轮又确认了一个真正属于“实现没收干净”的 direct-library 缺口：
  - `TSSLFactory.CreateContext(...)` 路径早已 reject 自定义 `HandshakeTimeout` / `BufferSize`
  - 但 `ISSLLibrary.SetDefaultConfig(...)` + `CreateContext(AType)` 之前还会留下另一套行为：
    - default-config 可写入自定义 `HandshakeTimeout` / `BufferSize`
    - backend `CreateContext(AType)` 又完全不消费这两个字段
  - 这不是单纯 wording 问题，而是一条真实的 silent-ignore drift

- 这条 drift 的根因也很清楚：
  - five backend library units 都会：
    - `SetDefaultConfig(...)` 存下归一化后的 `TSSLConfig`
    - `CreateContext(AType)` 手工把 context-safe 字段套到新 context
  - 但在本批修复前，这条 handoff 缺少和 factory 对齐的 connection-scope validator
  - 结果就是 direct-library path 比 factory 多留了一块“看起来能配、实际上不生效”的历史面

- 当前修法刻意走 shared seam，而不是五份散改：
  - `src/fafafa.ssl.context.config.pas`
    新增 `ValidateDirectLibraryConnectionScope(...)`
  - 统一在五个 backend 的 `CreateContext(AType)` 入口 fail-fast：
    - `TOpenSSLLibrary.CreateContext`
    - `TFreePascalSSLLibrary.CreateContext`
    - `TWinSSLLibrary.CreateContext`
    - `TMbedTLSLibrary.CreateContext`
    - `TWolfSSLLibrary.CreateContext`
  - 这样后续若继续做 `TSSLConfig` slimming，不需要再分别担心 backend library path 会不会偷偷回退

- 这也让 public truth 更完整了一步：
  - `docs/reference/API_REFERENCE.md`
    现在明确：
    - `HandshakeTimeout` / `BufferSize`
      在 factory request path 和 direct-library context path 都 reject 自定义值
  - `docs/reference/ARCHITECTURE.md`
    也同步改成：
    - 这两个字段不属于 context/factory/direct-library config 主路径

- focused evidence 说明这批不是“为了统一而统一”：
  - 新 shell contract 首次运行 RED，直接暴露 docs/source 还没把 direct-library connection-scope truth 固定下来
  - 新 `tests/test_freepascal_library_default_config_connection_scope_clarification.pas`
    首次运行 RED，直接暴露 FreePascal direct-library path 还在 silent accept
  - 修复后两者 GREEN
  - `tests/test_factory_connection_scope_clarification.pas`
    继续 GREEN，说明 shared helper 没有把已冻结的 factory 行为拉歪

- 因而 `HandshakeTimeout` / `BufferSize` 这两个字段现在已经把三条高层入口都收到了同一套 truth：
  - one-shot factory request path：reject
  - factory-held library default path：reject
  - direct-library default-config path：reject
  - 后续不该再把这条 direct-library silent-ignore drift 当成未验证区域重新拉起

- 继续沿 `TSSLConfig` 的第一条真正实现切片往下做后，这轮又确认了一条 live runtime drift：
  - public docs / migration map / active guides 已经把 logging owner 说成：
    - `LogLevel` 走 `GetDefaultConfig(...)` / `SetDefaultConfig(...)`
    - `LogCallback` 走 `ISSLLibrary.SetLogCallback(...)`
  - 但 5 个 backend 的 `SetDefaultConfig(...)` 在修复前仍会直接执行：
    - `FLogCallback := LConfig.LogCallback`
  - 这意味着 callback owner 其实还是双挂：
    - default-config path 可以安装/替换 callback
    - dedicated setter path 也可以安装/替换 callback

- 这条 drift 和前面的 logging docs truth 不同，它已经不是 wording 问题，而是 runtime/source 仍未兑现“owner 单一”的接口语义：
  - 旧的 `tests/test_factory_logging_scope_clarification.pas`
    原本只证明 request path reject + dedicated setter dispatch
  - 当它被增强成：
    - `SetDefaultConfig(LogCallback)` 不应再安装 callback
    - 已安装 callback 后续不应被 `SetDefaultConfig(...)` 顺手清掉
    就立即 RED

- 当前修法继续遵循前面 `ServerName` / option-bridge 那条“先收高层 owner，再保留 source compatibility”的路线：
  - 不移除 `TSSLConfig.LogCallback` 字段
  - 但把 active owner 收到唯一入口：
    - `SetDefaultConfig(...)` 只继续更新 `LogLevel` 和其他 default-config 字段
    - `SetLogCallback(...)` 独占 runtime callback state
  - `GetDefaultConfig(...)` 仍然反映当前 callback 真相，因此 snapshot 读面没被掐掉

- 这也让 callback 的 runtime 规则终于变得可预测：
  - 如果没有显式 `SetLogCallback(...)`
    - 即使有人把 method pointer 塞进 `DefaultConfig.LogCallback`
    - 后续 `Log(...)` 也不会开始 dispatch
  - 如果 callback 已通过 `SetLogCallback(...)` 安装
    - 后续再用 `SetDefaultConfig(...)` 调 `LogLevel`
    - callback 会继续保持，不会被顺手清掉

- 这轮顺手也修掉了两份 focused test 自己的旧混合入口：
  - `tests/test_freepascal_library_default_config_server_name_clarification.pas`
  - `tests/test_openssl_library_default_config_server_name_clarification.pas`
    之前都还借 `DefaultConfig.LogCallback := ...` 来抓 warning
  - 现在已经改成：
    - `DefaultConfig.LogLevel := ...`
    - `Lib.SetLogCallback(...)`
  - 因而这些 direct-library warning 测试本身也不再反向把旧 owner 教回去

- focused evidence 说明这批不是“为了设计洁癖而收口”：
  - 新 source contract 首次运行 RED，直接点出 OpenSSL 仍在让 `SetDefaultConfig(...)` 装 callback；其余 backend 同样存在
  - 强化后的 `tests/test_factory_logging_scope_clarification.pas`
    首次运行 RED，直接暴露：
    - `GetDefaultConfig(...)` 仍回显了来自 `SetDefaultConfig(LogCallback)` 的 callback
    - `Log(...)` 也会立刻开始 dispatch
  - 修复后同一批 focused runtime/source/docs contracts 全部 GREEN

- 因而 `LogLevel` / `LogCallback` 这条线现在不只 docs truth 已稳，runtime/source truth 也已真正对齐：
  - callback owner 已单一
  - dedicated setter 与 default-config 的职责已分开
  - 后续不该再把“`SetDefaultConfig(...)` 还能不能安装 callback”当成未验证区域重新拉起

- 继续顺着 mixed-scope / compatibility 测试面往下看后，这轮确认了一个更偏“测试完整性”但依然真实的问题：
  - `tests/test_factory_logic.pas`
  - `tests/test_data_structures.pas`
    已经承担了核心 `TSSLConfig` record-shape coverage：
    - deprecated `ServerName`
    - option-bridge booleans
    - `BufferSize` / `HandshakeTimeout` 这类 mixed-scope field visibility
  - 但它们在测试结束后仍保留：
    - `WriteLn('按回车键退出...')`
    - `ReadLn`

- 这条问题没有造成当前 headless run 卡死，但它会留下两个负面信号：
  - 自动化输出始终带着“手工程序尾巴”
  - 核心测试继续看起来像 demo，而不是 CI-friendly test binary

- 这轮修法保持得很克制：
  - 不改任何断言和 coverage 目标
  - 只移除交互式退出逻辑
  - 并把 `INTENTIONAL_COMPAT` 注释补完整：
    - 不只说明 `ServerName` / option-bridge booleans
    - 也明确说明 `BufferSize` / `HandshakeTimeout` 这类 mixed-scope record-shape 字段仍是故意保留的可见面

- focused evidence 也足够直接：
  - 修复前 direct run 的最终输出都以“按回车键退出...”收尾
  - 修复后同样两份测试都可直接在 `timeout 2 ./...` 下跑完
  - 输出尾部只剩测试总结，不再要求或暗示手工输入

- 因而这两份 core compat tests 当前已经重新回到更合理的位置：
  - 仍然保留需要的 `v1.x` record-shape / compatibility coverage
  - 但不再把自己伪装成必须手工退出的演示程序
  - 后续不该再把这两份文件的交互尾巴当成未验证区域重新拉起

- 继续往下扫后，这轮又确认了第二组同类但更“顶层核心”的非交互残留：
  - `tests/test_exceptions.pas`
  - `tests/test_base_interface_contract.pas`
  - 它们分别锁住异常层级/构造语义，以及 `fafafa.ssl.base` 的接口/常量/record-shape 契约

- 这两份文件的细节和前一批 core compat tests 不完全一样：
  - 在当前 headless shell 里，末尾 `ReadLn` 会因为 stdin EOF 直接返回，因此 `timeout 2 ./...` 并不会稳定超时
  - 但源码仍保留：
    - `WriteLn('按回车键退出...')`
    - `ReadLn`
  - 结果就是自动化输出仍然带着“按回车键退出...”尾巴，而且退出行为继续依赖运行方式

- 因而这条问题的真实边界不是“必卡死”，而是：
  - 顶层 core tests 仍残留手工演示语义
  - 自动化输出被无意义提示污染
  - 是否需要人工输入不应由 shell/pipe/tty 形态来决定

- 这轮最合适的护栏不是再假装造一个 runtime hang，而是直接把自动化要求写成 focused source contract：
  - 新增 `tests/scripts/test_top_level_core_tests_noninteractive_contract.sh`
  - 专门禁止：
    - `tests/test_exceptions.pas`
    - `tests/test_base_interface_contract.pas`
    重新出现 `ReadLn` 或“按回车键退出...”

- focused evidence 也说明这条合同是有价值的，不是形式主义：
  - 新脚本首次运行立即 RED，直接命中 `tests/test_exceptions.pas`
  - 移除两份文件末尾交互逻辑后，脚本转 GREEN
  - 重新编译并 direct run 后，输出尾部只剩测试总结，不再留下交互提示

- repo-wide 扫描也顺手给出了范围真相：
  - `ReadLn` 残留并不只这两处
  - 但其余命中主要落在：
    - `tests/examples/*`
    - `tests/diagnostic/*`
    - benchmark / file-read helpers
    - 多份 WinSSL 专项程序
  - 因而当前这批保持在“顶层 core tests 自动化面”是正确收口，不应把 examples/diagnostics/Windows-specialized 程序混进同一批

- 在顶层 core tests 收口之后，repo-wide 剩余交互尾巴的下一层主面也已经被压实：
  - `tests/unit/test_winssl_comprehensive.pas`
  - `tests/winssl/test_winssl_context_comprehensive.pas`
  - `tests/winssl/test_winssl_errors_comprehensive.pas`
  - `tests/winssl/test_winssl_monitoring.pas`
  - `tests/winssl/test_winssl_connection_edge_cases.pas`
  - `tests/winssl/test_winssl_certstore.pas`
  - `tests/winssl/test_winssl_session_management.pas`
  - `tests/winssl/test_winssl_library_basic.pas`
  - `tests/winssl/test_winssl_certificate_loading.pas`

- 这批文件不是 examples / benchmark，而是真正仍有自动化入口和验证角色的 WinSSL 测试程序：
  - `run_winssl_tests.ps1` 明确把 `tests/unit/test_winssl_comprehensive.pas`
    归类成 `Minimal, non-network, non-interactive tests`
  - `scripts/run_tests_windows.ps1` 仍试图自动编译运行 WinSSL unit-level tests
  - 多个文件仍有 `.lpi`、validation bundle、Windows checklist 引用

- 因而它们保留 `Press Enter to exit...` / `按回车键退出...` / `ReadLn` 的问题，已经不只是“输出不太好看”：
  - 这会直接让 active Windows test path 的 non-interactive 承诺失真
  - 也会让真正的自动化 runner 与源码语义对不上

- 当前最合适的修法仍然是 focused source contract，而不是假装在 Linux 本地能完整跑 WinSSL runtime：
  - 新增 `tests/scripts/test_winssl_active_tests_noninteractive_contract.sh`
  - 只守住这批活跃 WinSSL 测试程序
  - 不把 examples / diagnostics / benchmark 混成同一批

- 这条合同首次运行立即 RED，直接命中 `tests/unit/test_winssl_comprehensive.pas`：
  - 证明问题不是历史残影，而是当前源码仍在主动保留交互式退出逻辑

- 修复后，这条线的验证比前两批还更完整了一层：
  - source contract 转 GREEN
  - `tests/unit/test_winssl_comprehensive.pas` 在 Linux 下可编译运行其非 Windows 分支，并且退出不再依赖 `ReadLn`
  - `tests/unit/test_winssl_comprehensive.pas`
  - `tests/winssl/test_winssl_session_management.pas`
    的 Win64 交叉编译也通过，说明这次尾部清理没有破坏 Windows 语法面

- 这也让测试完整性路线图更清楚了一步：
  - 活跃自动化测试面的交互尾巴，当前已经从
    - 顶层 core tests
    - WinSSL 活跃测试程序
    这两层基本收干净
  - 剩余 `ReadLn` 命中主要收缩到：
    - examples
    - diagnostics
    - benchmark
    - 少量非自动化/手工验证型程序

- 因而如果继续沿“测试完整性”推进，下一层已经不该再和主测试面混做：
  - 那会是 examples / diagnostics / benchmark 的单独分类清理
  - 而不是继续把 active test automation 问题当成未收口区域

- 反过来讲，这也意味着总体主路线可以重新抬回更高层：
  - `TSSLConfig` broader slimming / freeze 后续
  - 或 `ISSLConnection` 核心 surface slimming roadmap

- 在把主测试面的交互尾巴收干净之后，这轮又确认了另一个“看起来像未完成、其实主要缺 execution receipt”的文档缺口：
  - `docs/plans/2026-05-04-backend-context-optional-interface-completion-audit.md`
  - `docs/plans/2026-05-04-backend-context-native-handle-completion-audit.md`
  - `docs/plans/2026-05-04-backend-http-hooks-interface-completion-audit.md`
  - `docs/plans/2026-05-04-backend-session-native-handle-completion-audit.md`
  - `docs/plans/2026-05-04-backend-certificate-store-native-handle-completion-audit.md`
  - `docs/plans/2026-05-04-backend-diagnostics-interface-completion-audit.md`
  - 这些 plan 都已经对应到 `tests/contract/test_backend_contract.pas` 里的真实 contract，
    但文档本身还缺 execution result

- live 代码检查把这一点说得很明确：
  - `tests/contract/test_backend_contract.pas` 当前已包含：
    - Contract 12: Context optional interface alignment
    - Contract 13: Context native-handle interface alignment
    - Contract 14: Context HTTP hooks interface alignment
    - Contract 15: Session native-handle interface alignment
    - Contract 17: Certificate-store native-handle interface alignment
    - Contract 18: Diagnostics interface alignment
  - 换句话说，问题不在“contract 还没写”，而在“文档还没留下当前验证结果”

- 这条缺口如果不收，会继续误导总路线图判断：
  - 后续新会话很容易把这些 interface surface 当成“也许还没真正审过”
  - 结果就是重复拉起同一批 optional-surface 审计，而不是继续推进更高层的 design debt

- 当前最合适的修法因此不是重开大设计，而是做一次 focused completion-audit revalidation：
  - 重新编译并运行 `tests/contract/test_backend_contract.pas`
  - 直接用 live 结果给这 6 份 plan 补 `Focused Revalidation Result (2026-05-18)`
  - 同时明确说明：本批没有重跑 `compile_all_modules.py` / `run_minimal_ci_gate.sh --fast-local`，因为没有生产代码改动，且当前工作流明确避免重复拉重门禁

- focused evidence 进一步确认这批不是在补“纸面文档”：
  - 6 份 plan 全部被 source scan 证实 `MISSING_RESULT`
  - `tests/contract/test_backend_contract.pas` 当前 focused run 结果：
    - `Total Tests: 135`
    - `Passed: 111`
    - `Failed: 0`
    - `Skipped: 24`
  - OpenSSL / WolfSSL / MbedTLS / FreePascal 上述 optional public surface 全部 PASS
  - WinSSL 继续保持已有平台边界 truth：
    - 当前 Linux 主机不把 WinSSL backend 当作 live runtime truth
    - session native-handle 仍明确需要 dedicated Windows batch

- 这让“接口设计/实现完整性”的证据盘又收紧了一层：
  - context optional interfaces
  - context native-handle
  - context HTTP hooks
  - session native-handle
  - certificate-store native-handle
  - diagnostics
  这些 surface 现在不只是“有计划”，而是已经有 focused live contract 结果

- 因而总体路线图可以更安心地继续前移：
  - 不需要再怀疑这些 optional surface 是否缺 backend contract
  - 下一条高优先级应重新回到 broader interface debt：
    - `TSSLConfig` public-surface slimming 后续
    - 或 `ISSLConnection` 核心 surface slimming / completion audit
  - 这些现在比继续清手工示例程序的 `ReadLn` 更接近“接口设计完整”这个总目标

- 继续对照最新提交与活跃文档后，一个新的 workflow drift 也已经很明确：
  - `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md` 仍把 `TSSLConfig` 写成默认 immediate next step
  - 但 `docs/reference/API_REFERENCE.md` 的 `ISSLConnection` 区块其实还停留在旧接口世界
  - 这会直接误导调用方，也会把后续会话拉回错误的路线优先级

- `docs/reference/API_REFERENCE.md` 当前在 `ISSLConnection` / `WinSSL Session 管理` 上的 active-doc drift 是真实且高价值的：
  - 文档仍承诺：
    - `GetCipherBits`
    - `VerifyPeerCertificate`
    - `GetSessionID`
    - `IsSessionResumed`
    - `GetSessionData`
    - `SetSessionData`
  - 但当前源码真相实际是：
    - `DoHandshake` / `IsHandshakeComplete` / `Renegotiate`
    - `WantRead` / `WantWrite` / `GetError`
    - `GetSelectedALPNProtocol`
    - `SetTimeout` / `GetTimeout`
    - `SetBlocking` / `GetBlocking`
    - `GetContext`
    - `GetStateString`
    - `GetSession` / `SetSession` / `IsSessionReused`
    - `GetVerifyResult` / `GetVerifyResultString`
    - `GetOCSPStaplingEnabled` / `GetOCSPResponse` / `IsOCSPResponseVerified` / `GetOCSPResponseStatus`

- 这个 drift 不只是“签名列表旧了”：
  - `GetNativeHandle` 已经不是核心 `ISSLConnection`，而属于可选 `ISSLNativeHandleAccess`
  - `ISSLConnectionInfo` 当前也已经承接：
    - `GetConnectionInfo`
    - `GetContext`
    - `GetSelectedALPNProtocol`
    - `GetStateString`
  - `WinSSL Session 管理` 示例里对 `ISSLSession` 的说明也还没跟上当前 `GetID` / `Serialize` / `Clone` surface

- 因此当前最安全也最值得立即落地的一批，不是直接动 `ISSLConnection` public signature，而是先做 `surface truth freeze`：
  - 把活跃文档修回当前源码真相
  - 把 compatibility-core mirrors 与 optional owners 说明写清楚
  - 用 focused shell contract 把旧接口名回流风险钉住

- 在沿着 `ISSLConnection` 主线继续下钻时，新的证据又把一个隐藏的工作流偏差暴露出来：
  - 我们原本准备从 `ISSLConnectionInfo` 这组 mirror 开第一刀
  - 但 live repo 重新核对后发现：
    - `ISSLConnectionInfo`
    - `ISSLSessionResumption`
    - `ISSLCertificateVerification`
    这几条连接层 optional surface 其实都已经有 execution result
  - 反而是另外 3 份更早的 connection-layer 旧计划还缺当前 execution receipt

- 这 3 条缺口都直接落在 `ISSLConnection` 主面，而不是外围文档噪音：
  - `backend-client-connection-sni-interface-alignment`
  - `backend-connection-native-handle-interface-alignment`
  - `backend-ocsp-connection-interface-alignment`
  - 如果不补，会继续制造“这些 connection surface 也许没真正验证过”的假象

- focused live revalidation 进一步证明这里的问题仍然是“证据缺口”，不是“实现缺口”：
  - `tests/contract/test_backend_contract.pas` 当前结果仍是：
    - `Total Tests: 135`
    - `Passed: 111`
    - `Failed: 0`
    - `Skipped: 24`
  - `Contract 8` 当前 truth：
    - OpenSSL / WolfSSL / MbedTLS / FreePascal 都继续 PASS
    - WinSSL 继续按 Linux 主机边界 SKIP
  - `Contract 10` 当前 truth：
    - OpenSSL / WolfSSL / FreePascal 的 OCSP-capable connection 继续 PASS
    - MbedTLS 的 OCSP-absent 继续 PASS
    - WinSSL 继续 SKIP
  - `Contract 11` 当前 truth：
    - OpenSSL / WolfSSL / MbedTLS 的 native-handle surface 继续 PASS
    - FreePascal 的 absent 继续 PASS
    - WinSSL 继续 SKIP

- 因此连接层当前最准确的状态已经更明确：
  - 连接层 completion-audit contract 本身并不缺
  - 大部分连接层旧 plan 的 execution evidence 也不再缺
  - 真正残留的 connection-layer evidence gap，当前已经收缩到这 3 份旧 plan 的 focused receipt write-back
  - 这批补完之后，`ISSLConnection` 主线就可以更干净地转向真实的 `compatibility-core slimming`

- 在继续往 `ISSLConnection` slimming 主线推进时，又暴露出一个更纯粹的设计文档 drift：
  - `INTERFACE_DESIGN_V2.md` 里虽然已经在谈“最小 core + 扩展接口”
  - 但它自己对 `ISSLConnectionInfo` 这组 mirrors 的 owner 和迁移顺序并不自洽

- 当前 drift 主要集中在 4 个点：
  - 层次图漏掉 `ISSLConnectionInfo`
  - 仍保留 `ISSLAdvanced` 这个当前没有明确 public 落点的空壳名
  - `TBaseSSLConnection` 实现类示例没把 `ISSLConnectionInfo` 列进去
  - migration table 把 `GetConnectionInfo` 错归给 `ISSLDiagnostics`

- 更关键的是，设计文档在还没完成 Stage-A demotion 前，就过早把后续路线写死了：
  - `GetStateString` 直接写成“合并到 GetState”
  - `GetContext` 直接写成“通常不需要”
  - `GetSelectedALPNProtocol` 直接写成 `ISSLClientConnection`
  - 这会让下一批实现很容易跳过必要的中间层，直接做过度激进的收瘦

- 当前最安全的修法因此不是马上改 source，而是先冻结 Stage-A migration map：
  - 先承认这 4 个方法在当前 `v1.x` 里是 compatibility-core duplicates
  - 先把它们统一 demote 到 `ISSLConnectionInfo` 作为第一步设计锚点
  - 只有在这一步稳定后，才继续决定：
    - `GetSelectedALPNProtocol` 是否只留给客户端扩展
    - `GetStateString` 是否并入 `GetState`
    - `GetContext` 是否最终彻底退出 public surface

- 但仅有 migration map 还不够，因为 active docs 仍然会把用户拉回 core mirrors：
  - `API_REFERENCE.md` 还在示例 `LConn.GetConnectionInfo` / `LConn.GetSelectedALPNProtocol` / `LConn.GetStateString`
  - `INTEGRATION_GUIDE.md` 也还在直接教 `Conn.GetSelectedALPNProtocol` / `Conn.GetStateString`

- 这会形成一个典型的“设计和公开教学互相打架”的问题：
  - 设计文档在说“Stage A 先 demote 到 `ISSLConnectionInfo`”
  - 用户文档却还在教“直接从 core 上拿”
  - 后续一旦真的开始收 core，这类 active guidance 会立刻成为回流点

- 因此这条线的下一步应该是 active guidance de-emphasis，而不是再补一层抽象路线：
  - 先把用户可见示例统一成 `Supports(..., ISSLConnectionInfo, ...)`
  - 让公开教学路径开始与 Stage-A demotion map 同向
  - 然后再进入 source-facing slimming prep

- active guidance 对齐之后，source-facing gap 也变得更可见了：
  - 设计文档已经说清了 Stage-A map
  - 用户文档也已经开始优先走 `ISSLConnectionInfo`
  - 但 source 本身还没有明确说明这 4 个 mirrors 当前属于 `compatibility-core duplicates`

- 这会带来一个现实问题：
  - 下一批如果直接从 source 开刀，很容易又退回“这到底是正式 owner 还是偶然重复”的争论
  - 特别是 `src/fafafa.ssl.base.pas` 和 `src/fafafa.ssl.connection.base.pas` 本身还没把这件事写死

- 所以下一个安全动作是 source classification freeze：
  - 在 source comments 里把 Stage-A target 和 duplicate truth 写明
  - 让 source / 设计文档 / active docs 三层都对齐
  - 这样后续第一条真实实现切片才不会失去锚点

- source classification freeze 之后，再看 4 个 mirrors 的 live coupling，`GetContext` 已明显成为第一优先对象：
  - 活跃文档里只剩 `CAPABILITY_MATRIX_GUIDE.md` 一处仍直接教 `Conn.GetContext`
  - `API_REFERENCE.md` 虽然已经承认 `GetContext` 也由 `ISSLConnectionInfo` 暴露，但优先路径说明还没把它明确点出来
  - 生产源码里除 `TBaseSSLConnection.GetContext` 实现外，不再有额外活跃调用点

- 这和另外 3 个 mirrors 有明显差别：
  - `GetStateString` 还连着多份 integration/runtime 日志路径
  - `GetSelectedALPNProtocol` 带客户端 owner 语义
  - `GetConnectionInfo` 的使用面最广

- 因此当前最安全、也最有推进价值的动作不是直接碰 public signature，而是先收掉 `GetContext` 的 active guidance：
  - 让 capability 示例改走 `ISSLConnectionInfo.GetContext`
  - 把 API reference 的 first guidance 明确扩展到 `GetContext`
  - 这样下一批才适合进入 `GetContext` 的 source/class split feasibility

- `GetContext` active guidance 收掉之后，剩下最关键的 live coupling 就集中到了 contract 层：
  - `tests/contract/test_backend_contract.pas` 还在并列读取 `LConn.GetContext` 与 `LConnInfoAccess.GetContext`
  - 这让测试叙事看起来像在承认双 owner，而不是“optional owner + core mirror”

- 这也是为什么下一刀不该直接讨论 public deprecation：
  - 如果测试层还保留双 owner 话术，后续任何 `GetContext` 路线讨论都会继续模糊
  - 先把 contract 改成 `ISSLConnectionInfo.GetContext` 为主、`ISSLConnection.GetContext` 为 mirror proof，才算真正把 owner 语义压实

- 这一刀落下之后，`GetContext` 的 owner 语义终于在测试层也和路线图一致了：
  - `tests/contract/test_backend_contract.pas` 现在先验证 `ISSLConnectionInfo.GetContext` 对创建 context 的 owner truth
  - `ISSLConnection.GetContext` 只在 owner truth 通过后再做 mirror-equality proof
  - focused `test_backend_contract` 结果仍保持：
    - `Total Tests: 135`
    - `Passed: 111`
    - `Failed: 0`
    - `Skipped: 24`

- 这说明当前并没有引出新的 backend 行为漂移：
  - OpenSSL / WolfSSL / MbedTLS / FreePascal 的 connection-info contract 继续 PASS
  - WinSSL 继续保持 Linux 主机上的平台 skip truth
  - 因而下一批可以更放心地讨论 `GetContext` 的更强 feasibility / deprecation 路线，而不是回头怀疑 contract 是否已跟上

- 当前再做一层 source/class split 扫描后，`GetContext` 的 remaining live surface 已经小到足够直接 freeze：
  - 生产源码里没有新的 direct call dependency
  - 活跃文档只剩 `ConnInfo.GetContext`
  - non-script direct core `LConn.GetContext` 只剩 backend contract 的 mirror proof

- 这很关键，因为它意味着 `GetContext` 这条线已经不再是“还要继续考古”的状态：
  - 继续补 evidence 的收益已经很低
  - 更合理的下一步会是：
    - 要么真正进入 public deprecation wording route
    - 要么确认 `GetContext` 已足够干净，然后把主线切到下一条 mirror

- 把主线切到 `GetStateString` 后，最值钱的第一刀不是 backend runtime，而是普通测试路径：
  - `tests/connection/test_connection_basic.pas` 属于 generic smoke，仍直接教 `LConnection.GetStateString`
  - `tests/integration/test_real_https_connection.pas` 也还把 `Conn.GetStateString` 用作普通握手失败输出

- 这两类文件比 backend-specific runtime tests 更像“普通推荐路径”：
  - 它们更容易把新读者带回 core getter
  - 但又不需要先讨论 backend-specific owner 语义

- 所以 `GetStateString` 的当前最优路径是先做 active test de-emphasis：
  - 先把 generic/integration 测试切到 `ISSLConnectionInfo.GetStateString`
  - 让 residual direct core usage 收缩到 backend-specific runtime / contract 层
  - 然后再决定这些 residual 是做 allowlist freeze 还是继续 deeper migration

- 这批 focused 编译还顺手暴露出一个同文件的真实 drift，而且已经修掉：
  - `tests/connection/test_connection_basic.pas` 不只直接用了 `GetStateString`
  - 它还把 `GetNativeHandle` 当成 `ISSLConnection` 核心方法，并继续用 `FillChar` 初始化 `TSSLConfig`
  - 前者与当前 `ISSLNativeHandleAccess` truth 不一致，后者会误把 library-scoped logging 字段带进 `TSSLFactory.CreateContext(const AConfig)`

- 这类 drift 值得在同一批里一起修：
  - 因为它们都属于“普通 generic test 还在教旧 public surface”
  - 修完后 `test_connection_basic` 才重新变成可用的 focused proof，而不是一个本身就带着旧接口假设的弱信号

- `GetStateString` active-test de-emphasis 完成后，再看 residual 命中，范围已经很窄：
  - 普通测试和活跃文档都只剩 `ISSLConnectionInfo.GetStateString`
  - direct core `GetStateString` 只剩 backend contract mirror proof 与 OpenSSL / WolfSSL backend-specific runtime tests

- 这意味着 `GetStateString` 也已经到了适合直接 freeze allowlist 的时点：
  - 没必要再重复扫“普通路径到底还有没有 core getter”
  - 更合理的是把 residual file set 固定下来，然后把路线切到更强 wording 或下一条 mirror

- 把主线切到 `GetSelectedALPNProtocol` 后，第一刀同样不该先碰 backend-specific runtime：
  - `tests/integration/test_real_https_connection.pas` 还直接把 `Conn.GetSelectedALPNProtocol` 当普通握手成功路径
  - `tests/integration/test_cross_backend_consistency_contract.pas` 也还把 `Conn.GetSelectedALPNProtocol` 当归一化探测输出

- 这两个 ordinary integration/contract 文件比 backend-specific runtime ALPN tests 更像“公开推荐路径”：
  - 它们更容易把新读者带回 core getter
  - 但又不需要提前决定 `GetSelectedALPNProtocol` 最终是否只留给客户端扩展

- 所以 `GetSelectedALPNProtocol` 的当前最优路径，是先做 active test de-emphasis：
  - 先把 ordinary integration/contract 测试切到 `ISSLConnectionInfo.GetSelectedALPNProtocol`
  - 让 residual direct core usage 收缩到 backend-specific runtime / contract 层
  - 然后再决定这些 residual 是做 allowlist freeze，还是继续进入更强 client-owner 讨论

- 这批落下后，`GetSelectedALPNProtocol` 的 ordinary 测试路径已经和 Stage-A demotion map 对齐：
  - `tests/integration/test_real_https_connection.pas` 现在通过 helper 走 `ISSLConnectionInfo`
  - `tests/integration/test_cross_backend_consistency_contract.pas` 也不再直接读 core getter
  - 当前 residual direct-core `GetSelectedALPNProtocol` 已收缩到 backend contract mirror proof 与 MbedTLS/WinSSL backend-specific runtime files

- 当 `GetSelectedALPNProtocol` 的 residual surface 缩到这 4 个文件后，继续重复扫 ordinary 路径的收益已经很低：
  - `tests/contract/test_backend_contract.pas` 属于 connection-info mirror proof
  - `tests/mbedtls/test_mbedtls_alpn.pas` 与两份 WinSSL 测试都属于 backend-specific runtime residuals

- 这意味着 ALPN 这条线已经到了适合直接 freeze allowlist 的时点：
  - source comments 需要把 preferred-access / owner / residual-surface truth 写明
  - focused contract 需要把剩余 direct-core file set 固定下来
  - 做完后，这条线就不再需要反复拉起 residual archaeology

- 当前这层 freeze 落下后，`GetSelectedALPNProtocol` 的 design state 已经更明确：
  - ordinary docs/tests 全部优先走 `ISSLConnectionInfo.GetSelectedALPNProtocol`
  - direct core getter 只剩 contract mirror proof 与 backend-specific runtime residuals
  - 因而下一刀更适合讨论 stronger client-owner / deprecation wording，或者把主线切到 `GetConnectionInfo`

- `GetConnectionInfo` 和前 3 条 mirror 不一样，它在开始这批之前就已经没有 ordinary docs/tests 的 direct-core 教学残留：
  - direct core `GetConnectionInfo` 命中一开始就只剩 backend contract mirror proof 与 backend-specific OpenSSL/WinSSL files
  - 这意味着它不需要再走一轮 active-test de-emphasis，可以直接 freeze allowlist

- 当 `GetConnectionInfo` 的 residual surface 缩到这组 backend-specific / contract files 后，这条线也进入了和另外 3 条 mirror 相同的状态：
  - ordinary docs/tests 全部优先走 `ISSLConnectionInfo.GetConnectionInfo`
  - source comments 明确 preferred-access / owner / residual-surface truth
  - direct core getter 只剩 contract mirror proof 与 backend-specific runtime/contract residuals

- 到这一步，`ISSLConnectionInfo` 这 4 条 Stage-A mirror 路线已经全部完成 residual freeze：
  - `GetConnectionInfo`
  - `GetContext`
  - `GetSelectedALPNProtocol`
  - `GetStateString`
  - 因而主线应从“继续找残余”切换成“更强 owner/deprecation 决策”或“backend implementation-completeness 审查”

- 当前把主线真正切到 backend implementation-completeness 审查后，`GetConnectionInfo` 立刻暴露出一个共享层缺口：
  - `TSSLConnectionInfo` 活跃文档仍容易给人“完整信息”的印象
  - 但共享基类 `TBaseSSLConnection.GetConnectionInfo` 之前只填最小字段
  - 这不是某个 backend override 漏写，而是 shared layer 没有把已存在的 connection metadata 折进 record

- 这批最小且真实的共享层补齐点已经明确并落地：
  - `ServerName`
    - 对所有 client-capable backend 来说，连接对象自身已经持有 `FServerName`
    - 共享 `GetConnectionInfo` 可以安全补齐，不需要等待 backend-specific cipher/path 信息
  - `SessionId`
    - 对已经 connected / handshake-complete 且当前 session 可用的连接，`ISSLSession.GetID` 已足够作为 shared metadata 来源
    - 因而它也适合在 shared layer 补齐，而不是留空等 backend override

- 这次实现还确认了一个重要的 Pascal/接口引用计数边界：
  - 不应在 `TBaseSSLConnection.GetConnectionInfo` 中直接对 `Self` 执行 `Supports(Self, ISSLClientConnection, ...)`
  - `TBaseSSLConnection` 与 backend connection 都是 `TInterfacedObject`
  - 多份 focused test 会直接以 concrete object 实例创建连接，而不是先拿 interface ref
  - 在这种路径下，临时 interface 引用可能把对象推入错误的 `_Release` 生命周期，从而触发 fresh-connection `EAccessViolation`
  - 更安全的 shared design 是让基类走 protected virtual hook，由各 backend override 返回已有 `FServerName`

- 当前这条 `GetConnectionInfo` completeness 路线已经不再需要反复验证“ServerName/SessionId 到底能不能从共享层拿”：
  - focused mock contract 已证明 `ConnectionInfo.ServerName` 会镜像 `ISSLClientConnection.GetServerName`
  - focused OpenSSL cipher guard contract 已证明这次 shared enrichment 没有重新引入 fresh-connection AV
  - 因此下一批真正剩下的不是这两项 metadata，而是：
    - `PeerCertificate`
    - backend-specific crypto detail fields：
      - `CipherSuiteId`
      - `KeyExchange`
      - `Cipher`
      - `Hash`
      - `KeySize`
      - `MacSize`

- 继续沿着 implementation-completeness 主线盘点后，`PeerCertificate` 被证实和 `ServerName` / `SessionId` 属于同一类共享层缺口：
  - OpenSSL / WinSSL / FreePascal / MbedTLS / WolfSSL 都已经实现了 `DoGetPeerCertificate`
  - 各 backend 的 `ISSLCertificate.GetInfo` 也都已经存在
  - 当前只有 WinSSL override 会显式把 `PeerCertificate` 写回 `TSSLConnectionInfo`
  - 因而这不是“底层能力不够”，而是 shared `GetConnectionInfo` 还没有统一折叠当前对端证书信息

- 这使得 `PeerCertificate` 成为当前最值得优先修的 completeness 字段：
  - 它不要求先做每个 backend 的 cipher ID / kex / hash 枚举映射
  - 它也不需要引入新的 backend-specific runtime patch
  - shared base 只要在 `GetPeerCertificate <> nil` 时取 `GetInfo`，就能让非 WinSSL 路径不再继续空着

- focused mock proof 现在已经把这条 shared truth 钉住：
  - `ConnectionInfo.PeerCertificate.Subject` 会镜像 `ISSLCertificate.GetInfo.Subject`
  - `ConnectionInfo.PeerCertificate.Issuer` 会镜像 `ISSLCertificate.GetInfo.Issuer`
  - 这说明 `PeerCertificate` 不再只是 WinSSL override 局部拥有的 metadata，而是共享 `GetConnectionInfo` 的通用输出

- 当前 `GetConnectionInfo` 剩余的 completeness debt 因而又收缩了一层：
  - 已经由 shared layer 补齐：
    - `ServerName`
    - `SessionId`
    - `PeerCertificate`
  - 真正还需要 backend-specific 审查/修补的只剩：
    - `CipherSuiteId`
    - `KeyExchange`
    - `Cipher`
    - `Hash`
    - `KeySize`
    - `MacSize`

- 对剩余 6 个 crypto detail 字段做静态盘点后，shared-vs-backend 的边界已经更清楚了：
  - `CipherSuiteId` / `MacSize`
    - 当前更偏底层库/平台专属 detail
    - WinSSL override 已经直接掌握这些信息
    - OpenSSL 也有部分低层来源，但这条线不适合先做 shared 归一
  - `Cipher` / `Hash` / `KeySize`
    - 在 OpenSSL / MbedTLS / WolfSSL / FreePascal 上，很多时候已经能从 negotiated `CipherSuite` 名称稳定推导
    - 因而它们更适合作为 implementation-completeness 的 first shared slice
  - `KeyExchange`
    - 只在 cipher-suite name 仍显式携带 legacy prefix 时适合 shared 推导
    - TLS 1.3 标准名字本身不会给出这项 detail，因此不应在 shared layer 里假装“总能推出来”

- 这使得当前最稳妥的修法不再是“一次补全 6 项”，而是先做 name-derived normalization：
  - `Cipher`
  - `Hash`
  - `KeySize`
  - 以及带 legacy prefix 时的 `KeyExchange`

- focused mock proof 现在已经把这条新 shared truth 钉住：
  - 对 negotiated cipher-suite name `ECDHE-RSA-AES128-GCM-SHA256`
  - `ConnectionInfo.KeyExchange` 会推导为 `sslKexECDHE_RSA`
  - `ConnectionInfo.Cipher` 会推导为 `sslCipherAES128GCM`
  - `ConnectionInfo.Hash` 会推导为 `sslHashSHA256`
  - `ConnectionInfo.KeySize` 会推导为 `128`

- 因而当前 `GetConnectionInfo` implementation-completeness 主线又收缩了一层：
  - 已由 shared layer 补齐：
    - `ServerName`
    - `SessionId`
    - `PeerCertificate`
    - `Cipher`
    - `Hash`
    - `KeySize`
    - 以及 legacy-prefix case 的 `KeyExchange`
  - 更值得继续深入的剩余问题已经缩到：
    - `CipherSuiteId`
    - `MacSize`
    - 以及无法只靠名字稳定推导的更细平台差异

- `CipherSuiteId` 之所以适合作为下一刀，是因为它已经具备了比 `MacSize` 更强的双重 truth：
  - shared layer 能对标准 TLS 1.3 suite name 稳定推导 `CipherSuiteId`
  - OpenSSL 也能通过 low-level helper 给出直接值
  - 这使它适合先形成一个 shared + backend 双闭环，而不是继续停留在“待盘点”状态

- 这批还确认了一个具体的 OpenSSL API 结构性小坑：
  - `TSSL_CIPHER_get_protocol_id` 之前只在 `fafafa.ssl.openssl.api.core` 中出现了类型声明
  - 但 `TOpenSSLConnection` 实际使用的 active loader / var export 路径在 `fafafa.ssl.openssl.api.ssl`
  - 结果就是 connection 层一旦直接引用 `SSL_CIPHER_get_protocol_id`，会先遇到编译时符号缺口
  - 因而真正需要补的是 `api.ssl` 的导出与加载链，而不是只在 connection unit 侧增加 `uses`

- OpenSSL focused guard 也暴露出一个值得记录的 contract 细节：
  - 旧测试用 `StubSSLGetCurrentCipherNonNil` 返回假指针 `Pointer(1)` 来模拟 “有 current cipher，但 helper 缺失”
  - 在引入 `CipherSuiteId` low-level helper 之后，如果仍保留真实 `SSL_CIPHER_get_protocol_id` / `SSL_CIPHER_get_id`，测试会因为对假指针做 low-level 解引用而触发 AV
  - 这不是产品路径新崩溃，而是 contract 需要同步扩展：
    - 当测试场景声明 cipher helper unavailable 时，也必须一并置空 `protocol_id` / `get_id`
  - 单独的 truth contract 再去证明 low-level helper 可用时的 `CipherSuiteId` 回填行为

- 因而当前 `GetConnectionInfo` implementation-completeness 主线再往下推进时，最合理的剩余焦点已经变成：
  - `MacSize`
  - 以及无法只靠名字或统一 low-level helper 稳定归一的更细平台差异

- 在继续盘 `MacSize` 时，静态审查暴露出一个比“字段未统一”更先要修的 WinSSL truth bug：
  - `TSecPkgContext_ConnectionInfo.aiCipher` 在仓库定义里明确是“加密算法 ID”
  - 同一份 WinSSL 代码也一直把它当算法字段来生成 cipher 名称和 `Cipher` 枚举
  - 但 `GetConnectionInfo` 之前却直接执行：
    - `Result.CipherSuiteId := Word(ConnInfo.aiCipher)`
  - 这说明当前问题不是“WinSSL 还没补 suite id”，而是“已经把错误来源写进了 suite id”

- 这也修正了我们上一批对 WinSSL 的一个过度乐观假设：
  - 之前把 WinSSL 视为“已经直接掌握 `CipherSuiteId` / `MacSize` 的 backend”
  - 但从当前静态证据看：
    - WinSSL 通过 `SECPKG_ATTR_CONNECTION_INFO` 掌握的是算法级字段
    - 真实 cipher-suite id/name 更应该走 Schannel `SECPKG_ATTR_CIPHER_INFO`
  - 因而 WinSSL 这条线需要先做 truth correction，不能直接拿旧实现当 completeness 参考

- 这次 WinSSL 修复后的更准确结论是：
  - `CipherSuiteId`
    - shared TLS 1.3 路径已有 name-derived truth
    - OpenSSL 路径已有 low-level truth
    - WinSSL 路径现在也改为官方 `CipherInfo.dwCipherSuite` truth
  - `MacSize`
    - 当前全仓只有 WinSSL 在填值
    - 但它用的是 `dwHashStrength div 8`
    - 这更像 hash-strength proxy，而不是已经跨 backend 统一定义好的“记录层 MAC/tag 长度”
  - 因而下一批不该直接照着 WinSSL 现值去扩散实现，而应先把语义矩阵盘清楚

- `MacSize` 语义矩阵现在已经盘清到一个可稳定复用的层次：
  - shared layer 之前确实完全没有统一 `MacSize`
  - OpenSSL / FreePascal / MbedTLS / WolfSSL 主要都依赖 shared `GetConnectionInfo`
  - WinSSL 则独自把 `dwHashStrength div 8` 写进 `MacSize`
  - 因而“WinSSL 有值、其他 backend 没值”并不代表 WinSSL 更接近统一 truth，只代表它先填了一个 backend-local proxy

- 当前最安全、也最能跨 backend 复用的收法已经验证成立：
  - 对可识别 AEAD suite name，shared layer 可以稳定给出 auth-tag 长度：
    - `GCM` / `POLY1305` / `OCB` / `CCM` -> `16`
    - `CCM_8` -> `8`
  - 这条共享语义同时覆盖：
    - OpenSSL
    - FreePascal
    - MbedTLS
    - WolfSSL
    - 以及改成 inherited-first 之后的 WinSSL

- WinSSL 因而也不该再把 `dwHashStrength div 8` 当作无条件 truth：
  - 它现在更适合扮演：
    - shared path 已经识别出 AEAD suite 时不覆盖
    - shared path 没有稳定 `MacSize` 时的 legacy fallback
  - 这把 WinSSL 的 `MacSize` 从“可能误导 AEAD/TLS1.3 的主值”降格成了“缺省保底的 backend-local best-effort”

- 这次收口也把 `MacSize` 主线的剩余边界说清楚了：
  - 还没统一的不是 AEAD/TLS 1.3 这组场景
  - 而是 legacy non-AEAD suites 是否值得继续补更强 low-level `MacSize` truth
  - 如果后续不想继续在 connection-info completeness 上深挖，就可以比较放心地把主线切回 owner / deprecation wording route

- OpenSSL 这条 legacy/non-AEAD `MacSize` 路径现在也已经从“理论上能做”落成了真实实现：
  - 在这批之前，OpenSSL connection-info 虽然已经能拿到 current cipher，但并没有把：
    - `SSL_CIPHER_is_aead`
    - `SSL_CIPHER_get_digest_nid`
    - `EVP_get_digestbynid`
    - `EVP_MD_size`
    这组能力接进 `MacSize`
  - 而且问题不只在 connection unit：
    - `api.ssl` 的 active export/binding path 缺 `is_aead` / `digest_nid`
    - `api.evp` 的 active export/binding path 缺 `EVP_get_digestbynid`

- 当前修完后的更准确结论是：
  - shared 仍然是 AEAD `MacSize` 的第一 owner
  - OpenSSL 现在额外补齐了 legacy/non-AEAD digest truth
  - 因而 OpenSSL 已经不再属于“legacy `MacSize` 完全空白”的 backend

- 这也进一步缩小了剩余未统一面：
  - 高价值未完成项不再是 “OpenSSL 要不要补”
  - 而是：
    - WinSSL fallback 是否还值得强化
    - MbedTLS / WolfSSL 是否有同等级 low-level source
  - 如果这几条静态盘点后收益不高，就应该把默认主线切回 owner / deprecation wording，而不是继续在 `MacSize` 这一个字段上无限细抠

- WolfSSL 这条 legacy/non-AEAD `MacSize` 路径现在也已经从“头文件里有线索”落成了真实实现：
  - active binding 现在已经把：
    - `wolfSSL_GetHmacSize`
    接进 `TWolfSSLConnection.GetConnectionInfo`
  - 而且保持了与前两批一致的 owner discipline：
    - shared 仍然是 AEAD `MacSize` 第一 owner
    - WolfSSL 只在 shared path 仍未给值时，才回退到 backend-local HMAC truth

- 这次 WolfSSL focused proof 也补出了一条值得记住的测试前置条件：
  - optional backend 的 focused contract 不能只引 `wolfssl.api` / `wolfssl.connection`
  - 如果测试要经过 `TSSLFactory.CreateContext(..., sslWolfSSL)`，还必须：
    - define `ENABLE_WOLFSSL`
    - `uses fafafa.ssl.wolfssl.lib`
  - 否则工厂按设计会直接拒绝：
    - `WolfSSL backend is not enabled (define ENABLE_WOLFSSL)`
  - 这不是产品 bug，而是 optional backend registration truth，后续不该再重复误判

- 当前修完后的更准确结论是：
  - shared 仍然拥有 AEAD `MacSize` truth
  - OpenSSL 现在拥有 legacy digest truth
  - WolfSSL 现在拥有 legacy HMAC truth
  - WinSSL 保留 guarded fallback

- 因而当前 `GetConnectionInfo` implementation-completeness 主线剩余的 `MacSize` 面，又进一步缩小到了：
  - MbedTLS 是否也有值得接入的 low-level source
  - 如果 MbedTLS 实现成本高或真相不够稳，就该收住这条线并切回 owner / deprecation wording route

- MbedTLS 这条 `GetConnectionInfo` 路径现在也已经从“头文件里有 source，但仓库没接 runtime truth”落成了真实实现：
  - active binding 现在已经把：
    - `mbedtls_ssl_get_ciphersuite_id`
    - `mbedtls_ssl_get_ciphersuite_id_from_ssl`
    - `mbedtls_ssl_ciphersuite_from_id`
    - `mbedtls_ssl_ciphersuite_get_cipher_key_bitlen`
    接进 `TMbedTLSConnection.GetConnectionInfo`
  - MbedTLS 现在不只补 `MacSize`：
    - 也补了更稳的 `CipherSuiteId`
    - 以及 `KeySize`

- 这批还暴露并修正了一个更底层的 MbedTLS interface truth bug：
  - `src/fafafa.ssl.mbedtls.base.pas` 里原先把：
    - `MBEDTLS_MD_SHA1`
    - `MBEDTLS_MD_RIPEMD160`
    的枚举值写反了
  - 这不一定会立刻打出长度错误，因为 SHA1 / RIPEMD160 都是 20 字节
  - 但它会让任何依赖 `mbedtls_md_info_from_type(MBEDTLS_MD_SHA1)` 的真实摘要路径走到错误算法
  - 当前 focused runtime proof 已用 canonical SHA1(`abc`) 把这条常量真相钉住

- 这批还顺手确认了一个 shared completeness gap：
  - shared cipher-suite parser 虽然已经能理解很多 OpenSSL/WolfSSL 风格名字
  - 但对 MbedTLS 常见的连字符命名：
    - `TLS-RSA-...`
    - `AES-128[-GCM]`
    - `AES-256[-GCM]`
    之前并不完整
  - 当前已经补齐
  - 因而 helpers unavailable 时，MbedTLS 的 shared baseline 也比之前更接近统一 truth

- 当前修完后的更准确结论是：
  - shared 继续拥有 AEAD `MacSize` truth
  - OpenSSL 现在拥有 legacy digest truth
  - WolfSSL 现在拥有 legacy HMAC truth
  - MbedTLS 现在拥有 ciphersuite-info + digest truth
  - WinSSL 保留 guarded fallback

- 因而当前 `GetConnectionInfo` implementation-completeness 这条 backend-truth 路线，已经接近一个自然收口点：
  - 下一步更应该做一次 completion audit
  - 确认 FreePascal 是否仍然存在必须单独补的缺口
  - 如果没有，就该把默认主线切回 owner / deprecation wording route，而不是继续机械地往每个 backend 里找新 helper

- `FreePascal` completion audit 现在已经把这个“自然收口点”真正坐实了：
  - `TFreePascalConnection` 没有 dedicated `GetConnectionInfo` override
  - 它当前的 backend truth 主要是：
    - client / server runtime 把 negotiated TLS 1.3 suite 写成标准 `FCipherName`
    - session / resumption state 保留 `FCipherSuite: Word`
  - focused server/client proof 进一步证明 shared `GetConnectionInfo` 已能稳定补齐：
    - `CipherSuiteId`
    - `KeySize`
    - `MacSize`
    - `ServerName`
    - `SessionId`
    - `IsResumed`

- 因而当前更准确的 route-level 结论是：
  - `FreePascal` 不需要像 `OpenSSL` / `WolfSSL` / `MbedTLS` 那样继续补 backend-local helper
  - 当前 `GetConnectionInfo` implementation-completeness 主线已经可以视为基本完成
  - 默认主线应切回 owner / deprecation wording route，而不是继续按 backend 名单机械深挖

- 顺着这条主线继续审查后，又暴露出一个更偏 workflow 的真问题：
  - `tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
    之前仍假定 direct core `GetConnectionInfo` 只剩 10 个命中
  - 但后续 completeness / proof 批次已经把这个 residual surface 扩张到了 15 个命中
  - 这说明路线图说“`ISSLConnectionInfo` 是 owner”，测试面却还在默默扩大 direct core getter 的使用

- 当前修法没有去“改大数字掩盖问题”，而是把 owner/mirror 路线真正收紧：
  - `tests/contract/test_backend_contract.pas` 的 `Contract 19` 现在先验证：
    - `ISSLConnectionInfo.GetConnectionInfo`
  - 再验证：
    - `ISSLConnection.GetConnectionInfo`
      只是 mirror
  - FreePascal / OpenSSL / WolfSSL / MbedTLS 的 completeness proof 与 shared builder proof
    也都改成优先走 `ISSLConnectionInfo`

- 这批还顺手确认了一个测试层面的生命周期坑：
  - 在 OpenSSL / WolfSSL focused tests 里，
    concrete connection object 一旦被 `ISSLConnectionInfo` 接口引用接管，
    就不能再继续走手工 `Free`
  - 否则会在测试自身打出 `EInvalidPointer` / `EAccessViolation`
  - 当前已经把这些 helper 收成“接口接管生命周期，失败分支才手工释放”的模式

- 因而当前更准确的 `GetConnectionInfo` route 结论是：
  - stale residual allowlist 已被修正
  - residual direct-core surface 已缩回真正值得保留的少量 mirror/core-surface proof
  - 下一步应进入更强的 owner / deprecation wording route，
    而不是继续容忍普通 completeness proof 留在 direct core getter 上

- 继续静态审剩余 residual 之后，WinSSL 这两份 direct-core 文件的性质也已经更清楚：
  - `tests/winssl/test_winssl_connection_info.pas`
  - `tests/winssl/test_winssl_connection_edge_cases.pas`
  - 它们不是普通 completeness proof 漏迁移
  - 它们实际在覆盖的是 WinSSL compatibility-core surface 本身：
    - `GetConnectionInfo`
    - `GetProtocolVersion`
    - `GetCipherName`
    的 direct core 行为与一致性

- 因而当前更准确的收口方式不是继续把这两份文件也迁到 `ISSLConnectionInfo`：
  - 而是把它们显式归类为：
    - `INTENTIONAL_CORE_SURFACE`
  - 然后用 focused source contract 守住这条边界

- 这也把 `GetConnectionInfo` route 的残余不确定性进一步降到最小：
  - backend contract 负责 mirror/owner proof
  - WinSSL residual files 负责 intentional direct-core surface proof
  - 默认下一步就可以直接进入更强的 wording / deprecation 路线，而不必再在 residual 分类上打转

- 顺着这条 route 再看 source/doc truth，当前剩下的真实问题已经不是实现漂移，而是公开表述仍然偏弱：
  - `src/fafafa.ssl.base.pas` 虽然已经写了 preferred-access / compatibility note
  - 但 `API_REFERENCE` 与 `INTERFACE_DESIGN_V2` 仍可能让读者把 `ISSLConnection.GetConnectionInfo` 理解成“还在 core 上，所以还是正常主入口”
  - 尤其 `INTERFACE_DESIGN_V2` 的旧例子只写了 `LConn.GetConnectionInfo;  // 仍然存在`

- 这类漂移不需要动 runtime/ABI 就能修：
  - 只要把 source comment、公开 API 文档、v2 迁移文档统一成更强的 owner/mirror wording
  - 再用 focused shell contract 守住
  - 就能避免后续从“它还在 core 里”这个误区重新拉起路线讨论

- 当前修完后的更准确结论是：
  - `ISSLConnectionInfo.GetConnectionInfo` 已经不只是“推荐路径”
  - 它现在在 source/doc truth 上都明确成为默认 owner
  - `ISSLConnection.GetConnectionInfo` 则只作为 `v1.x` compatibility-core mirror 保留

- 因而 `GetConnectionInfo` 路线下一步不该再重复做 wording 清扫：
  - 真正还剩的路线问题已经变成：
    - 是否要进入第一条 public slimming slice
    - 以及这条 slice 是 compiler-level deprecation feasibility，还是到此为止后转去下一条 mirror

- 对这条 feasibility 再继续静态审后，结论已经足够明确：
  - production source 当前没有继续扩散 `.GetConnectionInfo(...)` 调用
  - active docs 也已经不再把它当主入口
  - direct core residual 调用只剩：
    - backend contract mirror proof
    - WinSSL intentional core-surface tests
  - 因而 `ISSLConnection.GetConnectionInfo` 进入 compiler-level deprecation 是可行的，而不是会打穿普通实现面的大动作

- 当前这一刀真正需要处理的风险不是 runtime，而是 compile noise：
  - 如果直接把声明标成 `deprecated`，intentional residual tests 会重新开始吐 warning
  - 但这些 residual 文件已经足够小，完全可以像 `.WithSNI(...)` 一样做局部 warning quarantine

- 当前修完后的更准确结论是：
  - `ISSLConnection.GetConnectionInfo` 现在在 source/doc/compiler 三层都被明确定义为 compatibility-only mirror
  - 这条 getter 的第一条真正 public slimming slice 已经落地
  - 后续不该再在它身上反复做 wording/deprecation archaeology，而应切去下一条 mirror 的 feasibility / slimming 选择

- `GetContext` 这条线在这一轮之前，其实已经比 `GetConnectionInfo` 更“干净”：
  - active docs 不再教 `Conn.GetContext`
  - owner primacy 已固定为 `ISSLConnectionInfo.GetContext`
  - source/class split allowlist 也已经 freeze 到只剩：
    - core/interface declarations
    - shared base implementation
    - backend contract mirror proof

- 这意味着它真正还缺的不是新的实现迁移，而是最后一层 compiler-surface truth：
  - public declaration 自身还不是 compiler `deprecated`
  - 因而 source/doc 虽然已经说它只是 compatibility mirror，但编译器层面还没有把这件事说死

- 当前这一刀需要处理的风险也很小：
  - non-script direct core `GetContext` 已只剩 backend contract 一处
  - 因而只要给这处 mirror proof 做本地 warning quarantine，就不会像更大的 surface 那样带来到处补 suppression 的副作用

- 当前修完后的更准确结论是：
  - `ISSLConnection.GetContext` 现在也已经在 source/doc/compiler 三层都被明确定义为 compatibility-only mirror
  - `GetContext` 的第一条真正 public slimming slice 已经落地
  - 后续不该再在它身上反复做 wording/deprecation archaeology，而应把主线切去下一条 mirror 的 feasibility / slimming 选择

- `GetStateString` 这条线在进入 compiler deprecation 之前，其实也已经足够“干净”：
  - active generic/integration tests 已经切到 `ISSLConnectionInfo.GetStateString`
  - residual direct-core surface 已 freeze 到：
    - backend contract mirror proof
    - OpenSSL server OCSP stapling runtime proof
    - WolfSSL server OCSP stapling runtime proof
  - ordinary docs/tests 已不再把 `Conn.GetStateString` 当推荐路径

- 这意味着它真正还缺的，也不是新的 backend/runtime 迁移，而是最后一层 compiler-surface truth：
  - source/doc 虽然已经写明 owner 是 `ISSLConnectionInfo.GetStateString`
  - 但 public core declaration 自身还没进入 compiler `deprecated`
  - 因而编译器层面还没有把“compatibility-only mirror”这件事彻底说死

- 当前这一刀的风险同样是 compile noise，而不是 runtime 行为：
  - non-script direct core `GetStateString` 已只剩 3 个 residual 文件
  - 它们都属于 intentional mirror/runtime proof，不需要迁移行为
  - 因而完全适合沿用前两批模式：
    - declaration 做 compiler `deprecated`
    - residual callsite 做局部 warning quarantine
    - 用 focused shell contract + backend contract proof 收口

- 因而当前更准确的路线判断是：
  - `GetStateString` 的下一步不是继续做 residual archaeology
  - 而是直接进入 compiler deprecation alignment
  - 做完之后，这条 getter 也应像 `GetConnectionInfo` / `GetContext` 一样，退出反复拉起的 wording/compiler 治理队列

- 当前修完后的更准确结论是：
  - `ISSLConnection.GetStateString` 现在也已经在 source/doc/compiler 三层都被明确定义为 compatibility-only mirror
  - `GetStateString` 的第一条真正 public slimming slice 已经落地
  - 这条线后续不该再继续做 wording/deprecation archaeology，而应把主线切去下一条 mirror 的 feasibility / slimming 选择

- 这批还顺手暴露出一个 workflow 层的小真相：
  - `tests/scripts/test_isslconnectioninfo_active_guidance_contract.sh`
    仍依赖一条较旧的共享 guidance 句式：
    - “如果你在写新代码，并且需要连接信息 / ALPN / 状态字符串这组 mirrors”
  - 我们在强化 `GetContext` 时把总句拆散了，但脚本契约还没跟着调整
  - 当前做法不是弱化新文案，而是补回这条总句，同时保留单独的 `GetContext` / `GetStateString` compiler-deprecated guidance

- `GetSelectedALPNProtocol` 这条线在进入 compiler deprecation 之前，也已经足够“干净”：
  - active integration/contract tests 已经切到 `ISSLConnectionInfo.GetSelectedALPNProtocol`
  - residual direct-core surface 已 freeze 到：
    - backend contract mirror proof
    - MbedTLS runtime proof
    - WinSSL ALPN/SNI runtime proof
    - WinSSL edge-case runtime proof
  - ordinary docs/tests 已不再把 `Conn.GetSelectedALPNProtocol` 当推荐路径

- 这意味着它真正还缺的，也不是新的 backend/runtime 迁移，而是最后一层 compiler-surface truth：
  - source/doc 虽然已经写明 owner 是 `ISSLConnectionInfo.GetSelectedALPNProtocol`
  - 但 public core declaration 自身还没进入 compiler `deprecated`
  - 因而编译器层面还没有把“compatibility-only mirror”这件事彻底说死

- 当前这一刀的风险同样是 compile noise，而不是 runtime 行为：
  - non-script direct core `GetSelectedALPNProtocol` 已只剩 4 个 residual 文件
  - 它们都属于 intentional mirror/runtime proof，不需要迁移行为
  - 因而完全适合沿用前面三批模式：
    - declaration 做 compiler `deprecated`
    - residual callsite 做局部 warning quarantine
    - 用 focused shell contract + backend contract proof 收口

- 这批还顺手暴露出第二条 workflow 层的小真相：
  - `tests/scripts/test_isslconnectioninfo_migration_targets_contract.sh`
    一直还锁着 4 条 mirrors 的旧迁移表文案
  - 之前因为没重跑它，这个漂移一直潜伏着
  - 当前修法不是只改 ALPN 一条，而是把 4 条 mirror row 的 required truth 一起同步到当前 compiler-deprecated wording，避免后续再被旧契约反复误拦

- 当前修完后的更准确结论是：
  - `ISSLConnection.GetSelectedALPNProtocol` 现在也已经在 source/doc/compiler 三层都被明确定义为 compatibility-only mirror
  - `GetSelectedALPNProtocol` 的第一条真正 public slimming slice 已经落地
  - 到这一步，`ISSLConnectionInfo` 这 4 条 Stage-A mirrors 都已经完成 compiler-surface 收口
  - 因而下一步不该再继续做 mirror wording/deprecation archaeology，而应把主线切回 interface-design completeness / implementation-completeness 审查

- 当这 4 条 mirror 的 compiler-surface 收口完以后，继续盘点 `ISSLConnection` 剩余的 optional-owner surfaces，最像“普通 guidance 还在教回 core”的下一组不是 session/证书验证，而是 diagnostics：
  - `docs/reference/API_REFERENCE.md` 的健康检查 / 性能监控 / 完整诊断 / 告警示例仍直接使用：
    - `LConn.IsHealthy`
    - `LConn.GetHealthStatus`
    - `LConn.GetPerformanceMetrics`
    - `LConn.GetDiagnosticInfo`
  - `tests/test_sslctxboth_roleless_handshake_clarification.pas` 也仍把 `LConn.GetHealthStatus` 当普通读取路径
  - 相比之下，session / certificate-verification 线已经有更多 backend-specific runtime 依赖，不适合作为这一步的最小收口批次

- 这条 diagnostics 线的好处是 owner truth 已经先有了：
  - `tests/contract/test_backend_contract.pas` 的 `Contract 18: Diagnostics interface alignment`
    已经证明：
    - `Supports(LConn, ISSLDiagnostics, ...)` 对当前可用 backend 成立
    - `GetHealthStatus` / `GetPerformanceMetrics` / `GetDiagnosticInfo` 的 owner path 已有 cross-backend 自洽性证据
  - 因而当前更值钱的不是再补实现，而是把 ordinary docs/tests 也切到同一条 owner path

- 当前修完后的更准确结论是：
  - ordinary diagnostics guidance 现在已经优先走 `ISSLDiagnostics`
  - `sslCtxBoth` 这份 generic dual-context boundary test 也不再把 `GetHealthStatus` 当默认 core 读取路径
  - 所有当前本机可用 backend 的该测试都已证明 `ISSLDiagnostics` owner interface 可直接用于这条边界
  - 因而 `ISSLDiagnostics` 这组能力面暂时不再是“普通路径仍在教回 core”的残余点

- 沿着“普通 guidance 仍偏 core”的盘点再往下看，下一组最值钱的不是生产实现层，而是 certificate-verification 这条 owner path：
  - `docs/INTEGRATION_GUIDE.md`
    的阻塞/非阻塞握手失败示例仍直接用 `Conn.GetVerifyResultString`
  - 同一文档的排错条目也仍写 `Conn.GetVerifyResult / Conn.GetVerifyResultString`
  - `docs/reference/API_DOCUMENTATION.md` 的 CT 示例失败路径也还直接抛 `Conn.GetVerifyResultString`
  - `tests/integration/test_cross_backend_consistency_contract.pas`
    与 `tests/integration/test_cross_backend_errors_contract.pas`
    也还把 verify-result mirrors 当普通 core 读取路径

- 这条线之所以适合先收 guidance，而不是先碰 runtime，是因为 owner truth 已经先有了：
  - `tests/contract/test_backend_contract.pas` 的 `Contract 21: Certificate-verification interface alignment`
    已经证明：
    - `Supports(LConn, ISSLCertificateVerification, ...)` 对当前可用 backend 成立
    - `GetVerifyResult` / `GetVerifyResultString` / `GetPeerCertificateChain`
      的 owner path 已有 cross-backend 自洽性证据
  - 因而当前更值钱的不是再补实现，而是把 ordinary docs/tests 也切到同一条 owner path

- 当前修完后的更准确结论是：
  - ordinary certificate-verification guidance 现在已经优先走 `ISSLCertificateVerification`
  - 两份通用 integration/contract 测试也不再把 `GetVerifyResult / GetVerifyResultString` 当默认 core 读取路径
  - 因而 `ISSLCertificateVerification` 这组能力面暂时也不再是“普通路径仍在教回 core”的残余点

- 沿着“普通 guidance 仍偏 core”的盘点继续往下，session-resumption 这一组当时最适合先收的点也已经被证实：
  - `docs/reference/API_REFERENCE.md`
    的 session-resumption / WinSSL session 示例原本还直接使用：
    - `LConn.GetSession`
    - `LConn.SetSession`
    - `LConn.IsSessionReused`
  - `docs/reference/API_DOCUMENTATION.md`
    的会话缓存 / 性能问题示例也还直接写：
    - `Connection.GetSession`
    - `Connection.SetSession`
  - `docs/INTEGRATION_GUIDE.md`
    的 resumed-session + early-data 例子原本也还直接读：
    - `InitialStream.Connection.GetSession`
  - `tests/integration/test_e2e_scenarios.pas`
    也还把：
    - `Conn1.GetSession`
    - `Conn2.SetSession`
    - `Conn2.IsSessionReused`
    当普通 e2e 路径

- 这条线之所以适合先收 guidance，而不是先碰实现，是因为 owner truth 本来就已经先有了：
  - `tests/contract/test_backend_contract.pas` 的 `Contract 20: Session-resumption interface alignment`
    已经证明：
    - `Supports(LConn, ISSLSessionResumption, ...)` 对当前可用 backend 成立
    - `GetSession / SetSession / IsSessionReused` 的 owner path 已有 cross-backend 自洽性证据
  - 因而当前更值钱的不是再补 backend runtime，而是把 ordinary docs/tests 也切到同一条 owner path

- 当前修完后的更准确结论是：
  - ordinary session-resumption guidance 现在已经优先走 `ISSLSessionResumption`
  - 通用 E2E session-resumption 场景也不再把 `GetSession / SetSession / IsSessionReused` 当默认 core 路径
  - 因而 `ISSLSessionResumption` 这组能力面暂时也不再是“普通路径仍在教回 core”的残余点

- 顺手做的轻量下一队列扫描也给出了一个更清楚的后继候选：
  - `docs/reference/API_DOCUMENTATION.md`
    仍保留多处 direct core OCSP 示例：
    - `Connection.GetOCSPStaplingEnabled`
    - `Connection.IsOCSPResponseVerified`
    - `Connection.GetOCSPResponseStatus`
  - 同文件里虽然已经有 `ISSLOCSPStapling` owner-path 示例，但 ordinary surface 仍是“双真相并存”
  - 因而若继续沿 optional-owner surface 推进，`ISSLOCSPStapling` active-guidance de-emphasis 当前是最像下一批的边界清晰候选

- 这条 OCSP 线之所以适合先收 guidance，而不是先碰 runtime，同样是因为 owner truth 本来就已经先有了：
  - `tests/contract/test_backend_contract.pas`
    已经锁住 `ISSLOCSPStapling` 的 capability / owner interface 边界
  - `docs/reference/API_DOCUMENTATION.md`
    也早已有一套 `Supports(Connection, ISSLOCSPStapling, OCSP)` 的 owner-path 示例
  - 真正的问题只是同一文档还并存着 4 段 direct-core ordinary 示例，容易把读者重新带回 core mirrors

- 当前修完后的更准确结论是：
  - ordinary OCSP stapling guidance 现在已经优先走 `ISSLOCSPStapling`
  - 当前已识别的高价值 optional-owner ordinary-guidance sweep 已全部完成：
    - `ISSLDiagnostics`
    - `ISSLCertificateVerification`
    - `ISSLSessionResumption`
    - `ISSLOCSPStapling`
  - 因而下一步不该再继续围绕这几组 surface 做同类文案清扫，而应把主线切回更大的 interface-design completeness / backend implementation completeness 审查

- 当前 WinSSL session-resumption lane 的下一条真实问题已经从“会不会 crash / 会不会误报”收缩成了“public truth 会不会继续过强承诺”：
  - `src/fafafa.ssl.winssl.lib.pas` 原先仍把 `SessionTicketsSupport` 写成 `sslSupportStable`
  - `KnownIssues` 也没有写入当前 dedicated Windows truth
  - 这会让 capability 发布继续暗示“WinSSL tickets/resumption 已稳定闭环”

- 活跃 WinSSL 参考文档里也确实残留了多处旧时代承诺，而不是只有一处 wording 漂移：
  - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md` 还把 `Session 复用` 写成“✅ 支持 | 完整支持”
  - `docs/reference/WINSSL_PERFORMANCE_TUNING.md` 还在写 `减少握手时间 70-90%`
  - 同一性能文档里还混着 direct core `GetSession` / `IsSessionResumed`
  - `docs/reference/API_REFERENCE.md` 甚至还保留了 WinSSL `性能提升 70-90%` 的对比表

- 这说明当前真正要守住的不是“有没有 session-resumption public surface”，而是“不要把 public surface 误写成 runtime-proven stable behavior”：
  - `SupportsSessionTickets=True` 仍然可以保留，因为 Schannel / public surface 确实存在
  - 但 `SessionTicketsSupport` 必须降到 `sslSupportExperimental`
  - `SessionCacheSupport` 可以继续保持 `sslSupportStable`
  - 活跃文档必须同步明确：
    - `observed_reuse=false`
    - `session_configured=true`
    - run `26037518301`

- focused docs truth contract 也顺手揭示了一条很有价值的流程细节：
  - 状态报告第一次 RED 并不是因为实现或主文档仍然错
  - 而是 `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md` 没把 `windows-gate` 这个最终 truth source 关键词显式写出来
  - 这类 failure 很便宜，但价值很高，因为它能把“口头知道 Windows CI 是真相来源”固定成可回归检查的文字证据

- 当前这批收口后的更准确结论是：
  - WinSSL session-resumption / tickets 的 public surface 现在已与当前 GitHub Windows runtime truth 对齐
  - 当前默认真相是：
    - surface 存在
    - shared crash 已关闭
    - native resumed-handshake 仍未在 fafafa.ssl 中被 dedicated Windows proof 证实
  - 因而下一步不该再反复做 docs/capability truth 清扫，而应直接转向 native resumed-handshake / session tickets 行为调查

- 在继续往 WinSSL native resumed-handshake 深挖时，又确认了一条真正的 context/runtime 实现缺口：
  - `TWinSSLContext` 虽然暴露了 `SetSessionCacheMode(...)`、`SetOptions(...)`、`ssoEnableSessionTickets`
  - 但 `EnsureCredentialsAcquired` 原先没有把这些 truth 映射到 Schannel `dwFlags`
  - setter 也没有触发 credential rebuild
  - 这意味着 session cache / tickets 在 WinSSL 上一度只是“Pascal 字段有值”，而不是“runtime credential path 真的响应”

- 这条缺口比继续猜测 server/runtime 行为更值得先收，因为它位于更上游的 canonical context layer：
  - `TWinSSLContext` 复用同一个 `CredHandle` 给多条连接
  - 这正是 Schannel reconnect / session cache 的关键 carrier
  - 如果这里的 option truth 都没下沉到 credential acquisition，那么后面看到的 `observed_reuse=false` 至少一部分会被这个 wiring 缺口污染

- 当前修完后的更准确结论是：
  - `SetSessionCacheMode(...)` 改变后会要求重建 credential
  - `SetOptions(...)` 改变后也会要求重建 credential
  - `EnsureCredentialsAcquired` 会把：
    - `not FSessionCacheEnabled`
    - `not (ssoEnableSessionTickets in FOptions)`
    映射成 `SCH_CRED_DISABLE_RECONNECTS`
  - 这还不等于 WinSSL 已经稳定命中 resumed handshake，但至少把“context-level session controls 其实没接到 runtime”这条实现缺口先关掉了

- 在继续审查 WinSSL `ISSLSession` 时，又确认了一条更细但同样真实的对象层实现缺口：
  - `TWinSSLSession.Serialize` 原先只返回 `FSessionData`
  - `SetSessionMetadata(...)` 并不会刷新 `FSessionData`
  - `Deserialize(...)` 也不会恢复 `ID/timeout/protocol/cipher/resumed`
  - 这让 WinSSL session object 的 serialization surface 基本处于“接口存在但对象不自洽”的状态

- 这条缺口虽然不直接等于 Windows native resumed-handshake，但仍然很值得先收：
  - `TWinSSLSession` 是 WinSSL backend 对外暴露的 canonical `ISSLSession`
  - 如果它自身的 serialize/deserialize 都不能 round-trip metadata，那么调用方连“缓存/传递这个 session object 的元数据”都拿不到稳定语义
  - 这会持续污染对 WinSSL session surface 完整性的判断

- 当前修完后的更准确结论是：
  - WinSSL session object 现在已经能 round-trip 自身 metadata
  - `SetTimeout(...)` / `SetSessionMetadata(...)` 也会同步刷新 serialized payload
  - 这仍不等于 serialized payload 可以直接驱动 Schannel native reconnect
  - 但至少把“WinSSL `ISSLSession` 序列化接口几乎是空壳”这条对象层缺口先关掉了

- 在继续深挖 WinSSL native resumed-handshake 时，又确认了一个更上游的 truth correction：
  - 我们上一批把 `SCH_CRED_DISABLE_RECONNECTS` 直接映射到了 client credential path
  - 但按 Schannel 官方语义，这个 flag 是 server-side truth，不应该直接挂在 client `SCHANNEL_CRED`
  - 这意味着“client-side session cache/ticket disable truth 已完全接线到 Schannel credential flags”这个说法过强，需要立即收紧

- 顺着官方文档再往下压，也把 WinSSL reconnect 的核心模型压清楚了：
  - client-side Schannel reconnect/cache lookup 当前更准确的条件是：
    - same `target name`
    - same `credential handle`
    - same process / logon session
  - 因而 `ISSLSessionResumption.SetSession(...)` 在 WinSSL 上更准确的定位不是“native handle injection”
  - 而是 compatibility metadata surface；若 native reconnect 发生，其根因仍应优先归到 Schannel auto-cache 命中

- 当前修完后的更准确结论是：
  - server-side disable truth 仍可通过 `SCH_CRED_DISABLE_RECONNECTS` 表达
  - client-side native reconnect 解释模型已经收紧回官方 Schannel auto-cache truth
  - 这能让后续 `observed_reuse=false` 的调查少掉一条错误分支：不再误以为 WinSSL `SetSession(...)` 已经是可验证的 native reconnect 注入点

- 在 client reconnect truth 收紧后，当前 WinSSL native resumed-handshake 调查也进一步明确了一条方法论边界：
  - shared/public path 当前故意保持 conservative false
  - 因而 `observed_reuse=false` 不能再单独拿来断言 “Schannel native reconnect 没发生”
  - 如果还想继续往下压，必须把 public truth 与 native observation 分开收集

- 当前最小而安全的下一刀因此已经固定：
  - 不改生产 `winssl.connection` 的 shared reconnect 逻辑
  - 只在 dedicated `tests/winssl/test_winssl_session_resumption.pas` 里通过：
    - `ISSLNativeHandleAccess.GetNativeHandle`
    - `PCtxtHandle`
    - `QueryContextAttributesW(..., SECPKG_ATTR_SESSION_INFO, ...)`
    去额外读取一次 native `SSL_SESSION_RECONNECT` 观测

- 这条 dedicated evidence lane 的价值在于：
  - 一旦 GitHub Windows artifact 给出
    - `native_probe_succeeded=true`
    - `native_observed_reuse=true|false`
    我们就能区分两种完全不同的后续路线：
    - public truth 仍保守，但 native reconnect 其实已命中
    - current same-context same-target proof 连 native reconnect 也没命中
  - 这样后续就不必再反复重开 shared probe crash / docs truth / client reconnect truth 那几条已经收口的旧 lane

- 当前新的 dedicated proof 程序已经把这条分离写成稳定 marker：
  - `native_probe label=initial_handshake ...`
  - `native_probe label=same_context_attempt_N ...`
  - `summary ... observed_reuse=... native_observed_reuse=... native_probe_succeeded=... require_native_reuse=...`
  - 因而接下来真正需要的新事实，只剩 GitHub Windows live artifact

- GitHub Windows live run `26042437486` 现在已经把这个问题压到了更窄、也更直接的一层：
  - broader suite 的 `WinSSL Session Resumption Truth` 先成功输出：
    - `initial_handshake` 的 public reuse signal
    - `initial handshake must not report reuse: PASS`
  - 但在第一条 `native_probe` marker 之前就以 `exit_code=-1073741819` 退出
  - 这说明当前这版 native probe 不是“观测不到结果”，而是“调用方式本身不安全”

- 因而当前更准确的结论已经变化了：
  - `ISSLNativeHandleAccess.GetNativeHandle -> PCtxtHandle -> QueryContextAttributesW(..., SECPKG_ATTR_SESSION_INFO, ...)`
    这条 public-handle probe 路径，至少在 GitHub Windows runner 的 broader suite 默认 lane 上还不能直接开启
  - public truth 与 native observation 的分离方向本身没错
  - 但 native observation 目前必须退回 opt-in experimental lane，不能默认挂到 broader suite

- 这也让下一步修法变得很明确：
  - 默认 broader suite 只保留 public truth + `native_probe disabled_by_default` 记录
  - `FAFAFA_WINSSL_ENABLE_NATIVE_PROBE=1` 只留给未来更安全的专门重跑
  - 未来若还要继续追 native observation，优先要解决的是“如何做一个不会把 dedicated process 打崩的 WinSSL-specific probe seam”，而不是继续重写 public truth 或 reconnect 文档

- 当前这条更安全的 follow-up 也已经在本地落成：
  - native probe 不再在 broader suite 默认 lane 自动开启
  - 只有显式设置 `FAFAFA_WINSSL_ENABLE_NATIVE_PROBE=1` 或 `FAFAFA_WINSSL_REQUIRE_NATIVE_REUSE=1` 才会真正执行 risky probe
  - 默认 lane 继续保留：
    - public signal markers
    - `native_probe ... reason=disabled_by_default`
  - `summary ... native_probe_enabled=false`
  - 这让 broader suite 可以继续提供稳定 public truth，同时把 risky probe 明确降格成 opt-in experimental evidence

- GitHub Windows live rerun `26043523820` 已把这条 quarantine 真正闭环：
  - `WinSSL Session Resumption Truth` lane 已恢复 PASS
  - runtime artifact 真实记录了：
    - `native_probe label=initial_handshake available=false reason=disabled_by_default`
    - `native_probe label=same_context_attempt_N available=false reason=disabled_by_default`
    - `summary ... native_probe_enabled=false native_observed_reuse=false native_probe_succeeded=false`
  - 这说明 broader suite 默认 lane 当前已经安全，且记录语义与实现对齐

- 同一个 rerun 也把新的 first hard blocker 压得非常清楚：
  - 失败已经不在 `session_resumption`
  - 而在 `WinSSL Integration Tests (Multi-Scenario)` 对 `api.github.com` 的“必须 2xx/3xx”假设
  - 但该 host 的：
    - TCP connect
    - TLS handshake
    - request send
    - response receive
    - status-line parse
    都是成功的
  - 因而当前失败不是 WinSSL transport/handshake 失败，而是 test oracle 过强

- 这让当前更准确的修法也很明确：
  - 外部 HTTP 集成测试应该验证“状态行可解析且不是 5xx”
  - 而不是把外部 API 的鉴权/限流/策略响应强行约束成 `2xx/3xx`
  - 当前这条修法已在本地落地，并补了 focused contract 防止回归

- GitHub Actions live run `26044471873` 现已把当前 repo-level truth 压清：
  - `windows-gate` 已恢复 PASS
  - `linux-gate` 保持 PASS
  - overall failure 只剩 `macos-gate`
  - 因而 WinSSL native-probe / integration-multi 这两条 Windows lane 都不该再反复拉起

- `macos-gate` 当前失败不再是泛泛的“OpenSSL path 问题”，而是更具体的一类 loader 选择风险：
  - artifact probe 证明 runner 环境里存在 Homebrew `OpenSSL 3.6.2`
  - 但模块测试实际只打印 `3.x (libcrypto.3.dylib)` 这种“请求名”级别信息
  - `Store/TS/CT` 仍然 PASS，而 `PEM/EVP/PKCS12/CMS/OCSP` 成片缺符号
  - 这与“generic fallback 误加载到错误库面”的症状更一致，而不是简单的 `OPENSSL_ROOT` 不可见

- `GetOpenSSLVersionString` 不能作为“实际已加载 Homebrew OpenSSL 3.x”的硬证据：
  - `src/fafafa.ssl.openssl.api.core.pas` 里的 `LoadedCryptoLibName` 来自 `TryLoadOpenSSLLibraries(...)` 的请求名记录
  - 它不会回填 `TOpenSSLLoader.GetVersionInfo.VersionString`
  - 所以日志里的 `3.x (libcrypto.3.dylib)` 只能说明“代码试图按 3.x 路径初始化”，不能单独证明最终加载的动态库就是 Homebrew OpenSSL 3.x

- 当前最小而正确的修法已经明确并落地：
  - 在 `src/fafafa.ssl.openssl.loader.pas` 中新增 `TryLoadLibraryFromOpenSSLRoot(...)`
  - 当 `OPENSSL_ROOT` 存在时，先尝试 `OPENSSL_ROOT/lib/libcrypto*.dylib` / `libssl*.dylib` 的绝对路径
  - 只有这条绝对路径失败后，才退回现有 generic `libcrypto.*` / `libssl.*` fallback
  - 这样可以把 macOS runner 明确锚定到 workflow probe 已验证过的 Homebrew OpenSSL root

- 这条 macOS loader 修法当前已完成本地静态闭环，但还缺 live runtime 复核：
  - focused source contract 已完成 `RED -> GREEN`
  - 现有 loader Pascal contract 编译与运行继续 PASS
  - 真正的 done 条件仍然是新的 GitHub macOS rerun 把 `PEM/EVP/PKCS12/CMS/OCSP` 的成片缺失压掉

- 后续 live rerun 进一步说明这条 `OPENSSL_ROOT` 优先级修法不是最终根因：
  - 新 commit 已把 `OPENSSL_ROOT/lib/...` 绝对候选优先级落地
  - 但新的 macOS module artifact 与旧 artifact 失败模式基本一致
  - 所以这条线应保留为“已做过且合理的 loader hardening”，而不是继续当成主怀疑反复深挖

- 当前更高置信度的结构性事实已经压清：
  - 继续 PASS 的 `TS/CT/Store` 主要是 direct `GetCryptoProcAddress(...)` / `GetSSLProcAddress(...)` 手工绑定
  - 持续失败的 `EVP/PEM/PKCS12/CMS/OCSP` 主要走 `LoadFunctions(...)` 或 batch-binding 表
  - 因而失败面更像“batch binding / symbol-name 假设 / wrapper drift”而不是单纯路径问题

- 旧的 macOS probe 设计也确实有一个证据盲区：
  - `wave_b_macos_gate_probe_*.json` 只报告环境和 `OPENSSL_ROOT`
  - 它并不会告诉我们：
    - `TOpenSSLLoader` 最终识别出的真实版本字符串
    - direct `PEM_read_bio_X509` / `PKCS12_new` / `CMS_sign` / `OCSP_REQUEST_new` 是否真的能从当前句柄解析
    - wrapper `LoadOpenSSLPEM` / `LoadOpenSSLCMS` / `LoadOpenSSLOCSP` 是否与 direct symbol truth 一致

- 当前这批新增的 loader/symbol probe 因而改变了下一步调试方法：
  - 新 probe 会直接产出：
    - `loader_version_string`
    - `api_version_string`
    - direct symbol truth
    - wrapper/module truth
  - 这能把后续判断压缩成三种可操作分支：
    - `loader_version_string` 就不对：继续查 loader/path 选择
    - direct symbols 存在但 wrapper/module 失败：继续查 batch-binding 表或 wrapper 逻辑
    - direct symbols 本身不存在但 loader 版本又是 Homebrew OpenSSL 3.x：继续查 symbol-name/export 假设

- 本地新 probe 的结果也验证了它的诊断价值，而不只是“新增了一个文件”：
  - 在本机 Linux/OpenSSL 3.5.5 上，probe 成功同时给出：
    - `loader_version_string = OpenSSL 3.5.5 27 Jan 2026`
    - `api_version_string = 3.x (libcrypto.so.3)`
  - 这再次证明：
    - `loader_version_string` 才是实际加载句柄上的版本真相
    - `api_version_string` 只是历史请求名/分类字符串，不能再单独拿来判断 macOS 是否真加载到了 Homebrew OpenSSL 3.x

## 2026-05-19

- GitHub Actions live run `26048015976` 现已把 macOS loader/symbol lane 真正关掉：
  - `linux-gate` / `macos-gate` / `windows-gate` / `summary` 全部 `success`
  - `wave_b_macos_loader_symbol_probe_wave_b_b2_20260518_macos_loader_symbol_probe_07e526b.json` 直接证明：
    - `loader_version_string = OpenSSL 3.6.2 7 Apr 2026`
    - direct symbol truth 全部为 `true`
    - `evp/pem/pkcs12/cms/ocsp/ts/ct/store` module truth 全部为 `true`
  - 因而当前主线不该再回头怀疑 macOS loader/path、symbol export、或 batch-binding 漂移

- 在继续做 interface/backend completeness 审查时，generic session persistence seam 暴露出一个独立真 bug：
  - `src/fafafa.ssl.session.cache.pas` 的 `SaveToFile(...)` 先把 `FCache.Count` 写进文件头
  - 但写 payload 时又会 `Continue` 跳过 invalid/expired session
  - 这会让文件头条目数大于真实写入条目数，后续 `LoadFromFile(...)` 直接读坏文件

- 这条问题的价值在于它不是单 backend 噪声，而是公共 persistence 路径的结构性缺口：
  - 任何把 invalid/expired session 与 valid session 混存后再持久化的路径，都可能写出自相矛盾的 cache 文件
  - 表面现象会像“偶发 load 失败”或“缓存文件有时损坏”，很容易被误判成 backend-specific session 问题

- 当前最小正确修法已经明确且落地：
  - `SaveToFile(...)` 改成先写占位计数
  - 只对真实写出的条目递增 `WrittenCount`
  - 最后回填真实计数，而不是继续相信 `FCache.Count`

- 新增 focused contract 也把这条真相锁死了：
  - `tests/test_session_cache_persistence_contract.pas` 先用一个 valid + 一个 invalid session 打出 fresh RED
  - 修复后同一契约转绿，并直接证明：
    - `LoadFromFile(...)` 不再因为 skipped entry 把文件头读坏
    - valid persisted session 能恢复
    - invalid skipped session 不会被 materialize

- 在继续横向审 session object completeness 时，`MbedTLS/WolfSSL` 的 c-library session serialization 又暴露出一类更隐蔽的实现漂移：
  - `TMbedTLSSession.Deserialize(...)` 之前只要收到非空字节就会把 payload 缓存进 `FSerializedData` 并返回 `True`
  - `TWolfSSLSession.Deserialize(...)` 在 `wolfSSL_d2i_SSL_SESSION` 缺失时也会走同类“缓存成功”路径
  - 这会把“helper 缺失、根本没恢复 native session”伪装成 public `ISSLSession` 已成功 deserialize

- 这不是单纯的风格问题，而是 public interface 对外撒谎：
  - `Deserialize(...) = True` 会让调用方自然认为 backend 已 materialize 出可继续使用的 session object
  - 但 helper 缺失时，真实状态只是“把原始字节留在了一个 Pascal 字段里”
  - 后续 `Serialize(...)` 若再把这些字节回放出来，会进一步制造“这条 round-trip 能力存在”的假象

- `MbedTLS` 当前 actually 有官方 helper 可以接线：
  - 本机头文件 `/usr/include/mbedtls/ssl.h` 已提供：
    - `mbedtls_ssl_session_load`
    - `mbedtls_ssl_session_save`
  - 因而当前更正确的修法不是继续接受 fake success，而是把 helper 真正绑定进来

- 当前这批修复后，c-library session serialization 的最小真相已明确：
  - `TMbedTLSSession`
    - helper 缺失时 fail-closed
    - helper 存在时会真实 load/save native session
  - `TWolfSSLSession`
    - `wolfSSL_d2i_SSL_SESSION` 缺失时 fail-closed
  - 这意味着 public `ISSLSession` 现在至少不会再宣称一条并不存在的 deserialize 路径

- focused + cross-contract 结果也支持把这条问题标记为“已收口，而非待继续猜测”：
  - `tests/test_mbedtls_framework.pas` 当前 `104 passed / 0 failed`
  - `tests/test_wolfssl_framework.pas` 当前 `112 passed / 0 failed`
  - `tests/contract/test_backend_contract.pas` 当前 `135 total / 111 passed / 0 failed / 24 skipped`
  - 这说明本批修法没有把现有 backend completeness contract 打回去

- 当前更高价值的下一刀也因此更清楚：
  - 不是继续重审 helper-less fake success
  - 而是去看 `Clone()` / ownership / metadata-vs-native-handle 这类更深一层的 session object 语义是否仍有漂移

- 继续沿这条线深挖后，`MbedTLS/WolfSSL` 的 `Clone()` 又暴露出另一条 public interface drift：
  - `TMbedTLSSession.Clone()` 之前只复制字段和 `FSerializedData`，但把 `FSession=nil`
  - `TWolfSSLSession.Clone()` 之前也同样把 clone 降成 metadata shell
  - 结果是同一个 valid/resumable session，clone 后立刻变成：
    - `IsValid=False`
    - `IsResumable=False`
    - native handle 消失

- 这和其它 backend 当前真相已经明显不一致：
  - `OpenSSL.Clone()` 通过 `SSL_SESSION_up_ref` 保留 native session
  - `FreePascal.Clone()` 做完整深拷贝
  - `WinSSL.Clone()` 至少不会把 metadata/session object 本身降级成 invalid
  - 因而 `MbedTLS/WolfSSL` 这条不是“实现风格不同”，而是 `ISSLSession.Clone()` contract drift

- focused RED 也把这个问题钉得非常直接：
  - `MbedTLS` 在 deserialized session 上新增 clone 断言后，先红在：
    - clone valid
    - clone resumable
    - clone native handle
  - `WolfSSL` 同样先红在这三条

- 当前最小正确修法已经落地并通过验证：
  - `TMbedTLSSession.Clone()` 现在会在 native session 存在时，基于 serialize/deserialize 重新 materialize clone session
  - `TWolfSSLSession.Clone()` 也改成同一路径，避免继续吐出 invalid metadata shell
  - `TWolfSSLSession.Serialize()` 同时收紧为：
    - native session 存在时优先输出真实 `i2d` bytes
    - 不再在已有 cached bytes 时无条件优先回放 stale payload

- 当前这批收口后的可复用结论是：
  - c-library session object 不只要“能 serialize/deserialize”
  - 还必须保证 `Clone()` 不会把一个可用 session 降级成不可用壳对象
  - 下一刀最值得继续压的是 source-lifetime/ownership 边界，而不是再回头重开 clone validity 本身

- 沿着 source-lifetime 继续压后，当前真正的硬缺口被进一步收缩成了 `WolfSSL.FromConnection()`：
  - `OpenSSL.DoGetSession()` 当前用的是 `SSL_get1_session`，本身已经 secure ownership
  - `MbedTLS.FromContext()` 当前会先分配独立 session，再调用 `mbedtls_ssl_get_session(...)`
  - 但 `WolfSSL.FromConnection()` 之前是直接包住 `wolfSSL_get_session()` 返回的内部 session 指针，并标成 `AOwnsSession=False`
  - 这意味着源连接一旦释放，public `ISSLSession` 里的 native handle 就可能悬空

- 这条问题和之前的 clone truth 一样，不是“实现风格不同”，而是 public interface 真实可用性漂移：
  - 调用方拿到 `GetSession()` 返回值时，自然会认为它是可独立持有的 session object
  - 如果它仍绑定在源连接 lifetime 上，这个对象管理语义就是假的

- 当前最小正确修法已经落地：
  - 新增 `wolfSSL_SESSION_dup` 绑定
  - `TWolfSSLSession.FromConnection()` 现在会先 secure ownership：
    - 优先 `wolfSSL_SESSION_dup`
    - 否则退到 `i2d/d2i` duplication
    - 两条都不可用时直接 `fail-closed`

- focused RED/GREEN 也把这条 ownership truth 钉住了：
  - 新增 `WolfSSL Session Source Lifetime Contract`
  - 先证明：
    - 有 duplication helper 时必须复制 borrowed session
    - 没有 ownership helper 时不能继续把 borrowed handle 递出去
  - 修复后两条都转绿

- 当前更值得继续推进的方向也再次收敛：
  - 不再泛化地怀疑所有 c-library session extraction 都有 lifetime 问题
  - `OpenSSL` / `MbedTLS` 当前没有同类硬缺口
  - 下一刀更适合查 `GetPeerCertificate` / metadata extraction completeness 是否仍弱于其它 backend

- `MbedTLS/WolfSSL` session metadata / peer-certificate completeness 这批已经形成新的可复用真相：
  - `TMbedTLSSession.FromContext(...)` 之前虽然拿到了独立 session，但 metadata 仍停在：
    - `FProtocolVersion := sslProtocolTLS12`
    - `FCipherName := ''`
    - `GetPeerCertificate = nil`
  - `TWolfSSLSession.FromConnection(...)` 之前虽然已补 protocol / cipher，但 peer cert 仍完全缺席
  - focused RED 证明这不是“文档味道”，而是 live surface 缺口：
    - `MbedTLS` 先红在 protocol / cipher / peer cert / peer-cert-preserving clone
    - `WolfSSL` 先红在 peer cert / peer-cert-preserving clone

- 本机头文件也把两条 ownership/materialization 边界钉得更明确了：
  - `/usr/include/mbedtls/ssl.h` 对 `mbedtls_ssl_get_peer_cert()` 直接写明：
    - 若要跨后续 SSL API 调用继续使用该指针，调用方必须自己复制
  - `/usr/include/wolfssl/test.h` 的示例路径对 `wolfSSL_get_peer_certificate(ssl)` 会在使用后显式 `wolfSSL_FreeX509(peer)`
  - 因而这批的安全做法不是继续保留 borrowed/live native handle，而是：
    - `MbedTLS`: `DER copy -> owned reload`
    - `WolfSSL`: `native X509 -> DER export -> owned reload`

- `MbedTLS` 这批还顺手暴露并修掉了一个更底层的证书 clone 真 bug：
  - `TMbedTLSCertificate.Clone()` 之前只复制 `FPEMData/FDERData/FInfo`
  - 但不会重新 materialize `FX509Crt`
  - 结果 clone 后很多读取路径虽然带着缓存字节，却仍然是 native-handle 空壳
  - 这条 bug 正是 session peer-cert clone 断言最后两盏红灯的真实根因

- 当前收口后的 route truth 已明确：
  - `TMbedTLSSession.GetPeerCertificate()` / `TWolfSSLSession.GetPeerCertificate()` 现在都返回可独立持有的 cert clone
  - `TMbedTLSSession.Clone()` / `TWolfSSLSession.Clone()` 会继续保留 peer-cert truth
  - `TWolfSSLCertificate.SaveToDER()` 现在不再只依赖缓存数据；native `WOLFSSL_X509` 也能真实导出
  - 这意味着 c-library session object 当前终于不再弱于连接态真相太多，至少 protocol / cipher / peer cert 三条已经做实

- 继续往 connection-level surface 深挖后，`MbedTLS` 又暴露出一条真实 lifetime trap：
  - `/usr/include/mbedtls/ssl.h` 已明确警告 `mbedtls_ssl_get_peer_cert()` 返回的是会随 дальнейший SSL API 调用变化的指针
  - 但 `TMbedTLSConnection.DoGetPeerCertificate()` / `DoGetPeerCertificateChain()` 之前仍直接：
    - `TMbedTLSCertificate.Create(LPeerCert, False)`
  - 这意味着 public `ISSLCertificate` / `TSSLCertificateArray` surface 还在把 backend-internal borrowed pointer 直接递出去

- focused RED 也把这条问题钉得很硬，而不是风格争议：
  - 新增 contract 先红在 4 个点：
    - `GetPeerCertificate()` native handle 仍等于源 fixture handle
    - `GetPeerCertificateChain()[0]` native handle 仍等于源 fixture handle
    - cert-copy helper 缺失时 `GetPeerCertificate()` 没有 fail-closed
    - cert-copy helper 缺失时 `GetPeerCertificateChain()` 也没有 fail-closed

- 当前最小正确修法已经落地，并且保持了低风险：
  - 不新造额外 native-copy helper
  - 直接复用上一批已修好的 `TMbedTLSCertificate.Clone()` materialization 路线
  - connection surface 当前变成：
    - borrowed `Pmbedtls_x509_crt`
    - 临时包一层 non-owning cert
    - 立刻 `Clone()` 成 owned cert
    - helper 不足时自然 `nil` / empty chain

- 当前收口后的 connection truth 已明确：
  - `TMbedTLSConnection.GetPeerCertificate()` 现在返回的 cert handle 不再与 source fixture handle 同指针
  - `TMbedTLSConnection.GetPeerCertificateChain()` 的单叶子 cert 也一样
  - 这说明 `MbedTLS` 的 connection-level peer-cert surface 已经从“borrowed wrapper”收口为“owned public object”

- 继续沿着 c-library certificate object completeness 深挖后，`WolfSSL` 又暴露出一条真实的 clone-truth 漂移：
  - `TWolfSSLCertificate.Clone()` 之前只复制：
    - `FPEMData`
    - `FDERData`
    - `FInfo`
  - 但不会重新 materialize `FX509`
  - 这意味着 loaded cert clone 后虽然还可能保留缓存字节和部分摘要语义，却会丢掉 native X509 真相

- focused RED 也把这条问题钉得很硬，而不是实现风格差异：
  - `Clone keeps native handle for loaded certificate` 先红
  - `Clone preserves subject truth` 先红
  - `Clone preserves issuer truth` 先红
  - `Clone preserves fingerprint truth` 当时继续 PASS
  - `Clone fails closed when X509 materialization helper is unavailable` 先红
  - 这说明当前缺口不是“所有 metadata 都错”，而是 loaded clone 退化成了带缓存 DER 的 metadata shell

- 当前最小正确修法也已经明确并落地：
  - 不继续暴露 borrowed/native alias
  - 也不手写额外字段级复制逻辑
  - 直接复用已经存在的证书载入路径：
    - 优先使用 `FDERData`
    - 否则从 `FPEMData` 转 DER
    - 再不行就从 native `WOLFSSL_X509` 导出 DER
    - 最后统一 `LoadFromDER(...)` 重建 owned cert
  - 如果 `wolfSSL_X509_d2i` 等 materialization helper 不可用，则直接 `fail-closed`

- 当前收口后的 route truth 已明确：
  - `TWolfSSLCertificate.Clone()` 现在不再把 loaded cert clone 成 native-handle 空壳
  - `GetSubject` / `GetIssuer` / `GetFingerprintSHA256` 这类 public metadata truth 现在会在 clone 后继续保留
  - 这也把 `WolfSSL` certificate completeness 的剩余边界说清了：
    - 当前问题已经不是 clone 会不会退化
    - 下一刀更适合横向审其它 backend 的 certificate clone / connection-level completeness，而不是再重开这条 WolfSSL clone 空壳问题

- 继续沿着 `WolfSSL` connection-level certificate surface 深挖后，单证书入口又暴露出一条更细的 completeness 漂移：
  - `/usr/include/wolfssl/test.h` 的官方示例里，
    `wolfSSL_get_peer_certificate(ssl)` 会在使用后显式 `wolfSSL_FreeX509(peer)`
  - 这说明这条 API 返回值并不是 “马上会被连接内部回收的 borrowed trap”
  - 但 `TWolfSSLConnection.GetPeerCertificate()` 之前仍然直接把这条 source native cert 包成 public wrapper 返回
  - 同时同一 backend 的：
    - `GetPeerCertificateChain()`
    - `TWolfSSLSession.FromConnection()`
    - `TWolfSSLCertificate.Clone()`
    已经都在走 owned/materialized truth
  - 所以当前问题不是“内部 lifetime 崩坏”，而是 `WolfSSL` 单证书 public surface 还没和现有 materialization 规则收齐

- focused RED 也把这条不一致钉成了具体行为，而不是代码风格问题：
  - `GetPeerCertificate must return an owned copy instead of the source native handle` 先红
  - `GetPeerCertificate should fail closed when cert-copy helper is unavailable` 先红
  - 这说明单证书入口当时既保留了 source native-handle provenance，也没有在 helper-loss 时收紧成 fail-closed

- 当前最小正确修法已经明确并落地：
  - 不去重做 chain 路径
  - 不重开 session/certificate clone 旧 lane
  - 只把 `TWolfSSLConnection.GetPeerCertificate()` 对齐到现有 materialization 规则：
    - 先拿 `wolfSSL_get_peer_certificate(...)` 返回的 native X509
    - 再 `SaveToDER`
    - 再 `LoadFromDER(...)` 重建 owned cert
    - DER 无法导出时直接 `nil`

- 当前收口后的 route truth 已明确：
  - `TWolfSSLConnection.GetPeerCertificate()` 现在返回的 cert handle 不再与 source native handle 同指针
  - helper-loss 时也不再继续返回 fake-complete wrapper
  - 这说明 `WolfSSL` connection single-cert surface 现在终于和 chain/session/certificate clone 的 public truth 口径一致了

- 继续沿着 connection-level certificate completeness 往下压后，`FreePascal` 又暴露出一条更像“链真相缺失”的问题：
  - `ISSLCertificate` 公共接口明确公开了：
    - `SetIssuerCertificate(...)`
    - `GetIssuerCertificate(...)`
  - `TFreePascalConnection` 握手后也已经能同时构建：
    - `FPeerCertificateChain`
    - `FPeerCertificate := FPeerCertificateChain[0]`
  - 但之前没有把 chain 相邻证书之间的 issuer link 接起来
  - 所以当时会出现：
    - `GetPeerCertificate()` 能拿到 leaf
    - `GetPeerCertificateChain()` 能拿到 leaf + issuer
    - 但 leaf 上的 `GetIssuerCertificate()` 仍为空

- focused RED 也把这条缺口钉得很实，而不是“文档味道”：
  - `Peer leaf certificate should preserve issuer link` 先红
  - `Peer chain leaf entry should preserve issuer link` 也会跟着红
  - 这说明当前缺口不是 handshake 没产出链，而是 public chain truth 没有沿 `ISSLCertificate` surface 对外接通

- 当前最小正确修法已经明确并落地：
  - 不重开 scripted TLS 1.3 handshake 主路径
  - 不动 OCSP / verify / certstore 流程
  - 只在 `TFreePascalConnection` 构建完 `FPeerCertificateChain` 后：
    - `chain[i].issuer := chain[i+1]`
    - 最后一个 cert 的 issuer link 置空

- 当前收口后的 route truth 已明确：
  - `GetPeerCertificate()` 返回的 leaf cert 现在会保留 issuer link
  - `GetPeerCertificateChain()[0]` 也会保留同一条 issuer-link truth
  - 这说明 `FreePascal` 连接态 peer cert public surface 已不再出现“leaf/chain 都在，但 issuer link 断掉”的 completeness 漂移

- 当前这轮 `GetVerifyResult` / `GetVerifyResultString` compiler-deprecated alignment 的唯一阻塞，已经确认不是实现问题，而是文档 wording 与旧 grep 规则冲突：
  - `docs/reference/API_REFERENCE.md` 一度写成了
    - `ISSLCertificateVerification.GetVerifyResult`
    - `ISSLCertificateVerification.GetVerifyResultString`
  - 但 residual-classification contract 会把这类 `TypeName.GetVerifyResult*` 视为活跃文档 direct-core 风险命中
  - 所以失败根因不是 owner path 错了，也不是 deprecated 设计错了，而是文档字面重新撞上了 allowlist 规则

- 这次最小正确修法也因此非常窄，并已落地：
  - `API_REFERENCE.md` 改成不带点号的 `ISSLCertificateVerification owner surface` 表达
  - `tests/scripts/test_getverifyresult_compiler_deprecated_contract.sh` 同步锁住新 wording
  - 不触碰 runtime 语义，不重开 residual subgroup freeze，不再扩张验证面

- focused 回归结果把这条结论钉死了：
  - `test_getverifyresult_compiler_deprecated_contract.sh` 已 PASS
  - `test_isslcertificateverification_residual_classification_contract.sh` 已 PASS
  - `test_isslcertificateverification_generic_examples_contract.sh` 已 PASS
  - hostname precedence 两个 focused 编译测试继续 PASS
  - `tests/contract/test_backend_contract.pas` 继续保持 `135 total / 111 passed / 0 failed / 24 skipped`

- 因而 `GetVerifyResult*` 这条 verify-result 路线现在可以视为阶段性关闭：
  - ordinary guidance / generic examples / residual subgroup freeze / compiler-deprecated alignment 都已经收齐
  - 后续更应该回到更大的接口设计与各 backend completeness 审查
  - 不应再把 verify-result wording / grep 误报当成新的核心实现问题反复拉起

- 当前这轮 broader interface-design 审查又压出一条真实且高可见的 canonical truth 漂移：
  - `src/fafafa.ssl.base.pas` 已明确把 `GetNativeHandle` 放在 `ISSLNativeHandleAccess`
  - `docs/reference/API_REFERENCE.md` 却还把 `GetNativeHandle` 列进 `ISSLContext` code listing
  - `docs/reference/INTERFACE_DESIGN_V2.md` 还把它画进 `ISSLConnection` core
  - 同一份 `INTERFACE_DESIGN_V2.md` 还把 `GetSelectedALPNProtocol` 误画进 `ISSLClientConnection`

- 这说明当前问题不是“设计讨论还没达成一致”，而是活跃 truth source 已经开始互相打架：
  - canonical source truth 已经是 optional native-handle surface
  - 但 active reference / design doc 仍在把它教回 core
  - 这会直接削弱我们前面已经花很多批次收出来的 `optional-owner` 分层语义

- 更关键的是，这条漂移不只停留在文档：
  - `tests/connection/test_ssl_connection_local.pas` 的 fresh compile RED 直接证明 generic smoke 还在按旧 core 假设读 `ClientConnection.GetNativeHandle` / `ServerConnection.GetNativeHandle`
  - 同文件也还在普通路径上直读 deprecated `ClientConnection.GetConnectionInfo`
  - 所以这是“活跃文档 + 活跃 generic test 一起失真”，不是孤立的注释错误

- 这批最小正确修法也因此很清楚，并已落地：
  - `API_REFERENCE` 的 `ISSLContext` listing 去掉 `GetNativeHandle`
  - 同页新增 `ISSLNativeHandleAccess` optional surface 说明
  - `INTERFACE_DESIGN_V2` 去掉：
    - `ISSLConnection` core 中的 `GetNativeHandle`
    - `ISSLClientConnection` 中错误的 `GetSelectedALPNProtocol`
  - migration table 也把 `GetNativeHandle` 的 owner 改成 `ISSLNativeHandleAccess`
  - `test_ssl_connection_local.pas` 改走：
    - `ISSLNativeHandleAccess`
    - `ISSLConnectionInfo.GetConnectionInfo`

- focused 回归结果说明这条面现在已经重新对齐：
  - 新 shell contract 已 PASS
  - `test_ssl_connection_local.pas` 已从 compile RED 转成 compile + runtime PASS：`27 passed / 0 failed`
  - 因而 `native-handle / owner-surface truth` 现在可以视为当前 canonical docs + generic smoke 的稳定真相

- 当前 Windows/WinSSL workflow 又暴露出一条比实现更先要修的“顶层报告失真”问题：
  - live GitHub run `26068984446` 里，`wave-b-b2-manual.yml` 的 opt-in native-probe 输入确实生效了：
    - `host=www.google.com`
    - `winssl_enable_native_probe=true`
  - 但 Windows broader runtime transcript 只走到第一条 public signal：
    - `signal label=initial_handshake reused=false info_resumed=false perf_reused=false`
  - 没有任何 `native_probe ...` marker，随后 `WinSSL Session Resumption Truth` 以 `-1073741819` 失败
  - 这说明当前活跃问题首先是“Windows opt-in runtime 在 live runner 上确实失败”，不是“只是本地无法复现”

- 真正误导后续路线判断的根因，不在 runtime transcript 本身，而在报告链没有传播这条失败：
  - 同一批生成的 `wave_b_cross_platform_summary_*.md` 仍写 `windows | PASS`
  - 同一批生成的 `wave_b_b2_handoff_bundle_*.md` 仍写 `handoff_state: CLOSED`
  - 根因是旧版 cross summary / handoff bundle 只信 Windows summary，对 sibling `winssl_runtime_suite_<run_id>.log` 的明确 `suite_end_status=FAIL` 没有任何提升逻辑

- 这批最小正确修法因此应当非常窄：
  - 不修改 `src/fafafa.ssl.winssl.connection.pas`
  - 不重开 safer native probe seam
  - 只让顶层 report/handoff 在 transcript 明确 `suite_end_status=FAIL` 时传播失败真相：
    - `generate_wave_b_cross_platform_summary.sh` 新增可选 `--windows-runtime-transcript`
    - 仅在 transcript 明确 `FAIL` 时，把 Windows state 提升成 `FAIL`
    - `prepare_wave_b_b2_handoff_bundle.sh` 把 sibling runtime transcript 传给 cross summary
    - 若 transcript 明确 `FAIL`，则 handoff state 至少落到 `NEEDS_GATE_REPAIR`

- 这批也顺手把一个漂移 fixture 拉回了当前仓库真相：
  - `test_prepare_wave_b_b2_handoff_bundle_gate_repair_state_contract.sh` 原先用空 runtime transcript，却还期待 consistency 为绿
  - 这与当前仓库已经固定下来的“Windows runtime transcript 至少应带 suite markers”规则不一致
  - 现在该 fixture 改成 substantive PASS transcript，避免以后把旧 fixture 噪声误认成新回归

- 当前应保留的边界也已经很明确：
  - `check_wave_b_b2_evidence_consistency.sh` 的 `CONSISTENT` 语义这批没有改
  - 也就是说，`CONSISTENT` 目前仍代表 evidence chain 自洽，不等于 Windows gate 已通过
  - 如果后续还要补 workflow truth，下一刀应单独收 `consistency` / `next actions` wording，而不是把实现调查、报告链修复、语义重写混成一批

- OpenSSL CT 这条线这次终于把真正的漂移边界钉实了：
  - 默认初始化下，`OpenSSL` capability 看起来仍是 `False/None`
  - 但这不是因为 public truth 设计正确，而只是因为 `osmCT` 默认没有被加载
  - 一旦其他代码路径把 `osmCT` 标记成 loaded，`src/fafafa.ssl.openssl.backed.pas` 就会把：
    - `sslFeatCertificateTransparency`
    - `SupportsCertificateTransparency`
    - `CertTransparencySupport`
    全部抬成可用
  - 与此同时，`TOpenSSLConnection` 仍然没有公开：
    - `ISSLCertificateTransparency`
    - `ISSLCertificateTransparencyValidation`

- 这说明之前的问题本质不是“默认 runtime 立即失真”，而是“低层 binding readiness 被误当成默认 backend public capability truth”：
  - 这类漂移更危险，因为平时不一定红
  - 但只要另一路代码加载了 CT binding，全仓 capability / selector / caller 判断就会被带偏

- 当前最小正确修法已经明确并落地：
  - 不扩到 OpenSSL CT connection surface
  - 不重开 FreePascal CT 主线
  - 只把 OpenSSL 默认 capability truth 固定回：
    - `sslFeatCertificateTransparency = False`
    - `SupportsCertificateTransparency = False`
    - `CertTransparencySupport = sslSupportNone`
  - 同时把 `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md` 的 CT 行改成“底层 API 可用性”而不是“默认 capability 直接映射”

- focused RED/ GREEN 证据也很干净：
  - 新增 `tests/openssl/test_openssl_features.pas` 的 public-surface truth contract
  - RED 首先报出：
    - `OpenSSL must not publish CT feature support merely because low-level CT bindings are marked loaded`
  - GREEN 后同一合同转绿，并且 `tests/contract/test_backend_contract.pas` 继续保持 `0 failed`

- 继续沿着 capability/public-surface 主线往下审时，又压出一条更直接会影响 selector 的硬问题：
  - `src/fafafa.ssl.openssl.backed.pas` 之前仍把 `SupportsTPM` 直接写成 `True`
  - `src/fafafa.ssl.winssl.lib.pas` 之前仍把 `SupportsPKCS11` / `SupportsTPM` 直接写成 `True`
  - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md` 也还把“智能卡 / TPM”写成已支持

- 这条线比普通文档 drift 更危险，因为 `src/fafafa.ssl.backend.selector.pas` 会直接消费这些 capability 字段：
  - `RequireTPM`
  - `RequirePKCS11`
  - platform-score / reason generation
  - 所以只要 capability 假阳性存在，auto backend selection 就可能被带偏

- 这次静态审查把边界也钉得很清楚：
  - `OpenSSL`
    - 当前确实有 shipped `LoadPrivateKeyFromPKCS11(...)` 路径
    - `TPKCS11BackendFactory.CreateBackend(btAuto)` 也仍是当前真实 loader bridge
    - 所以 `SupportsPKCS11` 这次不该顺手砍掉
  - 但同一 backend 当前并没有 shipped TPM public/runtime path
    - 之前的 `SupportsTPM=True` 只是把“也许能靠外部 engine/provider 生态接入”误写成默认 backend public capability
  - `WinSSL`
    - 当前已发布 surface 主要是系统证书存储 / PFX / DER / Schannel connection path
    - 仓库里没有 shipped PKCS#11 URI / smart-card 私钥加载路径
    - 也没有 dedicated TPM loading/runtime contract
    - 所以 `SupportsPKCS11=True` / `SupportsTPM=True` 同样属于“平台潜在能力被误抬成 public capability truth”

- focused RED 也把这条问题从“看起来怪”压成了真实回归：
  - 新 shell contract 首先红在：
    - `OpenSSL capability truth still advertises TPM without a shipped public/runtime path`
  - `tests/openssl/test_openssl_features.pas` 新增 runtime contract 首先红在：
    - `OpenSSL must not publish TPM capability without a shipped TPM public/runtime path`
  - 新增 auto-selector downstream contract 也同步红：
    - `Auto-backend selection must fail when TPM support is required but no shipped backend publishes it`

- 这批最小正确修法也因此非常窄，并已落地：
  - 不新增 TPM / smart-card / PKCS#11 新实现
  - 不改 selector 算法
  - 只把 capability truth 拉回当前已发布 surface：
    - `OpenSSL SupportsTPM := False`
    - `WinSSL SupportsPKCS11 := False`
    - `WinSSL SupportsTPM := False`
  - 同时把 WinSSL active capability doc 改成：
    - `智能卡 / PKCS#11` 当前 capability 不发布
    - `TPM` 当前 capability 不发布

- focused GREEN 结果把这条线钉得很实：
  - shell contract 已 PASS
  - OpenSSL feature suite 已 PASS
  - auto-backend TPM truth contract 已 PASS
  - `python3 scripts/compile_all_modules.py` 继续保持 `187/187` 成功

- 因而当前 `hardware-key capability truth` 这条线已经形成了稳定结论：
  - `OpenSSL` 当前只保留 shipped PKCS#11 truth，不再把 TPM 当作默认 backend capability
  - `WinSSL` 不再把平台潜在硬件密钥能力误写成已发布 `PKCS11/TPM` capability
  - 后续若继续审这组字段，默认下一刀应是 `OpenSSL SupportsPKCS11` 是否还需要更细的 runtime-readiness gate
  - 不应再把本轮已收掉的 WinSSL / TPM 假阳性重新拉起

- 顺着这条线继续压，一条更细但同样真实的 capability 漂移也被钉实了：
  - `OpenSSL` 当前虽然保留了 shipped `PKCS#11` loader path
  - 但 `src/fafafa.ssl.openssl.backed.pas` 里的 `SupportsPKCS11`
    之前仍是 unconditional `True`
  - 这会把“仓库里存在 PKCS#11 bridge”误写成“当前运行时一定具备 Provider/ENGINE backend readiness”

- 这类问题的风险不是文案层面的：
  - `GetCapabilities` 是 selector / caller / active capability doc 会共同消费的公开 truth
  - 一旦它把运行时缺失的 Provider/ENGINE surface 仍发布成 capability-positive，
    后续就会把“加载桥存在”和“backend 真能在当前 runtime 工作”混成一件事

- 这次确认到的更细真相已经足够形成稳定边界：
  - `TPKCS11BackendFactory.IsBackendAvailable(btAuto)` 已经是现成的 runtime truth source
  - 它检查的不是 token/slot 业务配置，而是当前 OpenSSL runtime 至少是否具备：
    - Provider path:
      - `OSSL_PROVIDER_load`
      - `OSSL_STORE_open`
      - `OSSL_STORE_expect`
    - ENGINE path:
      - `ENGINE_by_id`
      - `ENGINE_init`
      - `ENGINE_load_private_key`
  - 因而当前最小正确修法不是砍掉 `OpenSSL PKCS#11` capability，
    而是让 `SupportsPKCS11` 跟随这条现成 readiness truth

- focused RED/GREEN 也把这条边界钉实了：
  - 新增 runtime contract 首先红在：
    - `PKCS#11 capability must match PKCS#11 backend auto-detection readiness`
  - 在临时切掉 Provider / ENGINE 关键 surface 后，同一套测试继续要求：
    - `PKCS#11 capability must stop claiming supported when neither Provider nor ENGINE backend is runtime-ready`
  - 回到 runtime-aware 实现后，focused OpenSSL feature suite 重新转绿

- 因而当前 `OpenSSL PKCS#11 capability truth` 这条线已经形成稳定结论：
  - `OpenSSL` 继续保留已发布的 PKCS#11 public surface
  - 但 `SupportsPKCS11` 不再是 unconditional `True`
  - 当前 public capability truth 必须跟随 `TPKCS11BackendFactory.IsBackendAvailable(btAuto)`
  - 后续若继续深审 capability rows，应优先继续找其它“helper/binding exists 就被误抬成 capability true”的残余点，而不是重开已关闭的 TPM / WinSSL 路线

- 顺着这条线继续回头看测试闭环，又压实了一条 workflow 层面的真问题：
  - 我们刚把 OpenSSL `SupportsPKCS11` 收紧成 runtime truth
  - 但 `tests/scripts/test_hardware_key_capability_truth_contract.sh`
    仍要求源码里出现：
    - `Result.SupportsPKCS11 := True;`

- 这类问题虽然不改生产行为，但风险并不小：
  - 它会让 focused contract 自己落后于当前实现 truth
  - 后续一旦 rerun，这条合同会持续报红
  - 更糟的是，它会把“把源码改回旧的 unconditional capability”重新塑造成看起来像正确修法

- 这次确认到的最小正确修法也很清楚：
  - 不改生产源码
  - 不重开 `OpenSSL PKCS#11` runtime 逻辑
  - 只把静态合同改成守护当前 truth：
    - 继续保留 shipped `LoadPrivateKeyFromPKCS11(...)` 路径
    - 要求 `LPKCS11Ready := TPKCS11BackendFactory.IsBackendAvailable(btAuto)`
    - 要求 `Result.SupportsPKCS11 := LPKCS11Ready`
    - 明确禁止旧的 `Result.SupportsPKCS11 := True;`
  - 同时把 `docs/BACKEND_CAPABILITY_MATRIX.md` 的 OpenSSL runtime-readiness 说明也纳入合同守护

- 因而当前 `hardware-key` 这条线的稳定结论要再补一层：
  - 不只是源码和 active docs 要对齐
  - focused shell contracts 也必须同步到同一套 capability truth
  - 后续若继续推进 capability/completeness 审查，应优先警惕这类“源码已收口，但静态合同还卡在旧 truth”的回漂点

- 顺着这条思路继续复审 active docs，又压实了一条更上层的误导源：
  - 不只是 shell contract 会卡在旧 truth
  - 一些仍在项目根入口被看到的 active docs，也还保留着早期静态 capability 心智

- 当前已经确认的 3 个代表性漂移都很具体：
  - `docs/MIGRATION_GUIDE_V1.1.md`
    - 仍把 `WinSSL PKCS#11` / `WinSSL TPM` 写成已支持
    - 同时还把当前默认构建的 `OpenSSL FIPS` 写成已支持
  - `docs/BACKEND_SELECTION_GUIDE.md`
    - OpenSSL 评分示例仍把 `SupportsPKCS11: Yes` 写成 unconditional truth
  - `docs/CAPABILITY_MATRIX_GUIDE.md`
    - Windows 推荐代码示例仍要求：
      - `SupportsSystemCertStore and SupportsTPM`
    - 这会把当前并不存在的 WinSSL TPM published capability 再次暗示成真实入口条件

- 这类问题的风险和测试滞后是同一类：
  - 它们会把“runtime-aware truth”重新退回“品牌/平台静态能力”心智
  - 下一次开发或审查很容易又从旧前提出发

- 这批最小正确修法因此同样保持很窄：
  - 不改 backend 实现
  - 只把 3 份 active docs 重新锚回当前 truth：
    - `OpenSSL PKCS#11` = runtime-aware
    - `WinSSL PKCS11/TPM` = 当前 capability 不发布
    - `OpenSSL FIPS` = 默认构建不发布

- 因而当前 capability/completeness 路线的稳定结论又前进一步：
  - active source truth、focused contracts、active docs 三者现在要同时对齐
  - 后续再扫这条线时，应优先找仍残留“静态品牌能力心智”的入口示例，而不是重复怀疑已经收掉的单点实现

- 再往下一层核对时，还补出一个更细的 proof gap：
  - `OpenSSL SupportsPKCS11` 的 source truth 已改成 runtime-aware
  - `hardware-key` shell contract 也已同步
  - 但 selector / builder 的下游 focused proof 仍只覆盖了 `RequireTPM`
  - `RequirePKCS11Support` 这条线还缺一条直接证明

- 这类问题的风险在于：
  - capability source 看起来已经没问题
  - 但如果 downstream proof 没补，后续仍可能出现：
    - selector/builder 行为继续被旧假设带偏
    - 或者大家以为 “RequirePKCS11Support 在任何环境都应该失败/成功”

- 这次选择的最小正确修法也因此保持很窄：
  - 不动生产实现
  - 只新增一条 runtime-aware downstream contract：
    - 若当前任一已注册 backend 发布 `SupportsPKCS11=True`
      - auto-backend selection 必须成功
    - 否则必须失败
  - 这样 selector / builder 的下游结果就重新锚回当前 capability truth，而不是锚回本机偶然环境

- 因而当前这条线又形成了一个新基线：
  - capability truth 不只是 source/doc/test 各自正确
  - 还要补齐 selector / builder 下游 proof
  - 后续若继续推进 completeness，优先值最高的是继续找这些“上游 truth 已收口，但下游 focused proof 还缺位”的点

- 再次顺着 active docs 入口往下复审时，又确认了一类平行残余：
  - `PKCS11/TPM` 不是唯一还会在文档层回漂的 capability truth
  - `FIPS` 这条线在几个高可见 active docs 里也还保留着旧的 OpenSSL 静态能力心智

- 当前已经压实的 3 个具体漂移点是：
  - `docs/reference/BACKEND_ABSTRACTION_LAYER_DESIGN.md`
    - 仍把 `OpenSSL FIPS = ✅` 写成当前能力矩阵真相
  - `docs/reference/BACKEND_SELECTOR_DESIGN.md`
    - 仍把 `OpenSSL FIPS = ✅` 写成 selector 设计层默认能力
  - `docs/PLATFORM_SUPPORT.md`
    - 仍把 OpenSSL / WinSSL 对比写成：
      - 两边都“FIPS 模式支持”

- 这类问题的风险和前两批完全同源：
  - 会把“可通过特殊构建/模块进入 FIPS 路线”误写成“当前默认 backend capability 已发布”
  - 进而让后续设计、选型、甚至平台对比都从错误前提出发

- 这批最小正确修法也继续保持很窄：
  - 不改 backend 实现
  - 只把 active docs 重新锚回当前 source truth：
    - OpenSSL 默认构建 `SupportsFIPSMode = False`
    - WinSSL 当前 `SupportsFIPSMode = True`
    - OpenSSL 若要进入 FIPS 路线，需要专门模块/构建

- 因而当前 capability/completeness 路线的 docs 真相又向前收了一层：
  - 不只是 `PKCS11/TPM`
  - `FIPS` 这一类“特殊构建/平台合规能力”也不能再被 active docs 冒充成默认 shipped capability

- 顺着 builder/selector 入口继续往下看，还确认了一条比能力矩阵更靠近用户操作面的 drift：
  - `BACKEND_SELECTION_GUIDE.md` 里虽然前几批已经修过部分 capability 表述
  - 但几个最常被直接复制的入口示例，仍带着旧的静态心智

- 当前压实的 3 个高风险入口是：
  - `WithSecurityFirst`
    - 看起来像“安全优先快捷方式”
    - 但如果不额外写明，调用方很容易把它误读成“默认已经偏向 FIPS”
  - `RequirePKCS11Support`
    - 如果只写“要求支持 PKCS#11”
    - 调用方看不到这其实依赖当前已注册 backend 的已发布 capability
    - 也看不到 OpenSSL 路线还要再受 Provider / ENGINE runtime readiness 约束
  - “政府/金融系统”场景
    - 直接摆出 `FIPS + PKCS#11`
    - 但不说明当前默认 shipped backends 未必能自动满足

- 这类问题的风险比一般示例文案更高：
  - 它会把“需求表达”和“当前部署一定可满足”混成一件事
  - 结果是后续接口设计、选型、排障都从错误期望出发

- 这批最小正确修法因此继续保持窄 scope：
  - 不改 selector/builder 代码
  - 只把 guide 入口重新锚回：
    - `WithSecurityFirst` 不等于默认 FIPS
    - `RequirePKCS11Support` 是 runtime-aware requirement
    - `FIPS + PKCS#11` 场景是需求表达，不是当前默认 shipped deployment 保证

- 顺着这条线继续往行为层收口时，又确认了一个更真实的残余点：
  - 问题已经不在 guide wording 本身
  - 而在于 `WithSecurityFirst` / `CreateSecurityFirstRequirements` 还缺一个真正可执行的 downstream proof
  - 否则下次很容易又回到“security-first 是不是其实默认偏向 FIPS”的重复争论

- 这次补上的最小正确证据因此没有继续依赖本机环境，而是直接用 mock backends 固定能力矩阵：
  - 一个 non-FIPS backend：
    - 更符合当前 security-first 的综合安全/性能评分
  - 一个 FIPS-capable backend：
    - 只有在显式 `PreferFIPSCompliant=True` 时才应该翻盘

- 新 focused contract 现在已经直接证明了 3 个关键事实：
  - `CreateSecurityFirstRequirements.PlatformPreferences.PreferFIPSCompliant = False`
  - 默认 security-first selector 会选择更强的 non-FIPS backend，而不是因为“有 FIPS backend 存在”就自动转向它
  - `WithSecurityFirst` builder 路径默认构建出来的 context 同样来自 non-FIPS backend

- 这条 proof 还顺手把一条流程层误差也校正了：
  - `task_plan.md` 顶部原先那句“goal 工具保留已完成 goal、无法再次新建”已经过时
  - 当前线程级 goal 其实仍是 active
  - 因而后续应继续把 `task_plan.md` / `docs/plans/...` 视为同一总 goal 下的子批记录，而不是假设 goal 工具已经失效

- 再把主线切回 interface/backend completeness 时，`ISSLOCSPStapling` 的 residual 面已经可以被视为一个单独收口的 backend-specific subgroup：
  - ordinary docs 之前的问题已经在 active-guidance 批次收掉
  - 当前重新扫 direct-core `GetOCSP*` 命中后，实际只剩 4 个文件：
    - `tests/mbedtls/test_mbedtls_ocsp_capability.pas`
    - `tests/openssl/test_ocsp_connection_verification_regression.pas`
    - `tests/test_openssl_connection_ocsp_storectx_issuer_contract.pas`
    - `tests/test_wolfssl_ocsp_stapling_contract.pas`

- 这 4 个 residual files 的性质也已经足够明确：
  - `tests/mbedtls/test_mbedtls_ocsp_capability.pas`
    - 是 unsupported/fail-closed capability/runtime proof
  - `tests/openssl/test_ocsp_connection_verification_regression.pas`
    - 是 stapled-response status/verification runtime regression proof
  - `tests/test_openssl_connection_ocsp_storectx_issuer_contract.pas`
    - 是 storectx issuer fallback fail-closed contract proof
  - `tests/test_wolfssl_ocsp_stapling_contract.pas`
    - 是 WolfSSL public stapling surface default/runtime proof

- 因而这批最小正确动作不是再把它们迁成 `ISSLOCSPStapling` owner-path 写法，而是做 residual freeze：
  - 在 source comment 中明确 `ISSLOCSPStapling` 才是 owner surface
  - 在 residual files 中显式标注 `INTENTIONAL_OCSP_CORE_SURFACE`
  - 用 focused contract 守住这 4 个文件就是当前全部 direct-core `GetOCSP*` residual set

- 这样一来，`ISSLOCSPStapling` 这条线就和前面的 verify-result / ALPN / connection-info residual 路线一样，进入了“已分类冻结”的状态：
  - 后续不应再把这组 OCSP direct-core hits 当成普通 guidance 漂移反复拉起
  - 默认主线应继续回到更大的 backend implementation-completeness 审查

- 把主线继续往实现结构层收时，又暴露出一个和 earlier early-data/server-OCSP gating 同类的 public-path drift：
  - `tests/contract/test_backend_contract.pas` 的 `Contract 10` 一直要求：
    - `OCSPStaplingSupport<>None` -> client connection 暴露 `ISSLOCSPStapling`
    - `OCSPStaplingSupport=None` -> client connection 不暴露 `ISSLOCSPStapling`
  - 但 `TOpenSSLConnection` / `TWolfSSLConnection` 之前仍直接实现 `ISSLOCSPStapling`
  - 这意味着一旦 runtime capability 退到 `none`，public `CreateConnection(...)` 路径仍可能把 connection 误暴露成 OCSP-capable

- 这不是普通文案或 residual archaeology，而是 public factory/context path 的结构性 capability drift：
  - 问题不在 OCSP getter 逻辑本身
  - 问题在于 class/interface 暴露矩阵没有跟 capability truth 一起收缩
  - 和之前已修的：
    - `ISSLEarlyDataContext`
    - `ISSLEarlyDataConnection`
    - `ISSLServerOCSPStaplingContext`
    属于同一类风险

- 当前最小正确修法也因此没有重写 runtime OCSP 行为：
  - 只把 `OpenSSL` / `WolfSSL` 的 public connection creation path 改成 capability-aware subclass matrix：
    - `base`
    - `ocsp`
    - `early-data`
    - `early-data + ocsp`
  - 让 `CreateConnection(ASocket/AStream)` 按当前 capability truth 选择正确 subclass

- focused proof 现在已经把这条结构真相钉住：
  - source contract 直接守住：
    - base connection 不再无条件实现 `ISSLOCSPStapling`
    - dedicated `ocsp` / `early-data` / combined connection subclass 都存在
    - `CreateConnection(...)` 的 matrix selection 与 capability truth 对齐
  - `tests/contract/test_backend_contract.pas` 重新编译并运行全绿：
    - `OpenSSL` / `WolfSSL` 继续通过 `Contract 10`
    - `MbedTLS` 继续保持 client-side `ISSLOCSPStapling` absent
    - 其余 optional-interface contracts 没被这次矩阵调整打坏

- 因而这条 `client-side ISSLOCSPStapling` optional-interface drift 现在也已经进入关闭状态：
  - 后续不应再把 `OpenSSL` / `WolfSSL` client OCSP optional interface 当成未分类结构风险反复拉起
  - 默认主线应继续回到更大的 backend implementation-completeness 审查

- 顺着 backend completeness 继续往 capability/source truth 收时，又确认了一条真实而且之前没有冻住的漂移：
  - `SupportsCallbacks` 不是一个抽象文案问题
  - 它已经在各 backend 上出现了“runtime 存在但 capability 未发布”和“setter-only placeholder 却提前发布 capability”两种相反方向的偏差

- 这次把 `SupportsCallbacks` 的当前判定基线压实成了一个更可执行的定义：
  - 不是“接口上有 setter 就算支持”
  - 而是“至少一条 context callback path 具备真实 runtime wiring，才算 published callback capability”

- 在这个基线下，5 个 backend 的分类已经足够清楚：
  - `OpenSSL`
    - 已发布 `SupportsCallbacks=True`
    - verify/password/info callback 都有真实 thunk/runtime wiring
  - `WinSSL`
    - verify/info callback 在 connection/runtime path 被真实消费
    - 当前应发布 `SupportsCallbacks=True`
  - `FreePascal`
    - verify/password/info 当前只有 setter / field 存储
    - 未看到 runtime use-site
    - 因而 `SupportsCallbacks=True` 是误发布
  - `WolfSSL` / `MbedTLS`
    - 同样更接近 setter-only / storage-only
    - 在没有 runtime wiring 前不应发布 `SupportsCallbacks=True`

- 这次 RED 也给了很干净的双重证据：
  - source contract 先抓到了：
    - `WinSSL` capability 没有显式发布 `SupportsCallbacks=True`
  - 新增的 backend capability runtime truth contract 又在 Linux 上直接抓到了：
    - `FreePascal Native SupportsCallbacks mismatch: expected=False actual=True`

- 因而这批最小正确修法没有去提前重构 callback API，而是先把 capability 真相统一回来：
  - `WinSSL`
    - 补上 `Result.SupportsCallbacks := True`
  - `FreePascal`
    - 改回 `Result.SupportsCallbacks := False`
  - `WolfSSL` / `MbedTLS`
    - 显式固定 `Result.SupportsCallbacks := False`
  - `TSSLBackendCapabilities.SupportsCallbacks`
    - 源码注释补成“至少一条 callback 具备真实 runtime wiring”

- 这样做的价值不只是修两行 bool：
  - 它把 selector / capability audit / future docs 的判断基线统一了
  - 也把 callback 这条线从“到底算不算支持”的反复争论，推进到了下一个真正有价值的问题：
    - 对 `SupportsCallbacks=False` 的 backend，setter-only compatibility surface 是否应该 fail-closed
    - 还是至少要在 active docs / API reference 中明确标出 compatibility-only truth

- 顺着这条线继续往 setter semantics 收时，很快就确认这已经不是抽象设计问题，而是一个直接会误导调用方的 runtime drift：
  - `FreePascal` / `WolfSSL` / `MbedTLS` 虽然已经发布 `SupportsCallbacks=False`
  - 但 `SetVerifyCallback` / `SetPasswordCallback` / `SetInfoCallback` 仍会静默接收 non-nil 回调并存进字段
  - runtime 又永远不消费它们
  - 这属于典型的 silent no-op / misleading setter surface

- 与此同时，active docs 里还冒出了一个并行的 callback truth 漂移：
  - `docs/reference/API_REFERENCE.md` 的 callback 类型签名示例
  - 仍写着旧的：
    - `aPreverified / aCert`
    - `aHint / aMaxLen`
    - `aInfo`
  - 但当前源码真实签名已经是：
    - `TSSLCertificateInfo + ErrorCode + ErrorMessage`
    - `var Password + IsRetry`
    - `Where + Ret + State`

- 这次因此把 callback setter 的当前正确语义压成了一个更明确的基线：
  - `SupportsCallbacks=True`
    - non-nil callback assignment 允许继续工作
    - `nil` 用于清除并回到默认行为
  - `SupportsCallbacks=False`
    - non-nil callback assignment 必须 fail-closed 为 `unsupported`
    - `nil` 只保留为清除/保持默认行为的 compatibility operation

- 新的 RED 也给了两类直接证据：
  - shell/source contract 先抓到了：
    - `base` / `API_REFERENCE` 还没有 callback capability gating note
  - runtime contract 又直接抓到了：
    - `FreePascal Native must reject non-nil Verify callback while SupportsCallbacks=False`

- 因而这批最小正确修法继续保持窄 scope：
  - 不改 `OpenSSL` / `WinSSL` published callback path
  - 不发明新的 callback capability 结构
  - 只让 `FreePascal` / `WolfSSL` / `MbedTLS` 的 non-nil callback setter 显式 fail-closed
  - 同时把 active docs 的 callback gating 与 callback type signatures 对齐回源码真相

- 这样一来，callback 这条线现在已经从：
  - “bool 能力有没有发对”
  - 推进到了：
  - “未发布能力的 setter 还能不能 silently accept”
  - 并且把 active docs 的签名真相也一起收回来了

- 收口后剩下的下一个高价值问题也更清楚了：
  - `WinSSL` 当前被归入 `SupportsCallbacks=True`
  - 但从现有静态证据看，更像 verify/info 有 runtime use-site，password callback 未必真的接线
  - 也就是说，下一批更值得做的是 callback surface granularity 审查：
    - 要么补 WinSSL password callback runtime completeness
    - 要么承认当前 bool 只代表 partial callback publication，并把 active docs 写清

- 继续顺着这个残余点往下收后，静态证据已经足够明确：
  - `WinSSL` 并不是“三种 callback 都已发布”
  - 它更接近：
    - verify callback 已接线
    - info callback 已接线
    - password callback 未接线

- 这里最危险的不是 bool 本身，而是 `SetPasswordCallback` 的静默表象：
  - 代码会接收 non-nil password callback
  - 存入 `FPasswordCallback`
  - 但没有任何 runtime use-site
  - 同时 `tests/unit/test_winssl_comprehensive.pas` 还把这条 silent setter 当成“Password callback set”通过条件

- 因而这批的最小正确结论不是推翻 `SupportsCallbacks=True`，而是承认它当前是 coarse-grained publication：
  - 至少一条 callback path 已发布
  - 但具体 callback 种类仍可能 backend-specific
  - 当前 `WinSSL` 只应发布 verify/info runtime path
  - password callback 应 fail-closed

- 这次补上的修法因此继续保持窄 scope：
  - `TWinSSLContext.SetPasswordCallback`
    - non-nil -> `unsupported`
    - `nil` -> clear / no-op
  - `SetVerifyCallback` / `SetInfoCallback`
    - 保持当前 published path
  - `test_backend_callback_setter_fail_closed_contract`
    - 从“published backend 三个 callback 都能设”改成 WinSSL partial matrix
  - `test_winssl_comprehensive`
    - 改成 password callback unsupported 预期
  - active docs
    - `API_REFERENCE`
    - `WINSSL_DESIGN`
    一起写清 partial-publication truth

- 这样一来，callback 路线又向前收了一层：
  - 不只是 false-capability backend 不再 silently accept
  - 连 `SupportsCallbacks=True` 的 coarse-grained backend，也不再把未发布的具体 callback 种类继续伪装成“可安全配置”

- 把 WinSSL partial-publication 收口后，又暴露了一个纯 docs 层但很容易让人反复误读的缺口：
  - `API_REFERENCE` 已经写明 callback gating 和 WinSSL partial truth
  - 但最常先被看到的 active matrix docs 还没有这层信息
  - 这会导致：
    - 看 API 参考的人知道 `WinSSL` password callback 仍 unsupported
    - 只看能力矩阵的人却根本看不到 callback publication granularity

- 这次因此没有继续动代码，而是把 callback truth 从 API 参考页推进到了 active matrix 层：
  - `BACKEND_CAPABILITY_MATRIX`
    - 补了 `Context Callbacks` 快速参考行
    - 明确：
      - `OpenSSL` = 全发布
      - `WinSSL` = verify/info only
      - `FreePascal` / `WolfSSL` / `MbedTLS` = unpublished + fail-closed
  - `WINSSL_BACKEND_CAPABILITY_MATRIX`
    - 补了 `Context callbacks` 行
    - 补了 coarse `SupportsCallbacks=True` 的解释 note

- 这类修法虽然只动文档，但价值是实打实的：
  - 它把 callback granularity truth 从“分散在几个 batch 结果里”
  - 变成“在 active capability 总览里直接可见”
  - 以后再继续 callback/completeness 审查时，就不需要每次从 `API_REFERENCE` 反向解释到 matrix docs

- 顺着 callback 主线继续往 capability/source 里审时，又发现了另一条同构漂移：
  - `FreePascal` / `WolfSSL` 当前都把 `SupportsPasswordProtectedKeys` 发布成 `True`
  - 但实现侧并没有 published password-protected private-key runtime path
  - 这不是“文档说得不够细”，而是 capability 真值本身已经偏宽

- `FreePascal` 的问题最直接：
  - `LoadPrivateKey(const AFileName, APassword)`
  - `LoadPrivateKey(AStream, APassword)`
  - `LoadPrivateKeyPEM(const APEM, APassword)`
  这三条路径都只是存储 key material，`APassword` 之前直接落成 `if APassword <> '' then;`
  - 也就是说 caller 明确提供了 non-empty password，但 backend 没有消费，也没有 fail-fast
  - 这和之前 `SupportsCallbacks=False` backend 的 silent setter drift 属于同类错误：参数表面存在，但 runtime 不发布对应语义

- `WolfSSL` 的问题形态稍有不同，但本质相同：
  - `LoadPrivateKey*` 三条路径也没有真正消费 non-empty `APassword`
  - 同时源码里还保留着“密码回调需要单独设置”的旧注释
  - 但当前仓库已经明确：
    - `SupportsCallbacks=False`
    - password callback setter 对 non-nil 已 fail-closed
  - 所以继续把 `SupportsPasswordProtectedKeys=True` 留着，只会强化错误心智模型

- 这一批的最小正确结论因此不是“马上补齐 FreePascal/WolfSSL 的 encrypted key runtime”，而是先把真相收回：
  - `FreePascal` / `WolfSSL`
    - `SupportsPasswordProtectedKeys=False`
    - non-empty `APassword` -> fail-closed `unsupported`
  - `WinSSL`
    - 继续保留 coarse-grained `SupportsPasswordProtectedKeys=True`
    - 但 active docs 必须写清：
      - 当前只有 password-protected PFX/P12 import path 已发布
      - PEM private-key password path 仍 unsupported

- 这样收口之后，这条能力线也和 callback 线统一了治理规则：
  - 没有 published runtime path 的 capability，不再继续发布为 `True`
  - 没有 published runtime path 的非空输入参数，不再 silent-ignore，而是 fail-closed

- 继续顺着 private-key capability 往下审时，又在 `WinSSL` 上挖到一条更危险的残余：
  - 这次不只是 capability 说宽了
  - 还有真实 runtime path 在 unsupported 输入上 silent-success

- 静态证据已经很明确：
  - `src/fafafa.ssl.winssl.lib.pas`
    - 之前仍把 `SupportsDERPrivateKey=True`
    - 之前仍把 `SupportsPKCS8PrivateKey=True`
  - 但 `src/fafafa.ssl.winssl.context.pas`
    - `LoadPrivateKey(file/stream, password)` 实际只走 `PFXImportCertStore`
    - `LoadPrivateKeyPEM` 明确直接 `unsupported`
  - 也就是说 WinSSL 当前 published private-key path 本质上只有：
    - `PFX/P12` bundle import
  - bare DER / PKCS#8 private-key import 并没有 shipped runtime path

- 更严重的是 `LoadPrivateKey(AStream, APassword)` 的旧实现：
  - 在 `PFXImportCertStore(...) = nil` 的 else 分支里，错误地写成了：
    - `if AStream = nil then raise ...`
  - 由于进入该分支时 `AStream` 本来就是 non-nil
  - 所以普通 PEM/DER 私钥流在 WinSSL 上可能直接：
    - 不抛错
    - 不加载
    - 悄悄返回
  - 这属于比单纯 capability 假阳性更危险的 fail-open / silent-success 语义

- 这一批因此保持最小但强约束的修法：
  - 不补做 WinSSL 的 bare DER / PKCS#8 runtime
  - 只把 capability 收回到真值：
    - `SupportsDERPrivateKey=False`
    - `SupportsPKCS8PrivateKey=False`
    - `SupportsPKCS12=True`
  - 并把 non-PFX private-key 输入统一收紧为：
    - `nil` stream -> invalid param
    - non-PFX input -> `unsupported`

- 这批还顺手暴露了另一类容易反复误导人的 active docs drift：
  - `WINSSL_USER_GUIDE` 已经承认 `LoadPrivateKey` 是 `PFX`
  - 但 `WINSSL_QUICKSTART` / `WINSSL_BEST_PRACTICES` 仍然举：
    - `client.key`
    - `server.key`
    这种 bare key file 例子
  - 这会让调用方直接照着写出当前 WinSSL 不支持的调用

- 收口后，WinSSL 这条 private-key 路线现在才重新和源码真相一致：
  - password-protected path：
    - 仍只发布 `PFX/P12`
  - bare key format path：
    - `DER` / `PKCS#8` 当前都不发布
  - docs / guide 示例：
    - 也不再继续暗示 `client.key` / `server.key` 是 WinSSL 当前推荐路径

- 顺着 backend-specific active docs 继续往上抬一层后，又确认了一类更容易反复误导开发路线的漂移：
  - 有些高入口参考页已经不只是“某个 capability 讲宽”
  - 而是整个页面仍停留在更早阶段的 snapshot 心智
  - `docs/reference/API_INVENTORY.md` 就是这一类代表：
    - 仍只列 `OpenSSL` / `WinSSL` context/connection family
    - 仍把 shipped `OCSP` compatibility methods 写成“待实现”
    - 仍把 `PKCS#11` / `OCSP Stapling` 写成 next-step backlog

- 这类页面的风险比普通文档漂移更高：
  - 它们往往是“重新进入项目时最先打开的入口页”
  - 一旦高入口页先把人带回旧世界
  - 后续就算 source truth 已经收口，审查和讨论也会一直从错误前提出发

- 这次因此没有去补更多 runtime，而是先把高入口真相源重新定型：
  - `API_INVENTORY.md`
    - 不再承载 phase snapshot / 测试统计 / 性能数字 / next-step 待办
    - 只保留 current public-surface index
  - 这个方向的价值在于：
    - 以后继续审接口设计时，入口页先给出的是 current surface
    - 而不是一份已经失效的历史阶段报告

- 顺着这条线继续看 `PKCS11` 专题页，又确认了另一层同构 drift：
  - builder 示例虽然已经较新
  - 但高层叙事还没有显式强调：
    - 当前 published PKCS#11 path 只在 `OpenSSL` backend
    - capability truth 是 runtime-aware，而不是“仓库里有代码就算支持”
    - 其它 backend 当前 `SupportsPKCS11=False`

- 这类问题之所以要单独收，是因为它会直接模糊两个边界：
  - `Provider / ENGINE` 是 `OpenSSL` runtime backend 的内部选择
  - 它不等于“所有 SSL backend 都有 PKCS#11 public capability”
  - 如果不写清，后面再看 `WinSSL` / `FreePascal` / `MbedTLS` / `WolfSSL` 时，很容易误以为只是“尚未接线”，而不是“当前根本没有 published path”

- 这批收口后的稳定结论是：
  - 高入口参考页应该优先写 current source/runtime truth，而不是保留历史阶段叙事
  - `PKCS11` 当前的 public truth 是：
    - `OpenSSL` backend 有 shipped path
    - `SupportsPKCS11` 跟随 `TPKCS11BackendFactory.IsBackendAvailable(btAuto)`
    - 非 `OpenSSL` backend 当前不发布 PKCS#11 capability

- 因而当前路线图又更清晰了一层：
  - 后续如果继续做“接口设计与各 backend 实现”的全面验证
  - 下一优先级不该回头重扫已完成 capability 行
  - 而应继续审其它高入口 guide/reference 页面，看看是否还残留：
    - 统一等价接口叙事
    - 历史 phase snapshot 叙事
    - 或 backend-specific truth 被抹平的入口示例

- 顺着高入口 reference 页继续往下扫时，又抓到一个很典型但价值很高的入口页残余：
  - `docs/guides/WINSSL_QUICKSTART.md`
  - 这次的重点不再是 capability matrix，而是 quickstart 本身还在输出过期 runtime 结论

- 这条 drift 的危险性在于它不是单个错句，而是同页内部自相矛盾：
  - 在“配置选项”和“故障排查”里仍写：
    - `sslVerifyPeer` 待实现
    - `sslVerifyFailIfNoPeerCert` / mTLS 待实现
    - `LoadCAFile` 待实现
    - 证书验证未实现时才需要手动模式
  - 但同页 FAQ 又已经承认：
    - 自动证书验证已实现
    - 双向 TLS 已支持

- 这类 quickstart drift 的后果比深层参考页更直接：
  - 它会让第一次接触 WinSSL 的调用方立刻做出错误实现决策
  - 比如以为：
    - 生产环境还不能开 `sslVerifyPeer`
    - mTLS 还不能用
    - `LoadCAFile` 只是未来计划
  - 结果就是把已经闭合的实现路径继续当成缺口

- 这批确认下来的更细真相有三层：
  - 当前 WinSSL source/tests 已经明确支持：
    - `SetVerifyMode([sslVerifyPeer])`
    - `SetVerifyMode([sslVerifyPeer, sslVerifyFailIfNoPeerCert])`
    - `LoadCAFile(...)`
    - 本地/脚本化 mTLS 证据路径
  - 当前 quickstart 里还残留旧语法：
    - `Ctx.SetVerifyMode(sslVerifyPeer);`
    - `Ctx.SetVerifyMode(sslVerifyPeer or sslVerifyFailIfNoPeerCert);`
  - SNI 调试示例还在继续使用 deprecated context-level：
    - `Ctx.GetServerName`
    - 这会把 earlier SNI owner-path 收口再次讲散

- 这批最小正确修法因此继续保持很窄：
  - 不动 WinSSL 实现
  - 只把 quickstart 改回当前 public API / runtime truth：
    - `[]` / `[sslVerifyPeer]` / `[sslVerifyPeer, sslVerifyFailIfNoPeerCert]`
    - `LoadCAFile(...)`
    - per-connection `ISSLClientConnection.GetServerName`
    - 当前验证失败语义

- 这次也再次证明了一个路线图原则：
  - source truth 和 matrix truth 收口之后
  - 高入口 quickstart 仍然可能拖着旧阶段心智
  - 所以后续“接口设计完整 / 各 backend 实现完整 / 测试和文档完整”的推进，不能只盯 capability rows
  - 还要继续清理这些最容易被复制粘贴的入口示例页

- 顺着 quickstart 入口继续往 specialized guide 扫时，又确认了一类比“待实现措辞”更危险的 drift：
  - 指南直接示范了不存在的接口
  - `SECURITY_GUIDE.md` 的 HSM 段落就是这种情况

- 这类问题之所以优先级高，是因为它不是口径宽一点，而是会把调用方直接带去写根本不存在的代码：
  - `LoadPKCS11Engine(...)`
  - `LoadKeyFromHSM(...)`
  - `LContext.SetPrivateKey(...)`
  - 这些都不是当前 fafafa.ssl 的 public API

- 同时这页还混入了另一层 generic-truth drift：
  - 直接把 `LContext.LoadPrivateKey('server.key', 'strong-password')` 当作通用密码私钥示例
  - 但当前真实情况是：
    - 传 non-empty `APassword` 前必须先看 `SupportsPasswordProtectedKeys`
    - `WinSSL` 只有 PFX/P12 password path
    - `FreePascal` / `WolfSSL` 当前 non-empty password 会 fail-closed

- 这次收口后，这条线的稳定结论也更清晰了：
  - 安全指南如果讲密钥管理，不应该再跳过 capability gate
  - 也不应该把 backend-specific HSM path 伪装成 generic helper API
  - 当前 public truth 只有：
    - password-protected private key 先看 `SupportsPasswordProtectedKeys`
    - HSM / PKCS#11 path 只在 `OpenSSL` backend，且仍受 runtime-ready `SupportsPKCS11` 约束

- 这对总体路线图也有帮助：
  - 我们现在不只是清理“字段真值”和“入口页措辞”
  - 还在清理“会让用户直接写错代码”的 guide 示例层
  - 后续继续推进文档完整性时，优先级应继续放在这类 executable examples，而不是先去美化统计数字

- 在把“会让人写错代码”的 guide 示例收掉之后，下一类残余也变得清楚了：
  - 一些 specialized guides 虽然不再直接误导 API 调用
  - 但仍把历史测试快照混在当前正文里
  - `CMS_USER_GUIDE` / `PKCS12_USER_GUIDE` 是这一类代表

- 这类问题的核心不是“数字不漂亮”，而是文档角色混乱：
  - 指南正文本该回答“当前怎么用、当前 surface 是什么”
  - 但它们还夹带着：
    - `43/43`
    - `34/34`
    - `100.0%`
    - `总测试数`
    - 大段 captured expected output
    - 历史阶段更新日志
  - 一旦真实测试继续演化，这些数字就会迅速变成另一种高入口假真相

- 这次收口后，又补出了一个文档治理原则：
  - specialized guide 可以保留测试入口和验证命令
  - 但不应该把“某一次跑出来的固定统计结果”写成正文 truth
  - 正确做法是：
    - 保留命令
    - 说明成功标准
    - 明确以当前运行结果为准

- 这对后续“文档完整”阶段很关键：
  - 我们已经在收两类最常见回漂源：
    - guide 直接示范错 API
    - guide 把历史统计快照当 current truth
  - 后面继续推进时，就可以更系统地扫描剩余 specialized guides / performance docs / historical pages

- 顺着 specialized guides 继续往下扫时，又确认了 `PKCS7_USER_GUIDE.md` 里还有一类高入口 drift：
  - 它不是 helper/API 假路径
  - 也不只是 `预期输出`
  - 而是直接把固定状态、固定性能、固定通过率写成当前正文结论：
    - `Production Ready`
    - `100%`
    - `158/158`
    - 固定 `2 ms`
    - 固定 `500 ops/s`

- 这类 drift 的危险性在于：
  - 调用方会把某次历史跑数当作长期 capability/status truth
  - 甚至误以为 PKCS7 已经有某种稳定“完成度字段”或发布承诺
  - 但当前 source/reference 真相并不是这样

- 这批收口后，PKCS7 这条线的稳定结论更清楚了：
  - `PKCS7_USER_GUIDE` 当前应该明确只覆盖 `OpenSSL` backend surface
  - 当前 public 入口不只是 raw `PKCS7_sign` 这组函数
  - 还包括同单元里已经发布的 helper：
    - `SignData`
    - `VerifySignedData`
    - `EncryptData`
    - `DecryptData`

- 同时，`PKCS7` 当前能力判断也必须继续和 matrix truth 对齐：
  - 它没有一对一 capability 字段
  - 正确口径是：
    - `LoadPKCS7Functions`
    - 模块加载状态 `osmPKCS7`
    - focused tests

- 这批对总体路线图也有帮助：
  - 我们现在已经连续收掉三类文档回漂源：
    - guide 直接示范不存在的 API
    - guide 把历史测试统计快照写成 current truth
    - guide 把固定性能/状态结论写成 current truth
  - 所以下一步优先级继续放在高入口页是对的，不需要回头重扫已经闭合的 capability/source 线

- 新一轮 residual scan 也已经把下一队列收窄了：
  - `WINSSL_USER_GUIDE.md` 还残留固定运行数据表述
  - `QUICKSTART_30SEC.md` / `5_MINUTE_QUICKSTART.md` 还残留 captured `预期输出`
  - `ARCHITECTURE.md` 还残留阶段完成度口径
  - `PERFORMANCE_GUIDE` / `PERFORMANCE_OPTIMIZATION_GUIDE` 还有 phase/baseline 类旧叙事，但优先级低于前面的高入口页

- 顺着 WinSSL 高入口 guide 继续往下扫时，又确认了一类和 PKCS7 类似的运行时 drift：
  - `WINSSL_USER_GUIDE.md` 已经不再整体过度宣称“100% 完成”
  - 但它底部还保留了一张固定性能/稳定性表
  - 这会把某次 Windows 跑数误导成当前长期 runtime truth

- 这类 WinSSL drift 的特殊风险在于：
  - 它不是纯静态 API 指南
  - 它直接触碰 runtime baseline
  - 而 WinSSL 恰好又强依赖：
    - Windows 版本
    - Schannel
    - runner/宿主机
    - 网络路径
    - 目标站点
  - 所以固定 latency / throughput / success-rate 比一般文档数字更容易漂移

- 这批收口后，WinSSL runtime 口径又更清楚了一层：
  - 用户指南不该继续内嵌固定 benchmark snapshot
  - 当前更可靠的入口是：
    - `WINSSL_BACKEND_STATUS_REPORT`
    - `tests/windows/VALIDATION_BUNDLE.md`
    - `.github/workflows/wave-b-b2-manual.yml` 的 `windows-gate`
  - 当前成功标准也不该是“某个毫秒数”
  - 而应是：
    - fresh artifact / summary 存在
    - broader suite 当前可执行
    - session truth 仍与状态报告一致

- 这批也帮助我们把高入口队列再收紧了一步：
  - `WINSSL_QUICKSTART.md` 现在被抬升为更高优先级
  - 因为它仍保留：
    - `WinSSL 后端 100% 完成（所有 6 个阶段）`
    - FAQ 里的 `Phase 5 完成`
    - 以及若干阶段性完成口径
  - 这比一般 `预期输出` 更危险，因为它会直接影响第一次接触 WinSSL 的调用方心智

- 顺着 WinSSL first-contact quickstart 往下收时，又确认了一个很典型的“第一页心智漂移”模式：
  - 一页文档里同时混着：
    - 当前 verify/SNI/mTLS 语法已经是真实的
    - 但 FAQ / 比较段 / 页尾仍保留老阶段总结
  - 这会让用户在复制示例时得到正确代码，
  - 却在阅读解释时又被带回过时认知

- `WINSSL_QUICKSTART` 这次收口后，更清楚的一点是：
  - quickstart 不应承担“阶段完成度公告牌”的角色
  - 它更适合：
    - 告诉用户如何最快跑通零依赖客户端
    - 明确哪些能力已经是 current public surface
    - 明确哪些 runtime 结论仍要看状态报告/能力矩阵

- 这批也顺手修掉了一个很重要的内部矛盾：
  - 旧 quickstart 一边说：
    - server mode 已 Phase 5 完成
    - 自动证书验证已 Phase 1 完成
  - 另一边又在“选择 OpenSSL”里写：
    - 需要服务器模式（当前）
    - 需要完整证书验证（当前）
  - 这种“同页自相矛盾”比单点错句更伤路线图，因为它会让人误判后端实际边界

- 收口之后，这页的稳定原则也更明确了：
  - first-contact quickstart 可以保留：
    - 当前客户端 baseline
    - 当前 verify/SNI 配置
    - 当前权威入口
  - 但不应该再保留：
    - `100% 完成`
    - `Phase 1/5 完成`
    - 固定 benchmark 表
    - 把当前已发布 public surface 重新讲成“不如 OpenSSL 才能做”

- 这对后续路线图帮助很大：
  - WinSSL 第一接触页现在已经和用户指南、零依赖部署指南、状态报告形成同一条 truth chain
  - 下一步再清 `QUICKSTART_30SEC` / `5_MINUTE_QUICKSTART` / `ARCHITECTURE`，就可以继续把“高入口 captured snapshot / 完成度口径”这条线彻底打穿

- 顺着 WinSSL quickstart 线继续向下收时，又确认了另一类“通用入口漂移”：
  - 不一定是后端能力判断错误
  - 但会把某次历史运行的输出文本直接塞进 quickstart 正文
  - `QUICKSTART_30SEC.md` / `5_MINUTE_QUICKSTART.md` 就是这一类代表

- 这类问题的危险性在于：
  - 开发者会把 quickstart 当成“复制命令后应该看到的精确文本”
  - 一旦本机 OpenSSL 版本、目标站点、TLS 协商结果不同，
  - 就会误以为自己的环境出错，实际上只是文档把历史输出当成了 current truth

- 这批收口后，又补强了一条通用 quickstart 原则：
  - quickstart 应保留：
    - 当前命令
    - 当前示例入口
    - 当前成功标准
  - 但不该保留：
    - 固定 OpenSSL 版本字符串
    - 固定 TLS/cipher/HTTP 响应文本
    - placeholder clone URL

- 这也让高入口路线图继续收敛：
  - `QUICKSTART_30SEC` / `5_MINUTE_QUICKSTART` 已经和前面的 WinSSL quickstart / user guide 一样回到“命令 + 证据入口 + 成功标准”的文档角色
  - 当前明显残余的高入口 truth drift 已经主要收敛到 `ARCHITECTURE.md`

- 顺着通用 quickstart 线继续往下收时，又确认了 `ARCHITECTURE.md` 里的一个角色混淆问题：
  - 它已经在页首声明“当前执行顺序和阶段判断以 `docs/ROADMAP.md` 为准”
  - 但 backend 状态表自己又继续写：
    - `✅ 生产就绪`
    - `100% 完成`
  - 这会把 architecture 页重新变成另一种 release/status 面板

- 这批收口后，架构页的稳定原则也更清楚了：
  - architecture 应回答：
    - backend 如何分层
    - 哪些是默认/可选/当前主线
    - current truth source 去哪里看
  - 但不应该再直接承担：
    - shipped status 公告
    - runtime proof 公告
    - “百分百完成”式完成度表达

- 这也意味着我们这一轮高入口文档主线已经形成一个比较完整的 truth pattern：
  - specialized guides 不再直接示范错 API
  - specialized guides 不再把历史统计快照当 current truth
  - WinSSL high-entry docs 不再把阶段完成度和 benchmark snapshot 写成当前 truth
  - architecture 参考页也不再把 backend 讲成 release 公告牌

- 路线图因此出现了一个明显拐点：
  - 最危险的 high-entry drift 已经大幅下降
  - 接下来更值钱的工作会逐步从“第一页 / 入口页 truth 修正”
  - 转向“性能/历史型文档里仍残留的 phase / baseline / benchmark 快照清理”

- 顺着 WinSSL capability/source/runtime 边界继续往下审，又确认了一条容易反复误读的语义缺口：
  - `SessionCacheSupport=sslSupportStable`
    当前可以成立
  - 但它成立的层级是：
    - context-level session cache/control surface 已发布且已接线
  - 不是：
    - dedicated Windows runtime 已稳定观测到 resumed handshake

- 这条 drift 的具体危险点不在 runtime 实现，而在接口真相链没有写透：
  - `src/fafafa.ssl.base.pas`
    之前只写“会话缓存支持级别”
  - `src/fafafa.ssl.winssl.lib.pas`
    之前直接赋：
    - `Result.SessionCacheSupport := sslSupportStable`
    但没有把 stable 的层级写在旁边
  - `docs/reference/API_REFERENCE.md`
    甚至漏掉了 `SessionCacheSupport` 字段本身
  - 这会让后续读文档的人把：
    - capability stable
    - resumed-handshake proof
    混成同一个判断

- 这批之后，WinSSL session cache 这条线的稳定结论应该固定为：
  - `SessionCacheSupport`
    说的是 published cache/control surface truth
  - `SessionTicketsSupport`
    说的是 resumption/ticket surface 的支持级别
  - `observed_reuse=false` / `session_configured=true`
    仍是当前 dedicated Windows runtime truth
  - 因而不能再把 `SessionCacheSupport=stable`
    当作“resumed handshake 已经 runtime-proven”的旁证

- 顺着 WinSSL runtime 继续往下审，这次又钉死了一条更底层的事实：
  - 当前 `observed_reuse=false`
    在 broader/shared lane 上并不是“已经安全直接观测到 Schannel 没复用”
  - 而是 canonical shared path 为了避开 GitHub Windows 上的 AV，
    暂时撤下了 live `SECPKG_ATTR_SESSION_INFO` probe 后保留下来的 conservative public truth

- 这条事实的关键证据已经不再是文档猜测，而是源码直接写着：
  - `src/fafafa.ssl.winssl.connection.pas`
    - `UpdateSessionReuseTruthFromContext(...)`
      当前固定：
      - `ASessionId := ''`
      - `FSessionReused := False`
    - 并明确注释：
      - shared path 上的 `SECPKG_ATTR_SESSION_INFO` probe 仍因 Windows AV 风险而撤下

- 因而当前 WinSSL session-resumption lane 实际上有两层证据：
  - 第一层：
    - shared/public conservative truth
    - 看：
      - `observed_reuse`
      - `session_configured`
  - 第二层：
    - opt-in isolated native probe truth
    - 看：
      - `native_probe_enabled`
      - `native_observed_reuse`
      - `native_probe_succeeded`

- 之前真正会误导路线图的，不是“我们没有任何 session runtime 证据”，而是：
  - checklist / bundle / status report / API reference
    还容易把第一层 shared/public truth
    单独读成全部 runtime 结论
  - 这会让人误以为：
    - `observed_reuse=false`
      已经足够证明 native resumed-handshake 没发生

- 这批之后，WinSSL session-resumption lane 的稳定解释应该固定为：
  - public/shared 路径当前仍可安全给出 conservative truth
  - 更深 native resumed-handshake evidence 仍要看 isolated worker / opt-in probe
  - 所以 repo 下一步最值钱的动作，不再是继续改 wording，
    而是继续把最新 GitHub Windows native probe 证据拉回来

- 最新 GitHub Windows native-probe run `26104446972` 已经把下一处真实实现缺口进一步钉死：
  - 不是 workflow 没跑
  - 不是 summary marker 丢了
  - 也不是“只是 observed_reuse=false”
  - 而是 opt-in isolated worker 现在仍会在：
    - `native_probe label=initial_handshake stage=before_query_context_attributes`
    之后直接以：
    - `native_probe_worker exit_code=-1073741819`
    崩掉

- 这条 fresh runtime evidence 的价值很高：
  - 它说明当前问题已经收窄到：
    - `QueryContextAttributesW(..., SECPKG_ATTR_SESSION_INFO, ...)`
      这条 native probe 调用链本身
  - 不再是：
    - shared-path marker 解释不清
    - workflow capture 不完整
    - broader suite 没把 session lane 跑起来

- 因而当前 WinSSL session-resumption lane 的最真实状态应该是：
  - shared/public conservative truth：
    - 已稳定可记录
  - isolated native probe truth：
    - 仍是 investigatory lane
    - 当前最新 Windows run 还会在 `before_query_context_attributes`
      附近 crash
  - 这让下一步 source-side 审查方向非常明确：
    - 优先审 ABI / buffer / lifetime / attribute-binding 安全边界

- 顺着这条 fresh crash boundary 继续静态审查之后，又确认了一件更具体的事情：
  - 当前 isolated native probe 并不是“完全没有安全缓冲”
  - 但它确实还停在最直接的三参调用：
    - `QueryContextAttributesW(LCtxtHandle, SECPKG_ATTR_SESSION_INFO, @LSessionInfo)`
  - 对这种结构查询来说，官方同时还提供了：
    - `QueryContextAttributesExW(..., cbBuffer)`
  - 这给了我们一个比“继续猜 Windows 黑盒行为”更值钱的最小 source-side tightening 点

- 这批修完之后，probe-side source truth 应该这样理解：
  - canonical shared/public path：
    - 仍然保持 conservative truth，不重新碰 live session-info probe
  - isolated native probe path：
    - 现在会优先尝试
      - `QueryContextAttributesExW(..., SizeOf(SecPkgContext_SessionInfo))`
    - 只有在拿不到 ExW 入口时才回退到：
      - `QueryContextAttributesW(...)`
  - 同时 log 会多一条：
    - `stage=query_api api=query_context_attributes_exw|query_context_attributesw`
    方便直接看本次 probe 实际走了哪条 API 路径

- 这条 source-side tightening 的意义，不是宣称问题已经解决，而是把下一轮 Windows 调查的价值提高了：
  - 如果 crash 消失，说明之前很可能就是 probe 调用约束/size 路径不够收紧
  - 如果 crash 仍在，就可以把问题继续从“调用方式”缩到：
    - provider behavior
    - handle lifetime
    - `SECPKG_ATTR_SESSION_INFO` 本身的 runtime boundary

- 最新带上 `ExW 优先 + W 回退` 补丁的 Windows run `26106025515` 已经给出更细的新结论：
  - crash 还在
  - 但它不再只是停在
    - `before_query_context_attributes`
  - fresh log 已经明确告诉我们：
    - `stage=query_api api=query_context_attributesw`
  - 这说明这次 runner 上 `QueryContextAttributesEx*` 解析根本没成功，probe 实际还是走回了旧的 `QueryContextAttributesW`

- 这条新证据非常关键，因为它把问题再次缩小了一层：
  - 现在最值得追的不是：
    - `SECPKG_ATTR_SESSION_INFO` 结构是不是错了
    - `SizeOf(...)` 路径是不是没意义
  - 而是：
    - 为什么 `QueryContextAttributesEx*` 在当前 runner 上没有解析成功
    - 是模块不对、导出名不对，还是平台根本没有导出

- 所以这批新的 repo-side 修复重点不再是 probe 行为，而是 resolver 可观测性：
  - 增加候选模块/符号：
    - `secur32.dll`
    - `sspicli.dll`
    - `ExW / ExA / undecorated`
  - 明确用 `PAnsiChar(...)` 走 `GetProcAddress`
  - 再把解析结果直接打进 marker：
    - `stage=query_resolver module=... symbol=... resolved=...`

- 这能让下一轮 Windows run 直接回答一个比以前更硬的问题：
  - 到底是我们没有命中正确导出
  - 还是 runner 上确实没有任何 `QueryContextAttributesEx*` 导出可用

- 当前这批 resolver-diagnostics repo-side 收口还额外证明了一件对后续很有用的事：
  - 新增 resolver marker 和候选导出遍历之后
  - 周边几条关键静态契约仍然保持绿色：
    - session-resumption runtime truth
    - session-info probe allowlist
    - native-probe stage markers
    - native-probe handle metadata
  - 所以下一轮如果 Windows runtime 仍失败，优先应该继续收窄到真实 runner/export/provider 边界，而不是回头怀疑这批 repo-side 收口把公共语义打坏了

- 最新 Windows run `26107307586` 已经进一步排除了一个关键错误假设：
  - 不是 `QueryContextAttributesEx*` 没解析到
  - 实际 fresh marker 已经明确显示：
    - `module=sspicli.dll`
    - `symbol=QueryContextAttributesExW`
    - `resolved=true`
  - 但 worker 仍在：
    - `stage=query_api api=query_context_attributes_exw`
    之后以 `-1073741819` 退出

- 这说明当前最值钱的下一步不是再改 resolver，也不是急着引入 `SecurityFunctionTableW`：
  - 本地 `mingw-w64` 头和当前 Pascal 声明已经一致
  - 进一步查阅官方 SSPI surface 后，`SecurityFunctionTableW` 上的 `QueryContextAttributesEx*` 字段也不是一个可以直接依赖的稳定下一跳
  - 所以下一条最小调查批次应该先做 control query，对 extracted native handle 做 attribute 对照：
    - 先查 `SECPKG_ATTR_CONNECTION_INFO`
    - 再查 `SECPKG_ATTR_SESSION_INFO`
  - 这样下一轮 Windows run 才能把问题继续分成：
    - handle path / lifetime
    - 或 attribute-specific provider/runtime boundary

- run `26108237632` 已经把这个分叉彻底收口了：
  - 在同一个 extracted native handle 上：
    - `SECPKG_ATTR_CONNECTION_INFO` control query 成功
    - `QueryContextAttributesExW(..., SECPKG_ATTR_SESSION_INFO, ...)` 仍在调用后崩溃
  - 这说明：
    - handle path 本身不是当前主问题
    - 当前 crash 已经是 `SECPKG_ATTR_SESSION_INFO` 的 attribute-specific provider/runtime boundary

- 基于这条新证据，native probe worker 继续默认 hard-fail broader suite 的价值已经很低：
  - public/canonical truth 本来就没有依赖这条 probe
  - control query 已经证明 extracted handle path 正常
  - 继续让 broader suite 因 investigatory probe 非零退出而红，会持续放大一个已知 runtime boundary，而不是揭示新的 public-contract 缺口
  - 更合理的默认收口应该是：
    - evidence-only by default
    - strict only when `FAFAFA_WINSSL_REQUIRE_NATIVE_REUSE=1`

- 这条 evidence-only 收口已经被 fresh Windows CI run `26108902159` 真实验证：
  - `windows quick smoke` = PASS
  - `Run Windows Wave B gate` = PASS
  - `Run broader WinSSL runtime suite` = PASS
  - 说明我们已经把 Windows 主线从“被 investigatory native probe 拖红”收回到了“主线可绿、evidence 仍保留”的状态
  - 当前 cross-platform workflow 仍整体为 FAIL，只是因为 macOS lane 还独立失败，不再是 WinSSL native probe 主线阻塞

105. `ISSLConnection` convenience-surface 这条线目前新的真实残口不在 source / API reference，而在 active guides 的“推荐路径表达”：
   - 已复核：
     - `docs/guides/GETTING_STARTED.md`
       当前已经正确把主路径放在
       `TSSLContextBuilder` + `TSSLConnector` + `TSSLStream`
     - 漂移主要集中在：
       - `docs/INTEGRATION_GUIDE.md`
       - `docs/guides/MIGRATION_GUIDE.md`
       - `docs/guides/USER_GUIDE.md`
   - 问题本质不是 helper 不该存在，而是这些高可见 guide 仍直接展示：
     - `Conn.SetTimeout(...)`
     - `Conn.SetBlocking(...)`
     - `ReadString(...)`
     - `WriteString(...)`
     却缺少“这是 convenience / override，不是首选主路径”的显式说明
   - 最小正确修法已经压实为：
     - `INTEGRATION_GUIDE`
       明确 timeout/blocking 在 direct `ISSLConnection` 场景下只是 local override；
       如果走 `TSSLConnectionBuilder` / `TSSLConnector` / `TSSLAcceptor`，
       新代码仍应 builder-first / connector-first
     - `MIGRATION_GUIDE`
       明确 direct `ISSLConnection` 控制方式仍 shipped，
       但框架/transport 集成优先 `TSSLStream` 或 `Read` / `Write`
     - `USER_GUIDE`
       明确 `ReadString` / `WriteString` 只是快速文本往返示例，
       更复杂协议集成优先 `Read` / `Write` 或 `TSSLStream`
   - 这批 focused 验证时还顺手暴露了一个真实旧残口：
     - `test_readstring_active_example_signature_truth_contract.sh`
       一开始在 `MIGRATION_GUIDE` 上报红
     - 原因不是新说明文本，而是 migration guide 当前只展示了 `WriteString`，
       却没有把现行 `if LConn.ReadString(LResponse) then ...` 一起示例出来
   - 现在新的稳定基线应记为：
     - active guides 不再把 still-shipped convenience surface 误教成推荐主路径
     - `MIGRATION_GUIDE` 重新展示当前 `ReadString(out ...)` truth
     - `GETTING_STARTED` 已确认无需重复治理

106. 继续往更高入口的 landing docs 往外看时，新的真实 residual 也已经压实：
   - 根问题不是 `README.md` / `GETTING_STARTED.md` / `QUICKSTART.md`
     里出现了 direct `ISSLConnection`
   - 而是这些最高入口文档如果不显式说明“为什么这里回到 direct path”，
     新用户仍然会把它误判成普通新代码的推荐主路径
   - 当前已确认的三处高价值位置：
     - `README.md`
       `核心 API -> TLS 连接` 代码块直接展示 raw `Ctx.CreateConnection(...)`
     - `docs/guides/GETTING_STARTED.md`
       第 4 节 `直接用 ISSLConnection`
     - `docs/guides/QUICKSTART.md`
       WinSSL session-resumption 示例为了用 `ISSLSessionResumption`
       直接操作 connection
   - 这三处示例本身并不错误，但此前缺的都是同一层解释：
     - 普通客户端/服务端接入优先 builder + connector/acceptor + stream
     - direct `ISSLConnection` 是低层/高级/特定能力入口
     - WinSSL session-resumption 回到 direct path，
       是因为当前 public surface 通过 `ISSLSessionResumption` 挂在 connection 上
   - 现在新的稳定基线应记为：
     - root README 不再让 raw `CreateConnection` 代码块冒充 quickstart 主路径
     - `GETTING_STARTED` 已明确 direct `ISSLConnection` 只是低层入口
     - `QUICKSTART` 已明确 WinSSL session-resumption 示例的 direct-path 原因

107. 再继续往 backend-specific quickstarts 深挖后，当前新的 residual 也已经确认：
   - `MBEDTLS_USER_GUIDE` 和 `WINSSL_QUICKSTART`
     原本并不是接口名错了，也不是 capability truth 漂了
   - 真正缺的是：
     - 为什么这些专项示例会直接下探 `CreateConnection(...)`
     - 以及它们和通用 facade 主路径之间的关系
   - 当前最小正确修法已经压实为：
     - `MBEDTLS_USER_GUIDE`
       明确简单 HTTPS 示例是 backend raw shipped surface 演示，
       普通跨后端客户端仍优先 builder + connector + stream
     - `WINSSL_QUICKSTART`
       明确这页聚焦 Windows-native / WinSSL-specific path，
       所以会直接展示 `ISSLConnection`
   - 这批顺手也证明：
     - `test_mbedtls_active_docs_capability_truth_contract.sh`
     - `test_winssl_quickstart_status_phase_truth_contract.sh`
     - `test_public_unit_import_guidance_truth_contract.sh`
     在新增 direct-path 解释后仍保持绿色
   - 现在新的稳定基线应记为：
     - backend-specific quickstarts 也不会再把 direct path 误教成 generic main entry

108. active diagnostics / backend 故障页面里也存在同类但更细的小残口：
   - `TROUBLESHOOTING.md` 的
     - `LConn.SetTimeout(30000)`
     - `LConn.SetBlocking(False)`
   - `MBEDTLS_USER_GUIDE.md` 的
     - `Connection.SetTimeout(30000)`
   - 这些示例本身都不是错，但如果不加一句 current-role 说明，
     调用方仍会把它们误读成普通主路径配置，而不是：
     - direct-connection diagnostic override
     - backend-specific 故障调查入口
   - 当前最小正确修法已经压实为：
     - `TROUBLESHOOTING`
       明确 timeout / nonblocking 这两处都是 direct-connection
       diagnostic override / 调试入口
     - `MBEDTLS_USER_GUIDE`
       明确 `Connection.SetTimeout(...)` 只是 connection-level override，
       普通跨后端客户端仍优先 builder/connector/transport timer
   - 现在新的稳定基线应记为：
     - diagnostics/backends 页面不会再把 connection-level override
       误教成普通主路径配置建议

109. 高频专题页里剩下的 direct `CreateConnection(...)` 也有一类更细但真实的残口：
   - `COMMON_PITFALLS`、`security-best-practices`、`ERROR_HANDLING_BEST_PRACTICES`
     里的示例本身都没错
   - 但它们此前都还缺一句：
     - 为什么这里要直接下到 `ISSLConnection`
     - 以及如果不需要这层 low-level control，普通代码应回到什么入口
   - 当前最小正确修法已经压实为：
     - `COMMON_PITFALLS`
       把 direct path 明确标成 SNI pitfall 对照用法
     - `security-best-practices`
       把 direct path 明确标成 hostname/SNI 连接级责任展开
     - `ERROR_HANDLING_BEST_PRACTICES`
       把 direct path 明确标成 URL->socket ownership 与 exception/result
       边界示例
   - 这批顺手也证明：
     - active TLS guidance
     - selected secondary guides SNI drift
     - error-handling URL-driven SNI
     - security pinning helper truth
     这些既有 contract 在新增说明后都仍保持绿色
   - 现在新的稳定基线应记为：
     - 高频专题页也不会再把场景化 direct path 示例误教成 generic main entry

110. specialized owner-surface guides 里还存在最后一类容易让人误解的 direct path：
   - `OCSP_USAGE_GUIDE`
   - `CT_IMPLEMENTATION_GUIDE`
   - 它们其实早就在用正确的 optional interface / owner surface：
     - `ISSLOCSPStapling`
     - `ISSLCertificateTransparency`
     - `ISSLCertificateTransparencyValidation`
   - 真正缺的不是 owner path 迁移，而是：
     - 为什么这里必须回到 `CreateConnection(...)`
     - 以及如果不需要这些 owner surface，generic main path 仍是什么
   - 当前最小正确修法已经压实为：
     - `OCSP_USAGE_GUIDE`
       明确 OCSP runtime state 与 verify result 都属于连接 owner surface
     - `CT_IMPLEMENTATION_GUIDE`
       明确 CT runtime owner surface 挂在连接对象上
   - 这批也顺手证明：
     - `ISSLCertificateVerification` owner-path guidance
     - `ISSLOCSPStapling` owner-path guidance
     在新增 specialized owner-path 解释之后仍保持绿色
   - 现在新的稳定基线应记为：
     - OCSP / CT specialized guides 不会再把 owner-surface 示例误读成 generic facade 主路径

111. `EARLY_DATA_GUIDE` 也属于同一种“接口没错，但解释层还没讲透”的 residual：
   - 这页原本已经在正确使用：
     - `ISSLEarlyDataContext`
     - `ISSLEarlyDataConnection`
   - 真正缺的不是 early-data owner path 迁移，而是：
     - 为什么这里必须回到 `CreateConnection(...)`
     - 为什么还要同时从 context / connection 两侧读取和配置 owner surface
     - 以及如果不需要 early-data owner surface，generic main path 仍是什么
   - 当前最小正确修法已经压实为：
     - 在 `EARLY_DATA_GUIDE` 快速开始前明确：
       - 这里 direct path 是因为 `ISSLEarlyDataContext` /
         `ISSLEarlyDataConnection`
         分别挂在 context / connection 对象上
       - 不需要这层 owner surface 时，
         普通客户端仍可把握手入口保持在 `TSSLConnector` / `TSSLStream`
   - 这批也顺手证明：
     - `test_early_data_docs_truth_contract.sh`
       在新增 owner-surface 说明之后仍保持绿色
   - 现在新的稳定基线应记为：
     - `EARLY_DATA_GUIDE` 不会再把 early-data owner-surface 示例误读成 generic facade 主路径

112. `WINSSL_USER_GUIDE` 说明了另一种高入口 residual：
   - 这页的 capability / runtime truth 原本就大体正确
   - 真正缺的不是 WinSSL runtime 事实修复，而是：
     - 为什么入口页会直接展示 `ISSLConnection` / `CreateConnection(...)`
     - 为什么这里的 SNI 示例要落到连接对象上
     - 以及普通跨后端 HTTPS 客户端的 generic main path 仍是什么
   - 当前最小正确修法已经压实为：
     - 在 `统一 API` 段落后明确：
       - 这页作为 WinSSL-specific 用户指南，会直接展示 backend-facing path
       - 普通跨后端 HTTPS 客户端仍优先
         `TSSLContextBuilder` + `TSSLConnector` + `TSSLStream`
     - 在 `SNI 主机名` 段落后明确：
       - direct `CreateConnection(...)` +
         `ISSLClientConnection.SetServerName(...)`
         是因为 hostname/SNI 的 published surface 挂在连接对象上
       - 如果不需要直接操作这层 WinSSL-specific path，
         也可以改用 `TSSLConnector.ConnectSocket(..., host)`
   - 这批也顺手证明：
     - `test_winssl_quickstart_runtime_truth_contract.sh`
     - `test_active_direct_context_servername_surface_classification_contract.sh`
     - `test_winssl_user_guide_performance_truth_contract.sh`
     在新增 direct-path 解释之后仍保持绿色
   - 现在新的稳定基线应记为：
     - `WINSSL_USER_GUIDE` 不会再把 WinSSL backend-facing 示例误读成 generic facade 主入口

113. `WINSSL_BEST_PRACTICES` 暴露的是 WinSSL 文档里另一种更危险的 residual：
   - 不只是 direct `CreateConnection(...)` 解释层没写透
   - 它还把 WinSSL session public surface 讲成了默认性能优化路径：
     - `### 2. 启用 Session 复用`
     - `LConn.Connect;  // 快速握手`
     - checklist 里的 `启用 Session 复用`
   - 这与当前 WinSSL 权威 truth 冲突：
     - `observed_reuse=false`
     - `session_configured=true`
     - session resumption / tickets 仍只按实验性 public surface 理解
   - 当前最小正确修法已经压实为：
     - 在页首明确：
       - 这页作为 WinSSL-specific 最佳实践页，会直接展示 backend-facing path
       - 普通跨后端 HTTPS 客户端仍优先
         `TSSLContextBuilder` + `TSSLConnector` + `TSSLStream`
     - 把 session 小节改成实验性边界说明：
       - 当前 dedicated Windows runtime truth 仍是
         `observed_reuse=false` / `session_configured=true`
       - `ISSLSessionResumption` 示例继续保留，
         但只应按 connection owner path / 实验性 public surface 理解
       - checklist 不再把 Session public surface 当默认最佳实践
   - 这批也顺手证明：
     - `test_active_owner_path_docs_alignment_contract.sh`
     - `test_secondary_guides_connection_level_sni_api_drift_contract.sh`
     - `test_winssl_session_resumption_docs_truth_contract.sh`
     在新增 WinSSL session truth 说明之后仍保持绿色
   - 现在新的稳定基线应记为：
     - `WINSSL_BEST_PRACTICES` 不会再把 WinSSL session public surface 误读成默认性能优化路径

114. `PERFORMANCE_PROFILING_GUIDE` 暴露的是高可见性能页里同一类 truth 漂移：
   - 这页的问题不只是 direct `CreateConnection(...)` 没解释
   - 它还同时把：
     - `**预期提升**: 70-90% 握手时间减少`
     - `- [ ] 启用 Session 复用`
     - `| Session 复用握手 | < 10ms | 本地网络 |`
     这些固定说法写成了 current truth
   - 这与当前更 durable 的性能 truth 冲突：
     - WinSSL session public surface 仍应按
       `observed_reuse=false` / `session_configured=true`
       理解
     - profiling / benchmark 当前应该回到：
       - `scripts/run_phase2_performance_baseline.sh`
       - `tests/benchmarks/run_all_benchmarks.sh`
       - `docs/test_reports/PHASE2_PERFORMANCE_METRICS_TEMPLATE.md`
   - 当前最小正确修法已经压实为：
     - 在握手 profiling 样例后明确：
       - direct `CreateConnection(...)` 是为了 profiling caller-owned socket /
         握手计时边界
       - 普通跨后端 HTTPS 客户端仍优先
         `TSSLContextBuilder` + `TSSLConnector` + `TSSLStream`
     - 在 session 小节明确：
       - WinSSL session public surface 仍只按实验性 public truth 理解
       - 不要把示例直接读成已稳定命中的通用性能收益
     - 在目标表前明确：
       - 固定数字最多只是量级参考
       - 最新 baseline 应回到基准脚本与指标模板
   - 这批也顺手证明：
     - `test_active_owner_path_docs_alignment_contract.sh`
     - `test_secondary_guides_connection_level_sni_api_drift_contract.sh`
     - `test_winssl_session_resumption_docs_truth_contract.sh`
     在新增 profiling/runtime truth 说明之后仍保持绿色
   - 现在新的稳定基线应记为：
     - `PERFORMANCE_PROFILING_GUIDE` 不会再把固定性能 snapshot 和 WinSSL session public surface 误读成 current truth

115. 根 `README.md` 也存在一类更高优先级的首页 truth 漂移：
   - 问题不是 landing direct-path 分层出错
   - 而是首页仍在用固定性能/会话收益直接改写第一印象：
     - `能力矩阵缓存，10,000x+ 性能提升（>10M ops/s）`
     - `会话复用: 70-90% 握手性能提升`
   - 这与当前更 durable 的 truth 冲突：
     - 性能相关结论应回到 fresh benchmark / baseline 入口
     - `会话复用 / Session Ticket` 应按 backend-specific truth 理解
     - 尤其 WinSSL 当前仍要按 experimental public surface 理解
   - 当前最小正确修法已经压实为：
     - 首页性能 bullet 改成：
       - 具体收益请以 `PERFORMANCE_GUIDE` 与 benchmark/baseline 入口为准
     - 首页 session bullet 改成：
       - `Session Ticket` / `会话复用` 属于 backend-specific truth
       - WinSSL 当前不再被首页写成固定握手收益
   - 这批也顺手证明：
     - `test_landing_quickstarts_direct_path_classification_contract.sh`
     - `test_performance_guides_benchmark_truth_contract.sh`
     - `test_winssl_session_resumption_docs_truth_contract.sh`
     在新增 README truth 收口之后仍保持绿色
   - 现在新的稳定基线应记为：
     - 根 `README.md` 不会再把固定性能快照和固定 session 收益误读成 current truth

116. `TROUBLESHOOTING.md` 里还残留了一个更细但仍高可见的 WinSSL session 排障残口：
   - 问题不是 `ISSLSessionResumption` owner path 不该存在
   - 而是排障页仍把这段示例教成了默认成功路径：
     - `1. **启用 Session 复用**`
     - `// 后续连接 - 快速复用`
     - `LConn2.Connect;  // 快速握手`
   - 这与当前更 durable 的 WinSSL truth 冲突：
     - `observed_reuse=false`
     - `session_configured=true`
     - session public surface 仍只应按实验性 owner surface 理解
   - 当前最小正确修法已经压实为：
     - 在问题段开头明确：
       - 这里保留 direct `CreateConnection(...)` +
         `ISSLSessionResumption`
         是因为排障时要直接观察连接对象上的 session owner surface
       - 普通跨后端 HTTPS 客户端仍优先
         `TSSLContextBuilder` + `TSSLConnector` + `TSSLStream`
     - 在示例前明确：
       - 当前 dedicated Windows runtime truth 仍按
         `observed_reuse=false` / `session_configured=true`
         理解
       - 没有 dedicated Windows / target-specific validation 时，
         不要把 `SetSession(...)` + `Connect`
         直接读成已稳定命中的 resumed-handshake
     - 小节名与注释不再使用：
       - `启用 Session 复用`
       - `快速复用`
       - `快速握手`
   - 这批也顺手证明：
     - `test_session_resumption_guide_old_name_truth_contract.sh`
     - `test_diagnostics_connection_override_classification_contract.sh`
     - `test_winssl_session_resumption_docs_truth_contract.sh`
     在新增排障 truth 说明之后仍保持绿色
   - 现在新的稳定基线应记为：
     - `TROUBLESHOOTING.md` 不会再把实验性 WinSSL session owner surface
       误读成默认已命中的复用收益

117. 这轮 focused 回归还暴露了一个旧合同/文档基线重新脱节的点：
   - `test_migration_troubleshooting_connection_level_sni_omissions_contract.sh`
     仍要求 `MIGRATION_GUIDE.md` 的低层 `ISSLConnection`
     迁移示例显式展示连接级 SNI
   - 该页一度漂到了 `Supports(..., LClientConn)` 写法，
     导致旧合同按原始显式 `SetServerName(...)` 单行基线失败
   - 当前最小正确修法不是删合同，
     而是把低层迁移示例收回原始“显式展示连接级 SNI”基线：
     - `(LConn as ISSLClientConnection).SetServerName('example.com');`
   - 现在新的稳定基线应记为：
     - `MIGRATION_GUIDE.md` 与
       `test_migration_troubleshooting_connection_level_sni_omissions_contract.sh`
       再次对齐，不会在这类回归里反复误报

118. `backend feature capability parity` 这批证明，当前更值钱的 residual
   已经不是 capability producer 再次写错，而是 runtime consumer 是否仍偷跑第二套语义：
   - `GetCapabilities` / serializer / selector / diff
     这几层之前都已经各自收紧
   - 但如果没有直接验证
     `ISSLLibrary.IsFeatureSupported(...)`
     和 capability record 的 published truth，
     调用方仍可能在 runtime probe 与 capability 发布面之间读到两套结论
   - 新 focused contract 现在已经把这条线钉住：
     - `sslFeatSNI`
     - `sslFeatALPN`
     - `sslFeatSessionCache`
     - `sslFeatSessionTickets`
     - `sslFeatRenegotiation`
     - `sslFeatOCSPStapling`
     - `sslFeatCertificateTransparency`
     都必须满足：
     - `IsFeatureSupported(feature) =
        (对应 *Support <> sslSupportNone)`
   - 这次本机结果也给了一个很清楚的当前事实：
     - `OpenSSL`
     - `WolfSSL`
     - `MbedTLS`
     - `FreePascal Native`
     全部通过
     - `Windows Schannel`
       在当前非 Windows 环境只按
       `[SKIP] not available`
       处理，不把平台不可运行误判成 capability drift
   - 所以这批的正确结论不是“又发现 backend 实现缺口”，而是：
     - 当前 7 条 public feature 的 runtime consumer parity
       已经具备 durable proof
     - capability dual-truth 这条线以后不必再为
       `IsFeatureSupported(...)` vs `GetCapabilities`
       反复从头拉起

119. `RequireSystemCertStore` 这批再次说明，
   selector / builder 的 runtime-aware requirement 审查里，
   最容易反复误判的不是 source truth，
   而是 proof 自己有没有把额外筛选条件混进去：
   - 当前更值钱的真实残口并不是
     `SupportsSystemCertStore` producer 又写错了
   - 而是此前还没有一条 focused downstream contract，
     直接证明：
     - `SelectBestBackend(...)`
     - `TSSLContextBuilder.WithAutoBackendSelection(...).TryBuildClient(...)`
       的结果
       确实跟随当前 published capability truth
   - 这次第一版 RED 的根因也很典型：
     - 合同起点用了 `CreateDefaultRequirements(optBalanced)`
     - 但没有把：
       - `MinSecurityScore`
       - `MinPerformanceScore`
       - `MinCompatibilityLevel`
         清零
     - 于是 `RequireSystemCertStore` 的 proof
       被默认 balanced 评分阈值污染，
       误看成 selector / builder 行为异常
   - 把三项最低分数门槛显式清零后，
     合同才真正回到它该验证的单一问题：
     - 是否存在任一已注册且可用 backend
       发布 `SupportsSystemCertStore=True`
     - 若存在，则 selector / builder 必须成功，
       且选中的 backend 也必须发布该 capability
     - 若不存在，则 selector / builder 都必须失败，
       且 builder 要给出
       `No suitable SSL backend found for requirements`
   - 所以这批应沉淀成一个更稳的路线规则：
     - 对 selector / builder 的 requirement proof，
       必须先隔离掉默认 score threshold / preference 噪音
     - 不然很容易把“多条件筛选结果”误判成
       “单条 capability truth 漂移”
   - 当前最重要的 durable 结论是：
     - `RequireSystemCertStore` 的 downstream proof gap
       已经闭环
     - 当前 selector / builder 行为与
       `SupportsSystemCertStore` published truth 对齐，
       不必再为这条线反复从头拉起

120. `PreferHardwareAccel` 这批进一步把 selector / builder 的
   downstream proof 路线钉得更细了一层：
   - 对 `prefer` 类语义，
     最危险的误判不是“source truth 错了”，
     也不是“没命中偏好就是 bug”，
     而是：
     - capability record 里继续发布了
       `HasHardwareAcceleration`
     - 但 selector score / 排序 / builder 下游
       未必真的消费了它
   - 这次更稳的 proof 方式不是猜
     “偏好开启后一定会选哪个 backend”，
     因为 `prefer` 的语义本来就只是加权，
     不是硬性 requirement
   - 真正 durable 的检查点应该是三层：
     - baseline 与 preferred requirements
       的 qualifying backend 集合保持一致
     - `HasHardwareAcceleration=True` 的 backend
       score 必须按当前公式获得固定加分
     - `HasHardwareAcceleration=False` 的 backend
       score 必须保持不变
   - 在此基础上，再验证：
     - `SelectBestBackend(...)`
       是否真的返回 preferred 排序后的第一名
     - builder 下游是否沿用同一个 selected backend
   - 这条路线比“断言某个 backend 必须永远获胜”更稳，
     因为它直接钉住了 preference truth
     真正进入 score / selection / builder 的事实
   - 所以这批的正确结论仍然不是
     “又发现 backend 实现缺口”，而是：
     - `PreferHardwareAccel` 的 downstream proof gap
       已经闭环
     - 当前 `HasHardwareAcceleration` published truth
       已经被 selector / builder 真实消费
   - 当前 selector / builder 主线的 focused proof
     现在已经覆盖：
     - `RequirePKCS11Support`
     - `RequireTPM`
     - `RequireSystemCertStore`
     - `PreferHardwareAccel`
   - 同类下一条最直接的残口
     应该转向：
     - `PreferOSNative`

121. `PreferOSNative` 这批又把一类很容易重复误判的情况收紧了：
   - 当本机 runtime 缺少真实正例时，
     最危险的做法不是“先不验证”，
     也不是“只做 negative-only proof”，
     而是误把“环境里没有 live backend”
     当成“selection truth 已经被证明”
   - 对 `PreferOSNative` 来说，
     当前 Linux 环境正好就是这种情况：
     - `WinSSL` 不可用
     - 真实可用 backend 里没有 live `sslImplOSNative`
   - 这意味着更稳的 proof 方式
     不能再依赖 live runtime 自己给出正例，
     而要切到 controlled mock runtime：
     - baseline 时让 non-OS-native backend 略微领先
     - 开启 `PreferOSNative` 后，
       让 OS-native backend 按当前公式获得固定加分并反超
   - 这样验证到的就不再是
     “某个平台刚好怎么选”，
     而是 selector / builder
     是否真实消费
     `BackendImplType = sslImplOSNative`
     这条 published truth
   - 当前更 durable 的结论是：
     - `PreferOSNative` 的 downstream proof gap
       已经闭环
     - “Linux 上没有真实 OS-native backend”
       不再是后续重复拉起这条线的理由
   - 到这里为止，
     selector / builder 这组 platform preference / requirement
     focused proof 已经形成一条更完整的基线：
     - `RequirePKCS11Support`
     - `RequireTPM`
     - `RequireSystemCertStore`
     - `PreferHardwareAccel`
     - `PreferOSNative`
   - 所以下一步不该继续在这组 proof 上来回打转，
     而应切回更大的 public-surface / backend completeness 主线，
     优先处理：
     - `ISSLServerConnection` 文档/源码不一致
     - `ISSLConnection` 核心接口过宽
     - `TSSLConfig` 跨层职责混杂
122. `MbedTLS` 的“异步操作”当前真正发布的能力边界已经可以更精确地表述为：
  - 现有 public surface 确实支持
    `WantRead / WantWrite`
    这类非阻塞重试语义
  - 但这不等于当前已经发布了 dedicated async callback / job / event-loop capability
  - 因而专页只写
    `非阻塞 I/O`
    会把“可重试状态语义”和“正式 async capability”
    混成一个过宽结论
- 这批 focused contract 的价值不在于新增实现，而在于把三层 truth 锁到一起：
  - `src/fafafa.ssl.base.pas`
    的 active connection surface
  - `src/fafafa.ssl.mbedtls.connection.pas`
    的 native WANT_READ / WANT_WRITE 映射
  - `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
    的 capability wording
- 当前最重要的流程结论同样已经改变：
  - GitHub Actions `WinSSL Runtime Gate`
    run `26130501368`
    已 `success`
  - 这说明 Windows / WinSSL runtime 验证
    现在已经有自动主线承接
  - 所以下一步不该再回到
    “WinSSL 只能静态审查”
    这种旧前提，而应继续清理剩余的 source-backed capability drift
- 因而当前批收口后的默认顺序应为：
  - 先继续同类 residual：
    `MbedTLS Ed25519`
  - 再处理：
    `WinSSL Windows 7 SP1`
    平台支持表述
123. `MbedTLS` 专页把 `Ed25519` 写成
  `⚠️ 部分 | MbedTLS 3.x`
  当前并不是一个 source-backed 的 fafafa.ssl published truth：
  - `src/fafafa.ssl.mbedtls.lib.pas`
    的 capability record
    没有任何 `Ed25519`-specific published capability
  - `src/fafafa.ssl.mbedtls.certificate.pas`
    当前 `GetPublicKeyAlgorithm`
    仍返回：
    - `RSA`
  - 同文件里的 `GetSignatureAlgorithm`
    当前仍返回：
    - `SHA256withRSA`
- 这意味着当前更准确的结论不是
  `MbedTLS 3.x 理论上也许支持 Ed25519`
  这种上游投影，
  而是：
  - fafafa.ssl 当前 MbedTLS backend
    没有发布可证明的 `Ed25519`-specific capability / metadata surface
  - 因而活跃专页应记为：
    - `❌ 当前 capability 不发布`
- 这批也让总路线图的阶段判断更清楚了：
  - GitHub Actions `WinSSL Runtime Gate`
    run `26130501368`
    已 `success`
  - GitHub Actions `CI`
    run `26130974672`
    已 `success`
  - 所以当前 repo 已不再被
    “Windows 没法 runtime 验证”
    或
    “主 CI 还不稳”
    这类问题卡住
  - 更高价值的剩余工作
    继续集中在：
    - residual capability / active-doc drift
    - interface-design / backend completeness 主线
- 因而当前批收口后的默认顺序应为：
  - 先继续 residual：
    `WinSSL Windows 7 SP1`
    平台支持表述
  - 再切回：
    `ISSLConnection`
    / `TSSLConfig`
    / `ISSLServerConnection`
    这组更大的 completeness 问题
124. `WinSSL` 平台支持这条 residual 当前已经可以更明确地定性为：
  - 不是实现缺口
  - 而是多份活跃文档里仍残留旧的 `20348+` / `需更新` / `部分支持` 口径
- `src/fafafa.ssl.winssl.lib.pas`
  当前 source truth 已经很清楚：
  - `Initialize`: `Vista+`
  - `TLS 1.1 / 1.2`: `Windows 7+`
  - `TLS 1.3`: `Windows 10 Build 18362+`
- 因而：
  - `Windows 7 SP1`
    不应再写成：
    - `⚠️ 部分`
    - `需更新`
  - `Windows Server 2019`
    也不应再写成：
    - `TLS 1.3 = ⚠️`
  - `Windows 10 TLS 1.3`
    也不应继续按：
    - `20348+`
    讲解
- 这批收口后的路线图判断也更稳定了：
  - GitHub Actions `CI`
    run `26131189318`
    已 `success`
  - GitHub Actions `WinSSL Runtime Gate`
    run `26130501368`
    已 `success`
  - 所以当前 repo 不再被：
    - Linux 主线 CI
    - WinSSL Windows runtime automation
    - WinSSL 平台支持表述
    这几条 control-plane / doc-truth 基线卡住
- 因而当前默认下一步应正式切回：
  - `ISSLConnection`
  - `TSSLConfig`
  - `ISSLServerConnection`
  这组更大的 interface-design / backend completeness 主线
125. 当前 `WinSSL CertStore DN Query Contract` 在 GitHub Windows runtime 上剩余的真实根因，不只是“DN 顺序可能不同”，而是更基础的 truth-source 错位：
  - `TWinSSLCertificateStore.FindBySubject`
    / `FindByIssuer`
    现在虽然已经做了
    normalization
    和 exact-first / substring fallback
  - 但它们比较的 candidate
    仍来自：
    - `TWinSSLCertificate.GetSubject`
    - `TWinSSLCertificate.GetIssuer`
  - 这两个 getter
    当前走的是
    `CERT_NAME_SIMPLE_DISPLAY_TYPE`
    更接近 simple display name，
    不是 full X.500 DN
  - 所以像
    `CN=Test Signer,O=Test Org`
    或
    `O=Test Org,CN=Test Signer`
    这类 component query
    在 WinSSL 上
    根本没有足够的 candidate truth
    可以匹配
- 这意味着当前更安全的最小修复
  不一定要立刻改 public getter surface，
  但至少应把 store-query lane
  的 truth source
  切回 native
  `CERT_CONTEXT^.pCertInfo^.Subject/Issuer`
  并通过
  `CertNameToStrW(..., CERT_X500_NAME_STR ...)`
  生成 full DN candidate
- 这样可以：
  - 修复当前 GitHub Windows runtime 红灯
  - 继续保留现有
    `GetSubject` / `GetIssuer`
    作为 display-oriented surface
    的兼容性
  - 同时让
    `FindBySubject`
    / `FindByIssuer`
    回到 shared certstore contract
126. `WinSSL` 这层 native 结构体还有一个容易踩坑的静态事实：
  - `CERT_CONTEXT.pCertInfo`
    在当前
    `fafafa.ssl.winssl.base.pas`
    里被声明成了裸 `Pointer`
  - 所以调用点如果直接写：
    - `LContext^.pCertInfo^.Issuer`
    - `LContext^.pCertInfo^.Subject`
    会在 Windows 编译期直接炸出
    `Illegal qualifier`
- 这不影响当前修复方向本身，
  但说明 WinSSL native bridge
  仍有一些旧声明
  没有完全类型化；
  调用点必须先显式转成
  `PCERT_INFO`
  再取
  `Issuer` / `Subject`
127. 这次 `WinSSL CertStore DN Query` 收口后的最终结论已经比较清楚：
  - 真正让 Windows runtime 红灯转绿的关键
    不是去改整个 public getter family，
    而是：
    - 在 certstore query lane
      切回 native full-DN truth
    - 再补上
      `PCERT_INFO` 的显式类型转换
  - 最终
    `CI`
    run `26140837184`
    与
    `WinSSL Runtime Gate`
    run `26140837156`
    都已通过
- 因而当前 `FindBySubject`
  / `FindByIssuer`
  的 WinSSL runtime contract
  可以视为已闭环；
  后续若还要继续沿这个方向深挖，
  应该把问题正式切换成：
  - `TWinSSLCertificate.GetSubject`
    / `GetIssuer`
    本身的 public 语义
    是否也要和其它 backend
    对齐为 full-DN truth
  - 而不是再回头重查
    certstore query
    为什么匹配不到

- `context-level ServerName`
  这条主线
  当前又暴露出一种
  很容易让项目停滞的
  “历史中间态误导”：
  - `src/fafafa.ssl.context.compat.pas`
    已经恒返回 `''`
  - OpenSSL / WolfSSL / MbedTLS / WinSSL
    constructor
    继续调用它
    也不再产生任何 inherited fallback 行为

- 这意味着：
  - shared helper
    继续存在
    已经不是兼容保护，
    而是考古噪音
  - 它会持续制造一种假象：
    仿佛 backend
    还保留了一条
    context-to-connection
    的兼容桥

- 所以这批的正确收口
  不是再改 warning
  或继续写
  “helper 还在但没效果”
  的说明，
  而是：
  - 直接删除 helper file
  - 同时删掉四个 backend constructor
    的 dead helper call
  - 把 focused contract
    改成守
    “helper 不存在”
    这条最终真相

- focused retest
  也已经证明
  这一步只是在收源码真相，
  不是在改变 runtime 边界：
  - `tests/test_cross_backend_client_context_server_name_clarification.pas`
    仍然通过
  - 这说明：
    - deprecated context state
      仍可在 context API 上观察到
    - 但新 client connection
      继续不继承这份 state

- 因而当前
  `context-level ServerName`
  主线里
  最值得继续审的
  已经不再是 backend seam，
  而是剩余 public compatibility surface：
  - `TSSLConfig.ServerName`
  - direct `ISSLContext.SetServerName/GetServerName`
  - `WithSNI(...)`

- 继续沿着这组三个
  public compatibility surface
  往下看后，
  当前又确认了一条
  很典型的
  active-doc drift：
  - `docs/guides/MIGRATION_GUIDE.md`
    虽然方向上是在劝用户
    不要再用
    context-level SNI
  - 但它仍把
    `TSSLConfig.ServerName`
    / `ISSLContext.SetServerName(...)`
    / `TSSLContextBuilder.WithSNI(...)`
    的 literal 名称
    写回了活跃指南层

- 这会带来两个问题：
  - 它和
    `API_REFERENCE`
    当前负责
    frozen compatibility surface
    唯一字面说明
    的规则冲突
  - 也会让后续调用方
    在 migration guide
    这种高频入口里
    再次把这些旧 surface
    当成“仍值得直接学习的名字”

- 这次 focused RED
  还顺手暴露了一个
  contract coverage gap：
  - `TSSLConfig.ServerName`
    与
    `WithSNI(...)`
    的 shell contract
    已经能抓
    literal-name drift
  - direct `ISSLContext.SetServerName/GetServerName`
    的 contract
    之前只拦
    调用示例 / 指导语义
    没有同向限制
    literal 名称

- 所以这批最小正确修法
  不是扩大文档整改范围，
  而是：
  - 把
    `MIGRATION_GUIDE`
    改成 generic wording
  - 同时把
    direct context
    的 surface-truth contract
    补成和另外两条一样的
    literal-name guard

- 紧接着继续回跑
  `MIGRATION_GUIDE`
  自己的 active-truth contract
  后，又确认了一条
  更细但同样真实的
  workflow mismatch：
  - 上一轮把 guide
    从 literal-name 列表
    收成了 generic wording
  - 但
    `test_migration_guide_active_truth_contract.sh`
    仍要求旧的
    literal-name 行

- 这说明
  `MIGRATION_GUIDE`
  当前不只是“文案要收紧”，
  还要把
  raw `ISSLConnection`
  客户端示例
  的新真相
  一起写实：
  - 如果 guide 继续保留
    direct connection
    场景
  - 它就应该明确展示：
    - `Supports(LConn, ISSLClientConnection, LClientConn)`
    - `LClientConn.SetServerName(...)`

- 否则会出现一种很别扭的中间态：
  - guide 顶部已经不再点名
    frozen context-level SNI surface
  - 但 raw connection 示例
    仍没有显式展示
    当前 shipped 的
    client-role access path

- 所以这批最小正确修法是：
  - guide 示例改成
    `Supports(..., ISSLClientConnection, ...)`
  - contract 也同步改成：
    - 要求 generic compatibility wording
    - 禁止旧 literal-name 列表回流
    - 继续要求 raw connection
      场景显式走
      per-connection SNI

108. `WinSSL certificate.VerifyEx flag parity` 这批现在应作为证书级 `VerifyEx` published-flag 真空点的最新收口批次保留：
   - 新 plan：
     - `docs/plans/2026-05-20-winssl-certificate-verifyex-flag-parity.md`
   - 当前 source truth：
     - `src/fafafa.ssl.winssl.certificate.pas`
       之前：
       - 只把 revocation / CRL / OCSP
         映射到
         `CertGetCertificateChain`
         flags
       - 证书级 policy
         仍固定走
         `CERT_CHAIN_POLICY_BASE`
         且
         `dwFlags = 0`
       - 没有 cert-level
         `IgnoreExpiry`
         /
         `AllowSelfSigned`
         /
         `StrictChain`
         兑现逻辑
     - `src/fafafa.ssl.winssl.connection.pas`
       同时又已经在连接层兑现了：
       - `CERT_CHAIN_POLICY_IGNORE_NOT_TIME_VALID_FLAG`
       - `CERT_CHAIN_POLICY_ALLOW_UNKNOWN_CA_FLAG`
       - hostname policy
     - 这说明：
       - 连接层成功
         不能当作
         `ISSLCertificate.VerifyEx`
         已完成的证据
   - 当前 workflow truth：
     - `tests/winssl/test_winssl_cert_verify_ex.pas`
       之前只是常量/结构烟雾测试，
       没有真实运行时夹具
     - `tests/run_winssl_tests.ps1`
       之前根本没执行这个测试
     - `tests/winssl/test_winssl_cert_verify_ex.lpi`
       还错误固定了
       `TargetOS=linux`
       如果直接接进 Windows suite，
       会先把工程配置问题当成“代码失败”
   - 当前最小修正：
     - 把
       `test_winssl_cert_verify_ex.pas`
       升级成真实 runtime contract，
       覆盖：
       - `expired-signer.pem`
         + `ca_cert.pem`
         的
         `IgnoreExpiry`
         per-call 语义
       - `version1-cert.pem`
         + empty memory store
         的
         `AllowSelfSigned`
         per-call 语义
       - `signer_cert.pem`
         + `ca_cert.pem`
         的
         `StrictChain`
         fail-closed
     - 把
       `test_winssl_cert_verify_ex.lpi`
       收回到和现有 active WinSSL 项目文件一致的目标配置
     - 把这个 focused test
       接入
       `tests/run_winssl_tests.ps1`
     - 在
       `src/fafafa.ssl.winssl.certificate.pas`
       上补齐：
       - `sslCertVerifyIgnoreExpiry`
         -> `CERT_CHAIN_POLICY_IGNORE_NOT_TIME_VALID_FLAG`
       - `sslCertVerifyAllowSelfSigned`
         -> self-signed leaf
            时才加
            `CERT_CHAIN_POLICY_ALLOW_UNKNOWN_CA_FLAG`
       - `sslCertVerifyStrictChain`
         -> leaf 缺少
            `serverAuth`
            / `1.3.6.1.5.5.7.3.1`
            时明确 fail-closed
   - 当前 focused proof：
     - `git diff --check`
       - PASS
     - `xmllint --noout tests/winssl/test_winssl_cert_verify_ex.lpi`
       - PASS
     - 本地 Linux 环境没有
       `pwsh`
       与 Windows runtime，
       所以真正的编译/运行证明应由
       push 后的
       `WinSSL Runtime Gate`
       承接
   - 首轮 Windows CI 反馈补充了一条必须记住的 backend truth：
     - `CERT_CHAIN_POLICY_BASE`
       下，
       把 `ca_cert.pem`
       放进 memory-backed additional store
       并不会让它在 WinSSL cert-level `VerifyEx`
       上自动变成 trusted root
     - 所以
       `expired-signer.pem`
       这类 CA-signed expired fixture
       会先暴露
       `CERT_E_UNTRUSTEDROOT`
       把 expiry 错误遮住
     - 这意味着：
       - WinSSL 的 expiry contract
         不能直接照搬
         OpenSSL / FreePascal
         的那组
         `additional store`
         fixture 设计
   - 第二轮 Windows CI 反馈又补充了一条不同层面的 backend truth：
     - 运行时生成的
       expired self-signed leaf
       在当前 WinSSL cert-level `VerifyEx`
       上，
       一进入
       `sslCertVerifyAllowSelfSigned`
       分支
       就打出
       `EAccessViolation`
     - 所以当前批次里，
       `IgnoreExpiry`
       的稳定 runtime contract
       不应继续依赖
       generated self-signed
       路径
     - 当前更稳的做法是：
       - 临时把
         `ca_cert.pem`
         加入
         `CurrentUser\ROOT`
       - 用
         `expired-signer.pem`
         做 expiry-only fixture
       - 再验证
         `sslCertVerifyIgnoreExpiry`
         是否真正改变 cert-level 结果
   - 当前批收口后的默认下一步：
     - 观察
       `WinSSL Runtime Gate`
       是否一次性把 cert-level `VerifyEx`
       这组三个 published flags 收口
     - 若绿色，
       默认切回更大的 public completeness 主线：
       - `ISSLConnection`
       - `TSSLConfig`
       - `ISSLServerConnection`

- capability dual-truth
  这条线当前真正剩下的
  不是
  backend producer
  /
  serializer
  /
  diff
  实现层，
  而是
  public entry narration

- 具体来说：
  - `NormalizeLegacyCapabilityBooleans(...)`
  - support-level-first serializer precedence
  - support-level-first diff
  - backend `GetCapabilities`
    source normalization
  这些主链已经基本把 runtime/source truth 收住

- 当前更容易继续误导开发路线的，
  是：
  - `TSSLBackendCapabilities`
    record
    本身没把 paired feature truth model 直接写在声明处
  - 活跃入口文档里还残留：
    - `SupportsALPN=True` / `SupportsSNI=True`
    - `SupportsOCSPStapling=False`
    - `SupportsCertificateTransparency=False`
    - `Caps.SupportsALPN`
    这类 legacy-bool-first 读法

- 这意味着下一批最小正确修法
  不是再去动 backend 实现，
  而是：
  - 让
    `TSSLBackendCapabilities`
    record
    自身就声明：
    - paired feature
      以
      `*Support`
      为 source/runtime truth
    - legacy
      `Supports*`
      只是 compatibility projection
    - `SupportsTLS13`
      仍是当前唯一明确保留的主 bool truth
  - 同时把活跃矩阵 / 迁移指南
    的入口示例一起切回 support-level-first

- 这样收口后，
  capability
  这条线就不会再依赖
  “读过前几批计划/报告的人才知道该信哪套字段”
  才能保持正确方向

- 当前又确认了一条
  不属于 docs-only 的真实 public surface gap：
  主门面
  `src/fafafa.ssl.pas`
  虽然写着
  “导出所有公共接口和类型”，
  但 capability / native-handle 这组 public surface
  实际并没有完整闭合到
  `uses fafafa.ssl`

- 最直接的证据就是：
  一个只
  `uses fafafa.ssl`
  的最小 compile probe
  在修复前会直接报：
  - `TSSLBackendCapabilities`
  - `TSSLBackendImplType`
  - `TSSLFeatureSupportLevel`
  - `ISSLNativeHandleAccess`
  - `GetCapabilitiesDescription`
  - `IsFeatureStable`
  - `sslCipherAES256GCM`
  - `sslHashSHA256`
  - `sslKexECDHE_RSA`
  这类 identifier not found

- 这说明当前缺口不是
  capability helper
  本身没实现，
  而是：
  - 主门面没有把 capability 相关类型、
    enum values、
    helper functions
    一起穿透出来
  - 所以 capability helper surface
    在主门面上仍是半开状态

- 因而这批最小正确修法
  不是新增 backend 行为，
  而是补齐 façade export closure：
  - type / interface re-export
  - enum value const re-export
  - helper forwarding
  - compile-based contract

- 这类修复的价值很高，
  因为它直接把
  “主门面自称完整”
  变成了可编译验证的事实，
  而不是继续让调用方在 capability/native-handle 这组基础 public surface 上被迫 split
  `fafafa.ssl.base`

- 当前又确认了一条新的
  facade-only compile gap：
  主门面
  `src/fafafa.ssl.pas`
  仍未 re-export：
  - `TSSLStringArray`
  - `TSSLCertVerifyResult`

- 这两种类型都已经是当前 shipped certificate public surface
  的直接组成部分：
  - `ISSLCertificate.GetSubjectAltNames` /
    `GetKeyUsage` /
    `GetExtendedKeyUsage`
    使用
    `TSSLStringArray`
  - `ISSLCertificate.VerifyEx(...)`
    使用
    `TSSLCertVerifyResult`

- focused facade compile proof
  首轮就直接失败在：
  - `main facade must re-export TSSLStringArray`
  这证明问题不在 backend runtime，
  也不在文档解释，
  而在主门面 supporting-type export
  仍未闭合

- 这批最小正确修法
  不是新加 helper 或改证书行为，
  而是：
  - 在主门面补齐 supporting-type alias
  - 在 canonical API reference
    补一条主门面 supporting-type 覆盖说明
  - 用 facade-only compile contract
    把这层 truth 锁住

- 当前又确认了一条真实的
  backend semantic residual：
  `MbedTLS` /
  `WolfSSL`
  的 `VerifyHostname(...)`
  会在 SAN 不匹配时错误回退到 CN，
  同时还会把
  `*.example.com`
  这类 wildcard SAN
  先当成“非法 hostname”
  直接跳过

- 这不是抽象推断，
  而是新的 focused fixture proof
  首轮就同时打出两条 RED：
  - `SAN-vs-CN fixture prioritizes SAN over CN`
  - `Wildcard SAN fixture matches single-label subdomain`

- `WinSSL`
  的 `VerifyHostname(...)`
  也复用了相同的 SAN scan / CN fallback 模式，
  所以虽然本地没有 Windows runtime，
  这条实现风险同样应该同步收紧，
  否则 push 后很可能只会在
  `WinSSL Runtime Gate`
  再现同样问题

- 这批最小正确修法
  不是重构整个 hostname verifier，
  而是把 shared truth 收紧到
  `FreePascal` 已经证明正确的语义：
  - 只有当证书没有 relevant SAN
    时才允许回退到 CN
  - wildcard SAN
    应被视为合法 hostname pattern
    进入匹配

- `OpenSSL`
  这边没有暴露同类实现问题。
  新增 focused contract 后，
  它对：
  - `san-test.pem`
  - `san_cn_conflict_cert.pem`
  - `san_wildcard_cert.pem`
  三组夹具都保持绿色，
  说明当前 native helper-based 行为
  与期望 truth 一致

- 还顺手抓到一个
  与本批目标相邻、
  但不应扩 scope 的旧问题：
  `tests/certificate/test_certificate_unit.pas`
  当前本身还是陈旧坏档，
  仍按 `TStringList`
  读取已经迁移成
  `TSSLStringArray`
  的返回值。
  这次没有继续把范围拖进整份 legacy test，
  而是改成新增一个新的
  OpenSSL focused contract
  来承接本批 proof

- `tests/winssl/test_winssl_certificate_san.pas`
  在接入前也有两个静态问题：
  - 夹具路径依赖仓库根工作目录
  - `.lpi`
    仍硬编码
    `TargetOS=linux`
  这两处已经一起修正，
  所以现在这份测试终于可以被
  `tests/run_winssl_tests.ps1`
  真正调度

- `WinSSL Runtime Gate`
  `26172089572`
  现在已经全绿，
  这意味着上一批
  `VerifyHostname`
  parity
  的 Windows runtime handoff
  已经真正闭环，
  不需要再把
  `WinSSL`
  当成“只有静态修复、没有运行时证据”的 lane

- 当前又确认了一条
  public API truth sync residual：
  `ISSLCertificate`
  这组证书扩展 getter
  虽然源码早就统一成
  `TSSLStringArray`，
  但活跃面仍有：
  - `docs/guides/TROUBLESHOOTING.md`
  - `tests/certificate/test_certificate_unit.pas`
  在继续按旧
  `TStringList`
  心智教学 / 编写

- 这不是“旧文档里有点不优雅”，
  因为代表性测试文件
  `test_certificate_unit.pas`
  当前就会直接编译失败，
  报：
  - `got "TSSLStringArray" expected "TStringList"`
  这证明 residual
  已经落到了活跃测试面，
  不是 archive-only 噪音

- 这批最小正确修法
  不是重构全部证书辅助工具，
  而是严格收在
  public API 真相同步：
  - guide
    改回 array iteration
  - representative test
    改回
    `TSSLStringArray`
    + `ArrayContains(...)`
  - 再加一个 focused script contract
    禁止这条活跃 surface
    回退到 `Count/Free/IndexOf`

- 顺手又抓出同文件里的另一条旧测试假设：
  空证书日期不该被强行要求
  `NotAfter > NotBefore`
  当前更符合仓库整体语义的 truth
  是：
  - 未知日期 `(0,0)` 允许存在
  - 只有已知日期时才要求顺序正确

- 这样收口后，
  `ISSLCertificate`
  这组扩展 getter
  在：
  - source truth
  - active troubleshooting guide
  - representative OpenSSL cert test
  三层终于重新对齐

- 继续沿
  `ISSLCertificateStore`
  public truth
  往下看时，
  又抓到一条更典型的
  active-doc drift：
  `docs/guides/TROUBLESHOOTING.md`
  里还在把
  `LStore.Open(SSL_STORE_ROOT);`
  写进一个
  `ISSLCertificateStore`
  变量示例

- 这条 drift
  的问题不在
  “WinSSL helper 不该存在”，
  而在于它把两层 surface
  混写成一层：
  - generic public store flow
  - WinSSL concrete-only helper flow
  当前 shipped public interface
  只有：
  - `LoadFromFile`
  - `LoadFromPath`
  - `LoadSystemStore`
  - `AddCertificate`
  - `FindBy...`
  并没有
  `Open(...)`

- 因此这批最小正确修法
  不是去补 public `Open(...)`
  或扩 runtime，
  而是把活跃 troubleshooting
  重新写回：
  - `LoadSystemStore`
  - `AddCertificate`
  的 public flow
  同时明确说明：
  - 这只是给当前进程里注入的
    验证 store
    增加 CA
  - 若要持久写入 Windows
    系统证书存储，
    继续使用
    `certutil`
    或 WinSSL
    专用 helper

- 这类修法的价值在于：
  它继续把仓库从
  “接口签名表面完整”
  推进到
  “活跃排障文档也不再误教
  concrete-only 能力
  是 public contract”

- `optional backends certificate stream/memory truth`
  这一批真正新增的实现缺口
  不只是
  `LoadFromMemory`
  / `LoadFromStream`
  对 valid PEM
  过窄，
  还包括
  `WolfSSL`
  在 malformed PEM
  边界上没有 fail-closed

- 具体 root cause 是：
  `TWolfSSLCertificate.LoadFromMemory(...)`
  走 content-aware dispatch 后，
  带
  PEM begin/end marker
  但 base64 无效的输入
  会进入
  `LoadFromPEM(...)`
  再调用
  `TSSLUtils.PEMToDER(...)`
  而这里会抛
  `EBase64Error`
  不是返回
  `False`

- 这条 public truth
  的正确边界应是：
  - valid PEM memory / stream
    被接受
  - malformed PEM
    仍被拒绝
  - 拒绝方式必须是
    `False`
    +
    空状态，
    不能把 parser 异常暴露给调用方

- 当前最小正确修法是：
  - `WolfSSL LoadFromPEM`
    对
    `PEMToDER(...)`
    做
    `try/except`
  - `WolfSSL LoadFromFile`
    在读入真实文件后先
    `ResetLoadedState`
    再做 PEM preparse，
    并对同一条
    `PEMToDER(...)`
    路径做 fail-closed

- 收口后，
  optional backends
  的 certificate stream/memory surface
  已重新对齐到当前仓库其他 backend 的 content-aware truth：
  - valid PEM memory
    通过
  - `SaveToStream -> LoadFromStream`
    roundtrip
    通过
  - malformed PEM memory
    被稳定拒绝，
    且不再残留旧 cert state

- 顺着
  `ISSLCertificateStore`
  public-surface truth
  继续往下扫时，
  又抓到两处活跃
  `WinSSL`
  文档示例仍会直接误导调用方写出编译级错误：
  - `docs/guides/WINSSL_BEST_PRACTICES.md`
    还在对
    `ISSLCertificateStore`
    变量调用
    `Open(SSL_STORE_MY)`
  - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
    还在使用不存在的
    `TWinSSLCertStore`
    类名，
    并把：
    - `Store.Certificates`
    - `Cert.Subject`
    这类非 public surface
    写成可直接照抄的示例

- 这条 drift
  的问题不在
  “WinSSL 不该有 backend-specific helper”，
  而在于 active docs
  把三层东西混成了一层：
  - WinSSL helper
    `OpenSystemStore(...)`
  - concrete class
    `TWinSSLCertificateStore`
  - public
    `ISSLCertificateStore`
    /
    `ISSLCertificate`
    surface

- 当前源码真相是：
  - `OpenSystemStore(...)`
    helper
    返回
    `ISSLCertificateStore`
  - `ISSLCertificateStore`
    公开枚举路径是：
    - `GetCount`
    - `GetCertificate`
  - `ISSLCertificate`
    公开读主题路径是：
    - `GetSubject`
  - `Open` / `Close` / `IsOpen` / `GetAllCertificates`
    只属于
    `TWinSSLCertificateStore`
    concrete class

- 因而这批最小正确修法是：
  - `WINSSL_BEST_PRACTICES`
    改成
    `OpenSystemStore(SSL_STORE_MY)`
  - `WINSSL_BACKEND_CAPABILITY_MATRIX`
    改成：
    - `OpenSystemStore(SSL_STORE_MY)`
    - `GetCount`
    - `GetCertificate`
    - `GetSubject`
  - 同时新增 focused shell contract
    守住：
    - 错类名
    - 错接口
    - 错成员访问
    不再回流

## 2026-05-21

- 上一批 push 对应的
  GitHub Actions run
  `26176381529`
  已确认全绿：
  - `Code Quality (Light)`
  - `Minimal Gate (Linux)`
  - `FreePascal TLS 1.3 Completeness`

- 顺着
  `ISSLCertificate`
  /
  `ISSLCertificateStore`
  public-surface completeness
  继续往下扫时，
  抓到了一条真正落在 backend implementation
  而不是文档措辞上的 residual：
  - `TMbedTLSCertificateStore.FindByFingerprint`
    仍是 raw-string compare
  - `TWolfSSLCertificateStore.FindByFingerprint`
    也仍是 raw-string compare

- 这条残差之所以可信，
  是因为当前仓库其他 backend
  已经都兑现了 normalized query truth：
  - `OpenSSL`
  - `FreePascal`
  - `WinSSL`
  都支持
  去掉 `:`
  /
  `-`
  /
  空白并统一大小写

- 更关键的是，
  `MbedTLS`
  /
  `WolfSSL`
  自己内部其实早就有：
  - `NormalizeMbedTLSCertFingerprint(...)`
  - `NormalizeWolfCertFingerprint(...)`
  并已经用于：
  - `Contains`
  - `RemoveCertificate`
  - chain de-dup

- 所以当前不一致不是“能力做不到”，
  而是 `FindByFingerprint`
  这条 public query surface
  没有接上现成 truth

- focused RED 也非常干净：
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

- 因而这批最小正确修法
  不是重开 store cache/index 设计，
  而是直接把：
  - `TMbedTLSCertificateStore.FindByFingerprint`
  - `TWolfSSLCertificateStore.FindByFingerprint`
  收回到现有 normalize helper

- 修完后，
  两个 optional backend
  都已经重新对齐到：
  - lower-case query
  - 带 `:`
  - 带首尾空白
  仍能稳定命中同一张证书

- `Ed25519`
  这条线当前真正的残缺
  不在
  `MbedTLS`
  /
  `WolfSSL`
  getter 壳逻辑本身，
  而在它们共同依赖的
  `TX509Certificate`
  parser truth

- focused RED 证明：
  - `Algorithm.Name`
    仍暴露
    `1.3.101.112`
  - `KeyType`
    仍是
    `Unknown`
  - `KeySize`
    仍是
    `0`
  - `SignatureAlgorithm.Name`
    也仍暴露 OID

- 因而这批最小正确修法
  不是去补新的 backend-native API，
  而是直接在 shared parser
  里补：
  - OID name mapping
  - Edwards key type truth
  - Edwards key size truth

- 修完后，
  `Ed25519`
  证书的 public metadata truth
  已明确变成：
  - `Algorithm.Name = Ed25519`
  - `KeyType = Ed25519`
  - `KeySize = 256`
  - `SignatureAlgorithm.Name = Ed25519`

- `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
  里原先把
  `Ed25519`
  讲成“getter 仍返回 RSA 默认值”
  的说法
  已经不再符合当前源码真相；
  更准确的 active truth 是：
  - handshake capability
    仍未发布
  - 但 parser-backed certificate metadata
    已能暴露
    `Ed25519`
    算法名与
    `256-bit`
    公钥大小

- `Ed25519`
  在主 backend
  上的残差
  不是“所有算法 getter 都坏了”，
  而是两种不同类型的 implementation gap：
  - `OpenSSL`
    签名算法路径
    已经能给出
    可读
    `ED25519`
  - 真正缺的是
    `GetPublicKeyAlgorithm`
    仍只映射
    `RSA/DSA/DH/EC`
  - `WinSSL`
    则是
    public getter
    直接发布原始 OID，
    没把仓库已有的
    `OIDToName(...)`
    接进来

- 因而这批最小正确修法
  不是再去扩新的 native parser/binding，
  而是：
  - `OpenSSL`
    补
    `EVP_PKEY_ED25519`
    /
    `EVP_PKEY_ED448`
    到现有
    `EVP_PKEY_id(...)`
    case
  - `WinSSL`
    把
    `pszObjId`
    收口到
    `OIDToName(...)`
    并保留未知 OID fallback

- 这条 lane
  还补出一个 workflow 事实：
  只修 shared parser/docs
  时不会触发
  `WinSSL Runtime Gate`
  因为当前 workflow path filter
  只盯：
  - `src/fafafa.ssl.winssl*.pas`
  - `tests/winssl/**`
  等
  Windows 相关路径
  所以要拿到这条 runtime truth，
  需要把证明放进
  `tests/winssl/test_winssl_unit_comprehensive.pas`
  或改动
  `winssl`
  实现文件本身

- `GetPublicKey`
  在当前仓库里
  已经不是“尝试导出完整公钥”的 surface，
  而是一个已经冻结的最小 contract：
  - 非空
  - 与
    `GetPublicKeyAlgorithm`
    对齐

- 这条真相并不是只存在于历史文档：
  - `OpenSSL`
    当前源码直接返回
    `GetPublicKeyAlgorithm`
  - `FreePascal`
    也已经按同一语义收口
  - `MbedTLS`
    /
    `WolfSSL`
    之前的 public-surface completeness
    批次，
    也已经把
    `GetPublicKey = GetPublicKeyAlgorithm`
    当成 done 条件

- 因而
  `WinSSL.GetPublicKey`
  继续单独返回
  `SubjectPublicKeyInfo`
  PEM
  不是“实现更强”，
  而是主 backend public contract drift：
  - 同一接口
    在不同 backend
    发布了不同语义
  - 上层调用方无法把
    `ISSLCertificate.GetPublicKey`
    当作稳定 surface

- 这批最小正确修法
  不是补全仓库里尚未统一的
  “完整公钥导出能力”，
  而是先把
  `WinSSL`
  收回到当前 shared contract；
  真要做 PEM/DER 完整导出，
  应另开专题并跨 backend 统一设计

- 继续沿
  “活跃文档真相”
  主线扫描时，
  当前最值钱的新残留
  已不是实现缺口，
  而是两份高入口指南还在持续教学旧入口：
  - `docs/guides/FAQ.md`
    仍写：
    - `唯一要求：系统安装OpenSSL 1.1.1+或3.x。`
    - `TSSLLibrary.Instance.Initialize;`
    - `TSSLLibrary.Instance.SetCustomLibraryPath(...)`
  - `docs/guides/COMMON_PITFALLS.md`
    仍把
    `TSSLLibrary.Instance.SetCustomLibraryPath(...)`
    当成当前 macOS brew OpenSSL 方案

- 这类 drift
  的伤害并不亚于测试/实现真 bug：
  - 它会把新用户重新导回已过时的 loader 心智模型
  - 也会让后续静态审查误把
    `OpenSSL`
    当成仓库当前所有 backend 的全局唯一前提

- 当前更准确的公开真相应明确保留：
  - 普通新代码优先走：
    `fafafa.ssl`
    /
    `TSSLContextBuilder`
    /
    `TSSLConnector`
  - `OpenSSL`
    依赖与动态库路径修复，
    只属于相应 backend 的 runtime/fallback 话题
  - Windows 可以直接走
    `WinSSL`
  - `sslFreePascal`
    作为 pure Pascal backend，
    不应再被任何活跃 FAQ 写成“仍需系统 OpenSSL”

- 这次顺手也确认了一个容易被忽略的 FAQ 入口噪音：
  - 活跃 FAQ 里仍残留
    `yourusername`
    placeholder
    与错误文档链接
  - 如果不在这次一起修，
    后续入口审查还会反复被这些低级 drift 打断

- 在继续从高入口活跃文档往下扫时，
  `docs/CAPABILITY_MATRIX_GUIDE.md`
  又暴露出另一类很典型的 capability 文档漂移：
  - 它不是完全错误，
    但仍停留在
    `v1.2.0`
    视角
  - 所以会把“当前 capability 真相”与“能力矩阵最初引入时的历史语境”
    混在一起

- 这份 guide 当前最真实的问题主要有 5 类：
  - 版本头仍停在
    `v1.2.0`
  - quickstart capability 查询仍拆成
    `fafafa.ssl.base`
    /
    `fafafa.ssl.factory`
    而不是当前更高入口的
    `uses fafafa.ssl;`
  - backend compare / runtime init 示例仍用硬编码 backend 列表
  - 这会直接漏掉当前 shipped 的
    `sslFreePascal`
  - 支持入口仍残留
    `your-org`
    placeholder

- 这类 guide drift
  和 FAQ/COMMON_PITFALLS 的伤害不完全一样：
  - FAQ 更容易把新用户带回旧 bootstrap 路径
  - capability guide 则更容易把我们后续的 backend completeness 心智重新带偏，
    让人误以为：
    - shipped backend 集合仍只剩
      `OpenSSL / WolfSSL / MbedTLS / WinSSL`
    - `sslFreePascal`
      还不是当前 capability 讨论中的一等成员

- 当前更准确的 capability 文档真相应明确保留：
  - `TSSLFactory.GetLibraryInstance(...)`
    仍是当前 public library-entrypoint
  - 但普通 capability / helper 查询
    不必再拆回
    `uses fafafa.ssl.base`
    /
    `fafafa.ssl.factory`
  - backend 示例若想代表“当前 shipped 集合”，
    最稳的写法应改成
    `TSSLFactory.GetAvailableLibraries`
    驱动，
    而不是手写固定数组

- `CompatibilityLevel`
  这一栏也暴露出同类问题：
  - 源码当前已经发布：
    - `OpenSSL = 100`
    - `WinSSL = 90`
    - `WolfSSL = 85`
    - `MbedTLS = 75`
    - `FreePascal = 64`
  - 但活跃 guide 仍只列前四个，
    继续把
    `FreePascal`
    从 capability 讨论里静默漏掉

- `PLATFORM_SUPPORT.md`
  这轮又暴露了一条更直接的公开入口裂缝：
  - 它不是只在措辞上旧一点，
    而是仍在高入口平台文档里继续教学
    `CreateSSLLibrary()`
  - 但这条 helper
    已经不属于当前 shipped source public function

- 这类 drift
  比普通 broken link 更容易误导后续路线：
  - 因为读者会以为“平台文档里的工厂示例”就是当前权威 public entrypoint
  - 进而继续把不存在的 helper
    写回新文档 / 新示例 / 新测试

- 同一页还叠加了第二层 backend truth 漂移：
  - 平台表和各平台 backend 列表
    仍在静默漏掉
    `sslFreePascal`
  - auto-select 优先级表
    也只写到
    `OpenSSL`
    而没有把当前真实注册值里的
    `FreePascal=50`
    带上

- 这让平台文档会同时制造两种错觉：
  - 以为当前自动后端选择仍依赖旧 helper
  - 以为
    `sslFreePascal`
    还不是平台支持叙事中的正式 shipped backend

- macOS 区块还暴露了一条状态自相矛盾：
  - 顶部已经写
    `✅ 已发布`
  - 但已知问题区仍残留
    “平台验证正在进行中 / CI/CD 配置待完成”
  - 这会直接干扰我们对当前 release 路线是否已闭环的判断

- `ZERO_DEPENDENCY_DEPLOYMENT.md`
  这轮暴露的是另一种更危险的活跃文档漂移：
  - 它不是局部一个旧例子，
    而是整页从导入面到工厂入口再到 FAQ 代码块，
    都还停留在旧 helper 时代

- 这份文档当前最真实的问题有 4 组：
  - 旧 public helper 仍成片存在：
    - `CreateSSLLibrary(...)`
  - 旧单元导入仍成片存在：
    - `fafafa.ssl.abstract.types`
    - `fafafa.ssl.abstract.intf`
  - auto-detect 叙述仍按平台硬编码，
    与当前工厂
    highest-priority available backend
    真相不一致
  - 同页还有显式签名漂移：
    - `Lib.IsFeatureSupported('SNI')`
    - `Lib.IsFeatureSupported('ALPN')`

- 这类 drift
  的危害甚至比普通 doc wording 更大：
  - 因为调用方几乎会直接把这页的代码块 copy 进项目
  - 那么他们拿到的不是“略老一点的建议”，
    而是会立刻踩到：
    - 不存在的 public helper
    - 已移除的旧 unit
    - 错误的方法签名

- 同页 FAQ 的固定性能数字表
  也属于当前活跃文档真相的一部分：
  - 现在仓库已经明确把固定 `ms` / `MB/s` 表述视为不可靠长期 truth
  - 所以零依赖部署指南继续保留
    `~160 ms / ~150 ms`
    这种表，
    也会把后续路线判断带回过时 benchmark 心智

- 当前更准确的零依赖部署文档真相应明确保留：
  - WinSSL-specific path
    可以直接写
    `TSSLFactory.GetLibraryInstance(sslWinSSL)`
  - OpenSSL fallback
    则写
    `TSSLFactory.GetLibraryInstance(sslOpenSSL)`
  - auto-detect
    则必须解释成：
    工厂按当前注册优先级与可用性选择，
    而不是平台硬编码规则

- `DEPENDENCIES.md`
  这轮暴露出来的是“依赖真相写窄了”的另一类活跃文档问题：
  - 它不再像前几页那样整页都在教旧 helper，
    但它仍在用旧时代的依赖心智：
    - 好像 runtime 只分
      `WinSSL`
      /
      `OpenSSL`
    - 好像 FPC baseline 还是
      `3.3.1`

- 这类 drift
  的伤害和前几轮略不同：
  - 它不会立刻把用户导到不存在的 helper
  - 但会持续扭曲调用方对“当前后端依赖模型”的理解，
    让人忽略：
    `FreePascal`
    已经是 shipped 且无外部 SSL 动态库依赖的一等路径

- 当前更准确的依赖文档真相应明确保留：
  - `FPC 3.2.0+`
    是当前 shipped baseline，
    推荐
    `3.2.2+`
  - `WinSSL`
    是 Windows-native zero-dependency path
  - `FreePascal`
    在 Windows / Linux / macOS
    都属于“无外部 SSL 动态库”的正式 backend 路径
  - `OpenSSL`
    仍是最常见 / 功能更完整的依赖路径，
    但不该再被依赖文档写成各平台唯一 runtime 选择

- 同页 WinSSL 兼容表里的
  `Windows 10 (20348+)`
  也再次说明：
  - 版本阈值 drift
    不只出现在 WinSSL 专题文档里
  - 只要高入口依赖文档还留着旧数值，
    后续路线判断就会反复被错误平台阈值干扰

- `MIGRATION_GUIDE_V1.1.md`
  这轮暴露的是“历史迁移文档继续发布当前入口漂移”的问题：
  - 它表面上在讲
    v1.1
    的 native-handle 设计边界，
    但高入口示例已经把调用方带回：
    - `TSSLFactory.CreateLibrary(...)`
    - `Factory.CreateContext(...)`
    - `TSSLFactory.GetLibrary(...)`

- 这类 drift
  比普通历史说明更危险：
  - 因为迁移文档天然会被高级用户当成
    “底层用法权威页”
  - 一旦这里继续发布旧入口，
    后续高级示例、
    contract、
    backend 文档
    就会反复把旧 public surface 写回来

- 这页还叠加了第二层更细的 helper truth 漂移：
  - `GetNativeHandleSafe`
    参考段仍写
    `AContextMsg`
  - `TryGetNativeHandle`
    仍被写成：
    “对象不支持或句柄为 nil 时返回 False”
  - 示例甚至写成：
    `TryGetNativeHandle(Ctx, Pointer(SSL_CTX))`

- 但当前源码真实语义是：
  - 统一 helper
    优先走
    `fafafa.ssl.native_handle`
  - `TryGetNativeHandle`
    的布尔返回值
    代表对象是否支持
    `ISSLNativeHandleAccess`
  - 输出句柄
    仍可能是
    `nil`
    ，
    所以要求已初始化句柄时
    还要额外检查
    handle 本身

- 当前更准确的迁移文档真相应明确保留：
  - 普通新代码：
    `fafafa.ssl`
    +
    `TSSLContextBuilder`
    /
    `TSSLConnector`
  - fixed-backend / native-handle 高级场景：
    `TSSLFactory.GetLibraryInstance(...)`
    +
    `Lib.CreateContext(...)`
  - `sslFreePascal`
    已是 shipped backend，
    不是未来占位描述

- `ARCHITECTURE.md`
  这轮暴露的是“总览架构文档继续发布旧实现路线”的问题：
  - 它不只是一个旧 helper 漂移，
    而是会同时误导：
    - public entrypoint
    - backend 选择心智
    - backend 文件布局
    - `FreePascal`
      backend 的当前状态判断

- 这类 drift
  比单页 API 示例更危险：
  - 因为架构文档天然会被当成
    “设计路线与实现结构的权威图”
  - 一旦这里继续写
    `CreateLibrary`
    /
    旧 priority
    /
    “纯 FreePascal backend 未来才有”，
    后续设计讨论、
    新 backend 文档、
    甚至测试路线
    都会被带偏

- 这页当前最关键的错位有 4 组：
  - 普通入口仍没切到
    `TSSLContextBuilder`
    /
    `TSSLConnector`
  - 工厂段还把
    `CreateLibrary`
    当作当前公开入口
  - 优先级仍停留在旧值，
    没反映当前注册真相：
    - `WinSSL=200`
    - `MbedTLS=175`
    - `WolfSSL=150`
    - `OpenSSL=100`
    - `FreePascal=50`
  - backend 布局还在发布不存在的
    `fafafa.ssl.openssl.lib.pas`
    并静默漏掉
    `fafafa.ssl.freepascal.*`

- 当前更准确的架构文档真相应明确保留：
  - 普通新代码：
    `fafafa.ssl`
    +
    `TSSLContextBuilder`
    /
    `TSSLConnector`
  - fixed-backend / advanced：
    `TSSLFactory.GetLibraryInstance(...)`
    +
    `Lib.CreateContext(...)`
  - `TSSLFactory.CreateContext(...)`
    仍存在，
    但定位是 core / factory surface，
    不是取代 builder 的普通推荐入口
  - `FreePascal`
    backend
    已存在且 shipped；
    后续要继续推进的是
    capability parity
    和 runtime proof，
    不是再把它写回“未来才有”

- `docs/zh`
  这轮暴露出来的是“中文入口文档族整组停留在旧连接形态”的问题：
  - 它不只是旧工厂参数顺序，
    还叠加了：
    - `CreateConnection;`
    - `Connect(AHost, APort)`
    - `LoadSystemCertificates`

- 这类 drift
  的危险点在于：
  - 中文入口页往往就是很多读者的第一站
  - 如果这里继续发布不存在的连接形态，
    调用方拿去 copy 之后
    不是“稍微老一点的建议”，
    而是直接进入错误签名 / 错误对象生命周期模型

- 当前更准确的中文入口文档真相应明确保留：
  - 普通新代码：
    `TSSLContextBuilder`
    /
    `TSSLConnector`
  - fixed-backend 场景：
    `TSSLFactory.GetLibraryInstance(...)`
    或
    `TSSLContextBuilder.WithBackend(...)`
  - 低层 direct connection
    继续要求：
    - `CreateConnection(YourConnectedSocket)`
    - `ISSLClientConnection.SetServerName(...)`
    - `Connect`
  - 系统根证书
    对高入口文档
    应优先讲：
    `WithSystemRoots`
    而不是继续讲不存在的
    `LoadSystemCertificates`

- `docs/reference/ARCHITECTURE.md`
  这轮暴露的是另一种更隐蔽的
  reference drift：
  - 主说明段可能已经对了，
    但后面的里程碑 / 路线文字
    仍然会继续发布旧模块名
  - 这次实际残留的是：
    - `fafafa.ssl.types`
    - `fafafa.ssl.intf`

- 这类残留之所以危险，
  是因为它不会出现在最上面的
  “当前入口说明” 里，
  却会继续污染：
  - 设计讨论时对模块边界的认知
  - 后续文档作者对核心单元的引用
  - focused contract
    对旧术语的容忍度

- 当前这一页的 batch continuity
  已经明确显示出两个层次的 RED：
  - 第一层：
    reference 架构页缺少
    `TSSLContextBuilder`
    /
    `TSSLConnector`
    与当前 factory surface 说明
  - 第二层：
    在主说明段修正后，
    static contract
    继续抓出了更深处的旧模块名残留

- 当前更准确的 reference 架构页真相应明确保留：
  - 核心 base 单元：
    `fafafa.ssl.base`
  - 普通新代码入口：
    `fafafa.ssl`
    +
    `TSSLContextBuilder`
    /
    `TSSLConnector`
  - fixed-backend / core factory：
    `TSSLFactory.GetLibraryInstance(...)`
    /
    `TSSLFactory.CreateContext(...)`

- `docs/guides/CODE_STYLE.md`
  这轮暴露的是
  “活跃风格文档也会误发接口设计”
  的问题：
  - 它表面上只是 style guide
  - 但示例里如果继续写
    `LContext.CreateConnection;`
    就是在发布已经不存在的连接形态

- 这类 drift
  的危险性并不比入门文档低：
  - 因为风格指南经常被新人直接复制
  - 而且大家默认这里的示例
    “至少语义是真实的”
  - 一旦这里把 transport-first
    关系写错，
    后续代码就会围绕错误对象生命周期展开

- `docs/guides/MIGRATION_GUIDE_PHASE_2.4.md`
  当前更像一份
  “仍有历史保留价值的阶段说明”，
  不是当前 active migration guide；
  但历史文档也不能继续发布死掉的单元名

- 这轮核对后可以明确：
  - `Phase 2.4`
    类型安全主题本身仍然存在
  - 当前源码和测试依然保留：
    - `TSSLVersion`
    - `TKeyType`
    - `TCertificateFormat`
    - `TSecureData<T>`
    - `TResult<T, E>`
  - 但当前真实单元名已经是：
    `fafafa.ssl.safety`
  - `fafafa.ssl.types.safe`
    只剩历史文档与旧注释残留，
    不能再被当成可用入口

- 当前更准确的处理原则已经明确：
  - 活跃文档：
    必须直接对齐当前 public truth
  - 历史阶段文档：
    可以保留，
    但必须明确历史定位，
    且示例中的文件名 / 单元名
    仍要与当前源码说真话

- `type-safety`
  这条线这轮暴露的是
  “feature 仍在，但 façade 吸收不完整”
  的问题：
  - `src/fafafa.ssl.safety.pas`
    仍是活跃源码
  - `tests/test_type_safety.pas`
    仍是真实测试
  - 但主门面之前没有显式收进
    non-generic type-safety surface
  - 活跃文档也没有把这个边界讲清楚

- 这说明当前“接口设计完整”
  不能只看：
  - 有没有定义单元
  - 有没有历史迁移稿
  - 有没有孤立测试
  还要看：
  - 主门面是否真的把它吸收成当前 public surface
  - 活跃 API 文档是否讲的是同一套入口

- 这轮也给出了一个很重要的边界结论：
  - `TSSLVersion`
    /
    `TKeySize`
    /
    `TTimeoutDuration`
    /
    `TBufferSize`
    这组
    non-generic safety surface
    当前适合并入
    `fafafa.ssl`
  - 但
    `TSecureData<T>`
    /
    `TResult<T, E>`
    当前还不适合被说成
    “已经稳定挂在主门面”

- 这里不是主观保守，
  而是被当前 FPC / 代码事实逼出来的真相：
  - 我们尝试过把 generic pattern
    做成 façade alias
  - 编译期直接暴露出
    generic alias 语法/解析障碍
  - 所以当前更真实、更稳的 public truth
    是：
    - façade = non-generic safety surface
    - `fafafa.ssl.safety` = generic pattern 窄入口

- 当前更准确的文档与接口结论应明确保留：
  - 普通调用方若要
    `TSSLVersion`
    /
    `TKeySize`
    /
    `TTimeoutDuration`
    /
    `TBufferSize`
    可直接
    `uses fafafa.ssl`
  - 若要
    `TSecureData<T>`
    /
    `TResult<T, E>`
    则继续
    `uses fafafa.ssl.safety`
