# Findings - Interface Design And Backend Implementation Verification

## 2026-05-20

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
