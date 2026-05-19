# Task Plan - Interface Design And Backend Implementation Verification

## Goal

全面验证 `fafafa.ssl` 的公共接口设计、门面/工厂/builder/config 语义、以及各 backend 实现与 capability 发布是否一致；把发现写成可复用记录，并在边界清晰时直接修复高价值问题，避免后续反复从旧 release / old roadmap 入口重新拉起。

> note:
> - 本轮用户要求“执行一个 goal 全面的验证并记录”。
> - 线程级 goal 当前仍处于 active 状态；这份 `task_plan.md` 与新增 `docs/plans/...` 继续作为该总 goal 下各个 focused 批次的权威执行记录。

## Current Status

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
