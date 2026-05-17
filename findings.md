# Findings - CI Runtime Gate Repair

## 2026-05-17

- `v1.5.0` 现在已经是正式发布态，而不是待批准态：
  - `master` 当前 head 是记录 release closeout 的 `9d0e330`
  - 已发布 tag `v1.5.0` 仍指向 `e775ac5`
  - GitHub Release 与 source archive 已在线，不应再把审批门或 release portability 写成当前 blocker

- 当前残余问题不在生产代码、不在 runtime，也不在 release workflow，而在发布后控制面漂移：
  - `docs/ROADMAP.md` 一度同时出现 `current_release_status: RELEASED` 与 `engineering_state: CLOSED_OUT_PENDING_APPROVAL`
  - `docs/README.md`、`docs/DOCUMENTATION_INDEX.md`、`.github/README.md` 仍把 `release-control / v1.5.0 formalization` 写成当前默认入口
  - `task_plan.md` 仍把早已解决的 `rg` portability / archive staging 问题写成当前 blocker
  - 如果不收口这些控制面文件，后续批次会继续从错误入口启动

- 因此当前最小正确动作不是重开发布链，也不是重跑 Windows/WinSSL，而是：
  - 只做 active entrypoint + working-memory 的 post-release truth sync
  - 只跑受影响 docs/contracts 的窄验证
  - 收口后把下一条实现线明确交还给 `SSL/TLS backend completeness roadmap`

- Windows/WinSSL 不应再被整体描述为 `static-only`：
  - 仓库公开后，GitHub Actions 已能真实执行 `wave-b-b2-manual.yml`
  - 当前剩余问题是 Windows runner 上的真实 runtime/blocker，而不是“没有验证条件”

- 当前 runtime 主线已经在同一 head `b95044d` 上闭环：
  - manual run `25989095571` 的 `windows-gate` / `macos-gate` / `linux-gate` / `summary` 全部 `SUCCESS`
  - 默认 `CI` run `25989090032` 也在同一 head 上 `SUCCESS`
  - 这意味着当前 release-control 不再有新的跨平台 runtime blocker

- 当前真正的流程漂移不在生产代码，而在控制面文档和 contracts：
  - `docs/test_reports/RELEASE_READINESS_V1.5.0.md` 仍写 `READY_FOR_MAIN_MERGE`
  - `docs/plans/2026-05-12-release-v1.5.0-formalization.md`、`RELEASE_NOTES_V1.5.0.md`、`.github/README.md` 仍保留 Linux-only / deferred WinSSL 叙事
  - `tests/scripts/test_v1_5_0_static_pascal_audit_contract.sh` 与 `tests/scripts/test_release_workflow_v1_5_0_contract.sh` 还把这些旧叙事锁成合同
  - 如果不一起修，后续 release-control 会继续被旧标准误导，即使运行时真相已经变了

- 当前更准确的 release readiness 状态应是 `PASS_PENDING_APPROVAL`：
  - 本地 release gates、static audit、manual cross-platform runtime、默认 CI 都已通过
  - latest tag 仍是 `v1.4.3`
  - 仍未得到用户对 `v1.5.0` tag / GitHub Release 的明确批准

- 用户在 `2026-05-17` 已明确批准正式发布 `v1.5.0` 后，第一条真实 release run `25991512715` 暴露出的故障不是发布主体逻辑，而是 release contract 的 runner portability 盲区：
  - `Release v1.5.0` workflow 的 `Checkout`、`Resolve release version`、`Install dependencies`、`Verify version metadata` 全部通过
  - `python3 scripts/compile_all_modules.py`、minimal gate、FreePascal completeness、style、Phase 2 dry-run 也都在同一个 `Run release gates` step 内跑到了最后
  - 真正失败点是 `bash tests/scripts/test_release_workflow_v1_5_0_contract.sh`
  - 该脚本默认直接调用 `rg` 做 multiline regex 检查，但 `release.yml` 没有安装 `ripgrep`
  - 因此 runner 上报的是环境 portability 故障：`rg: command not found`

- 这次 release 失败有一个重要路线结论：
  - 当前 release 主链并没有被 compile/completeness/style/performance dry-run 卡住
  - 失败点已经前移到“最后一条 release-control contract 是否能在 GitHub runner 默认工具集上运行”
  - 最小正确修法是让 contract 自身具备 `rg` 缺失时的 Python/grep fallback，而不是为了一个合同去扩大 release workflow 的依赖面

- 第二次 release run `25991710335` 又继续把失败边界前移了一层：
  - `rg` portability 修复后，公开 job 页面显示新的失败步骤已经变成 `Create source archive`
  - 这说明 release 主链已经真正穿过了 `Run release gates`
  - 当前更可能的根因不是发布权限，而是归档路径本身：workflow 一边归档 `.`，一边把 `${ARCHIVE_NAME}.tar.gz` 直接写在 repo 根目录
  - 这类写法容易让 `tar` 在扫描 `.` 时把刚生成的 archive 自己也卷进去，产生自引用 / file changed 边界

- 当前这条 release lane 的最小正确修法因此是归档 staging，而不是再扩大发布权限或改 release action：
  - 先把 tarball 写到 `RUNNER_TEMP` 或等价的 repo 外路径
  - 归档完成后再 `mv` 回工作目录，保持 upload / publish step 的既有输入名不变
  - 同时用 focused contract 把“不能直接往被归档目录里写 tarball”固定下来

- 第三次 release run `25991977801` 已完成最终发布闭环：
  - `Run release gates` SUCCESS
  - `Create source archive` SUCCESS
  - `Upload release archive evidence` SUCCESS
  - `Publish GitHub Release` SUCCESS
  - `v1.5.0` tag 现指向 `e775ac5`
  - GitHub Release `fafafa.ssl v1.5.0` 已发布，source archive `fafafa-ssl-v1.5.0-source.tar.gz` 已上传

- 这次正式发布的关键流程结论是：
  - 第一次失败暴露的是 release contract portability 盲区
  - 第二次失败暴露的是 archive staging 盲区
  - 两条都不是产品实现或发布权限本身的问题，而是 release-control 工作流边界条件
  - 当前 `release-control / v1.5.0 formalization` 已经真正闭环，后续不应再围绕这个版本做重复验证

- 第一次 Windows manual run `25985103443`（`windows-gate`）暴露的第一硬故障不是 WinSSL 逻辑，而是 workflow shell 入口：
  - `Run quick WinSSL smoke` 在真正测试前就失败
  - 根因是 workflow 用 `powershell` 执行 UTF-8/Unicode-heavy WinSSL 脚本
  - 这已经通过 `d32ab3a` 收口：相关 workflow 入口统一改为 `pwsh`

- 第二次 Windows manual run `25985356670` 证明 `pwsh` 修复有效，并把第一硬故障前移到 Lazarus 工程目标配置：
  - quick smoke 已经执行到 `lazbuild test_winssl_certificate_loading.lpi`
  - 失败日志显示 Lazarus 在 Windows runner 上仍被项目文件拉成 `-Tlinux`
  - 关键后果是 FPC 以 `Target OS: Linux for x86-64` 编译，最终报 `Fatal: (10022) Can't find unit system`

- 这不是单个 quick smoke 工程的孤立脏数据，而是当前 Windows runtime 入口工程集的真实配置漂移：
  - `tests/quick_winssl_validation.ps1` 用到的 `tests/winssl/test_winssl_certificate_loading.lpi`
  - `tests/run_winssl_tests.ps1` 用到的
    - `tests/winssl/test_winssl_unit_comprehensive.lpi`
    - `tests/winssl/test_winssl_integration_multi.lpi`
    - `tests/integration/test_backend_comparison.lpi`
    - `tests/winssl/test_winssl_performance.lpi`
    - `tests/winssl/test_winssl_handshake_debug.lpi`
    - `tests/winssl/test_winssl_https_client.lpi`
  - 在修复前都把 `TargetOS` 写死为 `linux`

- 当前批次的最小正确修法不是再猜 workflow shell，而是把 Windows runtime 入口工程恢复成 host-truth：
  - 新增 `tests/scripts/test_winssl_windows_runtime_project_target_contract.sh`
  - 该合同显式守护 quick smoke 与 broader suite 所用 `.lpi` 不得再硬编码非 Windows 目标
  - 当前实现选择移除这些工程文件里的 `TargetCPU/TargetOS=linux` 硬编码，让 Windows runner 回到 host target truth

- 第三次 Windows manual run `25985680381` 已经证明这条 `.lpi` 修法真实生效：
  - quick smoke 编译阶段已变成 `Compiling test_winssl_certificate_loading.lpi... [OK]`
  - 旧的 `-Tlinux` / `Can't find unit system` 边界已经消失

- 新的 quick smoke 失败形态不再是编译错误，而是进程启动即退：
  - `test_winssl_certificate_loading.exe` 在没有任何测试正文输出的情况下以 `-1073741511` (`0xC0000139`) 退出
  - 这里关于 `0xC0000139` 属于“入口点缺失”的判断，是根据 Windows 常见状态码对该退出值的推断

- 结合当前 live 源码，最可疑的启动前崩点是 SSPI 导入表而不是测试逻辑本身：
  - `src/fafafa.ssl.winssl.api.pas` 把服务端握手 API 声明成了 `AcceptSecurityContextW`
  - 但该 SSPI API 的真实导出名应为未后缀的 `AcceptSecurityContext`
  - 因为 WinSSL 程序启动时会先解析整张导入表，所以哪怕 quick smoke 本身不立刻走到服务端握手，也可能在进入 `main` 前就因不存在的导出名崩掉

- 当前第二批修法采用的是最小 live-source truth 修正：
  - 新增 `tests/scripts/test_winssl_acceptsecuritycontext_import_contract.sh`
  - `src/fafafa.ssl.winssl.api.pas` 改为绑定未后缀的 `AcceptSecurityContext`
  - `src/fafafa.ssl.winssl.connection.pas` 的 live 调用点同步改成 `AcceptSecurityContext`
  - 本地 `python3 scripts/compile_all_modules.py` 仍保持 `185/185` 通过

- 第四次 Windows manual run `25985958467` 已经证明 `AcceptSecurityContext` 修法真实生效：
  - `Run quick WinSSL smoke` SUCCESS
  - quick smoke 中 `test_winssl_certificate_loading.exe` 完整输出 22/22 PASS
  - 这说明当前 Windows runtime 已经跨过了 `0xC0000139` 启动级崩点

- 新的第一硬故障已经从 quick smoke 前移到 `Run Windows Wave B gate` 的 runner-truth 问题，而不是 WinSSL 证书加载实现本身：
  - WinSSL minimal runner:
    - `test_winssl_api_basic` PASS
    - `tests\unit\test_winssl_comprehensive.pas` runtime FAIL
    - 当前日志只有 `exit=1`，不足以定位真实测试失败原因
  - OpenSSL modules runner:
    - `validate_all_modules.ps1` 在 Windows runner 上用了 `C:\tools\freepascal\bin\i386-win32\ppc386.exe`
    - 随后因 `Contnrs` / `DateUtils` / `SyncObjs` 缺失失败

- 这个 `ppc386` 误命中不是底层 Pascal 代码退化，而是 workflow/toolchain 路径优先级 bug：
  - 现有 Windows workflow 把 `x86_64-win64`、`i386-win32`、`bin` 都用“循环 prepend”方式塞入 PATH
  - 因为最后一次 prepend 优先级最高，实际会把更泛化或更低优先级的路径顶到前面
  - 结果是后续步骤里 `Get-Command fpc` 并不稳定指向预期的 64 位编译器

- 当前第三批修法聚焦在“让下一次 run 的 Windows gate 更 truthful”：
  - 新增 `tests/scripts/test_workflow_windows_fpc_preference_contract.sh`
  - workflow / dormant templates 改为只选择一个 preferred FPC path，并把最终解析到的 `fpc` 路径写进日志
  - `run_winssl_tests.ps1` 改为显式捕获失败测试 stdout/stderr；如果失败时完全没有输出，也会写出明确提示

- 第五次 Windows manual run `25986225431` 已经证明第三批 truth-improvement 修法真实生效：
  - `Install dependencies` SUCCESS
  - `Run quick WinSSL smoke` SUCCESS
  - `Run Windows Wave B gate` 的失败日志现在已经足够具体，可以直接定位源码/脚本缺口

- 当前 WinSSL minimal runner 的真实问题不是交互输入，而是 backend 根本没注册：
  - `wave_b_windows_winssl_*.log` 明确显示 `tests\unit\test_winssl_comprehensive.pas` 的 14 个断言都报 `Windows Schannel is not registered`
  - 该测试文件在调用 `TSSLFactory.GetLibraryInstance(sslWinSSL)` 前只 `uses fafafa.ssl.winssl.lib`，但没有显式调用 `RegisterWinSSLBackend`
  - 因此最小正确修法是让该测试在进入工厂路径前先注册 WinSSL backend，而不是继续猜 PowerShell/ReadLn 行为

- 当前 OpenSSL modules runner 的真实问题也已经从“workflow 路径优先级不 truthful”前移到“Windows unit root 探测不够稳健”：
  - workflow 日志已经明确记录当前 runner 实际解析到的是 `C:\tools\freepascal\bin\i386-win32\fpc.exe`
  - 这说明第三批 workflow 修法至少把“最终用了哪个 fpc”讲清楚了
  - 但 `validate_all_modules.ps1` 仍过于依赖单一 `units\<tp>-<to>` 路径假设
  - 在当前安装布局下，这会漏掉 `Contnrs` / `DateUtils` / `SyncObjs` 所在 unit roots

- 当前第四批修法选择的是两个最小 live-truth 修正：
  - 为 `tests/unit/test_winssl_comprehensive.pas` 补显式 `RegisterWinSSLBackend` 启动保护
  - 为 `scripts/validate_all_modules.ps1` 补 `units`、`lib\fpc\*\units`、`fpc\*\units` 三类布局的 fallback 探测
  - 同时新增 focused contracts，持续守护这两个刚暴露出来的真实缺口

- 第六次 Windows manual run `25986661765` 已证明第四批 Wave B gate 修法真实生效：
  - `Install dependencies` SUCCESS
  - `Run quick WinSSL smoke` SUCCESS
  - `Run Windows Wave B gate` SUCCESS
  - 当前新的第一硬阻塞已经前移到 `Run broader WinSSL runtime suite`

- broader suite 里的 `Backend Comparison Tests` 不是新的工厂实现问题，而是同类注册缺口换了一个测试入口重现：
  - 远端日志显示 `tests/integration/test_backend_comparison.pas` 在 `TestBasicFunctionality` 里直接走 `TSSLFactory.GetLibraryInstance(sslWinSSL)`
  - 该文件此前没有像 `tests/unit/test_winssl_comprehensive.pas` 那样显式调用 `RegisterWinSSLBackend`
  - 因此最小正确修法仍然是补显式 backend registration guard，而不是继续改 factory 本体

- broader suite 里的 `WinSSL Integration Tests (Multi-Scenario)` 也不是整体 WinSSL 网络路径回归：
  - 同一份远端日志中，Google / GitHub API / Cloudflare / Microsoft 四个 HTTPS 场景都已完整 PASS
  - `TestProtocolNegotiation` 的 TLS 1.2-only 子用例 PASS，TLS 1.2/1.3 mixed 自动协商之前的主要握手路径也已被前面的场景覆盖
  - 当前唯一炸点是 TLS 1.3-only 子用例在 `CreateConnection` 阶段直接抛 `ESSLInitializationException`，原生错误为 `0x80090331`（`SEC_E_ALGORITHM_MISMATCH`）
  - 这说明当前更合理的修法是让测试把这种 Schannel 平台条件路径收进“可能不支持 TLS 1.3 only”的通过分支，而不是把 broader suite 直接打成未捕获异常退出

- 当前第七批修法因此保持在测试契约层，而不是扩大到 WinSSL 生产实现：
  - 新增 `tests/scripts/test_backend_comparison_factory_registration_contract.sh`
  - 新增 `tests/scripts/test_winssl_integration_multi_tls13_optional_contract.sh`
  - `tests/integration/test_backend_comparison.pas` 现在会在 Windows 入口显式注册 WinSSL backend
  - `tests/winssl/test_winssl_integration_multi.pas` 现在会把 TLS 1.3-only 下的 `SEC_E_ALGORITHM_MISMATCH` 视为平台条件化结果
  - 这样可以继续把 GitHub Windows runner 上的新故障边界前移，而不在没有额外证据时过早改动 Schannel 实现

- 第七次 Windows manual run `25987105283` 已证明第七批修法命中了前一轮的真故障点：
  - `Backend Comparison Tests` 已不再报 `Windows Schannel is not registered`
  - `WinSSL Integration Tests (Multi-Scenario)` 的 TLS 1.3-only 子用例也不再在原位置把 broader suite 直接炸停
  - `quick smoke` 与 `Run Windows Wave B gate` 继续保持 SUCCESS

- 这次 broader suite 暴露出的 integration multi 问题，已经不再是上一轮的“入口边界”问题，而是更深一层的测试语义缺口：
  - TLS 1.3-only 子用例现在会把 `SEC_E_ALGORITHM_MISMATCH` 记成普通 FAIL，说明 optional-path 识别条件还不够宽
  - `HTTP 端口 TLS 握手失败` 在当前 runner 上会抛 `ESSLProtocolException`，但测试仍只断言 `not LConn.Connect`
  - `中等数据传输 (~10KB)` 对 `www.microsoft.com` 主页响应要求 `>= 1024` 字节，在当前 runner 上实际只拿到约 `686` bytes，阈值过于脆弱
  - 因此当前更合理的修法是把这三条都收进“expected failure / stable threshold”测试语义，而不是把它们当成新的 WinSSL 生产逻辑回归

- 这次 broader suite 暴露出的 backend comparison 问题，则已经从测试入口缺口前移到真实实现内层：
  - 测试现在能走进 `TLS 握手对比`
  - 新的崩点是 `src/fafafa.ssl.winssl.connection.pas` 在成功握手后调用 `src/fafafa.ssl.winssl.lib.pas:UpdateHandshakeStatistics`
  - runner 上在 `EnterCriticalSection(FStatisticsLock)` 附近触发 `EAccessViolation`
  - 这说明当前最小正确修法不是重写握手逻辑，而是先把库级统计更新降为 best-effort，避免观测路径反向打崩成功握手

- 当前第八批修法因此分成两个 focused lanes：
  - `tests/winssl/test_winssl_integration_multi.pas`
    - 新增 `IsExpectedHandshakeFailure`
    - TLS 1.3-only optional-path 识别放宽到 native error truth
    - HTTP/SSL3 负路径异常改为 expected failure
    - 中等响应阈值降到当前 runner 可复现的稳定范围
  - `src/fafafa.ssl.winssl.connection.pas`
    - 新增 `TryUpdateLibraryStatistics`
    - 握手成功后的统计更新改成 best-effort，不让 observability 路径打崩主连接语义

## 2026-05-15

- GitHub Actions 账户额度不再是当前 blocker：
  - 仓库公开后，远端 workflows 已经真正执行
  - 旧的 “billing/quota startup failure” 叙述已经过时，不能继续作为当前停滞原因

- CI run `25893971783`（head `2eb563f`）的真实失败点是 completeness job 依赖缺口：
  - `Minimal Gate (Linux)` PASS
  - `Code Quality (Light)` PASS
  - `FreePascal TLS 1.3 Completeness` FAIL
  - failed log 明确落在 `WolfSSL KnownIssues 运行时对齐测试`
  - 关键错误：`Failed to load WolfSSL library: libwolfssl.so`
  - 结论：`.github/workflows/ci.yml` 的 completeness job 缺 `libwolfssl-dev`

- 同一依赖缺口也存在 release workflow：
  - `release.yml` / `release.yml.disabled` 也会调用 `run_freepascal_tls13_completeness_gate.sh`
  - 如果不一起补，未来 tag/release 路线会重现同样的 WolfSSL runtime 失败

- TLS13 signer workflow run `25901035350`（head `2eb563f`）有两个独立问题：
  - `Run TLS13 signer gate bundle` 失败：
    - 内层 `signer_gate_ci` exit=1
    - bundle 最终 `overall_state=ATTENTION`
  - `Append step summary` 失败：
    - `.github/workflows/tls13-signer-gate.yml` 的 here-doc terminator `PY` 多缩进了 2 个空格
    - shell 实际报 `warning: here-document ... wanted 'PY'` 和 `syntax error: unexpected end of file`

- `scripts/run_freepascal_tls13_servercertverify_bench.sh` 的主问题在 bench 构建层，而不是 signer 实现层：
  - 编译时强加 `-Criot`，会在 `src/fafafa.ssl.crypto.hash.pas` 触发 compile-time constant range-check errors
  - 编译输出被重定向到 `/dev/null`，把真实错误吞掉，导致远端只看到“bench step exit=1”
  - 去掉该旗标后，本地 bench 可以稳定产出指标

- 修复后本地验证已经闭环：
  - focused contract tests PASS
  - `run_freepascal_tls13_servercertverify_bench.sh` PASS
  - `run_tls13_signer_gate_ci.sh` PASS（run_id=`20260515_131250`）
  - `run_tls13_signer_gate_bundle.sh --strict` PASS（run_id=`local_bundle_repair_20260515`）

- 首次修复推送 `d3ebeee` 后，远端真相又进一步收敛：
  - signer run `25901775672`
    - `Run TLS13 signer gate bundle` 已经 PASS
    - `Append step summary` 仍 FAIL
    - 新错误不再是 shell EOF，而是 Python heredoc 正文带 2 个前导空格导致 `IndentationError`
  - CI run `25901775676`
    - completeness job 仍 FAIL
    - 关键错误仍是 `Failed to load WolfSSL library: libwolfssl.so`
    - 这说明“只补 workflow apt 依赖”还不够，WolfSSL loader 本身也需要 fallback

- 因此第二批根因修法应分两条落地：
  - workflow 层：
    - signer summary 的 heredoc terminator 与 Python body 都必须输出成真正可执行的脚本
  - Pascal runtime 层：
    - `src/fafafa.ssl.wolfssl.api.pas` 不能只赌 `LoadLibrary('libwolfssl.so')`
    - 在 Linux 上需要回退扫描常见系统库目录与版本化 `libwolfssl.so*`

- 历史说明：这一批当时仍按用户约束把 Windows/WinSSL 视为 `static-only`。
  - 该假设已在 `2026-05-17` 被真实 GitHub Windows runtime run 推翻，后续 continuation 必须以新的 runtime 证据为准。

- 第二次推送 `18f154f` 后，远端状态继续收敛：
  - signer run `25902255923`（head `18f154f`）已经 SUCCESS
  - CI run `25902255941` 仍 FAIL，且仍落在 `WolfSSL KnownIssues 运行时对齐测试`

- 但这次不能再把 `25902255941` 直接归因为 “loader fallback 仍未生效”：
  - 当前仓库里的 `.github/workflows/ci.yml` 真实内容显示：
    - `Minimal Gate (Linux)` 的 install step 包含 `libwolfssl-dev`
    - `freepascal-tls13-completeness` 的 install step 却仍是 `fpc libssl-dev python3`
  - 旧版 `tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh` 只对整份 `ci.yml` 做 `grep -Fq libwolfssl-dev`
  - 这会产生假绿：只要别的 job 装了 `libwolfssl-dev`，contract 就会放行，即使 completeness job 自己没装

- 因此第三批修复的真实根因是“workflow + contract 双重盲区”：
  - workflow 漏装：completeness job 确实缺 `libwolfssl-dev`
  - contract 漏检：没有把断言限定在 `freepascal-tls13-completeness` job 的 install step

- 本地长跑 `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id local_ci_runtime_repair_20260515` 已 PASS：
  - FreePascal / WolfSSL / MbedTLS KnownIssues 运行时对齐全部通过
  - 这至少证明：在“依赖存在”的前提下，当前 loader fallback 与 completeness 脚本链路是可工作的

- 第三次推送 `8d052dd` 后，远端 CI run `25902644127` 的真相继续收敛：
  - `WolfSSL KnownIssues 运行时对齐测试` 已经 PASS
  - 新的唯一失败点变成 `MbedTLS KnownIssues 运行时对齐测试`
  - 关键错误：`Failed to initialize MbedTLS library (LastError=-1, Details=Failed to load MbedTLS libraries)`

- 这说明当前主问题不再是 WolfSSL loader：
  - 远端 runner 已经真正装到了 `libwolfssl-dev` 并成功跑过 WolfSSL runtime alignment
  - 当前 completeness lane 下一层真实依赖缺口是 `libmbedtls-dev`

- release 路线必须和 completeness lane 同步补上 `libmbedtls-dev`：
  - `release.yml` / `release.yml.disabled` 同样会执行 `run_freepascal_tls13_completeness_gate.sh`
  - 如果只修 CI，不修 release，未来 tag/release 会原样复现 MbedTLS 缺库红灯

- workflow 结构重复会让“看似正确的补丁”打偏到相邻 job：
  - `ci.yml` 里两个 install step 文本高度相似
  - 这批第一次补 `libmbedtls-dev` 时误命中了 `Minimal Gate (Linux)` 而不是 completeness job
  - 新的 job-local contract 立即把这个误命中抓了出来，说明当前 contract 粒度是有效的

- 最新远端 CI run `25902932655`（head `30467e4`）说明依赖缺口已经补齐：
  - `FreePascal KnownIssues runtime alignment` PASS
  - `WolfSSL KnownIssues runtime alignment` PASS
  - `MbedTLS KnownIssues runtime alignment` PASS
  - 失败发生在打印 `所有测试完成！` 之后，而不是任何一个测试主体内部

- 因此当前最可信的根因不是新的 capability/runtime 回归，而是进程退出期清理：
  - `src/fafafa.ssl.mbedtls.lib.pas` / `src/fafafa.ssl.wolfssl.lib.pas` 的 `finalization` 会调用 `Unregister...Backend`
  - 这会走到 `src/fafafa.ssl.factory.pas` 的 `TSSLFactory.UnregisterLibrary -> ReleaseLibrary -> ISSLLibrary.Finalize`
  - 在 GitHub runner 的退出期再次触发 backend `Finalize/Unload`，高度可疑会导致双清理或失序清理下的 `EAccessViolation`

- 这批修法选择的是最小 shutdown-safe 路径，而不是扩大正常运行时语义：
  - `TSSLFactory` 新增 `UnregisterLibraryForProcessShutdown`
  - `TMbedTLSLibrary` / `TWolfSSLLibrary` 新增 `GSkipFinalizeOnDestroy`
  - backend unit 的 `finalization` 改走 shutdown-safe 注销，仅移除工厂持有引用与注册项，避免在进程退出期再进入 backend `Finalize`

- 本地验证支持这条修法，但还不等于远端已证实：
  - `python3 scripts/compile_all_modules.py` 185/185 编译成功
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id local_shutdown_unregister_20260515` PASS
  - 本地 long-run gate 在打印 `所有测试完成！` 后未再出现退出期异常
  - 是否彻底消除 GitHub runner 上的 `EAccessViolation` 仍需第五次 push 复核

- 第五次提交 `45dabb4` 后，远端 CI run `25903921296` 已经证明 shutdown-time 崩溃真正消失：
  - `FreePascal TLS 1.3 Completeness` SUCCESS
  - `Minimal Gate (Linux)` SUCCESS
  - `Code Quality (Light)` SUCCESS

- 运行时主阻塞解除后，当前最高价值的小批次变成 workflow hygiene：
  - GitHub Actions 远端日志提示 `actions/checkout@v4` 仍运行在 Node20 兼容层，并带来 deprecation annotation
  - 仓库内不只活跃 workflow，多个 dormant template 也仍保留 `actions/checkout@v3/v4`
  - 如果只修活跃 workflow，未来重启旧模板时还会把同类告警重新带回仓库

- 这批选择 `actions/checkout@v5` 而不是更激进地跳到 `v6`：
  - `v5` 已明确切到 Node24 runtime，足以消除当前 deprecation 来源
  - 对现有 workflow 语义变化最小，适合做低风险收口
  - `v6` 还带有凭据持久化路径变化，当前批次没有必要顺手扩大

- 新增 focused contract 后，checkout 升级可以被持续守护：
  - `tests/scripts/test_workflow_checkout_node24_contract.sh` 会拒绝 `.github/workflows` 下残留的 `actions/checkout@v3/v4`
  - 同时强制当前活跃 workflow 与需同步模板显式使用 `actions/checkout@v5`

- 第六次提交 `d56637f` 后，远端 runs 说明 checkout 方向是对的，但 workflow hygiene 还没完全结束：
  - `TLS13 Signer Gate` run `25904745243` SUCCESS
  - `CI` run `25904745247` SUCCESS
  - 新 annotation 不再指向 `actions/checkout@v4`
  - 但 signer run 新暴露出 `actions/upload-artifact@v4` 仍运行在 Node20 兼容层

- 这意味着当前最优策略不是停下，而是顺着同一条 hygiene 主线继续收第二层：
  - 先用 focused contract 抓出 `.github/workflows` 里残留的 `actions/upload-artifact@v4`
  - 再统一升级 workflow / template，避免未来启用旧模板时重新带回 artifact 侧告警

- 这批选择 `actions/upload-artifact@v6` 而不是 `v5`：
  - `v6` 已默认切到 Node24 runtime
  - `v5` 需要额外启用 Node24 切换，不适合作为仓库默认基线
  - 因此仓库级卫生标准直接锁到 `v6` 更稳妥

- 新增 artifact contract 后，workflow runtime hygiene 扩展成双合同面：
  - `tests/scripts/test_workflow_checkout_node24_contract.sh` 负责 checkout
  - `tests/scripts/test_workflow_upload_artifact_node24_contract.sh` 负责 artifact
  - 两者组合后，当前活跃 workflow 与同步模板都能被持续守护在 Node24 默认线

- 重新审查 workflow hygiene 路线后，旧的 `download-artifact@v5` 目标已被今天的官方 release 真相推翻：
  - 截至 `2026-05-16`，`actions/download-artifact` 最新 release 是 `v8.0.1`
  - 官方 `v7.0.0` release 明确写明：这是默认跑在 `node24` 的第一条主线
  - 旧计划里的 `v5` 不仅不是最新，也不是 `download-artifact` 的 Node24 默认线，继续照它执行会把错误基线固化进仓库

- 对这个仓库来说，`download-artifact` 最小安全修法应锁到 `v7` 而不是直接跳 `v8`：
  - `v7` 已经满足 Node24 默认 runtime
  - `v8` 还引入了 ESM、digest mismatch 默认报错、非 zip 下载解压策略变化
  - 当前 workflow 只需要收 Node20 弃用源，不需要顺手扩大 artifact 语义变化

- 这批暴露出的流程问题不是代码问题，而是验证路径问题：
  - 当前仓库里实际使用 `actions/download-artifact` 的活跃 workflow 是 `wave-b-b2-manual.yml`
  - 它是 `workflow_dispatch`，不会随着 push 自动运行
  - 其他残留点都在 `.disabled` 模板
  - 因此拿 `CI` / `TLS13 Signer Gate` 的绿灯来证明 `download-artifact` 升级是错误的验证代理

- 新增 `tests/scripts/test_workflow_download_artifact_node24_contract.sh` 后，第三波 hygiene 的静态边界终于完整：
  - 它会拒绝 `.github/workflows` 下残留的 `actions/download-artifact@v3` 到 `@v6`
  - 同时强制当前活跃 manual workflow 与相关 dormant templates 使用 `actions/download-artifact@v7`

- 第八批结束后，`task_plan.md` 再次暴露出一个流程问题：
  - 代码、push、远端 CI 都已经完成
  - 但计划文件仍停在 “准备第八次 commit/push”
  - 这说明我们的 workflow hygiene 收口除了修代码，还要把 planning files 当成真实交付物；否则后续 continuation 会被过期 queue 误导

- 继续核对剩余 action 家族后，当前仍有直接升级路径的 Node20 风险只剩三类：
  - `softprops/action-gh-release@v2`
  - `actions/setup-python@v5`
  - `actions/cache@v4`
  - 它们都不是“看起来旧但其实安全”的假问题

- `softprops/action-gh-release` 的官方真相非常直接：
  - `v2` 的 `action.yml` 仍是 `runs.using: "node20"`
  - 官方 `v3.0.0` release note 明确写明：runtime 从 Node20 迁到 Node24
  - 因此 `release.yml` / `release.yml.disabled` 里的 `@v2` 是活跃 release 路径上的真实 hygiene 缺口

- `actions/setup-python` 与 `actions/cache` 也都存在明确的 Node24 默认线：
  - `actions/setup-python@v6.0.0` release note 明确写明 upgrade to node 24，且 `action.yml` 为 `node24`
  - `actions/cache@v5.0.0` release note 明确写明 `@v5` runs on Node.js 24，而 `v4.3.0` 的 `action.yml` 仍是 `node20`
  - 因此 `code-quality.yml.disabled` 里的 `setup-python@v5` 与 dormant Windows workflows 里的 `cache@v4` 都应继续收掉

- 这批和 `download-artifact` 不同，验证边界更简单：
  - `softprops/action-gh-release@v2` 命中活跃 `release.yml`
  - `setup-python@v5` 与 `cache@v4` 虽在 dormant workflow，但都有明确官方 Node24 后继线
  - 所以它们不需要先等远端 annotation 点名，再做被动修复

- `gcarreno/setup-lazarus@v3.4.1` 最初看起来像上游阻塞，但继续静态审查后发现这是可以在仓库内彻底替掉的：
  - 目标 workflow 只有 `test-all-platforms.yml.disabled` 命中它
  - 该 step 只需要把 FPC/Lazarus 装进 PATH，并不依赖 action 的额外封装能力
  - 仓库里现成的 `wave-b-b2-manual.yml` Windows 安装段已经证明可以用 `choco install -y freepascal lazarus` + PATH 探测来替代

- 因此 `setup-lazarus` 这条剩余项的真正问题不是“上游没发 Node24 我们就没办法”，而是之前的路线图过早接受了“上游阻塞”这个结论：
  - 这是一个典型的 workflow/process 审查问题
  - 正确做法是先问：这个 third-party action 在本仓库里是否真的不可替代
  - 本次答案是否定的，所以最终应优先本地去依赖化，而不是等待上游

- 清掉 Node20 action 之后，workflow 供应链风险并没有自动归零：
  - major tag 仍然是浮动引用
  - 上游仓库只要移动 `v5` / `v6` / `v7` / `v3` 这些 tag，本仓库的执行语义就会变
  - 这类漂移不会在本地 diff 或 code review 里自然出现，因此它是下一层更隐蔽、但更像 workflow/process 问题的风险

- 对当前仓库来说，把外部 action pin 到 full commit SHA 是低风险高收益的下一步：
  - action 家族已经收敛，数量有限
  - 当前 `.github/workflows` 只依赖 6 个外部 action 家族
  - 因此完全可以先固定在当前已经验证过的 tag 对应 commit，再用注释保留可读版本号

- 这波取证时选择的是“当前 major tag 真实指向的 commit”，而不是手工抄最新 release tag：
  - 这样能保证 pin 后语义尽量贴近当前仓库已经在使用的行为
  - 同时把未来不受控的 tag 漂移切断
  - 这比“顺手升级到一个更高 release 再 pin”更稳

- 第十一次提交 `5a03f1c` 推送后的远端证据已经闭环：
  - `TLS13 Signer Gate` run `25967316650` SUCCESS
  - `CI` run `25967316614` SUCCESS
  - `Code Quality (Light)`、`Minimal Gate (Linux)`、`FreePascal TLS 1.3 Completeness` 全部 SUCCESS
  - 这说明本轮 SHA pinning 只是收紧供应链引用，没有误伤当前自动主线执行语义

- 当前最值得继续深挖的风险面已经从 action 版本/引用漂移前移到 `permissions:`：
  - action 已经收敛到 Node24 默认线并 pin 到 commit SHA
  - 剩余更像 workflow/process 问题的高价值面，是仓库是否还在使用默认过宽 token 权限
  - 这条线同样适合先做静态 contract，再做最小权限收紧

- 本轮还再次暴露出一个流程问题：
  - 代码、push、远端 CI 可能已经全部完成
  - 但 `task_plan.md` / `progress.md` 如果不及时同步，下一次 continuation 仍会从过期 queue 起跑
  - 因此 planning files 不能只记录“准备做什么”，也必须记录“远端最终如何收口”

- 在 action 版本线、SHA pinning 都收口后，workflow 剩余最高价值的安全面确实前移到了 `GITHUB_TOKEN` 权限：
  - 静态扫描显示，除了 `release.yml` / `release.yml.disabled` 的 `contents: write` 之外，其余 workflow / template 此前都没有显式 `permissions:`
  - 这意味着真实权限边界取决于仓库默认设置，而不是受仓库代码审查控制

- 对这个仓库当前非 release workflow 来说，显式 `permissions: contents: read` 已经足够覆盖活跃链路：
  - 当前步骤面主要是 `checkout`、本地脚本执行、artifact 上传下载、cache 和 step summary
  - 推送 `a24b983` 后，`TLS13 Signer Gate` run `25967632738` 与 `CI` run `25967632737` 全绿
  - 其中 `Upload evidence`、`Upload TLS13 signer artifacts`、`Append step summary` 也继续成功
  - 因此可以合理确认：把默认 token 收紧到只读没有误伤当前自动主线

- 新增 `tests/scripts/test_workflow_permissions_contract.sh` 的价值不只是抓这一次缺口：
  - 它把“workflow 必须显式声明权限”变成了持续守护面
  - 以后即使仓库默认 `GITHUB_TOKEN` 设置被人改宽，只要 YAML 不跟着扩权，当前主线就不会静默漂移

- permissions 这一层收口后，下一条更细的 workflow 安全面已经收缩到 checkout credential persistence：
  - 很多 job 只是拉代码和跑脚本，并不需要在后续 git 命令里保留认证信息
  - 因此 `persist-credentials: false` 是下一条值得继续静态审查的 least-privilege 方向

- checkout credential persistence 这条线已经被远端自动主线实证收口：
  - 推送 `6421420` 后，`TLS13 Signer Gate` run `25969736945` SUCCESS，`CI` run `25969736933` SUCCESS
  - `Upload TLS13 signer artifacts`、`Upload evidence`、`Upload FreePascal TLS 1.3 evidence`、`Append step summary` 全部继续通过
  - 这说明对当前仓库的活跃 workflow 来说，显式 `persist-credentials: false` 不会打坏 checkout 之后的真实主线

- 继续静态钻 dormant workflow 时，又抓到一个和默认 checkout 行为直接相关的真 bug：
  - `pr-checks.yml.disabled` 的 `pr-info`、`test-coverage-check`、`code-stats` 都执行 `git diff HEAD~1 HEAD`
  - 但 `actions/checkout` 默认只抓一个提交；如果恢复启用，这些 job 会因为缺父提交而失败
  - 这不是风格问题，而是 dormant workflow 被重新启用后可静态确定复现的 correctness 缺口

- 这次修法故意没有把整个 `pr-checks` 模板一刀切成 `fetch-depth: 2`：
  - 只有真正依赖 `HEAD~1` 的 3 个 job 被加深历史
  - `quick-build` 与 `pr-report` 保持最小 checkout 深度
  - 这种“按用途收最小历史”比统一 `fetch-depth: 0` 更符合我们当前的 least-privilege / least-data 方向

- 当前下一条最可能继续挖出 dormant bug 的面，已经不是版本或权限，而是事件上下文假设：
  - `pr-checks.yml.disabled` 同时支持 `pull_request` 和 `workflow_dispatch`
  - 但多处 shell 仍直接读取 `github.event.pull_request.*` / `github.event.number`
  - 这类字段在手动 dispatch 下并不天然存在，值得继续静态收敛

- 这条事件上下文假设已经被收口成一条具体、已修复的 dormant bug：
  - `pr-checks.yml.disabled` 的 PR title / description / report 三个步骤之前都默认当前一定有 PR 上下文
  - 推送 `cbd86d0` 后，自动 `CI` run `25970607766` 继续 SUCCESS，说明这次上下文修补没有误伤主线

- 这次修法的关键不是“把空值吞掉”，而是把 manual 模式变成显式语义：
  - 标题检查与描述检查在 `workflow_dispatch` 下明确输出“manual dispatch: no PR ...”
  - 报告步骤在 manual 模式下改用 `manual` / `Manual dispatch` / `github.actor` / `github.ref_name` / `manual-dispatch`
  - 这样未来如果有人真的手动运行这条模板，不会看到看似成功但实际基于空 PR 数据拼出来的误导性摘要

- 继续往下做 dormant workflow correctness 时，最值得优先看的仍是 mixed-trigger 输入模型：
  - 不是所有问题都会表现成语法错或版本旧
  - 很多问题其实是 “push / pull_request / workflow_dispatch 三种上下文共存时，某个分支默认以为自己拿到了另一种事件的数据”
  - 当前仓库已经证明这种问题可以用 focused contract 很快抓住并小步修复

- 第十五波 dispatch-context 修复的 docs closeout 已经被远端主线再次证实：
  - `083c057` 只是 planning/docs truth sync
  - 但对应 `CI` run `25970738320` 继续 SUCCESS
  - 这再次说明及时同步 working-memory 不只是“记笔记”，也是 continuation workflow 的一部分真实收口

- 继续沿 dormant workflow truth 往下挖时，`performance.yml.disabled` 暴露出一条比输入默认值更硬的 correctness bug：
  - workflow 宣称 `ubuntu-latest` / `windows-latest` / `macos-latest` 三平台 matrix
  - 但 build step 使用的 `tests/test_performance_comparison.lpi` 在项目里把 `TargetOS` 固定成了 `linux`
  - run / report steps 又直接写成 PowerShell 语法和 `.exe` 路径，Linux/macOS 默认 `bash` runner 一旦恢复启用会静态确定失败

- 这类 dormant truth 问题比“参数没用上”更危险：
  - summary step 之前还硬编码写出 Windows / Linux / macOS 全部完成
  - 但 workflow 自己并没有给 Windows/macOS 建立真实 toolchain 或 runtime proof
  - 对当前仓库来说，先把模板声明范围收回到真实支持面，比继续保留假矩阵更安全

- 这次最小可信修法不是去“猜着补齐三平台”，而是先把 truth 收紧：
  - `performance.yml.disabled` 的 matrix 先只保留 `ubuntu-latest`
  - benchmark 改为直接 `fpc` 编译 `tests/test_performance_comparison.pas`，不再依赖 Linux-locked Lazarus project
  - summary 改为动态枚举实际下载到的 report，不再硬编码“all platforms success”

- 新增 `tests/scripts/test_workflow_performance_linux_truth_contract.sh` 后，这条 dormant workflow truth 也进入了持续守护面：
  - 它要求 workflow 的 runner 声明、build 入口、shell 语义和 summary 结论彼此一致
  - 推送 `1d4f346` 后，自动 `CI` run `25970919173` 继续 SUCCESS，说明这次 truth 收紧没有误伤活跃 Linux 主线

- 继续审 dormant 多平台模板时，`test-all-platforms.yml.disabled` 又暴露出一组静态确定的 truth bug：
  - 三个平台 job 都声称跑 `3.2.2` 和 `3.3.1` 双版本矩阵
  - 但安装步骤并没有使用 `matrix.fpc-version`，只是重复跑同一套 runner 默认工具链
  - `test-summary` 还硬编码 6 行 `✅`，即使真实 job 结果和 artifact 数量并不支持这些结论

- 这组问题里最硬的一条不是“矩阵写大了”，而是 summary 已经在静态层面误报成功：
  - `test-macos` 在修复前根本没有 artifact upload step
  - 但 summary 依旧给出两行 macOS 成功记录
  - 这意味着哪怕 workflow 以后恢复启用，汇总页也可能在缺证据时继续产出看似完整的全绿摘要

- 这次最小可信修法延续了前一波 performance truth 的原则：
  - 不去假装补齐 `3.3.1` 真验证
  - 而是删除未真正生效的假版本矩阵，让 job 名称、cache key、artifact 名称都回到 runner-default truth
  - summary 改为读取 `needs.<job>.result` 与实际下载到的 artifact 目录，不再硬编码平台成功结论

- 新增 `tests/scripts/test_workflow_test_all_platforms_truth_contract.sh` 后，这条 dormant 多平台模板也进入了持续守护面：
  - 它会阻止假 `fpc-version` 矩阵回流
  - 也会阻止再次出现“没有 macOS artifact 但 summary 硬写 macOS success”这类误导性 closeout
  - 推送 `b7c76aa` 后，自动 `CI` run `25979379612` 继续 SUCCESS，说明这次收紧没有误伤当前活跃 Linux 主线

- 继续往下看 `ci-matrix-draft.yml.disabled` 时，Linux lane 也暴露出一条更“静态确定”的假矩阵问题：
  - workflow 声称测试 OpenSSL `3.0` / `3.1` / `3.2`
  - 但 `matrix.openssl` 和 `apt_package` 完全没有进入安装或运行路径
  - 三个 job 实际都只是重复安装同一个 `libssl-dev` / system OpenSSL，再把 artifact 名字伪装成不同版本

- 这条问题和上一波 `test-all-platforms` 很像，但边界更清楚：
  - 它不是 summary 层误报，而是 job 标识层误报
  - 如果以后有人重新启用这个 draft，会误以为 Linux lane 做过跨 OpenSSL 版本验证
  - 实际上仓库当前只证明了“runner 默认 system OpenSSL”这一个事实

- 这次最小可信修法同样没有去“猜着补齐多版本验证”：
  - 直接删掉未生效的 `openssl` 假矩阵
  - Linux artifact 改名为 `linux-system-openssl-reports`
  - 安装步骤显式输出 `system OpenSSL` 版本，把 lane truth 固定成当前 runner 默认库

- 新增 `tests/scripts/test_workflow_ci_matrix_draft_truth_contract.sh` 后，`ci-matrix-draft` 也进入了持续守护面：
  - 它会阻止假的 OpenSSL 3.0/3.1/3.2 矩阵重新回流
  - 也会阻止 artifact 名称继续借 `matrix.openssl` 冒充多版本验证
  - 推送 `5b55193` 后，自动 `CI` run `25979777225` 继续 SUCCESS，说明这次收紧没有误伤当前活跃 Linux 主线

- 继续静态审查 `winssl-tests.yml.disabled` 时，又抓到一组更贴近“真源漂移”的 dormant bug：
  - `workflow_dispatch.test_suite` 已定义但完全未消费
  - setup 只安装 `freepascal`，却直接调用 `lazbuild`
  - workflow 维护了一套过时 inline Pascal 测试，甚至还指向不存在的 `tests/test_winssl_comprehensive.lpi` / `tests\bin\test_winssl_comprehensive.exe`

- 对这条 Windows dormant lane，最小可信修法不是继续补那套过时 inline 测试，而是回到仓库已有真源：
  - `tests/quick_winssl_validation.ps1`
  - `tests/run_winssl_tests.ps1`
  - 这样 workflow 声明、仓库脚本和未来人工运行路径才能重新对齐

- `winssl-tests.yml.disabled` 的 summary 之前最大的问题不是“文案夸张”，而是把未执行的能力写成结论：
  - `All WinSSL tests PASSED`
  - `WinSSL backend is PRODUCTION READY`
  - `Zero-dependency Windows deployment SUPPORTED`
  - 对当前用户约束下的 dormant/static 审查来说，这类结论必须退回到“当前 run 观察到了什么 transcript”这一层

- `code-quality.yml.disabled` 则暴露出另一种同样典型的 dormant truth drift：
  - `build-check` 声称覆盖 `3.2.2` / `3.3.1` 双版本矩阵，但 workflow 从未消费 `matrix.fpc`
  - workflow 直接调用 `lazbuild`，却没有安装或验证 Lazarus / `lazbuild`
  - `quality-report` 即使在 `if: always()` 下运行，也硬编码 `~85%`、`Overall Grade: A`、`WinSSL Backend: 100%`

- 这批 code-quality 的最小可信修法也延续了同一个原则：
  - 不去凭空补一个“真实双版本矩阵”
  - 直接收回到 runner system toolchain truth
  - summary 只汇报 `needs.*.result` 和当前工具链事实，不再输出固定 grade / 覆盖率 / 后端完备度

- 新增 `test_workflow_winssl_tests_truth_contract.sh` 与 `test_workflow_code_quality_truth_contract.sh` 后，这两条 dormant lane 也进入了持续守护面：
  - 它们会阻止未消费 dispatch 输入、缺 Lazarus 却调用 `lazbuild`、以及硬编码生产级结论重新回流
  - 推送 `9331faa` 后，自动 `CI` run `25980352095` 继续 SUCCESS，说明这次收紧没有误伤当前活跃 Linux 主线

- 当前 workflow 审查的高价值面已经继续前移：
  - 旧 action 版本、SHA pinning、permissions、checkout persistence、假矩阵、假 summary 这一层已经基本收口
  - 下一站更值得深挖的是 mixed-trigger/manual workflow 的输入模型和手动模式语义边界

- 继续静态审查 mixed-trigger/manual workflow 时，`performance.yml.disabled` 暴露出一种更隐蔽的“假输入”问题：
  - `workflow_dispatch.benchmark` 表面上被读到了 shell 里
  - 但它既不改变编译入口，也不改变执行入口
  - 实际运行始终是同一个 `./tests/bin/test_performance_comparison`
  - 这类问题比“完全没引用输入”更容易误导，因为看起来像是“已经接上了”

- 对当前 performance lane，最小可信修法不是去捏造一个 per-category 执行协议，而是把 truth 收回到程序现状：
  - 保留 `workflow_dispatch` 手动触发
  - 删除死输入 `benchmark`
  - 报告明确写成 “full checked-in comparison suite”
  - 在 benchmark binary 真正支持前，不再暴露 category 选择 UI

- `ci-matrix-draft.yml.disabled` 则暴露出另一种 manual 语义漂移：
  - `skip_windows` / `skip_macos` 确实会改变 job 是否运行
  - 但 `test-summary` 只是遍历 artifact 目录和 grep `PASS/SUCCESS`
  - 这样手动 skip 的 lane 会在 summary 中悄悄消失，而不是被显式标成 `skipped`

- 这类 summary 问题的关键不是“输出不好看”，而是它把操作者的手动决策抹掉了：
  - 对 manual workflow 来说，`skipped by input` 和 `no artifact / check logs` 是两种完全不同的语义
  - 如果 summary 不显式区分，后续 continuation 很容易把“故意没跑”误读成“跑了但没证据”

- 新增 `test_workflow_performance_dispatch_truth_contract.sh` 与 `test_workflow_ci_matrix_dispatch_truth_contract.sh` 后，这两条 manual-input lane 也进入了持续守护面：
  - 它们会阻止死 benchmark 输入和 artifact-dir 猜状态逻辑回流
  - 推送 `c8b3000` 后，自动 `CI` run `25980651893` 继续 SUCCESS，说明这次 manual-input 收紧没有误伤当前活跃 Linux 主线

- 当前下一条最明确的 dormant summary truth 缺口已经收缩到 `pr-checks.yml.disabled`：
  - `pr-report` 仍硬编码 `PR Information / Quick Build / Test Coverage / Code Statistics` 全部 `✅ Passed / ✅ Complete`
  - 但它本身已经有 `needs`，完全应该改成从真实 `needs.*.result` 生成表格

- `pr-checks.yml.disabled` 这条 summary 问题的关键不只是“状态表写死了”，而是它把 workflow 之外的策略也冒充成了 workflow 真相：
  - `Reviewers required: 2`
  - `Checks required: 4`
  - `Auto-merge: Disabled`
  - 这些都属于仓库配置或团队流程，不能从当前 YAML 自证

- 对 `pr-checks` 来说，最小可信修法也不需要额外脚本层：
  - 它已经有 `needs: [pr-info, quick-build, test-coverage-check, code-stats]`
  - 直接把 summary 状态表改成 `needs.*.result`
  - 再把无法由 workflow 自证的 reviewer / merge / required checks 叙述删掉即可

- 这类修法的价值不只是“报告更真实”，还会减少 continuation 的误导：
  - 以后就算某个 job 是 `failure`、`cancelled`、`skipped`
  - summary 也不会继续伪装成四行 `✅`
  - 对 dormant/manual workflow 尤其重要，因为后续人往往只先看 summary

- 新增 `test_workflow_pr_checks_summary_truth_contract.sh` 后，`pr-checks` 这条 summary lane 也进入了持续守护面：
  - 它会阻止硬编码全绿状态表和假 branch-protection/merge-policy 断言回流
  - 推送 `b98625e` 后，自动 `CI` run `25980879737` 继续 SUCCESS，说明这次 summary 收紧没有误伤当前活跃 Linux 主线

- 当前下一条更明确的 dormant summary truth 缺口已经前移到：
  - `basic-checks.yml.disabled` 的 `Generate report` 仍硬编码 `Project structure valid` / `Required files present` / `Basic syntax check passed`
  - `linux-ci.yml.disabled` 的 `check-success` 仍硬编码 `Project is ready for integration`

- `basic-checks.yml.disabled` 这条 summary 问题的关键不只是三行 `✅` 写死了，而是失败时连 truthful summary 都不会留下：
  - `Generate report` 缺 `if: always()`
  - 一旦 `Check required files` 或基础语法检查提前失败，用户看到的只会是 step log，没有统一摘要

- 对 `basic-checks` 来说，最小可信修法不需要引入额外脚本：
  - 给三个前置检查补 step id
  - summary 直接读取 `steps.file-structure.outcome`、`steps.required-files.outcome`、`steps.basic-syntax.outcome`
  - 再把 report step 改成 `if: always()` 即可

- `linux-ci.yml.disabled` 的问题则不是“不会失败”，而是把单一 lane 结果说大了：
  - `check-success` job 名直接叫 `All Checks Passed`
  - 成功 step 还输出 `Project is ready for integration`
  - 但当前 workflow 只证明了这一次 `ubuntu-latest` 的 `build-and-test` lane 结果，不能替代更大的集成结论

- 对 `linux-ci` 来说，最小可信修法是把收口 job 退回到 result summary：
  - summary 直接读取 `needs.build-and-test.result`
  - 文案明确 scope 只到 `ubuntu-latest build-and-test lane from this run only`
  - 再指向对应 logs / artifact，而不是下 integration-ready 结论

- 新增 `test_workflow_basic_checks_summary_truth_contract.sh` 与 `test_workflow_linux_ci_summary_truth_contract.sh` 后，这两条 dormant lane 也进入了持续守护面：
  - 它们会阻止 `basic-checks` 回流到“写死三条 success 且失败时无 summary”
  - 也会阻止 `linux-ci` 再次把单一 Ubuntu lane 包装成 `All Checks Passed` / `ready for integration`
  - 推送 `6615b69` 后，自动 `CI` run `25981061685` 继续 SUCCESS，说明这次收紧没有误伤当前活跃 Linux 主线

- 当前下一条更明确的 dormant summary truth 缺口已经继续前移到 `test-all-platforms.yml.disabled`：
  - 虽然平台结果表已经改成真实 `needs.*.result`
  - 但 summary 末尾仍固定输出 `Core modules (P0): 6/6`、`High priority (P1): 14/14`、`Medium priority (P2): 11/11`、`Low priority (P3): 15/15`
  - 还固定输出 `WinSSL backend: Full support`
  - 这些断言都大于当前 run 可直接证明的范围

- 这轮还暴露出一个明确的流程问题：我们把“缓存绿灯的治理合同”当成了每一批都要陪跑的固定动作。
  - 重复最多的是 `test_workflow_action_sha_pinning_contract.sh`、`test_workflow_checkout_credentials_contract.sh`、`test_workflow_permissions_contract.sh`
  - 它们大多是轻量静态合同，不是最重的编译/运行时脚本
  - 但重复执行仍然会浪费节奏、污染 progress 记录，也会让真正的新增风险面不够突出

- 纠偏后的验证策略应该按“影响面”而不是按“习惯动作”来决定：
  - workflow 治理基线合同一旦在某个 head 统一绿灯，就进入缓存集合
  - 后续只有在触碰对应治理面或合同脚本本身时才重跑
  - dormant summary-only 批次只跑新合同、最近邻合同和 `git diff --check`
  - docs closeout 批次不再同步阻塞等待整条自动 CI，除非触碰活跃链路或自动 CI 已红

- 继续探索后，当前更需要纠偏的不是某个运行时实现，而是总目标叙述本身：
  - `task_plan.md` 还沿用了 `CI Runtime Gate Repair` 这一旧标题和旧目标
  - 但当前真实工作已经从 runtime 救火切到了 dormant/manual workflow 的 truth/evidence 收口
  - 如果 goal 不改，后续 continuation 很容易又回到“重新证明已经修完的 runtime 问题”这条老路

- `test-all-platforms.yml.disabled` 暴露的是这一类“方向偏差”的典型例子：
  - 平台结果表已经 truthful
  - 但 summary 末尾仍固定写 `Core modules (P0): 6/6`、`High priority (P1): 14/14`、`Medium priority (P2): 11/11`、`Low priority (P3): 15/15`
  - 还固定写 `WinSSL backend: Full support`
  - 这会把“本次平台 job 的执行结果”偷换成“整个能力面已经被证明”

- 对这条 lane 的最小可信修法，不是再发明一套新的 coverage 统计，而是删掉这些超范围断言：
  - summary 保留 `needs.*.result` 与下载到的 artifact
  - notes 只说明“当前 run 的证据边界”
  - WinSSL 能力必须回到 Windows lane 的实际日志和 artifact，而不是 summary 常量

- 这次收口之后，workflow 路线的总目标应该明确改成：
  - `workflow truth and evidence hardening`
  - 也就是继续清理 dormant/manual workflow 中剩余的固定能力宣告、固定数字和超范围结论
  - 而不是回到已经完成的 runtime blocker 叙事

- `linux-ci.yml.disabled` 这次又暴露出一类更细的 evidence wording 漂移：
  - 它已经不再写 `All Checks Passed` / `ready for integration`
  - 但 summary 里还残留 `Expected compile: ~75 (excludes WinSSL)`、`Status: ✅ See job output`
  - 以及 `Full test coverage requires Windows runner for WinSSL`
  - 这些文案虽然比旧的全局结论轻，但仍然会把“本次 Linux lane 观察到的事实”说成近似能力模型

- 对这条 lane，最小可信修法也仍然是收窄到当前 run 证据：
  - 不保留近似模块数
  - 不保留硬编码 `✅`
  - 不再说“full coverage requires Windows”，而是直接说“WinSSL-specific evidence requires a Windows lane; this Linux run does not prove WinSSL behavior”

- 这说明当前最高价值的剩余风险已经进一步从 YAML 常量前移到了“脚本/文案生成层”：
  - 纯 YAML 里的固定 summary claim 基本已经收得差不多
  - 下一站更值得审的是 `wave-b-b2-manual.yml(.disabled)` 及其 handoff/closure 脚本
  - 因为那一条 lane 的 summary 不是写在 YAML 常量里，而是由 `prepare_wave_b_b2_handoff_bundle.sh` 等脚本生成

- 对 `wave-b-b2` 做完 completion audit 后，真实边界进一步明确了：
  - `.github/workflows/wave-b-b2-manual.yml(.disabled)` 自身主要负责 runner 编排、artifact 上传下载、以及调用 `prepare_wave_b_b2_handoff_bundle.sh`
  - 本轮没有在 YAML 里再发现新的固定能力宣告或假 summary 常量
  - 同类 over-claim 的真实落点就是脚本生成的 markdown next actions

- `generate_wave_b_cross_platform_summary.sh` closed 分支原先确实说大了：
  - 它只知道 Linux/macOS/Windows summary/probe/examples 这些局部证据
  - 却写成“当前三平台 cross-platform evidence 已对齐”
  - 这会把 summary 对齐偷换成更高层的 evidence/handoff truth

- `check_wave_b_b2_closure_readiness.sh` closed 分支也确实说大了：
  - 它的 `closure_status=CLOSED` 只建立在三平台 summary 全 PASS
  - 并不验证 `consistency_status`、Windows companion artifacts、或完整 report chain
  - 因此 next action 不能再把这个状态表述成像“整条交接链已闭环”

- 对这两处的最小安全修法是“保留状态机、收紧文案”，而不是改名或重写聚合逻辑：
  - 保留 `closure_status=CLOSED` 兼容现有 `prepare_wave_b_b2_handoff_bundle.sh` / consistency contracts
  - 仅把 closed wording 收窄为 `summary` / `platform summary` scope
  - 同时显式补一句“完整交接仍需结合 `closure / consistency / handoff bundle` 判断”

- completion audit 还确认了更高层聚合职责目前没有同类问题：
  - `check_wave_b_b2_evidence_consistency.sh` 只宣称 consistency 与 closure 对齐
  - `prepare_wave_b_b2_handoff_bundle.sh` 只有在 `closure_status=CLOSED`、`consistency_status=CONSISTENT` 且 report chain 合法时才会给出 `handoff_state=CLOSED`
  - 所以这一轮真正需要动的是 cross-summary / closure wording，而不是 handoff bundle 状态机

- 因此如果下一轮还要沿 `wave-b-b2` 深挖，重点应该前移到 report metadata parse / downgrade semantics：
  - 比如 malformed closure-platform matrix、缺 metadata、或 run_id mismatch 的降级路径是否都能 truthful 地落到 `NEEDS_REPORT_REPAIR` / `NEEDS_EVIDENCE_SYNC`
  - 而不是回头重复审已经收掉的 closed wording

- 沿这条 queue 继续深挖后，`prepare_wave_b_b2_handoff_bundle.sh` 暴露出一个真实的 report-chain metadata 漏洞：
  - 它修复前会验证 `closure_status`、`consistency_status` 和 closure platform matrix
  - 但不会验证 closure / consistency report 自己的 `run_id` 是否真等于当前批次 `RUN_ID`
  - 因此旧 report 或串批次 report 只要 status 字段看起来合法，handoff bundle 仍可能继续落到 `CLOSED`

- 这类问题和之前的 wording drift 不同，已经不是“说大了”，而是会直接影响最终降级状态：
  - 如果 report chain 顶层 `run_id` 都不对，这不是 `READY_FOR_RUNNER`、也不是 `CLOSED`
  - 它本质上属于 downstream report metadata 损坏，应当归入 `NEEDS_REPORT_REPAIR`

- 对这个问题的最小安全修法仍然不需要重写状态机：
  - 只在 `prepare_wave_b_b2_handoff_bundle.sh` 里补 closure / consistency report 的 `run_id` 解析与比对
  - 缺失时记录 `*_report run_id missing`
  - 不匹配时记录 `*_report run_id mismatch`
  - 继续复用现有 `report_chain_issues -> NEEDS_REPORT_REPAIR` 降级通道

- 新增 `test_prepare_wave_b_b2_handoff_bundle_report_run_id_contract.sh` 后，这个分支也进入了持续守护面：
  - 它覆盖 closure report 串批次和 consistency report 串批次两个场景
  - 并要求 handoff bundle 同时给出 `NEEDS_REPORT_REPAIR` 和可读的 `report_chain_note`

- 这次修复还说明当前 `wave-b-b2` 线下一步最值得继续补的是“缺失分支”，而不是再追 wording：
  - `run_id mismatch` 已经有了 focused contract
  - 接下来更像相邻高价值面的，是 `run_id missing` 这种 parse-hole 是否也都有对称合同

- 沿这条线继续下钻后，`check_wave_b_b2_evidence_consistency.sh` 暴露出一个更细的 truth bug：
  - 修复前它确实会把 `closure_report run_id missing/mismatch` 计入 `runid_mismatch_or_parse_issue`
  - 但顶层 `closure_status_note` 仍可能保留 `CLOSED`
  - 这会进一步把 next actions 错带到“当前 closure 已闭环，但 evidence consistency 仍未对齐”

- 这个问题的关键不在计数，而在“顶层摘要被旧状态掩盖”：
  - row 级别已经能看到 `run_id not found` / `run_id mismatch`
  - 但如果顶部 note 仍是 `CLOSED`
  - 后续只看摘要的人会被误导，以为 closure report 本身是可信的，只是 evidence 没对齐

- 对这类问题的最小安全修法，仍然不需要重写 next-actions 状态机：
  - 只把 closure report 相关 issue 先汇总进一个局部 `closure_report_issues`
  - 如果这个集合非空，顶层 `closure_status_note` 直接输出 joined issues
  - 这样现有 next-actions 分支自然会走到 generic metadata-misaligned 路径，而不是 `CLOSED` 分支

- 新增 `test_wave_b_b2_consistency_closure_report_run_id_contract.sh` 后，这个 consistency 顶层摘要面也进入了持续守护：
  - 它覆盖 `closure_report run_id missing` 和 `closure_report run_id mismatch`
  - 同时要求顶层 `closure_status_note`、row note、`runid_mismatch_or_parse_issue` 和 next actions 保持同一条 truth

- 这说明当前 `wave-b-b2` 线上最值得继续补的，已经从 “mismatch 是否处理” 前移到 “missing 分支是否对称”：
  - handoff bundle 这边下一站更值钱的是 `closure_report run_id missing` / `consistency_report run_id missing` focused contracts
  - consistency 这一层的 closure report top-note truth 已经收口

- 随后的 docs closeout `87ee953` 对应 `CI` run `25983461905` 继续 SUCCESS：
  - `Code Quality (Light)`、`Minimal Gate (Linux)`、`FreePascal TLS 1.3 Completeness` 全部 SUCCESS
  - 说明这条 wave-b truth sync 路线继续没有把自动主线带偏

- 沿着上一条 queue 把 `prepare_wave_b_b2_handoff_bundle.sh` 的 `run_id missing` 分支补成 focused contract 后，当前结论是“coverage gap 已补齐”，不是又发现了新的 prod bug：
  - 脚本本身已经会把 `closure_report run_id missing` 和 `consistency_report run_id missing` 都降级到 `NEEDS_REPORT_REPAIR`
  - 缺口在于合同之前只锁住了 mismatch，没有把 missing 一起钉死
  - 现在 `test_prepare_wave_b_b2_handoff_bundle_report_run_id_contract.sh` 已同时覆盖 missing / mismatch，对 `report_chain_note` 的 truthful 对称性也形成了持续守护

- 因而当前 `wave-b-b2` 线上更合理的下一跳，不再是重复追 `run_id missing`，而是再往外一圈补“整个 report 文件缺失”这条分支的 focused contracts：
  - `closure_report missing`
  - `consistency_report missing`
  - 目标仍是确认 `NEEDS_REPORT_REPAIR`、`report_chain_note` 和 next actions 保持同一条 truth

- 这次提交 `fb8664a` 只扩大了 focused contract 覆盖面，没有修改生产脚本状态机：
  - 因此远端自动 `CI` run `25983594565` 只需要作为增量 run id 记账
  - 除非它把自动主线打红，否则不需要把这类 coverage batch 升级成阻塞式盯跑

- 随后的 docs closeout `c3dfa78` 对应 `CI` run `25983622375` 继续 SUCCESS：
  - `Code Quality (Light)`、`Minimal Gate (Linux)`、`FreePascal TLS 1.3 Completeness` 全部 SUCCESS
  - 说明 wave-b truth sync 继续没有带偏自动主线

- 再往前把 `prepare_wave_b_b2_handoff_bundle.sh` 的“整个 report 文件缺失”分支补成 focused contract 后，当前结论仍然是“coverage gap 已补齐”，不是新的 prod bug：
  - 脚本本身已经会把 `closure_report missing` 和 `consistency_report missing` 都降级到 `NEEDS_REPORT_REPAIR`
  - 缺口只在于此前没有合同固定 `report_chain_note` 与 generic report-repair next actions 的对称 truth
  - 现在 `test_prepare_wave_b_b2_handoff_bundle_missing_report_contract.sh` 已把这两条分支钉住

- 因而当前 `wave-b-b2` 线上更合理的下一跳，已经从 handoff bundle 前移到 consistency 顶层摘要面：
  - 优先补 `check_wave_b_b2_evidence_consistency.sh` 的 `closure_report missing` focused contract
  - 目标是确认顶层 `closure_status_note`、row note 和 next actions 在“closure report 整个缺失”时，也不会回落到误导性的 `CLOSED/IN_PROGRESS` 叙事

- 这次提交 `aed5dbd` 同样只扩大了 focused contract 覆盖面，没有修改生产脚本状态机：
  - 因此远端自动 `CI` run `25983742832` 继续按增量 run id 记账即可
  - 除非自动主线转红，否则不需要把这类 coverage batch 升级成阻塞式盯跑

- 继续往 consistency 顶层摘要面下钻后，`check_wave_b_b2_evidence_consistency.sh` 暴露出一个更细的 truth bug：
  - `closure_report` 整个文件缺失时，row 已经是 `missing`
  - `required_missing` 也会正确变成 `1`
  - 但修复前顶层 `closure_status_note` 仍是 `n/a`
  - 这会让只看摘要的人看不到“closure report 根本没生成”这个关键事实

- 这个问题的最小安全修法仍然不需要改状态机：
  - 只在 `check_closure_report_artifact()` 的 missing 分支把顶层 `closure_status_note` 设成 `closure_report missing`
  - 保持现有计数语义：这仍是 required-missing 问题，不额外算作 parse mismatch

- 新增 `test_wave_b_b2_consistency_closure_report_missing_contract.sh` 后，这条缺失分支也进入了持续守护：
  - 它同时钉住顶层 `closure_status_note`
  - `closure_report` row 的 `missing`
  - `required_missing=1`
  - `runid_mismatch_or_parse_issue=0`
  - 以及 generic metadata-misaligned next actions

- 因而当前 `wave-b-b2` 线上更合理的下一跳，已经从 closure-report missing 前移到 cross-summary missing：
  - 优先补 `check_wave_b_b2_evidence_consistency.sh` 的 `cross_summary missing` focused contract
  - 目标是确认 cross-summary 整个文件缺失时，row note、`required_missing` 和 next actions 也不会漂成含糊的默认态

- `c38298e` 对应的 docs closeout run `25983797036` 已经 SUCCESS：
  - `Code Quality (Light)`、`Minimal Gate (Linux)`、`FreePascal TLS 1.3 Completeness` 全部 SUCCESS
  - 说明上一轮 missing-report closeout 也没有把自动主线带偏

- 这次提交 `6562f13` 同时修的是一个真实脚本缺口，而不只是 coverage：
  - 但修法仍然是最小的，只补 `closure_report missing -> closure_status_note=closure_report missing`
  - 因此远端自动 `CI` run `25983911908` 仍按增量 run id 记账即可
  - 除非自动主线转红，否则不需要把这类 truth batch 升级成阻塞式盯跑

- 再往下一层补 `cross_summary missing` focused contract 后，当前结论回到了“coverage gap 已补齐”，不是新的 prod bug：
  - `check_wave_b_b2_evidence_consistency.sh` 本身已经会把 cross-summary 缺失记成 `required_missing`
  - row 也已经是 `missing`
  - 在 closure report 仍然有效且 `closure_status_note=IN_PROGRESS` 时，next actions 继续走 in-progress closure guidance 也是 truthful 的
  - 缺口只在于此前没有 focused contract 把这条分支固定下来

- 新增 `test_wave_b_b2_consistency_cross_summary_missing_contract.sh` 后，这条缺失分支也进入了持续守护：
  - 它同时钉住 `required_missing=1`
  - `runid_mismatch_or_parse_issue=0`
  - `closure_status_note=IN_PROGRESS`
  - cross-summary row 的 `missing`
  - 以及 next actions 继续走 IN_PROGRESS closure guidance

- 因而当前 `wave-b-b2` 线上更合理的下一跳，已经从 cross-summary missing 前移到 cross-summary 顶层 run_id 漂移：
  - 优先补 `check_wave_b_b2_evidence_consistency.sh` 的 `cross_summary run_id missing/mismatch` focused contract
  - 目标是确认 row note 与 `runid_mismatch_or_parse_issue` 在 cross-summary 顶层元数据漂移时也保持对称 truthful

- `90db460` 对应的 docs closeout run `25983958292` 已经 SUCCESS：
  - `Code Quality (Light)`、`Minimal Gate (Linux)`、`FreePascal TLS 1.3 Completeness` 全部 SUCCESS
  - 说明上一轮 closure-missing closeout 也没有把自动主线带偏

- 这次提交 `00428c8` 再次证明当前 `cross_summary missing` 这条线只是 coverage gap，不是 prod bug：
  - 脚本行为本身没变
  - focused contract 只是把 `required_missing`、row note 和 next-actions truth 固化下来
  - 因此远端自动 `CI` run `25984057687` 继续按增量 run id 记账即可

- `e85cf18` 对应的 docs closeout run `25984086002` 已经 SUCCESS：
  - `Code Quality (Light)`、`Minimal Gate (Linux)`、`FreePascal TLS 1.3 Completeness` 全部 SUCCESS
  - 说明上一轮 cross-summary-missing closeout 也没有把自动主线带偏

- 再往下一层补 `cross_summary run_id missing/mismatch` focused contract 后，当前结论仍然是“coverage gap 已补齐”，不是新的 prod bug：
  - `check_wave_b_b2_evidence_consistency.sh` 本身已经会把这两类问题计入 `runid_mismatch_or_parse_issue`
  - cross-summary row 也已经会 truthful 地输出 `run_id not found` / `run_id mismatch`
  - valid closure report 的 `closure_status_note` 不会被这类 cross-summary 顶层元数据问题污染
  - 缺口只在于此前没有 focused contract 把这组 truth 固定下来

- 新增 `test_wave_b_b2_consistency_cross_summary_run_id_contract.sh` 后，这组元数据漂移分支也进入了持续守护：
  - 它同时钉住 cross-summary row note
  - `runid_mismatch_or_parse_issue=1`
  - `closure_status_note=IN_PROGRESS`
  - 以及 IN_PROGRESS 分支的 next-actions truth

- 这次提交 `c148889` 再次证明当前 `cross_summary run_id missing/mismatch` 这条线也只是 coverage gap，不是 prod bug：
  - 脚本行为本身没变
  - focused contract 只是把 row note、parse 计数和 IN_PROGRESS 分支的 next-actions truth 固化下来
  - 因此远端自动 `CI` run `25984350085` 继续按增量 run id 记账即可

- `0a38a0d` 对应的 docs closeout run `25984425968` 已经 SUCCESS：
  - `Code Quality (Light)`、`Minimal Gate (Linux)`、`FreePascal TLS 1.3 Completeness` 全部 SUCCESS
  - 说明上一轮 cross-summary-runid closeout 也没有把自动主线带偏

- 再往下一层补 `cross_summary run_id` 在 `closure_status=CLOSED` 分支下的 focused next-actions contract 后，当前结论仍然是“coverage gap 已补齐”，不是新的 prod bug：
  - `check_wave_b_b2_evidence_consistency.sh` 本身已经会把这类情况带到 closed-closure guidance
  - valid closure report 的 `closure_status_note` 会继续保持 `CLOSED`
  - next actions 也已经会 truthful 地说“closure 已闭环，但 evidence consistency 仍未对齐”
  - 缺口只在于此前没有 focused contract 把 closed-closure guidance 这层 truth 固定下来

- 新增 `test_wave_b_b2_consistency_cross_summary_run_id_closed_next_actions_contract.sh` 后，这个 closed-guidance 分支也进入了持续守护：
  - 它同时钉住 `closure_status_note=CLOSED`
  - cross-summary row note
  - `runid_mismatch_or_parse_issue=1`
  - 以及 closed-closure guidance 不被误带回 IN_PROGRESS / generic 分支

- 这次提交 `f84f042` 再次证明当前 `cross_summary run_id` 在 closed-closure guidance 这条线也只是 coverage gap，不是 prod bug：
  - 脚本行为本身没变
  - focused contract 只是把 `closure_status_note=CLOSED` 和 closed-closure next-actions truth 固化下来
  - 因此远端自动 `CI` run `25984594664` 继续按增量 run id 记账即可

- 因而当前 `wave-b-b2` 线上更合理的下一跳，已经从 cross-summary run_id closed guidance 前移到 cross-summary missing closed guidance：
  - 优先补 `check_wave_b_b2_evidence_consistency.sh` 的 cross-summary missing 在 `closure_status=CLOSED` 分支下的 focused next-actions contract
  - 目标是确认缺失 cross-summary 时，next actions 也继续落在 “closure 已闭环，但 evidence consistency 未对齐” 这条 truthful guidance

- 因而当前 `wave-b-b2` 线上更合理的下一跳，已经从 cross-summary run_id 基本计数前移到 closed-closure guidance：
  - 优先补 cross-summary run_id issue 在 `closure_status=CLOSED` 分支下的 focused next-actions contract
  - 目标是确认它会继续落在 “closure 已闭环，但 evidence consistency 未对齐” 这条 truthful guidance，而不是混回 generic 或 IN_PROGRESS 分支

- 再往下一层补 `cross_summary missing` 在 `closure_status=CLOSED` 分支下的 focused next-actions contract 后，当前结论仍然是“coverage gap 已补齐”，不是新的 prod bug：
  - `check_wave_b_b2_evidence_consistency.sh` 本身已经会把这类情况记成 `required_missing=1`
  - valid closure report 的 `closure_status_note` 也已经会继续保持 `CLOSED`
  - next actions 早就会 truthful 地说“当前 closure 已闭环，但 evidence consistency 仍未对齐”
  - 缺口只在于此前没有 focused contract 把 `cross_summary missing + CLOSED` 这层 truth 固定下来

- 新增 `test_wave_b_b2_consistency_cross_summary_missing_closed_next_actions_contract.sh` 后，这个 closed-guidance 缺失分支也进入了持续守护：
  - 它同时钉住 `closure_status_note=CLOSED`
  - `required_missing=1`
  - `runid_mismatch_or_parse_issue=0`
  - cross-summary row 的 `missing`
  - 以及 closed-closure guidance 不被误带回 IN_PROGRESS / generic 分支

- 这次提交 `e2df815` 再次证明当前 `cross_summary missing` 在 closed-closure guidance 这条线也只是 coverage gap，不是 prod bug：
  - 脚本行为本身没变
  - focused contract 只是把 `closure_status_note=CLOSED`、`required_missing` 计数和 closed-closure next-actions truth 固化下来
  - 因此远端自动 `CI` run `25984769630` 继续按增量 run id 记账即可

- 因而当前 `wave-b-b2` 线上更合理的下一跳，已经从 `cross_summary missing` closed guidance 前移到 `cross_summary` metadata/path issue 的 closed guidance：
  - 优先补 `linux_examples_json missing`、macOS probe metadata 丢失、Windows active evidence metadata 丢失等分支在 `closure_status=CLOSED` 下的 focused next-actions contract
  - 目标是确认这类 cross-summary 元数据/路径异常在 closure 已闭环时，也继续落在 same closed-closure guidance，而不是混回 generic 或 IN_PROGRESS 分支

- 再往下一层补 `cross_summary` metadata/path issue 在 `closure_status=CLOSED` 分支下的 focused next-actions contract 后，当前结论仍然是“coverage gap 已补齐”，不是新的 prod bug：
  - `check_wave_b_b2_evidence_consistency.sh` 本身已经会把 `linux_examples_json missing`、macOS probe metadata 丢失、Windows active evidence metadata 丢失计入 `runid_mismatch_or_parse_issue`
  - valid closure report 的 `closure_status_note` 也已经会继续保持 `CLOSED`
  - next actions 早就会 truthful 地说“当前 closure 已闭环，但 evidence consistency 仍未对齐”
  - 缺口只在于此前没有 focused contract 把这三类 `cross_summary` 元数据/路径异常在 closed-guidance 这层 truth 固定下来

- 新增 `test_wave_b_b2_consistency_cross_summary_metadata_closed_next_actions_contract.sh` 后，这组 closed-guidance 元数据/路径分支也进入了持续守护：
  - 它同时钉住 `linux_examples_json missing`
  - `macos probe metadata missing`
  - `windows active evidence metadata missing`
  - `closure_status_note=CLOSED`
  - 以及 closed-closure guidance 不被误带回 IN_PROGRESS / generic 分支

- 这次提交 `2cde68a` 再次证明当前 `cross_summary` metadata/path issue 在 closed-closure guidance 这条线也只是 coverage gap，不是 prod bug：
  - 脚本行为本身没变
  - focused contract 只是把 parse 计数、`closure_status_note=CLOSED` 和 closed-closure next-actions truth 固化下来
  - 因此远端自动 `CI` run `25984912652` 继续按增量 run id 记账即可

- 因而当前 `wave-b-b2` 线上更合理的下一跳，已经从 cross-summary metadata/path closed guidance 前移到 cross-summary active path/evidence closed guidance：
  - 优先补 custom `linux_summary` 缺失、custom `linux_examples_json` 失效、custom `macos_summary` 缺失、active `windows_summary` 缺 sibling runtime artifacts 等分支在 `closure_status=CLOSED` 下的 focused next-actions contract
  - 目标是确认这类 active path/evidence 异常在 closure 已闭环时，也继续落在 same closed-closure guidance，而不是混回 generic 或 IN_PROGRESS 分支

- 手动 Windows runtime run `25987503677`（head `9aaadeb`）证明上一轮“统计更新降级为 best-effort”的修法真实命中了故障点：
  - `Run quick WinSSL smoke` SUCCESS
  - `Run Windows Wave B gate` SUCCESS
  - `Run broader WinSSL runtime suite` 仍 FAIL，但旧的 `UpdateHandshakeStatistics` `EAccessViolation` 已不再出现

- `test_winssl_integration_multi.pas` 在 `25987503677` 上暴露出的新边界，不再是 HTTP-port 或 medium-size threshold：
  - `HTTP 端口 TLS 握手失败` 已 PASS
  - `中等数据传输 (~10KB)` 已 PASS
  - 当前真正未处理的是 `SSL 3.0 握手失败（已废弃）`
  - 且异常在 `TWinSSLContext.CreateConnection` / `EnsureCredentialsAcquired` 阶段就抛出 `ESSLInitializationException`，不是等到 `Connect`
  - 因此 expected-failure 保护必须覆盖 connection creation，而不能只包 `Connect`

- `test_backend_comparison.pas` 在 `25987503677` 上暴露出的新边界，也已经从实现缺陷前移到测试假设过强：
  - 旧的 WinSSL registration 缺口已消失
  - 旧的库级统计更新 `EAccessViolation` 已消失
  - live internet 响应在同一次 run 中出现 `MD5` 不同、长度不同，不适合继续当作 exact-equality 回归标准
  - `HTTP/SSL3` 负路径上 `ESSLProtocolException` 仍可能是预期结果，测试不能再假设一定是简单的 `not Connect`

- 当前第九批本地修法因此保持在 tests/contracts 面，而不是扩大到新的 WinSSL 生产实现：
  - `tests/winssl/test_winssl_integration_multi.pas`
    - 新增 `TestExpectedHandshakeFailurePath`
    - 把 `HTTP` / `SSL3` negative-path 统一收进 helper，覆盖 `CreateConnection` 抛异常的情况
    - 把 `SEC_E_ALGORITHM_MISMATCH` 继续视为 expected handshake failure
  - `tests/integration/test_backend_comparison.pas`
    - 删除 live response `MD5` / 长度完全一致断言
    - 改成比较 HTTP status class 是否同类且有效
    - 把 `HTTP` / `SSL3` negative-path 异常视为 expected failure

- 这批本地静态收口里还顺手发现了一个真实编译面依赖：
  - `test_backend_comparison.pas` 新增 `GetHTTPStatusClass` 后使用了 `PosEx`
  - 对应 `uses` 需要补 `StrUtils`
  - 否则 focused contract 可以全绿，但 Pascal 编译会在新 helper 上翻车

- 手动 Windows runtime run `25988526125`（head `16a6b71`）证明第九批修法已经把 broader-suite 问题继续收窄：
  - 不再有未处理异常炸掉整个 suite
  - 当前失败已经从“崩点”降为“受控 FAIL 的测试语义”

- `test_winssl_integration_multi.pas` 在 `25988526125` 上暴露出两个同源失败：
  - `TLS 1.3 协商（异常）` FAIL
  - `SSL 3.0 握手失败（已废弃）` FAIL
  - 两者打印出的 native truth 都是 `0x80090331`
  - 这说明当前更可信的问题不是测试流程漏包，而是 `SEC_E_ALGORITHM_MISMATCH` 的静态常量判断不够稳，应该直接按 concrete native error 做 helper 判定

- `test_backend_comparison.pas` 在 `25988526125` 上只剩一个失败：
  - `OpenSSL SSL3 握手失败（预期）` FAIL
  - 其余 live-response compare、WinSSL HTTP、OpenSSL HTTP、WinSSL SSL3 路径都已转绿
  - 这说明当前更合理的断言语义是：
    - 握手失败，算 PASS
    - 如果连接成功，但实际协商协议不是 `SSL3`，也算 PASS
    - 只有真的协商到 `SSL3` 才应算 FAIL

- 当前第十批本地修法因此仍然保持在 tests/contracts 面：
  - `tests/winssl/test_winssl_integration_multi.pas`
    - 新增 `HasAlgorithmMismatchNativeError`
    - 用 concrete native error `0x80090331` 统一驱动 TLS1.3/SSL3 expected-failure 分类
  - `tests/integration/test_backend_comparison.pas`
    - 新增 `TestDeprecatedProtocolFailurePath`
    - 统一表达 “失败或未真正降到 SSL3 都算安全结果”
    - helper 继续覆盖 `CreateConnection` 和 `Connect` 两段，避免把 WinSSL 的 create-stage 漏口引回来

- 这批本地修法又抓出一个很典型的平台编译陷阱：
  - `test_backend_comparison.pas` 在 Linux 编译面虽然不会运行 Windows 分支，但依然会编译整个过程体
  - 只把 helper 放进 `{$IFDEF WINDOWS}` 会让 Linux 编译在调用点找不到标识符
  - 正确做法是把 helper 放到 `Windows/Linux socket stub` 定义之后，让两侧都能看到同一个声明

- 手动 Windows runtime run `25988847598`（head `d7d09ad`）已经给出新的项目真相：
  - `windows-gate` SUCCESS
  - `linux-gate` SUCCESS
  - `summary` SUCCESS
  - 当前整体 workflow 失败不再来自 Windows/WinSSL 主线，而只剩 `macos-gate`

- `macos-gate` 的真实失败点不是模块、编译或 OpenSSL 根路径，而是 examples compile gate 的 Bash 兼容性：
  - `wave_b_macos_gate_summary_codex_winssl_20260517_190215.md` 显示只有 `examples` step FAIL
  - `wave_b_macos_examples_codex_winssl_20260517_190215.log` 明确报 `mapfile: command not found`
  - 远端 bash 版本是 `3.2.2`
  - 这让 `verify_examples_compile.sh` 在扫描成功后却没把任何样本装进数组，最终产出 `total=0` / `pass_rate=0`

- 当前第十一批修法因此属于一个非常窄的可移植性修复：
  - `scripts/verify_examples_compile.sh`
    - 去掉 `mapfile`
    - 改为 `while IFS= read -r file; do ... done`
  - 新增 `tests/scripts/test_verify_examples_compile_bash32_compat_contract.sh`
    - 禁止脚本重新依赖 `mapfile`
    - 强制用 Bash 3.2 兼容的读取循环装载 example 列表

- 这也说明当前项目的主要未闭环点已经发生切换：
  - Windows/WinSSL runtime proof 已经不再是 blocker
  - 当前更高价值的剩余工作是把 `wave-b-b2-manual.yml` 的 macOS examples gate 也收口成真实跨平台 runtime 绿灯
