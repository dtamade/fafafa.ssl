# Findings - Wave B/B2 Handoff Linux Next Actions Truth

## 2026-05-14
- 继续沿着 `cross summary -> closure -> consistency -> handoff bundle` 这条链往上审后，发现顶层 `prepare_wave_b_b2_handoff_bundle.sh` 还有同型缺口：
  - 它已经能读到 `closure_report`
  - 也已经根据 macOS / Windows platform state 生成 `Next Actions`
  - 但 Linux platform state 仍完全没接进去
- 这会制造一个真实的顶层交接误导：
  - 当 Linux 是唯一阻塞平台时
  - handoff bundle 会退化成只有 replay command
  - 操作者看不到“当前真正该修的就是 Linux baseline”
- 这批最小正确修法不是改 `handoff_state`，而是把 Linux state 也接入当前顶层动作生成逻辑：
  - Linux 非 `PASS` 时显式提示修复或重跑 Linux baseline
  - macOS / Windows 已 `PASS` 时继续避免错误提示
  - replay command 继续保留，顶层 refresh 入口不变

# Findings - Wave B Cross Summary Next Actions Truth

## 2026-05-14
- 继续沿着 `cross summary -> closure -> consistency -> handoff bundle` 这一条静态报告链深审后，又发现 `generate_wave_b_cross_platform_summary.sh` 自己还有一层旧模板漂移：
  - `Next Actions` 固定只提醒 macOS / Windows
  - 不提醒 Linux
  - 并且还继续提示“重新运行本脚本”
- 这与当前 repo truth 已经明显脱节：
  - Linux baseline 早已成为 handoff 必需前提
  - 完整刷新入口已经统一收敛到 `prepare_wave_b_b2_handoff_bundle.sh`
  - 三平台全 `PASS` 时，cross summary 也不该继续提示重跑平台 lane
- 这批最小正确修法不是改 summary 的状态计算，而是把 `Next Actions` 绑定到现有状态矩阵：
  - Linux 非 `PASS` 时显式提示修复 baseline
  - macOS / Windows 非 `PASS` 时继续给出 runner 修复动作
  - 三平台全 `PASS` 时改成 aligned/optional refresh 提示
  - 报告统一指回 `prepare_wave_b_b2_handoff_bundle.sh`，不再让操作者只刷新局部摘要

# Findings - Wave B/B2 Closure Linux Next Actions Truth

## 2026-05-14
- 继续沿着 `closure` 报告面深审后，发现上一轮虽然已经把最终 rerun 入口切到 `prepare_wave_b_b2_handoff_bundle.sh`，但 `Next Actions` 仍保留了另一层旧静态假设：
  - 只提醒 macOS / Windows
  - 不提醒 Linux
  - 好像 Linux baseline 永远不会成为 closure 的当前阻塞项
- 这与仓库当前真相已经冲突：
  - Linux baseline 早已是必需前提
  - `closure_readiness` 自己也能把 Linux summary 判成 `READY` / `FAIL` / `PENDING`
  - 但报告动作面却继续把 Linux 非 `PASS` 藏起来
- 这批最小正确修法不是改 `closure_status` 判定，而是把 `Next Actions` 绑定到现有平台状态矩阵：
  - Linux 非 `PASS` 时，明确要求修复或重跑 Linux baseline
  - macOS / Windows 非 `PASS` 时，也一并改成状态驱动的有效 summary 指引
  - 最终仍统一回到 `prepare_wave_b_b2_handoff_bundle.sh` 刷新完整交接链

# Findings - Wave B/B2 Consistency Next Actions Truth

## 2026-05-14
- 继续沿着 `consistency` 报告面深审后，发现上一批修掉 closure 元数据假绿灯之后，还有一条更表层的 repo-side 误导没有收掉：
  - 当只有 Linux evidence、`closure_status_note=IN_PROGRESS` 时
  - consistency 报告会高亮 `CONSISTENT`
  - 但正文没有任何 `Next Actions`
  - 也没有说明这只表示 evidence consistency，而不代表 handoff 已闭环
- 这会制造一个真实的操作偏差：
  - 调用者看到绿色 `CONSISTENT`
  - 可能直接把这份报告当成整条 Wave B/B2 handoff 的绿灯
  - 却看不到 closure 仍未闭环、也看不到应该回到 `prepare_wave_b_b2_handoff_bundle.sh` 刷新完整交接链
- 这批最小正确修法不是改 consistency gate 本身，而是把报告面补齐到当前 handoff 真相：
  - `Next Actions` 基于 `consistency_status + closure_status_note` 动态生成
  - `IN_PROGRESS` 时明确写出 handoff 尚未闭环
  - 一律指回当前唯一真实的 prepare 入口，而不是让操作者自己猜下一步

# Findings - Wave B/B2 Consistency Closure Status Parse Truth

## 2026-05-14
- 继续沿着 `closure / consistency / handoff` 这一条静态报告链深审后，发现 `check_wave_b_b2_evidence_consistency.sh` 还有一条更隐蔽的 repo-side 假绿灯：
  - `closure_report` 只要文件存在且 `run_id` 能对上
  - 就算内部已经缺了 `closure_status`
  - consistency 报告也仍会写成：
    - `consistency_status: **CONSISTENT**`
    - `runid_mismatch_or_parse_issue: 0`
    - `closure_status_note:` 空白
- 这不是“格式不完美”的小问题，而是会把坏掉的 closure 证据伪装成绿色：
  - 上层 handoff bundle 仍会继续消费 `closure_status`
  - 但底层 consistency 已经先把这份坏报告标绿
  - 后续操作者很难从 report 表面看出 closure 元数据其实已经断裂
- 这批最小正确修法不是扩大 strict 定义，而是把现有 `runid_mismatch_or_parse_issue` 语义补齐到 closure 元数据层：
  - `closure_report` 除了 run_id 之外，还必须带有效 `closure_status`
  - 允许值保持和当前 closure script 真相一致：`IN_PROGRESS` / `CLOSED`
  - 缺失或非法时：
    - `consistency_status` 必须落到 `INCONSISTENT`
    - 顶层 `closure_status_note` 要显式写出 parse issue
    - `closure_report` 那一行也不能继续写 `ok`

# Findings - Wave B/B2 Closure Next Action Truth

## 2026-05-14
- 继续沿着 Wave B/B2 handoff 静态报告面深审后，发现 closure readiness 报告还有一条同类的 stale guidance：
  - 它仍在告诉调用者“三平台 summary 回填后，复跑 `generate_wave_b_cross_platform_summary.sh`”
  - 但现在 repo 的上层收口入口已经是 `prepare_wave_b_b2_handoff_bundle.sh`
  - workflow summary 也已经不再直接走 `generate -> closure -> consistency`
- 这会制造一个真实的操作误导：
  - 调用者照着 closure report 去复跑 `generate`
  - 只能刷新 cross summary
  - 却不会同步刷新 consistency / handoff bundle / replay guidance
- 这批最小正确修法不是改 closure 判定逻辑，而是把 closure report 的最后一步指向真实上层入口：
  - 不再提示旧的 `generate_wave_b_cross_platform_summary.sh`
  - 改成提示复跑 Wave B/B2 handoff bundle 准备流程
- 修完后 closure report 的建议动作终于和 repo 当前结构一致了：
  - closure 仍只负责表达三平台是否闭环
  - 但它不再把操作者引向一个只能刷新 cross summary 的旧入口
  - 而是把后续刷新交还给现在真正的上层收口流程

# Findings - Wave B/B2 Strict Input Description Truth

## 2026-05-14
- 继续沿着 `strict_closure=true` 的 workflow 静态边界深审后，发现输入面还有一条更直接的用户契约漂移：
  - workflow 现在会把 `strict_closure=true` 映射到 `prepare_wave_b_b2_handoff_bundle.sh --strict`
  - 这条 strict 路径已经不是单纯的 “B2 not closed”
  - 它还会因为 evidence inconsistency 或 Windows runtime artifact 缺失而失败
- 这会制造一个真实的用户误导：
  - 操作者看到的是 “Fail workflow if B2 not closed”
  - 实际运行时却可能在 `closure_status == CLOSED` 的情况下仍因 consistency strictness 失败
  - 于是输入名和失败原因之间出现语义错位
- 这批最小正确修法不是改输入名或 strict 逻辑，而是先把描述文字收口到真实语义：
  - 保留兼容性的 `strict_closure` key
  - 但 description 必须明确这是完整 handoff strict gate，而不只是 closure-only
- 修完后 workflow 的输入面终于和内部实现说的是同一件事：
  - 操作者再看到 strict 失败，就不会误以为“明明 closure 已 closed 为什么还失败”
  - live 与 `.disabled` 模板的说明也继续保持一致

# Findings - Wave B/B2 Prepare Strict Metadata Truth

## 2026-05-14
- 继续沿着 `strict_closure=true` 的 handoff 静态边界深审后，发现一个脚本级元数据漂移：
  - `prepare_wave_b_b2_handoff_bundle.sh --strict` 为了保证“先写报告、再严格失败”，主流程不会把 `--strict` 直接传给 closure/consistency 的输出生成调用
  - 但它最后又会用 `--strict --dry-run` 做真正的失败判定
  - 这使得生成出来的 closure/consistency markdown 报告把 `strict_mode` 写成了 `false`
- 这会制造一个真实的 repo-side 证据矛盾：
  - handoff bundle 明明记录当前批次是 strict
  - closure/consistency 报告却声称不是 strict
  - 读报告的人很难区分“这是宽松模式下生成的报告”还是“严格模式下先写出来的失败前快照”
- 这批最小正确修法不是把主流程改成直接 strict 生成，因为那会让脚本在写完所有报告前提前退出；正确做法是：
  - 保持现有“先生成、后严格校验”的执行顺序
  - 只在 `prepare` 里同步已经落盘的 `strict_mode` 元数据
  - 让上传出来的三份报告对当前模式保持一致
- 修完后 strict 模式的证据链更自洽了：
  - `prepare --strict` 仍会先写全交接包，再以非 0 失败
  - 但 closure/consistency/handoff bundle 三份报告现在都会明确记录 `strict_mode: true`
  - 读报告的人不再需要猜“这是宽松生成 + 严格 dry-run”还是“真实宽松模式”

# Findings - Wave B/B2 Optional Runner Artifact Download Tolerance

## 2026-05-14
- 继续沿着 `wave-b-b2-manual` workflow 的 handoff 静态契约线深审后，发现 summary job 还有一层“比 handoff 更早”的失败面：
  - 它已经把 macOS/Windows 缺证据的判定语义交给 `prepare_wave_b_b2_handoff_bundle.sh`
  - 但 `Download macOS evidence` / `Download Windows evidence` 仍是无容错 `download-artifact`
  - 这样一来，只要 artifact 根本没生成，summary 就会在进入 handoff 判定前先失败
- 这会制造一个真实的 repo-side 语义绕过：
  - 本应由 handoff bundle 产出的 `PENDING` / `READY_FOR_RUNNER` / `NEEDS_EVIDENCE_SYNC`
  - 被更前面的 artifact 下载错误直接截断
  - 结果是 workflow 没法稳定产出交接报告，只留下一个过早失败的 summary job
- 这批最小正确修法不是放宽 Linux truth，而是做分层容错：
  - Linux artifact download 继续严格，因为 Linux summary/examples 是必需前提
  - macOS / Windows artifact download 加 `continue-on-error: true`
  - 让 summary 始终有机会进入 `prepare` 链，按现有缺证据语义收口
- 修完后 summary 的失败边界终于和 handoff 语义对齐了：
  - Linux 缺失仍会在 required truth 上硬失败
  - macOS/Windows 缺失不再在 download 步骤被提前截断
  - 交接报告可以继续稳定产出，并显式表达缺哪些非 Linux 证据

# Findings - Wave B/B2 Linux Baseline Required Workflow Truth

## 2026-05-14
- 继续沿着 `wave-b-b2-manual` workflow 静态契约线深审后，发现 handoff truth source 收口之后，又暴露出一条更基础的输入层漂移：
  - workflow 仍提供 `run_linux_baseline`，暗示 Linux baseline gate 可以被安全关闭
  - 但 summary/handoff 入口已经无条件依赖 `wave_b_ci_gate_summary_<run_id>.md` 和 `examples_compile_ci_gate_<run_id>.json`
  - `prepare_wave_b_b2_handoff_bundle.sh` 也明确把 Linux summary 当作必需证据
- 这会制造一个真实的假选项：
  - 用户把 `run_linux_baseline` 设成 `false`
  - `linux-gate` 与 Linux artifact download 被跳过
  - summary job 仍继续执行，并在 `prepare` 处因为缺失 Linux summary 直接失败
- 这批最小正确修法不是给这条坏分支加更多 fallback，而是删除这条假契约：
  - Linux baseline 恢复成 workflow 的固定前提
  - live 与 `.disabled` 模板一起移除 `run_linux_baseline`
  - workflow contracts 明确禁止再出现这条可关闭分支
- 修完后 workflow 的输入面与执行面重新一致了：
  - 操作者不再看到一个事实上不能使用的 `false` 选项
  - summary/handoff 所需的 Linux truth 也不再依赖隐式默认或跳过分支
  - live 与 `.disabled` 模板仍保持完全一致

# Findings - Wave B/B2 Disabled Workflow Handoff Truth Sync

## 2026-05-14
- 继续沿着同一条 workflow/handoff 静态契约线深审后，又发现一层很容易回流旧逻辑的 repo-side 漂移：
  - live `.github/workflows/wave-b-b2-manual.yml` 已经切到 `prepare_wave_b_b2_handoff_bundle.sh`
  - 但 `.github/workflows/wave-b-b2-manual.yml.disabled` 还停在旧的 `MACOS_*ARGS` / `WINDOWS_EVIDENCE_ARGS` + `generate/closure/consistency` 直连实现
- 这不是“disabled 就可以不管”的噪音问题，而是一个真实模板漂移：
  - 这个文件本身就是 repo 内的备用/恢复模板
  - 后续一旦人工启用、复制、或对照回填，很容易把刚刚删掉的平行 handoff 逻辑重新带回去
- 这批最小正确修法不是新增第二套 disabled 专属判断，而是让合同和模板都回到一个事实：
  - workflow handoff contract 同时约束 live 与 disabled 两个模板
  - disabled 模板的 summary step 同步到 `prepare` 单一入口
  - handoff bundle artifact 上传语义也保持一致

# Findings - Wave B/B2 Workflow Handoff Truth Source

## 2026-05-14
- 继续沿着 `Wave B/B2` handoff/workflow 这一条静态契约线深审后，发现现在最大的 repo-side 漂移点已经从脚本内部，转移到了 workflow 汇总入口本身：
  - `.github/workflows/wave-b-b2-manual.yml` 的 `summary` job 仍在自己拼 `MACOS_*ARGS` / `WINDOWS_*ARGS`
  - 并且直接串联 `generate -> closure -> consistency`
  - 没有调用 `prepare_wave_b_b2_handoff_bundle.sh`
- 这会制造一个真实的维护分叉：
  - 近期对 handoff bundle 做过的 replay-command、next-actions、Windows companion artifact、explicit-missing passthrough 等修复，都只落在 `prepare` 单一入口里
  - 但 workflow summary 仍停留在旧入口，自身不会生成 handoff bundle，也不会自动继承这些更新
  - 结果就是“本地静态交接真相”和“workflow 产出的 summary truth”开始分裂
- 这批最小正确修法不是再往 workflow 里补更多判断，而是反过来删掉这层重复逻辑：
  - summary job 只负责整理下载到的证据文件
  - 然后统一调用 `prepare_wave_b_b2_handoff_bundle.sh`
  - strict 模式仅映射到 `prepare --strict`
  - 最终 artifact upload 把 handoff bundle 一并纳入
- 修完后的 repo truth 更干净了：
  - workflow summary 不再重复 handoff 语义实现
  - macOS probe fallback、Windows companion runtime artifacts、explicit missing passthrough、replay command、state-driven next actions 全都回到 `prepare` 单一入口
  - 后续若再修 handoff 逻辑，workflow 不需要再同步补一份平行实现

# Findings - Wave B/B2 Handoff Bundle Next Actions

## 2026-05-14
- 继续沿着 `prepare_wave_b_b2_handoff_bundle.sh` 的静态交接面深审后，发现 bundle 的 `Next Actions` 还有一层 stale-template 误导：
  - 不管当前 truth 如何，它都会固定输出“跑 macOS runner”“跑 Windows runner”“回填后重跑”
  - 即使 Windows 已经 PASS，甚至整个 bundle 已经 `CLOSED`，也不例外
- 这会制造一个真实的 repo-side 交接误导：
  - artifact 表和 status 区已经能看出哪些平台还缺证据
  - 但 `Next Actions` 却给出过时、甚至错误的操作建议
  - 读 bundle 的人可能会重复跑已经 green 的平台，而看不出真正缺的是 companion runtime artifacts 还是另一个平台
- 这批最小正确修法不是改 handoff_state，而是把建议动作绑定到现有 truth：
  - 缺 macOS 才提示 macOS
  - 缺 Windows summary 才提示 Windows summary/live gate
  - Windows summary 已 PASS 但 companion runtime artifacts 缺失时，提示补 Windows runtime artifacts
  - `CLOSED` 时只保留闭环完成/可选复核提示

# Findings - Wave B/B2 Handoff Bundle Replay Command

## 2026-05-14
- 继续沿着 `prepare_wave_b_b2_handoff_bundle.sh` 的静态交接面深审后，发现 handoff bundle 文档本身还有一条容易误导后续人工重跑的契约缺口：
  - artifact 表已经记录了 custom `linux_summary`、custom `linux_examples`、custom `windows_summary`、以及 custom `output-dir`
  - 但 `Next Actions` 里的重跑命令仍只写 `scripts/prepare_wave_b_b2_handoff_bundle.sh --run-id <id> --strict`
  - 这会让下一次重跑重新走默认路径推导，而不是复现当前批次的真实 evidence chain
- 这会制造一个真实的 repo-side 交接偏移：
  - 报告表面上给了“下一步怎么做”
  - 但命令本身并不携带当前批次最关键的路径上下文
  - 一旦仓库里同 run_id 对应的默认文件和 custom 路径不一致，重跑结果就会漂移
- 这批最小正确修法不是改 evidence 判定，而是让 replay command 真正基于当前批次的 top-level truth 生成：
  - 始终保留 `linux_summary`、`linux_examples`、`output-dir`
  - 对 macOS/Windows 只保留显式传入或当前 active evidence 已存在的 top-level args
  - 不把默认 no-evidence 场景错误抬升成显式 missing

# Findings - Wave B/B2 Handoff Bundle Windows Artifact List

## 2026-05-14
- 继续沿着同一条 `prepare` 静态审查线深挖后，发现 handoff bundle 自己还有一层索引面缺口：
  - `prepare_wave_b_b2_handoff_bundle.sh` 现在已经会为 Windows summary 推导 sibling `quick smoke` / `runtime suite`
  - `check_wave_b_b2_evidence_consistency.sh` 也已经会把它们当成真实 evidence
  - 但最终生成的 handoff bundle artifact 表里仍只列 `windows_summary`
- 这会制造一个真实的 repo-side 索引不完整：
  - 交接报告表面上像是完整索引
  - 但真正决定 Windows strictness 的两份 companion logs 并没有被列出来
  - 读 bundle 的人看不到这两份 artifact 是否存在，只能再去翻 consistency report
- 这批最小正确修法不是改 strict 规则，而是让 bundle index 跟上现有 truth：
  - 什么时候 `WINDOWS_EVIDENCE_ARGS` 已经激活
  - bundle artifact list 就应同步列出 quick/runtime paths
  - 缺失显示 `NO`，存在显示 `YES`

# Findings - Wave B/B2 Explicit Missing Evidence Passthrough

## 2026-05-14
- 继续沿着同一条 `Wave B/B2` shell 契约面深审后，发现还有一层更靠前的 truth drift：
  - `check_wave_b_b2_evidence_consistency.sh` 已经学会了把显式 non-Linux evidence 当成 required
  - 但 `prepare_wave_b_b2_handoff_bundle.sh` 仍只会在文件已存在时才把 `--macos-summary` / `--windows-summary` / `--macos-probe` 传给下游
  - `generate_wave_b_cross_platform_summary.sh` 就算直接收到了显式缺失 summary，也仍会写成 `no evidence`
- 这会制造一个真实的 repo-side 假绿灯/假摘要组合：
  - 调用者明确给出某个缺失的 macOS/Windows summary path
  - handoff bundle 自己的 artifact 列表能看到那条缺失路径
  - 但 cross summary 却写成 `no evidence`
  - consistency 甚至还可能因为没收到那条显式路径而保持 `CONSISTENT`
- 这批最小正确修法不是再动 consistency 本身，而是把上游 truth source 补齐：
  - `prepare` 必须保留显式缺失 evidence path，而不是因为文件不存在就丢参
  - `generate` 必须把“显式缺失”展示成 missing-file truth，而不是和“默认没提供证据”混成一个 `no evidence`
- 这样能让整条 `prepare -> cross summary -> closure -> consistency -> handoff bundle` 的报告面重新对齐：
  - 调用者显式要求检查什么，就应该从最前面的摘要开始可见
  - 不应等到更深层脚本、甚至根本等不到 consistency 才发现路径曾经被吞掉

# Findings - Wave B/B2 Consistency Explicit Artifact Requiredness

## 2026-05-13
- 继续沿着同一个 `Wave B/B2` consistency public surface 深审后，发现还有一组更直接的 CLI 契约漂移：
  - 显式 `--macos-probe` 已经会变成 required
  - 但显式 `--macos-summary` / `--windows-summary` / `--windows-quick-log` / `--windows-runtime-transcript` 现在却还能被 strict 静默放过
- 这会制造一类真实的 repo-side 假绿灯：
  - 调用者已经明确指定要核对的 evidence path
  - 报告矩阵里虽然能看到 `missing`
  - 但 `required_missing` 仍然保持 `0`，最终 `CONSISTENT`
- 这批最小正确修法不是扩大 active-path 解析，而是把显式参数语义补齐到和 `macos_probe` 同一原则：
  - 显式传入就代表“这份 evidence 应被检查”
  - 缺失时必须计入 required
  - 对 Windows 来说，显式 `windows_summary` 还应像 active Windows truth 一样激活 sibling runtime strictness
- 这样能让 strict 模式重新对齐调用者意图：
  - 不是只在 cross summary 已声明 active truth 时才严格
  - 而是显式 CLI 也能真正驱动 required evidence 判定

# Findings - Wave B/B2 Consistency Existing Report Run ID Fallback

## 2026-05-13
- 继续沿着同一个 `Wave B/B2` consistency public surface 深审后，发现上一批 `cross summary -> active Linux summary` 的 run_id 修法之下，还有一层次生噪音没收干净：
  - 如果 active custom `linux_summary` 已经缺失
  - 但现有 `cross summary` 和 `closure report` 还都在，并且自己带着真实 `run_id`
  - checker 现在仍会回退到新的时间戳
- 这会制造一个真实的 repo-side 误报放大器：
  - 缺失 active Linux summary 本来已经足够让这批证据判成 `INCONSISTENT`
  - 但 direct consistency 还会再把 `cross_summary` / `closure_report` 一起标成 `run_id mismatch`
  - 结果报告里混入了两条脚本自己制造出来的噪音，而不是单纯反映真实缺口
- 这批最小正确修法不是放宽 mismatch 规则，而是继续把默认真值对齐到现有报告链：
  - 先看 active Linux summary
  - 读不到时，再回收现有 `cross summary` / `closure report` 自己写下来的 `run_id`
  - 只有现有报告链也没有真值时，才回退到时间戳
- 这样能让 strict 失败原因保持干净：
  - 缺哪个 active evidence，就报哪个缺口
  - 不再额外把已经对齐的 summary-chain 一起污染成 mismatch

# Findings - Wave B/B2 Consistency Cross Summary Run ID Inference

## 2026-05-13
- 继续沿着同一个 `Wave B/B2` consistency public surface 深审后，发现 `run_id` 还有一层更底层的时序漂移：
  - `check_wave_b_b2_evidence_consistency.sh` 现在已经会从 `cross summary` 继承 active custom `linux_summary`
  - 但它是在 `RUN_ID` 已经定成“显式参数或当前时间戳”之后才做这一步
  - 结果就是 direct consistency 可能对着同一批现有证据，自己先制造出一个新的时间戳批次号
- 这会制造一个真实的 repo-side 假红灯：
  - 调用者只传已有的 `cross summary + closure report`
  - 汇总面其实已经承认了 active custom `linux_summary`
  - checker 却先生成新的 `RUN_ID`，随后把那份 active Linux summary、cross summary、closure report 一起记成 `run_id mismatch`
- 这批最小正确修法不是放宽 mismatch 规则，也不是让调用者强制补 `--run-id`，而是继续把默认真值对齐到 active evidence：
  - 显式 `--run-id` 仍然优先
  - 否则先看 cross summary 是否已经声明 active `linux_summary`
  - 再从那份 active Linux summary 推导真实 `run_id`
  - 只有连 active Linux truth 都缺失时，才回退到时间戳
- 这样能让 direct consistency 和同族 `generate/closure/prepare` 一样，默认围绕现有证据批次收口，而不是在已有链路里再凭空开一个新批次。

# Findings - Wave B/B2 Consistency Cross Summary Windows Summary Required

## 2026-05-13
- 继续沿着同一个 `Wave B/B2` consistency public surface 深审后，发现 Windows active-truth 还有一层 required 语义没有补完：
  - 前一批已经让 direct consistency 能继承 active custom `windows_summary` path
  - 但只要这份 active summary 随后消失，checker 仍会把它和 sibling runtime artifacts 记成 optional missing
  - 于是 strict 模式还可能给出假绿灯
- 这说明 Windows 问题其实分成了两层：
  - 第一层是 active path truth 要继承
  - 第二层是 active truth 一旦成立，required 语义也必须跟着走
- 这批最小正确修法不是改 companion-path 推导，也不是放宽 runtime strict 规则，而是把 “active custom `windows_summary` 已被 cross summary 承认” 视为 strict 事实：
  - `windows_summary` 本体成为 required evidence
  - sibling `windows_quick_log` / `windows_runtime_transcript` 也同步成为 required evidence
  - 即使 summary 文件已经缺失，也不能再退回 optional missing
- 这样能让 Windows strictness 真正变成一个闭合的 active-evidence 语义：
  - 不是只在 summary 文件刚好还在时才严格
  - 而是只要 cross summary 已承认它在用，strict 就必须到底

# Findings - Wave B/B2 Consistency Cross Summary Linux Summary Path

## 2026-05-13
- 继续沿着同一个 `Wave B/B2` consistency public surface 深审后，发现 `linux_summary` 本体也还有一层 active-truth 漂移：
  - `cross summary` 已经会明确写出 `- linux_summary: <custom path>`
  - 但 `check_wave_b_b2_evidence_consistency.sh` 之前完全不会消费这条 active Linux 事实
  - 结果就是只要默认 run-specific Linux summary 还在，consistency 就可能绕开真正正在服役的 custom Linux summary
- 这会制造一个真实的 repo-side 假绿灯窗口：
  - cross summary 可能已经承认某份 custom Linux summary 为当前真值
  - 之后那份 active custom summary 即使消失
  - consistency 仍可能继续对着默认 run-specific summary 给出 `CONSISTENT`
- 这批最小正确修法不是重新设计 run-id，也不是去改 `generate` 的输出，而是继续把 checker 的 truth source 对齐到 active evidence：
  - 显式 `--linux-summary` 仍然优先
  - 未显式传参时，先继承 cross summary 已声明的 active custom `linux_summary`
  - `linux_summary` 本来就是 required evidence，所以只要 active summary 缺失，就应直接反映为 `INCONSISTENT`
- 这样能让 `linux_summary` 和前面已经补齐的 `linux_examples_json` / `macOS summary` / `windows_summary` 保持同一条原则：
  - consistency 不该被一个默认文件名牵着走
  - 而应优先校验汇总面已经承认的 active evidence path

# Findings - Wave B/B2 Consistency Cross Summary macOS Summary Path

## 2026-05-13
- 继续沿着同一个 `Wave B/B2` consistency public surface 深审后，发现 macOS evidence 也有和 Windows 类似的一层 active-truth 漂移：
  - `cross summary` 已经会在 macOS 行写出 `summary: <custom path> (overall=...)`
  - 但 `check_wave_b_b2_evidence_consistency.sh` 之前完全不会消费这条 active macOS 事实
  - 结果就是只要 direct consistency 没显式传 `--macos-summary`，它就可能继续盯着默认路径并保持 green
- 这和 probe-only 场景不是一回事，问题恰好出在 summary truth 被静默绕开：
  - cross summary 可能已经承认 macOS `PASS`
  - 之后那份 active custom macOS summary 即使被删除或漂移
  - consistency 仍可能继续报告 `CONSISTENT`
- 这批最小正确修法不是放宽 inactive probe 边界，也不是把 summary/probe 逻辑混在一起，而是继续保持语义分层：
  - probe-only 仍走 `macos_probe`
  - active summary 则应走 `macos_summary`
  - 未显式传参时，从 cross summary 继承 active custom `macOS summary` path
- 对 strict 语义来说，这份 inherited active summary 也应被当成 required evidence：
  - 否则 cross summary 已承认的 macOS evidence 仍能在 consistency 里被静默丢掉
  - 但 inactive default probe 继续不应污染 summary 场景

# Findings - Wave B/B2 Consistency Cross Summary Windows Summary Path

## 2026-05-13
- 继续沿着同一个 `Wave B/B2` consistency public surface 深审后，发现 Windows evidence 还有比 companion-path 更深一层的 truth drift：
  - `cross summary` 已经会在 Windows 行写出 `summary: <custom path> (overall=...)`
  - 但 `check_wave_b_b2_evidence_consistency.sh` 之前完全不会消费这条 active Windows 事实
  - 结果就是只要 direct consistency 没显式传 `--windows-summary`，它就可能完全绕开 active custom Windows evidence
- 这会制造一个真实的 repo-side 假绿灯窗口：
  - cross summary 已经承认 Windows summary 存在
  - 按现有 strict 语义，这意味着 sibling `quick smoke` / `runtime suite` 应该变成 required evidence
  - 但 consistency 如果还盯着默认 `test-reports/wave_b_windows_gate_summary_<run_id>.md`，就会把这些 runtime artifacts 错当成 optional missing，最终保持 `CONSISTENT`
- 这批最小正确修法不是改 strict 规则，而是把 Windows truth source 补齐到和 `linux_examples_json` / `macos_probe` 同等级：
  - 显式 `--windows-summary` / `--windows-quick-log` / `--windows-runtime-transcript` 继续优先
  - 未显式传参时，先继承 cross summary 已声明的 active custom `windows_summary`
  - 再从这份 active summary 派生 sibling runtime artifact 路径
- 这样能让 Windows runtime strictness 真正跟着 active evidence 走，而不是跟着一个默认文件名走：
  - 有效堵住 direct consistency 绕开 active Windows evidence 的假绿灯空间
  - 同时保留默认 `test-reports/...` 主路径行为不变

# Findings - Wave B/B2 Consistency Windows Companion Path

## 2026-05-13
- 继续沿着同一个 `Wave B/B2` consistency public surface 深审后，发现 Windows runtime artifacts 还有一条 direct entrypoint 没对齐：
  - `prepare_wave_b_b2_handoff_bundle.sh` 之前已经会在 custom `windows_summary` 场景下按 sibling path 推导 `quick smoke` / `runtime suite`
  - 但 `check_wave_b_b2_evidence_consistency.sh` 本体仍把这两份 artifact 默认死写在 `test-reports/...`
- 这会制造一个真实的 repo-side 假红灯：
  - custom `windows_summary` 明明已经存在
  - 同目录 sibling `winssl_quick_smoke_<run_id>.log` / `winssl_runtime_suite_<run_id>.log` 也已经存在
  - direct consistency 却仍会因为去错目录而在 strict 模式下失败
- 这批最小正确修法不是改 Windows strict 规则，而是把 companion-path 真值来源对齐到 `windows_summary`：
  - 显式传入 `--windows-quick-log` / `--windows-runtime-transcript` 仍然优先
  - 未显式传参时，默认 sibling path 跟随 `windows_summary`
  - 默认 `test-reports/...` 主路径因为本来就同目录，所以行为保持不变
- 这样能让 direct consistency 和 handoff entrypoint 的 Windows companion 语义重新对齐：
  - 严格性不下降
  - 只是停止对已经存在的 sibling evidence 制造假缺失

# Findings - Wave B/B2 Consistency Cross Summary Linux Examples Path

## 2026-05-13
- 继续沿着同一组 `Wave B/B2` consistency public surface 深挖后，发现 `linux_examples_json` 还有更深一层 truth drift：
  - `cross summary` 会明确写出当前实际使用的 `- linux_examples_json: ...`
  - 但 `check_wave_b_b2_evidence_consistency.sh` 之前完全不会消费这条 active-path 事实
  - 结果就是只要默认 generic JSON 还在，consistency 就可能绕过真正正在服役的 custom JSON
- 这不是单纯的“默认路径不一致”，而是一扇真实的假绿灯窗口：
  - cross summary 可能是拿 custom `linux_examples_json` 生成出来的
  - 之后这份 active custom JSON 即使损坏
  - consistency 仍可能继续对着旧 generic JSON 给出 `CONSISTENT`
- 这批最小正确修法不是扩大 CLI，也不是去改 `generate/prepare` 的输出格式，而是把 consistency 的 truth source 补齐：
  - 显式 `--linux-examples` 仍然优先
  - 未显式传参时，先继承 cross summary 已声明的 active path
  - 只有 cross summary 没提供时，才回退到现有的 run-specific > generic 默认规则
- 这样能把 `linux_examples_json` 的逻辑拉到和 `macos_probe` 同一层级：
  - evidence consistency 不只是看“某个默认文件是否存在”
  - 而是优先校验当前汇总面已经承认的 active evidence path

# Findings - Wave B/B2 Consistency Generic Linux Examples Fallback

## 2026-05-13
- 继续沿着同一组 `Wave B/B2` public script surface 做静态深审后，发现 `linux_examples_json` 还有一条 direct entrypoint 语义没有对齐：
  - `generate_wave_b_cross_platform_summary.sh` 已经支持“run-specific 优先，generic fallback”
  - `prepare_wave_b_b2_handoff_bundle.sh` 也已经沿用同一规则
  - 但 `check_wave_b_b2_evidence_consistency.sh` 仍默认只认 `test-reports/examples_compile_ci_gate_<run_id>.json`
- 这会制造一个真实的 repo-side 假不一致：
  - cross summary 明明已经消费了 `test-reports/examples_compile_ci_gate.json`
  - 直接跑 consistency 却仍把 linux examples 记成 missing
  - strict 模式因此错误返回非 0
- 这批最小正确修法不是回退前面 run-specific 优先的策略，也不是去改 producer 产物命名，而是把 direct consistency 入口补齐到同一默认规则：
  - 显式 `--linux-examples` 继续优先
  - 否则先找 `examples_compile_ci_gate_<run_id>.json`
  - 缺失时再回退到旧 generic `examples_compile_ci_gate.json`
- 这样能保持当前仓库对“新路径优先、旧路径兼容”的统一语义：
  - run-specific 证据仍然是首选真值
  - direct consistency 不再因为 generic fallback 仍在服役而制造假红灯

# Findings - Wave B/B2 Ignore Inactive macOS Probe Consistency

## 2026-05-13
- 继续沿着同一条 `Wave B/B2` macOS probe consistency 链做静态深审后，发现上一批修法又露出一个相反方向的 repo-side 假红灯：
  - `check_wave_b_b2_evidence_consistency.sh` 已经能在 probe-only 场景下显式列出 `macos_probe`
  - 但它仍会在“macOS summary 已是权威证据”的场景里，仅因默认路径下刚好存在 `wave_b_macos_gate_probe_<run_id>.json` 就继续跟踪 probe
  - 一旦这份默认 probe 是损坏 JSON，strict consistency 就会被错误打成 `INCONSISTENT`
- 这说明上一批把 probe 证据接进 consistency 的修法还缺一条边界：
  - active probe-only 场景应该显式跟踪 `macos_probe`
  - authoritative macOS summary 场景则不应再被旁边一个无关默认 probe 文件污染
- 这批最小正确修法不是删掉 `--macos-probe` 或 probe-only 推断，而是只收紧跟踪条件：
  - 显式 `--macos-probe` 继续有效
  - cross summary 明确给出 `PROBE_ONLY/PROBE_OK` 时继续跟踪
  - 仅仅因为默认路径文件存在，不再自动把它列入 artifact matrix
- 这批 focused RED 还顺手抓出了一个测试层误报：
  - 新 contract 第一版用 `rg "macos_probe"` 检查报告里是否出现 probe 行
  - 但这批的 `run_id=consistency_ignore_inactive_macos_probe` 本身就会把同名子串带进路径里
  - 因而在生产脚本已正确忽略 inactive probe 后，contract 仍会假失败
- 对应地，这批测试面的最小修法也要保持收口：
  - 否定断言改成精确匹配表格行 `| macos_probe |`
  - 防止后续静态审查再被路径/文件名里的同名子串误导

# Findings - Wave B/B2 macOS Probe Consistency Hardening

## 2026-05-13
- 在补完 handoff/workflow 的 macOS probe fallback 之后继续静态深审，发现 consistency report 仍然保留一个证据盲区：
  - cross summary 已经能显示 `macos = PROBE_ONLY/PROBE_OK`
  - handoff bundle 也已经会把 probe 列入 artifact list
  - 但 `check_wave_b_b2_evidence_consistency.sh` 仍完全不知道这份 probe JSON
- 这会制造一种“汇总面承认了 probe，审计面却看不见 probe”的 repo-side 假完整：
  - handoff bundle 的 cross summary 说 macOS 证据来自 probe
  - consistency 矩阵却不列出 `macos_probe`
  - workflow summary job 也不会把 probe 传给 consistency 检查
- 最小正确修法不是让 closure readiness 支持 probe，而是继续保持语义分层：
  - cross summary 可以消费 macOS summary 或 macOS probe
  - consistency 需要知道 macOS probe 是否存在且 JSON 合法
  - closure readiness 仍只以 macOS summary 为闭环依据
- 这批因此需要第三组参数面：
  - `MACOS_CROSS_ARGS` 负责 cross summary
  - `MACOS_SUMMARY_ARGS` 负责 closure/evidence 里的 macOS summary 传递
  - `MACOS_CONSISTENCY_ARGS` 只在 probe-only 场景下把 probe 传给 consistency

# Findings - Wave B Cross Summary macOS Probe Default Hardening

## 2026-05-13
- 在刚补完 handoff/workflow 的 macOS probe fallback 之后继续静态续审，发现 direct public entrypoint 还留着一半旧语义：
  - `generate_wave_b_cross_platform_summary.sh` 已经支持显式 `--macos-probe`
  - 但如果调用者不传这个参数，即使 run-specific `wave_b_macos_gate_probe_<run_id>.json` 已经存在，脚本仍会把 macOS 记成 `PENDING / no evidence`
- 这说明上一批修法虽然收住了 handoff 与 workflow 汇总面，但 direct script surface 仍然不完整：
  - 同一批仓库证据在 `prepare`/workflow 下能看到 `PROBE_ONLY`
  - 直接跑 `generate` 却还会丢掉 probe-only 证据
  - 对外表现成同一公共脚本族内部默认行为不一致
- 这批最小正确修法就是把 default-path truth 补到 `generate` 自己身上：
  - run_id 一旦确定
  - `MACOS_PROBE` 未显式传入时，就默认指向 `test-reports/wave_b_macos_gate_probe_<run_id>.json`
  - 仍保持 macOS summary 优先，不扩大到 closure/evidence 语义变更

# Findings - Wave B/B2 macOS Probe Fallback Hardening

## 2026-05-13
- 继续静态深审 `Wave B/B2` 脚本链时，发现 macOS probe-only 证据在 handoff 入口和 workflow 汇总入口里同时被丢弃：
  - `generate_wave_b_cross_platform_summary.sh` 明确支持 `--macos-probe`
  - 但 `prepare_wave_b_b2_handoff_bundle.sh` 和 `.github/workflows/wave-b-b2-manual.yml` 都只会传 `--macos-summary`
  - 因而一旦 runner 只有 `wave_b_macos_gate_probe_<run_id>.json`、还没有 summary，cross summary 就会把 macOS 错误降成 `PENDING / no evidence`
- 这个缺口不是文案问题，而是 repo-side 真实证据损失：
  - docs/manifest 早就把 `PROBE_ONLY` 作为有效过渡状态
  - 但 handoff bundle 和 workflow summary 阶段却没有把 probe 接进来
  - 结果就是已有 probe 的阶段性证据在最终汇总里被静默抹掉
- 最小正确修法不是扩 closure/evidence 的语义，而是先把参数面拆清楚：
  - cross summary 可以消费 `--macos-summary` 或 `--macos-probe`
  - closure readiness / evidence consistency 仍只应消费 macOS summary
  - 因此需要分离 `MACOS_CROSS_ARGS` 与 `MACOS_SUMMARY_ARGS`，而不是复用一组 `MACOS_ARGS`
- 这批回归时实际还抓到了一个很值钱的二次缺口：
  - 直接把 `--macos-probe` 塞进旧的共享参数数组会让 `closure/evidence` 报 `Unknown option: --macos-probe`
  - 说明这类“一个新证据入口横穿多脚本”的改动必须显式区分参数消费面，不能只在入口多加一个 option

# Findings - Wave B Cross Summary Run ID Help Sync

## 2026-05-13
- 在刚修完 `Wave B/B2` run_id 继承逻辑后继续做静态扫尾，发现还有一条轻量但真实的契约漂移：
  - `generate_wave_b_cross_platform_summary.sh` 的 `--help` 仍宣称 `--run-id` 默认值是“时间戳”
  - 这和刚落地的实际行为已经不一致，因为脚本现在会优先从 Linux summary 推导 run_id
- 这不是运行时 bug，但会误导后续人工调用与静态审查：
  - 读 help 的人会误以为 omitted `--run-id` 必然创建新批次
  - 实际上当前脚本已经按 Linux summary run_id 对齐
- 最小正确修法就是文案同步，不应顺手再动逻辑或输出格式。

# Findings - Wave B/B2 Infer Run ID From Linux Summary

## 2026-05-13
- 继续静态深审 `Wave B/B2` 脚本链时，发现刚修好的 Linux examples 默认路径之外还有一条更底层的 shared drift：
  - 多个脚本都支持“自动选最新 Linux summary”或“显式传 Linux summary”
  - 但在未显式传 `--run-id` 时，它们仍会各自生成新的时间戳 run_id
  - 这会把同一批 handoff 证据拆成“Linux summary 的旧 run_id”和“新生成报告的当前时间戳 run_id”
- 这个缺口不是理论问题，而是会直接污染 repo-side 默认调用语义：
  - `prepare_wave_b_b2_handoff_bundle.sh` 看起来像是在为现有 Linux 证据生成闭环包
  - 实际却会把 cross summary / closure / consistency / bundle 统统命名成新的 run_id
  - consistency 随后再把 linux_summary 标成 run_id mismatch，形成自造的 `INCONSISTENT`
- 最小正确修法不是让 evidence consistency 放宽校验，而是把 run_id truth source 对齐：
  - 显式 `--run-id` 仍然优先
  - 未显式传参时，Linux summary 的 `- run_id:` 才应该是默认真值
  - 只有连 summary run_id 都读不到时，才回退到时间戳
- 这次 focused 合同证明修法覆盖到了完整 handoff 链，而不是只修单个脚本的输出名：
  - cross / closure / consistency / bundle 四份产物都会继承 Linux summary run_id
  - 在只有 Linux 证据但内部对齐的场景下，consistency 重新回到 `CONSISTENT`

# Findings - Wave B/B2 Run-Specific Linux Examples Default Hardening

## 2026-05-13
- 继续静态深审 `Wave B/B2` 脚本链时，发现 Linux examples JSON 默认路径还残留一条内部不一致：
  - `scripts/check_wave_b_b2_evidence_consistency.sh` 已经默认使用 `test-reports/examples_compile_ci_gate_<run_id>.json`
  - 但 `scripts/generate_wave_b_cross_platform_summary.sh` 与 `scripts/prepare_wave_b_b2_handoff_bundle.sh` 仍可能先吃旧 generic `test-reports/examples_compile_ci_gate.json`
- 这个缺口会制造一种很隐蔽的 repo-side 假一致：
  - run-specific JSON 明明已存在
  - handoff bundle 却仍可能引用 generic JSON
  - 最终 cross summary / consistency report 看上去完整，但实际消费的是旧批次 residue
- 最小正确修法不是强制全链只认 run-specific，也不是现在就重写 `run_wave_b_ci_gate.sh`：
  - 仓库里仍有 generic JSON 的历史与本地默认产物路径
  - 因此当前安全策略应是“run-specific 优先，generic fallback”
  - 这样能先堵住 stale-generic 漂移，同时保持现有非 workflow 调用可兼容
- focused contract 说明这次修法真正压到了 handoff 链路，而不只是单脚本 dry-run：
  - prepare 生成的 cross summary 会改写 `linux_examples_json`
  - consistency report 也会同步跟踪 run-specific JSON 路径

# Findings - Wave B/B2 Handoff Bundle Windows Companion Path Hardening

## 2026-05-13
- 继续静态深审 `Wave B/B2` 脚本链时，发现 `prepare_wave_b_b2_handoff_bundle.sh` 仍保留了一条旧的路径假设：
  - 只要 `windows_summary` 存在，它就会开启 Windows runtime artifact 校验
  - 但默认 companion 路径仍死写为 `test-reports/winssl_quick_smoke_<run_id>.log` 与 `test-reports/winssl_runtime_suite_<run_id>.log`
  - 这和前面已经补好的“支持自定义 summary 路径 / absolute path”语义不一致
- 这个缺口会制造一种很误导的 repo-side 假失败：
  - 调用者明明提供了自定义 `windows_summary`
  - sibling quick log / runtime transcript 也真实存在于同目录
  - handoff bundle 仍会因为去错目录而产出 `INCONSISTENT`
- 最小正确修法不是继续扩 CLI，而是先把默认推导补齐到更一致的静态语义：
  - 默认 companion artifacts 跟随 `windows_summary` 同目录
  - 因而默认 `test-reports/...` 行为在仓库现有主路径上完全保持不变
  - 只有自定义 summary 路径时，才获得更合理的 sibling-log 推导
- 回归证明这批修法没有破坏已有收口：
  - handoff bundle 的自定义 companion-path contract 变绿
  - 之前的 absolute-output contract 继续通过
  - evidence consistency 本体 contract 继续通过

# Findings - Wave B Cross-Platform Summary Absolute Input Hardening

## 2026-05-13
- 在刚修完 absolute output 之后继续静态审查，发现 `generate_wave_b_cross_platform_summary.sh` 还有一条同族缺陷：
  - 输出路径已经能走 absolute path
  - 但输入读取仍大量写死为 `"$PROJECT_ROOT/$SOME_PATH"`
  - 因此 absolute `--linux-summary` / `--linux-examples` / `--macos-summary` / `--windows-summary` 在跨目录调用时会被误判为不存在
- 这个缺陷在入口就能打出真实 RED，而不是“报告内容有点不对”那种软漂移：
  - absolute `--linux-summary` 直接卡死在 `Linux summary not found`
  - 后续 JSON、macOS summary、Windows summary 读取也都存在同类风险
- 最小正确修法仍然是路径归一化，而不是重做状态机：
  - 保留原有 option surface、原有输出格式、原有 checklist 计算
  - 只把输入读取统一切到 `*_ABS` 变量
- 回归结果说明这次修法没有破坏旧契约：
  - 相对输入路径的三个已有 cross-platform summary contract 继续通过
  - 上一批 absolute output contract 也继续通过

# Findings - Wave B/B2 Absolute Output Path Hardening

## 2026-05-13
- 静态续审这组 `Wave B/B2` 脚本时，发现了一个纯 repo-side 的路径语义缺陷，和运行平台无关：
  - 三份报告脚本都公开支持 `--output FILE`
  - 但真正写文件时却统一用了 `"$PROJECT_ROOT/$OUTPUT_FILE"`
  - 这导致 absolute `--output` 并不会写到调用者指定的位置，而会被错误镜像到仓库根下的伪路径
- 这个缺陷不是只影响单脚本调用，而是会向上污染 `prepare_wave_b_b2_handoff_bundle.sh` 的 absolute `--output-dir` 语义：
  - handoff bundle 自己会正确创建 absolute 输出目录
  - 但子脚本若把 absolute report path 再拼一次 `PROJECT_ROOT`，交接链就会出现“脚本提示成功、目标绝对路径却没有文件”的假成功
- 这批最小正确修法不是重做整套 path API，而是只把报告写出点归一化：
  - `generate_wave_b_cross_platform_summary.sh` 新增本地 `resolve_path(...)`
  - `check_wave_b_b2_closure_readiness.sh` 与 `check_wave_b_b2_evidence_consistency.sh` 复用已有 `resolve_path(...)`
  - 只修 `OUTPUT_FILE` 的最终写出与目录创建，不顺手扩大到输入路径重构
- focused contract 也证明了这次修法覆盖到了完整链路，而不只是单文件写出：
  - 从 `/tmp` 执行
  - 相对输入仍按项目根解析
  - absolute output / absolute output-dir 现在都能真正落到目标位置

# Findings - Wave B/B2 Windows Runtime Evidence Consistency Hardening

## 2026-05-13
- 延续上一个 `WinSSL` Windows-runtime handoff 结论后，当前最高价值的 repo-side 风险不在 Linux 绿线，而在最终证据闭环的“假完整”空间：
  - Windows job 明明已经生成 quick smoke 与 broader runtime suite transcript
  - 但 final summary / consistency 路径只显式围绕 `wave_b_windows_gate_summary_<run_id>.md`
- 这个缺口已经可以从现有仓库事实直接证明：
  - `.github/workflows/wave-b-b2-manual.yml` 的 summary 阶段只构造 `WINDOWS_ARGS=(--windows-summary ...)`
  - `scripts/check_wave_b_b2_evidence_consistency.sh` 只认识 `windows_summary`，并把它当成一个可选 markdown artifact
  - 脚本对 `winssl_quick_smoke_<run_id>.log` / `winssl_runtime_suite_<run_id>.log` 完全无感
- 因而 strict consistency 目前存在真实的误判窗口：
  - 只要 `windows_summary`、cross summary、closure report 存在且 run_id 看起来一致
  - 即使 quick smoke / runtime transcript 丢失，最终 consistency 仍可能保持 `CONSISTENT`
- 这批最小正确修法不是重新设计 Wave B/B2 全链路，而是：
  - 让 evidence consistency 显式纳入这两份 Windows runtime artifacts
  - 并在 `windows_summary` 存在时把它们提升为 required evidence
  - 同时把 workflow / handoff bundle 的参数链写清楚，避免未来再次只传 `--windows-summary`
- 修复后，Windows 证据链的 repo-side truth 更接近真实验收口径了：
  - `check_wave_b_b2_evidence_consistency.sh --strict` 现在会在 `windows_summary` 已存在但 quick smoke / runtime transcript 缺失时返回非 0
  - handoff bundle 本地 smoke 也已证明：缺这两份 artifact 时，不会再生成“看起来一致”的假闭环，而会明确落成 `consistency_status: INCONSISTENT`
- 这批有意不扩大到 `cross summary` 或 `closure readiness` 的职责重构：
  - cross summary 继续负责平台门禁摘要
  - closure readiness 继续负责三平台 summary 状态
  - runtime artifact 完整性则由 evidence consistency 作为最终 strict gate 收口
- `.github/workflows/wave-b-b2-manual.yml.disabled` 与 live workflow 也同步更新，避免下次从模板恢复时把这次修复回退掉

# Findings - Internal Context ServerName Warning Quarantine

## 2026-05-13
- 连续复跑 `FreePascal` verify-flags / session-resumption / CertificateVerify runtime 套件后，没有打出新的运行时红灯，因此本轮继续深审时不应硬造新的 runtime 问题。
- 但 focused 编译 `tests/test_builder_integration.pas` 暴露了一个真实且可重复的质量问题：
  - `factory`
  - `context builder`
  - `OpenSSL connection`
  - `OpenSSL backed config application`
  这 4 处内部兼容路径仍会稳定打印 deprecated `context-level ServerName` warning
- 这些 warning 不是用户代码误用，而是仓库自己为了保留 v1.x 兼容语义主动走了旧 API：
  - `TSSLConfig.ServerName`
  - `TSSLContextBuilder.WithSNI(...)`
  - `TOpenSSLConnection` 从 context fallback 继承默认 `ServerName`
- 因此这批的最小正确修法不是删除兼容行为，而是给这些“有意为之”的内部调用面补局部 warning quarantine：
  - 只在调用点 `{$PUSH}{$WARN 6058 off}{$WARN SYMBOL_DEPRECATED OFF}`
  - 调用后立刻 `{$POP}`
  - 不扩大到周边逻辑，也不改变 runtime SNI 语义
- 新增 focused compile contract 后，warning 问题被真正锁成了可回归的仓库事实，而不是口头约定：
  - 编译 `tests/test_builder_integration.pas`
  - 明确禁止这 4 个文件再打印 deprecated `ISSLContext.Get/SetServerName` warning

# Findings - WinSSL Pre-Handshake Verify Status Clarification

## 2026-05-12
- Fresh continuation review found the next cross-backend verify-status drift in `TWinSSLConnection` was subtler than the OpenSSL / WolfSSL / MbedTLS false-positive pattern:
  - a fresh WinSSL connection already tended to degrade `GetVerifyResult` to `-1`
  - but `DoGetVerifyResultString` mapped that pre-handshake state to `Certificate not available`
  - this publicly conflated “尚未验证” with “证书缺失/不可用”
- The right boundary here is the WinSSL handshake state machine, not a broader validation redesign:
  - `sslHsNotStarted` / `sslHsInProgress` mean verification has not finished yet
  - those states should therefore surface `-1 / Not verified`
  - `sslHsFailed` and `sslHsCompleted` should keep using the existing role-resolved validation path so real verification failures remain visible
- This batch intentionally stayed Linux-safe:
  - the RED proof is a focused source contract rather than fake runtime on a non-Windows host
  - Win64 cross-compiles plus the repo compile gate were enough to prove the narrow getter change did not break the WinSSL surface

# Findings - MbedTLS Pre-Handshake Verify Status Clarification

## 2026-05-12
- Fresh continuation review found the earlier MbedTLS helper-loss guard had not fully closed the public false-positive path:
  - `DoGetVerifyResult` still queried `mbedtls_ssl_get_verify_result(FSSLContext)` on a fresh connection
  - `DoGetVerifyResultString` still mapped `flags = 0` to `OK`
  - therefore a never-handshaken MbedTLS connection could still surface `0/OK`
- The new pre-handshake boundary is the stronger truth and should win ahead of helper-loss fallback:
  - before `FHandshakeComplete`, verify status has not been established yet
  - so the public contract must degrade to `GetVerifyResult = -1` and `GetVerifyResultString = Not verified`
- Preserving the older helper-loss contract required a test seam, not weaker production ordering:
  - once the pre-handshake guard was added, a fresh connection no longer reached the helper-loss string fallback
  - a test-only subclass that marks `FHandshakeComplete := True` is the narrowest safe way to keep asserting post-handshake helper-loss degradation
- The smallest safe repair stays narrowly scoped:
  - pre-handshake short-circuits to `-1 / Not verified`
  - completed-handshake helper-loss still degrades to `Verification status unavailable`
  - no handshake or certificate-validation redesign was needed

# Findings - WolfSSL Pre-Handshake Verify Status Clarification

## 2026-05-12
- Fresh continuation review found the same verify-status false-positive pattern also existed in `TWolfSSLConnection`:
  - constructors initialized `FLastNativeError := 0`
  - `DoGetVerifyResult` returned `FLastNativeError` whenever `FHandshakeComplete` was false
  - `DoGetVerifyResultString` treated `0` as `OK`
  - therefore a fresh never-handshaken connection could publicly surface `0/OK`
- The smallest safe repair is again a pre-handshake guard, not a larger WolfSSL verification redesign:
  - native error still wins when a real verify/handshake error exists
  - but no error plus no completed handshake must degrade to `-1 / Not verified`
  - completed successful handshakes keep the current `0 / OK` truth

# Findings - OpenSSL Pre-Handshake Verify Status Clarification

## 2026-05-12
- Fresh continuation review found the same verify-status false-positive pattern had not been fully closed in `TOpenSSLConnection`:
  - `DoGetVerifyResult` delegated straight to `SSL_get_verify_result(...)`
  - `DoGetVerifyResultString` turned `X509_V_OK` into `OK`
  - a freshly constructed stream connection could therefore surface `0/OK` before any handshake had happened
- The already-landed helper-loss guard did not cover this path:
  - helper-loss correctly degraded to `-1`
  - but normal helper availability still allowed pre-handshake success leakage
- The smallest safe repair is a pre-handshake guard, not a larger OpenSSL verification redesign:
  - before `FHandshakeComplete`, `GetVerifyResult` should stay unavailable (`-1`)
  - before `FHandshakeComplete`, `GetVerifyResultString` should stay `Not verified`
  - once handshake completes, existing helper-based success/failure mapping remains intact

# Findings - FreePascal Verify Result Status Clarification

## 2026-05-12
- Fresh post-commit review found a concrete verify-status drift in `TFreePascalConnection` getter semantics:
  - `DoGetVerifyResult` returned `0` whenever `FLastErrorCode = sslErrNone`
  - `DoGetVerifyResultString` returned `Not verified` whenever `FLastErrorString = ''`
  - a fresh never-handshaken connection therefore exposed a contradictory public state: integer success plus `Not verified`
- The same getter split also poisoned successful trusted handshakes:
  - after a clean handshake, `FLastErrorCode` still stayed `sslErrNone`
  - but no success string was ever written, so `GetVerifyResultString` continued to say `Not verified`
- The smallest safe repair is to key the getter boundary off `FHandshakeComplete`:
  - no handshake and no verify error => `GetVerifyResult = -1`, `GetVerifyResultString = Not verified`
  - handshake complete and no verify error => `GetVerifyResult = 0`, `GetVerifyResultString = OK`
  - existing failure paths keep surfacing actual `FLastErrorCode` / `FLastErrorString`

# Findings - MbedTLS Verify Result Helper Guard

## 2026-05-12
- Fresh continuation review found a narrow but user-visible false-positive path in `TMbedTLSConnection`:
  - `DoGetVerifyResult` initialized `Result := 0`
  - when `FSSLContext = nil` or `mbedtls_ssl_get_verify_result` was unavailable, the method exited immediately
  - that exposed public `GetVerifyResult = 0` even though verification status had not been queried at all
- `DoGetVerifyResultString` drifted the same way:
  - helper/context loss returned an empty string
  - callers therefore got neither a failure code nor a usable diagnostic
- The smallest safe repair is a guard-style degradation, not a verification redesign:
  - helper loss should map to a non-success sentinel (`-1`)
  - the string getter should expose a stable unavailable diagnostic
  - this keeps the batch aligned with existing verify-result guard patterns in other backends without touching handshake or certificate-validation flow

# Findings - WinSSL sslCtxBoth Verification Role Clarification

## 2026-05-12
- Fresh post-commit review found the next `sslCtxBoth` public gap had moved into WinSSL's certificate-validation path:
  - explicit `Connect` already means client handshake
  - explicit `Accept` already means server handshake
  - but `ValidatePeerCertificate(...)` still derived verification role from `FContext.GetContextType`
- That made dual-context verification drift concrete:
  - explicit `Connect` on `sslCtxBoth` could skip client-side hostname verification
  - explicit `Connect` on `sslCtxBoth` could also treat peer certificate presence as optional unless `sslVerifyFailIfNoPeerCert` was set
  - explicit `Accept` on `sslCtxBoth` could feed the wrong `AUTHTYPE_*` into `CERT_CHAIN_POLICY_SSL`
  - `DoGetVerifyResult` / `DoGetVerifyResultString` had the same role-source bug because they recomputed verification through the same role-less path
- The smallest safe repair was not a full dual-role state machine:
  - add a narrow connection-local "peer validation role" truth source
  - record it from explicit `Connect` / `Accept`
  - reuse it in `ValidatePeerCertificate(...)` and verify-result getters
  - keep the broader handshake-state design untouched
- Win64 cross-compile was important evidence here:
  - it not only revalidated the changed WinSSL compile surface
  - it also caught a fresh Pascal `if ... then ... else` semicolon slip during landing, which was fixed before closing the batch

# Findings - sslCtxBoth Roleless Handshake Clarification

## 2026-05-12
- Fresh post-commit review found a second `sslCtxBoth` truth gap one layer deeper than the client-capability batch:
  - `ISSLConnection.DoHandshake` is a public non-blocking entrypoint with no client/server parameter
  - but current backends still silently infer a role from `ContextType`
  - that makes `sslCtxBoth` inherently ambiguous on this API surface
- This ambiguity was not theoretical on the current host:
  - FreePascal / OpenSSL / MbedTLS dual-context `DoHandshake` all failed without a clear configuration boundary
  - WolfSSL dual-context `DoHandshake` even stayed in progress instead of surfacing a caller error
  - OpenSSL stream `Read/Write` also contained a hidden role-less handshake path when the connection was still disconnected
- The right repair for this batch is fail-fast, not another hidden default:
  - explicit `Connect` still means client
  - explicit `Accept` still means server
  - but role-less `DoHandshake` and OpenSSL's disconnected implicit stream handshake do not have enough information for `sslCtxBoth`
  - therefore the safe public contract is `sslErrConfiguration` with a message telling callers to choose `Connect` or `Accept` explicitly
- This batch intentionally stopped short of introducing a new per-connection role state machine.

# Findings - sslCtxBoth Client Capability Clarification

## 2026-05-12
- Fresh deep review found that `sslCtxBoth` is not merely under-tested; multiple runtime connection units still treat it as "not a client" in places where the public enum semantics imply client capability.
- The drift is currently concrete and reproducible on this Linux host:
  - FreePascal / OpenSSL / WolfSSL / MbedTLS stream connections created from `sslCtxBoth` contexts do not inherit the context fallback `ServerName`
  - FreePascal socket connections created from `sslCtxBoth` contexts also lose that fallback
  - FreePascal / OpenSSL `ISSLEarlyDataConnection.SetEarlyData(...)` reject `sslCtxBoth` immediately with `Early data is only available on client connections`
- The surrounding code shape explains why:
  - builder/factory/helper layers already treat `sslCtxBoth` as both-capable for client-scoped and server-scoped early-data configuration
  - but several connection units still gate client behavior on strict `GetContextType = sslCtxClient`
  - WolfSSL also gates pre-handshake client/server OCSP stapling preparation on strict equality, so the same pattern can leak into other client/server scoped connection behavior
- The narrowest safe repair for this batch is therefore capability-based connection gating:
  - client-capable checks should accept `sslCtxClient` and `sslCtxBoth`
  - server-capable checks should accept `sslCtxServer` and `sslCtxBoth`
  - this batch still does not need a broader redesign of implicit handshake role selection

# Findings - Early-Data Context Scope Clarification

## 2026-05-12
- Fresh deep review confirmed the broader mixed-scope early-data issue is real, but the right repair is not the same as the replay-store batch.
- `TSSLContextBuilder` and `TSSLConfig` are structurally combination objects:
  - they can legitimately carry both client early-data defaults and server early-data defaults at the same time
  - existing clone/import/export/shared-default usage already leans on that combined shape
- The actual bug was in context creation/application, not in the combined shape itself:
  - `BuildClient` and factory client paths were also applying `ServerEarlyDataPolicy` / `ServerMaxEarlyDataSize`
  - `BuildServer` and factory server paths were also applying `ClientEarlyDataEnabled`
  - `TSSLHelper.ConfigureServerEarlyData(...)` mutated client contexts
  - `TSSLHelper.ConfigureClientEarlyData(...)` mutated server contexts
- Runtime-source review narrowed the real consumption points:
  - client connections consult `GetClientEarlyDataEnabled`
  - server-side FreePascal early-data accept/ticket issuance consult `GetServerEarlyDataPolicy` / `GetServerMaxEarlyDataSize`
  - so opposite-side values on a concrete client/server context are leakage, not meaningful runtime truth
- The narrowest safe repair was therefore scope-aware application, not fail-fast:
  - keep builder/config as combined carrier objects
  - only apply the client subset when creating `sslCtxClient`
  - only apply the server subset when creating `sslCtxServer`
  - apply both only for `sslCtxBoth`
  - make the public helper methods respect the same context-type boundary
- Neighbor regression reruns exposed a separate test-quality issue:
  - `tests/test_factory_config_early_data_isolation.pas` reused fixed manual session labels when probing a default persistent replay-ledger
  - repeated local reruns could therefore be polluted by stale replay truth
  - switching those labels to per-run unique values hardened the regression without changing production behavior

# Findings - Client Replay-Store Scope Clarification

## 2026-05-12
- Deep continuation review narrowed the next real early-data drift to the most explicit server-only opt-in, not the broader client/server early-data state surface:
  - docs and public examples present `ServerEarlyDataReplayStoreFile` / `ServerEarlyDataReplayStoreDirectory` as FreePascal server-context opt-ins
  - but `ValidateClient`, `BuildClient`, `TryBuildClient`, factory default-config client creation, and factory one-shot client creation all silently accepted those fields
  - the client paths then dropped the replay-store request on the floor because no installer runs for client contexts
- That made the public contract dangerously misleading:
  - operators could believe a replay-store file/directory was in force
  - client builds would still succeed with zero warning
  - shared default configs could carry a server-only opt-in that client default-path creation silently ignored
- The narrowest safe repair was to tighten only the boundary where the misleading acceptance happened:
  - client builder validation now reports both replay-store fields as invalid
  - `BuildClient` / `TryBuildClient` now fail fast on both fields
  - factory client paths now raise `ESSLConfigurationException` instead of silently discarding the fields
  - server replay-store install behavior remains unchanged
- A neighboring regression exposed a useful truth-sync task:
  - `tests/test_factory_config_early_data_isolation.pas` had encoded the old no-op behavior by expecting a default-path client context to still build when shared defaults contained `ServerEarlyDataReplayStoreFile`
  - that contract is now updated to the stronger fail-fast truth while preserving the server-side replay-isolation assertions
- This batch intentionally did not broaden the scope to all early-data client/server fields:
  - `ClientEarlyDataEnabled` / `ServerEarlyDataPolicy` / `ServerMaxEarlyDataSize` still need a separate truth decision if we want to harden the rest of the mixed early-data config surface later

# Findings - Factory ServerName Scope Clarification

## 2026-05-12
- A fresh continuation review found another config-scope truth gap adjacent to the earlier builder/server-name work:
  - builder validation already warns that server-side connections ignore deprecated context-level SNI
  - but factory/config creation still applies `TSSLConfig.ServerName` unconditionally on both default-config and one-shot config paths
- That makes the public API inconsistent:
  - `WithSNI(...)` on server configs is compatibility-preserved but explicitly warned
  - `TSSLFactory.CreateContext(...)` has no warning surface, so the same client-only field is silently accepted
- The narrowest safe repair is to tighten the factory boundary only:
  - keep builder compatibility semantics unchanged
  - reject server-context `ServerName` at factory creation time instead of silently storing an inert setting
- Fresh RED/GREEN confirmed the split is now cleaner:
  - client default-config and one-shot factory paths still preserve `ServerName`
  - server default-config and one-shot factory paths now fail fast with `ESSLConfigurationException`
  - builder `ValidateServer` warning semantics remained intact throughout
- Updating `DumpSSLConfig(...)` to label `ServerName` as client-scoped also reduces future operator confusion when reading config dumps.

# Findings - Builder Server Smoke Truth

## 2026-05-12
- The next neighbor signal after the security-first fix was not a runtime bug in `BuildServer`; it was a misleading integration smoke.
- Source, docs, and existing builder contracts all agree on the current truth:
  - server contexts require certificate material
  - `BuildServer` without certificate should fail
  - client-side auto-selection convenience methods are unrelated to that server-certificate precondition
- `tests/test_builder_integration.pas` had been calling `.WithPerformanceFirst.BuildServer` without any certificate/key, then printing the expected failure as if it were part of the smoke output.
- The smallest correct repair was to change the smoke itself:
  - generate a temporary self-signed certificate/key pair
  - feed them through `WithCertificatePEM(...)` / `WithPrivateKeyPEM(...)`
  - keep runtime semantics unchanged

# Findings - Security-First Selector Viability

## 2026-05-12
- Fresh post-fix neighbor smoke exposed a second selector truth gap: `tests/test_backend_selector_basic.pas` still prints "安全优先需求 选择失败" on this host even with OpenSSL available.
- Runtime capability diagnostics show this is not because OpenSSL misses the hard security-first protocol/algorithm requirements:
  - OpenSSL currently satisfies TLS 1.3, required ciphers, required hashes, and required key exchanges
  - OpenSSL's current `GetSecurityScore(...)` on this host is `80`
  - `CreateSecurityFirstRequirements` still hard-codes `MinSecurityScore := 85`
- That makes the default security-first template self-contradictory against current shipped capability truth: a recommended path can become unselectable even when the strongest available backend meets all of its hard cryptographic requirements.
- The narrowest safe repair is to align the security-first template threshold with the highest currently attainable "strong default" backend score, rather than diluting the required protocol/cipher/hash/kex set.
- After alignment, the security-first selector path recovered cleanly without touching the global scoring rubric:
  - `CreateSecurityFirstRequirements.MinSecurityScore := 80`
  - selector security-first smoke now selects OpenSSL again
  - builder `WithSecurityFirst` client-context creation also succeeds again on this host
- A separate adjacent signal remains for later review:
  - `tests/test_builder_integration.pas` still prints `Server context requires a certificate` on the server-side performance-first smoke
  - that looks like a context-construction expectation issue, not part of the selector-threshold fix

# Findings - Backend Selector Required-Feature Truth

## 2026-05-12
- After the factory config-scope fix, the next highest-value product bug is in `src/fafafa.ssl.backend.selector.pas`, not another broad config refactor.
- Fresh static review shows `CalculateRequiredFeaturesScore(...)` only counts `sslFeatSNI` and `sslFeatALPN`.
- This means a caller can set `RequiredFeatures` to `SessionCache / SessionTickets / Renegotiation / OCSPStapling / CertificateTransparency`, and the selector will treat those requirements as if they were not requested.
- That is a real public-behavior bug, not just documentation drift:
  - `RequiredFeaturesTotal` / `RequiredFeaturesMatched` become inaccurate
  - unsupported backends can still survive filtering
  - the selector continues to read old boolean capability fields on the only two features it does check
- The narrowest safe repair is to add a selector-local feature helper and evaluate required features from support-level truth, treating any non-`none` support level as satisfying presence requirements.
- The first RED harness also surfaced a separate observability quirk:
  - `TSSLBackendMatchDetails.RequiredFeaturesTotal/Matched` names imply "feature-only" accounting
  - the current implementation actually counts all required dimensions (protocols / algorithms / platform requirements / features)
  - focused selector contracts therefore need a minimal requirement baseline such as `TLS12 + single feature`, otherwise default score floors can masquerade as feature-filter failures
- Removing the selector helper's dead `else` branch also cleared the fresh `Unreachable code` compiler warning emitted by FPC on this path.

# Findings - Factory Connection-Scope Clarification

## 2026-05-12
- Deep review pivoted from the earlier SNI/server-name suspicion after re-checking runtime constructors: server-side connections already gate context `ServerName` inheritance on `sslCtxClient`, so that path is not the next real bug.
- A fresher and higher-risk drift is in `TSSLConfig` scope truth:
  - `BufferSize` / `HandshakeTimeout` are present in the public config record
  - library defaults and `CreateDefaultConfig(...)` populate them with concrete values
  - `TSSLDebugUtils.DumpSSLConfig(...)` prints them as if they are real active config
  - but `TSSLFactory.CreateContext(...)` does not actually apply either field to runtime contexts/connections
- This is more dangerous than doc drift because callers can reasonably believe a custom timeout or buffer size has taken effect when it has been silently ignored.
- The narrowest safe repair is to fail fast at the factory boundary for scope-mismatched custom values, rather than expanding runtime surface mid-batch.
- Fresh RED proved the drift was real:
  - one-shot `TSSLFactory.CreateContext(const AConfig)` silently accepted custom `HandshakeTimeout` / `BufferSize`
  - library-default `TSSLFactory.CreateContext(AContextType, ALibType)` silently accepted the same custom values after `ISSLLibrary.SetDefaultConfig(...)`
- The landed fix keeps runtime behavior stable and only tightens the boundary:
  - factory now raises `ESSLConfigurationException` when either field is customized away from `0` / factory default sentinel values
  - config dump output now explicitly marks both fields as connection/transport scoped, not context-factory-applied runtime settings
- Related regression checks stayed green:
  - logging-scope clarification contract still passes
  - default-config request-safe logging contract still passes

# Findings - Interface Design Audit

## 2026-05-12
- User asked for a complete interface review to find problematic design, with Chinese output.
- Static review scope covers public Pascal API, factory/builder/facade entrypoints, backend connection declarations, and docs/source alignment.
- `ISSLConnection` is too fat: session resumption, certificate verification details, diagnostics, OCSP, connection info, convenience string methods, timeout/blocking, and context access are all still in core.
- The optional interfaces `ISSLDiagnostics`, `ISSLSessionResumption`, `ISSLCertificateVerification`, `ISSLOCSPStapling`, and `ISSLConnectionInfo` duplicate methods already present on core `ISSLConnection`, weakening capability-gated design.
- `ISSLContext.SetServerName` is deprecated, but `TSSLFactory.CreateContext(...)` and `TSSLContextBuilder.BuildClient/BuildServer` still push `ServerName` into context state, including server contexts.
- `ISSLServerConnection` appears in architecture/design docs but has no source declaration or implementation.
- `TSSLConfig` mixes library, context, connection, and backend-private fields. `LogLevel` / `LogCallback` are rejected by factory, while `BufferSize` / `HandshakeTimeout` are present in defaults/debug output but are not consumed by the main context creation path.
- `TSSLBackendCapabilities` has old boolean fields and newer support-level fields for the same features; serializer, diff, selector, and backend code can consume or emit both.
- `fafafa.ssl` facade still exports factory/helper/connector plus legacy convenience helpers, so the canonical entrypoint is not crisp.
- Formal report added at `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`.

---

# Findings - v1.5.0 Linux Static Audit Closeout

## 2026-05-12
- User changed release acceptance: GitHub quota is unavailable, so `v1.5.0` should close on Linux gates plus static review instead of waiting for remote Windows artifacts.
- Final delivery must merge the verified code back into `main`, not just leave it on `glm51`.
- Current branch `glm51` starts clean and already contains the prior release-prep commits `8491b91` and `d40bd48`.
- Fresh local Linux gates already rerun in this batch:
  - `python3 scripts/compile_all_modules.py`: PASS, `185/185`, `0 failed`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`: PASS, compile `185/185`, module tests `17/17`, Phase 2 dry-run exercised
- Static source scan found no `TODO`, `FIXME`, `skeleton`, or `placeholder` markers in active `src/fafafa.ssl*.pas`.
- The only `skeleton` program names are `tests/winssl/test_winssl_mtls_skeleton.pas` and `tests/winssl/test_winssl_ocsp_crl_skeleton.pas`; both are Windows-only test harnesses, not Linux release implementation gaps.
- Release notes previously risked implying `TSSLHelper` was removed. The implementation still publicly exports `TSSLHelper`; the correct release wording is that old global helper aliases/functions were removed while `TSSLHelper` remains available.
- Added `tests/scripts/test_v1_5_0_static_pascal_audit_contract.sh` and `docs/test_reports/STATIC_AUDIT_V1.5.0.md` to make the static review repeatable.
- Fresh continuation verification completed: FreePascal TLS 1.3 completeness is PASS with `17 passed / 0 failed`, the source style gate is PASS, the Phase 2 dry-run is PASS, and both release/static-audit contracts are PASS.

# Findings - v1.5.0 Release Formalization

## 2026-05-12
- Current tracked worktree started clean; `git clean -nd` was empty and `git clean -ndX` only reported ignored local `.ace-tool/`.
- Latest local tag is `v1.4.3`.
- Version source of truth is already updated in `src/fafafa.ssl.base.pas`: `FAFAFA_SSL_VERSION_STRING = '1.5.0'` and `FAFAFA_SSL_INTERFACE_VERSION = 10500`.
- `fafafa_ssl.lpk` is still stale at package version `1.0.0`.
- `README.md` still advertises `v1.4.2` in the badge and latest-version heading.
- `CHANGELOG.md` still keeps the `v1.5.0` entries under `[Unreleased]`.
- `RELEASE_NOTES_V1.5.0.md` does not exist yet.
- `.github/workflows/release.yml.disabled` is still the old release template: it calls `build_linux.sh`, generates inline release notes, and includes old API examples.
- `.github/workflows/release.yml` does not exist yet.
- No focused release workflow contract exists yet, so this batch should add one.
- The local `wave-b-b2-manual.yml` already has the current Windows runtime checklist lane: Lazarus / `lazbuild`, quick smoke, Wave B Windows gate, and broader WinSSL transcript.
- `origin/master` still has the older Wave B/B2 workflow without the current Windows runtime checklist lane, and there is no remote `glm51` branch yet.
- `gh` is installed and authenticated with `repo` and `workflow` scope, so GitHub workflow dispatch is feasible after pushing a branch that contains the updated workflow.
- Expired CT/SCT and CRL fixtures were the real FreePascal completeness blocker. The refreshed fixtures preserve the valid embedded SCT bytes, malformed SCT raw bytes, revoked serial `03E9`, and nonmatching serial `03EA`, with validity extended to 2036-05-08.
- Strict style gate was also a real release blocker: `python3 scripts/check_code_style.py src` reported 369 odd-indent errors across 44 source files. The fix is mechanical indentation-only and now the style gate passes.
- Versioned release files are now aligned: `CHANGELOG.md` has `[1.5.0] - 2026-05-12`, `README.md` advertises `v1.5.0`, `fafafa_ssl.lpk` is `1.5.0`, and `RELEASE_NOTES_V1.5.0.md` exists.
- Release automation is now current: `.github/workflows/release.yml` exists, `.github/workflows/release.yml.disabled` is synchronized to it, the workflow rejects non-`v1.5.0`, requires an already-existing approved tag, runs current gates, and uses `RELEASE_NOTES_V1.5.0.md` as the release body.
- Focused release/workflow contracts are green locally. The remaining release-signoff blocker is still real Windows-host `WinSSL` runtime proof from Wave B/B2 artifacts, not Linux-side code or documentation drift.
- Local release-prep batch was committed as `8491b91 chore: prepare v1.5.0 release` and pushed to `origin/glm51`.
- Wave B/B2 was dispatched on `glm51` as GitHub Actions run `25698425400` with `run_linux_baseline=true`, `strict_closure=true`, and `run_id=release_1_5_0_20260512`.
- That run failed before any platform runner executed: `setup` and `summary` failed, `linux-gate` / `macos-gate` / `windows-gate` were skipped, and artifact download returned `no valid artifacts found to download`.
- The failure annotation says the job was not started because recent account payments failed or the spending limit needs to be increased. This is an external GitHub Actions billing/spending-limit blocker, not a workflow entrypoint failure and not a `WinSSL` implementation failure.
- Therefore the release remains tag-blocked until billing access is restored or an equivalent trusted Windows host produces the required `WinSSL` runtime artifacts.

---

# Findings - Repo Hygiene And Ignore Consolidation

## 2026-05-12
- The repo is clean in terms of tracked changes, but it carries a large amount of ignored build output.
- The largest easy win is `tmp/`, which alone is about `6.0G`.
- Other clearly generated output directories include `bin/`, `tests/bin/`, `tests/lib/`, `examples/bin/`, `artifacts/`, and `tools/test_audit/bin/`.
- The existing `.gitignore` only covered top-level `tests/test_*` executables, so nested `tests/**/test_*` binaries could still appear as untracked clutter.
- The first broad cleanup pass removed generated output directories, but it also swept local ignored agent/config folders and `archive/` because the exclude list did not hold as expected.
- I added explicit ignore entries for `.agents/`, `.codex/`, `.fusion/`, and `.ace-tool/` so local agent/cache directories stop polluting repo status checks.
- `git check-ignore -v` now confirms nested `tests/**/test_*` executables are ignored while `tests/**/*.pas` and the current shell scripts remain visible, and benchmark report markdown is still ignored by the dedicated Bench/CI rule.
- The remaining visible untracked item is the new hygiene plan doc, which is intentional and ready to commit.
- A follow-up review found the rootless `task_plan.md` / `findings.md` / `progress.md` / `WARP.md` patterns were too broad because they could match archive docs with the same names.
- The example digital-signature `private.pem` / `public.pem` pair was generated output, so keeping it in the repository was the wrong default.
- The digital-signature password-protected key contract also assumed the ignored `tmp/` parent already existed; after the artifact sweep removed `tmp/`, that script failed before reaching the example behavior.
- The repo-hygiene follow-up batch is now committed.

# Findings - Working-Memory, Artifact Hygiene, And WinSSL Workflow Closeout

## 2026-05-12
- Current `HEAD` is `e80100a fix: batch 6 - compiler warning reduction and capabilities contract test`.
- The active worktree issue was not product drift; it was three generated ELF test binaries left under `tests/contract/` and `tests/wolfssl/`.
- Those artifacts are safe to delete and should not be committed.
- `task_plan.md` was lagging behind the current repo state, so this batch re-centered the working-memory files on the real truth instead of the older workflow batch.
- A fresh contract run then proved the Windows workflow was still below the runtime checklist: it did not yet install / verify Lazarus for `lazbuild`-based checks.
- That RED is actionable and belongs in this batch, because the repo already had a dedicated workflow-alignment plan and the current handoff still depends on a reviewable Windows lane.
- After the workflow update, the three relevant contracts are now green again, so the Windows lane is back in sync with the runtime checklist.
- The follow-up scope stayed narrow: root-anchor the repo-local ignore entries, remove the generated sample PEM pair, and make the related contract recreate its ignored `tmp/` parent.

# Findings - Backend Broad Completion Audit

## 2026-05-05
- 当前台账如果继续停在 `Win64 Cross-Target Fresh Revalidation`，会把“下一步该做什么”表达错位；更准确的当前批次应当是 `WinSSL Windows Runtime Proof Handoff`。
- `c10bf22` 之后，当前 Linux 主机上已经没有新的高价值 repo-side 收口项：
  - broad audit 已证明 public contract / capability truth / repo gates / WinSSL source+bundle contract 全部闭合
  - fresh `-Twin64` 交叉编译也已补齐
  - 因此不该再在 Linux 上虚构新的实现批次来回应“继续”
- 后续任何针对 broad objective 的 fresh 进展，都必须来自真实 Windows 主机上的运行时证据，而不是继续追加 Linux 侧 compile/source-contract 证明。
- 这类 Windows runtime 证据至少要包含：
  - quick smoke
  - WinSSL minimal gate
  - Wave B Windows gate summary + step logs
  - broader suite 对握手、证书存储、session resumption、online/error mapping、mTLS 的逐项记录
- Windows 主机上的 fresh failure 需要先分流：
  - 环境问题：`lazbuild` / PowerShell / 出网 / 权限
  - 入口问题：脚本或路径漂移
  - 实现问题：真实 WinSSL runtime 行为缺口
  只有最后一种，才值得重新打开 `src/fafafa.ssl.winssl.*` 生产修复批次。
- broad objective 的 blocker 还可以再收紧一层：上轮 fresh audit 已确认 Linux 主机上的 public surface、capability truth、compile gate、minimal CI gate、以及 WinSSL source/bundle contracts 都没有新 drift，但还没有 fresh 复核 Win64 cross-target compile 本身。
- 因为 `compile_all_modules.py` 默认跳过 WinSSL，所以如果不补显式 `-Twin64` 交叉编译，就仍然有一层“今天的 Linux 主机 compile proof 主要来自旧记录”的不确定性。
- fresh `-Twin64` 交叉编译现在把这层不确定性也清掉了：
  - `tests/winssl/test_winssl_session_management.pas` 可以在当前 Linux 主机上成功交叉编译到 Win64
  - `tests/integration/test_backend_comparison.pas` 也可以在当前 Linux 主机上成功交叉编译到 Win64
- 这意味着 broad blocker 不需要再写成“缺 Windows runtime proof + 可能缺 cross-target compile freshness”；更准确的当前状态是：
  - Linux 主机上的 public surface、capability truth、repo gate、source contract、bundle contract、Win64 compile proof 都闭合
  - 剩下的唯一 requirement 就是 Windows 主机上的真实 runtime 证据
- broad objective 不能再靠“前面已经做了很多批次”来推断完成；必须重新按 deliverable checklist 审计：
  - public interface contracts
  - capability / `KnownIssues` truth
  - Linux 主机可验证的实现 gate
  - `WinSSL` 的真实 Windows runtime proof
- 如果 `WinSSL` 仍然没有真实 Windows runtime evidence，那么即便 Linux 主机上的 public surface / compile / gate 全绿，broad objective 也不能标记完成。
- fresh broad audit 现在把“已完成”和“未完成”的边界重新钉死了：
  - `OpenSSL` / `WolfSSL` / `MbedTLS` / `FreePascal`：
    - public interface `Contract 1-21` 全绿
    - capability truth fresh 继续对齐
    - Linux 主机可验证的 compile / completeness / minimal CI gate 都全绿
  - `WinSSL`：
    - repo-side source contract 继续闭合
    - validation bundle contract 继续闭合
    - 但公开 runtime proof 仍然缺失
- `WinSSL` 当前更准确的状态不是“实现大概完整，只差一点点”，而是：
  - Linux 上的 source-contract、bundle docs、repo gate 已经尽量收口
  - 仍然没有真实 Windows 主机上的握手、证书存储、session resumption、server/client runtime 行为证据
  - 因此 broad objective 还没有完成，而且在当前 Linux 主机上也不能继续 productively 收口这一项
- `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md` 当前已经把剩余 requirement 说清楚了：
  - quick smoke
  - `run_winssl_tests.ps1` minimal gate
  - `scripts/run_wave_b_windows_gate.ps1`
  - broader manual WinSSL suite
  - 这些步骤缺任何一环，都不能把 WinSSL 写成“runtime proof complete”
- broad objective 的 public interface completion audit 现在更清楚了：
  - `tests/contract/test_backend_contract.pas` 已覆盖 `Contract 1-21`
  - 当前 Linux 主机上，`OpenSSL` / `WolfSSL` / `MbedTLS` / `FreePascal` 的 public interface 合同面是闭合的
  - `WinSSL` 这条线并不是合同失败，而是当前主机无法提供真实 Windows runtime evidence
- 因此当前剩余问题的重心，已经从“还有没有漏掉的公开接口漂移”转到了“capability / KnownIssues / runtime proof 真相是否准确”。
- `WolfSSL` / `MbedTLS` 的 capability `KnownIssues` 之前仍然属于 public truth drift：
  - `WolfSSL` 只写 `May require specific build options for full feature support`
  - `MbedTLS` 只写 `Optimized for embedded systems, may lack some enterprise features`
  - 这两条 wording 都没有把当前已验证的 capability 边界表达出来
- 当前 Linux 主机上，`WolfSSL` 更准确的 runtime truth 是：
  - capability object 仍会把 `OCSPStaplingSupport` 发布成 `experimental`
  - current host 的 early-data helper set 缺失，所以 `EarlyDataSupport = none`
  - 因此 `KnownIssues` 至少要明确 build/runtime helper 门控，以及 early-data 可能退化为 `none`
- 当前 Linux 主机上，`MbedTLS` 更准确的 runtime truth 是：
  - `EarlyDataSupport = none`
  - `OCSPStaplingSupport = none`
  - `CertTransparencySupport = none`
  - 因此 `KnownIssues` 至少要明确 early-data / OCSP stapling / CT 当前不支持，而不是泛泛地说“可能缺少企业特性”
- 这批 focused RED/GREEN 之后，当前 broad objective 仍然不能判定完成：
  - `FreePascal` 仍保留 experimental early-data caveat
  - `WolfSSL` 仍保留 helper-gated / experimental caveat
  - `WinSSL` 仍缺真实 Windows runtime proof
- WinSSL validation bundle 收口之后，当前还能继续推进、且不依赖外部主机的最高价值 repo-side 缺口，已经收窄到 early-data truth drift，而不是再开新的 backend 功能线。
- 当前 Linux 主机上的 `WolfSSL` early-data 真相需要继续按“双层语义”记录：
  - backend family 只有在 build/runtime helper 完整时，才应发布实验性 early-data 能力
  - current host 的 `libwolfssl.so` 缺少 `wolfSSL_write_early_data`、`wolfSSL_get_early_data_status`、`wolfSSL_CTX_set_max_early_data`、`wolfSSL_CTX_get_max_early_data`
  - 因此 current host 的 capability truth 仍然是 `sslSupportNone`，client context / connection 都必须保持 early-data interface absent
- 这批真正的 drift 主要落在文档层，而不是 `src/`：
  - `README.md` 顶部表格仍把 `FreePascal` early-data 写成 production ready，并把 `WolfSSL` 写成无条件实验性
  - `docs/guides/EARLY_DATA_GUIDE.md` 的支持矩阵仍把 `FreePascal` 写成完整支持/生产就绪，且没有写清 `WolfSSL` 的 helper-gated fallback
  - `docs/BACKEND_CAPABILITY_MATRIX.md` 的 `WolfSSL` early-data 段落仍把 interface surface 写成无条件已接通
- 为了让 focused contract 真正命中当前 host truth，测试侧需要两条显式前提：
  - `tests/test_openssl_wolfssl_early_data_connection_contract.pas` 必须显式 `uses` `fafafa.ssl.openssl.backed`、`fafafa.ssl.wolfssl.api`、`fafafa.ssl.wolfssl.lib`
  - `WolfSSL` 路径必须直接 `CreateWolfSSLLibrary`，不能继续走 `TSSLFactory.IsLibraryAvailable(...)` 的旧判断链
- focused Pascal contract 的首次失败不是新的 backend 行为漂移，而是测试文件里遗留了一个悬空 `else` 语法错误；修掉这个测试侧错误后，runtime 结论仍然稳定：
  - `OpenSSL` early-data connection contract 继续全绿
  - `WolfSSL` 在当前 host 继续证明为 capability `none` + context / connection interface absent
- 这批完成后，repo-side 剩余的 early-data 风险会更纯粹地收敛成两类：
  - 若未来 `wolfSSL` helper set 升级，需要补独立 runtime proof，而不是预先把 capability 调宽
  - broad objective 仍不能完成，因为 `WinSSL` 还缺真实 Windows 主机 runtime evidence
- completion audit 继续往 WinSSL runtime validation bundle 下钻后，确认 broad objective 还剩一个明确的 repo-side 收口点：
  - `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md` / `tests/windows/VALIDATION_BUNDLE.md` 仍然是旧模板，引用 `Run-WindowsValidation.ps1`、`Run-QuickValidation.ps1`、`test_cert_load`、`test_factory_mode` 等当前仓库并不使用的入口
  - `tests/quick_winssl_validation.ps1` / `tests/run_winssl_tests.ps1` 仍依赖调用者先切到正确 cwd，不能作为稳定的 Windows runtime validation entrypoint
- 这意味着“只差 Windows 主机实跑”这个结论之前还差半步：
  - 生产实现线已经不该在 Linux 上盲目继续扩
  - 但 runtime proof bundle 自身还需要先收口到当前真实入口链路
- 这批的最小正确修法不是重开 `src/fafafa.ssl.winssl.*`，而是:
  - 用 focused shell contract 锁住 validation bundle truth
  - 让 `tests/quick_winssl_validation.ps1` / `tests/run_winssl_tests.ps1` 自解析到 `tests/winssl`
  - 把 `tests/windows/*.md` 和 `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md` 都改成当前真实执行口径
- fresh verification 已证明这条 repo-side 收口线闭合:
  - `bash tests/scripts/test_winssl_windows_validation_bundle_contract.sh` 通过
  - `bash tests/scripts/test_wave_b_windows_gate_pwsh_and_verbose_contract.sh` 继续通过
  - `git diff --check` 通过
- 更准确的当前结论是：
  - WinSSL broad objective 还不能宣称完成
  - 但仓库内“Windows validation bundle 自己就已经过期/跑不起来”这层阻塞已经清掉
  - 剩余高风险未证实区域现在更纯粹地指向真实 Windows 主机 runtime evidence
- `FreePascal` early-data 默认 shipped path 这条 Linux 侧最高价值缺口已经落地到实现层，而不再只是文档收口：
  - `src/fafafa.ssl.freepascal.context.pas` 默认 server / both context 现在装配 `TFreePascalDefaultPersistentEarlyDataReplayLedger`
  - `src/fafafa.ssl.freepascal.earlydatareplay.pas` 新增默认 replay-store 路径解析、testing override seam、以及 backend-private managed persistent provider
  - `src/fafafa.ssl.freepascal.lib.pas` 的 `KnownIssues` 已收口到 `local persistent replay-store path + fail-closed + experimental`
- 默认路径解析当前遵循明确优先级：
  - testing override
  - 环境变量 `FAFAFA_SSL_FREEPASCAL_EARLY_DATA_REPLAY_STORE_DIR`
  - Windows `LOCALAPPDATA` / `APPDATA`
  - Unix `XDG_STATE_HOME` / `HOME/.local/state`
  - `GetAppConfigDir(False)`
  - `GetTempDir(False)`
  - 最终 canonical suffix: `fafafa.ssl/freepascal/early-data-replay`
- focused runtime 在第一次复跑时暴露的不是新的生产漂移，而是测试隔离问题：
  - `tests/test_freepascal_tls13_early_data.pas` 里的 scripted server 初始票据是固定值
  - 默认 shipped path 改成 durable 后，宿主机默认 replay-store 的历史 truth 会跨运行残留
  - 因此旧测试第一次失败在 `Accepted server connection should report accepted early-data status`，本质上是被前一次运行留下的 replay truth 提前拒绝
- 这个 fresh failure 的最小正确修法是“测试基线隔离”，不是回退生产实现：
  - 测试进程启动时先把默认 replay-store 目录固定到 `tmp/...` 下的进程级基线
  - `PrepareDefaultReplayStoreDirectoryForTesting(...)` 继续允许单测临时切目录
  - `ResetDefaultReplayStoreDirectoryForTesting` 改成回到该基线，而不是回到宿主真实默认路径
  - 这样既保住默认 durable shipped path 的真实行为，也避免默认路径测试被宿主状态污染
- fresh evidence 现在证明：
  - focused `test_freepascal_tls13_early_data` 通过
  - focused `test_capability_cache` 通过，并直接打印新的 `KnownIssues`
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_default_durable_shipped_path_20260504` 通过
  - `python3 scripts/compile_all_modules.py` 继续 `185/185`
  - `git diff --check` 通过
- broad objective 仍不能标记为“各个后端的接口和实现都完整”：
  - Linux 侧这条 `FreePascal` 默认 shipped-path caveat 已经从 “in-memory single-process” 收窄到 “local persistent + fail-closed + experimental / non-distributed wording”
  - 当前更硬的独立环境 blocker 仍是 `WinSSL` Windows 主机 runtime proof

## 2026-05-04
- completion audit against the actual current state 明确证明：当前 broad objective 还没有达到“各个后端的接口和实现都完整”。
- 这不是 interface surface 还没锁住，而是 implementation-level remaining gaps 仍然存在：
  - `FreePascal` `0-RTT / early data` 继续以 `sslSupportExperimental` 发布，`KnownIssues` 仍明确写着 “in-memory single-process anti-replay ledger”
  - `WinSSL` 当前仍缺 Windows 主机上的 runtime proof；Linux 侧只能证明 source-contract 与 Win64 cross-target compile
- 因此“contract 18-21 全绿”只能证明 connection optional public surface 已经闭合，不能外推成 overall completion。
- `docs/BACKEND_CAPABILITY_MATRIX.md` 存在一组直接可证的 truth drift：
  - `docs/ROADMAP.md`、`src/fafafa.ssl.freepascal.lib.pas`、`tests/test_capability_cache.pas` 都把 FreePascal `0-RTT / early data` 定义成 `experimental`
  - 同一能力对象也把 `OCSPStaplingSupport` / `CertTransparencySupport` 发布为 `sslSupportExperimental`
  - 但能力矩阵此前仍把 FreePascal `Early Data` 写成“完整支持（生产就绪）”，并在快速参考表中把 `Early Data` / `OCSP Stapling` / `Certificate Transparency` 都写成 `✅`
- 这批最小正确动作不是重开新的 backend 行为线，而是先把 capability matrix 收回到当前真实证据：
  - FreePascal `Early Data` 改成 `⚠️`
  - FreePascal `OCSP Stapling` / `Certificate Transparency` 明确写成“public surface 已暴露，但 capability 仍按 experimental 发布”
- 当前 completion audit 之后，更合适的后续排序是：
  - Linux 主机可继续推进的实现缺口：`FreePascal` early-data 默认 shipped path caveat
  - 独立环境 blocker：`WinSSL` Windows runtime proof
- `ISSLCertificateVerification` completion audit 同样确认是纯 contract closeout，而不是新的实现修复批次：
  - 新增 `Contract 21` 后，当前 Linux 可验证 backend 全绿
  - `OpenSSL` / `WolfSSL` / `MbedTLS` / `FreePascal` 都没有暴露新的 certificate-verification drift
- `ISSLCertificateVerification` 这条线的关键边界已经明确：
  - 该 optional interface 与 core `ISSLConnection` 的 verify surface 高度重叠
  - 因此当前 contract 只锁住同一 connection 对象上的 chain/result truth，不能把它误写成完整的 runtime trust/hostname/revocation 证明
- `Contract 21` 当前锁住的 certificate-verification truth 包括：
  - 所有公开 connection 都暴露 `ISSLCertificateVerification`
  - `GetVerifyResult` / `GetVerifyResultString` 与 core getter 一致
  - `GetPeerCertificateChain` 的长度与每个元素的 nilness / subject / issuer / serial 与 core getter 保持自洽
- 在 `ISSLDiagnostics`、`ISSLConnectionInfo`、`ISSLSessionResumption`、`ISSLCertificateVerification` 都收口为 completion audit 后，这一轮明确排队的 connection optional public surface 已全部收尽；下一步更适合回到总盘点，而不是继续假设还有同级未审计接口。
- `ISSLSessionResumption` completion audit 也确认是纯 contract closeout，而不是新的实现修复批次：
  - 新增 `Contract 20` 后，当前 Linux 可验证 backend 全绿
  - `OpenSSL` / `WolfSSL` / `MbedTLS` / `FreePascal` 都没有暴露新的 session-resumption drift
- `ISSLSessionResumption` 这条线的关键边界已经明确：
  - 该 optional interface 与 core `ISSLConnection` 的 session surface 高度重叠
  - 因此当前 contract 只能锁住同一 connection 对象上的 getter/reused truth，不能把它误写成真实跨连接恢复成功 proof
- `Contract 20` 当前锁住的 session-resumption truth 包括：
  - 所有公开 connection 都暴露 `ISSLSessionResumption`
  - `IsSessionReused` 与 core getter 一致
  - `GetConnectionInfo.IsResumed` 与 `IsSessionReused` 一致
  - `GetSession` 返回对象的 nilness、`IsValid`、`IsResumable`、协议/密码/超时/peer certificate nilness 与 core getter 保持自洽
- 在 `ISSLDiagnostics`、`ISSLConnectionInfo`、`ISSLSessionResumption` 都收口为 completion audit 后，当前最高收益的剩余未审计 public surface 已进一步收窄到 `ISSLCertificateVerification`。
- `ISSLConnectionInfo` completion audit 已确认是纯 contract closeout，而不是新的实现修复批次：
  - 新增 `Contract 19` 后，当前 Linux 可验证 backend 全绿
  - `OpenSSL` / `WolfSSL` / `MbedTLS` / `FreePascal` 都没有暴露新的 connection-info drift
- `Contract 19` 真正锁住的不是单一接口 presence，而是整组连接信息真相：
  - 所有公开 connection 都暴露 `ISSLConnectionInfo`
  - `ProtocolVersion` / `CipherSuite` / `ALPNProtocol` 与 core `ISSLConnection.GetConnectionInfo` 保持一致
  - `GetSelectedALPNProtocol` / `GetStateString` / `GetContext` 与 direct getter 和创建时 context 保持最小自洽
- 当前证据表明 `TBaseSSLConnection` 仍然是 `ISSLConnectionInfo` 的主要 truth source：
  - `OpenSSL` override 的 `GetConnectionInfo` / `GetStateString` 没有偏离共享语义
  - 本机 Linux 上 `WinSSL` 继续只有 unavailable/skip 证据，不能把它外推成 Windows runtime proof
- 在 `ISSLDiagnostics` 和 `ISSLConnectionInfo` 都收口为 completion audit 后，下一条最高收益的未审计 public surface 已进一步收窄到 `ISSLSessionResumption`，`ISSLCertificateVerification` 保持后续独立批次。
- `ISSLDiagnostics` 是当前最适合作为下一批的未审计 public interface：
  - 接口本身是 optional surface
  - `TBaseSSLConnection` 已统一实现 `GetHealthStatus` / `IsHealthy` / `GetPerformanceMetrics` / `GetDiagnosticInfo`
  - 因此最值得先确认的是“所有 backend connection 是否真的都暴露并继承了这套共享语义”
- 新增 `Contract 18` 后，`ISSLDiagnostics` 这条线直接全绿，说明当前 diagnostics public truth 没有新的 backend-specific drift：
  - `OpenSSL` / `WolfSSL` / `MbedTLS` / `FreePascal` 的公开 connection 都支持 `ISSLDiagnostics`
  - `HealthStatus.IsConnected` 与 `ISSLConnection.IsConnected` 一致
  - `IsHealthy` 与 `HealthStatus` 推导条件一致
  - `DiagnosticInfo.HealthStatus` / `PerformanceMetrics` 与 direct getter 保持自洽
- 这批的正确结论是 completion audit closeout，而不是继续展开生产代码改造。
- 因此下一条更合适的后续审计线，已经收窄到：
  - `ISSLConnectionInfo`
  - `ISSLSessionResumption`
  - `ISSLCertificateVerification`
- `certificate` / `certificate-store` native-handle completion audit 说明，前一轮“contract 1-15 全绿”还不是 broad goal 完成：
  - `ISSLCertificate` / `ISSLCertificateStore` 仍是公开接口面，之前没有 cross-backend completion audit
  - `Contract 16` / `Contract 17` 补上后，certificate 线直接全绿，但 store 线立即打出真实 RED
- `Contract 16` 的关键真相是：不能拿空 certificate wrapper 当 probe。
  - `OpenSSL.CreateCertificate` 会先分配 `X509`
  - `WolfSSL` / `MbedTLS` / `WinSSL` 的空 certificate wrapper 初始 native handle 可以是 `nil`
  - 所以 certificate native-handle truth 必须用已加载 fixture 验证，不能用“刚创建的空对象”误判
- `Contract 17` 的真实漂移非常集中：
  - `TMbedTLSCertificateStore.Create` 之前只建 `TInterfaceList`，没有调用 `AllocateStore`
  - `TWolfSSLCertificateStore.Create` 之前也只建 `TInterfaceList`，`FX509Store` 一直保持 `nil`
  - 因此两个 backend 虽然声明了 `ISSLNativeHandleAccess`，但公开 `CreateCertificateStore()` 返回对象时 `IsNativeHandleValid=False`
- 最小正确修复是 constructor 级别补真实 native store 分配，而不是改接口定义或放宽契约：
  - `TMbedTLSCertificateStore.Create` 直接调用 `AllocateStore`
  - `TWolfSSLCertificateStore.Create` 在 `wolfSSL_X509_STORE_new` 可用时立即分配 `FX509Store`
- 这批收口后，`Contract 16/17` 结果是：
  - `OpenSSL` / `WolfSSL` / `MbedTLS` 的 certificate 与 certificate-store native-handle 契约全绿
  - `FreePascal` 的 certificate / store 继续保持 `ISSLNativeHandleAccess` absent
  - `WinSSL` 仍只在 Linux 上保留 unavailable/skip 边界，不外推成 Windows runtime proof
- 这批没有扩大到 store 内容同步/verify parity：
  - `MbedTLS` / `WolfSSL` store 内部把证书列表写回 native store 的完整语义仍值得后续专批审计
  - 当前提交只锁住 public `CreateCertificateStore()` 返回对象的 native-handle availability truth
- WinSSL 当前最容易把后续工作带偏的，已经不只是代码漂移，还有文档漂移：
  - `docs/reference/WINSSL_DESIGN.md` 仍写“100% 完成”，且顶层类型示例落后于当前源码
  - `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md` 仍把 `DTLS`、`OCSP Stapling`、`Session Ticket` 等写成已证实支持
  - 这些表述已经和 `src/fafafa.ssl.winssl.lib.pas:GetCapabilities` 以及 `docs/BACKEND_CAPABILITY_MATRIX.md` 冲突
- 对 WinSSL 来说，更准确的文档边界必须分成三层：
  - public surface / capability truth
  - Linux 上已拿到的 source contract 与 Win64 cross-target compile proof
  - 仍需 Windows 主机的 runtime proof
- 只要本机 `wine` 继续直接退出 `159`，且没有 `pwsh`，就不能把 Linux 上的静态证据写成 WinSSL runtime 已完成；文档必须显式保留这个 blocker。
- `WINSSL_BACKEND_STATUS_REPORT.md` 最合理的角色不是功能许愿单，而是“当前证据报告”：哪些 surface 已锁住、哪些 compile path 已闭合、哪些 runtime 还没证。
- `src/fafafa.ssl.winssl.connection.pas` 之前把 `FContext: ISSLContext` 和 `ISSLLibrary` 直接硬转成 `TWinSSLContext` / `TWinSSLLibrary`，真实风险不在运行时分支猜测，而在 compile surface 已经给出明确信号：
  - `Class types "ISSLContext" and "TWinSSLContext" are not related`
  - `Class types "ISSLLibrary" and "TWinSSLLibrary" are not related`
- focused source contract 的初次 RED 直接命中了这条风险：`winssl connection no longer hard-casts ISSLContext to TWinSSLContext` 失败，说明问题确实还存在于源码层，而不是只存在于某次编译告警噪声里。
- 最小正确修复不是扩张 public `ISSLContext` / `ISSLLibrary`，而是在 WinSSL 私有边界内补 internal access interface：
  - `IWinSSLContextAccess`
  - `IWinSSLLibraryStatsAccess`
  - `TWinSSLContext` / `TWinSSLLibrary` 显式实现它们
  - `TWinSSLConnection` 统一通过 `Supports(...)` 查询 verify callback、info callback、CA store、library statistics updater
- `Supports(...)` 路线还额外暴露了一个中间 compile truth：internal interface 必须带有效 GUID，否则接口查询本身就不成立；给这两个 internal access interface 补 GUID 后，Win64 交叉编译才真正通过。
- 这批收口后，Win64 focused 编译里那两条“不相关 class types”告警已经消失，说明 connection/context/library 的协作边界重新回到了 interface-compatible 路径。
- 这仍然不是 Windows runtime proof。当前 Linux 主机上，WinSSL 更准确的完成度结论是：
  - source contract 已锁住不再硬转
  - Win64 compile surface 已闭合
  - Windows runtime 仍需独立环境证明
- `TWolfSSLLibrary.DetectCapabilities` 之前只有 `HasSNI` 按 helper surface 检测，`HasALPN` / `HasSessionTickets` 仍直接硬编码为 `True`；同时 `GetCapabilities` 还把 `SNISupport` / `ALPNSupport` / `SessionTicketsSupport` 无条件发布成 `stable`。
- 这意味着 WolfSSL library 的 public capability truth 分成了两层漂移：
  - `SupportsALPN` / `SupportsSessionTickets` 与 `IsFeatureSupported(...)` 直接跟着硬编码漂
  - `SNISupport` / `ALPNSupport` / `SessionTicketsSupport` 又进一步脱离 capability 布尔值本身
- deterministic helper-loss contract 稳定证明了这组漂移，不需要依赖不稳定 runtime：
  - 暂时清空 `wolfSSL_UseSNI`
  - 暂时清空 `wolfSSL_UseALPN` / `wolfSSL_ALPN_GetProtocol`
  - 暂时清空 `wolfSSL_get_session` / `wolfSSL_set_session`
  - 再让 `TWolfSSLLibrary.Initialize` 基于当前 helper state 做 capability 检测
- focused RED 给出的 7 个失败点都落在真实 truth-source 漂移上：
  - `SNISupport`
  - `SupportsALPN` / `ALPNSupport` / `sslFeatALPN`
  - `SupportsSessionTickets` / `SessionTicketsSupport` / `sslFeatSessionTickets`
- 最小正确修复依然只是收紧 capability truth，而不是扩充 WolfSSL 新功能：
  - `HasALPN := Assigned(wolfSSL_UseALPN) and Assigned(wolfSSL_ALPN_GetProtocol)`
  - `HasSessionTickets := Assigned(wolfSSL_get_session) and Assigned(wolfSSL_set_session)`
  - `SNISupport` / `ALPNSupport` / `SessionTicketsSupport` 基于同一组 capability 布尔值发布 `stable` 或 `none`
- 这批明确不改 connection/context 的 SNI、ALPN、session runtime 行为，也不扩大成 session resumption 审计；目标只是让 library public truth fail-closed。
- 收口后的证据闭环：
  - focused framework test：`110/110`
  - `python3 scripts/compile_all_modules.py`：`185/185`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`：`[PASS]`
- `TMbedTLSLibrary.DetectCapabilities` 之前把 `HasSNI` / `HasALPN` / `HasSessionTickets` 直接硬编码成 `True`，导致 library helper surface 缺失时，`GetCapabilities` 和 `IsFeatureSupported` 仍然会发布假阳性能力。
- 这条漂移可以用 deterministic helper-loss contract 稳定复现，而不需要依赖不稳定 runtime：
  - 先 `LoadMbedTLSLibrary`
  - 暂时清空 `mbedtls_ssl_set_hostname`
  - 暂时清空 `mbedtls_ssl_conf_alpn_protocols` / `mbedtls_ssl_get_alpn_protocol`
  - 暂时清空 `mbedtls_ssl_get_session` / `mbedtls_ssl_set_session`
  - 再让 `TMbedTLSLibrary.Initialize` 重新做 capability 检测
- focused RED 直接给出了 9 个 false-positive 失败点，说明问题不在测试假设，而在 capability truth source：
  - `SupportsSNI` / `SNISupport` / `sslFeatSNI`
  - `SupportsALPN` / `ALPNSupport` / `sslFeatALPN`
  - `SupportsSessionTickets` / `SessionTicketsSupport` / `sslFeatSessionTickets`
- 最小正确修复不是去补新 feature，而是把 capability 发布收窄到真实 helper surface：
  - `HasSNI := Assigned(mbedtls_ssl_set_hostname)`
  - `HasALPN := Assigned(mbedtls_ssl_conf_alpn_protocols) and Assigned(mbedtls_ssl_get_alpn_protocol)`
  - `HasSessionTickets := Assigned(mbedtls_ssl_get_session) and Assigned(mbedtls_ssl_set_session)`
  - `GetCapabilities` 里的 `SNISupport` / `ALPNSupport` / `SessionTicketsSupport` 也必须跟同一组布尔值对齐为 `stable` 或 `none`
- 这批明确不扩大到新的 SNI/ALPN/session runtime 实现，也不展开 session resumption 审计；目标只是把 public capability truth 从硬编码收成 fail-closed。
- 收口后的证据闭环：
  - focused framework test：`96/96`
  - `python3 scripts/compile_all_modules.py`：`185/185`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`：`[PASS]`
- `TMbedTLSConnection.DoRenegotiate` 之前只是静默返回 `False`，不会留下任何错误分类或诊断文案；这意味着 public `ISSLConnection.Renegotiate` 在 MbedTLS backend 上虽然形式可调用，但调用方拿不到“这条路径不支持”的稳定语义。
- 这不是 renegotiation 功能未实现本身的问题，而是 public contract 不完整：同样的 `False` 结果既可能表示 handshake precondition、native failure，也可能表示 backend 根本不支持。如果没有显式 `sslErrUnsupported`，调用方和测试都无法区分。
- 最小正确修复不是实现真正的 renegotiation，而是把这条路径收成显式 unsupported 语义：
  - `DoRenegotiate` 记录 `RecordError(sslErrUnsupported, ...)`
  - `DoGetError` 在没有 native error 但已有语义错误时优先返回 `FLastErrorCode`
  - `DoGetVerifyResultString` 在已有语义错误文案时优先返回 `FLastErrorString`
- focused RED/GREEN 证明收口后，MbedTLS framework contract 与前面 WolfSSL 的同类 contract 重新一致：
  - `Renegotiate` 仍然返回 `False`
  - `GetError(-1)` 稳定返回 `sslErrUnsupported`
  - `GetVerifyResultString` 稳定包含 `renegotiation`
- 这批最初想用 scripted TLS 1.3 server harness 做 WolfSSL client full-handshake runtime RED，但当前主机 `libwolfssl-dev 5.7.2-0.1+deb13u1` / `wolfSSL 5.7.2` 上，真实信号只有 `Connect=False / verify=OK` 一类模糊结果，不能拿来当最终 completion proof。
- 因此这批的可靠真相源必须收窄成 deterministic contract，而不是继续在当前 host 上反复追逐不稳定 runtime 行为：
  - 用真实 DER fixture 锁住 `LoadFromDER(...)`
  - 覆盖 `wolfSSL_get_peer_chain` / `wolfSSL_get_chain_count` / `wolfSSL_get_chain_length` / `wolfSSL_get_chain_cert`
  - 验证 `TWolfSSLConnection.GetPeerCertificateChain` 会 materialize 出 leaf + issuer 两张证书，并在 helper 缺失时 fail-closed 为 `[]`
- `src/fafafa.ssl.wolfssl.base.pas` 之前把 `WOLFSSL_ERROR_WANT_READ` / `WOLFSSL_ERROR_WANT_WRITE` / `WOLFSSL_ERROR_SYSCALL` / `WOLFSSL_ERROR_SSL` 写成 `-2/-3/-5/-85`，这对应的不是 `wolfSSL_get_error()` 的返回值；本机 `/usr/include/wolfssl/ssl.h` 的真实值是 `2/3/5/85`，所以原常量会把 error mapping 和 framework test 都带偏。
- `src/fafafa.ssl.wolfssl.api.pas` 里 `TwolfSSL_X509_d2i` 的类型签名是 `wolfSSL_X509_d2i(WOLFSSL_X509**, const unsigned char*, int)`，但 loader 之前却绑定到了 `wolfSSL_d2i_X509`；两者的第二个参数层级不同，这就是 `LoadFromDER(...)` / `LoadFromMemory(...)` AV 的根因。
- `TWolfSSLConnection.DoGetPeerCertificateChain` 现在不再无条件返回空数组，而是会通过 native peer-chain helpers 拉 DER bytes，再用 `TWolfSSLCertificate.LoadFromMemory(...)` materialize；任何 helper 缺失、长度异常或单张证书加载失败都会 fail-closed 回到空数组。
- 仓库级验证额外暴露了一个相邻漂移：`TWolfSSLConnection.DoRenegotiate` 之前只是静默返回 `False`，不会留下任何可诊断语义。最小正确收口不是实现 renegotiation，而是显式记录 `sslErrUnsupported` 和稳定诊断文案，让 framework contract 有真实错误分类可依赖。
- `tests/integration/test_backend_comparison.pas` 虽然名字看起来是 backend comparison，但依赖链会进入 `fafafa.ssl.factory` -> `fafafa.ssl.freepascal.context` -> `fafafa.ssl.freepascal.earlydatareplay.fileprovider`，所以它能补到前面 WinSSL 定向 compile 用例没有覆盖到的 shared cross-target surface。
- `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas` 的 `implementation uses` 之前写成 `{$IFDEF UNIX}Unix{$ENDIF};`；在 Linux host-target 下 `UNIX=True`，所以 `python3 scripts/compile_all_modules.py` 一直看不出问题；但在 `-Twin64` 下 `UNIX=False`，展开后会变成非法的 `uses ;`，这就是 `Syntax error, "identifier" expected but ";" found` 的直接根因。
- `src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas` 也有同型写法和同型错误，说明这不是单点疏漏，而是 replay store 家族里重复存在的 target-conditioned empty-uses drift。
- 这不是 FreePascal early-data provider 的行为 bug，而是 target-conditioned compile-surface drift；最小正确修复是把整个 `implementation uses` clause 放进 `{$IFDEF UNIX}` 块里，而不是改 provider 逻辑或重新设计锁文件路径。
- 两个 shared replay-store 单元都修完后，`tests/integration/test_backend_comparison.pas` 的 Win64 交叉编译已经成功，说明这条 Linux 侧 compile proof 现在真正补齐到了 backend-comparison 集成面，而不再只是若干 WinSSL 定向用例。
- 因此之前“Linux 侧只剩 Windows runtime proof”这个结论还差半步：在这次修复前，仓库里仍残留一个 Win64 cross-target compile blocker，只是它落在 FreePascal shared 单元而不是 WinSSL 单元本身。
- 如果 `test_backend_comparison.pas` 修复后也能 Win64 交叉编译成功，那么 Linux 侧的更准确结论才是：
  - 选定的 WinSSL / backend comparison 源码与 Win64 compile surface 已闭合
  - 当前真实剩余硬阻塞是 Windows runtime proof
  - 本机 `wine` 退出 `159` 且 `pwsh` 缺失，不能承担这个证明
- `TBaseSSLConnection` 之前把 `ISSLCertificateTransparency` / `ISSLCertificateTransparencyValidation` 直接挂在基类类声明上，导致所有继承它的 connection 都会被 `Supports(...)` 识别成“支持 CT”，哪怕 backend capability 明确是 `False/None`。
- 当前主机上的 `OpenSSL` 默认 capability truth 也不是“支持 CT”：
  - `SupportsCertificateTransparency=False`
  - `CertTransparencySupport=sslSupportNone`
  - 说明仓库里虽然有底层 OpenSSL CT binding，但默认初始化并没有把它发布成当前 backend 的 connection-level CT 能力。
- 因此这批最小且真实的 GREEN 不是给 `OpenSSL` 硬补一个新 surface，而是：
  - 把 CT interface 从 `TBaseSSLConnection` 的类声明中移出
  - 只让 `TFreePascalConnection` 继续显式实现 CT / validation interface
  - 让 `OpenSSL` / `WolfSSL` / `MbedTLS` 在当前 capability truth 下保持 interface absent
- 新增的 `Contract 9` 证明这不是文档问题，而是 public contract drift：
  - RED 时 `OpenSSL` / `WolfSSL` / `MbedTLS` 都在 capability 为 `False/None` 的情况下仍暴露 CT / validation interface
  - GREEN 后三者都已不再暴露，`FreePascal` 仍保留 non-stub CT surface
- `src/fafafa.ssl.mbedtls.context.pas` 与 `src/fafafa.ssl.winssl.context.pas` 当前仍把 `ISSLEarlyDataContext`、`ISSLServerOCSPStaplingContext` 写进类声明，但具体方法是存根或直接抛出不支持异常。
- `src/fafafa.ssl.mbedtls.lib.pas` 与 `src/fafafa.ssl.winssl.lib.pas` 的能力矩阵已经明确把 OCSP stapling 记为 `False/sslSupportNone`；`EarlyDataSupport` 没有设置为可用，因此接口暴露与能力矩阵存在事实漂移。
- `src/fafafa.ssl.connection.base.pas` 的连接级 OCSP/CT/CT validation surface 采用了“默认返回 not supported”的收敛模型，说明仓库对可选接口的一贯语义是“没能力就不暴露或返回 not supported”，而不是暴露后抛运行时异常。
- `docs/BACKEND_CAPABILITY_MATRIX.md` 的总览表目前仍把 WinSSL/MbedTLS/WolfSSL 的 Early Data 和多后端 OCSP stapling 写成过宽的 ✅/⚠️，与当前实现不一致。
- 新增的 `tests/contract/test_backend_contract.pas` 契约先在 MbedTLS 上打出 RED：不支持的 early-data / server-OCSP-stapling 可选接口仍被 `Supports(...)` 识别为真。
- 最小 GREEN 只需要把 `TMbedTLSContext` / `TWinSSLContext` 从接口声明里移除 `ISSLEarlyDataContext` 与 `ISSLServerOCSPStaplingContext`；这样既不改其他 TLS 行为，也能让 `Supports(...)` 与能力矩阵重新一致。
- 本机 Linux 验证覆盖了 MbedTLS 路径与全仓 185 核心模块编译，但 WinSSL 仍属于静态对称改动，未在 Windows 主机做 runtime 验证。
- 更大的未收口区域仍存在：`OpenSSL` / `WolfSSL` 的 Early Data public/runtime completeness 需要单独审计，当前批次没有扩大到这条线。
- `OpenSSL` context 早已暴露 `ISSLEarlyDataContext`，native binding 也已有 `SSL_write_early_data` / `SSL_get_early_data_status` / `SSL_get_max_early_data`，真实缺口是 connection class 没有实现 `ISSLEarlyDataConnection`，导致 helper surface 与能力宣称脱节。
- `OpenSSL` 本地头文件还明确暴露了 `SSL_SESSION_get_max_early_data` / `SSL_SESSION_set_max_early_data`，仓库之前没绑定这个 session-level truth source，连接实现只能停在 context-level。
- `WolfSSL` 本地头文件除了已有 `wolfSSL_write_early_data` / `wolfSSL_CTX_get_max_early_data`，还提供 `wolfSSL_get_max_early_data`、`wolfSSL_get_early_data_status`、`wolfSSL_SESSION_get_max_early_data`；因此 client connection 的 queue/status/limit surface 可以直接对齐到 native API，而不是继续把后端标成“计划中”。
- 新增的 `tests/test_openssl_wolfssl_early_data_connection_contract.pas` 先在 `OpenSSL` 上打出 RED：context 支持 early-data，但 connection 不支持 `ISSLEarlyDataConnection`，helper 也因此返回 `False`。
- 这批最小 GREEN 包括三件事：补 session/early-data native binding、给 `TOpenSSLConnection` / `TWolfSSLConnection` 加 `ISSLEarlyDataConnection`、以及把 `TWolfSSLLibrary.GetCapabilities.EarlyDataSupport` 从未设置状态收敛成 `sslSupportExperimental`。
- `WolfSSL` 当前主机上 backend 不可用，所以 focused contract 对它只能给出 `[SKIP]`；但 `python3 scripts/compile_all_modules.py` 和最小门禁都已编到 `src/fafafa.ssl.wolfssl.connection.pas` / `src/fafafa.ssl.wolfssl.lib.pas`，可以证明接口和能力路径至少已静态闭合。
- `docs/guides/EARLY_DATA_GUIDE.md` 之前还在示例中使用不存在的 `sslEarlyDataNotSent`，说明 guide 不只是 backend 状态过时，连 public enum truth 也漂了；这一处必须顺手修掉，否则调用方照抄会直接编译失败。
- `TWolfSSLContext` 已经实现 `ISSLServerOCSPStaplingContext`，builder 也会把 `server_ocsp_stapled_response_file` 加载进 `FServerStapledOCSPResponse`；但当前仓库没有任何 server handshake / callback 接线消费这块字节数组，所以这条 public surface 仍然是“只存不发”。
- `src/fafafa.ssl.wolfssl.api.pas` 里的 `wolfSSL_UseOCSPStapling` 绑定签名与本地 `/usr/include/wolfssl/ssl.h` 不一致：真实签名是 `(ssl, status_type, options)`，仓库里却只绑定了一个 `options` 参数，说明 client request path 目前不只是没调用，连 native seam 都是错的。
- 本地头文件同时暴露了 `wolfSSL_set_tlsext_status_ocsp_resp`、`wolfSSL_CTX_set_tlsext_status_cb`、`wolfSSL_CTX_set_tlsext_status_arg`，这正是把 caller-provided stapled-response bytes 挂进服务端握手所需的最小 API；仓库当前未绑定这几项。
- `TWolfSSLConnection.DoGetOCSPStaplingEnabled` 现在返回“`wolfSSL_GetOCSP_Response` 符号是否存在”，这和 `OpenSSL` / `FreePascal` 路径上“是否真的拿到了 stapled response”语义不一致，属于 connection surface 假阳性。
- `TWolfSSLLibrary.GetCapabilities` 当前对 OCSP stapling 的 truth 也不对：一方面 `DetectCapabilities` 默认把 `HasOCSP := False`，会把能力压成 `none`；另一方面 `GetCapabilities` 一旦依赖 `HasOCSP`，支持级别又会跳成 `stable`。这两个端点都不符合当前最合理的“experimental”真值。
- `TWolfSSLContext.CreateConnection(...)` 的真实返回路径之前没有走独立的 `src/fafafa.ssl.wolfssl.connection.pas`，而是继续实例化 `src/fafafa.ssl.wolfssl.context.pas` 里内嵌的旧版 `TWolfSSLConnection`。这意味着早前落在独立连接单元里的修复并不会进入真实 runtime path，必须先把 context 构造路径改到现代连接单元。
- 本批次的最小闭环现在是：
  - WolfSSL API binding 已对齐本地 header，client request 与 server status callback seam 都补齐
  - `TWolfSSLContext.CreateConnection(...)` 已改走现代连接单元
  - `TWolfSSLConnection.DoGetOCSPStaplingEnabled` 已从“符号存在”收紧为“实际拿到 stapled response”
  - `TWolfSSLLibrary.GetCapabilities.OCSPStaplingSupport` 已固定为 `sslSupportExperimental`
- 由于本机没有 `libwolfssl.so`，Pascal focused contract 仍只能给出 dependency skip；这批真正可执行的 RED/GREEN 证据来自新增的源码契约测试 + 全仓 compile/minimal gate。
- `TOpenSSLContext` 早已具备 `FServerStapledOCSPResponse`、`SetServerStapledOCSPResponse(...)`、`LoadServerStapledOCSPResponseFile(...)` 和 builder file-load 入口，但在本批之前没有任何 native callback 注册逻辑消费这些 bytes，实际 server path 仍然是“只存不发”。
- `src/fafafa.ssl.openssl.api.ssl.pas` 已经有 `SSL_CTX_set_tlsext_status_cb`、`SSL_CTX_set_tlsext_status_arg`、`SSL_set_tlsext_status_ocsp_resp` 的 binding，所以 `OpenSSL` 这条线的最小缺口不在 API 暴露，而在 `TOpenSSLContext` 没有把 public material seam 接到 native context seam。
- `OpenSSL` stapling callback 不能把普通 `GetMem` 指针直接交给 `SSL_set_tlsext_status_ocsp_resp(...)`；要保持 allocator 兼容，必须走 `CRYPTO_malloc` / `OPENSSL_free` / `CRYPTO_free` 这一侧的内存语义。当前 batch 顶部 helper 已按这个方向实现。
- 新增的 `tests/openssl/test_openssl_server_ocsp_stapling_callback_contract.pas` 证明了当前最小闭环：
  - `SetServerStapledOCSPResponse(...)` 会注册 callback 和 arg
  - 手工调用 callback 时会注入 caller-provided DER bytes
  - `ClearServerStapledOCSPResponse` 会注销 callback
  - `BuildServer + WithServerOCSPStapledResponseFile(...)` 会同时 load bytes 和注册 callback
- 续接到 runtime-proof 批次后，发现本地 sandbox 不能稳定创建 listen socket；因此 `OpenSSL` runtime 证据改成 `tests/openssl/test_openssl_server_ocsp_stapling_runtime.pas` 里的 scripted `TStream` TLS 1.3 对端，而不是继续依赖 localhost TCP。
- 这条 runtime test 真正打出的生产级 RED 不在 builder 或 DER file-load，而在 `src/fafafa.ssl.openssl.api.ssl.pas`：`SSL_CTX_set_tlsext_status_cb_impl` 之前错误地走了 `SSL_CTX_ctrl(...)`，而本机 `/usr/include/openssl/tls1.h` 宏要求的是 `SSL_CTX_callback_ctrl(...)`。修复后，real handshake 才开始实际调用 `SSL_set_tlsext_status_ocsp_resp(...)`。
- 为了让 OpenSSL 服务端在真实握手里稳定进入 stapling issuance path，当前批次还把 `status_type=ocsp` 同步补到了两层：
  - `src/fafafa.ssl.openssl.context.pas` 的 `ApplyServerOCSPStaplingConfiguration`
  - `src/fafafa.ssl.openssl.connection.pas` 的 `ApplyPreHandshakeOCSPStatusRequest(False)`
- builder runtime 期间出现的 `Accept` 失败不是新的生产 bug。定位结果是 `TSSLContextBuilder.Create` 默认带 `WithVerifyPeer`，而本批 direct server smoke 一直使用 `SetVerifyMode([])`；因此 runtime proof 里的 builder helper 必须显式 `WithVerifyNone`，否则脚本化客户端不提供证书时服务端会按预期拒绝握手。
- `tests/openssl/test_openssl_server_ocsp_stapling_runtime.pas` 现在锁住了 5 个真实场景：
  - direct `configured + requested => stapled DER surfaced`
  - direct `configured + not requested => absent`
  - direct `no material + requested => absent`
  - builder `no file + requested => handshake succeeds, absent`
  - builder `WithServerOCSPStapledResponseFile(...) + requested => stapled DER surfaced`
- 本批收口验证结果：
  - focused runtime test：PASS
  - `python3 scripts/compile_all_modules.py`：`185/185`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`：compile gate `185/185`，模块测试 `17/17`，phase2 baseline dry-run PASS
- 文档侧还存在一个紧邻的 truth 漂移：`docs/BACKEND_CAPABILITY_MATRIX.md` 的 OpenSSL server OCSP stapling 条目仍停留在“能 load / 能 callback”，`docs/guides/OCSP_USAGE_GUIDE.md` 的 server 示例也没有写出 builder 默认 verify 基线。
- docs truth 批次已补齐两点：
  - `BACKEND_CAPABILITY_MATRIX` 现在明确 OpenSSL server stapling 已有 focused TLS 1.3 runtime proof，并补充了当前范围与边界
  - `OCSP_USAGE_GUIDE` 的最小 server 示例现在显式写 `WithVerifyNone`，并说明 OpenSSL server-side issuance path 已不是“只有 callback contract”
- `docs-write` 规范要求跑格式化。仓库里的 `yarn prettier --write ...` 在当前 shell 下会落到家目录解析相对路径，因此实际使用了底层 prettier 可执行文件配绝对路径完成格式化；最终 `BACKEND_CAPABILITY_MATRIX.md` 和新 plan 文件被格式化，`OCSP_USAGE_GUIDE.md` 内容未变化，`task_plan.md` 无需额外格式调整。
- docs truth 批次的验证结果：
  - `python3 scripts/compile_all_modules.py`：`185/185`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`：compile gate `185/185`，模块测试 `17/17`，phase2 baseline dry-run PASS
- 把临时“直接预注入 response”探针撤掉后重新回跑 `tests/wolfssl/test_wolfssl_server_ocsp_stapling_runtime.pas`，当前主机上的真实失败形态更清楚了：`configured + requested` 场景不会进入 `wolfSSL_set_tlsext_status_ocsp_resp(...)`，`status_call_delta=0`，而 scripted client 仍只观测到额外握手类型 `8,20`（`EncryptedExtensions`、`Finished`），说明不是“已发出但解析不到”，而是旧版 WolfSSL 根本没走到 emission callback。
- 本机 `WolfSSL` 版本已核对为 Debian 包 `5.7.2-0.1+deb13u1`，运行时字符串 `wolfSSL 5.7.2`。
- `wolfSSL` 官方 `5.9.1` release notes 提到修复 “`OCSP_WANT_READ` 在 TLS 1.3 handshake message processing 中的处理”，这和当前 `5.7.2` 上“baseline handshake 已通，但 configured stapled response 不进入 emission path”的症状高度一致。
- 因此这批最诚实的收口不是继续扩大 repo-side 代码改动，而是把 runtime test 分成两层：
  - baseline 层始终执行：`no request`、`requested + no material`、`builder no-file`
  - emission 层只在 `wolfSSL >= 5.9.1` 主机上执行：direct configured、builder file-load
- 当前 `WolfSSL` server stapling truth 已更新为：
  - public surface、builder file-load、client request / consume、server status callback wiring 均已接通
  - scripted `TStream` baseline handshake 已有本机 runtime 证据
  - `configured + requested => stapled DER` 仍受 host `wolfSSL` 版本约束，旧版主机显式 skip，不再误报成本地生产代码缺口
- `MbedTLSConnection` / `TWolfSSLConnection` 早就各自带有 `SetServerName/GetServerName` 实现，但类声明没有挂 `ISSLClientConnection`。这会导致 capability 明明宣称 `SupportsSNI=True`，`Supports(Connection, ISSLClientConnection, ...)` 却返回 `False`，属于纯粹的 public interface 漂移。
- 新增的 `Contract 8` 证明这不是文档问题而是真实 RED：
  - `WolfSSL`: `SupportsSNI=True but connection does not expose ISSLClientConnection`
  - `MbedTLS`: `SupportsSNI=True but connection does not expose ISSLClientConnection`
- 这批的最小 GREEN 不需要重写 SNI 行为，只需要把现有的 `SetServerName/GetServerName` 公开挂进 `ISSLClientConnection`。
- 收口后，`OpenSSL` / `FreePascal` / `WolfSSL` / `MbedTLS` 四条当前 Linux 可验证的 SNI-capable backend 已对齐到同一 public contract：capability、connection interface 和 round-trip 行为一致。
- `ISSLOCSPStapling` 和前面的 CT 批次是同型问题：基类 `TBaseSSLConnection` 已经带有共享 getter/stub，但如果同时把 interface 声明也挂在基类上，就会让 unsupported backend 在 `Supports(...)` 上出现假阳性。
- 新增的 `Contract 10` 在当前 Linux 主机上直接打出了真实 RED：
  - `MbedTLS`: `SupportsOCSPStapling=False but connection still exposes ISSLOCSPStapling`
- 这说明问题不只是文档漂移，而是 public contract 漂移：调用方会在 capability 为 `False/sslSupportNone` 的后端上仍然拿到 `ISSLOCSPStapling`，随后只在运行时才撞到 `Not Supported` 存根。
- 当前最小且一致的 GREEN 不是给 `MbedTLS` / `WinSSL` 新增 OCSP 能力，而是：
  - 把 `ISSLOCSPStapling` 从 `TBaseSSLConnection` 的类声明中移出
  - 只让 `TFreePascalConnection` / `TOpenSSLConnection` / `TWolfSSLConnection` 显式实现该 interface
- `WinSSL` 虽然本机无法 runtime 验证，但它的 capability truth 仍是：
  - `SupportsOCSPStapling=False`
  - `OCSPStaplingSupport=sslSupportNone`
  因此和 `MbedTLS` 一样，不应该继续暴露 connection-level `ISSLOCSPStapling`。
- 这批收口后，当前仓库里连接级 OCSP public contract 已重新回到统一语义：
  - capable backend 暴露 `ISSLOCSPStapling` 且 getter 不落回基类 `Not Supported`
  - incapable backend 不再通过 `Supports(...)` 误判支持
- `docs/MIGRATION_GUIDE_V1.1.md` 把旧 `ISSLConnection.GetNativeHandle` 的迁移目标定义成可选接口 `ISSLNativeHandleAccess`；这不只是 context/certificate/store 层约定，而是 connection-level public contract 的一部分。
- 新增的 `Contract 11` 在当前 Linux 主机上打出了新的真实 RED：
  - `MbedTLS`: `C-library backend connection does not expose ISSLNativeHandleAccess`
- `TMbedTLSConnection` 本来就已经有 `DoGetNativeHandle`，只是类声明没挂 `ISSLNativeHandleAccess`；因此这不是“缺少 native seam”，而是纯粹的 optional-interface 漂移。
- `TWinSSLConnection` 也同型：已有 `DoGetNativeHandle` 返回 `@FCtxtHandle`，但类声明没挂 `ISSLNativeHandleAccess`，所以在 Windows 主机上应当也是同型 contract drift。
- 这批最小 GREEN 不需要改握手、session 或 helper 层，只需要：
  - `TMbedTLSConnection = class(..., ISSLClientConnection, ISSLNativeHandleAccess)`
  - `TWinSSLConnection = class(..., ISSLClientConnection, ISSLNativeHandleAccess)`
  - 补齐 `GetBackendType` / `IsNativeHandleValid`
- `FreePascal` connection 不实现 `ISSLNativeHandleAccess` 仍然是正确边界：纯 Pascal backend 不应该为了 API 对称而暴露假的 native handle。
- Linux 主机上对 `src/fafafa.ssl.winssl.connection.pas` 的额外单编会卡在 `winssl.context.pas` 依赖 `unit Windows`，这说明当前 WinSSL 证据边界是：
  - runtime: 不可用
  - Linux 单编: 受 Windows SDK 依赖阻塞
  - public contract: 仍可按结构对称性修正，并由 focused contract 设计锁住
- 新增的 `Contract 12` 把 context-level 和 connection-level early-data public surface 一起锁进 capability 双向契约后，当前 Linux 主机在 `WolfSSL` 上打出了真实 RED：
  - `EarlyDataSupport=None but client context still exposes ISSLEarlyDataContext`
  - `EarlyDataSupport=None but client connection still exposes ISSLEarlyDataConnection`
- 这不是 capability 写窄过头，而是当前 shared library 真值本来就是 `None`：
  - `/usr/include/wolfssl/ssl.h` 虽然声明了 early-data 相关 API
  - 但 `/usr/lib/x86_64-linux-gnu/libwolfssl.so` 当前并没有导出 `wolfSSL_write_early_data`、`wolfSSL_get_early_data_status`、`wolfSSL_CTX_set_max_early_data`、`wolfSSL_CTX_get_max_early_data`
  - 因此 `TWolfSSLLibrary.GetCapabilities` 返回 `EarlyDataSupport=None` 是符合本机 runtime 的
- `Supports(...)` 假阳性的根因是 Pascal 类声明静态挂接口，而不是方法体逻辑：
  - `TWolfSSLContext` 之前无条件实现 `ISSLEarlyDataContext`
  - `TWolfSSLConnection` 之前无条件实现 `ISSLEarlyDataConnection`
  - 所以即便 capability 已经收敛到 `None`，调用方仍会通过 `Supports(...)` 误判后端可用
- 这批最小且一致的 GREEN 是把接口暴露改成创建点按 capability 选择类，而不是把 capability 反向调宽：
  - `TWolfSSLEarlyDataContext = class(TWolfSSLContext, ISSLEarlyDataContext)`
  - `TWolfSSLEarlyDataConnection = class(TWolfSSLConnection, ISSLEarlyDataConnection)`
  - `TWolfSSLLibrary.CreateContext(...)` 只在 `EarlyDataSupport <> sslSupportNone` 时返回 early-data context 子类
  - `TWolfSSLContext.CreateConnection(...)` 只在同一 capability 条件下返回 early-data connection 子类
- 收口后 `Contract 12` 已全绿：
  - `WolfSSL` 在当前主机保持 `ISSLEarlyDataContext` / `ISSLEarlyDataConnection` absent
  - `WolfSSL` 仍保留 `OCSPStaplingSupport<>None` 时的 `ISSLServerOCSPStaplingContext`
  - 仓库级验证也通过：`185/185` compile、minimal CI gate PASS
- `docs/MIGRATION_GUIDE_V1.1.md` 对原生句柄迁移的 public contract 不只覆盖 connection，也明确覆盖 context：
  - C-library backend context 应支持 `ISSLNativeHandleAccess`
  - 纯 Pascal backend context 不应暴露伪 native handle
  - 推荐调用方式就是 `Supports(Ctx, ISSLNativeHandleAccess, NativeAccess)`
- 之前仓库只把这条契约锁到了 connection-level，context-level 还缺 cross-backend completion audit；因此新增 `Contract 13` 不是功能扩张，而是补齐迁移契约证据。
- `Contract 13` 在当前 Linux 主机上直接全绿，说明 context native-handle 这条线没有新的生产代码漂移：
  - `OpenSSL` / `WolfSSL` / `MbedTLS` 的 client/server context 都暴露 `ISSLNativeHandleAccess`
  - `GetBackendType` / `IsNativeHandleValid` / `GetNativeHandle` 均与 backend truth 一致
  - `FreePascal` context 继续保持该接口 absent
  - `WinSSL` 仍因平台不可用而 skip，不把 Linux 结果外推成 Windows runtime 证明
- 因此 context native-handle 这批的正确结论是 completion audit closeout，而不是继续展开新的实现修复。
- `ISSLHttpHooksAccess` 的真实 public truth 目前比 capability 文档更窄，只有 `TOpenSSLContext` 与 `TFreePascalContext` 暴露该接口；`WolfSSL` / `MbedTLS` / `WinSSL` context 当前都保持 absent。
- 这条 truth 不是孤立现象，而是当前使用面的既有边界：
  - `tests/config/test_context_builder_http_hooks.pas` 已验证 OpenSSL builder 可注入 hooks
  - `tests/test_freepascal_client_online_ocsp_runtime.pas` 已依赖 FreePascal context 暴露 hooks
  - 其余 backend 没有现成调用面要求暴露 context-level HTTP hooks
- 新增的 `Contract 14` 直接全绿，说明 context HTTP hooks 这条线没有新的生产代码漂移：
  - `OpenSSL` / `FreePascal` 的 client/server context 都暴露 `ISSLHttpHooksAccess`
  - `SetHTTPGetCallback` / `SetHTTPPostCallback` 后，getter 能稳定 round-trip
  - `WolfSSL` / `MbedTLS` 继续保持接口 absent
  - `WinSSL` 仍因平台不可用而 skip，不把 Linux 结果外推成 Windows runtime 证明
- 因此 HTTP hooks 这批的正确结论也是 completion audit closeout，而不是继续展开新的实现修复。
- 当前更值得继续审计的真实风险点转到了 `src/fafafa.ssl.wolfssl.context.pas`：接口区仍公开保留一套旧的 `TWolfSSLConnection` 类型声明，但 `TWolfSSLContext.CreateConnection(...)` 已切到 `fafafa.ssl.wolfssl.connection.TWolfSSLConnection`，存在 public API 残留与现代实现分叉的可能。
- 进一步核对源码后确认，上面这条风险的边界需要修正：`src/fafafa.ssl.wolfssl.context.pas` 里的旧 `TWolfSSLConnection` 并不在 `interface`，而是在 `implementation` 的私有残留实现。
- 因此这条线的真实问题不是 public API break，而是 single-truth-source 漂移：
  - `TWolfSSLContext.CreateConnection(...)` 的 socket/stream 路径早已只走 `src/fafafa.ssl.wolfssl.connection.pas`
  - 但 `wolfssl.context` 仍私藏一整套旧连接实现、旧流回调、以及多余依赖
  - 继续保留它只会制造未来维护时的假分叉
- 这批最小正确修复因此是删除 `implementation` 内的旧连接残留，而不是给 `wolfssl.context` 新增公开别名。
- focused script contract 证明收口后只剩一条现代 truth source：
  - `wolfssl.context` 不再保留旧 `TWolfSSLConnection` 私有类/构造/析构
  - `TWolfSSLContext.CreateConnection(...)` 继续稳定走 `fafafa.ssl.wolfssl.connection.TWolfSSLConnection`
  - `python3 scripts/compile_all_modules.py` 与 minimal CI gate 继续全绿，说明删掉的是纯死代码而非隐式依赖路径
- `ISSLNativeHandleAccess` 的迁移契约在 docs/实现侧已经覆盖到 context / connection，但 session-level 之前缺少 cross-backend focused contract，导致这条 surface 还没有被持续锁住。
- 新增的 `Contract 15` 在当前 Linux 主机上直接全绿，说明 session native-handle 这条线没有新的生产代码漂移：
  - `OpenSSL` session 通过 `SSL_SESSION_new` 拿到真实 session handle，`ISSLNativeHandleAccess` 与 `TryGetNativeHandle` round-trip 一致
  - `WolfSSL` / `MbedTLS` 的 session wrapper 都能稳定携带 opaque native handle，`GetBackendType` / `GetNativeHandle` / helper round-trip 一致
  - `FreePascal` session 继续保持 `ISSLNativeHandleAccess` absent，不会把纯 Pascal backend 伪装成有原生句柄
- 因为 `WolfSSL` / `MbedTLS` 当前没有廉价、稳定的独立 session allocator，这批 contract 的边界是 wrapped-surface completion audit，而不是完整 session resumption runtime proof。
- `WinSSL` session 仍然不能在这台 Linux 主机上被当作已证实：
  - backend runtime 本机不可用
  - 仓库里同时存在 `src/fafafa.ssl.winssl.session.pas` 与 `src/fafafa.ssl.winssl.connection.pas` 两套 session truth source
  - 其中 `connection.pas` 内的 `TWinSSLSession` 虽实现 `ISSLNativeHandleAccess`，但 `GetNativeHandle=nil`、`IsNativeHandleValid=False`
  - 因此本批对 `WinSSL` 明确 skip，留给后续 Windows/session truth-source 专批
- 这批的正确结论是 completion audit closeout，而不是继续扩成 session runtime/恢复逻辑重构。
- 后续继续下钻 WinSSL 时，真实高风险点不是 session 恢复算法本身，而是 source truth split：
  - `src/fafafa.ssl.winssl.connection.pas` 才是仓库内真实被测试和调用的 `TWinSSLSession` / `TWinSSLSessionManager`
  - `src/fafafa.ssl.winssl.session.pas` 是一套未被当前仓库运行面采用的平行旧实现
  - `tests/winssl/test_winssl_session_management.pas` 里甚至还保留了 `ISSLSession.GetNativeHandle` 这种过时假设
- 这批最小正确修复不是在 Linux 上伪造 WinSSL runtime proof，而是先把 source truth 收敛：
  - `src/fafafa.ssl.winssl.connection.pas` 的 `TWinSSLSession` 不再实现 `ISSLNativeHandleAccess`
  - `src/fafafa.ssl.winssl.session.pas` 收敛为 compatibility shim，真实实现只剩 `winssl.connection.TWinSSLSession`
  - WinSSL 测试与文档不再把 session 当成有原生句柄的对象
- focused source contract 证明收口后只剩一个真实 session implementation truth source：
  - `winssl.connection` 里的 `TWinSSLSession` 不再保留 `GetNativeHandle` / `GetBackendType` / `IsNativeHandleValid`
  - `winssl.session` 不再有独立 `TInterfacedObject` session 实现
  - `tests/winssl/test_winssl_session_management.pas` 改为断言 `ISSLNativeHandleAccess` absent
  - `docs/reference/WINSSL_DESIGN.md` 与 `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md` 已同步到当前真相
- 因此这批的正确结论是 WinSSL source-contract closeout：结构层面的重复实现和假 surface 已清掉，但 Windows runtime proof 仍需后续在 Windows 主机完成。
