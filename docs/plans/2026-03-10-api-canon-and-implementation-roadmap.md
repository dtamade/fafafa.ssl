# SSL/TLS API Canon & Implementation Roadmap

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 先把 fafafa.ssl 的 SSL/TLS 接口设计梳理成统一、全面、合理、优雅的 canon，再按该 canon 连续推进实现补全，重点建设纯 Pascal 后端。

**Architecture:** 主入口收口到 `TSSLContextBuilder`，采用“统一核心 API + 分层暴露高级能力 + 必要时暴露 backend-specific 扩展”的结构。第一阶段先完成 API 设计、contract、能力矩阵与文档真相，再进入实现期；纯 Pascal 后端先以 Linux 上生产可用的 HTTPS/TLS 客户端为第一里程碑。

**Tech Stack:** Free Pascal (ObjFPC), multi-backend SSL/TLS abstraction, Pascal test programs, shell contracts, file-based planning docs.

---

## 1. 已确认的路线图原则

### 1.1 北极星
- 第一优先：`SSL/TLS 接口设计非常全面、非常合理、非常优雅`
- 第二优先：`实现完全完整`
- 重点投资方向：`纯 Pascal 后端`

### 1.2 最高设计原则
- `API 易用与一致性` 优先于抽象纯净、性能极限或后端特色暴露

### 1.3 目标用户
- 主用户：`普通业务开发者`
- 次用户：`框架作者`

### 1.4 主入口策略
- 主入口：`TSSLContextBuilder`
- 辅入口：
  - `Connector/Stream` 作为业务快捷入口
  - `Factory + TSSLConfig` 作为兼容/底层入口

### 1.5 后端抽象策略
- `统一核心 API + 分层暴露高级能力`
- backend-specific 能力仅在确有必要时显式暴露

### 1.6 设计推进方式
- 第一阶段：`先把接口设计全面梳理完，再进入实现期`
- 采用：`文档 + contract/test` 双轨并行

### 1.7 兼容性策略
- 设计期允许 `受控 breaking changes`
- 但要求：
  - 迁移说明
  - 能保留 deprecated 过渡层时尽量保留
  - contract/test 与文档真相同步更新

### 1.8 平台优先级
- 一等平台：`Linux + Windows`
- 纯 Pascal 后端优先覆盖：`Linux`

---

## 2. 本路线图必须覆盖的模块

本阶段接口设计梳理必须覆盖：

1. `Context / Connection / Stream` 主调用链
2. `Certificate / CA / mTLS / PKCS12 / PKCS7`
3. `Backend selection / capability / fallback`
4. `Builder / Config / Factory` 三套入口关系
5. `错误模型 / Result / 异常语义`
6. `日志 / 观测 / 调试接口`
7. `纯 Pascal 后端专属 contract`
8. `性能与资源管理接口`

---

## 3. 纯 Pascal 后端的定位与第一里程碑

### 3.1 角色定位
- `无原生依赖的可移植后端`
- 但不是“只求能跑”，同时要求：
  - 性能可接受
  - 可持续优化
  - 可测、可观测、可部署

### 3.2 第一实现里程碑（M1）
- `先把 HTTPS/TLS 客户端做到生产可用`

### 3.3 M1 建议验收标准（已确认）
1. `TLS 1.2 / 1.3` 稳定握手
2. 默认开启证书链校验
3. 默认开启 hostname verification
4. 支持系统根证书
5. 支持自定义 CA / CA bundle
6. 支持 SNI
7. 支持 ALPN
8. 支持超时、取消、明确错误语义
9. 支持稳定的流式读写与关闭语义
10. 支持可观测性：日志 / 握手失败原因 / 对端证书信息

### 3.4 M1 暂不强制纳入
- OCSP / CRL 强校验
- PKCS#11
- 客户端证书 mTLS
- 0-RTT
- HTTP/2 完整协议层
- 激进性能目标

---

## 4. API Canon 目标形态

### 4.1 层次结构

#### Core API
- 面向普通业务开发者
- 默认安全
- 后端差异最小化
- 覆盖 80% 常见 HTTPS/TLS 客户端/服务端使用场景

#### Advanced API
- 面向框架作者
- 暴露高级 TLS、证书、能力矩阵与调试能力
- 保持 contract 明确，避免污染 Core API

#### Backend-Specific API
- 仅在必要时保留
- 必须明确标注：
  - 哪个 backend 专属
  - 为什么不进 Core/Advanced
  - fallback / unavailable 语义是什么

### 4.2 入口治理

#### `TSSLContextBuilder`
- 作为唯一推荐主入口
- 必须成为：
  - 配置语义最完整
  - 文档最完整
  - contract 最严格
  - backend 一致性最强

#### `TSSLFactory + TSSLConfig`
- 保留为兼容/底层入口
- 继续瘦身，避免承载 builder-only 语义
- 重点约束：library-scope vs request/context-scope

#### `Connector / Stream`
- 保留为业务快捷入口
- 以消费 `ISSLContext` 为主
- 不应重新发明一套平行配置 DSL

---

## 5. 分阶段路线图

### Phase A — API Canon 设计冻结

**目标**
- 明确哪些接口是 canon
- 明确哪些接口属于兼容层
- 明确哪些接口应废弃或降级

**必须产出**
- 一份 API 分层地图
- 一份 builder/factory/config 职责边界说明
- 一份 error/result/exception 语义说明
- 一份 backend capability 分层策略
- 一份 pure Pascal M1 contract 列表

**执行原则**
- 先做 contract 与文档真相
- 不急于补实现
- 允许受控 breaking changes

### Phase B — Core API Contract 固化

**目标**
- 把 Core API 的用户面行为锁成稳定 contract

**优先覆盖**
- context / connection / stream 主调用链
- timeout / close / cancellation
- cert/CA/hostname/system roots
- backend selection/fallback
- error model
- logging / observability

**输出**
- focused Pascal tests
- focused shell docs contracts
- README / docs/README / ARCHITECTURE 真相同步

### Phase C — Pure Pascal Client M1 实现

**目标**
- Linux 上生产可用 HTTPS/TLS client

**重点**
- 握手稳定性
- 证书验证
- hostname verification
- roots / CA
- SNI / ALPN
- 流式读写
- 错误与日志
- 性能基线

### Phase D — 服务端与框架作者能力

**目标**
- 在 Core API 稳定后，推进服务端/mTLS/框架作者扩展面

**重点**
- server builder canon
- mTLS / client cert
- advanced cert workflows
- capability / fallback / runtime diagnostics

### Phase E — 纯 Pascal 后端扩展与优化

**目标**
- 从 M1 客户端生产可用扩展到更完整 TLS 能力

**候选**
- mTLS
- 会话恢复
- 更强性能优化
- 更多 cipher / extension
- 更丰富 observability

---

## 6. 当前建议的立即执行顺序

### Wave 1: API Canon 文档化
- 梳理并重写：
  - `docs/reference/ARCHITECTURE.md`
  - `README.md`
  - `docs/README.md`
- 目标：
  - 写出 Core / Advanced / Backend-specific 三层
  - 写清 `Builder / Factory / Config / Connector` 关系
  - 写清兼容层与推荐路径

### Wave 2: Core API Contract Index
- 新建一份 “API contract current index”
- 汇总当前已经收口的 contract：
  - backend resolution
  - request vs library scope
  - file/PEM/PKCS11 precedence
  - ServerName precedence
  - observability/report-surface governance

### Wave 3: 纯 Pascal Client M1 契约清单
- 把 M1 的 10 条验收标准拆成 focused test/contracts
- 标出：
  - 已满足
  - 部分满足
  - 缺失

### Wave 4: 入口瘦身与兼容策略
- 盘点：
  - 哪些入口继续保留
  - 哪些入口只做兼容桥接
  - 哪些入口应标 deprecated

### Wave 5: helper 抽取与实现期准备
- 在不改外部语义前提下，抽 `BuildClient` / `BuildServer` 共用 material-loading helper
- 为进入实现期降低重复分支风险

---

## 7. DoD（路线图阶段完成标准）

### API 设计阶段完成，必须同时满足
- 有清晰的分层 API 文档
- 有主入口/兼容入口/废弃入口的明确定义
- 核心 contract 已通过 focused tests 锁定
- 主要 docs 已同步到当前真相
- 纯 Pascal Client M1 有明确验收标准与缺口清单

### 进入实现期前，不满足以下任一条都不算完成
- `TSSLContextBuilder` 未成为明确主入口
- `Factory + Config` 仍承担不清晰的双重职责
- Core API 与 backend-specific API 仍混在一起
- README / docs / tests 对同一语义给出相互冲突的结论

---

## 8. 执行协议（后续默认工作方式）

路线图确认后，默认采用以下执行协议：

- 我按路线图持续推进，不为每个小波次单独停下
- 只有以下情况才中断询问：
  1. 真阻塞
  2. 需要你拍板的设计分叉
  3. 路线图被新发现推翻
  4. 一个阶段已完成，需要你验收

- 每一波仍保持：
  - TDD / contract-first
  - focused verification
  - `task_plan.md` / `findings.md` / `progress.md` 回写
  - 月度汇总同步

---

## 9. 当前状态结论

当前已经具备进入路线图驱动执行的前提：

- builder / factory / backend 的若干高风险 contract 已连续收口
- PKCS11 / file / PEM / ServerName / backend resolution 已有一批真实 contract
- repo-noise / report-surface 主线已明显下降
- 现阶段最值得做的，不再是零散修补，而是先冻结 API canon

## 10. 下一执行波（默认）

默认下一波开始执行：

1. 重写并升级 `docs/reference/ARCHITECTURE.md` 为 API canon 文档
2. 生成 API 分层与入口关系图
3. 抽出“Core API 设计冻结”第一批 contract/doc 清单

