# GitHub Project 看板设置指南

**创建日期**: 2026-01-30  
**目的**: 为阶段 1 任务创建可视化项目看板

---

## 📋 快速设置步骤

### 1. 创建新项目

1. 访问 GitHub 仓库：https://github.com/dtamade/fafafa.ssl
2. 点击顶部导航栏的 **"Projects"** 标签
3. 点击 **"New project"** 按钮
4. 选择 **"Board"** 模板（看板视图）
5. 项目名称：`Phase 1: Security & Performance Enhancement`
6. 描述：`阶段 1：安全与性能强化（2026-02 ~ 2026-04）`
7. 点击 **"Create project"**

### 2. 配置看板列

创建以下列（从左到右）：

| 列名 | 描述 | 自动化规则 |
|------|------|-----------|
| 📋 **Backlog** | 待规划任务 | - |
| 🎯 **Ready** | 准备开始 | - |
| 🚧 **In Progress** | 进行中 | 自动：Issue 状态变为 "In Progress" |
| 👀 **In Review** | 审查中 | - |
| ✅ **Done** | 已完成 | 自动：Issue 关闭时移动到此列 |

**设置自动化**：
1. 点击列右上角的 `⋯` 菜单
2. 选择 **"Manage automation"**
3. 启用相应的自动化规则

### 3. 添加 Issues 到项目

#### 方法 A：批量添加（推荐）
1. 在项目页面，点击 **"+ Add item"**
2. 搜索并选择以下 Issues：
   - #2 - 🔒 实现证书透明度（CT）支持 - SCT 验证
   - #3 - 🔒 实现证书透明度（CT）支持 - CT 日志服务器集成
   - #4 - 🔒 完善 OCSP Stapling - 增强生产级实现
   - #5 - 🔒 完善 OCSP Stapling - 添加 OCSP 响应缓存
   - #6 - ⚡ 会话缓存优化 - 实现 TLS 1.3 会话票据持久化
   - #7 - ⚡ 会话缓存优化 - 添加会话缓存统计和监控
   - #8 - ⚡ 会话缓存优化 - 优化会话查找算法
   - #9 - 🍎 macOS 平台验证 - 完成 macOS CI/CD 集成
   - #10 - 🍎 macOS 平台验证 - 验证 OpenSSL 3.x 兼容性
   - #11 - 🍎 macOS 平台验证 - 添加系统根证书加载
   - #12 - ⚡ 添加性能回归测试到 CI

#### 方法 B：使用 GitHub CLI
```bash
# 首先获取项目 ID
gh project list --owner dtamade

# 然后添加 Issues（替换 PROJECT_ID）
for issue in 2 3 4 5 6 7 8 9 10 11 12; do
  gh project item-add PROJECT_ID --owner dtamade --url "https://github.com/dtamade/fafafa.ssl/issues/$issue"
done
```

### 4. 组织任务到列

将 Issues 拖放到相应的列：

**📋 Backlog**（暂无）

**🎯 Ready**（准备开始 - 2026-02-01）：
- #2 - SCT 验证
- #3 - CT 日志服务器集成

**🚧 In Progress**（当前无）

**👀 In Review**（当前无）

**✅ Done**（当前无）

### 5. 添加自定义字段（可选）

增强项目跟踪能力：

1. 点击项目右上角的 **"⋯"** → **"Settings"**
2. 在 **"Fields"** 部分，添加以下字段：

| 字段名 | 类型 | 选项 |
|--------|------|------|
| **Priority** | Single select | 🔴 High, 🟡 Medium, 🟢 Low |
| **Category** | Single select | 🔒 Security, ⚡ Performance, 🍎 Platform, 🔧 CI/CD |
| **Estimated Effort** | Single select | 1 week, 2 weeks, 3 weeks, 4 weeks |
| **Start Date** | Date | - |
| **Target Date** | Date | - |

3. 为每个 Issue 填写这些字段

### 6. 创建视图

创建不同的视图来查看任务：

#### 视图 1：按优先级
1. 点击 **"+ New view"**
2. 名称：`By Priority`
3. 布局：**Board**
4. 分组：**Priority**

#### 视图 2：按类别
1. 点击 **"+ New view"**
2. 名称：`By Category`
3. 布局：**Board**
4. 分组：**Category**

#### 视图 3：时间线
1. 点击 **"+ New view"**
2. 名称：`Timeline`
3. 布局：**Roadmap**
4. 日期字段：**Start Date** 和 **Target Date**

---

## 🎨 项目看板最佳实践

### 任务状态管理

| 状态 | 何时移动 | 负责人 |
|------|---------|--------|
| **Backlog** | 任务创建时 | 项目经理 |
| **Ready** | 任务准备好开始，依赖已满足 | 项目经理 |
| **In Progress** | 开始工作时 | 开发者 |
| **In Review** | 代码完成，等待审查 | 开发者 |
| **Done** | Issue 关闭，任务完成 | 自动 |

### 每日更新

**开发者职责**：
1. 每天开始工作前，将任务移动到 **In Progress**
2. 在 Issue 中添加进度评论
3. 完成后移动到 **In Review**
4. 审查通过后关闭 Issue（自动移动到 **Done**）

**项目经理职责**：
1. 每周审查 **Backlog** 和 **Ready** 列
2. 确保 **In Progress** 列不超过 3 个任务（避免并行过多）
3. 跟踪 **In Review** 列，确保及时审查

---

## 📊 项目指标跟踪

### 关键指标

在项目 **Insights** 标签中查看：

1. **Velocity**（速度）：每周完成的任务数
2. **Cycle Time**（周期时间）：从 Ready 到 Done 的平均时间
3. **WIP Limit**（在制品限制）：In Progress 列的任务数（建议 ≤ 3）
4. **Burndown**（燃尽图）：剩余任务数随时间的变化

### 每周报告

每周五生成报告：

```markdown
## 阶段 1 周报（Week X）

### 本周完成
- [ ] Issue #X - 任务名称

### 进行中
- [ ] Issue #Y - 任务名称

### 下周计划
- [ ] Issue #Z - 任务名称

### 风险和阻塞
- 无 / [描述风险]

### 指标
- 完成任务数：X
- 平均周期时间：Y 天
- 当前 WIP：Z
```

---

## 🔗 相关链接

- **项目看板**：https://github.com/dtamade/fafafa.ssl/projects/1
- **所有阶段 1 Issues**：https://github.com/dtamade/fafafa.ssl/issues?q=is%3Aissue+is%3Aopen+label%3Aphase-1
- **高优先级任务**：https://github.com/dtamade/fafafa.ssl/issues?q=is%3Aissue+is%3Aopen+label%3Ahigh-priority
- **阶段 1 实施计划**：[docs/PHASE_1_SECURITY_PERFORMANCE_PLAN.md](PHASE_1_SECURITY_PERFORMANCE_PLAN.md)

---

## 🛠️ 故障排查

### 问题：无法创建项目
**解决方案**：确保你有仓库的 **Admin** 或 **Write** 权限

### 问题：自动化规则不工作
**解决方案**：
1. 检查项目设置中的 **Workflows** 是否启用
2. 确保 Issue 状态字段正确设置

### 问题：Issues 不显示在项目中
**解决方案**：
1. 确保 Issues 已添加到项目
2. 检查视图的过滤器设置

---

## 📝 下一步

1. ✅ 创建项目看板
2. ✅ 添加所有 Issues
3. ✅ 配置自动化规则
4. ✅ 设置自定义字段
5. ✅ 创建多个视图
6. 📋 开始使用看板跟踪进度

---

**文档维护**:  
- 创建者: Claude Code (Sisyphus)  
- 创建日期: 2026-01-30  
- 版本: 1.0  
- 下次审查: 2026-02-15
