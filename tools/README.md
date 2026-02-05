# 能力矩阵工具

**版本**: v1.2.0
**目录**: `tools/`

本目录包含用于生成、序列化和可视化 fafafa.ssl 后端能力矩阵的工具。

---

## 🛠️ 工具列表

### 1. capability_visualizer.html

**功能**: Web 可视化工具，对比不同后端的能力

**特性**:
- 📊 直观的卡片式展示
- 📈 安全和性能评分可视化
- 📋 详细的功能对比表
- 🎨 现代化的 UI 设计
- 💾 支持加载 JSON 文件
- 🔍 示例数据内置

**使用方式**:

方式1：使用启动脚本（推荐）
```bash
./tools/visualize_capabilities.sh
```

方式2：手动使用
```bash
# 1. 生成 JSON 文件
./tests/test_capability_serialization

# 2. 在浏览器中打开
firefox tools/capability_visualizer.html
# 或
xdg-open tools/capability_visualizer.html

# 3. 在页面中加载生成的 JSON 文件
```

方式3：快速预览
```bash
# 直接在浏览器中打开，点击 "加载示例数据" 按钮
```

### 2. visualize_capabilities.sh

**功能**: 自动化脚本，一键生成并可视化

**特性**:
- 自动编译测试程序（如需要）
- 自动生成所有后端的 JSON 文件
- 自动在浏览器中打开可视化工具
- 跨平台支持（Linux/macOS）

**使用方式**:
```bash
cd /path/to/fafafa.ssl
./tools/visualize_capabilities.sh
```

---

## 📋 工作流程

### 完整工作流程

```bash
# 1. 编译测试程序
fpc -B -Fu./src tests/test_capability_serialization.pas

# 2. 运行测试生成 JSON 文件
./tests/test_capability_serialization

# 3. 查看生成的文件
ls -lh capability_*.json

# 4. 打开可视化工具
./tools/visualize_capabilities.sh
```

### 快速工作流程

```bash
# 一键完成所有步骤
./tools/visualize_capabilities.sh
```

---

## 📁 输出文件

测试程序会生成以下文件：

```
capability_openssl.json     # OpenSSL 后端能力
capability_openssl.xml      # OpenSSL 后端能力（XML 格式）
capability_wolfssl.json     # WolfSSL 后端能力（如可用）
capability_mbedtls.json     # MbedTLS 后端能力（如可用）
capability_winssl.json      # WinSSL 后端能力（如可用）
```

### JSON 格式示例

```json
{
  "supportsTLS13": true,
  "supportsALPN": true,
  "supportsSNI": true,
  "backendType": 1,
  "backendImplType": 1,
  "backendVersion": "OpenSSL 3.5.4 30 Sep 2025",
  "supportsDTLS": true,
  "sniSupport": "stable",
  "alpnSupport": "stable",
  "hasHardwareAcceleration": true,
  "hasSIMDOptimization": true,
  "supportsSystemCertStore": false,
  "supportsPKCS11": true,
  "supportsTPM": false,
  "compatibilityLevel": 100,
  "knownIssues": ""
}
```

---

## 🎨 可视化工具功能

### 卡片视图

每个后端显示为一张卡片，包含：
- 后端名称和版本
- 实现类型（Native/C Library/OS Native）
- 安全评分（0-100）
- 性能评分（0-100）
- 功能特性网格（12+ 个特性）

### 对比表

显示所有已加载后端的详细对比：
- 评分对比
- 协议支持对比
- 算法支持对比
- 平台特性对比

### 交互功能

- 文件拖拽上传
- 多文件同时加载
- 示例数据快速预览
- 响应式设计（移动端友好）

---

## 🔧 开发和扩展

### 添加新的可视化指标

编辑 `capability_visualizer.html` 中的 `renderFeature()` 调用：

```javascript
${renderFeature('🆕', '新功能', caps.yourNewFeature)}
```

### 添加新的对比维度

编辑 `renderComparisonTable()` 函数中的 `features` 数组：

```javascript
const features = [
    // ... 现有特性 ...
    { name: '新特性', key: 'yourNewFeature' }
];
```

### 自定义样式

修改 `<style>` 部分的 CSS 变量和样式。

---

## 📊 使用场景

### 1. 选择后端

在项目开始时，使用可视化工具对比不同后端的特性，选择最适合的后端。

### 2. 性能评估

对比不同后端的性能评分，选择高性能后端。

### 3. 安全审计

检查后端的安全特性，确保符合安全要求。

### 4. 文档生成

导出后端能力矩阵作为项目文档的一部分。

### 5. CI/CD 集成

在 CI/CD 流程中自动生成能力矩阵报告。

---

## 🐛 故障排除

### 问题：浏览器无法打开文件

**解决方案**:
```bash
# 手动复制文件路径
realpath tools/capability_visualizer.html

# 在浏览器地址栏输入: file:///完整路径
```

### 问题：JSON 文件未生成

**解决方案**:
```bash
# 手动运行测试程序
./tests/test_capability_serialization

# 检查错误信息
echo $?
```

### 问题：后端不可用

**解决方案**:
- OpenSSL: 确保系统已安装 OpenSSL
- WolfSSL: 需要编译时定义 ENABLE_WOLFSSL
- MbedTLS: 需要编译时定义 ENABLE_MBEDTLS
- WinSSL: 仅 Windows 平台可用

---

## 📚 相关文档

- **能力矩阵指南**: `docs/CAPABILITY_MATRIX_GUIDE.md`
- **API 参考**: `docs/reference/API_REFERENCE.md`
- **迁移指南**: `docs/MIGRATION_GUIDE_V1.1.md`

---

## 🤝 贡献

欢迎改进可视化工具！可以添加：
- 更多图表类型（雷达图、柱状图）
- 数据导出功能（CSV、PDF）
- 搜索和筛选功能
- 更多的可视化指标

---

**创建日期**: 2026-02-05
**作者**: fafafa.ssl 团队
**许可**: 与项目相同
