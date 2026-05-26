# Linux快速开始指南

本指南帮助 Linux 用户快速搭建 `fafafa.ssl` 开发环境并运行第一个示例。

## 当前推荐入口

如果你是在继续当前工程的验证或收口，先看这两页，再按下面的默认命令推进：

- `python3 scripts/compile_all_modules.py`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local`

## 系统要求

### 操作系统

- Ubuntu 20.04+ / Debian 11+
- Fedora 35+ / RHEL 8+
- Arch Linux (最新)
- 其他主流Linux发行版（x86_64）

### 必需软件

- **Free Pascal Compiler (FPC)** ≥ 3.2.0
- **OpenSSL** ≥ 3.0.0 (推荐) 或 1.1.1+
- **Git** (克隆仓库)
- **Python 3** (运行当前默认编译门禁)

### 可选软件

- **Lazarus IDE** ≥ 2.2.0 (图形化开发)

---

## 快速安装 (5分钟)

### Ubuntu/Debian

```bash
# 1. 安装依赖
sudo apt-get update
sudo apt-get install -y \
    fpc \
    fp-units-fcl \
    libssl3 \
    libssl-dev \
    git \
    python3

# 2. 克隆项目
git clone https://github.com/dtamade/fafafa.ssl.git
cd fafafa.ssl

# 3. 运行默认编译门禁
python3 scripts/compile_all_modules.py

# 4. 运行本地最小门禁
bash scripts/run_minimal_ci_gate.sh --fast-local

# 5. 可选：检查 Phase 2 入口
bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local
```

### Fedora/RHEL

```bash
# 1. 安装依赖
sudo dnf install -y \
    fpc \
    openssl \
    openssl-devel \
    git \
    python3

# 2. 克隆和验证（同上）
git clone https://github.com/dtamade/fafafa.ssl.git
cd fafafa.ssl
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local
```

### Arch Linux

```bash
# 1. 安装依赖
sudo pacman -S --needed fpc openssl git python

# 2. 克隆和验证（同上）
git clone https://github.com/dtamade/fafafa.ssl.git
cd fafafa.ssl
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local
```

---

## 验证安装

### 检查FPC版本

```bash
fpc -iV
# 期望输出: 3.2.2 或更高
```

### 检查OpenSSL版本

```bash
openssl version
# 期望输出: OpenSSL 3.0.x 或 1.1.1x
```

### 检查FCL单元

```bash
ls /usr/lib/fpc/$(fpc -iV)/fcl-base/
# 应该看到: base64.ppu, base64.o 等
```

---

## 第一个示例程序

### 示例1: 检测SSL库

创建 `test_detect.pas`:

```pascal
program test_detect;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl;

var
  Lib: ISSLLibrary;
begin
  // 自动检测最佳SSL库（Linux上是OpenSSL）
  Lib := TSSLFactory.GetLibraryInstance(sslAutoDetect);
  WriteLn('检测到: ', LibraryTypeToString(Lib.GetLibraryType));

  if Lib.Initialize then
  begin
    WriteLn('版本: ', Lib.GetVersionString);
    WriteLn('✓ SSL库初始化成功！');
  end
  else
    WriteLn('✗ 初始化失败');
end.
```

编译并运行：

```bash
fpc -Fusrc test_detect.pas
./test_detect

# 期望输出:
# 检测到: OpenSSL
# 版本: OpenSSL 3.0
# ✓ SSL库初始化成功！
```

### 示例2: SHA256哈希计算

创建 `test_hash.pas`:

```pascal
program test_hash;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.utils;

var
  Data: string;
  Hash: string;
begin
  Data := 'Hello, fafafa.ssl!';
  
  Hash := SHA256Hash(Data);
  WriteLn('数据: ', Data);
  WriteLn('SHA256: ', Hash);
end.
```

编译并运行：

```bash
fpc -Fusrc test_hash.pas
    
./test_hash

# 期望输出:
# 数据: Hello, fafafa.ssl!
# SHA256: 1a2b3c...（64位十六进制）
```

### 示例3: SSL上下文创建

参考 `examples/01_tls_client.pas`:

```bash
# 编译示例
fpc -Fu./src -Fu./examples examples/01_tls_client.pas

# 运行
./01_tls_client
```

---

## 在Lazarus中使用

### 1. 打开Lazarus IDE

```bash
lazarus-ide
```

### 2. 安装fafafa.ssl包

1. 菜单: **Package** → **Open Package File (.lpk)**
2. 选择: `fafafa_ssl.lpk`
3. 点击: **Compile**
4. 点击: **Use** → **Add to Project**

### 3. 创建新项目

1. 菜单: **Project** → **New Project** → **Application**
2. 右键项目 → **Inspector** → **Required Packages**
3. 添加: `fafafa_ssl`
4. 编写代码并运行

---

## 常见问题

### Q: 编译时报 "Can't find unit fafafa.ssl"

**A**: 未指定src路径

```bash
# 解决方案1: 手动指定
fpc -Fusrc your_program.pas

# 解决方案2: 先确认当前默认编译门禁可通过
python3 scripts/compile_all_modules.py
```

### Q: 运行时报 "libcrypto.so.3: cannot open shared object file"

**A**: OpenSSL未安装

```bash
# Ubuntu/Debian
sudo apt-get install libssl3

# Fedora
sudo dnf install openssl-libs
```

### Q: 编译时报 "Can't find unit base64"

**A**: FCL未安装

```bash
# Ubuntu/Debian
sudo apt-get install fp-units-fcl

# 验证
ls /usr/lib/fpc/$(fpc -iV)/fcl-base/base64.ppu
```

参考: [FCL依赖文档](FCL_DEPENDENCIES.md)

### Q: 测试失败

**A**: 检查测试输出

```bash
# 运行单个测试查看详细信息
mkdir -p tmp/linux_quickstart_test
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
    -FUtmp/linux_quickstart_test \
    -FEtmp/linux_quickstart_test \
    -otmp/linux_quickstart_test/test_openssl_simple \
    tests/test_openssl_simple.pas
./tmp/linux_quickstart_test/test_openssl_simple

# 运行当前最小门禁
bash scripts/run_minimal_ci_gate.sh --fast-local
```

### Q: 如何指定OpenSSL路径？

**A**: 使用环境变量

```bash
export LD_LIBRARY_PATH=/custom/path/to/openssl/lib:$LD_LIBRARY_PATH
fpc -Fusrc your_program.pas
./your_program
```

---

## 项目结构

```
fafafa.ssl/
├── src/                    # 核心源代码
│   ├── fafafa.ssl.pas                # 主门面 / 当前普通入口
│   ├── fafafa.ssl.context.builder.pas # 推荐 context builder 入口
│   ├── fafafa.ssl.factory.pas        # core factory surface / direct-library helper
│   ├── fafafa.ssl.openssl.backed.pas # OpenSSL ISSLLibrary 实现
│   └── ...
├── examples/               # 示例程序
│   ├── 01_tls_client.pas
│   ├── 02_certificate_validation.pas
│   └── ...
├── scripts/                # 自动化脚本
│   ├── compile_all_modules.py      # 当前默认编译门禁
│   ├── run_minimal_ci_gate.sh      # 当前默认本地最小门禁
│   └── run_phase2_performance_baseline.sh  # Phase 2 入口探测
├── tests/                  # 测试套件
│   ├── test_openssl_simple.pas
│   └── ...
├── docs/                   # 文档
│   ├── LINUX_QUICKSTART.md (本文档)
│   ├── FCL_DEPENDENCIES.md
│   └── ...
├── build_linux.sh          # 历史兼容构建脚本（非默认入口）
├── fafafa_ssl.lpk          # Lazarus包配置
└── README.md               # 项目主文档
```

---

## 下一步

3. **浏览主文档**: [README.md](../README.md)
4. **查看详细快速开始**: [QUICKSTART.md](QUICKSTART.md)
5. **查看 API**: [API_REFERENCE.md](../reference/API_REFERENCE.md)

---

## 性能提示

### 编译优化

```bash
# 开发模式（快速编译，带调试信息）
fpc -O1 -g -gl -Fusrc your_program.pas

# 发布模式（最大优化，无调试信息）
fpc -O3 -CX -XX -Xs -Fusrc your_program.pas
```

### 运行时优化

- 使用OpenSSL 3.x（比1.1.x快约15-20%）
- 启用系统CA自动加载（减少手动配置）
- 重用SSL Context（避免重复初始化）

---

## 获取帮助

- **GitHub Issues**: https://github.com/dtamade/fafafa.ssl/issues
- **文档索引**: [DOCUMENTATION_INDEX.md](../DOCUMENTATION_INDEX.md)
- **常见问题**: [FAQ.md](FAQ.md)

---

**更新日期**: 2026-05-21
**适用版本**: fafafa.ssl v1.5.0
**维护者**: fafafa.ssl团队
