# Linux快速开始指南

本指南帮助Linux用户快速搭建`fafafa.ssl`开发环境并运行第一个示例。

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

### 可选软件

- **Lazarus IDE** ≥ 2.2.0 (图形化开发)
- **Python 3** (运行构建脚本)

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
    git

# 2. 克隆项目
git clone https://github.com/yourusername/fafafa.ssl.git
cd fafafa.ssl

# 3. 构建项目
chmod +x build_linux.sh run_tests_linux.sh
./build_linux.sh

# 4. 运行测试
./run_tests_linux.sh
```

### Fedora/RHEL

```bash
# 1. 安装依赖
sudo dnf install -y \
    fpc \
    openssl \
    openssl-devel \
    git

# 2. 克隆和构建（同上）
git clone https://github.com/yourusername/fafafa.ssl.git
cd fafafa.ssl
chmod +x build_linux.sh run_tests_linux.sh
./build_linux.sh
./run_tests_linux.sh
```

### Arch Linux

```bash
# 1. 安装依赖
sudo pacman -S fpc openssl git

# 2. 克隆和构建（同上）
git clone https://github.com/yourusername/fafafa.ssl.git
cd fafafa.ssl
chmod +x build_linux.sh run_tests_linux.sh
./build_linux.sh
./run_tests_linux.sh
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
  fafafa.ssl.factory;

var
  LibType: TSSLLibraryType;
  Lib: ISSLLibrary;
begin
  // 自动检测最佳SSL库（Linux上是OpenSSL）
  LibType := DetectBestLibrary;
  WriteLn('检测到: ', GetLibraryTypeName(LibType));
  
  // 创建库实例
  Lib := GetLibraryInstance(LibType);
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
# 检测到: sslOpenSSL
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
fpc -Fu$HOME/freePascal/fpc/units/x86_64-linux/rtl-objpas \
    -Fu$HOME/freePascal/fpc/units/x86_64-linux/fcl-base \
    -Fusrc \
    test_hash.pas
    
./test_hash

# 期望输出:
# 数据: Hello, fafafa.ssl!
# SHA256: 1a2b3c...（64位十六进制）
```

### 示例3: SSL上下文创建

参考 `examples/01_basic_ssl_client.pas`:

```bash
# 编译示例
fpc -Fu$HOME/freePascal/fpc/units/x86_64-linux/rtl-objpas \
    -Fu$HOME/freePascal/fpc/units/x86_64-linux/fcl-base \
    -Fu$HOME/freePascal/fpc/units/x86_64-linux/fcl-json \
    -Fusrc \
    examples/01_basic_ssl_client.pas

# 运行
./examples/01_basic_ssl_client
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

### Q: 编译时报 "Can't find unit fafafa.ssl.factory"

**A**: 未指定src路径

```bash
# 解决方案1: 手动指定
fpc -Fusrc your_program.pas

# 解决方案2: 使用构建脚本
./build_linux.sh
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
fpc -Fusrc tests/test_openssl_simple.pas
./tests/test_openssl_simple

# 运行完整测试套件
./run_tests_linux.sh
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
│   ├── fafafa.ssl.factory.pas    # 工厂模式（推荐入口）
│   ├── fafafa.ssl.openssl.pas    # OpenSSL后端
│   └── ...
├── examples/               # 示例程序
│   ├── 01_basic_ssl_client.pas
│   ├── 02_certificate_validation.pas
│   └── ...
├── tests/                  # 测试套件
│   ├── test_openssl_simple.pas
│   └── ...
├── docs/                   # 文档
│   ├── LINUX_QUICKSTART.md (本文档)
│   ├── FCL_DEPENDENCIES.md
│   └── ...
├── build_linux.sh          # Linux构建脚本
├── run_tests_linux.sh      # Linux测试脚本
├── fafafa_ssl.lpk          # Lazarus包配置
└── README.md               # 项目主文档
```

---

## 下一步

1. **阅读主文档**: [README.md](../README.md)
2. **浏览示例**: [examples/](../examples/)
3. **查看API**: [docs/API_REFERENCE.md](API_REFERENCE.md)
4. **了解架构**: [PROJECT_VISION.md](../PROJECT_VISION.md)
5. **贡献代码**: [CONTRIBUTING.md](../CONTRIBUTING.md)

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

- **GitHub Issues**: https://github.com/yourusername/fafafa.ssl/issues
- **文档索引**: [DOCUMENTATION_INDEX.md](../DOCUMENTATION_INDEX.md)
- **常见问题**: [FAQ.md](FAQ.md)

---

**更新日期**: 2025-10-28  
**适用版本**: fafafa.ssl v1.0.0-rc  
**维护者**: fafafa.ssl团队

