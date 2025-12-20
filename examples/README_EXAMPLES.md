# fafafa.ssl 示例程序

## ✅ 可运行的示例

### 1. ultra_simple_test.pas
**状态**: ✅ 编译通过，可运行

**用途**: 验证编译器和基本功能

**编译**:
```bash
cd examples
fpc ultra_simple_test.pas
./ultra_simple_test
```

**输出**: 
- 测试编译器
- 测试类型系统
- 确认环境配置正确

---

## ⚠️  需要依赖的示例

以下示例需要先安装FreePascal的base64单元：

### 2. simple_test.pas
**状态**: ⚠️  需要base64单元

**用途**: 测试基本SSL功能（哈希、Base64、SSL支持检查）

**依赖**:
```bash
# 可能需要安装
sudo apt-get install fp-units-fcl
```

### 3. https_client_complete.pas  
**状态**: ⚠️  需要完整SSL库

**用途**: 完整的HTTPS客户端示例

---

## 📝 使用说明

### 快速开始

1. **验证编译器**:
```bash
cd examples
fpc ultra_simple_test.pas && ./ultra_simple_test
```

2. **安装依赖** (如果需要):
```bash
# Debian/Ubuntu
sudo apt-get install fp-units-fcl fp-units-net

# Fedora/RHEL
sudo yum install fpc-units-base

# Arch Linux
sudo pacman -S fpc
```

3. **测试基本功能** (安装依赖后):
```bash
fpc -Fu../src -Fu../src/openssl simple_test.pas
./simple_test
```

---

## 🐛 已知问题

1. **base64单元**: FreePascal的base64可能需要额外安装
2. **Sockets单元**: 需要fp-units-net包

---

## 💡 建议

如果遇到编译问题，从 `ultra_simple_test.pas` 开始，
它不需要任何外部依赖，可以验证环境配置。

然后逐步尝试更复杂的示例。


