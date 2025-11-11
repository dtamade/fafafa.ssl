# fafafa.ssl 文件重构执行计划

## 概述

根据 `ARCHITECTURE_FILE_ORGANIZATION.md` 的设计，本文档提供详细的重构步骤，确保文件组织符合新架构规范。

**当前状态：** 已完成 Git 提交（commit: 6190a4b）  
**目标：** 按照新规范重新组织所有文件，消除重复，建立清晰的层次结构

---

## 第一阶段：OpenSSL 文件重命名

### 当前 OpenSSL 文件结构问题

```
❌ 当前（混乱）:
src/fafafa.ssl.openssl.aes.pas      // 应该是 api.aes
src/fafafa.ssl.openssl.bio.pas      // 应该是 api.bio
src/fafafa.ssl.openssl.bn.pas       // 应该是 api.bn
...
```

### 目标结构

```
✅ 目标（清晰）:
src/fafafa.ssl.openssl.pas              // 主实现
src/fafafa.ssl.openssl.types.pas        // 类型定义（需创建/整合）
src/fafafa.ssl.openssl.api.pas          // 基础 API（需创建/整合）
src/fafafa.ssl.openssl.api.aes.pas      // AES 模块
src/fafafa.ssl.openssl.api.bio.pas      // BIO 模块
src/fafafa.ssl.openssl.api.bn.pas       // BigNum 模块
...
```

### 重命名映射表

| 旧文件名 | 新文件名 | 说明 |
|---------|---------|------|
| `fafafa.ssl.openssl.aead.pas` | `fafafa.ssl.openssl.api.aead.pas` | API 模块 |
| `fafafa.ssl.openssl.aes.pas` | `fafafa.ssl.openssl.api.aes.pas` | API 模块 |
| `fafafa.ssl.openssl.aria.pas` | `fafafa.ssl.openssl.api.aria.pas` | API 模块 |
| `fafafa.ssl.openssl.asn1.pas` | `fafafa.ssl.openssl.api.asn1.pas` | API 模块 |
| `fafafa.ssl.openssl.async.pas` | `fafafa.ssl.openssl.api.async.pas` | API 模块 |
| `fafafa.ssl.openssl.bio.pas` | `fafafa.ssl.openssl.api.bio.pas` | API 模块 |
| `fafafa.ssl.openssl.blake2.pas` | `fafafa.ssl.openssl.api.blake2.pas` | API 模块 |
| `fafafa.ssl.openssl.bn.pas` | `fafafa.ssl.openssl.api.bn.pas` | API 模块 |
| `fafafa.ssl.openssl.buffer.pas` | `fafafa.ssl.openssl.api.buffer.pas` | API 模块 |
| `fafafa.ssl.openssl.chacha.pas` | `fafafa.ssl.openssl.api.chacha.pas` | API 模块 |
| `fafafa.ssl.openssl.cmac.pas` | `fafafa.ssl.openssl.api.cmac.pas` | API 模块 |
| `fafafa.ssl.openssl.cmac.evp.pas` | `fafafa.ssl.openssl.api.cmac.evp.pas` | API 模块 |
| `fafafa.ssl.openssl.cms.pas` | `fafafa.ssl.openssl.api.cms.pas` | API 模块 |
| `fafafa.ssl.openssl.comp.pas` | `fafafa.ssl.openssl.api.comp.pas` | API 模块 |
| `fafafa.ssl.openssl.conf.pas` | `fafafa.ssl.openssl.api.conf.pas` | API 模块 |
| `fafafa.ssl.openssl.consts.pas` | `fafafa.ssl.openssl.api.consts.pas` | 常量（可能合并到 types） |
| `fafafa.ssl.openssl.core.pas` | `fafafa.ssl.openssl.api.core.pas` | 核心 API |
| `fafafa.ssl.openssl.crypto.pas` | `fafafa.ssl.openssl.api.crypto.pas` | 加密 API |
| `fafafa.ssl.openssl.ct.pas` | `fafafa.ssl.openssl.api.ct.pas` | 证书透明度 |
| `fafafa.ssl.openssl.des.pas` | `fafafa.ssl.openssl.api.des.pas` | DES 算法 |
| `fafafa.ssl.openssl.dh.pas` | `fafafa.ssl.openssl.api.dh.pas` | DH 密钥交换 |
| `fafafa.ssl.openssl.dsa.pas` | `fafafa.ssl.openssl.api.dsa.pas` | DSA 签名 |
| `fafafa.ssl.openssl.dso.pas` | `fafafa.ssl.openssl.api.dso.pas` | 动态对象 |
| `fafafa.ssl.openssl.ec.pas` | `fafafa.ssl.openssl.api.ec.pas` | 椭圆曲线 |
| `fafafa.ssl.openssl.ecdh.pas` | `fafafa.ssl.openssl.api.ecdh.pas` | ECDH |
| `fafafa.ssl.openssl.ecdsa.pas` | `fafafa.ssl.openssl.api.ecdsa.pas` | ECDSA |
| `fafafa.ssl.openssl.engine.pas` | `fafafa.ssl.openssl.api.engine.pas` | 引擎 |
| `fafafa.ssl.openssl.err.pas` | `fafafa.ssl.openssl.api.err.pas` | 错误处理 |
| `fafafa.ssl.openssl.evp.pas` | `fafafa.ssl.openssl.api.evp.pas` | EVP 接口 |
| `fafafa.ssl.openssl.hmac.pas` | `fafafa.ssl.openssl.api.hmac.pas` | HMAC |
| `fafafa.ssl.openssl.kdf.pas` | `fafafa.ssl.openssl.api.kdf.pas` | 密钥派生 |
| `fafafa.ssl.openssl.legacy_ciphers.pas` | `fafafa.ssl.openssl.api.legacy_ciphers.pas` | 遗留算法 |
| `fafafa.ssl.openssl.lhash.pas` | `fafafa.ssl.openssl.api.lhash.pas` | 哈希表 |
| `fafafa.ssl.openssl.md.pas` | `fafafa.ssl.openssl.api.md.pas` | 消息摘要 |
| `fafafa.ssl.openssl.modes.pas` | `fafafa.ssl.openssl.api.modes.pas` | 加密模式 |
| `fafafa.ssl.openssl.obj.pas` | `fafafa.ssl.openssl.api.obj.pas` | 对象标识符 |
| `fafafa.ssl.openssl.ocsp.pas` | `fafafa.ssl.openssl.api.ocsp.pas` | OCSP |
| `fafafa.ssl.openssl.param.pas` | `fafafa.ssl.openssl.api.param.pas` | 参数 |
| `fafafa.ssl.openssl.pem.pas` | `fafafa.ssl.openssl.api.pem.pas` | PEM 格式 |
| `fafafa.ssl.openssl.pkcs.pas` | `fafafa.ssl.openssl.api.pkcs.pas` | PKCS |
| `fafafa.ssl.openssl.pkcs7.pas` | `fafafa.ssl.openssl.api.pkcs7.pas` | PKCS#7 |
| `fafafa.ssl.openssl.pkcs12.pas` | `fafafa.ssl.openssl.api.pkcs12.pas` | PKCS#12 |
| `fafafa.ssl.openssl.provider.pas` | `fafafa.ssl.openssl.api.provider.pas` | Provider |
| `fafafa.ssl.openssl.rand.pas` | `fafafa.ssl.openssl.api.rand.pas` | 随机数 |
| `fafafa.ssl.openssl.rsa.pas` | `fafafa.ssl.openssl.api.rsa.pas` | RSA |
| `fafafa.ssl.openssl.sha.pas` | `fafafa.ssl.openssl.api.sha.pas` | SHA |
| `fafafa.ssl.openssl.ssl.pas` | `fafafa.ssl.openssl.api.ssl.pas` | SSL/TLS |
| `fafafa.ssl.openssl.stack.pas` | `fafafa.ssl.openssl.api.stack.pas` | 栈结构 |
| `fafafa.ssl.openssl.txt_db.pas` | `fafafa.ssl.openssl.api.txt_db.pas` | 文本数据库 |
| `fafafa.ssl.openssl.types.pas` | `fafafa.ssl.openssl.types.pas` | **保持不变** |
| `fafafa.ssl.openssl.ui.pas` | `fafafa.ssl.openssl.api.ui.pas` | 用户界面 |
| `fafafa.ssl.openssl.x509.pas` | `fafafa.ssl.openssl.api.x509.pas` | X.509 |
| `fafafa.ssl.openssl.x509_vfy.pas` | `fafafa.ssl.openssl.api.x509_vfy.pas` | X.509 验证 |
| `fafafa.ssl.openssl.x509v3.pas` | `fafafa.ssl.openssl.api.x509v3.pas` | X.509v3 |

---

## 第二阶段：WinSSL 文件整理

### 当前 WinSSL 文件

```
src/fafafa.ssl.winssl.pas           // 主实现（70KB，包含重复类型定义）
src/fafafa.ssl.winssl.types.pas     // 类型定义（16KB）
src/fafafa.ssl.winssl.api.pas       // API 绑定（13KB，新创建）
src/fafafa.ssl.winssl.optimized.pas // 优化版本（5KB）
```

### 需要的操作

1. **清理 `fafafa.ssl.winssl.pas`**
   - 移除内部的类型定义（迁移到 types.pas）
   - 移除 API 函数声明（应在 api.pas 中）
   - 只保留实现类 `TfafaSSLWinSSLContext`

2. **整合 `fafafa.ssl.winssl.types.pas`**
   - 确保所有类型定义统一
   - 消除与 winssl.pas 的重复

3. **完善 `fafafa.ssl.winssl.api.pas`**
   - 确保包含所有必要的 API 函数绑定
   - 检查是否需要拆分为子模块（可选）

---

## 第三阶段：抽象层整理

### 当前抽象层文件

```
src/fafafa.ssl.intf.pas          // 应该改名为 fafafa.ssl.base.pas
src/fafafa.ssl.types.pas         // 可能不存在，需要检查
src/fafafa.ssl.factory.pas       // 已存在
src/fafafa.ssl.certchain.pas     // 已存在
src/fafafa.ssl.log.pas            // 已存在
```

### 需要的操作

1. **重命名 `fafafa.ssl.intf.pas` → `fafafa.ssl.base.pas`**
   - 更新 unit 声明
   - 更新所有 uses 引用

2. **检查/创建 `fafafa.ssl.types.pas`**
   - 定义抽象层通用类型
   - 例如：`TSSLVersion`, `TSSLMethod`, `TCipherSuite` 等

3. **检查/创建 `fafafa.ssl.pas`（门面）**
   - 提供统一的入口 API
   - 简化用户使用

---

## 执行步骤

### Step 1: 备份和准备

```powershell
# 已完成 Git 提交，可以随时回滚
git log -1 --oneline
# 输出: 6190a4b docs: add architecture design before file reorganization
```

### Step 2: 批量重命名 OpenSSL 文件

```powershell
# 在 PowerShell 中执行批量重命名
cd D:\projects\Pascal\lazarus\My\libs\fafafa.ssl\src

# 重命名所有 OpenSSL API 模块文件
Get-ChildItem -Filter "fafafa.ssl.openssl.*.pas" | Where-Object {
    $_.Name -ne "fafafa.ssl.openssl.pas" -and
    $_.Name -ne "fafafa.ssl.openssl.types.pas" -and
    $_.Name -ne "fafafa.ssl.openssl.api.pas"
} | ForEach-Object {
    $oldName = $_.Name
    $newName = $oldName -replace "fafafa\.ssl\.openssl\.", "fafafa.ssl.openssl.api."
    Write-Host "Rename: $oldName -> $newName"
    # 取消注释下一行以执行重命名
    # Rename-Item $_.FullName $newName
}
```

### Step 3: 更新所有 unit 声明

需要使用脚本或手动更新每个文件的 `unit` 声明。

例如：
```pascal
// 旧：
unit fafafa.ssl.openssl.aes;

// 新：
unit fafafa.ssl.openssl.api.aes;
```

### Step 4: 更新所有 uses 引用

需要全局搜索替换所有引用：
```pascal
// 旧：
uses fafafa.ssl.openssl.aes;

// 新：
uses fafafa.ssl.openssl.api.aes;
```

### Step 5: 整理 WinSSL 文件

1. 检查 `winssl.pas` 中的类型定义
2. 将重复定义移到 `winssl.types.pas`
3. 更新 uses 引用

### Step 6: 整理抽象层

1. 重命名 `intf.pas` 为 `base.pas`
2. 创建/完善 `types.pas`
3. 创建/完善门面 `fafafa.ssl.pas`

### Step 7: 编译测试

```powershell
# 编译所有模块
fpc -B src/fafafa.ssl.openssl.pas
fpc -B src/fafafa.ssl.winssl.pas
fpc -B src/fafafa.ssl.pas

# 运行测试
.\tests\run_core_tests.ps1
```

### Step 8: 提交重构

```powershell
git add .
git commit -m "refactor: reorganize file structure according to architecture design

- Renamed all OpenSSL API modules: fafafa.ssl.openssl.* -> fafafa.ssl.openssl.api.*
- Cleaned up WinSSL type definitions and API bindings
- Renamed fafafa.ssl.intf.pas -> fafafa.ssl.base.pas
- Created/updated abstract layer types
- All tests passing

Refs: ARCHITECTURE_FILE_ORGANIZATION.md"
```

---

## 自动化脚本

为了简化重构过程，可以创建自动化脚本：

### PowerShell 重命名脚本

```powershell
# scripts/rename_openssl_files.ps1

$srcDir = "D:\projects\Pascal\lazarus\My\libs\fafafa.ssl\src"
cd $srcDir

# 定义排除列表
$excludeFiles = @(
    "fafafa.ssl.openssl.pas",
    "fafafa.ssl.openssl.types.pas",
    "fafafa.ssl.openssl.api.pas"
)

# 获取所有需要重命名的文件
$files = Get-ChildItem -Filter "fafafa.ssl.openssl.*.pas" | Where-Object {
    $excludeFiles -notcontains $_.Name
}

foreach ($file in $files) {
    $oldName = $file.Name
    $newName = $oldName -replace "fafafa\.ssl\.openssl\.", "fafafa.ssl.openssl.api."
    
    Write-Host "Renaming: $oldName -> $newName" -ForegroundColor Green
    Rename-Item $file.FullName $newName
}

Write-Host "`nRenamed $($files.Count) files" -ForegroundColor Cyan
```

### Pascal 更新 Unit 声明脚本

```pascal
// scripts/update_unit_declarations.pas

program update_unit_declarations;

uses
  SysUtils, Classes, StrUtils;

procedure UpdateUnitDeclaration(const FileName: string);
var
  Content: TStringList;
  I: Integer;
  Line: string;
begin
  Content := TStringList.Create;
  try
    Content.LoadFromFile(FileName);
    
    for I := 0 to Content.Count - 1 do
    begin
      Line := Content[I];
      
      // 更新 unit 声明
      if StartsText('unit fafafa.ssl.openssl.', Line) and
         not ContainsText(Line, '.api.') then
      begin
        Content[I] := StringReplace(Line, 
          'unit fafafa.ssl.openssl.',
          'unit fafafa.ssl.openssl.api.',
          [rfIgnoreCase]);
        WriteLn('Updated unit in: ', ExtractFileName(FileName));
        Break;
      end;
    end;
    
    Content.SaveToFile(FileName);
  finally
    Content.Free;
  end;
end;

var
  SearchRec: TSearchRec;
  SrcDir: string;
begin
  SrcDir := 'D:\projects\Pascal\lazarus\My\libs\fafafa.ssl\src\';
  
  if FindFirst(SrcDir + 'fafafa.ssl.openssl.api.*.pas', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      UpdateUnitDeclaration(SrcDir + SearchRec.Name);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
  
  WriteLn('Done!');
end.
```

---

## 风险和注意事项

⚠️ **风险：**
1. 大量文件重命名可能导致 Git 历史跟踪困难
2. 测试文件中的 uses 引用需要全部更新
3. 第三方代码引用需要手动调整

✅ **缓解措施：**
1. 使用 `git mv` 保留历史（但 Git 通常能自动跟踪）
2. 提供全局搜索替换脚本
3. 分阶段提交，每个阶段可独立回滚
4. 保留旧文件副本（.bak）作为备份

---

## 验收标准

重构完成后，需要满足以下标准：

- [ ] 所有文件命名符合 `ARCHITECTURE_FILE_ORGANIZATION.md` 规范
- [ ] 所有 OpenSSL API 模块以 `fafafa.ssl.openssl.api.*` 命名
- [ ] WinSSL 文件结构清晰，无重复定义
- [ ] 抽象层文件命名统一（base, types, factory 等）
- [ ] 所有模块能够独立编译
- [ ] 所有现有测试通过
- [ ] Git 提交历史清晰，有详细的 commit message

---

## 时间估算

- **Step 1-2**: 文件重命名 - 30 分钟
- **Step 3-4**: 更新声明和引用 - 2 小时
- **Step 5**: WinSSL 整理 - 1 小时
- **Step 6**: 抽象层整理 - 1 小时
- **Step 7**: 编译测试和修复 - 2 小时
- **Step 8**: 提交和文档 - 30 分钟

**总计：约 7 小时**

---

## 下一步

1. ✅ 已创建架构设计文档
2. ✅ 已提交当前状态到 Git
3. ⏭️ 执行文件重命名（等待确认）
4. ⏭️ 更新引用和声明
5. ⏭️ 测试验证
6. ⏭️ 最终提交

---

**准备就绪！** 等待你的确认后开始执行重构。

你是否希望现在开始执行重构？还是需要先审查计划？
