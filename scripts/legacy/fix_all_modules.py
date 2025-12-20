import os
import re
import subprocess

# 失败的模块列表
failed_modules = [
    'fafafa.ssl.openssl.api.aes',  # 已修复
    'fafafa.ssl.openssl.api.blake2',
    'fafafa.ssl.openssl.api.cmac',
    'fafafa.ssl.openssl.api.des',
    'fafafa.ssl.openssl.api.ec',
    'fafafa.ssl.openssl.api.ecdh',
    'fafafa.ssl.openssl.api.ecdsa',
    'fafafa.ssl.openssl.api.md',
    'fafafa.ssl.openssl.api.modes',
    'fafafa.ssl.openssl.api.pkcs',
    'fafafa.ssl.openssl.api.rand_old',
    'fafafa.ssl.openssl.api.sha',
    'fafafa.ssl.openssl.api.sha3',
]

print(f"修复 {len(failed_modules)} 个模块...")

fixed_count = 0

for module in failed_modules:
    pas_file = f'src/{module}.pas'
    
    if not os.path.exists(pas_file):
        print(f"⚠️  文件不存在: {pas_file}")
        continue
    
    print(f"\n修复: {module}")
    
    # 读取文件
    with open(pas_file, 'r', encoding='utf-8') as f:
        content = f.read()
    
    original_content = content
    
    # 查找 GetProcAddress 调用并添加类型转换
    # 匹配模式: 变量 := GetProcAddress(...)
    def add_type_conversion(match):
        var_name = match.group(1)
        proc_args = match.group(2)
        return f'{var_name} := T{var_name}(GetProcAddress({proc_args}))'
    
    # 修复 Load 过程中的 GetProcAddress 调用
    # 查找形如: 变量名 := GetProcAddress(...
    pattern = r'(\w+)\s*:=\s*GetProcAddress\('
    
    # 但要排除已经在函数调用中的
    content = re.sub(
        pattern,
        lambda m: f'{m.group(1)} := T{m.group(1)}(GetProcAddress(',
        content
    )
    
    if content != original_content:
        # 写回文件
        with open(pas_file, 'w', encoding='utf-8') as f:
            f.write(content)
        print(f"  ✅ 已修复")
        fixed_count += 1
    else:
        print(f"  ℹ️  无需修复")

print(f"\n{'='*60}")
print(f"修复完成: {fixed_count}/{len(failed_modules)} 个模块")
print(f"{'='*60}")
