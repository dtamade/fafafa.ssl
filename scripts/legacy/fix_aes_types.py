import re

# 读取文件
with open('src/fafafa.ssl.openssl.api.aes.pas', 'r', encoding='utf-8') as f:
    content = f.read()

# 修复 GetProcAddress 调用，添加类型转换
# 匹配: 变量名 := GetProcAddress(...)
pattern = r'(\w+)\s*:=\s*GetProcAddress\(([^)]+)\)'
replacement = r'T\1(GetProcAddress(\2))'

content = re.sub(pattern, replacement, content)

# 写回文件
with open('src/fafafa.ssl.openssl.api.aes.pas', 'w', encoding='utf-8') as f:
    f.write(content)

print("修复完成")
