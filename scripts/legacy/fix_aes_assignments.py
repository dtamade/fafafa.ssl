import re

# 读取文件
with open('src/fafafa.ssl.openssl.api.aes.pas', 'r', encoding='utf-8') as f:
    lines = f.readlines()

# 修复第 140-155 行
for i in range(139, 155):  # 0-indexed, so 139-154 corresponds to lines 140-155
    if i < len(lines):
        line = lines[i]
        # 替换: TAES_func(...) -> AES_func := TAES_func(...)
        line = re.sub(r'^  TAES_(\w+)\(GetProcAddress\(([^)]+)\)\);$', 
                      r'  AES_\1 := TAES_\1(GetProcAddress(\2));', line)
        lines[i] = line

# 写回文件
with open('src/fafafa.ssl.openssl.api.aes.pas', 'w', encoding='utf-8') as f:
    f.writelines(lines)

print("修复完成")
