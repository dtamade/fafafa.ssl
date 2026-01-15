import re

# 读取文件
with open('src/fafafa.ssl.openssl.api.engine.pas', 'r', encoding='utf-8') as f:
    content = f.read()

# 修复多余的右括号和分号
content = re.sub(r'GetProcAddress\([^)]+\)\);\)', r')', content)

# 写回文件
with open('src/fafafa.ssl.openssl.api.engine.pas', 'w', encoding='utf-8') as f:
    f.write(content)

print("修复完成")
