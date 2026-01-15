import re

modules = [
    'fafafa.ssl.openssl.api.blake2',
    'fafafa.ssl.openssl.api.des',
    'fafafa.ssl.openssl.api.ec',
    'fafafa.ssl.openssl.api.ecdh',
    'fafafa.ssl.openssl.api.ecdsa',
    'fafafa.ssl.openssl.api.md',
    'fafafa.ssl.openssl.api.pkcs',
    'fafafa.ssl.openssl.api.rand_old',
    'fafafa.ssl.openssl.api.sha',
    'fafafa.ssl.openssl.api.sha3',
]

for module in modules:
    pas_file = f'src/{module}.pas'
    print(f"修复: {module}")
    
    with open(pas_file, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # 修复形如: := TFunc(GetProcAddress(...); 的情况
    # 替换为: := TFunc(GetProcAddress(...));
    content = re.sub(r':=\s*T\w+\(GetProcAddress\(([^)]+)\);\s*;', r':= T\g<1>(GetProcAddress(\1));', content)
    
    with open(pas_file, 'w', encoding='utf-8') as f:
        f.write(content)
    
    print(f"  ✅ 完成")

print("\n所有模块修复完成")
