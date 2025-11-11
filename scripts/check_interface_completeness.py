#!/usr/bin/env python3
"""
检查接口实现的完整性
比较抽象接口定义和具体实现之间的差异
"""

import re
import sys
from pathlib import Path
from typing import Dict, List, Set, Tuple

class InterfaceMethod:
    def __init__(self, name: str, return_type: str, params: str):
        self.name = name
        self.return_type = return_type
        self.params = params
    
    def __repr__(self):
        if self.return_type:
            return f"function {self.name}({self.params}): {self.return_type}"
        else:
            return f"procedure {self.name}({self.params})"
    
    def __eq__(self, other):
        return self.name == other.name
    
    def __hash__(self):
        return hash(self.name)

def extract_interface_methods(content: str, interface_name: str) -> Dict[str, List[InterfaceMethod]]:
    """从接口定义中提取方法"""
    methods = []
    
    # 查找接口定义开始
    pattern = rf'{interface_name}\s*=\s*interface\s*\[.*?\]'
    match = re.search(pattern, content, re.DOTALL)
    if not match:
        return methods
    
    start = match.end()
    
    # 查找接口定义结束（下一个type声明或implementation）
    end_match = re.search(r'\n\s*(end;|implementation)', content[start:])
    if end_match:
        end = start + end_match.start()
    else:
        end = len(content)
    
    interface_content = content[start:end]
    
    # 提取函数和过程声明
    # function Name(...): ReturnType;
    func_pattern = r'function\s+(\w+)\s*(\([^)]*\))?\s*:\s*([^;]+);'
    for match in re.finditer(func_pattern, interface_content):
        name = match.group(1)
        params = match.group(2) if match.group(2) else "()"
        return_type = match.group(3).strip()
        methods.append(InterfaceMethod(name, return_type, params))
    
    # procedure Name(...);
    proc_pattern = r'procedure\s+(\w+)\s*(\([^)]*\))?\s*;'
    for match in re.finditer(proc_pattern, interface_content):
        name = match.group(1)
        params = match.group(2) if match.group(2) else "()"
        methods.append(InterfaceMethod(name, "", params))
    
    return methods

def extract_class_methods(content: str, class_name: str) -> Set[str]:
    """从类实现中提取已实现的方法名"""
    methods = set()
    
    # 首先在类声明部分查找方法声明
    pattern = rf'{class_name}\s*=\s*class\s*\([^)]+\)'
    match = re.search(pattern, content, re.DOTALL)
    if not match:
        return methods
    
    start = match.end()
    
    # 查找类声明结束 - 需要匹配正确的end（与class配对的）
    brace_count = 1
    i = start
    while i < len(content) and brace_count > 0:
        if content[i:i+6] == 'record' or (i > 0 and content[i-5:i+1] == 'class('):
            # 遇到嵌套结构
            nested_end = re.search(r'\bend\b', content[i+1:])
            if nested_end:
                i += nested_end.end()
                continue
        elif re.match(r'\bend\b', content[i:]):
            brace_count -= 1
            if brace_count == 0:
                break
            i += 3
        else:
            i += 1
    
    if i >= len(content):
        # 备用方案：查找第一个单独的end;
        end_match = re.search(r'\n\s*end;', content[start:])
        if end_match:
            end = start + end_match.start()
        else:
            return methods
    else:
        end = i
    
    class_decl = content[start:end]
    
    # 提取方法名（忽略private、protected、public等访问修饰符）
    func_pattern = r'function\s+(\w+)\s*[\(:]'
    for match in re.finditer(func_pattern, class_decl):
        methods.add(match.group(1))
    
    proc_pattern = r'procedure\s+(\w+)\s*[\(;]'
    for match in re.finditer(proc_pattern, class_decl):
        methods.add(match.group(1))
    
    return methods

def check_implementation(interface_file: Path, impl_file: Path, 
                        interface_name: str, class_name: str) -> Tuple[List[str], List[str]]:
    """检查实现的完整性"""
    
    with open(interface_file, 'r', encoding='utf-8') as f:
        interface_content = f.read()
    
    with open(impl_file, 'r', encoding='utf-8') as f:
        impl_content = f.read()
    
    # 提取接口方法
    interface_methods = extract_interface_methods(interface_content, interface_name)
    interface_method_names = {m.name for m in interface_methods}
    
    # 提取实现方法
    impl_methods = extract_class_methods(impl_content, class_name)
    
    # 找出缺失的方法
    missing = interface_method_names - impl_methods
    extra = impl_methods - interface_method_names
    
    # 获取缺失方法的详细信息
    missing_details = [m for m in interface_methods if m.name in missing]
    
    return missing_details, sorted(extra)

def main():
    project_root = Path(__file__).parent.parent
    src_dir = project_root / "src"
    
    # 定义要检查的接口和实现
    checks = [
        {
            'interface_file': src_dir / "fafafa.ssl.abstract.intf.pas",
            'interface': 'ISSLLibrary',
            'implementations': [
                ('fafafa.ssl.openssl.pas', 'TOpenSSLLibrary'),
                ('fafafa.ssl.winssl.lib.pas', 'TWinSSLLibrary'),
            ]
        },
        {
            'interface_file': src_dir / "fafafa.ssl.abstract.intf.pas",
            'interface': 'ISSLContext',
            'implementations': [
                ('fafafa.ssl.openssl.pas', 'TOpenSSLContext'),
                ('fafafa.ssl.winssl.context.pas', 'TWinSSLContext'),
            ]
        },
        {
            'interface_file': src_dir / "fafafa.ssl.abstract.intf.pas",
            'interface': 'ISSLConnection',
            'implementations': [
                ('fafafa.ssl.openssl.pas', 'TOpenSSLConnection'),
                ('fafafa.ssl.winssl.connection.pas', 'TWinSSLConnection'),
            ]
        },
        {
            'interface_file': src_dir / "fafafa.ssl.abstract.intf.pas",
            'interface': 'ISSLCertificate',
            'implementations': [
                ('fafafa.ssl.openssl.pas', 'TOpenSSLCertificate'),
                ('fafafa.ssl.winssl.certificate.pas', 'TWinSSLCertificate'),
            ]
        },
        {
            'interface_file': src_dir / "fafafa.ssl.abstract.intf.pas",
            'interface': 'ISSLCertificateStore',
            'implementations': [
                ('fafafa.ssl.openssl.pas', 'TOpenSSLCertificateStore'),
                ('fafafa.ssl.winssl.certstore.pas', 'TWinSSLCertificateStore'),
            ]
        },
        {
            'interface_file': src_dir / "fafafa.ssl.abstract.intf.pas",
            'interface': 'ISSLSession',
            'implementations': [
                ('fafafa.ssl.openssl.pas', 'TOpenSSLSession'),
            ]
        },
    ]
    
    total_issues = 0
    
    print("=" * 80)
    print("接口完整性检查报告")
    print("=" * 80)
    print()
    
    for check in checks:
        interface_file = check['interface_file']
        interface_name = check['interface']
        
        print(f"\n{'=' * 80}")
        print(f"接口: {interface_name}")
        print(f"{'=' * 80}\n")
        
        for impl_file_name, class_name in check['implementations']:
            impl_file = src_dir / impl_file_name
            
            if not impl_file.exists():
                print(f"⚠️  实现文件不存在: {impl_file}")
                total_issues += 1
                continue
            
            missing, extra = check_implementation(interface_file, impl_file, 
                                                 interface_name, class_name)
            
            print(f"实现类: {class_name}")
            print(f"文件: {impl_file_name}")
            
            if missing:
                print(f"\n🔴 缺失方法 ({len(missing)} 个):")
                for method in missing:
                    print(f"   - {method}")
                total_issues += len(missing)
            else:
                print("✅ 所有接口方法都已实现")
            
            if extra:
                print(f"\n⚠️  额外方法 ({len(extra)} 个，可能是辅助方法):")
                for method_name in extra[:10]:  # 只显示前10个
                    print(f"   - {method_name}")
                if len(extra) > 10:
                    print(f"   ... 还有 {len(extra) - 10} 个")
            
            print()
    
    print("=" * 80)
    if total_issues > 0:
        print(f"❌ 发现 {total_issues} 个问题")
        return 1
    else:
        print("✅ 所有接口都已完整实现")
        return 0

if __name__ == "__main__":
    sys.exit(main())

