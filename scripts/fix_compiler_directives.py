#!/usr/bin/env python3
"""
自动修复编译器指令格式问题
- 统一编译模式声明为 {$mode ObjFPC}{$H+}
- 移除重复的 {$H+} 声明
- 确保 Windows CODEPAGE 声明格式正确
"""

import re
import sys
from pathlib import Path
from typing import List, Tuple

def fix_compiler_directives(content: str, filename: str) -> Tuple[str, List[str]]:
    """修复编译器指令，返回修复后的内容和修改列表"""
    changes = []
    original = content
    
    # 1. 统一 {$mode ...} 格式（不区分大小写的MODE，统一为小写mode）
    # 匹配各种格式: {$MODE ObjFPC}, {$mode objfpc}, {$MODE OBJFPC} 等
    mode_pattern = r'\{\$(?:MODE|mode)\s+(?:ObjFPC|OBJFPC|objfpc)\}'
    if re.search(mode_pattern, content, re.IGNORECASE):
        new_content = re.sub(mode_pattern, '{$mode ObjFPC}', content, flags=re.IGNORECASE)
        if new_content != content:
            changes.append("统一编译模式为 {$mode ObjFPC}")
            content = new_content
    
    # 2. 处理紧跟的 {$H+}
    # 查找 {$mode ObjFPC} 后面紧跟或不紧跟的 {$H+}
    # 情况1: {$mode ObjFPC}{$H+} - 正确，保持
    # 情况2: {$mode ObjFPC}\n{$H+} - 合并为一行
    # 情况3: {$mode ObjFPC} 且后面没有{$H+} - 需要添加
    
    # 首先处理已经有{$H+}但格式不对的情况
    pattern1 = r'\{\$mode ObjFPC\}\s*\n\s*\{\$H\+\}'
    if re.search(pattern1, content):
        content = re.sub(pattern1, '{$mode ObjFPC}{$H+}', content)
        changes.append("合并 {$mode ObjFPC} 和 {$H+} 到同一行")
    
    # 然后检查是否缺少 {$H+}
    # 查找 {$mode ObjFPC} 后面没有紧跟 {$H+} 的情况
    pattern2 = r'\{\$mode ObjFPC\}(?!\{\$H\+\})'
    if re.search(pattern2, content):
        content = re.sub(pattern2, '{$mode ObjFPC}{$H+}', content)
        changes.append("添加缺失的 {$H+}")
    
    # 3. 移除重复的 {$H+} 声明（保留第一个紧跟mode的）
    # 分割文件为行来处理
    lines = content.split('\n')
    new_lines = []
    h_plus_found = False
    mode_line_idx = -1
    
    for i, line in enumerate(lines):
        # 找到 mode 声明行
        if '{$mode ObjFPC}{$H+}' in line:
            h_plus_found = True
            mode_line_idx = i
            new_lines.append(line)
        # 如果已经找到了mode行，且当前行是独立的{$H+}，跳过它
        elif h_plus_found and line.strip() == '{$H+}':
            if i > mode_line_idx:  # 只移除mode之后的重复{$H+}
                changes.append(f"移除第{i+1}行重复的 {{$H+}}")
                continue  # 跳过这一行
            else:
                new_lines.append(line)
        else:
            new_lines.append(line)
    
    if len(new_lines) != len(lines):
        content = '\n'.join(new_lines)
    
    # 4. 确保 Windows CODEPAGE 格式正确
    # {$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}
    codepage_pattern = r'\{\$IFDEF\s+WINDOWS\}\s*\{\$CODEPAGE\s+UTF8\}\s*\{\$ENDIF\}'
    if re.search(r'\{\$IFDEF\s+WINDOWS\}', content, re.IGNORECASE):
        if not re.search(codepage_pattern, content):
            # 尝试修复格式
            wrong_pattern = r'\{\$IFDEF\s+WINDOWS\}\s*\n\s*\{\$CODEPAGE\s+UTF8\}\s*\n\s*\{\$ENDIF\}'
            if re.search(wrong_pattern, content):
                content = re.sub(wrong_pattern, '{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}', content)
                changes.append("格式化 WINDOWS CODEPAGE 声明")
    
    return content, changes

def process_file(filepath: Path, dry_run: bool = False) -> bool:
    """处理单个文件"""
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            original_content = f.read()
        
        new_content, changes = fix_compiler_directives(original_content, filepath.name)
        
        if changes:
            print(f"\n📝 {filepath.relative_to(Path.cwd())}")
            for change in changes:
                print(f"   - {change}")
            
            if not dry_run:
                with open(filepath, 'w', encoding='utf-8') as f:
                    f.write(new_content)
                print("   ✅ 已保存")
            else:
                print("   🔍 预览模式（未保存）")
            
            return True
        
        return False
        
    except Exception as e:
        print(f"❌ 处理文件时出错 {filepath}: {e}")
        return False

def main():
    import argparse
    
    parser = argparse.ArgumentParser(description='修复编译器指令格式')
    parser.add_argument('path', nargs='?', default='src', 
                       help='要处理的目录或文件路径（默认: src）')
    parser.add_argument('--dry-run', action='store_true',
                       help='预览模式，不实际修改文件')
    
    args = parser.parse_args()
    
    project_root = Path(__file__).parent.parent
    target_path = project_root / args.path
    
    if not target_path.exists():
        print(f"❌ 路径不存在: {target_path}")
        return 1
    
    # 收集所有 Pascal 文件
    if target_path.is_file():
        files = [target_path]
    else:
        files = list(target_path.glob('**/*.pas'))
    
    print(f"{'='*80}")
    print(f"编译器指令格式修复工具")
    print(f"{'='*80}")
    print(f"模式: {'预览' if args.dry_run else '修改'}")
    print(f"路径: {target_path}")
    print(f"文件数: {len(files)}")
    print(f"{'='*80}")
    
    modified_count = 0
    
    for filepath in sorted(files):
        if process_file(filepath, args.dry_run):
            modified_count += 1
    
    print(f"\n{'='*80}")
    print(f"处理完成")
    print(f"{'='*80}")
    print(f"总文件数: {len(files)}")
    print(f"已修改: {modified_count}")
    print(f"未修改: {len(files) - modified_count}")
    
    if args.dry_run and modified_count > 0:
        print(f"\n💡 这是预览模式。要实际修改文件，请运行:")
        print(f"   python3 {Path(__file__).name} {args.path}")
    
    return 0

if __name__ == "__main__":
    sys.exit(main())

