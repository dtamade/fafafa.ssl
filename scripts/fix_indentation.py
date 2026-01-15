#!/usr/bin/env python3
"""
自动修复Pascal文件的缩进问题
确保所有缩进都是2空格的倍数
"""

import re
import sys
from pathlib import Path
from typing import List, Tuple

def fix_indentation(content: str) -> Tuple[str, int]:
    """修复缩进问题，返回修复后的内容和修复的行数"""
    lines = content.split('\n')
    fixed_lines = []
    fixed_count = 0
    
    for i, line in enumerate(lines, 1):
        # 跳过空行
        if not line.strip():
            fixed_lines.append(line)
            continue
        
        # 计算当前缩进
        stripped = line.lstrip()
        if not stripped:
            fixed_lines.append(line)
            continue
        
        current_indent = len(line) - len(stripped)
        
        # 如果缩进不是2的倍数，修复它
        if current_indent % 2 != 0:
            # 向下取整到最近的2的倍数
            new_indent = (current_indent // 2) * 2
            fixed_line = ' ' * new_indent + stripped
            fixed_lines.append(fixed_line)
            fixed_count += 1
        else:
            fixed_lines.append(line)
    
    return '\n'.join(fixed_lines), fixed_count

def process_file(filepath: Path, dry_run: bool = False) -> Tuple[bool, int]:
    """处理单个文件，返回是否修改和修复的行数"""
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            original_content = f.read()
        
        new_content, fixed_count = fix_indentation(original_content)
        
        if fixed_count > 0:
            if not dry_run:
                with open(filepath, 'w', encoding='utf-8') as f:
                    f.write(new_content)
            return True, fixed_count
        
        return False, 0
        
    except Exception as e:
        print(f"❌ 处理文件时出错 {filepath}: {e}")
        return False, 0

def main():
    import argparse
    
    parser = argparse.ArgumentParser(description='修复缩进格式')
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
    print(f"缩进格式修复工具")
    print(f"{'='*80}")
    print(f"模式: {'预览' if args.dry_run else '修改'}")
    print(f"路径: {target_path}")
    print(f"文件数: {len(files)}")
    print(f"{'='*80}\n")
    
    modified_count = 0
    total_fixed_lines = 0
    
    for filepath in sorted(files):
        modified, fixed_count = process_file(filepath, args.dry_run)
        if modified:
            modified_count += 1
            total_fixed_lines += fixed_count
            status = "🔍 预览" if args.dry_run else "✅ 已修复"
            print(f"{status} {filepath.relative_to(project_root)}: {fixed_count}行")
    
    print(f"\n{'='*80}")
    print(f"处理完成")
    print(f"{'='*80}")
    print(f"总文件数: {len(files)}")
    print(f"修改文件数: {modified_count}")
    print(f"修复行数: {total_fixed_lines}")
    
    if args.dry_run and modified_count > 0:
        print(f"\n💡 这是预览模式。要实际修改文件，请运行:")
        print(f"   python3 {Path(__file__).name} {args.path}")
    
    return 0

if __name__ == "__main__":
    sys.exit(main())

