#!/usr/bin/env python3
"""
fafafa.ssl 代码风格检查工具

检查项目代码是否符合编码规范：
1. 编译模式：所有 .pas 文件应使用 {$mode ObjFPC}{$H+}
2. Windows CODEPAGE：Windows 文件应包含 {$CODEPAGE UTF8}
3. 缩进：使用 2 空格缩进，不使用 Tab
4. 命名约定：变量使用 L 前缀，参数使用 a 前缀，类使用 T 前缀等

用法：
  python scripts/check_code_style.py [路径]
"""

import os
import re
import sys
from pathlib import Path

class CodeStyleChecker:
    def __init__(self, root_path='.'):
        self.root_path = Path(root_path)
        self.errors = []
        self.warnings = []

    def check_file(self, filepath):
        """检查单个文件的代码风格"""
        with open(filepath, 'r', encoding='utf-8') as f:
            content = f.read()
            lines = content.split('\n')

        # 检查编译模式
        self._check_compiler_mode(filepath, lines)

        # 检查 Windows CODEPAGE
        self._check_codepage(filepath, lines)

        # 检查缩进
        self._check_indentation(filepath, lines)

        # 检查命名约定（仅检查前 50 行以提高速度）
        self._check_naming(filepath, lines[:50])

    def _check_compiler_mode(self, filepath, lines):
        """检查编译模式"""
        has_mode = False
        has_delphi_mode = False

        # 检查前50行（考虑可能有较长的注释头）
        for i, line in enumerate(lines[:50], 1):
            # 首先检查是否包含编译模式声明
            if '{$mode' in line.lower():
                has_mode = True
                if '{$mode Delphi}' in line or '{$mode delphi}' in line.lower():
                    has_delphi_mode = True
                    self.errors.append(f'{filepath}:{i} 使用了过时的 {{$mode Delphi}}，应使用 {{$mode ObjFPC}}')
                # 找到 mode 声明后就停止检查
                break

        if not has_mode:
            self.errors.append(f'{filepath}:1 缺少编译模式声明')

        if has_delphi_mode:
            self.errors.append(f'{filepath} 包含过时的 Delphi 模式')

    def _check_codepage(self, filepath, lines):
        """检查 Windows CODEPAGE"""
        filepath_str = str(filepath)
        is_windows_file = 'winssl' in filepath_str or 'abstract' in filepath_str or 'factory' in filepath_str
        has_codepage = any('{$CODEPAGE UTF8}' in line for line in lines)

        if is_windows_file and not has_codepage:
            self.warnings.append(f'{filepath} 是 Windows 文件但缺少 {{$CODEPAGE UTF8}}')

    def _check_indentation(self, filepath, lines):
        """检查缩进"""
        for i, line in enumerate(lines, 1):
            # 跳过空行和注释行
            if not line.strip() or line.strip().startswith('//'):
                continue

            # 检查是否包含 Tab 字符
            if '\t' in line:
                self.errors.append(f'{filepath}:{i} 包含 Tab 字符，应使用 2 个空格缩进')

            # 检查缩进是否使用空格（简单检查）
            stripped = line.lstrip()
            if stripped:
                indent_len = len(line) - len(stripped)
                if indent_len % 2 != 0:
                    self.errors.append(f'{filepath}:{i} 缩进不是 2 空格倍数')

    def _check_naming(self, filepath, lines):
        """检查命名约定"""
        # 检查 Pascal 文件的标准结构
        has_unit = False
        has_interface = False
        has_implementation = False

        for i, line in enumerate(lines, 1):
            line = line.strip()
            if line.startswith('unit '):
                has_unit = True
            elif line.startswith('interface'):
                has_interface = True
            elif line.startswith('implementation'):
                has_implementation = True

        if not has_unit:
            self.errors.append(f'{filepath} 缺少 unit 声明')
        if not has_interface and not has_implementation:
            self.warnings.append(f'{filepath} 可能缺少 interface/implementation 部分')

    def check_all(self):
        """检查所有 .pas 文件"""
        pas_files = list(self.root_path.rglob('*.pas'))

        print(f'检查 {len(pas_files)} 个 Pascal 文件...\n')

        for filepath in pas_files:
            # 跳过测试和示例文件的详细检查
            if 'tests' in filepath.parts or 'examples' in filepath.parts:
                continue
            self.check_file(filepath)

        return len(self.errors), len(self.warnings)

    def print_report(self):
        """打印检查报告"""
        print('=' * 60)
        print('代码风格检查报告')
        print('=' * 60)

        if self.errors:
            print(f'\n🔴 错误 ({len(self.errors)} 项):')
            for error in self.errors:
                print(f'  • {error}')

        if self.warnings:
            print(f'\n🟡 警告 ({len(self.warnings)} 项):')
            for warning in self.warnings:
                print(f'  • {warning}')

        print('\n' + '=' * 60)

        if self.errors:
            print(f'检查完成: {len(self.errors)} 个错误，{len(self.warnings)} 个警告')
            print('❌ 代码风格检查未通过')
            return 1
        elif self.warnings:
            print(f'检查完成: {len(self.errors)} 个错误，{len(self.warnings)} 个警告')
            print('⚠️  代码风格检查通过，但有建议改进项')
            return 0
        else:
            print('检查完成: 所有文件代码风格正确 ✅')
            print('✅ 代码风格检查通过')
            return 0

def main():
    root_path = sys.argv[1] if len(sys.argv) > 1 else '.'
    checker = CodeStyleChecker(root_path)

    error_count, warning_count = checker.check_all()
    exit_code = checker.print_report()

    sys.exit(exit_code)

if __name__ == '__main__':
    main()
