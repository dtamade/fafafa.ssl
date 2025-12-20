#!/usr/bin/env python3
"""
批量编译fafafa.ssl所有核心模块
用于验证Linux环境下的编译兼容性
"""

import os
import argparse
import subprocess
import sys
from pathlib import Path

# 项目根目录
PROJECT_ROOT = Path(__file__).parent.parent.resolve()
SRC_DIR = PROJECT_ROOT / "src"

# FPC单元路径配置
FPC_BASE = Path.home() / "freePascal" / "fpc" / "units" / "x86_64-linux"
UNIT_PATHS = [
    FPC_BASE / "rtl-objpas",
    FPC_BASE / "rtl",
    FPC_BASE / "rtl-unix",
    FPC_BASE / "rtl-extra",
    FPC_BASE / "fcl-base",
    FPC_BASE / "fcl-json",
    FPC_BASE / "fcl-process",
    FPC_BASE / "pthreads",
    PROJECT_ROOT / "src",
]

# 排除的文件（WinSSL在Linux上无法编译）
EXCLUDE_PATTERNS = [
    "fafafa.ssl.winssl",  # Windows专用
    "rand_old.pas",        # 已废弃
    "fafafa.ssl.http.simple",  # 依赖 socket/HTTP 示例，非核心单元
]

def should_compile(file_path):
    """判断文件是否应该编译"""
    file_name = file_path.name
    
    # 排除特定模式
    for pattern in EXCLUDE_PATTERNS:
        if pattern in file_name:
            return False
    
    # 只编译.pas文件
    return file_path.suffix == ".pas"

def compile_module(pas_file, rebuild):
    """编译单个模块"""
    # 构建FPC命令
    cmd = ["fpc"]

    if rebuild:
        cmd.append("-B")
    
    # 添加单元路径
    for unit_path in UNIT_PATHS:
        if unit_path.exists():
            cmd.append(f"-Fu{unit_path}")
    
    # 语法模式和其他选项
    cmd.extend([
        "-Mobjfpc",     # ObjFPC模式
        "-Scgi",        # 语法选项
        "-O2",          # 优化级别
        "-g",           # 调试信息
        "-gl",          # 使用行号信息
        "-vewnhi",      # 详细错误信息
        str(pas_file)   # 源文件
    ])
    
    try:
        result = subprocess.run(
            cmd,
            cwd=PROJECT_ROOT,
            capture_output=True,
            text=True,
            timeout=30
        )
        return result.returncode == 0, result.stdout, result.stderr
    except subprocess.TimeoutExpired:
        return False, "", "Compilation timeout"
    except Exception as e:
        return False, "", str(e)

def main():
    """主函数"""
    parser = argparse.ArgumentParser(description="批量编译 fafafa.ssl 核心模块")
    parser.add_argument(
        "--rebuild",
        action="store_true",
        help="使用 fpc -B 强制全量重编译（用于规避增量产物导致的链接/调试符号不一致）",
    )
    args = parser.parse_args()

    print("=" * 60)
    print("fafafa.ssl 批量编译测试 - Linux环境")
    print("=" * 60)
    print()
    
    # 收集所有.pas文件
    all_files = list(SRC_DIR.glob("**/*.pas"))
    compile_files = [f for f in all_files if should_compile(f)]
    
    print(f"发现 {len(all_files)} 个.pas文件")
    print(f"将编译 {len(compile_files)} 个核心模块")
    print(f"跳过 {len(all_files) - len(compile_files)} 个文件 (WinSSL/deprecated)")
    print()
    
    # 编译统计
    success_count = 0
    failed_count = 0
    failed_files = []
    
    # 逐个编译
    for i, pas_file in enumerate(compile_files, 1):
        rel_path = pas_file.relative_to(SRC_DIR)
        print(f"[{i}/{len(compile_files)}] 编译 {rel_path}...", end=" ")
        
        success, stdout, stderr = compile_module(pas_file, args.rebuild)
        
        if success:
            print("✓ 成功")
            success_count += 1
        else:
            print("✗ 失败")
            failed_count += 1
            failed_files.append((rel_path, stderr))
    
    # 输出摘要
    print()
    print("=" * 60)
    print("编译摘要")
    print("=" * 60)
    print(f"总文件数: {len(compile_files)}")
    print(f"编译成功: {success_count} ({success_count/len(compile_files)*100:.1f}%)")
    print(f"编译失败: {failed_count} ({failed_count/len(compile_files)*100:.1f}%)")
    print()
    
    # 显示失败文件
    if failed_files:
        print("失败的文件:")
        for file_path, error in failed_files[:10]:  # 只显示前10个
            print(f"  - {file_path}")
            # 提取关键错误信息
            error_lines = [line for line in error.split('\n') if 'Error:' in line or 'Fatal:' in line]
            for line in error_lines[:3]:
                print(f"    {line.strip()}")
        
        if len(failed_files) > 10:
            print(f"  ... 还有 {len(failed_files) - 10} 个失败文件")
    
    # 检查是否达标
    success_rate = success_count / len(compile_files) * 100
    target_rate = 98.0
    
    print()
    if success_rate >= target_rate:
        print(f"✅ 编译成功率 {success_rate:.1f}% 达到目标 ({target_rate}%)")
        return 0
    else:
        print(f"⚠️  编译成功率 {success_rate:.1f}% 未达到目标 ({target_rate}%)")
        return 1

if __name__ == "__main__":
    sys.exit(main())
