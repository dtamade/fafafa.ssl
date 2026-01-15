#!/bin/bash
# 快速编译测试脚本

set -e

BASEDIR=$HOME/freePascal/fpc/units/x86_64-linux
SRCDIR=../src

FPC_OPTS="-Fu$SRCDIR \
  -Fu$BASEDIR/rtl-objpas \
  -Fu$BASEDIR/rtl-console \
  -Fu$BASEDIR/rtl-extra \
  -Fu$BASEDIR/fcl-base \
  -Fu$BASEDIR/fcl-json \
  -Fu$BASEDIR/fcl-net \
  -Mobjfpc -Scgi -O1 -g -gl"

if [ -z "$1" ]; then
  echo "用法: $0 <test_name.pas>"
  exit 1
fi

echo "编译: $1"
fpc $FPC_OPTS "$1"

if [ $? -eq 0 ]; then
  echo "✓ 编译成功"
  
  # 运行可执行文件
  EXENAME="${1%.pas}"
  if [ -f "./$EXENAME" ]; then
    echo ""
    echo "========== 运行测试 =========="
    "./$EXENAME"
  fi
fi
