#!/bin/bash
# 批量修正旧单元名为新单元名

cd /home/dtamade/projects/fafafa.ssl

echo "=== 批量修正单元名 ==="
echo ""

# 统计需要修改的文件数
FILES=$(grep -rl "fafafa\.ssl\.openssl\." --include="*.pas" examples/ tests/ | grep -v "fafafa\.ssl\.openssl\.api\." | wc -l)
echo "找到 $FILES 个需要修改的文件"
echo ""

# 备份
echo "创建备份..."
tar -czf unit_names_backup_$(date +%Y%m%d_%H%M%S).tar.gz examples/ tests/ 2>/dev/null
echo "备份完成"
echo ""

# 批量替换
echo "开始替换..."

# 查找所有需要修改的文件
grep -rl "fafafa\.ssl\.openssl\." --include="*.pas" examples/ tests/ | grep -v "fafafa\.ssl\.openssl\.api\." | while read file; do
  echo "处理: $file"
  
  # 替换所有 fafafa.ssl.openssl.XXX 为 fafafa.ssl.openssl.api.XXX
  # 但要排除已经是 fafafa.ssl.openssl.api.XXX 的
  sed -i 's/fafafa\.ssl\.openssl\.core/fafafa.ssl.openssl.api.core/g' "$file"
  sed -i 's/fafafa\.ssl\.openssl\.bio/fafafa.ssl.openssl.api.bio/g' "$file"
  sed -i 's/fafafa\.ssl\.openssl\.types/fafafa.ssl.openssl.api.types/g' "$file"
  sed -i 's/fafafa\.ssl\.openssl\.err/fafafa.ssl.openssl.api.err/g' "$file"
  sed -i 's/fafafa\.ssl\.openssl\.evp/fafafa.ssl.openssl.api.evp/g' "$file"
  sed -i 's/fafafa\.ssl\.openssl\.pem/fafafa.ssl.openssl.api.pem/g' "$file"
  sed -i 's/fafafa\.ssl\.openssl\.x509/fafafa.ssl.openssl.api.x509/g' "$file"
  sed -i 's/fafafa\.ssl\.openssl\.rsa/fafafa.ssl.openssl.api.rsa/g' "$file"
  sed -i 's/fafafa\.ssl\.openssl\.dsa/fafafa.ssl.openssl.api.dsa/g' "$file"
  sed -i 's/fafafa\.ssl\.openssl\.dh/fafafa.ssl.openssl.api.dh/g' "$file"
  sed -i 's/fafafa\.ssl\.openssl\.ec/fafafa.ssl.openssl.api.ec/g' "$file"
  sed -i 's/fafafa\.ssl\.openssl\.bn/fafafa.ssl.openssl.api.bn/g' "$file"
  sed -i 's/fafafa\.ssl\.openssl\.rand/fafafa.ssl.openssl.api.rand/g' "$file"
  sed -i 's/fafafa\.ssl\.openssl\.hmac/fafafa.ssl.openssl.api.hmac/g' "$file"
  sed -i 's/fafafa\.ssl\.openssl\.md/fafafa.ssl.openssl.api.md/g' "$file"
  sed -i 's/fafafa\.ssl\.openssl\.sha/fafafa.ssl.openssl.api.sha/g' "$file"
  sed -i 's/fafafa\.ssl\.openssl\.aes/fafafa.ssl.openssl.api.aes/g' "$file"
  sed -i 's/fafafa\.ssl\.openssl\.des/fafafa.ssl.openssl.api.des/g' "$file"
  sed -i 's/fafafa\.ssl\.openssl\.buffer/fafafa.ssl.openssl.api.buffer/g' "$file"
  sed -i 's/fafafa\.ssl\.openssl\.asn1/fafafa.ssl.openssl.api.asn1/g' "$file"
  sed -i 's/fafafa\.ssl\.openssl\.crypto/fafafa.ssl.openssl.api.crypto/g' "$file"
  sed -i 's/fafafa\.ssl\.openssl\.consts/fafafa.ssl.openssl.api.consts/g' "$file"
  sed -i 's/fafafa\.ssl\.openssl\.utils/fafafa.ssl.openssl.utils/g' "$file"
done

echo ""
echo "=== 替换完成 ==="
echo ""

# 验证
echo "验证结果:"
REMAINING=$(grep -rl "fafafa\.ssl\.openssl\." --include="*.pas" examples/ tests/ | grep -v "fafafa\.ssl\.openssl\.api\." | grep -v "fafafa\.ssl\.openssl\.utils" | wc -l)
echo "剩余未修正文件: $REMAINING"
echo ""

if [ $REMAINING -eq 0 ]; then
  echo "✅ 所有文件已修正！"
else
  echo "⚠️  还有 $REMAINING 个文件需要手动检查"
  grep -rl "fafafa\.ssl\.openssl\." --include="*.pas" examples/ tests/ | grep -v "fafafa\.ssl\.openssl\.api\." | grep -v "fafafa\.ssl\.openssl\.utils" | head -10
fi

echo ""
echo "备份文件: unit_names_backup_*.tar.gz"
echo "如需恢复: tar -xzf unit_names_backup_*.tar.gz"


