# rename_openssl_files.ps1
# 将所有 OpenSSL API 模块文件重命名为 fafafa.ssl.openssl.api.* 格式

$srcDir = "D:\projects\Pascal\lazarus\My\libs\fafafa.ssl\src"
Push-Location $srcDir

Write-Host "===========================================" -ForegroundColor Cyan
Write-Host "  OpenSSL 文件重命名脚本" -ForegroundColor Cyan
Write-Host "===========================================" -ForegroundColor Cyan
Write-Host ""

# 定义排除列表（这些文件不需要重命名）
$excludeFiles = @(
    "fafafa.ssl.openssl.pas",
    "fafafa.ssl.openssl.types.pas",
    "fafafa.ssl.openssl.api.pas"
)

# 获取所有需要重命名的文件
$files = Get-ChildItem -Filter "fafafa.ssl.openssl.*.pas" | Where-Object {
    $excludeFiles -notcontains $_.Name
}

Write-Host "找到 $($files.Count) 个文件需要重命名`n" -ForegroundColor Yellow

# 显示将要执行的操作
Write-Host "预览重命名操作：" -ForegroundColor Green
Write-Host "----------------------------------------"
$counter = 0
foreach ($file in $files) {
    $counter++
    $oldName = $file.Name
    $newName = $oldName -replace "fafafa\.ssl\.openssl\.", "fafafa.ssl.openssl.api."
    Write-Host "$counter. $oldName"
    Write-Host "   -> $newName" -ForegroundColor Gray
}

Write-Host "----------------------------------------`n"
Write-Host "开始重命名..." -ForegroundColor Yellow

# 执行重命名
$renamed = 0
foreach ($file in $files) {
    $oldName = $file.Name
    $newName = $oldName -replace "fafafa\.ssl\.openssl\.", "fafafa.ssl.openssl.api."
    
    try {
        Rename-Item $file.FullName $newName -ErrorAction Stop
        $renamed++
        Write-Host "✓ $oldName" -ForegroundColor Green
    }
    catch {
        Write-Host "✗ $oldName - 错误: $_" -ForegroundColor Red
    }
}

Write-Host ""
Write-Host "===========================================" -ForegroundColor Cyan
Write-Host "完成！成功重命名 $renamed/$($files.Count) 个文件" -ForegroundColor Green
Write-Host "===========================================" -ForegroundColor Cyan

Pop-Location
