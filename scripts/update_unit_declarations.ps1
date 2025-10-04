# update_unit_declarations.ps1
# 更新所有重命名文件中的 unit 声明

$srcDir = "D:\projects\Pascal\lazarus\My\libs\fafafa.ssl\src"
Push-Location $srcDir

Write-Host "==========================================="
Write-Host "  更新 Unit 声明脚本"
Write-Host "==========================================="
Write-Host ""

# 获取所有 API 文件
$files = Get-ChildItem -Filter "fafafa.ssl.openssl.api.*.pas"

Write-Host "找到 $($files.Count) 个文件需要更新`n"

$updated = 0
foreach ($file in $files) {
    $content = Get-Content $file.FullName -Raw -Encoding UTF8
    
    # 从文件名提取模块名
    if ($file.Name -match "fafafa\.ssl\.openssl\.api\.(.+)\.pas") {
        $moduleName = $matches[1]
        $oldUnit = "unit fafafa.ssl.openssl.$moduleName;"
        $newUnit = "unit fafafa.ssl.openssl.api.$moduleName;"
        
        if ($content -match [regex]::Escape($oldUnit)) {
            $content = $content -replace [regex]::Escape($oldUnit), $newUnit
            Set-Content $file.FullName -Value $content -Encoding UTF8 -NoNewline
            $updated++
            Write-Host "✓ $($file.Name)" -ForegroundColor Green
        }
        else {
            Write-Host "- $($file.Name) (已是正确的)" -ForegroundColor Gray
        }
    }
}

Write-Host ""
Write-Host "==========================================="
Write-Host "完成！更新了 $updated/$($files.Count) 个文件"
Write-Host "==========================================="

Pop-Location
