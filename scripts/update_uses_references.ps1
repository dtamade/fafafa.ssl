# update_uses_references.ps1
# 更新所有文件中对 OpenSSL 模块的 uses 引用

param(
    [string]$ProjectRoot = ""
)

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
if ([string]::IsNullOrWhiteSpace($ProjectRoot)) {
    $resolvedProjectRoot = Split-Path -Parent $scriptDir
}
elseif ([System.IO.Path]::IsPathRooted($ProjectRoot)) {
    $resolvedProjectRoot = $ProjectRoot
}
else {
    $resolvedProjectRoot = Join-Path (Get-Location).Path $ProjectRoot
}
$resolvedProjectRoot = [System.IO.Path]::GetFullPath($resolvedProjectRoot)
$srcDir = $resolvedProjectRoot

if (-not (Test-Path $srcDir)) {
    throw "project root not found: $srcDir"
}

Write-Host "==========================================="
Write-Host "  更新 Uses 引用脚本"
Write-Host "==========================================="
Write-Host ""

# 需要更新的模块列表
$modules = @(
    'aead', 'aes', 'aria', 'asn1', 'async', 'bio', 'blake2', 'bn', 'buffer',
    'chacha', 'cmac', 'cmac.evp', 'cms', 'comp', 'conf', 'consts', 'core',
    'crypto', 'ct', 'des', 'dh', 'dsa', 'dso', 'ec', 'ecdh', 'ecdsa',
    'engine', 'err', 'evp', 'hmac', 'kdf', 'legacy_ciphers', 'lhash', 'md',
    'modes', 'obj', 'ocsp', 'param', 'pem', 'pkcs', 'pkcs7', 'pkcs12',
    'provider', 'rand', 'rand_old', 'rsa', 'scrypt_whirlpool', 'seed', 'sha',
    'sha3', 'sha3.evp', 'sm', 'srp', 'ssl', 'stack', 'store', 'thread', 'ts',
    'txt_db', 'ui', 'utils', 'x509', 'x509v3'
)

# 获取所有 Pascal 文件
$files = Get-ChildItem -Path $srcDir -Recurse -Filter "*.pas"

Write-Host "扫描 $($files.Count) 个文件...`n"

$updated = 0
$totalChanges = 0

foreach ($file in $files) {
    $content = Get-Content $file.FullName -Raw
    $originalContent = $content
    $fileChanged = $false
    
    foreach ($module in $modules) {
        $oldRef = "fafafa.ssl.openssl.$module"
        $newRef = "fafafa.ssl.openssl.api.$module"
        
        if ($content -match [regex]::Escape($oldRef)) {
            $content = $content -replace [regex]::Escape($oldRef), $newRef
            $fileChanged = $true
            $totalChanges++
        }
    }
    
    if ($fileChanged) {
        Set-Content $file.FullName -Value $content -NoNewline
        $updated++
        $relPath = $file.FullName
        if ($relPath.StartsWith($srcDir, [System.StringComparison]::OrdinalIgnoreCase)) {
            $relPath = $relPath.Substring($srcDir.Length).TrimStart('\', '/')
        }
        Write-Host "✓ $relPath" -ForegroundColor Green
    }
}

Write-Host ""
Write-Host "==========================================="
Write-Host "完成！"
Write-Host "更新了 $updated 个文件"
Write-Host "总共 $totalChanges 处引用"
Write-Host "==========================================="
