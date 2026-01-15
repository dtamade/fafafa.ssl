# Document Line Counter
# Purpose: Automatically count lines in documentation files to avoid manual errors
# Usage: .\count_lines.ps1 [Pattern]

param(
    [string]$Pattern = "*",
    [switch]$Markdown,
    [switch]$Pascal
)

$rootDir = Split-Path -Parent $PSScriptRoot

Write-Host "=== Document Line Counter ===" -ForegroundColor Cyan
Write-Host "Root directory: $rootDir`n" -ForegroundColor Gray

$filters = @()
if ($Markdown) {
    $filters += "*.md"
}
if ($Pascal) {
    $filters += "*.pas", "*.lpr"
}
if ($filters.Count -eq 0) {
    $filters = @("*.md", "*.pas", "*.lpr")
}

$results = @()

foreach ($filter in $filters) {
    $files = Get-ChildItem -Path $rootDir -Filter $filter -Recurse -File | 
             Where-Object { $_.Name -like "*$Pattern*" } |
             Where-Object { $_.FullName -notlike "*\node_modules\*" -and 
                           $_.FullName -notlike "*\.git\*" }
    
    foreach ($file in $files) {
        $lines = (Get-Content $file.FullName -ErrorAction SilentlyContinue | Measure-Object -Line).Lines
        if ($null -eq $lines) { $lines = 0 }
        
        $relativePath = $file.FullName.Substring($rootDir.Length + 1)
        
        $results += [PSCustomObject]@{
            File = $file.Name
            Path = $relativePath
            Lines = $lines
            Extension = $file.Extension
        }
    }
}

# Sort and display
$results = $results | Sort-Object -Property Lines -Descending

Write-Host "Found $($results.Count) file(s):`n" -ForegroundColor Green

# Display as table
$results | Format-Table -Property @{
    Label = "File"
    Expression = { $_.File }
    Width = 40
}, @{
    Label = "Lines"
    Expression = { $_.Lines }
    Width = 10
    Alignment = "Right"
}, @{
    Label = "Path"
    Expression = { $_.Path }
} -AutoSize

# Summary statistics
$totalLines = ($results | Measure-Object -Property Lines -Sum).Sum
$avgLines = [math]::Round(($results | Measure-Object -Property Lines -Average).Average, 0)
$maxLines = ($results | Measure-Object -Property Lines -Maximum).Maximum

Write-Host "`n=== Statistics ===" -ForegroundColor Cyan
Write-Host "Total files: $($results.Count)" -ForegroundColor White
Write-Host "Total lines: $totalLines" -ForegroundColor White
Write-Host "Average lines per file: $avgLines" -ForegroundColor White
Write-Host "Largest file: $maxLines lines" -ForegroundColor White

# Export to CSV if requested
if ($env:EXPORT_CSV -eq "1") {
    $csvPath = Join-Path $rootDir "line_count_report.csv"
    $results | Export-Csv -Path $csvPath -NoTypeInformation
    Write-Host "`nExported to: $csvPath" -ForegroundColor Yellow
}
