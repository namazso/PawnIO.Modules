# SPDX-License-Identifier: LGPL-2.1-or-later
[CmdletBinding()]
param([string]$MinGwBin, [string]$CachePath, [switch]$Offline)
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$testRoot = $PSScriptRoot
$repoRoot = Split-Path -Parent (Split-Path -Parent $testRoot)
if (-not $CachePath) { $CachePath = Join-Path $testRoot '.build' }
$CachePath = [IO.Path]::GetFullPath($CachePath)
New-Item -ItemType Directory -Force -Path $CachePath | Out-Null
if (-not $MinGwBin) { $MinGwBin = Split-Path -Parent (Get-Command cmake -ErrorAction Stop).Source }
$testCmake = Join-Path $MinGwBin 'cmake.exe'
$testGcc = Join-Path $MinGwBin 'gcc.exe'
$testMake = Join-Path $MinGwBin 'mingw32-make.exe'
foreach ($testTool in @($testCmake, $testGcc, $testMake)) {
    if (-not (Test-Path -LiteralPath $testTool -PathType Leaf)) { throw "Missing tool: $testTool" }
}
$testInclude = Join-Path $repoRoot 'include'
if (-not (Test-Path -LiteralPath (Join-Path $testInclude 'pawnio.inc'))) {
    throw 'Run from a PawnIO.Modules checkout containing include/pawnio.inc.'
}
$testArchive = Join-Path $CachePath 'pawn-4.1.7152.zip'
if (-not (Test-Path -LiteralPath $testArchive)) {
    if ($Offline) { throw 'Offline dependency missing: pawn-4.1.7152.zip' }
    Invoke-WebRequest -Uri 'https://www.compuphase.com/pawn/pawn-4.1.7152.zip' -OutFile $testArchive
}
if ((Get-FileHash -LiteralPath $testArchive -Algorithm SHA256).Hash -ne
        'C2E7212098A68AD1CBACC8B5CFE44A7E6F95B75A82D1E5D43CCF9330F991A87B') {
    throw 'Pawn source archive hash mismatch.'
}
$testSource = Join-Path $CachePath 'pawn-7152-source'
Expand-Archive -LiteralPath $testArchive -DestinationPath $testSource -Force
$testBuild = Join-Path $CachePath 'ddr5-readonly-mock-build'
& $testCmake -S $testRoot -B $testBuild -G 'MinGW Makefiles' `
    "-DPAWN_SOURCE=$testSource" '-DCMAKE_BUILD_TYPE=Debug' "-DCMAKE_C_COMPILER=$testGcc" "-DCMAKE_MAKE_PROGRAM=$testMake"
if ($LASTEXITCODE -ne 0) { throw 'Mock toolchain configure failed.' }
& $testCmake --build $testBuild --parallel 2
if ($LASTEXITCODE -ne 0) { throw 'Mock toolchain build failed.' }
$testBinary = Join-Path $testBuild 'Ddr5ReadOnly.amx'
$testCompilerOutput = & (Join-Path $testBuild 'pawncc.exe') (Join-Path $repoRoot 'Ddr5ReadOnly.p') `
    '-C64' '-;+' '-(+' '-p' "-i$testInclude" "-o$testBinary" 2>&1
$testCompileExit = $LASTEXITCODE
$testCompilerOutput | ForEach-Object { Write-Host $_ }
if ($testCompileExit -ne 0 -or "$testCompilerOutput" -match '(?i)warning\s+\d+|error\s+\d+') {
    throw 'The module must compile without warnings or errors.'
}
& (Join-Path $testBuild 'readonly-module-tests.exe') $testBinary
if ($LASTEXITCODE -ne 0) { throw 'Compiled-module mock tests failed.' }
Write-Host "Unsigned AMX SHA256: $((Get-FileHash -LiteralPath $testBinary -Algorithm SHA256).Hash)"
Write-Host 'User-mode mocks only. No driver installation, kernel execution or real-hardware validation.'
