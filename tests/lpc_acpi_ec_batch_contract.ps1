$ErrorActionPreference = 'Stop'

$module = Join-Path $PSScriptRoot '..\LpcACPIEC.p'
$source = Get-Content -LiteralPath $module -Raw

function Assert-True {
    param(
        [bool]$Condition,
        [string]$Message
    )

    if (-not $Condition) {
        throw "ASSERT FAILED: $Message"
    }
}

function Get-RequiredCells {
    param([int]$Count)
    return [int][Math]::Ceiling($Count / 8.0)
}

function Test-RangeContract {
    param(
        [int]$Start,
        [int]$Count,
        [int]$OutputCells
    )

    if ($Start -lt 0 -or $Start -ge 256 -or $Count -le 0 -or $Count -gt 256) {
        return $false
    }
    if (($Start + $Count) -gt 256) {
        return $false
    }
    return $OutputCells -eq (Get-RequiredCells $Count)
}

Assert-True ($source -match 'DEFINE_IOCTL_SIZED\(ioctl_ec_read_byte, 1, 1\)') 'single-register IOCTL contract is missing'
Assert-True ($source -match 'DEFINE_IOCTL\(ioctl_ec_read_range\)') 'range IOCTL contract is missing'
Assert-True ($source -match 'out_size != required_cells') 'range output size must be exact'
Assert-True ($source -match 'pack_bytes_le\(data, out, count\)') 'range output must use little-endian packing'
Assert-True ($source -match 'EC_COMMAND_READ = 0x80') 'RD_EC command is missing'
Assert-True ($source -match 'EC_STATUS_INPUT_BUFFER_FULL = 0x02') 'IBF status bit is missing'
Assert-True ($source -match 'EC_STATUS_OUTPUT_BUFFER_FULL = 0x01') 'OBF status bit is missing'

$validCases = @(
    @(0, 1, 1),
    @(0, 8, 1),
    @(0, 9, 2),
    @(0, 256, 32),
    @(255, 1, 1)
)
foreach ($case in $validCases) {
    Assert-True (Test-RangeContract -Start $case[0] -Count $case[1] -OutputCells $case[2]) "valid range rejected: start=$($case[0]) count=$($case[1]) cells=$($case[2])"
}

$invalidCases = @(
    @(0, 0, 0),
    @(256, 1, 1),
    @(255, 2, 1),
    @(0, 9, 1),
    @(0, 8, 2)
)
foreach ($case in $invalidCases) {
    Assert-True (-not (Test-RangeContract -Start $case[0] -Count $case[1] -OutputCells $case[2])) "invalid range accepted: start=$($case[0]) count=$($case[1]) cells=$($case[2])"
}

Write-Output 'LpcACPIEC batch contract smoke passed.'
