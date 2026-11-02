#Requires -Version 5.1
# Install every app in Windows/winget-packages.json. Apps that are already
# installed are left alone (no upgrades). Safe to re-run. install.ps1 only
# installs what the PowerShell setup needs; run this for everything else.
[CmdletBinding(SupportsShouldProcess)]
param()
$ErrorActionPreference = 'Stop'

$apps = Join-Path $PSScriptRoot 'Windows\winget-packages.json'
if ($PSCmdlet.ShouldProcess($apps, 'winget import')) {
    winget import --import-file $apps --no-upgrade --ignore-unavailable `
        --accept-package-agreements --accept-source-agreements
    if ($LASTEXITCODE -ne 0) { Write-Warning "winget import reported problems (exit code $LASTEXITCODE)." }
}
