#Requires -Version 5.1
# Install every app in Windows/winget-packages.json. Apps that are already
# installed are left alone (no upgrades); apps with a Version are pinned there.
# Safe to re-run. install.ps1 only installs what the PowerShell setup needs;
# run this for everything else.
[CmdletBinding(SupportsShouldProcess)]
param()
$ErrorActionPreference = 'Stop'

& (Join-Path $PSScriptRoot 'Windows\Import-WingetList.ps1') -Path (Join-Path $PSScriptRoot 'Windows\winget-packages.json') -WhatIf:$WhatIfPreference
