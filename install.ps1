#Requires -Version 5.1
# Bootstrap dotfiles on Windows: install apps with winget, then point chezmoi at
# this repo and apply. Safe to re-run. Works in Windows PowerShell 5.1, so it
# runs on a fresh machine before PowerShell 7 is installed. Needs Developer Mode
# (or admin) so chezmoi can create symlinks.
[CmdletBinding(SupportsShouldProcess)]
param(
    # Don't install the apps listed in Windows/winget-packages.json
    [switch]$SkipApps
)
$ErrorActionPreference = 'Stop'

# Pick up PATH changes from installers without opening a new shell
function Update-Path {
    $env:PATH = [Environment]::GetEnvironmentVariable('PATH', 'Machine') + [IO.Path]::PathSeparator +
                [Environment]::GetEnvironmentVariable('PATH', 'User')
}

$apps = Join-Path $PSScriptRoot 'Windows\winget-packages.json'
if (-not $SkipApps -and $PSCmdlet.ShouldProcess($apps, 'winget import')) {
    # Apps that are already installed are left alone (no upgrades)
    winget import --import-file $apps --no-upgrade --ignore-unavailable `
        --accept-package-agreements --accept-source-agreements
    if ($LASTEXITCODE -ne 0) { Write-Warning "winget import reported problems (exit code $LASTEXITCODE); carrying on." }
    Update-Path
}

# PSFzf (fzf key bindings for the profile) comes from the PowerShell Gallery,
# and must be installed by PowerShell 7 so it lands in 7's module folder
$pwsh = Get-Command pwsh -ErrorAction SilentlyContinue
if (-not $SkipApps -and $pwsh -and $PSCmdlet.ShouldProcess('PSFzf', 'Install-Module')) {
    & $pwsh.Source -NoProfile -Command 'if (-not (Get-Module -ListAvailable PSFzf)) { Install-Module PSFzf -Scope CurrentUser -Repository PSGallery -Force }'
}

if (-not (Get-Command chezmoi -ErrorAction SilentlyContinue)) {
    winget install --id twpayne.chezmoi --exact --source winget --accept-package-agreements
    Update-Path
}

# Older setups linked Windows Terminal's LocalState with a junction. Remove only
# the link (never its contents) so chezmoi can replace it with a symlink.
$wtState = "$env:LOCALAPPDATA\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState"
$item = Get-Item -LiteralPath $wtState -Force -ErrorAction SilentlyContinue
if ($item.LinkType -eq 'Junction' -and $PSCmdlet.ShouldProcess($wtState, 'Remove legacy junction')) {
    [IO.Directory]::Delete($wtState, $false)
}

chezmoi init --source $PSScriptRoot
if ($PSCmdlet.ShouldProcess($HOME, 'chezmoi apply')) {
    chezmoi apply --verbose
}
