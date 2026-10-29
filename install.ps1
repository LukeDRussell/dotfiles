#Requires -Version 7
# Bootstrap dotfiles on Windows: install chezmoi, point it at this repo, apply.
# Safe to re-run. Needs Developer Mode (or admin) so chezmoi can create symlinks.
[CmdletBinding(SupportsShouldProcess)]
param()
$ErrorActionPreference = 'Stop'

if (-not (Get-Command chezmoi -ErrorAction SilentlyContinue)) {
    winget install --id twpayne.chezmoi --exact --source winget --accept-package-agreements
    $env:PATH = [Environment]::GetEnvironmentVariable('PATH', 'Machine') + [IO.Path]::PathSeparator +
                [Environment]::GetEnvironmentVariable('PATH', 'User')
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
