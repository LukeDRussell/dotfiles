#Requires -Version 5.1
# Bootstrap dotfiles on Windows: install just what the PowerShell setup needs,
# then point chezmoi at this repo and apply. Safe to re-run. Works in Windows
# PowerShell 5.1, so it runs on a fresh machine before PowerShell 7 is
# installed. Needs Developer Mode (or admin) so chezmoi can create symlinks.
# All other apps: install-apps.ps1.
[CmdletBinding(SupportsShouldProcess)]
param()
$ErrorActionPreference = 'Stop'

# Pick up PATH changes from installers without opening a new shell
function Update-Path {
    $env:PATH = [Environment]::GetEnvironmentVariable('PATH', 'Machine') + [IO.Path]::PathSeparator +
                [Environment]::GetEnvironmentVariable('PATH', 'User')
}

# What the PowerShell profile and prompt rely on, in Windows/winget-shell.json
# (add to it with `wgadd <id> shell`): PowerShell 7, which the profile is
# written for; Git (prompt's branch, and this repo); chezmoi (deploys the
# dotfiles); zoxide (z); lsd (ll, lla, tree, ...); fzf (Ctrl-R, Ctrl-T, Alt-C);
# Neovim (EDITOR and git's editor). Already-installed packages are left alone;
# ones with a Version are pinned there.
& (Join-Path $PSScriptRoot 'Windows\Import-WingetList.ps1') -Path (Join-Path $PSScriptRoot 'Windows\winget-shell.json') -WhatIf:$WhatIfPreference
Update-Path
foreach ($cmd in 'pwsh', 'git', 'chezmoi', 'zoxide', 'lsd', 'fzf', 'nvim') {
    if (-not (Get-Command $cmd -ErrorAction SilentlyContinue)) { Write-Warning "$cmd isn't on PATH yet; open a new terminal if the profile misses it." }
}

# PSFzf (fzf key bindings for the profile) comes from the PowerShell Gallery,
# and must be installed by PowerShell 7 so it lands in 7's module folder
$pwsh = Get-Command pwsh -ErrorAction SilentlyContinue
if ($pwsh -and $PSCmdlet.ShouldProcess('PSFzf', 'Install-Module')) {
    & $pwsh.Source -NoProfile -Command 'if (-not (Get-Module -ListAvailable PSFzf)) { Install-Module PSFzf -Scope CurrentUser -Repository PSGallery -Force }'
}

# Older setups linked Windows Terminal's LocalState with a junction. Remove only
# the link (never its contents) so chezmoi can replace it with a symlink.
$wtState = "$env:LOCALAPPDATA\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState"
$item = Get-Item -LiteralPath $wtState -Force -ErrorAction SilentlyContinue
if ($item.LinkType -eq 'Junction' -and $PSCmdlet.ShouldProcess($wtState, 'Remove legacy junction')) {
    [IO.Directory]::Delete($wtState, $false)
}

# Windows recreates these known folders if they're deleted, so hide them instead
foreach ($name in 'Favorites', 'Links', 'Contacts', 'Searches') {
    $dir = Get-Item -LiteralPath (Join-Path $HOME $name) -Force -ErrorAction SilentlyContinue
    if ($dir -and -not ($dir.Attributes -band [IO.FileAttributes]::Hidden) -and
        $PSCmdlet.ShouldProcess($dir.FullName, 'Hide folder')) {
        $dir.Attributes = $dir.Attributes -bor [IO.FileAttributes]::Hidden
    }
}

chezmoi init --source $PSScriptRoot
if ($PSCmdlet.ShouldProcess($HOME, 'chezmoi apply')) {
    chezmoi apply --verbose
}
