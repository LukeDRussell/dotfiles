# oh-my-posh init pwsh | Invoke-Expression

New-Alias -name which -Value Get-Command
New-Alias -name ll -Value Get-ChildItem
New-Alias -name wget -Value Invoke-WebRequest
New-Alias -name docker -Value podman

Remove-Alias diff -Force
Import-Module -Name Microsoft.WinGet.CommandNotFound

function pip {
  uv pip
}
