oh-my-posh init pwsh | Invoke-Expression

New-Alias -name which -Value Get-Command
New-Alias -name ll -Value Get-ChildItem
New-Alias -name wget -Value Invoke-WebRequest
New-Alias -name uvx -Value "uv tool run"

#f45873b3-b655-43a6-b217-97c00aa0db58 PowerToys CommandNotFound module

Import-Module -Name Microsoft.WinGet.CommandNotFound
#f45873b3-b655-43a6-b217-97c00aa0db58
(& uv generate-shell-completion powershell) | Out-String | Invoke-Expression
