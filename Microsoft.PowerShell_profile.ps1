oh-my-posh init pwsh | Invoke-Expression

Get-ChildItem "$PROFILE\..\Completions\" | ForEach-Object {
    . $_.FullName
}

New-Alias -name which -Value Get-Command
New-Alias -name ll -Value Get-ChildItem
New-Alias -name wget -Value Invoke-WebRequest
