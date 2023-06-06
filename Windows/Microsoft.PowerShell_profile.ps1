oh-my-posh init pwsh | Invoke-Expression

Get-ChildItem "$PROFILE\..\Completions\" | ForEach-Object {
    . $_.FullName
}

function pdm-venv-activate {
    Invoke-Expression (pdm venv activate in-project)
}

New-Alias -name which -Value Get-Command
New-Alias -name pdm-va -Value pdm-venv-activate