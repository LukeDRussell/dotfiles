$PSStyle.FileInfo.Directory = $PSStyle.Foreground.BrightBlue

# Prompt, in the style of Aphrodite (github.com/win0err/aphrodite-terminal-theme)
# plus an OS icon:
#    [venv] user@host:~\dir <branch> 12:34:56
#   $
# The branch is green when clean, yellow with changes; $ turns red after a
# failed command (# instead of $ when elevated). Icons need a Nerd Font.
$global:PromptHost = [Net.Dns]::GetHostName()
$global:PromptHasGit = [bool](Get-Command git -ErrorAction SilentlyContinue)
$global:PromptChar = if (([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) { '#' } else { '$' }

function global:prompt {
    $ok = $?
    $exitCode = $global:LASTEXITCODE
    $e = [char]27

    $path = $PWD.ProviderPath
    if ($path -and $path.StartsWith($HOME, [StringComparison]::OrdinalIgnoreCase)) {
        $path = '~' + $path.Substring($HOME.Length)
    }
    if (-not $path) { $path = $PWD.Path }   # e.g. registry locations

    $git = ''
    if ($global:PromptHasGit -and $PWD.Provider.Name -eq 'FileSystem') {
        # One git call gives both the branch (first line) and whether anything changed
        $lines = @(git --no-optional-locks status --porcelain --branch 2>$null)
        if ($LASTEXITCODE -eq 0 -and $lines.Count -gt 0) {
            $branch = ($lines[0] -replace '^## ', '' -replace '\.\.\..*$', '') -split ' ' | Select-Object -Last 1
            if ($branch -eq 'branch)') { $branch = 'HEAD' }   # "HEAD (no branch)"
            $colour = if ($lines.Count -gt 1) { 93 } else { 92 }
            $git = " $e[${colour}m<$branch>"
        }
    }

    $venv = if ($env:VIRTUAL_ENV) { "$e[37m[$(Split-Path -Leaf $env:VIRTUAL_ENV)] " } else { '' }
    $ssh = if ($env:SSH_CONNECTION) { "$e[90m[$e[95mssh$e[90m]" } else { '' }
    $charColour = if ($ok) { 0 } else { 31 }
    $time = Get-Date -Format 'HH:mm:ss'

    $global:LASTEXITCODE = $exitCode
    "$e[94m`u{e70f}$e[0m $venv$e[36m$env:USERNAME$e[90m@$e[94m$global:PromptHost$ssh$e[90m:$e[0m$path$git $e[90m$time$e[0m`n$e[${charColour}m$global:PromptChar$e[0m "
}

if (Get-Command podman -ErrorAction SilentlyContinue) {
    Set-Alias -name docker -Value podman
}

Set-Alias -name which -Value Get-Command
Set-Alias -name ll -Value Get-ChildItem
Set-Alias -name wget -Value Invoke-WebRequest

$localBin = "$HOME/.local/bin"
if (Test-Path -LiteralPath $localBin -PathType Container) {
    $env:PATH += [IO.Path]::PathSeparator + $localBin
}

# enable vi motions on the shell and as editor
Import-Module PSReadLine
# The prompt is two lines; tell PSReadLine so redraws line up
Set-PSReadLineOption -ExtraPromptLineCount 1
$OnViModeChange = [scriptblock]{
    if ($args[0] -eq 'Command') {
        # Steady block in normal mode, as in bash
        Write-Host -NoNewLine "`e[2 q"
    }
    else {
        # Steady bar in insert mode, as in bash
        Write-Host -NoNewLine "`e[6 q"
    }
}

Set-PsReadLineOption -EditMode Vi
Set-PSReadLineOption -ViModeIndicator Script -ViModeChangeHandler $OnViModeChange

function Convert-ToLF {
    [CmdletBinding(SupportsShouldProcess)]
    param(
        [Parameter(Mandatory, Position = 0, ValueFromPipeline, ValueFromPipelineByPropertyName)]
        [Alias('FullName')]
        [string[]]$Path
    )
    process {
        foreach ($p in $Path) {
            foreach ($file in (Resolve-Path -LiteralPath $p -ErrorAction Stop).ProviderPath) {
                $bytes = [IO.File]::ReadAllBytes($file)
                $n = $bytes.Length

                # Detect encoding from BOM, then fall back to a null-byte heuristic
                if     ($n -ge 3 -and $bytes[0] -eq 0xEF -and $bytes[1] -eq 0xBB -and $bytes[2] -eq 0xBF) {
                    $name = 'UTF-8 (BOM)';    $enc = [Text.UTF8Encoding]::new($false);          $skip = 3
                }
                elseif ($n -ge 4 -and $bytes[0] -eq 0xFF -and $bytes[1] -eq 0xFE -and $bytes[2] -eq 0 -and $bytes[3] -eq 0) {
                    Write-Warning "$file looks like UTF-32; skipping."; continue
                }
                elseif ($n -ge 2 -and $bytes[0] -eq 0xFF -and $bytes[1] -eq 0xFE) {
                    $name = 'UTF-16 LE (BOM)'; $enc = [Text.UnicodeEncoding]::new($false, $false); $skip = 2
                }
                elseif ($n -ge 2 -and $bytes[0] -eq 0xFE -and $bytes[1] -eq 0xFF) {
                    $name = 'UTF-16 BE (BOM)'; $enc = [Text.UnicodeEncoding]::new($true, $false);  $skip = 2
                }
                elseif ($n -ge 2 -and $bytes[0] -ne 0 -and $bytes[1] -eq 0) {
                    $name = 'UTF-16 LE (no BOM)'; $enc = [Text.UnicodeEncoding]::new($false, $false); $skip = 0
                }
                elseif ($n -ge 2 -and $bytes[0] -eq 0 -and $bytes[1] -ne 0) {
                    $name = 'UTF-16 BE (no BOM)'; $enc = [Text.UnicodeEncoding]::new($true, $false);  $skip = 0
                }
                else {
                    $name = 'UTF-8';          $enc = [Text.UTF8Encoding]::new($false);          $skip = 0
                }

                if ($PSCmdlet.ShouldProcess($file, "Convert from $name to UTF-8 (no BOM) with LF endings")) {
                    $text  = $enc.GetString($bytes, $skip, $n - $skip)
                    $fixed = ($text -replace "`r`n", "`n" -replace "`r", "`n").TrimEnd("`n") + "`n"
                    [IO.File]::WriteAllText($file, $fixed, [Text.UTF8Encoding]::new($false))
                    Write-Host "Fixed: $file  [$name -> UTF-8, LF]"
                }
            }
        }
    }
}
Set-Alias crlf2lf Convert-ToLF

# zoxide: generated by the installed zoxide, so it always matches its version
if (Get-Command zoxide -ErrorAction SilentlyContinue) {
    Invoke-Expression (& { (zoxide init powershell | Out-String) })
}
