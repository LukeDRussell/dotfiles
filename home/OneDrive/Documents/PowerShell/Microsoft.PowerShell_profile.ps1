$PSStyle.FileInfo.Directory = $PSStyle.Foreground.BrightBlue

if (Get-Command oh-my-posh -ErrorAction SilentlyContinue) {
    oh-my-posh init pwsh --config '~/.config/oh-my-posh/config.omp.json' | Invoke-Expression
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
$OnViModeChange = [scriptblock]{
    if ($args[0] -eq 'Command') {
        # Set the cursor to a blinking block.
        Write-Host -NoNewLine "`e[2 q"
    }
    else {
        # Set the cursor to a blinking line.
        Write-Host -NoNewLine "`e[5 q"
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



# =============================================================================
#
# Utility functions for zoxide.
#

# Call zoxide binary, returning the output as UTF-8.
function global:__zoxide_bin {
    $encoding = [Console]::OutputEncoding
    try {
        [Console]::OutputEncoding = [System.Text.Utf8Encoding]::new()
        $result = zoxide @args
        return $result
    } finally {
        [Console]::OutputEncoding = $encoding
    }
}

# pwd based on zoxide's format.
function global:__zoxide_pwd {
    $cwd = Microsoft.PowerShell.Management\Get-Location
    if ($cwd.Provider.Name -eq "FileSystem") {
        $cwd.ProviderPath
    }
}

# cd + custom logic based on the value of _ZO_ECHO.
function global:__zoxide_cd($dir, $literal) {
    $dir = if ($literal) {
        if ($null -eq $dir) {
            Microsoft.PowerShell.Management\Set-Location
        } else {
            Microsoft.PowerShell.Management\Set-Location -LiteralPath $dir -Passthru -ErrorAction Stop
        }
    } else {
        if ($dir -eq '-' -and ($PSVersionTable.PSVersion -lt 6.1)) {
            Microsoft.PowerShell.Utility\Write-Error "cd - is not supported below PowerShell 6.1. Please upgrade your version of PowerShell."
        }
        elseif ($dir -eq '+' -and ($PSVersionTable.PSVersion -lt 6.2)) {
            Microsoft.PowerShell.Utility\Write-Error "cd + is not supported below PowerShell 6.2. Please upgrade your version of PowerShell."
        }
        else {
            Microsoft.PowerShell.Management\Set-Location -Path $dir -Passthru -ErrorAction Stop
        }
    }
}

# =============================================================================
#
# Hook configuration for zoxide.
#

# Hook to add new entries to the database.
$global:__zoxide_oldpwd = __zoxide_pwd
function global:__zoxide_hook {
    $result = __zoxide_pwd
    if ($result -ne $global:__zoxide_oldpwd) {
        if ($null -ne $result) {
            zoxide add "--" $result
        }
        $global:__zoxide_oldpwd = $result
    }
}

# Initialize hook.
$global:__zoxide_hooked = (Microsoft.PowerShell.Utility\Get-Variable __zoxide_hooked -ErrorAction Ignore -ValueOnly)
if ($global:__zoxide_hooked -ne 1) {
    $global:__zoxide_hooked = 1
    $global:__zoxide_prompt_old = $function:prompt

    function global:prompt {
        if ($null -ne $__zoxide_prompt_old) {
            & $__zoxide_prompt_old
        }
        $null = __zoxide_hook
    }
}

# =============================================================================
#
# When using zoxide with --no-cmd, alias these internal functions as desired.
#

# Jump to a directory using only keywords.
function global:__zoxide_z {
    if ($args.Length -eq 0) {
        __zoxide_cd $null $true
    }
    elseif ($args.Length -eq 1 -and ($args[0] -eq '-' -or $args[0] -eq '+')) {
        __zoxide_cd $args[0] $false
    }
    elseif ($args.Length -eq 1 -and (Microsoft.PowerShell.Management\Test-Path -PathType Container -LiteralPath $args[0])) {
        __zoxide_cd $args[0] $true
    }
    elseif ($args.Length -eq 1 -and (Microsoft.PowerShell.Management\Test-Path -PathType Container -Path $args[0] )) {
        __zoxide_cd $args[0] $false
    }
    else {
        $result = __zoxide_pwd
        if ($null -ne $result) {
            $result = __zoxide_bin query --exclude $result "--" @args
        }
        else {
            $result = __zoxide_bin query "--" @args
        }
        if ($LASTEXITCODE -eq 0) {
            __zoxide_cd $result $true
        }
    }
}

# Jump to a directory using interactive search.
function global:__zoxide_zi {
    $result = __zoxide_bin query -i "--" @args
    if ($LASTEXITCODE -eq 0) {
        __zoxide_cd $result $true
    }
}

# =============================================================================
#
# Commands for zoxide. Disable these using --no-cmd.
#

Microsoft.PowerShell.Utility\Set-Alias -Name z -Value __zoxide_z -Option AllScope -Scope Global -Force
Microsoft.PowerShell.Utility\Set-Alias -Name zi -Value __zoxide_zi -Option AllScope -Scope Global -Force

# =============================================================================
#
# To initialize zoxide, add this to your configuration (find it by running
# `echo $profile` in PowerShell):
#
# Invoke-Expression (& { (zoxide init powershell | Out-String) })
