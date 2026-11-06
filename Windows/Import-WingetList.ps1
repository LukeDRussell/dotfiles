#Requires -Version 5.1
# Install one of the winget-*.json lists here, then pin every package in it
# that has a Version, so `winget upgrade --all` leaves it at that version.
# Already-installed packages are left alone (no upgrades), so a Version or
# Scope only applies when a package is first installed; for one installed at
# another version or scope this warns, and -Force uninstalls it and installs
# the list's version and scope instead. Used by install.ps1, install-apps.ps1,
# install-games.ps1 and the profile's wgin. Works in Windows PowerShell 5.1.
# Safe to re-run.
[CmdletBinding(SupportsShouldProcess)]
param(
    [Parameter(Mandatory)]
    [string]$Path,
    [switch]$Force
)
$ErrorActionPreference = 'Stop'
$Path = (Resolve-Path -LiteralPath $Path).ProviderPath
$agree = '--accept-source-agreements', '--disable-interactivity'

# 0.102.3001 and 0.102.3001.0 are the same version
function ConvertTo-Version([string]$Text) {
    $parts = @($Text -split '\.')
    while ($parts.Count -lt 4) { $parts += '0' }
    $version = $null
    if ([version]::TryParse(($parts -join '.'), [ref]$version)) { $version } else { $Text }
}

# The installed version from winget list's table: the column after the ID
function Get-InstalledVersion([string]$Id, [string]$Source) {
    $line = winget list --id $Id --exact --source $Source @agree 2>$null |
        Where-Object { $_ -match "\s$([regex]::Escape($Id))\s" } | Select-Object -First 1
    if (-not $line) { return $null }
    $after = ($line -split [regex]::Escape($Id), 2)[1].Trim() -split '\s+' | Where-Object { $_ -notin '<', '>' }
    @($after)[0]
}

if ($PSCmdlet.ShouldProcess($Path, 'winget import')) {
    winget import --import-file $Path --no-upgrade --ignore-unavailable --accept-package-agreements @agree
    if ($LASTEXITCODE -ne 0) { Write-Warning "winget import of $Path reported problems (exit code $LASTEXITCODE)." }
}

$json = Get-Content -Raw -LiteralPath $Path | ConvertFrom-Json
foreach ($source in $json.Sources) {
    $sourceName = $source.SourceDetails.Name
    foreach ($package in @($source.Packages) | Where-Object { $_.Version -or $_.Scope }) {
        $id = $package.PackageIdentifier
        $want = $package.Version
        $scope = $package.Scope

        # Is it installed, and at the list's version and scope?
        $installed = Get-InstalledVersion $id $sourceName
        $problems = @()
        if ($installed -and $want -and (ConvertTo-Version $installed) -ne (ConvertTo-Version $want)) {
            $problems += "at $installed, not $want"
        }
        if ($installed -and $scope) {
            winget list --id $id --exact --source $sourceName --scope $scope @agree *> $null
            if ($LASTEXITCODE -ne 0) {
                $problems += if ($scope -eq 'machine') { 'per user, not machine-wide' } else { 'machine-wide, not per user' }
            }
        }
        if ($problems) {
            $what = "$id is installed $($problems -join ' and ')"
            if (-not $Force) {
                Write-Warning "$what. Run again with -Force (wgin -Force) to reinstall it."
            }
            elseif ($PSCmdlet.ShouldProcess($id, "Reinstall ($what)")) {
                # A pin at another version could get in the way; it's set again below
                winget pin remove --id $id --exact --source $sourceName @agree *> $null
                winget uninstall --id $id --exact --source $sourceName --silent @agree
                if ($LASTEXITCODE -ne 0) { Write-Warning "Couldn't uninstall $id (exit code $LASTEXITCODE)."; continue }
                $options = @('--id', $id, '--exact', '--source', $sourceName, '--silent', '--accept-package-agreements') + $agree
                if ($want) { $options += '--version', $want }
                if ($scope) { $options += '--scope', $scope }
                winget install @options
                if ($LASTEXITCODE -ne 0) { Write-Warning "Couldn't install $id (exit code $LASTEXITCODE)."; continue }
            }
        }

        if ($want -and $PSCmdlet.ShouldProcess("$id $want", 'winget pin add')) {
            # --force replaces an existing pin, e.g. one at an older version
            winget pin add --id $id --exact --version $want --source $sourceName --force @agree | Out-Null
            if ($LASTEXITCODE -ne 0) { Write-Warning "Couldn't pin $id to $want (exit code $LASTEXITCODE)." }
        }
    }
}
