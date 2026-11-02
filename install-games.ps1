#Requires -Version 5.1
# Game save backups with Ludusavi: install it, link its config (Luke-Desktop
# only, see home/.chezmoiignore), and schedule `ludusavi backup --force` at
# sign-in and daily. Backups go to OneDrive\Game Saves. Safe to re-run.
[CmdletBinding(SupportsShouldProcess)]
param()
$ErrorActionPreference = 'Stop'

if ($PSCmdlet.ShouldProcess('mtkennerly.ludusavi', 'winget install')) {
    winget install --id mtkennerly.ludusavi --exact --source winget --no-upgrade `
        --accept-package-agreements --accept-source-agreements --disable-interactivity | Out-Null
}

# chezmoi replaces the config with a link into this repo; keep any existing copy
$config = "$env:APPDATA\ludusavi\config.yaml"
$item = Get-Item -LiteralPath $config -Force -ErrorAction SilentlyContinue
if ($item -and -not $item.LinkType -and $PSCmdlet.ShouldProcess($config, 'Back up')) {
    Copy-Item -LiteralPath $config "$config.bak" -Force
}
if ($PSCmdlet.ShouldProcess($config, 'chezmoi apply')) {
    chezmoi apply --force --verbose (Split-Path $config)
}

# winget installs it as a portable app, with no Start menu entry. The Links
# path stays the same across upgrades; run without arguments, it opens the GUI.
$ludusavi = "$env:LOCALAPPDATA\Microsoft\WinGet\Links\ludusavi.exe"
$shortcut = "$env:APPDATA\Microsoft\Windows\Start Menu\Programs\Ludusavi.lnk"
if ($PSCmdlet.ShouldProcess($shortcut, 'Create shortcut')) {
    $lnk = (New-Object -ComObject WScript.Shell).CreateShortcut($shortcut)
    $lnk.TargetPath = $ludusavi
    $lnk.IconLocation = @((Get-Item -LiteralPath $ludusavi).Target)[0] + ',0'
    $lnk.Description = 'Back up and restore game saves'
    $lnk.Save()
}

# conhost --headless runs the console app without flashing a window
$user = "$env:USERDOMAIN\$env:USERNAME"
$action = New-ScheduledTaskAction -Execute "$env:WINDIR\System32\conhost.exe" -Argument "--headless `"$ludusavi`" backup --force"
$logon = New-ScheduledTaskTrigger -AtLogOn -User $user
$logon.Delay = 'PT5M'
$daily = New-ScheduledTaskTrigger -Daily -At 21:00
# StartWhenAvailable catches up a daily run missed while the PC was off
$settings = New-ScheduledTaskSettingsSet -StartWhenAvailable -AllowStartIfOnBatteries -DontStopIfGoingOnBatteries `
    -ExecutionTimeLimit (New-TimeSpan -Hours 1) -MultipleInstances IgnoreNew
$principal = New-ScheduledTaskPrincipal -UserId $user -LogonType Interactive -RunLevel Limited
if ($PSCmdlet.ShouldProcess('Ludusavi backup', 'Register scheduled task')) {
    Register-ScheduledTask -TaskName 'Ludusavi backup' -Description 'Back up game saves to OneDrive\Game Saves (ludusavi backup --force)' `
        -Action $action -Trigger $logon, $daily -Settings $settings -Principal $principal -Force | Out-Null
}
