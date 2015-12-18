# ASM Workstation Boxstarter script
# Based on https://gist.github.com/awr/a5136e574556ed50ce94

Write-Host "ASM Workstation Boxstarter script"

$profile = "C:\Users\$($env:UserName)"
$email = Read-Host "Git email: "
$name = Read-Host "Git name: "

Write-Host ">> Chocolatey"
choco feature enable -n allowGlobalConfirmation
choco feature enable -n=autoUninstaller -y
Write-Host "<< Chocolatey"

Write-Host ">> Windows config"
Set-WindowsExplorerOptions -EnableShowHiddenFilesFoldersDrives -EnableShowFileExtensions
Set-TaskbarOptions -Size Small -Lock -Dock Top
Set-CornerNavigationOptions -DisableUpperRightCornerShowCharms -DisableUpperLeftCornerSwitchApps -DisableUsePowerShellOnWinX
Set-StartScreenOptions -EnableBootToDesktop -EnableDesktopBackgroundOnStart -EnableShowStartOnActiveScreen
Disable-UAC
Disable-InternetExplorerESC
Update-ExecutionPolicy
Install-WindowsUpdate -AcceptEula
Update-ExecutionPolicy Unrestricted
Write-Host "<< Windows config"

Write-Host ">> Install software"
Write-Host ">>>> General"
cinst google-chrome-x64 -y
Install-ChocolateyPinnedTaskBarItem "${env:ProgramFiles(x86)}\Google\Chrome\Application\chrome.exe"
cinst paint.net -y
cinst 7zip.install -y
cinst clink -y
cinst ConEmu -y
Install-ChocolateyPinnedTaskBarItem "${env:ProgramFiles}\ConEmu\ConEmu64.exe"
cinst sysinternals -y
cinst Wget -y
cinst boxstarter -y
Write-Host "<<<< General"

Write-Host ">>>> Development"
cinst fiddler -y
cinst p4merge -y
cinst git.install -y -params '"/GitAndUnixToolsOnPath"'
cinst Git-Credential-Manager-for-Windows -y
cinst nodejs.install -y
cinst notepadplusplus.install -y
Install-ChocolateyPinnedTaskBarItem "${env:ProgramFiles}\Notepad++\notepad++.exe"
cinst atom -y
cinst SublimeText3 -y
Install-ChocolateyPinnedTaskBarItem "${env:ProgramFiles}\Sublime Text 3\sublime_text.exe"
Install-ChocolateyFileAssociation ".txt" "$env:programFiles\Sublime Text 3\sublime_text.exe"
Install-ChocolateyFileAssociation ".log" "$env:programFiles\Sublime Text 3\sublime_text.exe"
cinst visualstudiocode -y
cinst visualstudio2015community -y -packageParameters "--Features SQL"
Install-ChocolateyPinnedTaskBarItem "${env:ProgramFiles(x86)}\Microsoft Visual Studio 14.0\Common7\IDE\devenv.exe"
cinst linqpad --version 5.02.03 -y
Install-ChocolateyPinnedTaskBarItem "${env:ProgramFiles(x86)}\LINQPad5\LINQPad.exe"
Write-Host "<<<< Development"
Write-Host "<< Install software"

Write-Host ">> Folders"
if (!(Test-Path "C:\proj")) {
    cd /
    mkdir proj
}
Write-Host "<< Folders"

Write-Host ">> Git config"
git config --global user.name $name
git config --global user.email $email
git config --global push.default simple
git config --global merge.tool p4merge
git config --global diff.guitool p4merge
git config --global difftool.p4merge.path "C:/Program Files/Perforce/p4merge.exe"
git config --global difftool.p4merge.cmd '\"C:/Program Files/Perforce/p4merge.exe\" \"$REMOTE\" \"$MERGED\"'
git config --global mergetool.p4merge.path "C:/Program Files/Perforce/p4merge.exe"
git config --global mergetool.p4merge.cmd '\"C:/Program Files/Perforce/p4merge.exe\" \"$BASE\" \"$LOCAL\" \"$REMOTE\" \"$MERGED\"'
Write-Host "<< Git config"

Write-Host ">> Clone config repo"
cd /proj
git clone https://github.com/ametzger/config.git
Write-Host "<< Clone config repo"

Write-Host ">> Configure PowerShell"
(new-object Net.WebClient).DownloadString("http://psget.net/GetPsGet.ps1") | iex
Install-Module PSReadLine
cmd /c mklink "${profile}\Documents\WindowsPowerShell\profile.ps1" "C:\proj\config\winders\profile.ps1"
Write-Host "<< Configure Powershell"