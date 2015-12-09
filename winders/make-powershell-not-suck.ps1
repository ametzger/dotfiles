# assumes "config" repo has been cloned to c:\proj\config
Set-ExecutionPolicy Unrestricted

# install psget
(new-object Net.WebClient).DownloadString("http://psget.net/GetPsGet.ps1") | iex

# install psreadline
Install-Module PSReadLine

# link profile with sane PSReadLine config
cmd /c mklink c:\Users\asm\Documents\WindowsPowerShell\profile.ps1 c:\proj\config\winders\profile.ps1