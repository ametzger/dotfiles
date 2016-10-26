

# Chocolatey profile
$ChocolateyProfile = "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1"
if (Test-Path($ChocolateyProfile)) {
  Import-Module "$ChocolateyProfile"
}

if ($host.Name -eq 'ConsoleHost')
{
    Import-Module PSReadline
	Set-PSReadlineOption -EditMode Emacs
	
	Set-PSReadLineOption -HistorySearchCursorMovesToEnd 
	Set-PSReadlineKeyHandler -Key UpArrow -Function HistorySearchBackward
	Set-PSReadlineKeyHandler -Key DownArrow -Function HistorySearchForward
	
	Set-PSReadlineKeyHandler -Key Shift+Ctrl+C -Function Copy
	Set-PSReadlineKeyHandler -Key Ctrl+V -Function Paste
	
	Set-Theme Agnoster
}
