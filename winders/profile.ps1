# requires PSReadLine (https://github.com/lzybkr/PSReadLine) via PsGet (http://psget.net/)

if ($host.Name -eq 'ConsoleHost')
{
    Import-Module PSReadline
	Set-PSReadlineOption -EditMode Emacs
	
	Set-PSReadLineOption -HistorySearchCursorMovesToEnd 
	Set-PSReadlineKeyHandler -Key UpArrow -Function HistorySearchBackward
	Set-PSReadlineKeyHandler -Key DownArrow -Function HistorySearchForward
	
	Set-PSReadlineKeyHandler -Key Shift+Ctrl+C -Function Copy
	Set-PSReadlineKeyHandler -Key Ctrl+V -Function Paste
}