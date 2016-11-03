$codeDirectory = "C:\proj"

pushd $codeDirectory

$preludeDirectory = $codeDirectory+"\prelude"
If ((Test-Path -Path $preludeDirectory) -eq 0)
{
	Write-Host Cloning prelude repository...
	git clone https://github.com/bbatsov/prelude.git
	
	pushd $preludeDirectory

	Write-Host Creating prelude symlinks...
	Remove-Item -Recurse personal
	$configDir = $codeDirectory+"\config\emacs"
	New-Item -Path personal -ItemType SymbolicLink -Value $configDir+"\personal"
	New-Item -Path prelude-modules.el -ItemType SymbolicLink -Value $configDir+"\prelude-modules.el"
}

Write-Host Creating .emacs.d symlinks...
pushd $env:HOME

$emacsPath = $env:HOME+"\.emacs.d"
If ((Test-Path -Path $emacsPath) -eq 1)
{
	Write-Host Renaming old .emacs.d to .emacs.d.bak
	Rename-Item $emacsPath ".emacs.d.bak"
}
New-Item -Path .emacs.d -ItemType SymbolicLink -Value $preludeDirectory
