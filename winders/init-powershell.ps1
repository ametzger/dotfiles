#Requires -RunAsAdministrator
Set-ExecutionPolicy Unrestricted

Write-Host "Installing modules, will prompt for input"

Install-Module -Name PSReadLine
Install-Module -Name posh-git
Install-Module -Name oh-my-posh

$profilePath = "$env:USERPROFILE\Documents\WindowsPowerShell"
$profileFilePath = "$profilePath\Microsoft.PowerShell_profile.ps1"
$linkPath = "$pwd\profile.ps1"

if (!(Test-Path -Path $profilePath))
{
    New-Item -ItemType Directory -Path $profilePath
}

Write-Host "Creating symlink from" $linkPath "to" $profileFilePath

New-Item -Path $profileFilePath -ItemType SymbolicLink -Value $linkPath

