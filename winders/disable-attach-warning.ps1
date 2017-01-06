$vsversion = "14.0" # VS 2015 (optionally 12, 11, 10, 9, etc...)
$disable = 1 # set to 0 to enable the warning message

# not using Get-Process here because powershell instance can be 64 bit and devenv is 32 bit
if (!(get-wmiobject win32_process -filter "name='devenv.exe'")) {
    # Create or (force) update the property
    New-ItemProperty -Path "HKCU:\Software\Microsoft\VisualStudio\$vsversion\Debugger" -Name DisableAttachSecurityWarning -Value $disable -PropertyType 'DWORD' -Force
    Write-Host Done!
}
else {
    Write-Error "Please close Visual Studio first!"
}