# Set %HOME% to %USERPROFILE% to allow Emacs .emacs.d to work

$homeVar = $env:HOME
if (-Not $homeVar)
{
    $homeFolder = $env:USERPROFILE
    Write-Host "%HOME% not defined, setting to $homeFolder"
    [Environment]::SetEnvironmentVariable("HOME", $homeFolder, [EnvironmentVariableTarget]::User)
}

# Set %PATH% to include Perforce dir so p4merge works in git

$userPath = [Environment]::GetEnvironmentVariable("PATH", [EnvironmentVariableTarget]::User)
if (-Not ($userPath -Like '*Perforce*'))
{
    $p4MergePath = "C:\Program Files (x86)\Perforce"
    Write-Host "Adding $p4MergePath to %PATH%"
    $updatedPath = "$path;$p4MergePath"
    Write-Host "Updating %PATH% to $updatedPath"
    [Environment]::SetEnvironmentVariable("Path", $updatedPath, [EnvironmentVariableTarget]::User)
}
