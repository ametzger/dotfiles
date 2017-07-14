# enable global confirm in choco
choco feature enable -n=allowGlobalConfirmation

$packages = @(
    "7zip.install"
    "ag"
    "awscli"
    "AWSTools.Powershell"
    "clink"
    "ConEmu"
    "curl"
    "dropbox"
    "Emacs"
    "fiddler"
    "fiddler4"
    "Firefox"
    "GoogleChrome"
    "hackfont"
    "html-tidy"
    "jq"
    "nodejs.install"
    "notepadplusplus.install"
    "P4Merge"
    "pidgin"
    "python3"
    "kitty"
    "putty"
    "putty.portable"
    "rdcman"
    "redis-64"
    "ruby"
    "screenhero"
    "slack"
    "sysinternals"
    "windirstat"
)

ForEach ($packageName In $packages)
{
    Write-Host "Installing $packageName..."
    choco install -y $packageName
    Write-Host "Finished installing $packageName"
}

# Install git.install with the unix stuff (ls, etc) on %PATH%
choco install -y git.install -params '"/GitAndUnixToolsOnPath"'
