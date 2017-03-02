# enable global confirm in choco
choco feature enable -n=allowGlobalConfirmation


choco install -y 7zip.install
choco install -y ag
choco install -y awscli
choco install -y AWSTools.Powershell
choco install -y clink
choco install -y ConEmu
choco install -y curl
choco install -y dropbox
choco install -y Emacs
choco install -y fiddler
choco install -y fiddler4
choco install -y Firefox
choco install -y git.install -params '"/GitAndUnixToolsOnPath"'
choco install -y GoogleChrome
choco install -y hackfont
choco install -y html-tidy
#choco install -y nmap
choco install -y nodejs.install
choco install -y notepadplusplus.install
choco install -y P4Merge
choco install -y pidgin
#choco install -y pip
choco install -y kitty
choco install -y putty
choco install -y putty.portable
choco install -y rdcman
choco install -y redis-64
choco install -y ruby
choco install -y screenhero
choco install -y sharpkeys
choco install -y slack
choco install -y sysinternals
choco install -y terminals
#choco install -y UrlRewrite # installed later on by install-development-iis.ps1 in backend-scripts, fails if IIS not installed
choco install -y vlc
