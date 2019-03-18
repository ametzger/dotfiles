# -*- mode: fish-mode -*-
# vi: set ft=sh

# Fish #########################################################################
# suppress fish prompt greeting
set -g -x fish_greeting ''


# Paths ########################################################################
# add ~/bin to PATH if it exists
if test -d $HOME/bin
  set -g fish_user_paths "$HOME/bin" $fish_user_paths
end

# add ~/.local/bin to PATH if it exists
if test -d $HOME/.local/bin
  set -g fish_user_paths "$HOME/.local/bin" $fish_user_paths
end

# VMWare Fusion utils (e.g. vmrun)
if test -d "/Applications/VMWare Fusion.app/Contents/Library"
  set -g fish_user_paths "/Applications/VMWare Fusion.app/Contents/Library"
end

# Baseline #####################################################################
set -gx LSCOLORS 'exfxcxdxbxegedabagacad'
set -gx CLICOLOR true

set -gx EDITOR nvim
set -gx VISUAL nvim

set -gx SSH_KEY_PATH $HOME/.ssh/rsa_id

# Used by eden prompt to suppress name
set -gx default_user asm


# Vendor #######################################################################
if test -d $HOME/proj/go
  set -gx GOPATH $HOME/proj/go
  set -g fish_user_paths "$GOPATH/bin"
end

set -gx DOTNET_CLI_TELEMETRY_OPTOUT true


# Aliases ######################################################################
alias ll "exa -lah"

# Better editors
alias vim "nvim"

# Python + django
alias p "pipenv run"
alias m "pipenv run python manage.py"
alias sp "pipenv run python manage.py shell_plus"

alias tf "terraform"


# Private ######################################################################
if test -e $HOME/.profile-private.sh
  source $HOME/.profile-private.sh
end

if test -e $HOME/.jellyfish/environment
  source $HOME/.jellyfish/environment
end
