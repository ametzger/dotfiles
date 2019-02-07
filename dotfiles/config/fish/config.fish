# suppress fish prompt greeting
set -g -x fish_greeting ''

# add homebrew sbin to PATH
set -g fish_user_paths "/usr/local/sbin" $fish_user_paths

# add ~/bin to PATH if it exists
if test -d $HOME/bin
    #echo "Adding ~/bin to PATH"
    set -g fish_user_paths "$HOME/bin" $fish_user_paths
end

# add ~/.local/bin to PATH if it exists
if test -d $HOME/.local/bin
    #echo "Adding ~/bin to PATH"
    set -g fish_user_paths "$HOME/.local/bin" $fish_user_paths
end

set -Ux EDITOR nvim
set -gx default_user "asm"

alias e "emacsclient -nac"
alias mvim "emacsclient -nac"
alias subl "emacsclient -nac"
alias mate "emacsclient -nac"
