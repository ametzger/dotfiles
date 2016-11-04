# suppress fish prompt greeting
set -g -x fish_greeting ''

# add homebrew sbin to PATH
set -g fish_user_paths "/usr/local/sbin" $fish_user_paths

# add ~/bin to PATH if it exists
if test -d ~/bin
    #echo "Adding ~/bin to PATH"
    set -g fish_user_paths "~/bin" $fish_user_paths
end

alias e "emacsclient -nac"
alias mvim "emacsclient -nac"
alias subl "emacsclient -nac"
alias mate "emacsclient -nac"
