# -*- shell-script -*-
## Read for interactive shells

# Handle dumb terms
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

## profiling: uncomment to enable profiling
# zmodload zsh/datetime
# setopt PROMPT_SUBST
# PS4='+$EPOCHREALTIME %N:%i> '

# logfile=$(mktemp zsh_profile.XXXXXXXX)
# echo "Logging to $logfile"
# exec 3>&2 2>$logfile

# setopt XTRACE

## various interactive-only config
export LSCOLORS='exfxcxdxbxegedabagacad'
export CLICOLOR=true

# force readline keybinds
bindkey -e

autoload -U compinit && compinit

## prompt
# VCS integration setup
autoload -Uz vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
zstyle ':vcs_info:git:*' formats '%b'

# Enable colors in prompt
autoload -U colors && colors
zmodload zsh/datetime

export VIRTUAL_ENV_DISABLE_PROMPT=1
# Based on https://github.com/sindresorhus/pure but with less
# functionality
# TODO: Pure has a nice timer function that fires if a command runs
# for more than 5 seconds, adding the timestamp seems to be a workable
# solution but might be nice to add that back in
export PS1='
%{$fg[blue]%}%~%{$reset_color%} %F{242%}%{$vcs_info_msg_0_%}%f%u
%{%(?.$fg[magenta].$fg[red])%}❯%{$reset_color%} '
RPROMPT='%F{242}%*%f'

## aliases
if command -v exa >/dev/null 2>&1; then
  alias ls='exa'
  alias ll='exa -lah'
fi

if command -v zoxide >/dev/null 2>&1; then
  eval "$(zoxide init zsh)"
fi

if command -v bat >/dev/null 2>&1; then
  export BAT_THEME="ansi-dark"
  alias cat='bat'
fi

alias em='emacsclient -nc'
alias e='emacsclient -nw'
alias vim='nvim'
alias p='pipenv run'
alias m='p python manage.py'
alias sp='m shell_plus'
alias tf='terraform'

## tools
# if command -v rbenv >/dev/null 2>&1; then
#   eval "$(rbenv init -)"
# fi

if command -v pyenv >/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

if command -v direnv >/dev/null 2>&1; then
  export DIRENV_LOG_FORMAT=''
  eval "$(direnv hook zsh)"
fi

# if [[ -d $NVM_DIR ]]; then
#   [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
# fi

[ -f $HOME/.fzf.zsh ] && source ~/.fzf.zsh

## extra env vars
[ -f $HOME/.localrc ] && source ~/.localrc

## antigen
# To manage plugins, update `zsh.d/plugins.txt` then run `bin/antigen`
# to regenerate this file.
source ~/.zsh.d/plugins.zsh

# More profiling, see top of file
# unsetopt XTRACE
# exec 2>&3 3>&-
