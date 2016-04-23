# Options
setopt autocd autopushd pushdtohome pushdignoredups
setopt noclobber nobeep nohup
setopt listtypes extendedglob completeinword alwaystoend markdirs
setopt hashcmds menucomplete allexport
setopt extendedhistory histignoredups prompt_subst
unsetopt bgnice sharehistory correct correctall

# Variables
# Various necessities
SHELL=/bin/zsh
HISTFILE=$HOME/.zhistory
HISTSIZE=1000
SAVEHIST=1000
HOSTNAME="`hostname`"
PAGER='less'
EDITOR='/usr/bin/vim'
TZ="America/New_York"
TERM="rxvt"

# use emacs line editing
bindkey -e

umask 022 # Create files rw-r--r-- (644)

# Path
# /usr/local/bin is first so Homebrew-installed packages get preference
PATH=/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin
# If ~/bin exists, add it to $PATH
if [ -d $HOME/bin ]; then
	PATH=$PATH:$HOME/bin
fi
if [ -d /usr/local/share/npm/bin ]; then
  PATH=$PATH:/usr/local/share/npm/bin
fi
if [ -f $HOME/.dnx/dnvm/dnvm.sh ]; then
  source $HOME/.dnx/dnvm/dnvm.sh
fi
if [ -d /usr/local/share/dotnet/bin ]; then
  PATH=$PATH:/usr/local/share/dotnet/bin
fi

# Titling
# Set title to: "<user>@<hostname> | <process>:<dir>"
function title() {
  a=${(V)1//\%/\%\%}
  a=$(print -Pn "%40>...>$a" | tr -d "\n")
  print -Pn "\e]2;$2 | $a:$3\a"
}
function precmd() {
    title "zsh" "$USER@%m" "%55<...<%~"
    if [[ `uname` == "Darwin" ]]; then
        print -Pn "\033]1;%~\007"
    fi
}
function preexec() {
  title "$1" "$USER@%m" "%35<...<%~"
}

# fpath
if [ -d /usr/local/share/zsh/site-functions ]; then
  export FPATH=/usr/local/share/zsh/site-functions:$FPATH
fi

# Prompt
# Git stuff
source ~/.zsh/git-prompt/zshrc.sh
# Initialize colors
autoload colors zsh/terminfo
if [[ "$terminfo[colors]" -ge 8 ]]; then
   colors
fi
# Alias colors to $PR_<color> for prompt
for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
   eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
   eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
   (( count = $count + 1 ))
done
PR_NO_COLOR="%{$terminfo[sgr0]%}"
# Set the OS X term if on OS X, Linux if not
# Also change colors if we are root
if [[ `uname` == "Darwin" ]]; then
	export CLICOLOR=1
	export LSCOLORS=ExGxFxdxCxDxDxhbadacec
	PROMPT='$PR_BLUE%2c $PR_NO_COLOR$(git_super_status)%# '
	# user@host /some/directory%
	#PROMPT="%{%(#~$PR_RED~$PR_BLUE)%}%n$PR_WHITE@%{%(#~$PR_RED~$PR_BLUE)%}%m %{%(#~$PR_BLUE~$PR_GREEN)%}%2c$PR_NO_COLOR$(git_super_status) %(!.#.%%) "
elif [[ `hostname` == "komondor" ]]; then
	# [user@host:/some/directory]%
	PS1="[%{%(#~$PR_RED~$PR_GREEN)%}%n$PR_WHITE@%{%(#~$PR_RED~$PR_GREEN)%}%m$PR_NO_COLOR:%{%(#~$PR_GREEN~$PR_BLUE)%}%2c$PR_NO_COLOR]%(!.#.%%) "
else
	# [user@host:/some/directory]%
	PS1="[$PR_MAGENTA%n$PR_WHITE@$PR_MAGENTA%m$PR_NO_COLOR:$PR_BLUE%2c$PR_NO_COLOR]%(!.#.%%) "
fi


# Aliases
alias grep="grep --color=auto"   # Color grep
# OS X specific aliases
if [[ `uname` == "Darwin" ]] then
	alias ls="ls -FG"            # BSD ls color, human readable
  alias bu="brew update&&brew upgrade"
fi
# Linux specific stuff
if [[ `uname` == "Linux" ]] then
	alias ls="ls --color -h"                         # color, human units
	alias au="sudo apt-get update && \
		sudo apt-get dselect-upgrade"			             # upgrade packages
fi

# Completion
autoload -U compinit
compinit
autoload -U incremental-complete-word
zle -N incremental-complete-word
autoload -U insert-files
zle -N insert-files
autoload -U predict-on
zle -N predict-on
# SSH completion stuff
local knownhosts
knownhosts=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[0-9]*}%%\ *}%%,*})
# Modules
zmodload -a zsh/stat stat
zmodload -a zsh/zpty zpty
zmodload -a zsh/zprof zprof
zmodload -ap zsh/mapfile mapfile &>/dev/null
# Completion options
bindkey ' ' magic-space
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' tag-order '! users' -
zstyle ':completion:*:default' list-colors '${LS_COLORS}'
zstyle ':completion:*' completer _complete _list _oldlist _expand _ignored _match _correct _approximate
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' add-space true
zstyle ':completion:*:processes' command 'ps axjf'
zstyle ':completion:*:processes-names' command 'ps axho command'
zstyle ':completion:*:(ssh|scp|sftp):*' hosts $knownhosts
zstyle ':completion:*:cd:*' ignore-parents parent pwd
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $((($#PREFIX+$#SUFFIX)/3 )) numeric )'
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' menu select=long-list select=0
zstyle ':completion:*' substitute 1
zstyle ':completion:*' use-compctl true
zstyle ':completion:*' verbose true
zstyle ':completion:*' word true

# Keybindings
bindkey "^[[A" history-search-backward
bindkey "^[[B" history-search-forward
