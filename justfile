help:
	just --list

# Install baseline Homebrew packages
brew-packages:
	brew install amazon-ecs-cli
	brew install antibody
	brew install awscli
	brew install bat
	brew install cmake
	brew install coreutils
	brew install curl
	brew install direnv
	brew install elixir
	brew install exa
	brew install fd
	brew install fish
	brew install fzf
	brew install git
	brew install gnu-sed
	brew install gnupg
	brew install go
	brew install httpie
	brew install hyperfine
	brew install jq
	brew install kakoune
	brew install lapack
	brew install libvterm
	brew install libxmlsec1
	brew install mas
	brew install mitmproxy
	brew install mtr
	brew install neovim
	brew install nmap
	brew install node
	brew install openblas
	brew install openssl@3
	brew install pgbouncer
	brew install pipx
	brew install postgresql
	brew install rabbitmq
	brew install rbenv-bundler
	brew install rbenv-vars
	brew install reattach-to-user-namespace
	brew install redis
	brew install ripgrep
	brew install ruby-install
	brew install semgrep
	brew install starship
	brew install tmuxinator
	brew install tokei
	brew install tree
	brew install wget
	brew install wrk
	brew install z
	brew install zlib
	brew install zoxide
	brew install zsh
	brew install zsh-completions

	brew tap d12frosted/emacs-plus
	brew install d12frosted/emacs-plus/emacs-plus@28 --with-modern-alecive-flatwoken-icon
	if test ! -d '/Applications/Emacs.app'; then ln -s "$(brew --cellar emacs-plus)/Emacs.app" /Applications; fi

	brew tap homebrew/cask-fonts
	brew install --cask font-cascadia-code
	brew install --cask font-go
	brew install --cask font-go-mono-nerd-font
	brew install --cask font-ibm-plex
	brew install --cask font-input
	brew install --cask font-jetbrains-mono

	# Some of these packages are already installed on some machines
	# (e.g. by MDM), in that case don't confuse brew by trying to install
	# them.
	if test ! -d '/Applications/1Password 7.app'; then brew install --cask 1password; fi
	brew install --cask 1password-cli
	brew install --cask alacritty
	brew install --cask alfred
	brew install --cask appcleaner
	brew install --cask datagrip
	brew install --cask docker
	brew install --cask fantastical
	brew install --cask firefox
	if test ! -d '/Applications/Google Chrome.app'; then brew install --cask google-chrome; fi
	brew install --cask iterm2
	brew install --cask pycharm
	brew install --cask rectangle
	if test ! -d '/Applications/Slack.app'; then brew install --cask slack; fi
	brew install --cask spotify
	brew install --cask vimr
	if test ! -d '/Applications/zoom.us.app'; then brew install --cask zoom; fi

brew-packages-extra:
	brew install clojure
	brew install cowsay

	brew tap qmk/qmk
	brew install qmk/qmk/qmk

	brew install --cask discord
	brew install --cask dropbox
	brew install --cask ngrok
	brew install --cask transmission
	brew install --cask vagrant

mas:
    mas install 1289583905  # Pixelmator Pro
    mas install 1475387142  # Tailscale
    mas install 1055511498  # Day One
    mas install 904280696   # Things
    mas install 1435957248  # Drafts

brew-services:
	brew services start postgresql
	brew services start rabbitmq
	brew services start redis

rustup:
	curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

rust:
	rustup install nightly
	rustup component add rustfmt
	rustup component add rls
	rustup component add rust-analysis
	rustup component add rust-src

crates:
	cargo install cargo-check
	cargo install cargo-edit
	cargo install cargo-outdated
	cargo install cargo-watch
	cargo install tokei

zsh:
# TODO(asm,2022-05-23): apparently antibody has been abandoned, need
# to replace with something newer
	antibody bundle < ~/.zsh.d/plugins.txt > ~/.zsh.d/plugins.zsh

fish:
	curl -sL https://get.oh-my.fish | fish
