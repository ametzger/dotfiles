# -*- makefile -*-

help:
	just --list

brew:
	/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

brew-packages:
	brew install amazon-ecs-cli
	brew install antibody
	brew install awscli
	brew install bat
	brew install clojure
	brew install consul
	brew install coreutils
	brew install cowsay
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
	brew install hugo
	brew install hyperfine
	brew install jq
	brew install kakoune
	brew install kops
	brew install lapack
	brew install libvterm
	brew install libxmlsec1
	brew install macvim
	brew install markdown
	brew install mas
	brew install minikube
	brew install mitmproxy
	brew install mtr
	brew install neovim
	brew install nmap
	brew install node
	brew install openblas
	brew install openssl@3
	brew install packer
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
	brew install transmission-cli
	brew install tree
	brew install vault
	brew install watchman
	brew install wget
	brew install wrk
	brew install yajl
	brew install z
	brew install zlib
	brew install zoxide
	brew install zsh
	brew install zsh-completions

	brew tap d12frosted/emacs-plus
	brew install d12frosted/emacs-plus/emacs-plus@27 --with-modern-papirus-icon

	brew tap qmk/qmk
	brew install qmk/qmk/qmk

	brew tap homebrew/cask-fonts
	brew install --cask font-cascadia-code
	brew install --cask font-go
	brew install --cask font-go-mono-nerd-font
	brew install --cask font-ibm-plex
	brew install --cask font-input
	brew install --cask font-jetbrains-mono

	brew install --cask 1password
	brew install --cask 1password-cli
	brew install --cask alacritty
	brew install --cask alfred
	brew install --cask appcleaner
	brew install --cask datagrip
	brew install --cask discord
	brew install --cask docker
	brew install --cask dropbox
	brew install --cask fantastical
	brew install --cask firefox
	brew install --cask google-chrome
	brew install --cask iterm2
	brew install --cask ngrok
	brew install --cask omnifocus
	brew install --cask pycharm
	brew install --cask session-manager-plugin
	brew install --cask slack
	brew install --cask spotify
	brew install --cask sublime-text
	brew install --cask transmission
	brew install --cask vagrant
	brew install --cask vimr
	brew install --cask zoom

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
	cargo install bat
	cargo install cargo-check
	cargo install cargo-edit
	cargo install cargo-outdated
	cargo install cargo-watch
	cargo install exa
	cargo install fd-find
	cargo install just
	cargo install ripgrep
	cargo install src
	cargo install tokei
	cargo install hyperfine

zsh:
	antibody bundle < ~/.zsh.d/plugins.txt > ~/.zsh.d/plugins.zsh

fish:
	curl -sL https://get.oh-my.fish | fish
