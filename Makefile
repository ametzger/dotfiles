COLOR_GREEN=$(shell echo "\033[0;32m")
COLOR_NONE=$(shell echo "\033[0m")

.PHONY: help install zsh vim fish tmux pyenv

help:
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

.DEFAULT_GOAL := help

STOW_PACKAGES := alacritty bash emacs-doom emacs-spacemacs fish git misc python ruby ssh tmux vim zsh

install: ## Setup dotfiles
ifeq (, $(shell which stow))
$(error "No stow in $(PATH), may need to brew/apt/yum install")
endif
	@echo '$(COLOR_GREEN)==> Linking dotfiles...$(COLOR_NONE)'
	@stow --target=$(HOME) $(STOW_PACKAGES)
	@echo '$(COLOR_GREEN)==> Done!$(COLOR_NONE)'

uninstall: ## Remove dotfiles
	@echo '$(COLOR_GREEN)==> Removing dotfiles...$(COLOR_NONE)'
	@stow --target=$(HOME) -D $(STOW_PACKAGES)
	@echo '$(COLOR_GREEN)==> Done!$(COLOR_NONE)'

zsh: ## Install ZSH dependencies
	@echo '$(COLOR_GREEN)==> Installing ZSH packages...$(COLOR_NONE)'
	@antibody bundle < ~/.zsh.d/plugins.txt > ~/.zsh.d/plugins.zsh
	@echo '$(COLOR_GREEN)==> Done!$(COLOR_NONE)'

vim: ## Install Vim dependencies
	@echo '$(COLOR_GREEN)==> Installing Vim Plug...$(COLOR_NONE)'
	@curl -sfLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	@mkdir -p ~/.local/share/nvim/site/autoload
	@cp ~/.vim/autoload/plug.vim ~/.local/share/nvim/site/autoload/plug.vim
	@echo '$(COLOR_GREEN)==> Done!$(COLOR_NONE)'

fish: ## Install Fish dependencies
	@echo '$(COLOR_GREEN)==> Installing Fish dependencies...$(COLOR_NONE)'
	@curl -sL https://get.oh-my.fish | fish
	@echo '$(COLOR_GREEN)==> Done!$(COLOR_NONE)'

tmux: ## Install tmux dependencies
	@echo '$(COLOR_GREEN)==> Installing tmux dependencies...$(COLOR_NONE)'
	@git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
	@echo '$(COLOR_GREEN)==> Done!$(COLOR_NONE)'

pyenv: ## Install pyenv
	@echo '$(COLOR_GREEN)==> Installing pyenv...$(COLOR_NONE)'
	@git clone https://github.com/pyenv/pyenv.git ~/.pyenv
	@echo '$(COLOR_GREEN)==> Done!$(COLOR_NONE)'
