COLOR_GREEN=$(shell echo "\033[0;32m")
COLOR_NONE=$(shell echo "\033[0m")

.PHONY: help

help:
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

.DEFAULT_GOAL := help

install: ## Setup dotfiles
ifeq (, $(shell which stow))
	@echo '$(COLOR_GREEN)==> Installing Stow...$(COLOR_NONE)'
	brew install stow
endif
	@echo '$(COLOR_GREEN)==> Linking dotfiles...$(COLOR_NONE)'

	stow --target=$(HOME) alacritty bash emacs-doom emacs-spacemacs fish git misc python ruby ssh tmux vim zsh

	@echo '$(COLOR_GREEN)==> Done!$(COLOR_NONE)'
