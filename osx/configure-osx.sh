#!/usr/bin/env bash

# assumes XCode command line stuff already installed

echo "setup key repeat"
defaults write NSGlobalDomain KeyRepeat -int 0.1
defaults write NSGlobalDomain InitialKeyRepeat -int 15

echo "setup system params"
defaults write NSGlobalDomain WebKitDeveloperExtras -bool true
chflags nohidden ~/Library
mkdir ~/Desktop/Screenshots
defaults write com.apple.screencapture location ~/Desktop/Screenshots

echo "install brew"
ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"
