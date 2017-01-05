#!/bin/bash

FILES="tmux.conf
vim
vimrc
zsh
zshrc
zlogout"

cwd=$(pwd)

for f in $FILES
do
  if [[ -f ~/.$f ]] || [[ -d ~/.$f ]];
  then
    echo "Moving ~/.$f to ~/.$f.bak"
    mv ~/.$f ~/.$f.bak
  fi

  echo "Linking ~/.$f => $cwd/$f"
  ln -s $cwd/$f ~/.$f
done

if [[ `uname` == "Linux" ]]; then
    echo "Linking ~/.gitconfig => $cwd/gitconfig-linux"
    ln -s $cwd/gitconfig-linux ~/.gitconfig
else
    echo "Linking ~/.gitconfig => $cwd/gitconfig"
    ln -s $cwd/gitconfig ~/.gitconfig
fi

if [[ ! -d ~/.config/fish ]]; then
   mkdir -p ~/.config/fish
fi

echo "Linking ~/.config/fish/config.fish => $cwd/config.fish"
ln -s $cwd/config.fish ~/.config/fish/config.fish

if [[ ! -d ~/.ssh ]]; then
    mkdir -p ~/.ssh
fi

echo "Linking ~/.ssh/config => $cwd/ssh/config"
ln -s $cwd/ssh/config ~/.ssh/config
