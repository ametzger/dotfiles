#!/bin/bash

FILES="gitconfig
tmux.conf
vim
vimrc
zsh
zshrc"

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

echo "Linking ~/.config/fish/config.fish => $cwd/config.fish"
ln -s $cwd/config.fish ~/.config/fish/config.fish
