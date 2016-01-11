#!/usr/bin/env bash

brew install --with-cocoa emacs
pushd ~/proj
git clone https://github.com/bbatsov/prelude.git
ln -s ~/proj/prelude ~/.emacs.d
pushd ~/proj/prelude
rm -rf personal
ln -s ~/proj/config/emacs/personal personal
