#!/usr/bin/env bash

brew install --with-cocoa emacs
pushd ~/proj
git clone https://github.com/ametzger/prelude.git
ln -s ~/proj/prelude ~/.emacs.d

