#!/bin/bash
# Dotfile Symlinks
ln -s ~/repos/dotfiles/.bashrc ~/.bashrc -f

rm -r ~/.doom.d/
ln -s ~/repos/dotfiles/.doom.d/ ~/.doom.d

# TODO Needs to be updated to check for running under WSL, and if so grab the correct Windows User Profile Path for nextcloud / zettels link.
# See .bashrc for example of WSL1 & 2 checks
# ln -s /mnt/c/Users/BenSchmidt/Nextcloud/Documents/zettels/ ~/zettels
ln -s ~/Nextcloud/Documents/org/ ~/org
