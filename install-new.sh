#!/bin/bash

set -ex

sudo apt install -y emacs-nox git fish tmux ccrypt
sudo chsh -s $(which fish) ragnar

if [ ! -f ~/.ssh/id_ed25519 ] ; then
  sudo mkdir -p /mnt/cdrom
  sudo umount -q /dev/cdrom || true
  sudo mount /dev/cdrom /mnt/cdrom
  mkdir -p ~/.ssh
  cp /mnt/cdrom/id_ed25519 ~/.ssh/
  sudo umount /mnt/cdrom
  ccrypt -d ~/.ssh/id_ed25519
fi

if [ ! -f ~/.ssh/id_ed25519.pub ] ; then
  ssh-keygen -f ~/.ssh/id_ed25519 -y > ~/.ssh/id_ed25519.pub
fi

mkdir -p ~/code
cd ~/code

if [ ! -d ~/code/confs ] ; then
  git clone git@github.com:ragzter/confs.git
fi

cd ~/code/confs
mkdir -p ~/.config
mkdir -p ~/.config/fish
mkdir -p ~/.config/fish/functions
ln -fs `pwd`/.emacs-new ~/.emacs
ln -fs `pwd`/.tmux.conf ~/.tmux.conf
ln -fs `pwd`/fish/config.fish ~/.config/fish
ln -fs `pwd`/fish/functions/fish_prompt.fish ~/.config/fish/functions/fish_prompt.fish
