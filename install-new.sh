#!/bin/bash

set -ex

sudo apt install -y emacs-nox git fish tmux ccrypt
sudo chsh -s $(which fish) ragnar
sudo mkdir -p /mnt/cdrom
sudo umount -q /dev/cdrom || true
sudo mount /dev/cdrom /mnt/cdrom
mkdir -p ~/.ssh
cp /mnt/cdrom/id_ed25519 ~/.ssh/
sudo umount /mnt/cdrom
ccrypt -d ~/.ssh/id_ed25519
ssh-keygen -f ~/.ssh/id_ed25519 -y > ~/.ssh/id_ed25519.pub
mkdir -p ~/code
cd ~/code
git clone git@github.com:ragzter/confs.git
cd ~/code/confs
mkdir -p ~/.config
mkdir -p ~/.config/fish
mkdir -p ~/.config/fish/functions
ln -fs `pwd`/.emacs-new ~/.emacs
ln -fs `pwd`/.tmux.conf ~/.tmux.conf
ln -fs `pwd`/fish/config.fish ~/.config/fish
ln -fs `pwd`/fish/functions/fish_prompt.fish ~/.config/fish/functions/fish_prompt.fish
