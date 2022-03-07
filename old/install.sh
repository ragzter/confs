#!/bin/bash

# Warning: This script has not been tested yet
# Todo: Install `eslint_d`

set -ex

# Install things I want

sudo apt install fonts-fantasque-sans xfonts-terminus dmenu xmonad fish xinit git sudo
## Emacs

### Get packages needed for compiling

sudo apt install autoconf automake libtool texinfo build-essential xorg-dev libgtk2.0-dev libjpeg-dev libncurses5-dev libdbus-1-dev libgif-dev libtiff-dev libm17n-dev libpng-dev librsvg2-dev libotf-dev libgnutls28-dev libxml2-dev

### Fetch source

cd ~/code

git clone --depth 1 --branch emacs-26.3 git://git.sv.gnu.org/emacs.git

### Compile, build and install

cd emacs
./autogen.sh
./configure
make bootstrap
sudo make install

## Google Chrome

mkdir -p ~/Downloads
cd ~/Downloads

wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
sudo apt install ./google-chrome-stable_current_amd64.deb

# Put configuration files in home directory

cd ~/code/confs

ln -s `pwd`/.emacs ~/.emacs
ln -s `pwd`/.xinitrc ~/.xinitrc
ln -s `pwd`/.Xmodmap ~/.Xmodmap
mkdir -p ~/.xmonad/
ln -s `pwd`/xmonad.hs ~/.xmonad/xmonad.hs
ln -s `pwd`/.zshrc ~/.zshrc
ln -s `pwd`/.Xresources ~/.Xresources
mkdir -p ~/.config
mkdir -p ~/.config/fish
mkdir -p ~/.config/fish/functions
ln -s `pwd`/fish/config.fish ~/.config/fish
ln -s `pwd`/fish/fish_variables ~/.config/fish
ln -s `pwd`/fish/functions/fish_prompt.fish ~/.config/fish/functions/fish_prompt.fish
