
setxkbmap dvorak
xset r rate 200 30
xmodmap ~/.Xmodmap
xrdb -merge ~/.Xresources

exec xmonad
