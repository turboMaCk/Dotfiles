#!/bin/bash

##################
# Autostart list #
##################
# This script should be fired when xmonad is starging

compton &
numlockx on &
feh --bg-fill ~/.xmonad/wallpaper.jpg &
stalonetray &
dropbox &
caprine &
volumeicon &
redshift-gtk &
rescuetime &
browserpass &
xmodmap ~/.xmonad/Xmodmap
