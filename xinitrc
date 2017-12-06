#!/bin/bash

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
	for f in /etc/X11/xinit/xinitrc.d/?* ; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

# Initialize Cursor
xsetroot -cursor_name left_ptr

# update x resources db
xrdb -merge ~/.Xresources

# fonts
xset +fp /usr/share/fonts/local

# xmodmap
[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap
numlockx on

# compositer

# set background and xompositor
feh --bg-center ~/Dotfiles/images/wallpaper.jpg &
exec xmonad
