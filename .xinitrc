#!/bin/sh

# start some nice programs
if [ -d /etc/X11/xinit/xinitrc.d ]; then
	for f in /etc/X11/xinit/xinitrc.d/?*.sh; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

setxkbmap -option compose:ralt
amixer sset 'Master' 5%
# Set screen saver timeout.
xset s 3600

startup
# Start status bar.
xstatus &

exec bspwm 
