#!/bin/sh

# If on mac, just dont run this script
if [ "$(uname)" = "Darwin" ]; then
    exit 0
fi

# start some nice programs
if [ -d /etc/X11/xinit/xinitrc.d ]; then
	for f in /etc/X11/xinit/xinitrc.d/?*.sh; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

command -v amixer >/dev/null && amixer sset 'Master' 5%

startup

exec bspwm
