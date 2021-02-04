#!/bin/bash

killall -q polybar

while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Script should be given window manager as argument
if [ "$1" == "i3" ]; then
    polybar i3 &
elif [ "$1" == "bspwm" ]; then
    polybar bspwm &
else
    # Run default bar
    polybar main &
fi

echo "Polybar launched..."
