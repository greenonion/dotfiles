#!/usr/bin/env bash
setxkbmap -option grp:switch,grp:alt_shift_toggle,grp_led:scroll,caps:escape us,gr &
/usr/bin/xscreensaver -no-splash &
# remove annoying beeps
xset b off
# start kbdd to keep per-window keyboard layouts
/usr/bin/kbdd &
exec i3
