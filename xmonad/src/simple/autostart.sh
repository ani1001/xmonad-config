#!/bin/bash

function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}

# Set up an icon tray

trayer --edge top --align right --SetDockType true --SetPartialStrut true \
 --expand true --width 10 --transparent true --tint 0x191970 --height 16 &

run nitrogen --restore &
run picom &
run lxpolkit &
run nm-applet &
run volumeicon &
run mpd &
run urxvtd -q -o -f &
run emacs -daemon &
