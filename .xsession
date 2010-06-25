#!/bin/bash

#
# ~/.xsession
#
# Executed by xdm/gdm/kdm at login
#

#/bin/bash --login -i ~/.xinitrc

# Load resources
#xrdb -merge .Xresources

# MPD settings
export MPD_PORT=6601
 
# Set up an icon tray
# trayer --edge top --align right --SetDockType true --SetPartialStrut true \
# --expand true --width 10 --transparent true --tint 0x000000 --height 12 &
stalonetray &
 
# Set the background color
#xsetroot -solid midnightblue

# Set the wallpaper
#feh --bg-center "/usr/share/archlinux/wallpaper/creation.jpg"
eval "$(cat ~/.fehbg)"
# /home/m00nblade/.scripts/wallpaper.sh &

#feh --bg-scale "$(find $HOME/.wallpaper -type f \
#    -name '*.jpg' -o -name '*.png' | shuf -n 1)"

# Enable eyecandy
xcompmgr -c -t-5 -l-5 -r4.2 -o.55 &

# Start the Thunar daemon
thunar --daemon & 

# Set the keyboard layout to hr
setxkbmap hr 

# Swap Esc and Caps Lock
xmodmap /home/m00nblade/.speedswapper 

# Start urxvtd
urxvtd -q -f -o

# Mount the truecrypt volume if it's not mounted
[[ "$(truecrypt -t -l)" =~ "encrypted" ]] || truecrypt /data/private2 /data/encrypted &

# Start the battery monitor
batterymon &

# Start conky
conky -c $HOME/.conky_$(cat $HOME/.config/.conky_current) | dzen2 -e '' \
    -x '300' -h '16' -w '885' -ta r -fg '#ccccdd' \
    -bg '#000000' -fn 'Monaco-8' > /dev/null 2>&1 &

# Start xmonad
exec ck-launch-session /home/m00nblade/.cabal/bin/xmonad
 
#exec xmonad 

