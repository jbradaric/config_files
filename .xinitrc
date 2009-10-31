#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice
#exec openbox-session

# start urxvt
urxvtd -q -f -o

## XMonad settings
# Set cursor
xsetroot -cursor_name left_ptr
# Start XMonad
exec ck-launch-session xmonad
