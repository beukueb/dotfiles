#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice
#xrandr --output DVI-0 --auto --rotate normal --pos 0x0 --output DVI-1 --auto --pos 1280x0 #AMD
#xrandr --output DP-2 --auto --rotate normal --pos 0x0 --output DP-1 --auto --pos 1280x0 #NVIDEA
xrandr --output DP-1 --auto --rotate normal --pos 0x0 --output DP-2 --auto --pos 1920x0 #NVIDEA
exec startfluxbox
#exec awesome
