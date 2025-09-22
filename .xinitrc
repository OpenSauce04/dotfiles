xrandr --output DP-1 --mode 1920x1080 --rate 120
xrandr --output HDMI-1 --mode 1920x1080 --rate 120

/usr/libexec/xfce-polkit &

exec dbus-run-session icewm-session
