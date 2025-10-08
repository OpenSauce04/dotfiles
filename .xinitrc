xrandr --output DP-2 --mode 1920x1080 --rate 120
xrandr --output HDMI-1 --mode 1920x1080 --rate 120

/usr/lib/mate-polkit/polkit-mate-authentication-agent-1 &

exec dbus-run-session icewm-session
