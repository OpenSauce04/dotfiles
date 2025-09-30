xrandr --output DisplayPort-1 --mode 1920x1080 --rate 120
xrandr --output HDMI-A-0 --mode 1920x1080 --rate 120

/usr/lib/mate-polkit/polkit-mate-authentication-agent-1 &

exec dbus-run-session icewm-session
