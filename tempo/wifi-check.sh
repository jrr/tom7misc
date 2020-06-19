#!/bin/bash

# If we can't ping the database machine (tries 4 times), assume the network is down,
# and restart both the wlan0 interface, and tempo.
# This is probably not the greatest strategy! If the database machine is down,
# we might still want to be able to connect to the pi, but this will probably
# disconnect us every minute. Also, if there is some momentary packet loss, we
# may disconnect when we don't need to.
#
# Instead check if wlan0 is "up"?

cd /home/pi/tom7misc/tempo
date > /home/pi/last-wifi-check
/bin/ping -q -c4 10.0.0.202 || (/sbin/ip link set wlan0 down ; sleep 1 ; /sbin/ip link set wlan0 up ; sleep 4 ; /home/pi/tom7misc/tempo/restart.sh; date > /home/pi/last-restart-due-to-wifi )
