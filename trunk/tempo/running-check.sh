#!/bin/bash

# If tempo.exe isn't running, try restarting it.
#
# One scenario to protect against is that the database machine (or
# wifi) goes down, and we restart with wifi-check but tempo fails
# to connect, and just exits. If this happens

# (Not turned on yet. I'd rather have tempo be as stable as possible,
# including during startup, then to lean on this as a crutch for now.
# But eventually it does make sense to do something like this in case
# of rare crashes.)

# cd /home/pi/tom7misc/tempo
# date > /home/pi/last-wifi-check
# /bin/ping -q -c4 10.0.0.202 || (/sbin/ip link set wlan0 down ; sleep 1 ; /sbin/ip link set wlan0 up ; sleep 4 ; /home/pi/tom7misc/tempo/restart.sh; date > /home/pi/last-restart-due-to-wifi )
