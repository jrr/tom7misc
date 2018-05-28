#!/bin/bash

# run as root!

cd /home/pi/tom7misc/ppuppy
# Show that we've booted
/home/pi/tom7misc/ppuppy/set.exe 2 3 4 5 6 7 8 9
/home/pi/tom7misc/ppuppy/clear.exe 2
# always do this since cpu scaling causes us problems
echo performance > /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor
# If getbit doesn't exist (wasn't compiled) then also exit.
[ -e /home/pi/tom7misc/ppuppy/getbit.exe ] || exit 0
/home/pi/tom7misc/ppuppy/clear.exe 3
# If the hardware switch isn't set, then do not do any more automatically.
/home/pi/tom7misc/ppuppy/getbit.exe 24 || exit 0
# Also, if this file exists, don't automatically boot ppuppy.
[ -e /boot/debug.txt ] && exit 0
# Turn off HDMI to save power.
tvservice -o
/home/pi/tom7misc/ppuppy/clear.exe 5
# And, run ppuppy (never coming back).
/home/pi/tom7misc/ppuppy/prod.exe noint
# in case I forgot to compile it or something
/home/pi/tom7misc/ppuppy/ppuppy.exe noint
# But if we do come back (binary doesn't exist, etc.), then try to make this evident
/home/pi/tom7misc/ppuppy/clear.exe 6 7 8 9
echo "should not get here"

exit -1
