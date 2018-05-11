#!/bin/sh

# run as root!
echo performance > /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor
# echo powersave > /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor
tvservice -o
