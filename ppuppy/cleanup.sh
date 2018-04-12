#!/bin/sh

killall -9 bluealsa
killall -9 bluetoothd
killall -9 hciattach
killall -9 thd
killall -9 cron
killall -9 systemd-rfkill

tvservice -o
