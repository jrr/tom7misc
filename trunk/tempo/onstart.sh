#!/bin/bash

# Wait for networking etc.
sleep 8

cd /home/pi/tom7misc/tempo
./tempo.exe
# TODO: On exit, retry. Periodically reboot.
