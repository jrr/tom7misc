#!/bin/bash

make -j 2 tempo.exe || exit -1
sudo killall tempo.exe
sudo nohup ./tempo.exe > /dev/null &
disown

