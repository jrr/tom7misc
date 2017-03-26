#!/bin/bash

set +x
make cube.exe

for i in `seq 0 359`;
do
    ./cube.exe -minutes 60 -players 3 -states 3 -nolines -nonums -yaw 11 -pitch $i -roll 17 -o "abcframes/y$i.svg"
    rsvg-convert --keep-aspect-ratio --width=1080 "abcframes/y$i.svg" -o "abcframes/f$i.png"
done
