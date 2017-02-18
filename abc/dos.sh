#!/bin/bash

make -C dosbox && dosbox/src/dosbox -c "mount d: /home/tom/abc/dos" -c "d:" -c "dir"
