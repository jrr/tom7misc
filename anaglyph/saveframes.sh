#!/bin/sh

if [ "$#" -ne 1 ]; then
    echo "Pass one argument, the output directory. Reads from 'frames/'."
    exit -1
fi

OUTDIR="$1"
mkdir "${OUTDIR}"
time pngcrush -d "${OUTDIR}" -reduce frames/*.png
rm -f frames/*.png
