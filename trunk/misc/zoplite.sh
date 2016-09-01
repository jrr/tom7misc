#!/bin/bash

# /bin/find . -name "*.png" | xargs -n 1 -P 8 zopinplace.sh

if test "$#" -ne 1; then
    echo "zopinplace.sh filename.png"
    exit 1
fi

set -x
TMP=`mktemp -p . -t zopfli_XXXXXXXX.png`
echo "Optimizing $1 to ${TMP}..."
nice -n 16 zopflipng.exe --splitting=3 --filters=01234mepb --lossy_8bit --lossy_transparent "$1" "${TMP}" || exit -1
rm -f "$1"
mv "${TMP}" "$1"
