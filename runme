#!/bin/sh
clear
stty cbreak raw -echo min 0
if [ -f hd.img ]
then
    ./8086tiny bios fd.img hd.img
else
    ./8086tiny bios fd.img
fi
stty cooked echo
