# 8086tiny: a tiny, highly functional, highly portable PC emulator/VM
# Copyright 2013, Adrian Cable (adrian.cable@gmail.com) - http://www.megalith.co.uk/8086tiny
#
# This work is licensed under a Creative Commons Attribution-ShareAlike 3.0 Unported License.
# http://creativecommons.org/licenses/by-sa/3.0/

# Set -DNO_GRAPHICS to compile without SDL/graphics support
OPTS=`sdl-config --cflags --libs`
#OPTS=-DNO_GRAPHICS

8086tiny: 8086tiny.c
	${CC} 8086tiny.c ${OPTS} -o 8086tiny -O3 -fsigned-char
