# 8086tiny: a tiny, highly functional, highly portable PC emulator/VM
# Copyright 2013, Adrian Cable (adrian.cable@gmail.com) - http://www.megalith.co.uk/8086tiny
#
# This work is licensed under the MIT License. See included LICENSE.TXT.

# Set -DNO_GRAPHICS to compile without SDL/graphics support
OPTS_ALL=-O3 -fsigned-char
OPTS_SDL=`sdl-config --cflags --libs`
OPTS_NOGFX=-DNO_GRAPHICS

8086tiny: 8086tiny.c
	${CC} 8086tiny.c ${OPTS_SDL} ${OPTS_ALL} -o 8086tiny
	strip 8086tiny

no_graphics: 8086tiny.c
	${CC} 8086tiny.c ${OPTS_NOGFX} ${OPTS_ALL} -o 8086tiny
	strip 8086tiny

clean:
	rm 8086tiny
