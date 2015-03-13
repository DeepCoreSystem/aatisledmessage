// #############################################################################
// #                        --- AATiS LED Scroller ---                         #
// #############################################################################
// # font.h - Header for font table                                            #
// #############################################################################
// #              Version: 1.0 - Compiler: AVR-GCC 4.8.2 (Linux)               #
// #     (c) 2014 by Malte P�ggel - www.MALTEPOEGGEL.de - malte@poeggel.de     #
// #############################################################################
// #  This program is free software; you can redistribute it and/or modify it  #
// #   under the terms of the GNU General Public License as published by the   #
// #        Free Software Foundation; either version 3 of the License,         #
// #                  or (at your option) any later version.                   #
// #                                                                           #
// #      This program is distributed in the hope that it will be useful,      #
// #      but WITHOUT ANY WARRANTY; without even the implied warranty of       #
// #           MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.            #
// #           See the GNU General Public License for more details.            #
// #                                                                           #
// #  You should have received a copy of the GNU General Public License along  #
// #      with this program; if not, see <http://www.gnu.org/licenses/>.       #
// #############################################################################

#ifndef FONT_H
 #define FONT_H

 #include <avr/pgmspace.h>
 extern PROGMEM const unsigned char fontdata[256][6];

#endif
