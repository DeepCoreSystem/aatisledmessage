// #############################################################################
// #                        --- AATiS LED Scroller ---                         #
// #############################################################################
// # matrix.h - LED matrix functions                                           #
// #############################################################################
// #              Version: 1.0 - Compiler: AVR-GCC 4.8.2 (Linux)               #
// #     (c) 2014 by Malte Pöggel - www.MALTEPOEGGEL.de - malte@poeggel.de     #
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

#ifndef MATRIX_H
 #define MATRIX_H

 #include <avr/io.h>

 // Uncomment to swap both matrix displays
 //#define SWAP_MATRIX

 // Global displaybuffer
 uint8_t displaybuffer[10];

 // Functions
 void matrix_init( void );

#endif
