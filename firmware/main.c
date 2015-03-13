// #############################################################################
// #                        --- AATiS LED Scroller ---                         #
// #############################################################################
// # main.c - Main                                                             #
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

#include <avr/io.h>
#include <avr/wdt.h>
#include <avr/interrupt.h>

#include <avr/pgmspace.h>
#include "usbdrv/usbdrv.h"
#include "matrix.h"
#include "scroll.h"


int main(void)
 {
  // Enable watchdog
  wdt_enable(WDTO_1S);

  // Initialization stuff
  usbInit();
  matrix_init();
  scroll_init();

  // Enable interrupts
  sei();

  // Main loop
  for(;;)
   {
    // Reset watchdog
    wdt_reset();

    // Poll scrolling routines
    scroll_poll();

    // Poll USB
    usbPoll();
   }
 }
