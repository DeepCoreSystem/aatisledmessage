// #############################################################################
// #                        --- AATiS LED Scroller ---                         #
// #############################################################################
// # matrix.c - LED matrix functions                                           #
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
#include <avr/interrupt.h>
#include "matrix.h"

uint8_t column_counter;


void matrix_init( void )
 {
  // Outputs for 7442 column driver
  DDRC |= (1<<PC0)|(1<<PC1)|(1<<PC2)|(1<<PC3);
  PORTC = 0x0F;

  // Pixeldata outputs for current column (INT0 / PD2 used for USB)
  DDRD |= (1<<PD0)|(1<<PD1)|(1<<PD3)|(1<<PD4)|(1<<PD5)|(1<<PD6)|(1<<PD7);

  // Setup timer for LED display multiplexing
  // Prescaler 12MHz / 64 = 0,005ms per tick, overflow every 1,365ms
  // Enable timer overflow interrupt
  TCCR0B |= (1<<CS01) | (1<<CS00);
  TIMSK0 |= (1<<TOIE0);

  // Init variables
  column_counter = 0;
  for(uint8_t i=0; i<10; i++)
   {
    displaybuffer[i] = 0x00;
   }
 }


// Timer ISR
ISR( TIMER0_OVF_vect, ISR_NOBLOCK ) 
 {
  uint8_t portvalue, buffervalue;
  // Column counter
  if(++column_counter>=10) column_counter=0;
  // Read value from displaybuffer
  #ifndef SWAP_MATRIX
  buffervalue = displaybuffer[column_counter];
  #else
  if(column_counter>=5) 
   {
    buffervalue = displaybuffer[column_counter-5];
   } else {
    buffervalue = displaybuffer[column_counter+5];
   }
  #endif
  // Invert & shift for correct output
  portvalue = 0xFB - (((buffervalue&0x7C)<<1)+(buffervalue&0x03));
  // Disable output, set next column, enable output again
  PORTD = 0xFB;
  PORTC = column_counter;
  PORTD = portvalue;
 }
