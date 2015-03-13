// #############################################################################
// #                        --- AATiS LED Scroller ---                         #
// #############################################################################
// # scroll.c - Text scroll functions                                          #
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
#include <avr/eeprom.h>
#include "scroll.h"
#include "matrix.h"
#include "font.h"
#include "eeprom.h"

// Text in EEPROM
struct scroll_msg ee_msg[4] EEMEM = {EEPDATA0, EEPDATA1, EEPDATA2, EEPDATA3};

unsigned char txtbuffer[3]; // Buffer for text data scrolled on screen
uint8_t scroll_pos;         // Position counts from 0...5 steps the screen is moved left
uint8_t scroll_update;      // Flag set in timer interrupt. Screen is updated in main loop via scroll_poll();
uint8_t scroll_speed;       // Preload for timer counter to set scrolling speed
uint8_t msg_counter;        // Current active message (0...3)
uint8_t msg_char;           // Position for next character loaded into buffer
uint8_t msg_length;         // Length of active message in chars


// *** Initialization ***
void scroll_init( void )
 {
  // Clear buffer
  for(uint8_t i=0; i<3; i++)
   {
    txtbuffer[i]='\0';
   }

  // Init values
  scroll_pos = 0;
  scroll_update = 0;
  scroll_speed = 0;
  msg_counter = 0;
  msg_char = 0;

  // Setup timer for scrolling
  // Prescaler 12MHz / 256 = 0,085ms per tick, overflow every 5,6s
  // Enable timer overflow interrupt
  TCCR1B |= (1<<CS12);
  TIMSK1 |= (1<<TOIE1);

  // Load first message
  scroll_loadmsg();
 }


// *** Start scrolling if stopped before ***
void scroll_enable( void )
 {
  // Enable timer interrupt
  TIMSK1 |= (1<<TOIE1);

  // Load current message
  scroll_loadmsg();
 }


// *** Stop message scrolling ***
void scroll_disable( void )
 {
  uint8_t row;

  // Disable timer interrupt
  TIMSK1 &= ~(1<<TOIE1);

  // Clear displaybuffer
  for(row=0; row<10; row++)
   {
    displaybuffer[row] = 0x00;
   }
 }


// *** Changes the active message ***
void scroll_setmsg( uint8_t msg )
 {
  msg_counter = msg;
  scroll_loadmsg();
 }


// *** Load a new message to the buffer ***
void scroll_loadmsg( void )
 {
  uint8_t i;
  uint8_t used;

  // Reset positions
  scroll_pos = 0;
  msg_char = 0;

  // Is this message activated and length ok?
  for(i=0; i<4; i++)
   {
    // Get status and length
    used = eeprom_read_byte(&ee_msg[msg_counter].used);
    msg_length = eeprom_read_byte(&ee_msg[msg_counter].length);
    // Check
    if(used>0&&msg_length>0)
     {
      break;
     }
    // Message was invalid, skip.
    if(++msg_counter>=4) msg_counter = 0;
   }
  // All messages disabled?
  if(i==4)
   {
    scroll_disable();
   }

  // Set time parameters
  scroll_speed = eeprom_read_byte(&ee_msg[msg_counter].speed);
  TCNT1L = 0x00;
  TCNT1H = scroll_speed;
  scroll_update = 0;

  // Load first character
  txtbuffer[0] = '\0';
  txtbuffer[1] = '\0';
  txtbuffer[2] = eeprom_read_byte(&ee_msg[msg_counter].message[msg_char++]);
 }


// *** Should be called in main loop and will refresh the screen if necesary ***
void scroll_poll( void )
 {
  // Update needed? If not return here.
  if(scroll_update==0) return;
  scroll_update=0;

  // Scroll left, load new character if >5
  if(++scroll_pos>5)
   {
    scroll_pos=0; // Reset scroll position
    // Shift txtbuffer
    txtbuffer[0] = txtbuffer[1];
    txtbuffer[1] = txtbuffer[2];
    if(msg_char<msg_length)
     {
      // Fill with new character from EEPROM
      txtbuffer[2] = eeprom_read_byte(&ee_msg[msg_counter].message[msg_char++]);
     } else {
      // Fill with null char
      txtbuffer[2] = '\0';
      // Last character scrolled out?
      if(msg_char>=(msg_length+2))
       {
        // Switch to next message
        if(++msg_counter>=4) msg_counter = 0;
        scroll_loadmsg();
       } else {
        // Last character still on screen
        msg_char++;
       }
     }
   }

  // Refresh screen
  scroll_refresh();
 }


// *** Copy chars from txtbuffer to displaybuffer, scrollpos pixels moved left ***
void scroll_refresh( void )
 {
  uint8_t row;       // Row position 0...9
  uint8_t offset;    // Scrolled position
  uint8_t character; // Current char from txtbuffer
  for(row=0; row<10; row++)
   {
    offset = row + scroll_pos;
    character = txtbuffer[(offset/6)];
    displaybuffer[row] = pgm_read_byte(&fontdata[character][(offset%6)]);
   }
 }


// *** Timer ISR ***
ISR( TIMER1_OVF_vect, ISR_NOBLOCK ) 
 {
  // On overflow we will preload TCNT1 and set our update flag.
  TCNT1L = 0x00;
  TCNT1H = scroll_speed;
  scroll_update = 1;
 }
