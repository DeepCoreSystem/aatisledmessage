// #############################################################################
// #                        --- AATiS LED Scroller ---                         #
// #############################################################################
// # usbrequests.h - USB communication protocol                                #
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

#ifndef USBREQUESTS_H
 #define USBREQUESTS_H

 // Request that the device sends back wValue and wIndex. This is used with
 // random data to test the reliability of the communication.
 #define CUSTOM_RQ_ECHO    0

 // Read from EEPROM where wIndex[0] is the EEPROM position, wIndex[1] is the message (0...3)
 // and in case of EE_MESSAGE wValue[1] is the index of the message sting.
 // EEPROM data is returned into the 2nd byte sent
 #define CUSTOM_RQ_EEREAD  1

 // Write to EEPROM where wIndex[0] is the EEPROM position, wIndex[1] is the message (0...3),
 // wValue[0] is the data to be written and in case of EE_MESSAGE wValue[1] is the index of the message sting.
 #define CUSTOM_RQ_EEWRITE 2

 // Write to Pixelbuffer where wIndex is the address,
 // and wValue is the data to be written
 #define CUSTOM_RQ_PXWRITE 3

 // Start scrolling where wIndex is the message (0...3) 
 // if wIndex >3 we will start with the last active message before stop
 #define CUSTOM_RQ_START   4

 // Stop scrolling
 // This command will stop the output of the current message and clear the screen
 #define CUSTOM_RQ_STOP    5

 // EEPROM positions
 #define CUSTOM_EE_USED    0
 #define CUSTOM_EE_SPEED   1
 #define CUSTOM_EE_MESSAGE 2
 #define CUSTOM_EE_LENGTH  3

#endif
