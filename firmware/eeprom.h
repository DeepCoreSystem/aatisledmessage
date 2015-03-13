// #############################################################################
// #                        --- AATiS LED Scroller ---                         #
// #############################################################################
// # eeprom.h - Predefined EEPROM data                                         #
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

#ifndef EEPROM_H
 #define EEPROM_H

 struct scroll_msg
  {
   uint8_t used;
   uint8_t speed;
   unsigned char message[120];
   uint8_t length;
  };

 #define EEPDATA0 {1, 0xF0, "Bei dieser superhellen LED Laufschrift k\366nnen maximal vier Texte mit bis zu 120 Zeichen \374ber USB programmiert werden!", 117}
 #define EEPDATA1 {1, 0xF0, "Jeder wackere Bayer vertilgt bequem zwo Pfund Kalbshaxen.", 57}
 #define EEPDATA2 {1, 0xF0, "Das Pferd frisst keinen Gurkensalat!", 36}
 #define EEPDATA3 {1, 0xF0, "Grumpy wizards make toxic brew for the evil Queen and Jack.", 59}

#endif
