// #############################################################################
// #                        --- AATiS LED Scroller ---                         #
// #############################################################################
// # usbrequests.c - USB communication protocol                                #
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
#include <avr/eeprom.h>
#include "usbdrv/usbdrv.h"
#include "usbrequests.h"
#include "eeprom.h"
#include "matrix.h"
#include "scroll.h"


usbMsgLen_t usbFunctionSetup(uchar data[8])
 {
  usbRequest_t *rq = (void *)data;
  static uint8_t replyBuffer[4];
  uint8_t len = 0;
  usbMsgPtr = (int)replyBuffer;

  if(rq->bRequest==CUSTOM_RQ_ECHO)
   {
    replyBuffer[0] = rq->wValue.bytes[0];
    replyBuffer[1] = rq->wValue.bytes[1];
    replyBuffer[2] = rq->wIndex.bytes[0];
    replyBuffer[3] = rq->wIndex.bytes[1];
    len = 4;
   } else {
    // First byte of buffer is returned if command successful.
    replyBuffer[0] = 0;
    // Read EEPROM
    if(rq->bRequest==CUSTOM_RQ_EEREAD)
     {
      if(rq->wIndex.bytes[0]==CUSTOM_EE_USED)
       {
        if(rq->wIndex.bytes[1]<4)
         {
          replyBuffer[1] = eeprom_read_byte(&ee_msg[rq->wIndex.bytes[1]].used);
          len = 2;
         }
       } else
        if(rq->wIndex.bytes[0]==CUSTOM_EE_SPEED)
         {
          if(rq->wIndex.bytes[1]<4)
           {
            replyBuffer[1] = eeprom_read_byte(&ee_msg[rq->wIndex.bytes[1]].speed);
            len = 2;
           }
         } else
          if(rq->wIndex.bytes[0]==CUSTOM_EE_MESSAGE)
           {
            if(rq->wIndex.bytes[1]<4&&rq->wValue.bytes[1]<120)
             {
              replyBuffer[1] = eeprom_read_byte(&ee_msg[rq->wIndex.bytes[1]].message[rq->wValue.bytes[1]]);
              len = 2;
             }
           } else
            if(rq->wIndex.bytes[0]==CUSTOM_EE_LENGTH)
             {
              if(rq->wIndex.bytes[1]<4)
               {
                replyBuffer[1] = eeprom_read_byte(&ee_msg[rq->wIndex.bytes[1]].length);
                len = 2;
               }
             }
     } else
      // Write EEPROM
      if(rq->bRequest==CUSTOM_RQ_EEWRITE)
       {
        if(rq->wIndex.bytes[0]==CUSTOM_EE_USED)
         {
          if(rq->wIndex.bytes[1]<4)
           {
            eeprom_write_byte(&ee_msg[rq->wIndex.bytes[1]].used, rq->wValue.bytes[0]);
            len = 1;
           }
         } else
          if(rq->wIndex.bytes[0]==CUSTOM_EE_SPEED)
           {
            if(rq->wIndex.bytes[1]<4)
             {
              eeprom_write_byte(&ee_msg[rq->wIndex.bytes[1]].speed, rq->wValue.bytes[0]);
              len = 1;
             }
           } else
            if(rq->wIndex.bytes[0]==CUSTOM_EE_MESSAGE)
             {
              if(rq->wIndex.bytes[1]<4&&rq->wValue.bytes[1]<120)
               {
                eeprom_write_byte(&ee_msg[rq->wIndex.bytes[1]].message[rq->wValue.bytes[1]], rq->wValue.bytes[0]);
                len = 1;
               }
             } else
              if(rq->wIndex.bytes[0]==CUSTOM_EE_LENGTH)
               {
                if(rq->wIndex.bytes[1]<4)
                 {
                  eeprom_write_byte(&ee_msg[rq->wIndex.bytes[1]].length, rq->wValue.bytes[0]);
                  len = 1;
                 }
               }
       } else
        // Write to pixelbuffer
        if(rq->bRequest==CUSTOM_RQ_PXWRITE)
         {
          if(rq->wIndex.bytes[0]<10)
           {
            displaybuffer[rq->wIndex.bytes[0]] = rq->wValue.bytes[0];
            len = 1;
           }
         } else
          // Start scrolling
          if(rq->bRequest==CUSTOM_RQ_START)
           {
            if(rq->wIndex.bytes[0]<4)
             {
              scroll_setmsg(rq->wIndex.bytes[0]);
              len = 1;
             }
            scroll_enable();
           } else
            // Stop scrolling
            if(rq->bRequest==CUSTOM_RQ_STOP)
             {
              scroll_disable();
              len = 1;
             }
   }
  return len;
}
