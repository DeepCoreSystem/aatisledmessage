// #############################################################################
// #                      --- AATiS LED Scroller GUI ---                       #
// #############################################################################
// # usbrequests.pas - USB communication helper functions                      #
// #############################################################################
// #                  Version: 1.0 - Compiler: Borland Delphi 5                #
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

unit usbrequests;

interface

uses libusb;

// PID VID
const USB_VID = $16C0;
      USB_PID = $05DC;
      USB_MAN = 'AATiS e.V.';
      USB_NAM = 'LED Message';

// Request codes
const CUSTOM_RQ_ECHO    = 0;
      CUSTOM_RQ_EEREAD  = 1;
      CUSTOM_RQ_EEWRITE = 2;
      CUSTOM_RQ_PXWRITE = 3;
      CUSTOM_RQ_START   = 4;
      CUSTOM_RQ_STOP    = 5;

// EEPROM adresses
const CUSTOM_EE_USED    = 0;
      CUSTOM_EE_SPEED   = 1;
      CUSTOM_EE_MESSAGE = 2;
      CUSTOM_EE_LENGTH  = 3;

// Current USB device
var dev: pusb_device;

function usbrequests_init(): boolean;
function usbrequests_check(): boolean;
function usbrequests_eeread( eepos: Byte; msg: Byte; index: Byte; data: PByte ): boolean;
function usbrequests_eeread_range( eepos: Byte; msg: Byte; index_start: Byte; index_no_bytes: Byte; data: PByte ): boolean;
function usbrequests_eewrite( eepos: Byte; msg: Byte; index: Byte; data: Byte ): boolean;
function usbrequests_eewrite_range( eepos: Byte; msg: Byte; index_start: Byte; index_no_bytes: Byte; data: PByte ): boolean;
function usbrequests_pxwrite( addr: Byte; data: Byte ): boolean;
function usbrequests_start( msg: Byte ): boolean;
function usbrequests_stop(): boolean;


implementation

// ***** Initialize libusb and find matching device *****
// Parameters: none
// Return val: false if failed, true if found
function usbrequests_init(): boolean;
var r: integer;
    bus: pusb_bus;
    udev: pusb_dev_handle;
    S: array [0..255] of char;
begin
     result := false;
     usb_init;
     if usb_find_busses<1 then exit;
     if usb_find_devices<1 then exit;
     bus := usb_get_busses;
     if bus=nil then exit;
     while Assigned(bus) do
     begin
          dev := bus^.devices;
          while Assigned(dev) do
          begin
               if (dev^.descriptor.idVendor=USB_VID) and (dev^.descriptor.idProduct=USB_PID) then
               begin
                    if (dev^.descriptor.iManufacturer>0) and (dev^.descriptor.iProduct > 0) then
                    begin
                         udev:= usb_open(dev);
                         if Assigned(udev) then
                         begin
                              r := usb_get_string_simple(udev, dev^.descriptor.iManufacturer, S, sizeof(S));
                              if (r>0) then
                              begin
                                   if (S=USB_MAN) then
                                   begin
                                        r := usb_get_string_simple(udev, dev^.descriptor.iProduct, S, sizeof(S));
                                        if (r>0) then
                                        begin
                                             if (S=USB_NAM) then
                                             begin
                                                  result := true;
                                                  usb_close(udev);
                                                  break;
                                             end;
                                        end;
                                   end;
                              end;
                              usb_close(udev);
                         end;
                    end;
               end;
               dev := dev^.next;
          end;
          bus := bus^.next;
     end;
end;


// ***** Check if the device is ready *****
// Parameters: none
// Return val: false if device was disconnected, true if device is fine
function usbrequests_check(): boolean;
begin
     result := false;
     if Assigned(dev) then
     begin
          result := true;
     end;
end;


// ***** Read EEPROM *****
// Parameters: eepos: Adresses as defined above, msg: Message 0...3, index: Index if reading from string, data: Pointer to read value
// Return val: false if anything failed, true if done
function usbrequests_eeread( eepos: Byte; msg: Byte; index: Byte; data: PByte ): boolean;
var r: integer;
    udev: pusb_dev_handle;
    buffer: array[0..3] of byte;
begin
     result := false;
     if Assigned(dev) then
     begin
          udev:= usb_open(dev);
          if Assigned(udev) then
          begin
               r := usb_control_msg(udev, (USB_TYPE_VENDOR or USB_RECIP_DEVICE or USB_ENDPOINT_IN), CUSTOM_RQ_EEREAD, (index shl 8), (eepos+(msg shl 8)), buffer, sizeof(buffer), 5000);
               if (r=2) and (buffer[0]=0) then
               begin
                    data^ := buffer[1];
                    result := true;
               end;
               usb_close(udev);
          end;
     end;
end;


// ***** Read EEPROM range *****
// Parameters: eepos: Adresses as defined above, msg: Message 0...3, index_start: Index for start reading from string, index_no_bytes: Number of bytes to be read, data: Pointer to read values
// Return val: false if anything failed, true if done
function usbrequests_eeread_range( eepos: Byte; msg: Byte; index_start: Byte; index_no_bytes: Byte; data: PByte ): boolean;
var r: integer;
    index: integer;
    index_end: integer;
    udev: pusb_dev_handle;
    buffer: array[0..3] of byte;
begin
     result := false;
     if Assigned(dev) then
     begin
          udev:= usb_open(dev);
          if Assigned(udev) then
          begin
               index_end := index_start+index_no_bytes-1; // Array counts from 0
               for index := index_start to index_end do
               begin
                    r := usb_control_msg(udev, (USB_TYPE_VENDOR or USB_RECIP_DEVICE or USB_ENDPOINT_IN), CUSTOM_RQ_EEREAD, (index shl 8), (eepos+(msg shl 8)), buffer, sizeof(buffer), 5000);
                    if (r=2) and (buffer[0]=0) then
                    begin
                         data^ := buffer[1];
                         Inc(data);
                    end else begin
                        break;
                    end;
               end;
               result := true;
               usb_close(udev);
          end;
     end;
end;


// ***** Write EEPROM *****
// Parameters: eepos: Adresses as defined above, msg: Message 0...3, index: Index if writing to string, data: Value to be written
// Return val: false if anything failed, true if done
function usbrequests_eewrite( eepos: Byte; msg: Byte; index: Byte; data: Byte ): boolean;
var r: integer;
    udev: pusb_dev_handle;
    buffer: array[0..3] of byte;
begin
     result := false;
     if Assigned(dev) then
     begin
          udev:= usb_open(dev);
          if Assigned(udev) then
          begin
               r := usb_control_msg(udev, (USB_TYPE_VENDOR or USB_RECIP_DEVICE or USB_ENDPOINT_IN), CUSTOM_RQ_EEWRITE, (data+(index shl 8)), (eepos+(msg shl 8)), buffer, sizeof(buffer), 5000);
               if (r=1) and (buffer[0]=0) then result := true;
               usb_close(udev);
          end;
     end;
end;


// ***** Write EEPROM range *****
// Parameters: eepos: Adresses as defined above, msg: Message 0...3, index_start: Index for start writing from string, index_no_bytes: Number of bytes to be written, data: Pointer to values to be written
// Return val: false if anything failed, true if done
function usbrequests_eewrite_range( eepos: Byte; msg: Byte; index_start: Byte; index_no_bytes: Byte; data: PByte ): boolean;
var r: integer;
    index: integer;
    index_end: integer;
    udev: pusb_dev_handle;
    buffer: array[0..3] of byte;
begin
     result := false;
     if Assigned(dev) then
     begin
          udev:= usb_open(dev);
          if Assigned(udev) then
          begin
               index_end := index_start+index_no_bytes-1; // Array counts from 0
               for index := index_start to index_end do
               begin
                    r := usb_control_msg(udev, (USB_TYPE_VENDOR or USB_RECIP_DEVICE or USB_ENDPOINT_IN), CUSTOM_RQ_EEWRITE, (data^+(index shl 8)), (eepos+(msg shl 8)), buffer, sizeof(buffer), 5000);
                    if (r=1) and (buffer[0]=0) then
                    begin
                         Inc(data);
                    end else begin
                        break;
                    end;
               end;
               result := true;
               usb_close(udev);
          end;
     end;
end;


// ***** Write to pixelbuffer *****
// Parameters: addr: Address (display column) 0...9, data: Pixeldata
// Return val: false if anything failed, true if done
function usbrequests_pxwrite( addr: Byte; data: Byte ): boolean;
var r: integer;
    udev: pusb_dev_handle;
    buffer: array[0..3] of byte;
begin
     result := false;
     if Assigned(dev) then
     begin
          udev:= usb_open(dev);
          if Assigned(udev) then
          begin
               r := usb_control_msg(udev, (USB_TYPE_VENDOR or USB_RECIP_DEVICE or USB_ENDPOINT_IN), CUSTOM_RQ_PXWRITE, data, addr, buffer, sizeof(buffer), 5000);
               if (r=1) and (buffer[0]=0) then result := true;
               usb_close(udev);
          end;
     end;
end;


// ***** Start scrolling *****
// Parameters: msg: Message 0...3 to start with. If >3 start with last message.
// Return val: false if anything failed, true if done
function usbrequests_start( msg: Byte ): boolean;
var r: integer;
    udev: pusb_dev_handle;
    buffer: array[0..3] of byte;
begin
     result := false;
     if Assigned(dev) then
     begin
          udev:= usb_open(dev);
          if Assigned(udev) then
          begin
               r := usb_control_msg(udev, (USB_TYPE_VENDOR or USB_RECIP_DEVICE or USB_ENDPOINT_IN), CUSTOM_RQ_START, 0, msg, buffer, sizeof(buffer), 5000);
               if (r=1) and (buffer[0]=0) then result := true;
               usb_close(udev);
          end;
     end;
end;


// ***** Stop scrolling *****
// Parameters: none
// Return val: false if anything failed, true if done
function usbrequests_stop(): boolean;
var r: integer;
    udev: pusb_dev_handle;
    buffer: array[0..3] of byte;
begin
     result := false;
     if Assigned(dev) then
     begin
          udev:= usb_open(dev);
          if Assigned(udev) then
          begin
               r := usb_control_msg(udev, (USB_TYPE_VENDOR or USB_RECIP_DEVICE or USB_ENDPOINT_IN), CUSTOM_RQ_STOP, 0, 0, buffer, sizeof(buffer), 5000);
               if (r=1) and (buffer[0]=0) then result := true;
               usb_close(udev);
          end;
     end;
end;

end.
