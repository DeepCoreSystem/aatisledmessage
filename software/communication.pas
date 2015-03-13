// #############################################################################
// #                      --- AATiS LED Scroller GUI ---                       #
// #############################################################################
// # communication.pas - USB communication protocol                            #
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

unit communication;

interface

uses usbrequests;

type
  PByte = ^Byte;
  PBoolean = ^Boolean;

function communication_setmessage( msgid: Byte; used: Boolean; speed: Byte; message: String ): boolean;
function communication_getmessage( msgid: Byte; used: PBoolean; speed: PByte; message: PString ): boolean;
function communication_getmessageusedflag( msgid: Byte; used: PBoolean ): boolean;

implementation

function communication_setmessage( msgid: Byte; used: Boolean; speed: Byte; message: String ): boolean;
var msgdata: Array[1..120] of Byte;
    r: boolean;
    len: Integer;
    i: Integer;
begin
     result := false;

     // Write message used flag
     if(used) then
     begin
          r := usbrequests_eewrite( CUSTOM_EE_USED, msgid, 0, 1 );
     end else begin
          r := usbrequests_eewrite( CUSTOM_EE_USED, msgid, 0, 0 );
     end;
     if(r=false) then exit;

     // Write speed
     r := usbrequests_eewrite( CUSTOM_EE_SPEED, msgid, 0, speed );
     if(r=false) then exit;

     // Write message string and length
     len := Length(message);
     for i := 1 to len do
     begin
          msgdata[i] := Ord(message[i]);
     end;
     r := usbrequests_eewrite_range( CUSTOM_EE_MESSAGE, msgid, 0, len, @msgdata );
     if(r=false) then exit;
     r := usbrequests_eewrite( CUSTOM_EE_LENGTH, msgid, 0, len );
     if(r=false) then exit;

     result := true;
end;


function communication_getmessage( msgid: Byte; used: PBoolean; speed: PByte; message: PString ): boolean;
var msgdata: Array[1..120] of Byte;
    data: byte;
    r: boolean;
    len: Integer;
    i: Integer;
    s: String;
begin
     result := false;

     // Read message used flag
     r := usbrequests_eeread( CUSTOM_EE_USED, msgid, 0, @data );
     if(r=false) then exit;
     if(data=0) then used^ := false else used^ := true;

     // Read speed
     r := usbrequests_eeread( CUSTOM_EE_SPEED, msgid, 0, @data );
     if(r=false) then exit;
     speed^ := data;

     // Read message string and length
     r := usbrequests_eeread( CUSTOM_EE_LENGTH, msgid, 0, @data );
     if(r=false) then exit;
     len := data;

     r := usbrequests_eeread_range( CUSTOM_EE_MESSAGE, msgid, 0, len, @msgdata );
     if(r=false) then exit;
     s := '';
     for i := 1 to len do
     begin
          s := s + chr(msgdata[i]);
     end;
     message^ := s;

     result := true;
end;


function communication_getmessageusedflag( msgid: Byte; used: PBoolean ): boolean;
var data: byte;
    r: boolean;
begin
     result := false;

     // Read message used flag
     r := usbrequests_eeread( CUSTOM_EE_USED, msgid, 0, @data );
     if(r=false) then exit;
     if(data=0) then used^ := false else used^ := true;

     result := true;
end;

end.
