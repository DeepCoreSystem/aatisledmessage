// #############################################################################
// #                      --- AATiS LED Scroller GUI ---                       #
// #############################################################################
// # language.pas - Translation functions                                      #
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

unit language;

interface

uses Windows, SysUtils, Dialogs;

function GetLocale: String;
procedure InitTranslation;
function TranslateString( input: String ): String;

var lng: String;
const translation_german: array[0..19,0..1] of String = (
      ('LED Message', 'LED Laufschrift'),
      ('Message 1', 'Nachricht 1'),
      ('Message 2', 'Nachricht 2'),
      ('Message 3', 'Nachricht 3'),
      ('Message 4', 'Nachricht 4'),
      ('Enabled', 'Aktiviert'),
      ('Message text:', 'Nachricht:'),
      ('Speed:', 'Geschwindigkeit:'),
      ('Load', 'Laden'),
      ('Test', 'Vorschau'),
      ('Save', 'Speichern'),
      ('File', 'Datei'),
      ('Exit', 'Beenden'),
      ('Help', 'Hilfe'),
      ('About...', 'Über...'),
      ('About this program', 'Über dieses Programm'),
      ('Device not found, program will be closed.', 'Gerät nicht gefunden, Programm wird beendet.'),
      ('Message disabled, please enable and save!', 'Nachricht inaktiv, bitte aktivieren und speichern!'),
      ('Communication failed, please check connection!', 'Kommunikation fehlgeschlagen, bitte Anschluss prüfen!'),
      ('Settings changed, please safe first!', 'Einstellungen verändert, bitte erst speichern!')
  );

implementation

function GetLocale: String;
var
  Buffer : PChar;
  Size : integer;
begin
  result := '';
  Size := GetLocaleInfo (LOCALE_USER_DEFAULT, LOCALE_SENGLANGUAGE, nil, 0);
  GetMem(Buffer, Size);
  try
    GetLocaleInfo (LOCALE_USER_DEFAULT, LOCALE_SENGLANGUAGE, Buffer, Size);
    result := Buffer;
  finally
    FreeMem(Buffer);
  end;
end;


procedure InitTranslation;
begin
     lng := GetLocale;
end;


function TranslateString( input: String ): String;
var i: Integer;
begin
     result := input;
     if lng='German' then
     begin
          for i:=0 to Length(translation_german) do
          begin
               if (translation_german[i, 0] = input) or ('&'+translation_german[i, 0] = input) then
               begin
                    result := translation_german[i, 1];
                    break;
               end;
          end;
     end;
end;

end.
