// #############################################################################
// #                      --- AATiS LED Scroller GUI ---                       #
// #############################################################################
// # about.pas - About dialog                                                  #
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

unit about;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, jpeg, ExtCtrls, ShellApi, Language;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Memo1: TMemo;
    Label5: TLabel;
    Image1: TImage;
    Image2: TImage;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure Label5Click(Sender: TObject);
    procedure Image2Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form2: TForm2;

implementation

{$R *.DFM}

procedure TForm2.Button1Click(Sender: TObject);
begin
     Form2.Close;
end;


procedure TForm2.FormShow(Sender: TObject);
begin
     // Apply translation
     Form2.Caption := TranslateString(Form2.Caption);
     Label1.Caption := TranslateString(Label1.Caption);
     Label2.Caption := TranslateString(Label2.Caption);
     Label3.Caption := TranslateString(Label3.Caption);
     Label4.Caption := TranslateString(Label4.Caption);
     Label5.Caption := TranslateString(Label5.Caption);
     Button1.Caption := TranslateString(Button1.Caption);
end;


procedure TForm2.Label4Click(Sender: TObject);
begin
     ShellExecute(Handle, 'open', 'http://www.maltepoeggel.de/', nil, nil, SW_SHOW);
end;


procedure TForm2.Label5Click(Sender: TObject);
begin
     ShellExecute(Handle, 'open', 'http://www.aatis.de/', nil, nil, SW_SHOW);
end;


procedure TForm2.Image2Click(Sender: TObject);
begin
     ShellExecute(Handle, 'open', 'http://www.gnu.org/licenses/gpl-3.0', nil, nil, SW_SHOW);
end;

end.
