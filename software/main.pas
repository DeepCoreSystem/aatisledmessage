// #############################################################################
// #                      --- AATiS LED Scroller GUI ---                       #
// #############################################################################
// # main.pas - Main dialog                                                    #
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

unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, Menus, jpeg, usbrequests, communication, language;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    Panel2: TPanel;
    ComboBox1: TComboBox;
    BitBtn1: TBitBtn;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    Help1: TMenuItem;
    About1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;
  DataChanged: Boolean;

const error_not_found: String = 'Device not found, program will be closed.';
const error_msg_disabled: String = 'Message disabled, please enable and save!';
const error_comm_failed: String = 'Communication failed, please check connection!';
const warning_preview_save: String = 'Settings changed, please safe first!';

function LoadValues(): Boolean;
function SaveValues(): Boolean;
function ActivateCurrentMessage(): Boolean;

implementation

uses about;

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
var r: Boolean;
var result: Integer;
begin
     // Init
     r := usbrequests_init();
     if (r=true) then
     begin
          LoadValues();
     end
     else
     begin
          result := MessageDlg(TranslateString(error_not_found), mtError, [mbOk], 0);
          if (result = mrCancel) or (result = mrOk) then
             begin
                  Application.ShowMainForm := False;
                  Application.Terminate;
             end;
     end;
end;


procedure TForm1.FormShow(Sender: TObject);
begin
     // Apply translation
     Application.Title := TranslateString(Application.Title);
     Form1.Caption := TranslateString(Form1.Caption);
     ComboBox1.Items[0] := TranslateString(ComboBox1.Items[0]);
     ComboBox1.Items[1] := TranslateString(ComboBox1.Items[1]);
     ComboBox1.Items[2] := TranslateString(ComboBox1.Items[2]);
     ComboBox1.Items[3] := TranslateString(ComboBox1.Items[3]);
     ComboBox1.Text := ComboBox1.Items[0];
     CheckBox1.Caption := TranslateString(CheckBox1.Caption);
     Label1.Caption := TranslateString(Label1.Caption);
     Label2.Caption := TranslateString(Label2.Caption);
     BitBtn1.Caption := TranslateString(BitBtn1.Caption);
     BitBtn2.Caption := TranslateString(BitBtn2.Caption);
     BitBtn3.Caption := TranslateString(BitBtn3.Caption);
     File1.Caption := TranslateString(File1.caption);
     Exit1.Caption := TranslateString(Exit1.caption);
     Help1.Caption := TranslateString(Help1.caption);
     About1.Caption := TranslateString(About1.caption);
     // Reset changed flag
     DataChanged := false;
end;


procedure TForm1.Exit1Click(Sender: TObject);
begin
     Close;
end;


procedure TForm1.BitBtn1Click(Sender: TObject);
var r: Boolean;
begin
     r := SaveValues();
     if r = true then
     begin
          DataChanged := false;
          ActivateCurrentMessage();
     end;
end;


procedure TForm1.ComboBox1Change(Sender: TObject);
var r: Boolean;
begin
     r := LoadValues();
     if r = true then
     begin
          DataChanged := false;
     end;
end;


procedure TForm1.BitBtn2Click(Sender: TObject);
var r: Boolean;
begin
     r := LoadValues();
     if r = true then
     begin
          DataChanged := false;
     end;
end;


procedure TForm1.BitBtn3Click(Sender: TObject);
var e: Boolean;
    r: Boolean;
    i: Byte;
begin
     i := Form1.ComboBox1.ItemIndex;
     if (i<0) or (i>3) then i := 0;

     if DataChanged = false then
     begin
          r := communication_getmessageusedflag(i, @e);
          if r = true then
          begin
               if e = true then
               begin
               ActivateCurrentMessage();
               end
               else
               begin
                    MessageDlg(TranslateString(error_msg_disabled), mtError, [mbOk], 0);
               end;
          end
          else
          begin
               MessageDlg(TranslateString(error_comm_failed), mtError, [mbOk], 0);
          end;
     end
     else
     begin
          MessageDlg(TranslateString(warning_preview_save), mtWarning, [mbOk], 0);
     end;
end;


function LoadValues(): Boolean;
var i: Byte;
    e: Boolean;
    s: Byte;
    t: String;
    r: Boolean;
begin
     result := false;
     i := Form1.ComboBox1.ItemIndex;
     if (i<0) or (i>3) then i := 0;
     r := communication_getmessage(i, @e, @s, @t);
     if r = true then
     begin
          Form1.CheckBox1.Checked := e;
          Form1.TrackBar1.Position := s;
          Form1.Edit1.Text := t;
          result := true;
     end
     else
     begin
          MessageDlg(TranslateString(error_comm_failed), mtError, [mbOk], 0);
     end;
end;


function SaveValues(): Boolean;
var i: Byte;
    e: Boolean;
    s: Byte;
    t: String;
    r: Boolean;
begin
     result := false;
     i := Form1.ComboBox1.ItemIndex;
     if (i<0) or (i>3) then i := 0;
     e := Form1.CheckBox1.Checked;
     s := Form1.TrackBar1.Position;
     t := Form1.Edit1.Text;

     r := communication_setmessage(i, e, s, t);
     if r = true then
     begin
          result := true;
     end
     else
     begin
          MessageDlg(TranslateString(error_comm_failed), mtError, [mbOk], 0);
     end;

end;


function ActivateCurrentMessage(): Boolean;
var i: Byte;
    r: Boolean;
begin
     result := false;
     i := Form1.ComboBox1.ItemIndex;
     if (i<0) or (i>3) then i := 0;
     r := usbrequests_start(i);
     if r = true then
     begin
          result := true;
     end
     else
     begin
          MessageDlg(TranslateString(error_comm_failed), mtError, [mbOk], 0);
     end;
end;


procedure TForm1.About1Click(Sender: TObject);
begin
     Form2.show;
end;


procedure TForm1.Edit1Change(Sender: TObject);
begin
     DataChanged := true;
end;


procedure TForm1.CheckBox1Click(Sender: TObject);
begin
     DataChanged := true;
end;


procedure TForm1.TrackBar1Change(Sender: TObject);
begin
     DataChanged := true;
end;

end.
