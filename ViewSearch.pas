{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{ Name:             Unity for Debt Management                                                                                                                 }
{ Version:          0.1                                                                                                                                       }
{ (C)(R):           Tomasz Kandula                                                                                                                            }
{ Originate:        10-07-2016 (Concept & GUI)                                                                                                                }
{ IDE:              RAD Studio with Delphi XE2 (migrated to Delphi Tokyo)                                                                                     }
{ Target:           Microsoft Windows 7 or newer                                                                                                              }
{ Dependencies:     Synopse Zip and own libraries                                                                                                             }
{ NET Framework:    Required 4.6 or newer (Lync / Skype calls)                                                                                                }
{ LYNC version:     2013 or newer                                                                                                                             }
{                                                                                                                                                             }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
unit ViewSearch;

(* NOTE: DO NOT PLACE 'MAIN' REFERENCE IN THE IMPLEMENTATION SECTION BUT IN THE INTERFACE SECTION. THIS IS NECESSARY DUE TO CLASS EXTENSIONS DEFINED IN MAIN *)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Main, Buttons, StdCtrls, ExtCtrls, StrUtils;

{ ---------------------------------------------------------------- ! MAIN CLASS ! --------------------------------------------------------------------------- }
type
  TViewSearchForm = class(TForm)
    PanelClient: TPanel;
    SearchBox: TGroupBox;
    PanelBottom: TPanel;
    btnSearch: TSpeedButton;
    btnCancel: TSpeedButton;
    EditName: TEdit;
    Text1: TLabel;
    Text2: TLabel;
    EditNumber: TEdit;
    Text3: TLabel;
    EditEmail: TEdit;
    Text4: TLabel;
    EditEstatement: TEdit;
    Text5: TLabel;
    EditPhones: TEdit;
    CheckBoxName: TCheckBox;
    CheckBoxNumber: TCheckBox;
    CheckBoxEmail: TCheckBox;
    CheckBoxEstatement: TCheckBox;
    CheckBoxPhones: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure CheckBoxNameClick(Sender: TObject);
    procedure CheckBoxNumberClick(Sender: TObject);
    procedure CheckBoxEmailClick(Sender: TObject);
    procedure CheckBoxEstatementClick(Sender: TObject);
    procedure CheckBoxPhonesClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  ViewSearchForm: TViewSearchForm;

{ ------------------------------------------------------------- ! IMPLEMENTATION ZONE ! --------------------------------------------------------------------- }

implementation

{$R *.dfm}

uses
  Settings, Worker, Model;

{ ############################################################ ! MAIN THREAD EVENTS ! ####################################################################### }

{ ------------------------------------------------------------------------------------------------------------------------------------------------- ON CREATE }
procedure TViewSearchForm.FormCreate(Sender: TObject);
var
  AppSettings: TSettings;
begin
  { ------------------------------------------------------------------------------------------------------------------------------------- LOAD WINDOW CAPTION }
  AppSettings:=TSettings.Create;
  try
    ViewSearchForm.Caption:=AppSettings.TMIG.ReadString(ApplicationDetails, 'WND_SEARCH', APPNAME);
  finally
    AppSettings.Free;
  end;
  { PANEL BORDERS }
  //.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
  //.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
  //.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
  //.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
  //.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
end;


{ --------------------------------------------------------------------------------------------------------------------------------------------------- ON SHOW }
procedure TViewSearchForm.FormShow(Sender: TObject);
begin
  EditNumber.Enabled        :=True;
  EditName.Enabled          :=False;
  EditEmail.Enabled         :=False;
  EditEstatement.Enabled    :=False;
  EditPhones.Enabled        :=False;
  CheckBoxNumber.Checked    :=True;
  CheckBoxName.Checked      :=False;
  CheckBoxEmail.Checked     :=False;
  CheckBoxEstatement.Checked:=False;
  CheckBoxPhones.Checked    :=False;
  EditNumber.Text           :='';
  EditName.Text             :='';
  EditEmail.Text            :='';
  EditEstatement.Text       :='';
  EditPhones.Text           :='';
end;

{ --------------------------------------------------------------- ! MOUSE EVENTS ! -------------------------------------------------------------------------- }

{ --------------------------------------------------------------------------------------------------------------------------------------- NUMBER FIELD ON/OFF }
procedure TViewSearchForm.CheckBoxNumberClick(Sender: TObject);
begin
  if CheckBoxNumber.Checked then
    EditNumber.Enabled:=True
      else
      begin
        EditNumber.Enabled:=False;
        EditNumber.Text:='';
      end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------- NAME FIELD ON/OFF }
procedure TViewSearchForm.CheckBoxNameClick(Sender: TObject);
begin
  if CheckBoxName.Checked then
    EditName.Enabled:=True
      else
      begin
        EditName.Enabled:=False;
        EditName.Text:='';
      end
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------- EMAIL FIELD ON/OFF }
procedure TViewSearchForm.CheckBoxEmailClick(Sender: TObject);
begin
  if CheckBoxEmail.Checked then
    EditEmail.Enabled:=True
      else
      begin
        EditEmail.Enabled:=False;
        EditEmail.Text:='';
      end;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------- ESTATEMENTS FIELD ON/OFF }
procedure TViewSearchForm.CheckBoxEstatementClick(Sender: TObject);
begin
  if CheckBoxEstatement.Checked then
    EditEstatement.Enabled:=True
      else
      begin
        EditEstatement.Enabled:=False;
        EditEstatement.Text:='';
      end;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------- PHONES FIELD ON/OFF }
procedure TViewSearchForm.CheckBoxPhonesClick(Sender: TObject);
begin
  if CheckBoxPhones.Checked then
    EditPhones.Enabled:=True
      else
      begin
        EditPhones.Enabled:=False;
        EditPhones.Text:='';
      end;
end;

{ --------------------------------------------------------------- ! BUTTON CALLS ! -------------------------------------------------------------------------- }

{ ---------------------------------------------------------------------------------------------------------------------------------------------------- SEARCH }
procedure TViewSearchForm.btnSearchClick(Sender: TObject);
var
  Conditions:         string;
  StrEditName:        string;
  StrEditNumber:      string;
  StrEditEmail:       string;
  StrEditEstatement:  string;
  StrEditPhones:      string;
begin

  if (EditName.Enabled) and (EditName.Text = '') then
  begin
    MainForm.ExecMessage(False, mcWarn, 'Please provide with Customer Name.');
    Exit;
  end;

  if (EditNumber.Enabled) and (EditNumber.Text = '') then
  begin
    MainForm.ExecMessage(False, mcWarn, 'Please provide with Customer Number.');
    Exit;
  end;

  if
    (
      CheckBoxNumber.Checked = False
    )
    and
    (
      CheckBoxName.Checked = False
    )
    and
    (
      CheckBoxEmail.Checked = False
    )
    and
    (
      CheckBoxEstatement.Checked = False
    )
    and
    (
      CheckBoxPhones.Checked = False
    )
  then
  begin
    MainForm.ExecMessage(False, mcWarn, 'Please provide with at least one condition.');
    Exit;
  end;

  if EditNumber.Enabled     then StrEditNumber    :=TAddressBook.CUSTOMER_NUMBER + EQUAL + QuotedStr(EditNumber.Text)     + _AND;
  if EditName.Enabled       then StrEditName      :=TAddressBook.CUSTOMER_NAME   + EQUAL + QuotedStr(EditName.Text)       + _AND;
  if EditEmail.Enabled      then StrEditEmail     :=TAddressBook.EMAILS          + EQUAL + QuotedStr(EditEmail.Text)      + _AND;
  if EditEstatement.Enabled then StrEditEstatement:=TAddressBook.ESTATEMENTS     + EQUAL + QuotedStr(EditEstatement.Text) + _AND;
  if EditPhones.Enabled     then StrEditPhones    :=TAddressBook.PHONE_NUMBERS   + EQUAL + QuotedStr(EditPhones.Text)     + _AND;

  Conditions:=StrEditNumber + StrEditName + StrEditEmail + StrEditEstatement + StrEditPhones;
  Conditions:=LeftStr(Conditions, Length(Conditions) - Length(_AND));

  TTAddressBook.Create(
                        adOpenForUser,
                        MainForm.sgAddressBook,
                        '',
                        '',
                        '',
                        '',
                        WHERE + Conditions
                      );
  (* Close; *)

end;

{ ---------------------------------------------------------------------------------------------------------------------------------------------- CLOSE WINDOW }
procedure TViewSearchForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

end.
