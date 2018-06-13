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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Buttons, StdCtrls, ExtCtrls, StrUtils, Main;

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
    Splitter: TBevel;
    PanelEditNumber: TPanel;
    PanelEditName: TPanel;
    PanelEmail: TPanel;
    PanelPhones: TPanel;
    PanelEstatement: TPanel;
    Text6: TLabel;
    Text7: TLabel;
    Text8: TLabel;
    Text9: TLabel;
    CheckBoxUserAlias: TCheckBox;
    CheckBoxCoCode: TCheckBox;
    CheckBoxAgent: TCheckBox;
    CheckBoxDivision: TCheckBox;
    PanelUserAlias: TPanel;
    EditUserAlias: TEdit;
    PanelCoCode: TPanel;
    EditCoCode: TEdit;
    PanelAgent: TPanel;
    EditAgent: TEdit;
    PanelDivision: TPanel;
    EditDivision: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure CheckBoxNameClick(Sender: TObject);
    procedure CheckBoxNumberClick(Sender: TObject);
    procedure CheckBoxEmailClick(Sender: TObject);
    procedure CheckBoxEstatementClick(Sender: TObject);
    procedure CheckBoxPhonesClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CheckBoxUserAliasClick(Sender: TObject);
    procedure CheckBoxCoCodeClick(Sender: TObject);
    procedure CheckBoxAgentClick(Sender: TObject);
    procedure CheckBoxDivisionClick(Sender: TObject);
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
  PanelEditNumber.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
  PanelEditName.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
  PanelEmail.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
  PanelEstatement.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
  PanelPhones.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
  PanelUserAlias.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
  PanelCoCode.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
  PanelAgent.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
  PanelDivision.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
end;


{ --------------------------------------------------------------------------------------------------------------------------------------------------- ON SHOW }
procedure TViewSearchForm.FormShow(Sender: TObject);
begin

  EditNumber.Enabled        :=True;
  EditName.Enabled          :=False;
  EditEmail.Enabled         :=False;
  EditEstatement.Enabled    :=False;
  EditPhones.Enabled        :=False;
  EditUserAlias.Enabled     :=False;
  EditCoCode.Enabled        :=False;
  EditAgent.Enabled         :=False;
  EditDivision.Enabled      :=False;

  CheckBoxNumber.Checked    :=True;
  CheckBoxName.Checked      :=False;
  CheckBoxEmail.Checked     :=False;
  CheckBoxEstatement.Checked:=False;
  CheckBoxPhones.Checked    :=False;
  CheckBoxUserAlias.Checked :=False;
  CheckBoxCoCode.Checked    :=False;
  CheckBoxAgent.Checked     :=False;
  CheckBoxDivision.Checked  :=False;

  EditNumber.Text           :='';
  EditName.Text             :='';
  EditEmail.Text            :='';
  EditEstatement.Text       :='';
  EditPhones.Text           :='';
  EditUserAlias.Text        :='';
  EditCoCode.Text           :='';
  EditAgent.Text            :='';
  EditDivision.Text         :='';

  EditNumber.Color          :=clCream;
  EditName.Color            :=clWhite;
  EditEmail.Color           :=clWhite;
  EditEstatement.Color      :=clWhite;
  EditPhones.Color          :=clWhite;
  EditUserAlias.Color       :=clWhite;
  EditCoCode.Color          :=clWhite;
  EditAgent.Color           :=clWhite;
  EditDivision.Color        :=clWhite;

end;

{ --------------------------------------------------------------- ! MOUSE EVENTS ! -------------------------------------------------------------------------- }

{ --------------------------------------------------------------------------------------------------------------------------------------- NUMBER FIELD ON/OFF }
procedure TViewSearchForm.CheckBoxNumberClick(Sender: TObject);
begin
  if CheckBoxNumber.Checked then
  begin
    EditNumber.Enabled:=True;
    EditNumber.Color:=clCream;
  end
  else
  begin
    EditNumber.Enabled:=False;
    EditNumber.Text:='';
    EditNumber.Color:=clWhite;
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------- NAME FIELD ON/OFF }
procedure TViewSearchForm.CheckBoxNameClick(Sender: TObject);
begin
  if CheckBoxName.Checked then
  begin
    EditName.Enabled:=True;
    EditName.Color:=clCream;
  end
  else
  begin
    EditName.Enabled:=False;
    EditName.Text:='';
    EditName.Color:=clWhite;
  end
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------- EMAIL FIELD ON/OFF }
procedure TViewSearchForm.CheckBoxEmailClick(Sender: TObject);
begin
  if CheckBoxEmail.Checked then
  begin
    EditEmail.Enabled:=True;
    EditEmail.Color:=clCream;
  end
  else
  begin
    EditEmail.Enabled:=False;
    EditEmail.Text:='';
    EditEmail.Color:=clWhite;
  end;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------- ESTATEMENTS FIELD ON/OFF }
procedure TViewSearchForm.CheckBoxEstatementClick(Sender: TObject);
begin
  if CheckBoxEstatement.Checked then
  begin
    EditEstatement.Enabled:=True;
    EditEstatement.Color:=clCream;
  end
  else
  begin
    EditEstatement.Enabled:=False;
    EditEstatement.Text:='';
    EditEstatement.Color:=clWhite;
  end;
end;

{ --------------------------------------------------------------------------------------------------------------------------------------- PHONES FIELD ON/OFF }
procedure TViewSearchForm.CheckBoxPhonesClick(Sender: TObject);
begin
  if CheckBoxPhones.Checked then
  begin
    EditPhones.Enabled:=True;
    EditPhones.Color:=clCream;
  end
  else
  begin
    EditPhones.Enabled:=False;
    EditPhones.Text:='';
    EditPhones.Color:=clWhite;
  end;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------- USER ALIAS FIELD ON/OFF }
procedure TViewSearchForm.CheckBoxUserAliasClick(Sender: TObject);
begin
  if CheckBoxUserAlias.Checked then
  begin
    EditUserAlias.Enabled:=True;
    EditUserAlias.Color:=clCream;
  end
  else
  begin
    EditUserAlias.Enabled:=False;
    EditUserAlias.Text:='';
    EditUserAlias.Color:=clWhite;
  end;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------- AGENT FIELD ON/OFF }
procedure TViewSearchForm.CheckBoxAgentClick(Sender: TObject);
begin
  if CheckBoxAgent.Checked then
  begin
    EditAgent.Enabled:=True;
    EditAgent.Color:=clCream;
  end
  else
  begin
    EditAgent.Enabled:=False;
    EditAgent.Text:='';
    EditAgent.Color:=clWhite;
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------------- CO CODE FIELD ON/OFF }
procedure TViewSearchForm.CheckBoxCoCodeClick(Sender: TObject);
begin
  if CheckBoxCoCode.Checked then
  begin
    EditCoCode.Enabled:=True;
    EditCoCode.Color:=clCream;
  end
  else
  begin
    EditCoCode.Enabled:=False;
    EditCoCode.Text:='';
    EditCoCode.Color:=clWhite;
  end;
end;

{ ------------------------------------------------------------------------------------------------------------------------------------- DIVISION FIELD ON/OFF }
procedure TViewSearchForm.CheckBoxDivisionClick(Sender: TObject);
begin
  if CheckBoxDivision.Checked then
  begin
    EditDivision.Enabled:=True;
    EditDivision.Color:=clCream;
  end
  else
  begin
    EditDivision.Enabled:=False;
    EditDivision.Text:='';
    EditDivision.Color:=clWhite;
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
  StrEditUserAlias:   string;
  StrEditCoCode:      string;
  StrEditAgent:       string;
  StrEditDivision:    string;
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
    and
    (
      CheckBoxUserAlias.Checked = False
    )
    and
    (
      CheckBoxCoCode.Checked = False
    )
    and
    (
      CheckBoxAgent.Checked = False
    )
    and
    (
      CheckBoxDivision.Checked = False
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
  if EditUserAlias.Enabled  then StrEditUserAlias :=TAddressBook.USER_ALIAS      + EQUAL + QuotedStr(EditUserAlias.Text)  + _AND;
  if EditCoCode.Enabled     then StrEditCoCode    :=TAddressBook.COCODE          + EQUAL + QuotedStr(EditCoCode.Text)     + _AND;
  if EditAgent.Enabled      then StrEditAgent     :=TAddressBook.AGENT           + EQUAL + QuotedStr(EditAgent.Text)      + _AND;
  if EditDivision.Enabled   then StrEditDivision  :=TAddressBook.DIVISION        + EQUAL + QuotedStr(EditDivision.Text)   + _AND;

  Conditions:=StrEditNumber + StrEditName + StrEditEmail + StrEditEstatement + StrEditPhones + StrEditUserAlias + StrEditCoCode + StrEditAgent + StrEditDivision;
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
