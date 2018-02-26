{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
{                                                                                                                                                             }
{ Name:             Unity for Debt Management                                                                                                                 }
{ Version:          0.1                                                                                                                                       }
{ (C)(R):           Tomasz Kandula                                                                                                                            }
{ Originate:        10-07-2016 (Concept & GUI)                                                                                                                }
{ IDE:              Delphi XE2 / Delphi Tokyo                                                                                                                 }
{ Target:           Microsoft Windows 7 or newer                                                                                                              }
{ Dependencies:     Ararat Synapse (modified third-party) and own libraries                                                                                   }
{ NET Framework:    Required 4.6 or newer (Lync / Skype calls)                                                                                                }
{ LYNC version:     2013 or newer                                                                                                                             }
{ Initial:          02-12-2016 (ALPHA)                                                                                                                        }
{ 1st Release:      27-11-2017 (BETA 1)                                                                                                                       }
{ 2nd Release:      04-12-2017 (BETA 2)                                                                                                                       }
{ 3rd Release:      18-12-2017 (BETA 3)                                                                                                                       }
{ 4th Release:      27-12-2017 (BETA 4)                                                                                                                       }
{ 5th Release:      05-01-2018 (BETA 5)                                                                                                                       }
{ 6th Release:      19-01-2018 (BETA 6)                                                                                                                       }
{ 7th Release:      22-02-2018 (BETA 7)                                                                                                                       }
{ RC:               __-__-2018                                                                                                                                }
{ RTM:              __-__-2018                                                                                                                                }
{                                                                                                                                                             }
{ ----------------------------------------------------------------------------------------------------------------------------------------------------------- }
unit Tracker;

interface

(* NOTE: DO NOT PLACE 'MAIN' REFERENCE IN THE IMPLEMENTATION SECTION BUT IN THE INTERFACE SECTION. THIS IS NECESSARY DUE TO CLASS EXTENSIONS DEFINED IN MAIN *)

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Buttons, StrUtils, ADODB, Main;

{ --------------------------------------------------------------- ! MAIN CLASS ! ---------------------------------------------------------------------------- }
type
  TTrackerForm = class(TForm)
    FrameBottom: TShape;
    Label6: TLabel;
    EmailFromList: TComboBox;
    TextStatTo: TLabeledEdit;
    ErrorStatTo: TLabel;
    AppMain: TShape;
    btnOK: TSpeedButton;
    btnCancel: TSpeedButton;
    FrameTop: TShape;
    TextReminder1: TLabeledEdit;
    TextReminder2: TLabeledEdit;
    TextReminder3: TLabeledEdit;
    SplitLine: TBevel;
    TextMailTo: TLabeledEdit;
    TextLegalTo: TLabeledEdit;
    LayoutList: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ErrorLegalTo: TLabel;
    ErrorMailTo: TLabel;
    TextReminder4: TLabeledEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  public
    procedure GetData;
  end;

var
  TrackerForm: TTrackerForm;

{ ------------------------------------------------------------- ! IMPLEMENTATION ZONE ! --------------------------------------------------------------------- }

implementation

uses
  Model, SQL, Worker;

{$R *.dfm}

{ -------------------------------------------------------------- ! EXECUTE ON CREATE ! ---------------------------------------------------------------------- }
procedure TTrackerForm.FormCreate(Sender: TObject);
begin
  { ------------------------------------------------------------------------------------------------------------------------------------------ WINDOW CAPTION }
  TrackerForm.Caption:=Settings.TMIG.ReadString(Settings.ApplicationDetails, 'WND_TRACKER', Settings.APPNAME);
end;

{ --------------------------------------------------------------------------------------------------------------------------------------------------- ON SHOW }
procedure TTrackerForm.FormShow(Sender: TObject);
begin
  LayoutList.Items.Clear;
  EmailFromList.Items.Clear;
  TextReminder1.Text  :='0';
  TextReminder2.Text  :='0';
  TextReminder3.Text  :='0';
  TextReminder4.Text  :='0';
  TextMailTo.Text     :='';
  TextStatTo.Text     :='';
  TextLegalTo.Text    :='';
  ErrorMailTo.Visible :=True;
  ErrorStatTo.Visible :=True;
  ErrorLegalTo.Visible:=True;
  btnOK.Enabled       :=False;
end;

{ ----------------------------------------------------------------------------------------------------------------------------------------------- ON ACTIVATE }
procedure TTrackerForm.FormActivate(Sender: TObject);
begin
  if DataBase.LastError = 0 then
  begin
    Sleep(100);
    GetData;
  end
    else
      MainForm.MsgCall(2, 'Cannot connect with database. Please contact IT support.');
end;

{ ---------------------------------------------------------------------------------------------------------------------------------- RETRIVE AND DISPLAY DATA }
procedure TTrackerForm.GetData;
var
  SetKeys:     TStringList;
  TblTracker:  TTracker;
  iCNT:        integer;
begin
  { ---------------------------------------------------------------------------------------------------------------------------------------------- INITIALIZE }
  Screen.Cursor:=crHourGlass;
  SetKeys:=TStringList.Create();
  { ----------------------------------------------------------------------------------------------------------------------------------------- LIST OF LAYOUTS }
  try
    Settings.TMIG.ReadSection(Settings.VariousLayouts, SetKeys);
    for iCNT:=0 to SetKeys.Count - 1 do
      { ADD TO LIST ONLY THOSE FOR WHICH KEY EQUALS 'TRACKER' }
      if MidStr(SetKeys.Strings[iCNT], 1, 7) = 'TRACKER' then
        if Settings.TMIG.ReadString(Settings.VariousLayouts, 'TRACKER' + IntToStr(iCNT), '') <> '' then
          LayoutList.Items.Add(Settings.TMIG.ReadString(Settings.VariousLayouts, 'TRACKER' + IntToStr(iCNT), ''));
  finally
    SetKeys.Free;
    if LayoutList.Items.Count > 0 then LayoutList.ItemIndex:=0;
  end;
  { ------------------------------------------------------------------------------------------------------------------------------------------------ READ OUT }
(*
  TblTracker:=TTracker.Create;
  try
    { SETUP }
    TblTracker.idThd :=MainThreadID;
    TblTracker.CUID  :=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('CUID',    1, 1), MainForm.sgAgeView.Row];
    TblTracker.COCODE:=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('CO CODE', 1, 1), MainForm.sgAgeView.Row];
    TblTracker.BRANCH:=MainForm.sgAgeView.Cells[MainForm.sgAgeView.ReturnColumn('AGENT'  , 1, 1), MainForm.sgAgeView.Row];
    { READ FROM ADDRESS BOOK }
    TblTracker.TableSelect:=TblTracker.tbl_addressbook;
    if TblTracker.Read then
    begin
      if TblTracker.EMAILS      <> '' then TextMailTo.Text:=TblTracker.EMAILS;
      if TblTracker.ESTATEMENTS <> '' then TextStatTo.Text:=TblTracker.ESTATEMENTS;
    end;
    { READ FROM COMPANY TABLE }
    TblTracker.TableSelect:=TblTracker.tbl_company;
    if TblTracker.Read then
    begin
      if TblTracker.LEGALTO <> '' then TextLegalTo.Text:=TblTracker.LEGALTO;
      TextReminder1.Text:=TblTracker.REMINDER1;
      TextReminder2.Text:=TblTracker.REMINDER2;
      TextReminder3.Text:=TblTracker.REMINDER3;
      TextReminder4.Text:=TblTracker.LEGALACTION;
      EmailFromList.Items.Add(TblTracker.SEND_NOTE_FROM);
      EmailFromList.ItemIndex:=0;
    end;
  finally
    TblTracker.Free;
    if TextMailTo.Text  <> '' then ErrorMailTo.Visible :=False;
    if TextLegalTo.Text <> '' then ErrorStatTo.Visible :=False;
    if TextStatTo.Text  <> '' then ErrorLegalTo.Visible:=False;
    if not (ErrorLegalTo.Visible) and not (ErrorMailTo.Visible) and not (ErrorStatTo.Visible) then btnOK.Enabled:=True;
  end;
*)
  Screen.Cursor:=crDefault;
end;

{ ------------------------------------------------------------------- ! APPLICATION EVENTS ! ---------------------------------------------------------------- }

{ ----------------------------------------------------------------------------------------------------------------------------------------------------- CLOSE }
procedure TTrackerForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------------- OK | PROCESS }
procedure TTrackerForm.btnOKClick(Sender: TObject);
begin
  TTInvoiceTrackerRefresh.Create('ADD');
  Close;
end;

end.
