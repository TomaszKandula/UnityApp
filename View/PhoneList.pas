
{$I \Include\Header.inc}

unit PhoneList;

interface

uses
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, Buttons, StdCtrls, InterposerClasses;

type

    /// <summary>
    ///     View form class with helpers for dispaying phone list from Address Book.
    /// </summary>

    TPhoneListForm = class(TForm)
        PhoneList: TMemo;
        btnCancel: TSpeedButton;
        PanelPhoneList: TPanel;
        btnSubmit: TSpeedButton;
        PanelMemo: TPanel;
        procedure FormCreate(Sender: TObject);
        procedure btnCancelClick(Sender: TObject);
        procedure btnSubmitClick(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
    private
        function  CheckPhoneList(List: TMemo): boolean;
    end;

var
  PhoneListForm: TPhoneListForm;

implementation

{$R *.dfm}

uses
    Main, Actions, Settings;


// ------------------------------------------------------------------------------------------------------------------------------------------- CLASS HELPERS //


/// <summary>
///     Check if given component (TMemo) holds only restricted characters.
/// </summary>

function TPhoneListForm.CheckPhoneList(List: TMemo): boolean;
const
    Restricted = ['0'..'9', #10, #13];
var
    iCNT:  integer;
    Str:   string;
    Check: integer;
begin
    Result:=False;
    Check:=0;

    Str:=List.Text;
    for iCNT:=0 to Str.Length do
    begin
        if CharInSet(Str[iCNT], Restricted)
            then
                Inc(Check);
    end;

    if Check = str.Length then Result:=True;

end;


// ------------------------------------------------------------------------------------------------------------------------------------- MAIN THREAD METHODS //


/// <summary>
///     Initialize and setup caption and the panel borders for TMemo component inside of it.
/// </summary>

procedure TPhoneListForm.FormCreate(Sender: TObject);
var
    AppSettings: TSettings;
begin
    AppSettings:=TSettings.Create;
    try
        ActionsForm.Caption:=AppSettings.TMIG.ReadString(ApplicationDetails, 'WND_PHONELIST', APPCAPTION);
    finally
        AppSettings.Free;
    end;
    PanelMemo.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
end;

/// <summary>
///     Clear all.
/// </summary>

procedure TPhoneListForm.FormShow(Sender: TObject);
begin
    PhoneList.Lines.Clear;
    PhoneList.Lines.AddStrings(ActionsForm.Cust_Phone.Items);
end;


// ------------------------------------------------------------------------------------------------------------------------------------------ BUTTONS EVENTS //


/// <summary>
///     Submit changes, check if only restricted characters are entered.
/// </summary>

procedure TPhoneListForm.btnSubmitClick(Sender: TObject);
begin

    if not(CheckPhoneList(PhoneList)) then
    begin
        MainForm.MsgCall(mcWarn, 'Please remove letters before saving the list.' + CRLF + 'Only numbers and break line are allowed.');
        Exit;
    end;

    ActionsForm.Cust_Phone.Items.Clear;
    ActionsForm.Cust_Phone.Items.AddStrings(PhoneList.Lines);

    if ActionsForm.Cust_Phone.Items.Count > 0
        then
            ActionsForm.Cust_Phone.ItemIndex:=0;

    Close;

end;

/// <summary>
///     Dismiss window.
/// </summary>

procedure TPhoneListForm.btnCancelClick(Sender: TObject);
begin
    Close;
end;


// ----------------------------------------------------------------------------------------------------------------------------------------- KEYBOARD EVENTS //

/// <summary>
///     Close window.
/// </summary>

procedure TPhoneListForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = ESC then Close;
end;

end.
