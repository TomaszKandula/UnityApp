unit View.SqlSearch;

// ------------------------------------------------------------------------------
// Application GUI / view that can have direct calls to logic layer interface.
// Calls must have reference to callback method that is defined the same as
// callback signature. All views use Lazy Initialization pattern.
// ------------------------------------------------------------------------------

interface


uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Variants,
    System.Classes,
    System.StrUtils,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.Buttons,
    Vcl.StdCtrls,
    Vcl.ExtCtrls,
    Unity.Interposer;


type


    TSqlSearchForm = class(TForm)
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
        CheckBoxNameEqual: TCheckBox;
        CheckBoxEmailEqual: TCheckBox;
        CheckBoxEstatEqual: TCheckBox;
        CheckBoxNameCase: TCheckBox;
        CheckBoxEmailCase: TCheckBox;
        CheckBoxEstatCase: TCheckBox;
        CheckBoxAliasEqual: TCheckBox;
        CheckBoxAliasCase: TCheckBox;
        txtWarning: TLabel;
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
        procedure CheckBoxNameEqualClick(Sender: TObject);
        procedure CheckBoxEmailEqualClick(Sender: TObject);
        procedure CheckBoxEstatEqualClick(Sender: TObject);
        procedure CheckBoxAliasEqualClick(Sender: TObject);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
    private
        var FCollate1: string;
        var FCollate2: string;
        var FCollate3: string;
        var FCollate4: string;
        var FIsEqual1: string;
        var FIsEqual2: string;
        var FIsEqual3: string;
        var FIsEqual4: string;
        var FStrEditName:       string;
        var FStrEditNumber:     string;
        var FStrEditEmail:      string;
        var FStrEditEstatement: string;
        var FStrEditPhones:     string;
        var FStrEditUserAlias:  string;
        var FStrEditCoCode:     string;
        var FStrEditAgent:      string;
        var FStrEditDivision:   string;
        procedure Initialize;
        procedure ClearAll;
        procedure PerformSearch;
        procedure MatchCase;
        procedure IsEqual;
        procedure AttachOption;
        procedure UpdateOptions;
        procedure ResetFields;
        procedure ResetCheckboxes;
        procedure ResetCheckboxCaptions;
        procedure ResetCheckboxTicks;
        procedure ResetCheckboxDisable;
        procedure ResetFieldColors;
        procedure ResetFieldTexts;
    end;


    function SqlSearchForm: TSqlSearchForm;


implementation


{$R *.dfm}


uses
    View.Main,
    Unity.Settings,
    DbModel,
    Unity.Statics,
    Async.AddressBook;


var vSqlSearchForm: TSqlSearchForm;


function SqlSearchForm: TSqlSearchForm;
begin
    if not(Assigned(vSqlSearchForm)) then Application.CreateForm(TSqlSearchForm, vSqlSearchForm);
    Result:=vSqlSearchForm;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------- CLASS HELPERS //


procedure TSqlSearchForm.Initialize;
begin

    var Settings: ISettings:=TSettings.Create;
    SqlSearchForm.Caption:=Settings.GetStringValue(TConfigSections.ApplicationDetails, 'WND_ABSEARCH', TCommon.APPCAPTION);

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


procedure TSqlSearchForm.ClearAll;
begin
    ResetFields;
    ResetCheckboxes;
    ResetFieldTexts;
    ResetFieldColors;
end;


procedure TSqlSearchForm.MatchCase;
begin
    if CheckBoxNameCase.Checked  then FCollate1:=TSql.MATCHCASE;
    if CheckBoxEmailCase.Checked then FCollate2:=TSql.MATCHCASE;
    if CheckBoxEstatCase.Checked then FCollate3:=TSql.MATCHCASE;
    if CheckBoxAliasCase.Checked then FCollate4:=TSql.MATCHCASE;
end;


procedure TSqlSearchForm.IsEqual;
begin
    if CheckBoxNameEqual.Checked  then FIsEqual1:=TSql.EQUAL else FIsEqual1:=TSql.LIKE;
    if CheckBoxEmailEqual.Checked then FIsEqual2:=TSql.EQUAL else FIsEqual2:=TSql.LIKE;
    if CheckBoxEstatEqual.Checked then FIsEqual3:=TSql.EQUAL else FIsEqual3:=TSql.LIKE;
    if CheckBoxAliasEqual.Checked then FIsEqual4:=TSql.EQUAL else FIsEqual4:=TSql.LIKE;
end;


procedure TSqlSearchForm.AttachOption;
begin
    if EditName.Enabled       then FStrEditName      :=DbModel.TAddressBook.CustomerName + FIsEqual1 + QuotedStr(EditName.Text)       + FCollate1 + TSql._AND;
    if EditEmail.Enabled      then FStrEditEmail     :=DbModel.TAddressBook.Emails       + FIsEqual2 + QuotedStr(EditEmail.Text)      + FCollate2 + TSql._AND;
    if EditEstatement.Enabled then FStrEditEstatement:=DbModel.TAddressBook.Estatements  + FIsEqual3 + QuotedStr(EditEstatement.Text) + FCollate3 + TSql._AND;
    if EditUserAlias.Enabled  then FStrEditUserAlias :=DbModel.TAddressBook.UserAlias    + FIsEqual4 + QuotedStr(EditUserAlias.Text)  + FCollate4 + TSql._AND;
end;


procedure TSqlSearchForm.UpdateOptions;
begin
    if EditNumber.Enabled   then FStrEditNumber  :=DbModel.TAddressBook.CustomerNumber + TSql.EQUAL + QuotedStr(EditNumber.Text)   + TSql._AND;
    if EditPhones.Enabled   then FStrEditPhones  :=DbModel.TAddressBook.PhoneNumbers   + TSql.EQUAL + QuotedStr(EditPhones.Text)   + TSql._AND;
    if EditCoCode.Enabled   then FStrEditCoCode  :=DbModel.TAddressBook.CoCode         + TSql.EQUAL + QuotedStr(EditCoCode.Text)   + TSql._AND;
    if EditAgent.Enabled    then FStrEditAgent   :=DbModel.TAddressBook.Agent          + TSql.EQUAL + QuotedStr(EditAgent.Text)    + TSql._AND;
    if EditDivision.Enabled then FStrEditDivision:=DbModel.TAddressBook.Division       + TSql.EQUAL + QuotedStr(EditDivision.Text) + TSql._AND;
end;


procedure TSqlSearchForm.ResetFields;
begin
    EditNumber.Enabled:=True;
    EditName.Enabled:=False;
    EditEmail.Enabled:=False;
    EditEstatement.Enabled:=False;
    EditPhones.Enabled:=False;
    EditUserAlias.Enabled:=False;
    EditCoCode.Enabled:=False;
    EditAgent.Enabled:=False;
    EditDivision.Enabled:=False;
end;


procedure TSqlSearchForm.ResetCheckboxes;
begin
    ResetCheckboxCaptions;
    ResetCheckboxTicks;
    ResetCheckboxDisable;
end;


procedure TSqlSearchForm.ResetCheckboxCaptions;
begin
    CheckBoxNameEqual.Caption:='Equal';
    CheckBoxEmailEqual.Caption:='Equal';
    CheckBoxEstatEqual.Caption:='Equal';
    CheckBoxAliasEqual.Caption:='Equal';
end;


procedure TSqlSearchForm.ResetCheckboxTicks;
begin
    CheckBoxAliasCase.Checked:=False;
    CheckBoxNameCase.Checked:=False;
    CheckBoxEmailCase.Checked:=False;
    CheckBoxEstatCase.Checked:=False;
    CheckBoxAliasEqual.Checked:=True;
    CheckBoxEstatEqual.Checked:=True;
    CheckBoxEmailEqual.Checked:=True;
    CheckBoxNumber.Checked:=True;
    CheckBoxName.Checked:=False;
    CheckBoxEmail.Checked:=False;
    CheckBoxEstatement.Checked:=False;
    CheckBoxPhones.Checked:=False;
    CheckBoxUserAlias.Checked:=False;
    CheckBoxCoCode.Checked:=False;
    CheckBoxAgent.Checked:=False;
    CheckBoxDivision.Checked:=False;
    CheckBoxNameEqual.Checked:=True;
end;


procedure TSqlSearchForm.ResetCheckboxDisable;
begin
    CheckBoxNameCase.Enabled:=False;
    CheckBoxEmailCase.Enabled:=False;
    CheckBoxEstatCase.Enabled:=False;
    CheckBoxAliasCase.Enabled:=False;
    CheckBoxAliasEqual.Enabled:=False;
    CheckBoxEstatEqual.Enabled:=False;
    CheckBoxEmailEqual.Enabled:=False;
    CheckBoxNameEqual.Enabled:=False;
end;


procedure TSqlSearchForm.ResetFieldColors;
begin
    EditNumber.Color:=clCream;
    EditName.Color:=clWhite;
    EditEmail.Color:=clWhite;
    EditEstatement.Color:=clWhite;
    EditPhones.Color:=clWhite;
    EditUserAlias.Color:=clWhite;
    EditCoCode.Color:=clWhite;
    EditAgent.Color:=clWhite;
    EditDivision.Color:=clWhite;
end;


procedure TSqlSearchForm.ResetFieldTexts;
begin
    EditNumber.Text:='';
    EditName.Text:='';
    EditEmail.Text:='';
    EditEstatement.Text:='';
    EditPhones.Text:='';
    EditUserAlias.Text:='';
    EditCoCode.Text:='';
    EditAgent.Text:='';
    EditDivision.Text:='';
end;


procedure TSqlSearchForm.PerformSearch;
begin

    if (EditName.Enabled) and (string.IsNullOrEmpty(EditName.Text)) then
    begin
        MainForm.ExecMessage(False, TMessaging.TWParams.MessageWarn, 'Please provide with Customer Name.');
        Exit;
    end;

    if (EditNumber.Enabled) and (string.IsNullOrEmpty(EditNumber.Text)) then
    begin
        MainForm.ExecMessage(False, TMessaging.TWParams.MessageWarn, 'Please provide with Customer Number.');
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
        MainForm.ExecMessage(False, TMessaging.TWParams.MessageWarn, 'Please provide with at least one condition.');
        Exit;
    end;

    MatchCase;      // Match case
    IsEqual;        // "Like" or "Equal"
    AttachOption;   // Attach "like" or "equal" and/or collate option
    UpdateOptions;  // Do not use "like" and match case

    var Conditions: string:=
        FStrEditNumber +
        FStrEditName +
        FStrEditEmail +
        FStrEditEstatement +
        FStrEditPhones +
        FStrEditUserAlias +
        FStrEditCoCode +
        FStrEditAgent +
        FStrEditDivision;

    Conditions:=LeftStr(Conditions, Length(Conditions) - Length(TSql._AND));

    MainForm.DebugMsg(Conditions);

    var AddressBook: IAddressBook:=TAddressBook.Create();
    AddressBook.OpenAddressBookAsync('', MainForm.sgAddressBook, Conditions);

end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- STARTUP //


procedure TSqlSearchForm.FormCreate(Sender: TObject);
begin
    Initialize;
end;


procedure TSqlSearchForm.FormShow(Sender: TObject);
begin
    ClearAll;
end;


// -------------------------------------------------------------------------------------------------------------------------------------------- MOUSE EVENTS //


/// <summary>
/// Change caption from default Equal to Like, to indicate change in search function.
/// </summary>

procedure TSqlSearchForm.CheckBoxNameEqualClick(Sender: TObject);
begin
    if not(CheckBoxNameEqual.Checked) then
        CheckBoxNameEqual.Caption:='Like'
            else
                CheckBoxNameEqual.Caption:='Equal';
end;


procedure TSqlSearchForm.CheckBoxEmailEqualClick(Sender: TObject);
begin
    if not(CheckBoxEmailEqual.Checked) then
        CheckBoxEmailEqual.Caption:='Like'
            else
                CheckBoxEmailEqual.Caption:='Equal';
end;


procedure TSqlSearchForm.CheckBoxEstatEqualClick(Sender: TObject);
begin
    if not(CheckBoxEstatEqual.Checked) then
        CheckBoxEstatEqual.Caption:='Like'
            else
                CheckBoxEstatEqual.Caption:='Equal';
end;


procedure TSqlSearchForm.CheckBoxAliasEqualClick(Sender: TObject);
begin
    if not(CheckBoxAliasEqual.Checked) then
        CheckBoxAliasEqual.Caption:='Like'
            else
                CheckBoxAliasEqual.Caption:='Equal';
end;


/// <summary>
/// Change background color on check box change, reset the TEdit.
/// </summary>

procedure TSqlSearchForm.CheckBoxNumberClick(Sender: TObject);
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
        FStrEditNumber:='';
        EditNumber.Color:=clWhite;
    end;
end;


procedure TSqlSearchForm.CheckBoxPhonesClick(Sender: TObject);
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
        FStrEditPhones:='';
        EditPhones.Color:=clWhite;
    end;
end;


procedure TSqlSearchForm.CheckBoxAgentClick(Sender: TObject);
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
        FStrEditAgent:='';
        EditAgent.Color:=clWhite;
    end;
end;


procedure TSqlSearchForm.CheckBoxCoCodeClick(Sender: TObject);
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
        FStrEditCoCode:='';
        EditCoCode.Color:=clWhite;
    end;
end;


procedure TSqlSearchForm.CheckBoxDivisionClick(Sender: TObject);
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
        FStrEditDivision:='';
        EditDivision.Color:=clWhite;
    end;
end;


/// <remarks>
/// Alter another check boxes.
/// </remarks>

procedure TSqlSearchForm.CheckBoxNameClick(Sender: TObject);
begin
    if CheckBoxName.Checked then
    begin
        EditName.Enabled:=True;
        EditName.Color:=clCream;
        CheckBoxNameEqual.Enabled:=True;
        CheckBoxNameCase.Enabled:=True;
    end
    else
    begin
        EditName.Enabled:=False;
        EditName.Text:='';
        FStrEditName:='';
        EditName.Color:=clWhite;
        CheckBoxNameEqual.Enabled:=False;
        CheckBoxNameCase.Enabled:=False;
    end
end;


procedure TSqlSearchForm.CheckBoxEmailClick(Sender: TObject);
begin
    if CheckBoxEmail.Checked then
    begin
        EditEmail.Enabled:=True;
        EditEmail.Color:=clCream;
        CheckBoxEmailEqual.Enabled:=True;
        CheckBoxEmailCase.Enabled:=True;
    end
    else
    begin
        EditEmail.Enabled:=False;
        EditEmail.Text:='';
        FStrEditEmail:='';
        EditEmail.Color:=clWhite;
        CheckBoxEmailEqual.Enabled:=False;
        CheckBoxEmailCase.Enabled:=False;
    end;
end;


procedure TSqlSearchForm.CheckBoxEstatementClick(Sender: TObject);
begin
    if CheckBoxEstatement.Checked then
    begin
        EditEstatement.Enabled:=True;
        EditEstatement.Color:=clCream;
        CheckBoxEstatEqual.Enabled:=True;
        CheckBoxEstatCase.Enabled:=True;
    end
    else
    begin
        EditEstatement.Enabled:=False;
        EditEstatement.Text:='';
        FStrEditEstatement:='';
        EditEstatement.Color:=clWhite;
        CheckBoxEstatEqual.Enabled:=False;
        CheckBoxEstatCase.Enabled:=False;
    end;
end;


procedure TSqlSearchForm.CheckBoxUserAliasClick(Sender: TObject);
begin
    if CheckBoxUserAlias.Checked then
    begin
        EditUserAlias.Enabled:=True;
        EditUserAlias.Color:=clCream;
        CheckBoxAliasEqual.Enabled:=True;
        CheckBoxAliasCase.Enabled:=True;
    end
    else
    begin
        EditUserAlias.Enabled:=False;
        EditUserAlias.Text:='';
        FStrEditUserAlias:='';
        EditUserAlias.Color:=clWhite;
        CheckBoxAliasEqual.Enabled:=False;
        CheckBoxAliasCase.Enabled:=False;
    end;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------- BUTTONS CALLS //


procedure TSqlSearchForm.btnSearchClick(Sender: TObject);
begin
    PerformSearch;
end;


procedure TSqlSearchForm.btnCancelClick(Sender: TObject);
begin
    Close;
end;


// ----------------------------------------------------------------------------------------------------------------------------------------- KEYBOARD EVENTS //


procedure TSqlSearchForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close;
end;


end.

