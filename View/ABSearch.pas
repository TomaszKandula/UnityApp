unit ABSearch;


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
    InterposerClasses;


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
        CheckBoxNameEqual: TCheckBox;
        CheckBoxEmailEqual: TCheckBox;
        CheckBoxEstatEqual: TCheckBox;
        CheckBoxNameCase: TCheckBox;
        CheckBoxEmailCase: TCheckBox;
        CheckBoxEstatCase: TCheckBox;
        CheckBoxAliasEqual: TCheckBox;
        CheckBoxAliasCase: TCheckBox;
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
    private
        var FCollate1: string;
        var FCollate2: string;
        var FCollate3: string;
        var FCollate4: string;
        var FIsEqual:  string;
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


var
    ViewSearchForm: TViewSearchForm;


implementation


{$R *.dfm}


uses
    Main,
    Settings,
    Worker,
    DbModel,
    Helpers;


// ------------------------------------------------------------------------------------------------------------------------------------------- CLASS HELPERS //


procedure TViewSearchForm.Initialize;
begin

    var Settings: ISettings:=TSettings.Create;
    ViewSearchForm.Caption:=Settings.GetStringValue(TConfigSections.ApplicationDetails, 'WND_ABSEARCH', TUnityApp.APPCAPTION);

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


procedure TViewSearchForm.ClearAll;
begin
    ResetFields;
    ResetCheckboxes;
    ResetFieldTexts;
    ResetFieldColors;
end;


procedure TViewSearchForm.MatchCase;
begin
    if CheckBoxNameCase.Checked  then FCollate1:=TSql.MATCHCASE;
    if CheckBoxEmailCase.Checked then FCollate2:=TSql.MATCHCASE;
    if CheckBoxEstatCase.Checked then FCollate3:=TSql.MATCHCASE;
    if CheckBoxAliasCase.Checked then FCollate4:=TSql.MATCHCASE;
end;


procedure TViewSearchForm.IsEqual;
begin
    if CheckBoxNameEqual.Checked then FIsEqual:=TSql.EQUAL else FIsEqual:=TSql.LIKE;
    if CheckBoxEmailCase.Checked then FIsEqual:=TSql.EQUAL else FIsEqual:=TSql.LIKE;
    if CheckBoxEstatCase.Checked then FIsEqual:=TSql.EQUAL else FIsEqual:=TSql.LIKE;
    if CheckBoxAliasCase.Checked then FIsEqual:=TSql.EQUAL else FIsEqual:=TSql.LIKE;
end;


procedure TViewSearchForm.AttachOption;
begin
    if EditName.Enabled       then FStrEditName      :=TAddressBook.CustomerName + FIsEqual + QuotedStr(EditName.Text)       + FCollate1 + TSql._AND;
    if EditEmail.Enabled      then FStrEditEmail     :=TAddressBook.Emails       + FIsEqual + QuotedStr(EditEmail.Text)      + FCollate2 + TSql._AND;
    if EditEstatement.Enabled then FStrEditEstatement:=TAddressBook.Estatements  + FIsEqual + QuotedStr(EditEstatement.Text) + FCollate3 + TSql._AND;
    if EditUserAlias.Enabled  then FStrEditUserAlias :=TAddressBook.UserAlias    + FIsEqual + QuotedStr(EditUserAlias.Text)  + FCollate4 + TSql._AND;
end;


procedure TViewSearchForm.UpdateOptions;
begin
    if EditNumber.Enabled   then FStrEditNumber  :=TAddressBook.CustomerNumber + TSql.EQUAL + QuotedStr(EditNumber.Text)   + TSql._AND;
    if EditPhones.Enabled   then FStrEditPhones  :=TAddressBook.PhoneNumbers   + TSql.EQUAL + QuotedStr(EditPhones.Text)   + TSql._AND;
    if EditCoCode.Enabled   then FStrEditCoCode  :=TAddressBook.CoCode         + TSql.EQUAL + QuotedStr(EditCoCode.Text)   + TSql._AND;
    if EditAgent.Enabled    then FStrEditAgent   :=TAddressBook.Agent          + TSql.EQUAL + QuotedStr(EditAgent.Text)    + TSql._AND;
    if EditDivision.Enabled then FStrEditDivision:=TAddressBook.Division       + TSql.EQUAL + QuotedStr(EditDivision.Text) + TSql._AND;
end;


procedure TViewSearchForm.ResetFields;
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


procedure TViewSearchForm.ResetCheckboxes;
begin
    ResetCheckboxCaptions;
    ResetCheckboxTicks;
    ResetCheckboxDisable;
end;


procedure TViewSearchForm.ResetCheckboxCaptions;
begin
    CheckBoxNameEqual.Caption:='Equal';
    CheckBoxEmailEqual.Caption:='Equal';
    CheckBoxEstatEqual.Caption:='Equal';
    CheckBoxAliasEqual.Caption:='Equal';
end;


procedure TViewSearchForm.ResetCheckboxTicks;
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


procedure TViewSearchForm.ResetCheckboxDisable;
begin
    CheckBoxNameCase.Enabled := False;
    CheckBoxEmailCase.Enabled := False;
    CheckBoxEstatCase.Enabled := False;
    CheckBoxAliasCase.Enabled := False;
    CheckBoxAliasEqual.Enabled := False;
    CheckBoxEstatEqual.Enabled := False;
    CheckBoxEmailEqual.Enabled := False;
    CheckBoxNameEqual.Enabled := False;
end;


procedure TViewSearchForm.ResetFieldColors;
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


procedure TViewSearchForm.ResetFieldTexts;
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


procedure TViewSearchForm.PerformSearch;
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

    // Match case
    MatchCase;

    // "Like" or "Equal"
    IsEqual;

    // Attach "like" or "equal" and/or collate option
    AttachOption;

    // Do not use "like" and match case
    UpdateOptions;

    // Build conditions
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

    // Execute, leave window opened
    TTAddressBook.Create(
        TEnums.TActionTask.adOpenForUser,
        MainForm.sgAddressBook,
        '',
        '',
        '',
        '',
        '',
        TSql.WHERE + Conditions
    );

end;


// ------------------------------------------------------------------------------------------------------------------------------------------------- STARTUP //


procedure TViewSearchForm.FormCreate(Sender: TObject);
begin
    Initialize;
end;


procedure TViewSearchForm.FormShow(Sender: TObject);
begin
    ClearAll;
end;


// -------------------------------------------------------------------------------------------------------------------------------------------- MOUSE EVENTS //


/// <summary>
/// Change caption from default Equal to Like, to indicate change in search function.
/// </summary>

procedure TViewSearchForm.CheckBoxNameEqualClick(Sender: TObject);
begin
    if not(CheckBoxNameEqual.Checked) then
        CheckBoxNameEqual.Caption:='Like'
            else
                CheckBoxNameEqual.Caption:='Equal';
end;


procedure TViewSearchForm.CheckBoxEmailEqualClick(Sender: TObject);
begin
    if not(CheckBoxEmailEqual.Checked) then
        CheckBoxEmailEqual.Caption:='Like'
            else
                CheckBoxEmailEqual.Caption:='Equal';
end;


procedure TViewSearchForm.CheckBoxEstatEqualClick(Sender: TObject);
begin
    if not(CheckBoxEstatEqual.Checked) then
        CheckBoxEstatEqual.Caption:='Like'
            else
                CheckBoxEstatEqual.Caption:='Equal';
end;


procedure TViewSearchForm.CheckBoxAliasEqualClick(Sender: TObject);
begin
    if not(CheckBoxAliasEqual.Checked) then
        CheckBoxAliasEqual.Caption:='Like'
            else
                CheckBoxAliasEqual.Caption:='Equal';
end;


/// <summary>
/// Change background color on check box change, reset the TEdit.
/// </summary>

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


/// <remarks>
/// Alter another check boxes.
/// </remarks>

procedure TViewSearchForm.CheckBoxNameClick(Sender: TObject);
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
        EditName.Color:=clWhite;
        CheckBoxNameEqual.Enabled:=False;
        CheckBoxNameCase.Enabled:=False;
    end
end;


procedure TViewSearchForm.CheckBoxEmailClick(Sender: TObject);
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
        EditEmail.Color:=clWhite;
        CheckBoxEmailEqual.Enabled:=False;
        CheckBoxEmailCase.Enabled:=False;
    end;
end;


procedure TViewSearchForm.CheckBoxEstatementClick(Sender: TObject);
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
        EditEstatement.Color:=clWhite;
        CheckBoxEstatEqual.Enabled:=False;
        CheckBoxEstatCase.Enabled:=False;
    end;
end;


procedure TViewSearchForm.CheckBoxUserAliasClick(Sender: TObject);
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
        EditUserAlias.Color:=clWhite;
        CheckBoxAliasEqual.Enabled:=False;
        CheckBoxAliasCase.Enabled:=False;
    end;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------- BUTTONS CALLS //


procedure TViewSearchForm.btnSearchClick(Sender: TObject);
begin
    PerformSearch;
end;


procedure TViewSearchForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;


end.

