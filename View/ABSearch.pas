
{$I \Include\Header.inc}

unit ABSearch;


interface


uses
    Windows,
    Messages,
    SysUtils,
    Variants,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    Buttons,
    StdCtrls,
    ExtCtrls,
    StrUtils,
    InterposerClasses;


type

    /// <summary>
    ///     Search view form class with helpers for Address Book string grid page.
    /// </summary>

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
        procedure Initialize;
        procedure ClearAll;
        procedure PerformSearch;
    public
        { EMPTY }
    end;


var
    ViewSearchForm: TViewSearchForm;


implementation


{$R *.dfm}


uses
    Main,
    Settings,
    Worker,
    Model;


// ------------------------------------------------------------------------------------------------------------------------------------------- CLASS HELPERS //


/// <summary>
///     Execute at form creation. Setup the caption and panel borders for TEdit components residing inside.
/// </summary>

procedure TViewSearchForm.Initialize;
var
    Settings: ISettings;
begin

    Settings:=TSettings.Create;
    ViewSearchForm.Caption:=Settings.GetStringValue(ApplicationDetails, 'WND_ABSEARCH', APPCAPTION);

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


/// <summary>
///     Clear all and set default states before display to the user.
/// </summary>

procedure TViewSearchForm.ClearAll;
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

    CheckBoxNameEqual.Checked :=True;
    CheckBoxNameEqual.Caption :='Equal';
    CheckBoxNameEqual.Enabled :=False;

    CheckBoxEmailEqual.Checked:=True;
    CheckBoxEmailEqual.Caption:='Equal';
    CheckBoxEmailEqual.Enabled:=False;

    CheckBoxEstatEqual.Checked:=True;
    CheckBoxEstatEqual.Caption:='Equal';
    CheckBoxEstatEqual.Enabled:=False;

    CheckBoxAliasEqual.Checked:=True;
    CheckBoxAliasEqual.Caption:='Equal';
    CheckBoxAliasEqual.Enabled:=False;

    CheckBoxAliasCase.Checked :=False;
    CheckBoxNameCase.Checked  :=False;
    CheckBoxEmailCase.Checked :=False;
    CheckBoxEstatCase.Checked :=False;

    CheckBoxNameCase.Enabled  :=False;
    CheckBoxEmailCase.Enabled :=False;
    CheckBoxEstatCase.Enabled :=False;
    CheckBoxAliasCase.Enabled :=False;

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


/// <summary>
///     Check if required fields are not empty and perform search.
/// </summary>

procedure TViewSearchForm.PerformSearch;
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
    EqualLike:          string;
    Collate1:           string;
    Collate2:           string;
    Collate3:           string;
    Collate4:           string;
begin

    if (EditName.Enabled) and (string.IsNullOrEmpty(EditName.Text)) then
    begin
        MainForm.ExecMessage(False, mcWarn, 'Please provide with Customer Name.');
        Exit;
    end;

    if (EditNumber.Enabled) and (string.IsNullOrEmpty(EditNumber.Text)) then
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

    // Match case
    if CheckBoxNameCase.Checked  then Collate1:=MATCHCASE;
    if CheckBoxEmailCase.Checked then Collate2:=MATCHCASE;
    if CheckBoxEstatCase.Checked then Collate3:=MATCHCASE;
    if CheckBoxAliasCase.Checked then Collate4:=MATCHCASE;

    // "Like" or "Equal"
    if CheckBoxNameEqual.Checked then EqualLike:=EQUAL else EqualLike:=LIKE;
    if CheckBoxEmailCase.Checked then EqualLike:=EQUAL else EqualLike:=LIKE;
    if CheckBoxEstatCase.Checked then EqualLike:=EQUAL else EqualLike:=LIKE;
    if CheckBoxAliasCase.Checked then EqualLike:=EQUAL else EqualLike:=LIKE;

    // Attach "like" or "equal" and/or collate option
    if EditName.Enabled       then StrEditName      :=TAddressBook.CUSTOMER_NAME   + EqualLike + QuotedStr(EditName.Text)       + Collate1 + _AND;
    if EditEmail.Enabled      then StrEditEmail     :=TAddressBook.EMAILS          + EqualLike + QuotedStr(EditEmail.Text)      + Collate2 + _AND;
    if EditEstatement.Enabled then StrEditEstatement:=TAddressBook.ESTATEMENTS     + EqualLike + QuotedStr(EditEstatement.Text) + Collate3 + _AND;
    if EditUserAlias.Enabled  then StrEditUserAlias :=TAddressBook.USER_ALIAS      + EqualLike + QuotedStr(EditUserAlias.Text)  + Collate4 + _AND;

    // Do not use "like" and match case
    if EditNumber.Enabled     then StrEditNumber    :=TAddressBook.CUSTOMER_NUMBER + EQUAL + QuotedStr(EditNumber.Text)     + _AND;
    if EditPhones.Enabled     then StrEditPhones    :=TAddressBook.PHONE_NUMBERS   + EQUAL + QuotedStr(EditPhones.Text)     + _AND;
    if EditCoCode.Enabled     then StrEditCoCode    :=TAddressBook.COCODE          + EQUAL + QuotedStr(EditCoCode.Text)     + _AND;
    if EditAgent.Enabled      then StrEditAgent     :=TAddressBook.AGENT           + EQUAL + QuotedStr(EditAgent.Text)      + _AND;
    if EditDivision.Enabled   then StrEditDivision  :=TAddressBook.DIVISION        + EQUAL + QuotedStr(EditDivision.Text)   + _AND;

    // Build conditions
    Conditions:=StrEditNumber + StrEditName + StrEditEmail + StrEditEstatement + StrEditPhones + StrEditUserAlias + StrEditCoCode + StrEditAgent + StrEditDivision;
    Conditions:=LeftStr(Conditions, Length(Conditions) - Length(_AND));

    // Execute, leave window opened
    TTAddressBook.Create(
        adOpenForUser,
        MainForm.sgAddressBook,
        '',
        '',
        '',
        '',
        '',
        WHERE + Conditions
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
///     Change caption from default Equal to Like, to indicate change in search function.
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
///     Change background color on check box change, reset the TEdit.
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
///     Additionally, alter another check boxes.
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

