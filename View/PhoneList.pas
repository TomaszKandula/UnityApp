unit PhoneList;


interface


uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Variants,
    System.Classes,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.ExtCtrls,
    Vcl.Buttons,
    Vcl.StdCtrls,
    InterposerClasses;


type


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


    function PhoneListForm: TPhoneListForm;


implementation


{$R *.dfm}


uses
    Main,
    Actions,
    Settings,
    Helpers,
    Statics;


var vPhoneListForm: TPhoneListForm;


const
    Restricted = ['0'..'9', #10, #13];


function PhoneListForm: TPhoneListForm;
begin
    if not(Assigned(vPhoneListForm)) then Application.CreateForm(TPhoneListForm, vPhoneListForm);
    Result:=vPhoneListForm;
end;


// ------------------------------------------------------------------------------------------------------------------------------------------- CLASS HELPERS //


/// <summary>
/// Check if given component (TMemo) holds only restricted characters.
/// </summary>

function TPhoneListForm.CheckPhoneList(List: TMemo): boolean;
begin

    Result:=False;

    var Check: integer:=0;
    var Str: string:=List.Text;

    for var iCNT: integer:=0 to Str.Length do
    begin
        if CharInSet(Str[iCNT], Restricted)
            then
                Inc(Check);
    end;

    if Check = str.Length then Result:=True;

end;


// ------------------------------------------------------------------------------------------------------------------------------------- MAIN THREAD METHODS //


procedure TPhoneListForm.FormCreate(Sender: TObject);
begin
    PanelMemo.PanelBorders(clWhite, clSkyBlue, clSkyBlue, clSkyBlue, clSkyBlue);
end;


procedure TPhoneListForm.FormShow(Sender: TObject);
begin
    PhoneList.Lines.Clear;
    PhoneList.Lines.AddStrings(ActionsForm.Cust_Phone.Items);
end;


// ------------------------------------------------------------------------------------------------------------------------------------------ BUTTONS EVENTS //


procedure TPhoneListForm.btnSubmitClick(Sender: TObject);
begin

    if not(CheckPhoneList(PhoneList)) then
    begin
        MainForm.MsgCall(TCommon.TMessage.Warn, 'Please remove letters before saving the list.' + TChars.CRLF + 'Only numbers and break line are allowed.');
        Exit;
    end;

    ActionsForm.Cust_Phone.Items.Clear;
    ActionsForm.Cust_Phone.Items.AddStrings(PhoneList.Lines);

    if ActionsForm.Cust_Phone.Items.Count > 0
        then
            ActionsForm.Cust_Phone.ItemIndex:=0;

    Close;

end;


procedure TPhoneListForm.btnCancelClick(Sender: TObject);
begin
    Close;
end;


// ----------------------------------------------------------------------------------------------------------------------------------------- KEYBOARD EVENTS //


procedure TPhoneListForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close;
end;


end.


