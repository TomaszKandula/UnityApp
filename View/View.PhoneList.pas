unit View.PhoneList;

// --------------------------------------------------------------------------------------
// This is application view (GUI) that can have direct calls to logic layer interface(s).
// Calls must carry reference(s) to callback method that is defined same as callback
// signature (delegate). All views use lazy initialization pattern.
// --------------------------------------------------------------------------------------

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
    Unity.Enums,
    Unity.Grid,
    Unity.Panel, Vcl.Imaging.pngimage;


type


    TPhoneListForm = class(TForm)
        PhoneList: TMemo;
        PanelPhoneList: TPanel;
        btnSubmit: TSpeedButton;
        PanelMemo: TPanel;
        PanelButtons: TPanel;
        ImageGrip: TImage;
        procedure FormCreate(Sender: TObject);
        procedure btnCancelClick(Sender: TObject);
        procedure btnSubmitClick(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
    strict private
        function  CheckPhoneList(List: TMemo): boolean;
    end;


    function PhoneListForm(): TPhoneListForm;


implementation


{$R *.dfm}


uses
    View.Main,
    View.Actions,
    Unity.Constants,
    Unity.Helpers,
    Unity.Settings;


var vPhoneListForm: TPhoneListForm;


const
    Restricted = ['0'..'9', #10, #13];


function PhoneListForm(): TPhoneListForm;
begin
    if not(Assigned(vPhoneListForm)) then Application.CreateForm(TPhoneListForm, vPhoneListForm);
    Result:=vPhoneListForm;
end;


{$REGION 'LOCAL HELPERS'}


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


{$ENDREGION}


{$REGION 'STARTUP'}


procedure TPhoneListForm.FormCreate(Sender: TObject);
begin
    PanelMemo.Borders(clWhite, $00E3B268, $00E3B268, $00E3B268, $00E3B268);
end;


procedure TPhoneListForm.FormShow(Sender: TObject);
begin
    PhoneList.Lines.Clear();
    PhoneList.Lines.AddStrings(ActionsForm.Cust_Phone.Items);
end;


{$ENDREGION}


{$REGION 'MOUSE CLICK EVENTS'}


procedure TPhoneListForm.btnSubmitClick(Sender: TObject);
begin

    if not(CheckPhoneList(PhoneList)) then
    begin

        THelpers.MsgCall(
            PhoneListForm.Handle,
            TAppMessage.Warn,
            'Please remove letters before saving the list.' + TChars.CRLF + 'Only numbers and break line are allowed.'
        );

        Exit();

    end;

    ActionsForm.Cust_Phone.Items.Clear();
    ActionsForm.Cust_Phone.Items.AddStrings(PhoneList.Lines);

    if ActionsForm.Cust_Phone.Items.Count > 0
        then
            ActionsForm.Cust_Phone.ItemIndex:=0;

    Close();

end;


procedure TPhoneListForm.btnCancelClick(Sender: TObject);
begin
    Close();
end;


{$ENDREGION}


{$REGION 'KEYBOARD EVENTS'}


procedure TPhoneListForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = Char(VK_ESCAPE) then Close();
end;


{$ENDREGION}


end.

