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
unit Send;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls;

{ ------------------------------------------------------------------ ! MAIN CLASS ! ------------------------------------------------------------------------- }
type
  TSendForm = class(TForm)
    AppMain: TShape;
    btnCancel: TSpeedButton;
    btnSendEmail: TSpeedButton;
    Text_Subject: TMemo;
    Text1: TLabel;
    Text2: TLabel;
    Text_Body: TMemo;
    StatementAttach: TCheckBox;
  private
  public
  end;

var
  SendForm: TSendForm;

{ -------------------------------------------------------------- ! IMPLEMENTATION ZONE ! -------------------------------------------------------------------- }

implementation

uses
  Main, Settings;

{$R *.dfm}

{ ############################################################# ! MAIN FORM METHODS ! ####################################################################### }



end.
