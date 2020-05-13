unit Sync.Utility;

// -------------------------------------------------------
// Application logic (business layer). Anyone can call it.
// Cannot hold references to Views.
// -------------------------------------------------------

interface


uses
    Unity.Grid,
    Unity.Records,
    Layout.AgeViewModel;


type


    IUtility = interface(IInterface)
    ['{14DFD8C6-97D5-4C81-84D3-8CD5924A27A5}']
        /// <summary>
        /// Allow to sync. load grid layout that has been saved in JSON file.
        /// Note: there is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method is always executed in the main thread.
        /// </remarks>
        function LoadAgeLayoutSync(FileName: string; var ALayoutColumns: TLayoutColumns): TCallResponse;
        /// <summary>
        /// Allow to sync. write grid layout that in JSON file.
        /// Note: there is no separate notification.
        /// </summary>
        /// <remarks>
        /// This method is always executed in the main thread.
        /// </remarks>
        function SaveAgeLayoutSync(FileName: string; SourceGrid: TStringGrid): TCallResponse;
    end;


    /// <remarks>
    /// Concrete implementation. Never call it directly, you can inherit from this class
    /// and override the methods or/and extend them.
    /// </remarks>
    TUtility = class(TInterfacedObject, IUtility)
    public
        constructor Create();
        destructor Destroy(); override;
        function LoadAgeLayoutSync(FileName: string; var ALayoutColumns: TLayoutColumns): TCallResponse; virtual;
        function SaveAgeLayoutSync(FileName: string; SourceGrid: TStringGrid): TCallResponse; virtual;
    end;


implementation


uses
    System.SysUtils,
    System.Classes,
    REST.Json,
    Unity.Service;


constructor TUtility.Create();
begin
end;


destructor TUtility.Destroy();
begin
    inherited;
end;


function TUtility.LoadAgeLayoutSync(FileName: string; var ALayoutColumns: TLayoutColumns): TCallResponse;
begin

    var LCallResponse: TCallResponse;
    try

        var Strings:=TStringList.Create();
        try
            Strings.LoadFromFile(FileName);
            ALayoutColumns:=TJson.JsonToObject<TLayoutColumns>(Strings.Text);
            LCallResponse.IsSucceeded:=True;
        finally
            Strings.Free();
        end;

    except
        on E: Exception do
        begin
            LCallResponse.IsSucceeded:=False;
            LCallResponse.LastMessage:='[LoadAgeLayoutSync]: Cannot execute. Error has been thrown: ' + E.Message;
            Service.Logger.Log(LCallResponse.LastMessage);
        end;

    end;

    Result:=LCallResponse;

end;


function TUtility.SaveAgeLayoutSync(FileName: string; SourceGrid: TStringGrid): TCallResponse;
begin

    var LCallResponse: TCallResponse;
    try

        var LayoutModel:=TLayoutColumns.Create(SourceGrid.ColCount);

        for var Index:=0 to SourceGrid.ColCount - 1 do
        begin

            if not Assigned(LayoutModel.Columns[Index]) then
                LayoutModel.Columns[Index]:=TDetails.Create();

            LayoutModel.Columns[Index].Number:=Index;
            LayoutModel.Columns[Index].Name  :=SourceGrid.Cells[Index, 0];
            LayoutModel.Columns[Index].Width :=SourceGrid.ColWidths[Index];

        end;

        var JsonContent:=TJson.ObjectToJsonString(LayoutModel);

        var Strings:=TStringList.Create();
        try
            Strings.Add(JsonContent);
            Strings.SaveToFile(FileName);
        finally
            Strings.Free();
        end;

        LCallResponse.IsSucceeded:=True;

    except
        on E: Exception do
        begin
            LCallResponse.IsSucceeded:=False;
            LCallResponse.LastMessage:='[SaveAgeLayoutSync]: Cannot execute. Error has been thrown: ' + E.Message;
            Service.Logger.Log(LCallResponse.LastMessage);
        end;

    end;

    Result:=LCallResponse;

end;


end.
