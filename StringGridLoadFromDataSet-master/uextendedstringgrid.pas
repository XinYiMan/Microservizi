unit uExtendedStringGrid;

{$mode objfpc}{$H+}

{

 NOTE IMPORTANTISSIME - DA LEGGERE PRIMA DI USARE
 1. Questa stringgrid è un interfaccia per consultare i dati di un dataset, non permette di modificare i dati direttamente
 2. Questa stringgrid è a senso unico, la stringgrid ha effetto sul dataset, ma il dataset non ha effetto sulla dbgrid una volta
    caricati i dati. Quindi vuol dire che se seleziono una riga dalla cella seleziono automaticamente anche il record nel recordset (ma non viceversa).
    Se si deve agire direttamente sul recordset senza passare da un iterazione grafica con la stringgrid usare le seguenti procedure e funzioni

    TStringGrid.First;
    TStringGrid.Next;
    TStringGrid.Prior;
    TStringGrid.Last;
    TStringGrid.Locate;

    e non

    TDataSet.First;
    TDataSet.Next;
    TDataSet.Prior;
    TDataSet.Last;
    TDataSet.Locate;
}


interface

uses
  Classes, SysUtils, Grids, db, controls, Graphics, LCLIntf;

type
    TColProperty = object
          fieldname            : string;
          visible              : boolean;
          width                : integer; // -1 = auto adjust
          alignment            : TAlignment;
          title_alignment      : TAlignment;
          dataset              : TDataSet;
          field_key            : string;
          field_value          : string;
          field_title          : string;
          is_checkbox          : boolean;
          check_value          : string;
          uncheck_value        : string;
          header_text          : string;
          filter_text          : string;
    end;
type
    TColumnsProperty  = array of TColProperty;

type
    TFilterBtnClick = procedure (fieldname: string) of object;

type
    TRowChangePosition = procedure (Sender : TObject; GridRowBeforePosition : integer; GridRowAfterPosition : integer; RecNoBeforePosition : integer; RecNoAfterPosition : integer) of object;

type

    { TStringGrid }

    TStringGrid = class(Grids.TStringGrid)
    private
          FErrorLoadFromDataSet       : string;
          FDataSet                    : TDataSet;
          FColumnsProperty            : TColumnsProperty;
          FOnFilterBtnClick           : TFilterBtnClick;
          NUM_ROW_TITLE               : integer;
          FShowFilterRow              : boolean;
          FShowHeaderRow              : boolean;
          FOnRowChangePosition        : TRowChangePosition;
          FEnableRowMove              : boolean;
          RowMoveInProgress           : boolean;
          NormalCursor                : TCursor;
          LoadingFromDataSet          : boolean;
          LocateInProgress            : boolean;
          RowSelectedInMouseDown      : integer;
          RowRecNoSelectedInMouseDown : integer;
          function Init() : boolean;
          function LoadTitleFromDS() : boolean;
          function SearchColumnsProperty(fieldname : string) : integer;
          function CountAdditionalFields() : integer;
          function GetValueIntoDataSet(DS: TDataSet; field_key: string;
            field_value: string; field_key_value: variant): string;
          procedure RowMove(Sender: TObject; Button: TMouseButton;
                                    Shift: TShiftState; X, Y: Integer);
          procedure SetEnableRowMove(AValue: boolean);
          procedure MoveSelection; override;
          procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
          procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
          procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
          procedure DrawTextInCell(aCol,aRow: Integer; aRect: TRect; aState: TGridDrawState); override;
          procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); override;
          procedure DrawRow(aRow: Integer); override;
    public
          constructor Create(AOwner: TComponent);override;
          function    LoadFromDataSet(ShowFiltersRow : boolean = false; ShowHeaderRow : boolean = false): boolean;
          function    GetColumnWidth(fieldname: string): integer;
          function    GetFieldNameFromColumn(aCol : integer) : string;
          procedure   First;
          procedure   Next;
          procedure   Prior;
          procedure   Last;
          function    Locate(const KeyFields: string; const KeyValues: Variant; SearchOptions: TLocateOptions) : boolean;

    published
          property DataSet                        : TDataSet read FDataSet write FDataSet;
          property ColumnsProperty                : TColumnsProperty read FColumnsProperty write FColumnsProperty;
          property OnFilterBtnClick               : TFilterBtnClick read FOnFilterBtnClick write FOnFilterBtnClick;
          property OnRowChangePosition            : TRowChangePosition read FOnRowChangePosition write FOnRowChangePosition;
          property EnableRowMove                  : boolean read FEnableRowMove write SetEnableRowMove;
    end;

implementation

{ TStringGrid }

function TStringGrid.Init(): boolean;
var
   i     : integer;
   qta   : integer;
begin
     try
        try

           result         := true;

           qta            := CountAdditionalFields() + FDataSet.Fields.Count;

           if Self.Columns.Count<>qta then //se le quantità sono diverse per me si tratta di una struttura dati diversa
              Self.Columns.Clear;

           if Self.Columns.Count<qta then
           begin
                for i := 0 to qta-1 do
                    Self.Columns.Add;
           end;

           Self.RowCount  := FDataSet.RecordCount + NUM_ROW_TITLE;
           Self.FixedCols := 0;
           Self.FixedRows := NUM_ROW_TITLE;

        finally

        end;
     except
           on E: Exception do
           begin

              result                := false;
              FErrorLoadFromDataSet := E.message;

           end;
     end;
end;

function TStringGrid.LoadTitleFromDS(): boolean;
var
   i, j : integer;
   idx  : integer;
begin
     result                := true;
     FErrorLoadFromDataSet := '';
     try
        try

           j := 0;
           for i := 0 to FDataSet.Fields.Count-1 do
           begin
                Self.Columns[j].Title.Caption   := FDataSet.Fields[i].FieldName;

                idx := Self.SearchColumnsProperty(FDataSet.Fields[i].FieldName);
                if idx>=0 then
                begin

                     with ColumnsProperty[idx] do
                     begin
                          Self.Columns[j].Title.Alignment := title_alignment;
                          if trim(field_title) <> '' then
                             Self.Columns[j].Title.Caption := trim(field_title)
                          else
                              Self.Columns[j].Title.Caption := fieldname;

                          if Self.FShowFilterRow then
                          begin
                               Self.Cells[j,1]   := filter_text;
                          end;

                          if Self.FShowHeaderRow then
                          begin
                               //HeadersText[j]   := header_text;
                               if Self.FShowFilterRow then
                                  Self.Cells[j,2] := header_text
                               else
                                   Self.Cells[j,1] := header_text;
                          end;
                     end;

                     if Assigned(ColumnsProperty[idx].dataset) then
                     begin
                          if (trim(ColumnsProperty[idx].field_key)<>'') and (ColumnsProperty[idx].dataset.FindField(ColumnsProperty[idx].field_key)<>nil) then
                          begin
                               if (trim(ColumnsProperty[idx].field_value)<>'') and (ColumnsProperty[idx].dataset.FindField(ColumnsProperty[idx].field_value)<>nil) then
                               begin
                                    Inc(j);
                                    with ColumnsProperty[idx] do
                                    begin
                                         Self.Columns[j].Title.Alignment := title_alignment;
                                         if trim(field_title) <> '' then
                                            Self.Columns[j].Title.Caption := trim(field_title)
                                         else
                                             Self.Columns[j].Title.Caption := fieldname;

                                         if Self.FShowFilterRow then
                                         begin
                                              Self.Cells[j,1]   := filter_text;
                                         end;

                                         if Self.FShowHeaderRow then
                                         begin
                                              //HeadersText[j]   := header_text;
                                              if Self.FShowFilterRow then
                                                 Self.Cells[j,2] := header_text
                                              else
                                                  Self.Cells[j,1] := header_text;
                                         end;

                                    end;

                               end;
                          end;
                     end;

                end;
                Inc(j);

           end;


        finally

        end;
     except
           on E: Exception do
           begin

              result                := false;
              FErrorLoadFromDataSet := E.message;

           end;
     end;
end;

function TStringGrid.SearchColumnsProperty(fieldname: string): integer;
var
   i          : integer;
begin
     result := -1;
     i      := 0;
     while (i<Length(ColumnsProperty)) and (result=-1) do
     begin
          if UpperCase(trim(fieldname)) = UpperCase(trim(ColumnsProperty[i].fieldname)) then
          begin
               result := i;
          end else begin
             Inc(i);
          end;
     end;
end;

function TStringGrid.CountAdditionalFields(): integer;
var
   i : integer;
begin
     result := 0;
     for i:=0 to Length(ColumnsProperty)-1 do
     begin
          if Assigned(ColumnsProperty[i].dataset) then
          begin
               if (trim(ColumnsProperty[i].field_key)<>'') and (ColumnsProperty[i].dataset.FindField(ColumnsProperty[i].field_key)<>nil) then
               begin
                    if (trim(ColumnsProperty[i].field_value)<>'') and (ColumnsProperty[i].dataset.FindField(ColumnsProperty[i].field_value)<>nil) then
                    begin
                         result := result + 1;
                    end;
               end;
          end;
     end;
end;

function TStringGrid.GetValueIntoDataSet(DS: TDataSet; field_key: string;
  field_value: string; field_key_value: variant): string;
var
   exit   : boolean;
   rec_no : integer;
begin
     result := '';

     if Assigned(DS) then
     begin
          if DS.Active then
          begin
               DS.DisableControls;
               if DS.RecordCount>0 then
               begin
                    rec_no := DS.RecNo;
                    exit   := false;
                    DS.First;
                    while (not DS.EOF) and (not exit) do
                    begin
                         if DS.FieldByName(field_key).Value = field_key_value then
                         begin
                              result := DS.FieldByName(field_value).AsString;
                              exit   := true;
                         end else begin
                              DS.Next;
                         end;
                    end;
                    DS.RecNo := rec_no;
               end;
               DS.EnableControls;

          end;
     end;

end;

procedure TStringGrid.RowMove(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
   ACol, ARow  : integer;
   RecNo       : integer;
begin
     Self.MouseToCell(X, Y, ACol, ARow);
     if Assigned(Self.FOnRowChangePosition) then
     begin
          RecNo := Self.FDataSet.RecNo;
     end else begin
          RecNo := 0;
     end;

     if (aRow>=Self.NUM_ROW_TITLE) and (RowSelectedInMouseDown>=Self.NUM_ROW_TITLE) and (aRow<>RowSelectedInMouseDown) then
        Self.FOnRowChangePosition(Sender, RowSelectedInMouseDown, aRow, RowRecNoSelectedInMouseDown, RecNo );
end;

procedure TStringGrid.SetEnableRowMove(AValue: boolean);
begin
  if FEnableRowMove=AValue then Exit;
  FEnableRowMove:=AValue;

  if AValue then
     Self.Options          := Self.Options + [goRowMoving]
  else
      Self.Options          := Self.Options - [goRowMoving];
end;

procedure TStringGrid.MoveSelection;
begin
     if (not LoadingFromDataSet) and (not LocateInProgress) then
     begin
          if Assigned(Self.FDataSet) then
          begin
               if Self.FDataSet.RecordCount>0 then
                  if Self.Row >= Self.NUM_ROW_TITLE then
                     Self.FDataSet.RecNo := Self.Row - Self.NUM_ROW_TITLE + 1;
          end;
     end;

     if (not Self.RowMoveInProgress) then //altrimenti nel momento in cui muovo il record per riordinarlo mi nasconde il cursor modificato
        inherited MoveSelection;
end;

constructor TStringGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NUM_ROW_TITLE               := 1;
  Self.Options                := Self.Options + [goColSizing, goRowSelect];
  Self.Options                := Self.Options - [goEditing, goRangeSelect];

  FEnableRowMove              := false;
  RowMoveInProgress           := false;
  LoadingFromDataSet          := false;
  LocateInProgress            := false;
end;

procedure TStringGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
   aCol, aRow : integer;
begin
  inherited MouseDown(Button, Shift, X, Y);

  Self.MouseToCell(X, Y, aCol, aRow);

  if (Self.RowCount > NUM_ROW_TITLE) and (aRow>=NUM_ROW_TITLE) then
  begin
       if FEnableRowMove then
       begin
            RowMoveInProgress := true;
            NormalCursor      := Self.Cursor;
            Self.Cursor       := crDrag;
       end;
  end;

  //-----------------------------------------------
  if Button = mbRight then
  begin
       Self.Row      := aRow;
  end;
  //-----------------------------------------------


  RowSelectedInMouseDown := aRow;
  if Self.FDataSet.Active then
     RowRecNoSelectedInMouseDown := Self.FDataSet.RecNo
  else
      RowRecNoSelectedInMouseDown := 0;

end;

procedure TStringGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
   x1,y1,x2,y2,iQuart : integer;
   aRow, aCol         : integer;
   aRect, bRect       : TRect;
begin

  Self.MouseToCell(X, Y, aCol, aRow);

  if FShowFilterRow then
  begin

       if (aRow = 1) then
       begin
            aRect := Self.CellRect(aCol, aRow);

            bRect.Left:=aRect.Right - 2 - 10;
            bRect.Top:=aRect.Top + 2;
            bRect.Right := aRect.Right-2;
            bRect.Bottom := aRect.Bottom - 2;

            x1:=bRect.Left;
            y1:=bRect.Top;
            x2:=bRect.Right;
            y2:=bRect.Bottom;

            if (x>=x1) and (x<=x2) then
               if (y>=y1) and (y<=y2) then
                  if Assigned(OnFilterBtnClick) then
                     OnFilterBtnClick(GetFieldNameFromColumn(aCol));
       end;

  end;

  if (Self.FEnableRowMove) and (Self.RowMoveInProgress) then
  begin
       RowMove(Self, Button, Shift, X, Y);
       Self.Cursor := NormalCursor;
  end;

  RowMoveInProgress := false;

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TStringGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
end;

procedure TStringGrid.DrawTextInCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
var
   bRect              : TRect;
   x1,y1,x2,y2        : integer;
begin
  inherited DrawTextInCell(aCol, aRow, aRect, aState);

  if FShowFilterRow then
  begin
       if (aRow = 1) then
       begin
            bRect.Left:=aRect.Right - 2 - 10;
            bRect.Top:=aRect.Top + 2;
            bRect.Right := aRect.Right-2;
            bRect.Bottom := aRect.Bottom - 2;

            x1:=bRect.Left;
            y1:=bRect.Top;
            x2:=bRect.Right;
            y2:=bRect.Bottom;
            Self.Canvas.Brush.Style := bsSolid;
            Self.Canvas.Brush.Color := clForm;
            Self.Canvas.Pen.Style   := psSolid;
            Self.Canvas.Pen.Color   := clForm;
            Self.Canvas.FillRect(x1,y1,x2,y2);
            Self.Canvas.TextRect(bRect, bRect.Left, bRect.Top , '...' );
       end;

  end;

end;

procedure TStringGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
begin
     inherited DrawCell(ACol, ARow, ARect, aState);
end;

procedure TStringGrid.DrawRow(aRow: Integer);
begin
     inherited DrawRow(aRow);
end;

function TStringGrid.LoadFromDataSet(ShowFiltersRow: boolean; ShowHeaderRow: boolean): boolean;
var
   i, j        : integer;
   idx         : integer;
   cell_value  : string;
   is_combo    : boolean;
begin
     LoadingFromDataSet    := true;

     NUM_ROW_TITLE         := 1;
     FShowFilterRow        := ShowFiltersRow;
     FShowHeaderRow        := ShowHeaderRow;

     if ShowFiltersRow then
        Inc(NUM_ROW_TITLE);

     if ShowHeaderRow then
        Inc(NUM_ROW_TITLE);

     if Assigned(FDataSet) then
     begin
          result                := true;
          FErrorLoadFromDataSet := '';
          try
             try
                FDataSet.DisableControls;

                result := Self.Init();
                if result then
                begin
                   result := Self.LoadTitleFromDS();
                   if result then
                   begin
                        if FDataSet.RecordCount>0 then
                        begin
                             FDataSet.First;
                             while not FDataSet.EOF do
                             begin
                                  j          := 0;
                                  for i := 0 to FDataSet.Fields.Count-1 do
                                  begin


                                       Self.Cells[j, FDataSet.RecNo-1+NUM_ROW_TITLE] := FDataSet.Fields[i].AsString;

                                       Inc(j);

                                       idx := Self.SearchColumnsProperty(FDataSet.Fields[i].FieldName);
                                       if idx>=0 then
                                       begin

                                            if Assigned(ColumnsProperty[idx].dataset) then
                                            begin
                                                 if (trim(ColumnsProperty[idx].field_key)<>'') and (ColumnsProperty[idx].dataset.FindField(ColumnsProperty[idx].field_key)<>nil) then
                                                 begin
                                                      if (trim(ColumnsProperty[idx].field_value)<>'') and (ColumnsProperty[idx].dataset.FindField(ColumnsProperty[idx].field_value)<>nil) then
                                                      begin
                                                           cell_value := GetValueIntoDataSet(ColumnsProperty[idx].dataset, ColumnsProperty[idx].field_key, ColumnsProperty[idx].field_value, FDataSet.Fields[i].AsString);
                                                           if Self.Cells[j, FDataSet.RecNo-1+NUM_ROW_TITLE] <> cell_value then
                                                           begin
                                                                Self.Cells[j, FDataSet.RecNo-1+NUM_ROW_TITLE] := cell_value;
                                                           end;

                                                           Inc(j);
                                                      end;
                                                 end;
                                            end;

                                       end;



                                  end;

                                  FDataSet.Next;
                             end;
                             j := 0;
                             for i := 0 to FDataSet.Fields.Count - 1 do
                             begin

                                  idx := Self.SearchColumnsProperty(FDataSet.Fields[i].FieldName);
                                  if idx>=0 then
                                  begin
                                       Self.Columns[j].Visible   := ColumnsProperty[idx].visible;
                                       Self.Columns[j].Alignment := ColumnsProperty[idx].alignment;
                                       if ColumnsProperty[idx].visible then
                                       begin
                                            if ColumnsProperty[idx].width>=0 then
                                               Self.Columns[j].Width     := ColumnsProperty[idx].width
                                            else
                                              Self.AutoAdjustColumn(j);
                                       end;


                                       is_combo := false;
                                       if Assigned(ColumnsProperty[idx].dataset) then
                                       begin
                                            if (trim(ColumnsProperty[idx].field_key)<>'') and (ColumnsProperty[idx].dataset.FindField(ColumnsProperty[idx].field_key)<>nil) then
                                            begin
                                                 if (trim(ColumnsProperty[idx].field_value)<>'') and (ColumnsProperty[idx].dataset.FindField(ColumnsProperty[idx].field_value)<>nil) then
                                                 begin

                                                      Self.Columns[j].Visible := false;

                                                      Inc(j);
                                                      is_combo := true;

                                                      Self.Columns[j].Visible   := ColumnsProperty[idx].visible;
                                                      Self.Columns[j].Alignment := ColumnsProperty[idx].alignment;
                                                      if ColumnsProperty[idx].visible then
                                                      begin
                                                           if ColumnsProperty[idx].width>=0 then
                                                              Self.Columns[j].Width     := ColumnsProperty[idx].width
                                                           else
                                                             Self.AutoAdjustColumn(j);
                                                      end;

                                                 end;
                                            end;
                                       end;
                                       if (not is_combo) then
                                       begin
                                            if (ColumnsProperty[idx].is_checkbox) and (trim(ColumnsProperty[idx].check_value)<>'') and (trim(ColumnsProperty[idx].uncheck_value)<>'') then
                                            begin
                                                 Self.Columns[j].ButtonStyle    := cbsCheckboxColumn;
                                                 Self.Columns[j].ValueChecked   := ColumnsProperty[idx].check_value;
                                                 Self.Columns[j].ValueUnChecked := ColumnsProperty[idx].uncheck_value;
                                            end;
                                       end;




                                  end;

                                  Inc(j);
                             end;

                        end;
                   end;
                end;
             finally
                    FDataSet.EnableControls;
             end;
          except
                on E: Exception do
                begin

                   result                := false;
                   FErrorLoadFromDataSet := E.message;

                end;
          end;
     end else begin
          result := false;
     end;
     LoadingFromDataSet := false;
end;

function TStringGrid.GetColumnWidth(fieldname: string): integer;
var
   idx  : integer;
   i    : integer;
   j    : integer;
   exit : boolean;
begin
     result := -1;
     i      := 0;
     j      := 0;
     exit   := false;
     while (i<Self.DataSet.Fields.Count) and (not exit) do
     begin

          idx := Self.SearchColumnsProperty(FDataSet.Fields[i].FieldName);
          if idx>=0 then
          begin

               if Assigned(ColumnsProperty[idx].dataset) then
               begin
                    if (trim(ColumnsProperty[idx].field_key)<>'') and (ColumnsProperty[idx].dataset.FindField(ColumnsProperty[idx].field_key)<>nil) then
                    begin
                         if (trim(ColumnsProperty[idx].field_value)<>'') and (ColumnsProperty[idx].dataset.FindField(ColumnsProperty[idx].field_value)<>nil) then
                         begin
                              Inc(j);
                         end;
                    end;
               end;

          end;

          if UpperCase(trim(Self.DataSet.Fields[i].FieldName)) = UpperCase(trim(FieldName)) then
          begin

                result := Self.Columns[j].Width;
                exit   := true;

          end else begin
              Inc(i);
              Inc(j);
          end;
     end;
end;

function TStringGrid.GetFieldNameFromColumn(aCol: integer): string;
var
   exit : boolean;
   i, j : integer;
   idx  : integer;
begin
     result := '';
     exit   := false;
     i      := 0;
     j      := 0;
     while (i<Self.Columns.Count) and (not exit) do
     begin

          idx := Self.SearchColumnsProperty(FDataSet.Fields[i-j].FieldName);
          if idx>=0 then
          begin

               if Assigned(ColumnsProperty[idx].dataset) then
               begin
                    if (trim(ColumnsProperty[idx].field_key)<>'') and (ColumnsProperty[idx].dataset.FindField(ColumnsProperty[idx].field_key)<>nil) then
                    begin
                         if (trim(ColumnsProperty[idx].field_value)<>'') and (ColumnsProperty[idx].dataset.FindField(ColumnsProperty[idx].field_value)<>nil) then
                         begin
                              if aCol = i+1 then
                              begin
                                   result := Self.DataSet.Fields[i-j].FieldName;
                                   exit   := true;
                              end;
                              Inc(j);
                              Inc(i);
                         end;
                    end;
               end;

          end;

          if not exit then
          begin
               if aCol = i then
               begin
                    result := Self.DataSet.Fields[i-j].FieldName;
                    exit   := true;
               end;
          end;

          Inc(i);
     end;

end;

procedure TStringGrid.First;
begin
     Self.Row := Self.NUM_ROW_TITLE;
end;

procedure TStringGrid.Next;
begin
     if Self.Row < Self.RowCount - 1 then
        Self.Row := Self.Row + 1;
end;

procedure TStringGrid.Prior;
begin
     if Self.Row > Self.NUM_ROW_TITLE then
        Self.Row := Self.Row - 1;
end;

procedure TStringGrid.Last;
begin
     Self.Row := Self.RowCount - 1;
end;

function TStringGrid.Locate(const KeyFields: string; const KeyValues: Variant;
  SearchOptions: TLocateOptions): boolean;
begin
     if Assigned(Self.FDataSet) then
        if Self.FDataSet.Active then
           if Self.FDataSet.RecordCount>0 then
           begin
                LocateInProgress := true;
                Self.FDataSet.Locate(KeyFields, KeyValues, SearchOptions);
                Self.Row := Self.FDataSet.RecNo + Self.NUM_ROW_TITLE - 1;
                LocateInProgress := false;
           end;
end;

end.

