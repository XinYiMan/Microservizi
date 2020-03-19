unit uFrmMultiSearchConsole;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, CheckLst,
  ExtCtrls, db;

type
    ArrayOfVariant = array of variant;

type

  { TFrmMultiSearchConsole }

  TFrmMultiSearchConsole = class(TForm)
    Btn_Abort: TButton;
    Btn_Ok: TButton;
    CheckListBox_Items: TCheckListBox;
    ImgUnChkAll: TImage;
    ImgChkAll: TImage;
    Timer_Filter: TTimer;
    Txt_Filter: TEdit;
    procedure Btn_AbortClick(Sender: TObject);
    procedure Btn_OkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImgChkAllClick(Sender: TObject);
    procedure ImgUnChkAllClick(Sender: TObject);
    procedure Timer_FilterTimer(Sender: TObject);
    procedure Txt_FilterChange(Sender: TObject);
    procedure Txt_FilterKeyPress(Sender: TObject; var Key: char);
  private
         FSaved               : boolean;
         FDataSet             : TDataSet;
         FValuesSorted        : boolean;
         FResult              : ArrayOfVariant;
         Fkey_field           : string;
         Fvalue_field         : string;
         function ExistsValueInVariantArray(myarray: ArrayOfVariant;
           value_serched: variant): boolean;
         procedure SaveResult(is_updated : boolean = false);
  public
        function Saved : boolean;
        procedure LoadFromDataSet(key_field : string; value_field : string; checked_values_load : ArrayOfVariant; ValuesSorted : boolean = true);
  published
        property DataSet : TDataSet read FDataSet write FDataSet;
        property CheckedValues : ArrayOfVariant read FResult write FResult;
  end;

  TMyObjectClass = class
    value : variant;
  end;

var
  FrmMultiSearchConsole: TFrmMultiSearchConsole;

const
     FILTER_HINT = 'Filtra voci...';
     MSGABORT    = 'Annulla';
     MSGOK       = 'Ok';
     MSGERROR    = 'Errore: ';
     MSGCHKALL   = 'Seleziona tutto';
     MSGUNCHKALL = 'Deseleziona tutto';

implementation

{$R *.lfm}

{ TFrmMultiSearchConsole }

procedure TFrmMultiSearchConsole.FormCreate(Sender: TObject);
begin
     Self.Txt_Filter.Hint     := FILTER_HINT;
     Self.Txt_Filter.TextHint := FILTER_HINT;
     Self.Btn_Abort.Caption   := MSGABORT;
     Self.Btn_Ok.Caption      := MSGOK;
     Self.FSaved              := false;
     Self.ImgChkAll.Hint      := MSGCHKALL;
     Self.ImgUnChkAll.Hint    := MSGUNCHKALL;
end;

procedure TFrmMultiSearchConsole.FormDestroy(Sender: TObject);
var
   Counter : integer;
begin
  for Counter:=0 to Self.CheckListBox_Items.Items.Count-1 do
    Self.CheckListBox_Items.Items.Objects[Counter].Destroy;
  Self.CheckListBox_Items.Clear;
end;

procedure TFrmMultiSearchConsole.FormShow(Sender: TObject);
begin
     Self.Txt_Filter.SetFocus;
end;

procedure TFrmMultiSearchConsole.ImgChkAllClick(Sender: TObject);
begin
     Self.CheckListBox_Items.CheckAll(cbChecked);
end;

procedure TFrmMultiSearchConsole.ImgUnChkAllClick(Sender: TObject);
begin
     Self.CheckListBox_Items.CheckAll(cbUnChecked);
end;

procedure TFrmMultiSearchConsole.Timer_FilterTimer(Sender: TObject);
begin
     Self.Timer_Filter.Interval := 0;
     Self.SaveResult(true);
     Self.LoadFromDataSet(Fkey_field, Fvalue_field, FResult, FValuesSorted);
end;

procedure TFrmMultiSearchConsole.Txt_FilterChange(Sender: TObject);
begin
     Self.Timer_Filter.Interval := 1000;
end;

procedure TFrmMultiSearchConsole.Txt_FilterKeyPress(Sender: TObject;
  var Key: char);
begin
     Self.Timer_Filter.Interval := 0;
end;

function TFrmMultiSearchConsole.ExistsValueInVariantArray(
  myarray: ArrayOfVariant; value_serched : variant): boolean;
var
   i : integer;
begin
     result := false;
     i      := 0;
     while (i<Length(myarray)) and (not result) do
     begin

          if myarray[i] = value_serched then
             result := true
          else
              Inc(i);

     end;
end;

procedure TFrmMultiSearchConsole.SaveResult(is_updated: boolean);
var
   i : integer;
begin
     if not is_updated then
        SetLength(FResult,0);
     for i := 0 to Self.CheckListBox_Items.Items.Count-1 do
     begin
          if Self.CheckListBox_Items.Checked[i] then
          begin

               if not Self.ExistsValueInVariantArray(FResult, TMyObjectClass(Self.CheckListBox_Items.Items.Objects[i]).value) then
               begin
                    SetLength(FResult, Length(FResult)+1);
                    FResult[Length(FResult)-1] := TMyObjectClass(Self.CheckListBox_Items.Items.Objects[i]).value;
               end;

          end;
     end;
end;

function TFrmMultiSearchConsole.Saved: boolean;
begin
     result := Self.FSaved;
end;

procedure TFrmMultiSearchConsole.LoadFromDataSet(key_field: string;
  value_field: string; checked_values_load: ArrayOfVariant;
  ValuesSorted: boolean);
var
   rec_no : integer;
   myobj  : TMyObjectClass;
begin
     try
        try

           Fkey_field           := key_field;
           Fvalue_field         := value_field;
           FValuesSorted        := ValuesSorted;

           Self.CheckListBox_Items.Clear;

           if Assigned(Self.FDataSet) and (Self.FDataSet.Active) then
           begin

                if Assigned(Self.FDataSet.FindField(key_field)) and Assigned(Self.FDataSet.FindField(value_field)) then
                begin
                     Self.FDataSet.DisableControls;
                     if Self.FDataSet.RecordCount>0 then
                     begin
                          rec_no := Self.FDataSet.RecNo;
                          Self.FDataSet.First;
                          while not Self.FDataSet.EOF do
                          begin

                               if Self.CheckListBox_Items.Items.IndexOf(Self.FDataSet.FieldByName(value_field).AsString) = -1 then
                               begin

                                    if (Self.Txt_Filter.Text='') or (Pos(Self.Txt_Filter.Text, Self.FDataSet.FieldByName(value_field).AsString)>0) then
                                    begin
                                         myobj       := TMyObjectClass.Create;
                                         myobj.value := Self.FDataSet.FieldByName(key_field).AsVariant;

                                         Self.CheckListBox_Items.AddItem(Self.FDataSet.FieldByName(value_field).AsString, myobj);

                                         if Self.ExistsValueInVariantArray(checked_values_load, myobj.value) then
                                            Self.CheckListBox_Items.Checked[Self.CheckListBox_Items.Items.Count-1] := true;
                                    end;

                               end;

                               Self.FDataSet.Next;
                          end;
                          Self.FDataSet.RecNo := rec_no;
                     end;
                     Self.FDataSet.EnableControls;
                end;
           end;

        finally
          if ValuesSorted then
             Self.CheckListBox_Items.Sorted := true;
       end;
     except
           on E: Exception do
           begin
                ShowMessage(MSGERROR + E.Message);
           end;
     end;
end;

procedure TFrmMultiSearchConsole.Btn_AbortClick(Sender: TObject);
begin
     Close;
end;

procedure TFrmMultiSearchConsole.Btn_OkClick(Sender: TObject);
begin
   Self.SaveResult;
   Self.FSaved := true;
   Close;
end;

end.

