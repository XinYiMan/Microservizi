unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
  uExtendedStringGrid, BufDataset, db, uFrmMultiSearchConsole, uFrmTextSearch,
  uFrmNumericSearch;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    CheckBox1: TCheckBox;
    VDSet2: TBufDataset;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    VDSet: TBufDataset;
    StringGrid1: TStringGrid;
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StringGrid1Click(Sender: TObject);
    procedure StringGrid1Selection(Sender: TObject; aCol, aRow: Integer);
  private
         procedure MyFilterClick(fieldname: string);
         procedure MyRowChangePosition(Sender : TObject; GridRowBeforePosition : integer; GridRowAfterPosition : integer; RecNoBeforePosition : integer; RecNoAfterPosition : integer);
  public
        procedure LoadData;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
   ColumnsProperty : TColumnsProperty;
   i, j, k         : integer;
begin

  Self.StringGrid1.OnFilterBtnClick := @MyFilterClick;

  VDSet2.FieldDefs.Add('id',ftInteger);
  VDSet2.FieldDefs.Add('description',ftString, 50);
  VDSet2.CreateDataset;
  VDSet2.Open;
  VDSet2.Append;
  VDSet2.Fields[0].Value := 1;
  VDSet2.Fields[1].Value := 'value 1';
  VDSet2.Post;
  VDSet2.Append;
  VDSet2.Fields[0].Value := 2;
  VDSet2.Fields[1].Value := 'value 2';
  VDSet2.Post;
  VDSet2.Append;
  VDSet2.Fields[0].Value := 3;
  VDSet2.Fields[1].Value := 'value 3';
  VDSet2.Post;



  VDSet.FieldDefs.Add('Id_posto',ftInteger);
  VDSet.FieldDefs.Add('Id',ftInteger);
  VDSet.FieldDefs.Add('Id_tipo',ftInteger);
  for i := 1 to 30 do
  begin
       VDSet.FieldDefs.Add('Field_' + IntToStr(i),ftString,50);
  end;
  VDSet.FieldDefs.Add('eliminato',ftInteger);
  VDSet.CreateDataset;
  VDSet.Open;


  k := 1;
  for i := 1 to 3000 do
  begin
       VDSet.Append;
       VDSet.Fields[0].Value := k;
       VDSet.Fields[1].Value := i;
       VDSet.Fields[2].Value := k;
       if k = 3 then
          k := 1
       else
           Inc(k);
       for j := 3 to 32 do
       begin
            VDSet.Fields[j].Value := IntToStr(i) + ' ' + IntToStr(j-2);
       end;
       if (i mod 2)=0 then
          VDSet.FieldByName('eliminato').AsInteger:=1
       else
           VDSet.FieldByName('eliminato').AsInteger:=0;
       VDSet.Post;
  end;

  Self.LoadData;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
end;

procedure TForm1.StringGrid1Click(Sender: TObject);
begin
     //Self.Caption := Self.VDSet.FieldByName('ID').AsString + ' ' + Self.StringGrid1.GetFieldNameFromColumn(Self.StringGrid1.Col);
end;

procedure TForm1.StringGrid1Selection(Sender: TObject; aCol, aRow: Integer);
begin
     Self.Caption := Self.VDSet.FieldByName('ID').AsString + ' ' +  IntToStr(aRow);
end;

procedure TForm1.MyFilterClick(fieldname: string);
begin
     ShowMessage(fieldname);
end;

procedure TForm1.MyRowChangePosition(Sender: TObject;
  GridRowBeforePosition: integer; GridRowAfterPosition: integer;
  RecNoBeforePosition: integer; RecNoAfterPosition: integer);
begin
     ShowMessage(IntToStr(GridRowBeforePosition) + ' --> ' + IntToStr(GridRowAfterPosition) + '  ' + IntToStr(RecNoBeforePosition) + '-->' + IntToStr(RecNoAfterPosition));
end;

procedure TForm1.LoadData;
var
   ColumnsProperty  : TColumnsProperty;
begin
  SetLength(ColumnsProperty,4);
  with ColumnsProperty[0] do
  begin
       fieldname             := 'Id_tipo';
       visible               := true;
       width                 := -1; // -1 = auto adjust
       alignment             := taRightJustify;
       dataset               := VDSet2;
       field_key             := 'id';
       field_value           := 'description';
       field_title           := 'tipo';
       header_text           := 'my header Id_tipo';
       filter_text           := 'my filter Id_tipo';
       title_alignment       := taCenter;
  end;
  with ColumnsProperty[1] do
  begin
       fieldname             := 'Field_4';
       visible               := true;
       width                 := -1; // -1 = auto adjust
       alignment             := taRightJustify;
       field_title           := 'Campo 4';
       header_text           := 'my header 4';
       filter_text           := 'my filter 4';
       title_alignment       := taCenter;
  end;
  with ColumnsProperty[2] do
  begin
       fieldname             := 'Id_posto';
       visible               := true;
       width                 := -1; // -1 = auto adjust
       alignment             := taRightJustify;
       dataset               := VDSet2;
       field_key             := 'id';
       field_value           := 'description';
       field_title           := 'posto';
       header_text           := 'my header posto';
       filter_text           := 'my filter posto';
       title_alignment       := taCenter;
  end;
  with ColumnsProperty[3] do
  begin
       fieldname             := 'eliminato';
       visible               := true;
       width                 := -1; // -1 = auto adjust
       alignment             := taCenter;
       field_title           := 'Deleted';
       is_checkbox           := true;
       check_value           := '1';
       uncheck_value         := '0';
       header_text           := 'my header eliminato';
       filter_text           := 'my filter eliminato';
       title_alignment       := taCenter;
  end;

  //Self.StringGrid1.SelfPositioningFromDataSet := true;
  Self.StringGrid1.DataSet             := VDSet;
  Self.StringGrid1.ColumnsProperty     := ColumnsProperty;
  Self.StringGrid1.OnRowChangePosition := @MyRowChangePosition;
  Self.StringGrid1.LoadFromDataSet(true, true);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin


     Self.VDSet.RecNo := 57;
     Self.VDSet.Edit;
     Self.VDSet.FieldByName('Field_4').AsString:=DateTimeToStr(now);
     Self.VDSet.Post;

     Self.LoadData;

end;

procedure TForm1.Button10Click(Sender: TObject);
var
   app : TFrmMultiSearchConsole;
   str : string;
   i   : integer;
begin
     app            := TFrmMultiSearchConsole.Create(nil);
     app.Caption    := 'Filtro';
     app.DataSet    := Self.VDSet2;
     app.LoadFromDataSet('id','description',[1]);
     //app.DataSet := Self.VDSet;
     //app.LoadFromDataSet('id','id',[1]);
     app.ShowModal;
     if app.Saved then
     begin
          str := '';
          for i := 0 to Length(app.CheckedValues)-1 do
              str := str + ' ' + string(app.CheckedValues[i]);

          str := trim(str);
          if str<>'' then
             ShowMessage(str);
     end;
     app.Free;
     app := nil;
end;

procedure TForm1.Button11Click(Sender: TObject);
var
   app : TFrmTextSearch;
begin
     app                           := TFrmTextSearch.Create(nil);
     app.Caption                   := 'Filtro';
     app.Txt_Search.Text           := 'hello world';
     app.Cmb_Type.ItemIndex        := 1;
     app.Chk_CaseSensitive.Checked := true;
     app.ShowModal;
     if app.Saved then
     begin

          ShowMessage(app.Txt_Search.Text);

     end;
     app.Free;
     app := nil;
end;

procedure TForm1.Button12Click(Sender: TObject);
var
   app : TFrmNumericSearch;
begin
     app                           := TFrmNumericSearch.Create(nil);
     app.Caption                   := 'Filtro';
     app.Txt_Da.Text               := '20.52';
     app.Cmb_Type.ItemIndex        := 1;
     app.ShowModal;
     if app.Saved then
     begin

          ShowMessage(app.Txt_Da.Text + ' ' + app.Txt_A.Text);

     end;
     app.Free;
     app := nil;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
     Self.StringGrid1.AutoFillColumns := true;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
     Self.StringGrid1.AutoFillColumns := false;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
   a, b, c : integer;
begin
     a := Self.StringGrid1.GetColumnWidth('Field_3');
     b := Self.StringGrid1.GetColumnWidth('Field_4');
     c := Self.StringGrid1.GetColumnWidth('id_tipo');

     ShowMessage(IntToStr(a) + ' ' + IntToStr(b) + ' ' + IntToStr(c));
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
     if Self.VDSet.RecNo<Self.VDSet.RecordCount then
     begin
          Self.StringGrid1.Next;
     end else begin
          Self.StringGrid1.First;
     end;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
     if Self.VDSet.RecNo>1 then
     begin
          Self.StringGrid1.Prior;
     end else begin
          Self.StringGrid1.Last;
     end;
end;

procedure TForm1.Button7Click(Sender: TObject);
var
   ID : integer;
begin
    ID := StrToIntDef(InputBox('Locate','Insert ID to search', '0'),0);
    Self.StringGrid1.Locate('ID',ID,[]);
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
     Self.StringGrid1.First;
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
     Self.StringGrid1.Last;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
     Self.StringGrid1.EnableRowMove := Self.CheckBox1.Checked;
end;

end.

