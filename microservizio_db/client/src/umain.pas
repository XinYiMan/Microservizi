unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BufDataset, DB, Forms, Controls, Graphics, Dialogs,
  StdCtrls, DBGrids, Grids, uMicroServiceDBClient, uJsonToBufferDataset,
  uExtendedStringGrid,uGetHtml;

type

  { TForm1 }

  TForm1 = class(TForm)
    BufDataset1: TBufDataset;
    Button1: TButton;
    Button10: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Txt_CustomPassword: TEdit;
    Txt_Field: TEdit;
    Txt_Table: TEdit;
    Txt_Condition: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    StringGrid1: TStringGrid;
    procedure Button10Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
         MicroServiceDBClient1 : TMicroServiceDBClient;
  public

  end;

var
  Form1: TForm1;

implementation
uses
    uConsts;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin

     Self.Memo1.Clear;
     if not Assigned(MicroServiceDBClient1) then
     begin
          MicroServiceDBClient1 := TMicroServiceDBClient.Create(Self.Edit1.Text,Txt_CustomPassword.Text);
     end;
     if MicroServiceDBClient1.GetInfo then
     begin
          MicroServiceDBClient1.GetServerInfoForPrint(Self.Memo1.Lines);
     end;

end;

procedure TForm1.Button10Click(Sender: TObject);
var
   IdError : integer;
   Error   : string;
   value   : TDateTime;
begin

     Self.Memo1.Clear;
     if not Assigned(MicroServiceDBClient1) then
     begin
          MicroServiceDBClient1 := TMicroServiceDBClient.Create(Self.Edit1.Text,Txt_CustomPassword.Text);
     end;
     value := MicroServiceDBClient1.Current_TimeStamp(IdError, Error, now);
     if IdError = ERR_NO_ERROR_CODE then
     begin
          Self.Memo1.Append('Date time from server: ' + DateTimeToStr(value));
     end else begin
         Self.Memo1.Append(IntToStr(IdError) + ': ' + Error);
     end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
   IdError : integer;
   Error   : string;
begin

     Self.Memo1.Clear;
     if not Assigned(MicroServiceDBClient1) then
     begin
          MicroServiceDBClient1 := TMicroServiceDBClient.Create(Self.Edit1.Text,Txt_CustomPassword.Text);
     end;
     if MicroServiceDBClient1.Login('root','toor', IdError, Error) then
     begin
          Self.Memo1.Append('Login ok');
     end else begin
         Self.Memo1.Append(IntToStr(IdError) + ': ' + Error);
     end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
   IdError    : integer;
   Error      : string;
   json       : string;
   fields_str : string;
   appBuf     : TJsonToBufferDataSet;
begin

     Self.Memo1.Clear;
     if not Assigned(MicroServiceDBClient1) then
     begin
          MicroServiceDBClient1 := TMicroServiceDBClient.Create(Self.Edit1.Text,Txt_CustomPassword.Text);
     end;
     json := MicroServiceDBClient1.Select(Self.Edit2.text, fields_str, IdError, Error);
     if json <> '' then
     begin
          Self.Memo1.Append(DateTimeToStr(now) + ': start json conversion');

          json := '{"Fields":' + fields_str + ',"RowsCols":' + json + '}';

          //Self.Memo1.Append(json);

          Self.BufDataset1.DisableControls;
          appBuf := TJsonToBufferDataSet.Create(Self.BufDataset1);
          appBuf.DestroyStructure;
          appBuf.CreateStructureFromJsonString(json, Error);
          Self.Memo1.Append(Error);
          appBuf.LoadFromJsonString(json, Error);
          Self.Memo1.Append(Error);
          appBuf.Free;
          appBuf := nil;
          Self.BufDataset1.EnableControls;

          Self.Memo1.Append(DateTimeToStr(now) + ': end json conversion');

          Self.Memo1.Append('Record count: ' + IntToStr(Self.BufDataset1.RecordCount));

          Self.Memo1.Append(DateTimeToStr(now) + ': start LoadFromDataSet');
          Self.StringGrid1.DataSet := Self.BufDataset1;
          Self.StringGrid1.LoadFromDataSet(false,false);
          Self.Memo1.Append(DateTimeToStr(now) + ': end LoadFromDataSet');

     end else begin
         Self.Memo1.Append(IntToStr(IdError) + ': ' + Error);
     end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
   IdError    : integer;
   Error      : string;
begin

     Self.Memo1.Clear;
     if not Assigned(MicroServiceDBClient1) then
     begin
          MicroServiceDBClient1 := TMicroServiceDBClient.Create(Self.Edit1.Text,Txt_CustomPassword.Text);
     end;

     if MicroServiceDBClient1.Execute(Self.Edit3.text, IdError, Error) then
     begin
          Self.Memo1.Append('Execute ok');
     end else begin
         Self.Memo1.Append(IntToStr(IdError) + ': ' + Error);
     end;

end;

procedure TForm1.Button5Click(Sender: TObject);
var
   IdError    : integer;
   Error      : string;
begin

     Self.Memo1.Clear;
     if not Assigned(MicroServiceDBClient1) then
     begin
          MicroServiceDBClient1 := TMicroServiceDBClient.Create(Self.Edit1.Text,Txt_CustomPassword.Text);
     end;

     if MicroServiceDBClient1.Script(Self.Edit3.text, IdError, Error) then
     begin
          Self.Memo1.Append('Execute ok');
     end else begin
         Self.Memo1.Append(IntToStr(IdError) + ': ' + Error);
     end;

end;

procedure TForm1.Button6Click(Sender: TObject);
var
   IdError               : integer;
   Error                 : string;
   operating_system      : string;
   web_app_date_start    : string;
   web_app_current_date  : string;
begin
     Self.Memo1.Clear;
     if not Assigned(MicroServiceDBClient1) then
     begin
          MicroServiceDBClient1 := TMicroServiceDBClient.Create(Self.Edit1.Text,Txt_CustomPassword.Text);
     end;

     if MicroServiceDBClient1.Diagnostic(operating_system, web_app_date_start, web_app_current_date, IdError, Error) then
     begin
          Self.Memo1.Append('Operating system: ' + operating_system);
          Self.Memo1.Append('Web app start: ' + web_app_date_start);
          Self.Memo1.Append('Web app current date: ' + web_app_current_date);
     end else begin
         Self.Memo1.Append(IntToStr(IdError) + ': ' + Error);
     end;
end;

procedure TForm1.Button7Click(Sender: TObject);
var
   IdError               : integer;
   Error                 : string;
begin
     if not Assigned(MicroServiceDBClient1) then
     begin
          MicroServiceDBClient1 := TMicroServiceDBClient.Create(Self.Edit1.Text,Txt_CustomPassword.Text);
     end;

     if MicroServiceDBClient1.Manual(Self.Memo1.Lines ,IdError, Error) then
     begin

     end else begin
         Self.Memo1.Append(IntToStr(IdError) + ': ' + Error);
     end;
end;

procedure TForm1.Button8Click(Sender: TObject);
var
   IdError       : integer;
   Error         : string;
   return_value  : string;
begin

     Self.Memo1.Clear;
     if not Assigned(MicroServiceDBClient1) then
     begin
          MicroServiceDBClient1 := TMicroServiceDBClient.Create(Self.Edit1.Text,Txt_CustomPassword.Text);
     end;

     if MicroServiceDBClient1.GetFieldFromTable(Self.Txt_Field.text, Self.Txt_Table.Text, Self.Txt_Condition.Text, return_value, IdError, Error) then
     begin

          Self.Memo1.Append('return_value: ' + return_value);

     end else begin
         Self.Memo1.Append(IntToStr(IdError) + ': ' + Error);
     end;

end;

procedure TForm1.Button9Click(Sender: TObject);
begin
     Self.Memo1.Clear;
     if Assigned(MicroServiceDBClient1) then
     begin
          if not MicroServiceDBClient1.JWTIsNull then
             Self.Memo1.Append('Logout ok')
          else
              Self.Memo1.Append('No logged');
          MicroServiceDBClient1.Free;
          MicroServiceDBClient1 := nil;
     end else begin
          Self.Memo1.Append('No logged');
     end;
end;


procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     if Assigned(MicroServiceDBClient1) then
     begin
          MicroServiceDBClient1.Free;
          MicroServiceDBClient1 := nil;
     end;
end;

end.

