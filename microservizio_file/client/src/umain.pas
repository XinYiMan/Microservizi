unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BufDataset, DB, Forms, Controls, Graphics, Dialogs,
  StdCtrls, DBGrids, Grids, ComCtrls, uMicroServiceFileClient,
  uJsonToBufferDataset, uGetHtml;

type

  { TForm1 }

  TForm1 = class(TForm)
    BufDataset1: TBufDataset;
    Button1: TButton;
    BtnUploadFile: TButton;
    BtnDownloadFile: TButton;
    BtnConnect: TButton;
    BtnCreateDir: TButton;
    BtnDeleteDir: TButton;
    BtnDeleteFile: TButton;
    BtnDisconnect: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    ProgressBar1: TProgressBar;
    SaveDialog1: TSaveDialog;
    StringGrid1: TStringGrid;
    Txt_CustomPassword: TEdit;
    Memo1: TMemo;
    procedure BtnConnectClick(Sender: TObject);
    procedure BtnDeleteDirClick(Sender: TObject);
    procedure BtnDisconnectClick(Sender: TObject);
    procedure BtnUploadFileClick(Sender: TObject);
    procedure BtnDownloadFileClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BtnCreateDirClick(Sender: TObject);
    procedure BtnDeleteFileClick(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
  private
         MicroServiceFileClient1 : TMicroServiceFileClient;
  public
         procedure LoadRemoteDir(dir: string='');
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
     Self.Memo1.Clear;
     if not Assigned(MicroServiceFileClient1) then
     begin
          MicroServiceFileClient1 := TMicroServiceFileClient.Create(Self.Edit1.Text,Txt_CustomPassword.Text);
     end;
     if MicroServiceFileClient1.GetInfo then
     begin
          MicroServiceFileClient1.GetServerInfoForPrint(Self.Memo1.Lines);
     end;

end;

procedure TForm1.BtnUploadFileClick(Sender: TObject);
var
   filename : string;
   IdError  : integer;
   Error    : string;
begin

       Self.Memo1.Clear;
       if Assigned(MicroServiceFileClient1) then
       begin
            if not MicroServiceFileClient1.JWTIsNull then
            begin
                 if OpenDialog1.Execute then
                 begin
                      filename := OpenDialog1.Filename;

                      if MicroServiceFileClient1.UploadFile(filename, MicroServiceFileClient1.GetRemoteCurrentDirName, ExtractFileName(filename), IdError ,Error, Self.ProgressBar1) then
                      begin
                           LoadRemoteDir(MicroServiceFileClient1.GetRemoteCurrentDirName);
                      end else begin
                          Self.Memo1.Append(IntToStr(IdError) + ': ' + Error);
                      end;
                 end;
            end else begin
                     Self.Memo1.Append('No logged');
            end;
       end else begin
           Self.Memo1.Append('No logged');
       end;

end;

procedure TForm1.BtnConnectClick(Sender: TObject);
var
   IdError : integer;
   Error   : string;
begin
     Self.Memo1.Clear;
     if not Assigned(MicroServiceFileClient1) then
     begin
          MicroServiceFileClient1 := TMicroServiceFileClient.Create(Self.Edit1.Text,Txt_CustomPassword.Text);
     end;

     if MicroServiceFileClient1.Login('root','toor', IdError, Error) then
     begin
          LoadRemoteDir(MicroServiceFileClient1.GetRemoteCurrentDirName);
     end else begin
         Self.Memo1.Append(IntToStr(IdError) + ':' + Error);
     end;
end;

procedure TForm1.BtnDeleteDirClick(Sender: TObject);
var
   IdError               : integer;
   Error                 : string;
   dirname               : string;
   IsDir                 : boolean;
begin
     Self.Memo1.Clear;

     if Assigned(MicroServiceFileClient1) then
     begin
          if not MicroServiceFileClient1.JWTIsNull then
          begin
            IsDir   := true;
            dirname := MicroServiceFileClient1.GetRemoteCurrentDirName;
            if Self.StringGrid1.Row>2 then
            begin
                 dirname := dirname + Self.StringGrid1.Cells[0, Self.StringGrid1.Row];
                 if Self.StringGrid1.Cells[3, Self.StringGrid1.Row] = 'DIR' then
                    IsDir := true
                 else
                     IsDir := false;
            end;

            if IsDir then
            begin
              if MicroServiceFileClient1.DeleteRemoteDir(dirname, IdError, Error) then
              begin
                LoadRemoteDir(MicroServiceFileClient1.GetRemoteCurrentDirName);
              end else begin
                  Self.Memo1.Append(IntToStr(IdError) + ': ' + Error);
              end;
            end else begin
                  ShowMessage('Not directory selected.');
            end;
          end else begin
               Self.Memo1.Append('No logged');
          end;
     end else begin
         Self.Memo1.Append('No logged');
     end;
end;

procedure TForm1.BtnDisconnectClick(Sender: TObject);
begin
     Self.Memo1.Clear;
     if Assigned(MicroServiceFileClient1) then
     begin
          if not MicroServiceFileClient1.JWTIsNull then
             Self.Memo1.Append('Logout ok')
          else
              Self.Memo1.Append('No logged');
          MicroServiceFileClient1.Free;
          MicroServiceFileClient1 := nil;
     end else begin
          Self.Memo1.Append('No logged');
     end;
end;

procedure TForm1.BtnDownloadFileClick(Sender: TObject);
var
   filename : string;
   IdError  : integer;
   Error    : string;
   start    : TDateTime;
begin
     Self.Memo1.Clear;
     if Assigned(MicroServiceFileClient1) then
     begin
          if not MicroServiceFileClient1.JWTIsNull then
          begin
               if Self.StringGrid1.Row>2 then
               begin
                    if SaveDialog1.Execute then
                    begin
                      filename := SaveDialog1.Filename;

                      start := now;
                      if MicroServiceFileClient1.DownloadFile(filename, MicroServiceFileClient1.GetRemoteCurrentDirName + MicroServiceFileClient1.ServerDirSeparator + Self.StringGrid1.Cells[0, Self.StringGrid1.Row], IdError, Error, Self.ProgressBar1) then
                      begin
                           Self.Memo1.Append(DateTimeToStr(start));
                           Self.Memo1.Append('File downloaded.');
                           Self.Memo1.Append(DateTimeToStr(now));
                      end else begin
                          Self.Memo1.Append(IntToStr(IdError) + ': ' + Error);
                      end;

                    end;
               end;
          end else begin
               Self.Memo1.Append('No logged');
          end;
     end else begin
         Self.Memo1.Append('No logged');
     end;


end;

procedure TForm1.BtnCreateDirClick(Sender: TObject);
var
   IdError               : integer;
   Error                 : string;
   dirname               : string;
begin
     Self.Memo1.Clear;

     if Assigned(MicroServiceFileClient1) then
     begin
       if not MicroServiceFileClient1.JWTIsNull then
       begin
         dirname :=   InputBox('Question','Insert directory name', '');
         if dirname<> '' then
         begin

           if MicroServiceFileClient1.CreateRemoteDir(MicroServiceFileClient1.GetRemoteCurrentDirName + MicroServiceFileClient1.ServerDirSeparator + dirname + MicroServiceFileClient1.ServerDirSeparator, IdError, Error) then
           begin
             LoadRemoteDir(MicroServiceFileClient1.GetRemoteCurrentDirName);
           end else begin
               Self.Memo1.Append(IntToStr(IdError) + ': ' + Error);
           end;

         end;
       end else begin
            Self.Memo1.Append('No logged');
       end;
     end else begin
         Self.Memo1.Append('No logged');
     end;
end;

procedure TForm1.BtnDeleteFileClick(Sender: TObject);
var
   IdError               : integer;
   Error                 : string;
   filename              : string;
   IsFile                : boolean;
begin
     Self.Memo1.Clear;

     if Assigned(MicroServiceFileClient1) then
     begin
       if not MicroServiceFileClient1.JWTIsNull then
       begin
         IsFile   := false;
         filename := MicroServiceFileClient1.GetRemoteCurrentDirName;
         if Self.StringGrid1.Row>2 then
         begin
              filename := filename + Self.StringGrid1.Cells[0, Self.StringGrid1.Row];
              if Self.StringGrid1.Cells[3, Self.StringGrid1.Row] <> 'DIR' then
                 IsFile := true
              else
                  IsFile := false;
         end;

         if IsFile then
         begin
           if MicroServiceFileClient1.DeleteRemoteFile(filename, IdError, Error) then
           begin
             LoadRemoteDir(MicroServiceFileClient1.GetRemoteCurrentDirName);
           end else begin
               Self.Memo1.Append(IntToStr(IdError) + ': ' + Error);
           end;
         end else begin
               ShowMessage('Not file selected.');
         end;
       end else begin
            Self.Memo1.Append('No logged');
       end;
     end else begin
         Self.Memo1.Append('No logged');
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
     if not Assigned(MicroServiceFileClient1) then
     begin
          MicroServiceFileClient1 := TMicroServiceFileClient.Create(Self.Edit1.Text,Txt_CustomPassword.Text);
     end;

     if not MicroServiceFileClient1.JWTIsNull then
     begin
       if MicroServiceFileClient1.Diagnostic(operating_system, web_app_date_start, web_app_current_date, IdError, Error) then
       begin
            Self.Memo1.Append('Operating system: ' + operating_system);
            Self.Memo1.Append('Web app start: ' + web_app_date_start);
            Self.Memo1.Append('Web app current date: ' + web_app_current_date);
       end else begin
           Self.Memo1.Append(IntToStr(IdError) + ': ' + Error);
       end;
     end else begin
            Self.Memo1.Append('No logged');
     end;
end;

procedure TForm1.Button7Click(Sender: TObject);
var
   IdError               : integer;
   Error                 : string;
begin
     if not Assigned(MicroServiceFileClient1) then
     begin
          MicroServiceFileClient1 := TMicroServiceFileClient.Create(Self.Edit1.Text,Txt_CustomPassword.Text);
     end;

     if MicroServiceFileClient1.Manual(Self.Memo1.Lines , IdError, Error) then
     begin

     end else begin
         Self.Memo1.Append(IntToStr(IdError) + ': ' + Error);
     end;
end;

procedure TForm1.Button8Click(Sender: TObject);
var
   IdError               : integer;
   Error                 : string;
   space                 : Int64;
begin
     Self.Memo1.Clear;
     if not Assigned(MicroServiceFileClient1) then
     begin
          MicroServiceFileClient1 := TMicroServiceFileClient.Create(Self.Edit1.Text,Txt_CustomPassword.Text);
     end;

     if not MicroServiceFileClient1.JWTIsNull then
     begin
       space := MicroServiceFileClient1.RemoteFreeSpace(IdError, Error);
       if space<>-1 then
       begin

            Self.Memo1.Append('Remote free space: ' + IntToStr(space) + ' Byte');
            Self.Memo1.Append('Remote free space: ' + IntToStr(space div 1024) + ' KByte');

       end else begin
           Self.Memo1.Append(IntToStr(IdError) + ': ' + Error);
       end;
     end else begin
           Self.Memo1.Append('No logged');
     end;


end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
     if Assigned(MicroServiceFileClient1) then
     begin
          MicroServiceFileClient1.UrlServer := Edit1.Text;
     end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     if Assigned(MicroServiceFileClient1) then
     begin
          MicroServiceFileClient1.Free;
          MicroServiceFileClient1 := nil;
     end;
end;

procedure TForm1.StringGrid1DblClick(Sender: TObject);
begin
     if StringGrid1.Row>0 then
     begin

       if Assigned(MicroServiceFileClient1) then
       begin

         if Self.StringGrid1.Cells[3, StringGrid1.Row] = 'DIR' then
            LoadRemoteDir(MicroServiceFileClient1.GetRemoteCurrentDirName + MicroServiceFileClient1.ServerDirSeparator + Self.StringGrid1.Cells[0, StringGrid1.Row]);

       end;

     end;
end;

procedure TForm1.LoadRemoteDir(dir : string = '');
var
   IdError               : integer;
   Error                 : string;
   i                     : integer;
   content               : TArrayOfTMicroServiceFileIitem;
begin
     Self.Memo1.Clear;
     Self.StringGrid1.ColCount := 4;
     Self.StringGrid1.RowCount := 1;

     Self.StringGrid1.Cells[0 , 0] := 'Name';
     Self.StringGrid1.Cells[1 , 0] := 'Size';
     Self.StringGrid1.Cells[2 , 0] := 'Timestamp';
     Self.StringGrid1.Cells[3 , 0] := 'Type';


     if Assigned(MicroServiceFileClient1) then
     begin
       if MicroServiceFileClient1.RemoteDirContent(dir, content , IdError, Error) then
       begin

            Self.StringGrid1.RowCount := Self.StringGrid1.RowCount + 1;
            Self.StringGrid1.Cells[0 , 1] := '.';
            Self.StringGrid1.Cells[1 , 1] := '0 byte';
            Self.StringGrid1.Cells[2 , 1] := '';
            Self.StringGrid1.Cells[3 , 1] := 'DIR';

            Self.StringGrid1.RowCount := Self.StringGrid1.RowCount + 1;
            Self.StringGrid1.Cells[0 , 2] := '..';
            Self.StringGrid1.Cells[1 , 2] := '0 byte';
            Self.StringGrid1.Cells[2 , 2] := '';
            Self.StringGrid1.Cells[3 , 2] := 'DIR';

            for i := 0 to Length(content)-1 do
            begin

                 Self.StringGrid1.RowCount := Self.StringGrid1.RowCount + 1;
                 Self.StringGrid1.Cells[0 , i + 3] := content[i].FileName;
                 Self.StringGrid1.Cells[1 , i + 3] := IntToStr(content[i].FileSize) + ' byte';
                 Self.StringGrid1.Cells[2 , i + 3] := DateTimeToStr(content[i].TimeStamp);
                 if content[i].IsDir then
                    Self.StringGrid1.Cells[3 , i + 3] := 'DIR';

            end;

       end else begin
           Self.Memo1.Append(IntToStr(IdError) + ': ' + Error);
       end;
       Self.Label1.Caption:=MicroServiceFileClient1.GetRemoteCurrentDirName;
     end;

     Self.StringGrid1.AutoAdjustColumns;
end;

end.

