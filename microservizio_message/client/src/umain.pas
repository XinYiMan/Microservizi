unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BufDataset, Forms, Controls, Graphics, Dialogs,
  StdCtrls, DBGrids, ExtCtrls, uMicroServiceMessageClient,
  uFramethreadMessageClient;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnSendMessage: TButton;
    Btn_Start: TButton;
    Btn_End: TButton;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    FramethreadMessageClient1: TFramethreadMessageClient;
    Timer1: TTimer;
    Txt_Destination: TEdit;
    Txt_Message: TEdit;
    Txt_TypeMessage: TEdit;
    Memo1: TMemo;
    Txt_SessionId: TEdit;
    Txt_Position: TEdit;
    Txt_CustomPassword: TEdit;
    procedure BtnSendMessageClick(Sender: TObject);
    procedure Btn_StartClick(Sender: TObject);
    procedure Btn_EndClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
         MicroServiceMessageClient1 : TMicroServiceMessageClient;
         MsgSended                  : integer;
         MsgReceved                 : integer;
  public
         procedure NewRemoteMessage(Sender: TObject; TypeMessage: string;
           Message: string; Source: string; id_connection_source : integer);
         procedure ErrorRemoteMessage(Sender: TObject; IdError: integer; Error: string);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BtnSendMessageClick(Sender: TObject);
begin
     MsgSended := 0;
     Self.Timer1.Interval := 1000;
     Self.Timer1.Enabled  := true;
end;


procedure TForm1.Btn_StartClick(Sender: TObject);
var
   IdError : integer;
   Error   : string;
begin
     if not FramethreadMessageClient1.Start('root','toor',Self.Edit1.Text,Txt_CustomPassword.Text, Self.Txt_Position.Text,IdError,Error) then
     begin
          ShowMessage(IntToStr(IdError) + ' : ' + Error);
     end;
end;

procedure TForm1.Btn_EndClick(Sender: TObject);
begin
     Self.FramethreadMessageClient1.Stop;
end;



procedure TForm1.FormCreate(Sender: TObject);
begin
     Self.FramethreadMessageClient1.OnNewMessage := @Self.NewRemoteMessage;
     Self.FramethreadMessageClient1.OnError      := @Self.ErrorRemoteMessage;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     if Assigned(MicroServiceMessageClient1) then
     begin
          MicroServiceMessageClient1.Free;
          MicroServiceMessageClient1 := nil;
     end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
   IdError : integer;
   Error   : string;
   Msg     : string;
begin
     Self.Timer1.Interval := 0;

     if Self.CheckBox1.Checked then
        Msg := '[' + IntToStr(MsgSended) + '] ' + DateTimeToStr(now)
     else
         Msg := Self.Txt_Message.Text;


     Self.Memo1.Clear;
     if Assigned(FramethreadMessageClient1) then
     begin
          if Self.FramethreadMessageClient1.SendMessage(Self.Txt_TypeMessage.Text, Msg, Self.Txt_Destination.Text, IdError, Error) then
          begin
               Self.Memo1.Append('Message sended: ' + Msg);
               Inc(MsgSended);
               Self.Caption:= IntToStr(MsgSended) + '/' + IntToStr(MsgReceved);
               Self.Invalidate;
               Application.ProcessMessages;
          end else begin
              Self.Memo1.Append(IntToStr(IdError) + ': ' + Error);
          end;
     end else begin
          Self.Memo1.Append('FramethreadMessageClient1 not assigned');
     end;

     if Self.CheckBox1.Checked then
     begin
          Self.Timer1.Interval := 200;
     end;

end;

procedure TForm1.NewRemoteMessage(Sender: TObject; TypeMessage: string;
  Message: string; Source: string; id_connection_source: integer);
begin
     Self.Memo1.Clear;
     Self.Memo1.Append('Source: ' + Source + ' [' + IntToStr(id_connection_source) + ']');
     Self.Memo1.Append('Message [' + TypeMessage + '] : ' + Message);

     Inc(MsgReceved);
     Self.Caption:= IntToStr(MsgSended) + '/' + IntToStr(MsgReceved);
     Self.Invalidate;
     Application.ProcessMessages;
end;

procedure TForm1.ErrorRemoteMessage(Sender: TObject; IdError: integer;
  Error: string);
begin
     Self.Memo1.Clear;
     Self.Memo1.Append(IntToStr(IdError) + ' : ' + Error);
end;

end.

