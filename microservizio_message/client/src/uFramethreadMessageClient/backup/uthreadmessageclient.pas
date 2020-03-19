unit uThreadMessageClient;

interface

uses
  Classes, sysutils, Forms, uMicroServiceMessageClient;

type

  { TThreadMessageClient }

  TThreadMessageClient = class(TThread)
  private
         FForm                      : TForm;
         MicroServiceMessageClient1 : TMicroServiceMessageClient;

         procedure NotifyMessage(MessagesList: TArrayOfItemViewMessages);
         procedure NotifyError(IdError: integer; Error: string);
  public
    constructor Create(MyForm: TForm; url: string;
      ApplicationCustomCryptKey: string; Position: string);
    Destructor Destroy; override;
    procedure Execute; override;
    function Login(UserName: string; Password: string; out IdError: integer; out
      Error: string): boolean;
    function LogOut(out IdError: integer; out
      Error: string): boolean;
    function SendMessage(TypeMessage: string; Message: string; Destination: string; out IdError: integer; out Error: string; IdDestination : integer = -1): boolean;
  end;

implementation
uses
    uFramethreadMessageClient, uConsts;

{ TEchoDaemon }

procedure TThreadMessageClient.NotifyMessage(MessagesList : TArrayOfItemViewMessages);
var
   i, j : integer;
   exit : boolean;
begin
  i    := 0;
  exit := false;
  while (i<FForm.ControlCount) and (not exit) do
  begin
       if FForm.Controls[i] is TFramethreadMessageClient then
       begin
            EnterCriticalSection(TFramethreadMessageClient(FForm.Controls[i]).MyCriticalSection);
            for j := 0 to Length(MessagesList)-1 do
            begin
                 TFramethreadMessageClient(FForm.Controls[i]).MessageTypeReaded     := MessagesList[j].typemessage;
                 TFramethreadMessageClient(FForm.Controls[i]).MessageReaded         := MessagesList[j].message;
                 TFramethreadMessageClient(FForm.Controls[i]).MessageSourceReaded   := MessagesList[j].source;
                 TFramethreadMessageClient(FForm.Controls[i]).MessageSourceIdReaded := MessagesList[j].id_connection_source;

                 Synchronize(@TFramethreadMessageClient(FForm.Controls[i]).ReadMessages);
            end;
            LeaveCriticalsection(TFramethreadMessageClient(FForm.Controls[i]).MyCriticalSection);
       end;
       Inc(i);
  end;
end;

procedure TThreadMessageClient.NotifyError(IdError : integer; Error : string);
var
   i    : integer;
   exit : boolean;
begin
  i    := 0;
  exit := false;
  while (i<FForm.ControlCount) and (not exit) do
  begin
       if FForm.Controls[i] is TFramethreadMessageClient then
       begin
            EnterCriticalSection(TFramethreadMessageClient(FForm.Controls[i]).MyCriticalSection);
            TFramethreadMessageClient(FForm.Controls[i]).LastIdError := IdError;
            TFramethreadMessageClient(FForm.Controls[i]).LastError   := Error;
            Synchronize(@TFramethreadMessageClient(FForm.Controls[i]).ExecuteError);
            LeaveCriticalsection(TFramethreadMessageClient(FForm.Controls[i]).MyCriticalSection);
       end;
       Inc(i);
  end;

end;

constructor TThreadMessageClient.Create(MyForm: TForm; url: string; ApplicationCustomCryptKey: string; Position : string);
begin
  inherited create(true);
  FForm := MyForm;
  FreeOnTerminate:=true;
  MicroServiceMessageClient1 := TMicroServiceMessageClient.Create(url, ApplicationCustomCryptKey, Position);
end;

destructor TThreadMessageClient.Destroy;
begin
     if Assigned(MicroServiceMessageClient1) then
     begin
          MicroServiceMessageClient1.Free;
          MicroServiceMessageClient1 := nil;
     end;
end;

procedure TThreadMessageClient.Execute;
var
   MessagesList : TArrayOfItemViewMessages;
   IdError      : integer;
   Error        : string;
begin

  repeat
    if terminated then break;

    if Assigned(MicroServiceMessageClient1) then
    begin

         try
            try

               SetLength(MessagesList,0);

               if MicroServiceMessageClient1.ViewMessages(MessagesList, IdError, Error) then
               begin
                    if Length(MessagesList)>0 then
                    begin

                         Self.NotifyMessage(MessagesList);

                    end;


               end else begin
                   Self.NotifyError(IdError, Error);
               end;

            finally

           end;
         except
               on E: Exception do
               begin
                    Self.NotifyError(ERR_GENERIC_ERROR_CODE, E.Message);
               end;
         end;
    end;

    Sleep(200);

  until false;

end;

function TThreadMessageClient.Login(UserName : string; Password : string; out IdError : integer; out Error: string): boolean;
begin
     result := false;
     if Assigned(MicroServiceMessageClient1) then
     begin
          if MicroServiceMessageClient1.Login(UserName,Password, IdError, Error) then
          begin
               result := true;
          end;
     end else begin
         IdError := -100;
         Error   := 'MicroServiceMessageClient1 not assigned';
     end;
end;

function TThreadMessageClient.LogOut(out IdError: integer; out Error: string
  ): boolean;
begin
  result := false;
  if Assigned(MicroServiceMessageClient1) then
  begin
       if MicroServiceMessageClient1.LogOut(IdError, Error) then
       begin
            result := true;
       end;
  end else begin
      IdError := -100;
      Error   := 'MicroServiceMessageClient1 not assigned';
  end;
end;

function TThreadMessageClient.SendMessage(TypeMessage: string; Message: string;
  Destination: string; out IdError: integer; out Error: string;
  IdDestination: integer): boolean;
begin
     result := false;
     if Assigned(MicroServiceMessageClient1) then
     begin

          if MicroServiceMessageClient1.SendMessage(TypeMessage, Message, Destination, IdDestination, IdError, Error) then
          begin
               result := true;
          end;

     end else begin
         IdError := -100;
         Error   := 'MicroServiceMessageClient1 not assigned';
     end;

end;

end.
