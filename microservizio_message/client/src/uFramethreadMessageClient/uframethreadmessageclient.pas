unit uFramethreadMessageClient;

{$mode objfpc}{$H+}

{
    Custom option: -dUseCThreads

}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, uThreadMessageClient, Dialogs;

type
    TNewMessageEvent = procedure(Sender: TObject; TypeMessage : string; Message : string; Source : string; id_connection_source : integer) of object;
    TErrorEvent      = procedure(Sender: TObject; IdError : integer; Error : string) of object;

type

  { TFramethreadMessageClient }

  TFramethreadMessageClient = class(TFrame)
    Image1: TImage;
  private
         TThreadMessageClient1 : TThreadMessageClient;
  public
        MyCriticalSection        : TRTLCriticalSection;
        FOnNewMessage            : TNewMessageEvent;
        FOnError                 : TErrorEvent;
        MessageTypeReaded        : string;
        MessageReaded            : string;
        MessageSourceReaded      : string;
        MessageSourceIdReaded    : integer;
        LastIdError              : integer;
        LastError                : string;
        function Start(UserName: string; Password: string; url: string;
          ApplicationCustomCryptKey: string; Position: string; out
  IdError: integer; out Error: string) : boolean;
        procedure Stop;
        procedure ReadMessages;
        procedure ExecuteError;
        function SendMessage(TypeMessage : string; Message : string; Destination : string; out IdError : integer; out Error: string; IdDestination : integer=-1) : boolean;
  published
        property OnNewMessage : TNewMessageEvent read FOnNewMessage write FOnNewMessage;
        property OnError      : TErrorEvent read FOnError write FOnError;
  end;

implementation
uses
    uConsts;

{$R *.lfm}

{ TFramethreadMessageClient }

function TFramethreadMessageClient.Start(UserName: string; Password: string;
  url: string; ApplicationCustomCryptKey: string; Position: string; out
  IdError: integer; out Error: string): boolean;
begin
     IdError := ERR_GENERIC_ERROR_CODE;
     Error   := '';
     result  := false;
     {$ifndef UseCThreads}
             {$ERROR custom option -dUseCThreads not setted}
     {$else}
            if not Assigned(TThreadMessageClient1) then
            begin
                 InitCriticalSection(MyCriticalSection);
                 TThreadMessageClient1 := TThreadMessageClient.Create(TForm(Self.Parent), url, ApplicationCustomCryptKey, Position);

                 if TThreadMessageClient1.Login(UserName,Password, IdError, Error) then
                 begin
                      TThreadMessageClient1.Start;
                      result := true;
                 end else begin
                     TThreadMessageClient1.Terminate;
                     TThreadMessageClient1 := nil;
                     DoneCriticalSection(MyCriticalSection);
                 end;
            end;
     {$endif}
end;

procedure TFramethreadMessageClient.Stop;
var
  IdError : integer;
  Error   : string;
begin
     {$ifdef UseCThreads}
             if Assigned(TThreadMessageClient1) then
             begin
                  TThreadMessageClient1.LogOut(IdError, Error);
                  TThreadMessageClient1.Terminate;
                  TThreadMessageClient1 := nil;
                  DoneCriticalSection(MyCriticalSection);
             end;
     {$endif}
end;

procedure TFramethreadMessageClient.ReadMessages;
begin
     if Assigned(OnNewMessage) then
        OnNewMessage(nil, Self.MessageTypeReaded, Self.MessageReaded, Self.MessageSourceReaded,Self.MessageSourceIdReaded);
end;

procedure TFramethreadMessageClient.ExecuteError;
begin
     if Assigned(OnError) then
        OnError(nil, Self.LastIdError, Self.LastError);
end;

function TFramethreadMessageClient.SendMessage(TypeMessage: string;
  Message: string; Destination: string; out
  IdError: integer; out Error: string; IdDestination: integer): boolean;
begin
     result := false;
     {$ifdef UseCThreads}
             if Assigned(TThreadMessageClient1) then
             begin
                  result := TThreadMessageClient1.SendMessage(TypeMessage, Message, Destination, IdError,Error, IdDestination);
             end;
     {$endif}
end;

end.

