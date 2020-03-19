unit uMemoryMessages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dateutils;

type
    TConnectionItem = object
      id           : integer;
      position     : string;
      create_time  : TDateTime;
      update_time  : TDateTime;
      logout       : boolean;
    end;

type
    TMessageItem = object
      id_connection        : integer;
      msg_text             : string;
      msg_type             : string;
      msg_destination      : string;
      msg_source           : string;
      deleted              : boolean;
      id_connection_source : integer;
    end;

type TArrayOfConnectionItem = array of TConnectionItem;
type TArrayOfMessageItem    = array of TMessageItem;

type

    { TMemoryMessages }

    TMemoryMessages = class

    public
          MyCriticalSection        : TRTLCriticalSection;

          constructor Create;
          destructor Free;
          function AddConnection(with_log: boolean; connection_position: string): integer;
          function AddMessage(id_session: integer; msgtext: string; msgtype: string;
            msgdestination: string; with_log: boolean; id_destination: integer
  ): boolean;
          function GetMessages(id_session: integer; out
            ResultMessages: TArrayOfMessageItem; with_log: boolean): boolean;
          procedure LogOut(id_session: integer);
          procedure RefreshUpdateTimeConnection(id_session: integer);
    private
           FMessages    : TArrayOfMessageItem;
           FConnections : TArrayOfConnectionItem;

           procedure PrintMessages();
           procedure PrintConnections();
           function GetNextId(): integer;
           function GetIdxFromIdSession(id_session: integer): integer;
           procedure DeleteOldMessages();
           procedure DeleteOldConnections();
           procedure SaveConnections();
           procedure LoadConnections();
    end;

CONST
     CONNECTIONS_LOG = 'connections.log';

implementation
uses
    uExtended_fphttpapp;

{ TMemoryMessages }

constructor TMemoryMessages.Create;
begin
     SetLength(FMessages,0);
     SetLength(FConnections,0);

     LoadConnections();

     InitCriticalSection(MyCriticalSection);
end;

destructor TMemoryMessages.Free;
begin
     DoneCriticalSection(MyCriticalSection);
end;

function TMemoryMessages.AddConnection(with_log: boolean; connection_position: string): integer;
var
   new_id : integer;
begin
     EnterCriticalSection(MyCriticalSection);
     try

        new_id := GetNextId();

        SetLength(FConnections,Length(FConnections)+1);
        with FConnections[Length(FConnections)-1] do
        begin
          id              := new_id;
          position        := connection_position;
          create_time     := now;
          update_time     := now;
          logout          := false;
        end;

        DeleteOldConnections();

        if with_log then
           PrintConnections();

        SaveConnections();
     finally
           LeaveCriticalsection(MyCriticalSection);
     end;
     result := new_id;
end;

function TMemoryMessages.AddMessage(id_session: integer; msgtext: string; msgtype: string;
  msgdestination: string; with_log: boolean; id_destination : integer) : boolean;
var
   myid : integer;
   i    : integer;
   ret  : boolean;
begin
     EnterCriticalSection(MyCriticalSection);
     ret := true;
     try
        myid := GetIdxFromIdSession(id_session);
        if myid<>-1 then
        begin
             for i := 0 to Length(FConnections) - 1 do
             begin
                  if i <> myid then
                  begin
                       if (id_destination = -1) or (id_destination = FConnections[i].id) then
                       begin
                            if (msgdestination = '') or (msgdestination = FConnections[i].position) then
                            begin
                                 SetLength(FMessages,Length(FMessages)+1);
                                 with FMessages[Length(FMessages)-1] do
                                 begin
                                   id_connection         := FConnections[i].id;
                                   msg_text              := msgtext;
                                   msg_type              := msgtype;
                                   msg_destination       := msgdestination;
                                   msg_source            := FConnections[myid].position;
                                   deleted               := false;
                                   id_connection_source  := FConnections[myid].id;
                                 end;
                            end;
                       end;
                  end;
             end;

             if with_log then
                PrintMessages();
        end else begin
            ret := false;
        end;

     finally
            LeaveCriticalsection(MyCriticalSection);
    end;
     result := ret;
end;

function TMemoryMessages.GetMessages(id_session: integer; out
  ResultMessages: TArrayOfMessageItem; with_log: boolean): boolean;
var
   myid : integer;
   i    : integer;
   ret  : boolean;
begin

     EnterCriticalSection(MyCriticalSection);
     ret := true;
     try

        DeleteOldMessages();
        SetLength(ResultMessages, 0);
        myid := GetIdxFromIdSession(id_session);
        if myid<>-1 then
        begin

             for i := 0 to Length(FMessages) - 1 do
             begin
                  if FMessages[i].id_connection = FConnections[myid].id then
                  begin
                       with FMessages[i] do
                       begin
                            if not deleted then
                            begin
                                 if (msg_destination = FConnections[myid].position) or (msg_destination = '') then
                                 begin
                                      SetLength(ResultMessages, Length(ResultMessages)+1);
                                      ResultMessages[Length(ResultMessages)-1].msg_text              := msg_text;
                                      ResultMessages[Length(ResultMessages)-1].msg_type              := msg_type;
                                      ResultMessages[Length(ResultMessages)-1].msg_source            := msg_source;
                                      ResultMessages[Length(ResultMessages)-1].id_connection_source  := id_connection_source;
                                 end;

                                 deleted         := true;
                            end;

                       end;
                  end;
             end;
             if with_log then
                PrintMessages();
        end else begin
            ret := false;
        end;

     finally
           LeaveCriticalsection(MyCriticalSection);
     end;
     result := ret;

end;

procedure TMemoryMessages.LogOut(id_session: integer);
var
   idx : integer;
begin
     idx := GetIdxFromIdSession(id_session);
     if idx<>-1  then
     begin
          Self.FConnections[idx].logout := true;
     end;
     Self.DeleteOldConnections();
     SaveConnections();
end;

procedure TMemoryMessages.RefreshUpdateTimeConnection(id_session: integer);
var
   idx : integer;
begin
     idx := GetIdxFromIdSession(id_session);
     if idx<>-1  then
     begin
          Self.FConnections[idx].update_time := now;
     end;
end;

procedure TMemoryMessages.PrintMessages();
var
   i : integer;
begin
     writeln('');
     writeln('---- START MESS----');
     for i := 0 to Length(FMessages) - 1 do
     begin
          writeln(IntToStr(FMessages[i].id_connection) + '-->' + FMessages[i].msg_destination + ': ' + FMessages[i].msg_text + ' [' + FMessages[i].msg_type + ']');
     end;
     writeln('---- END MESS-----');
     writeln('');
end;

procedure TMemoryMessages.PrintConnections();
var
   i : integer;
begin
     writeln('');
     writeln('---- START CONN----');
     for i := 0 to Length(FConnections) - 1 do
     begin
          writeln(IntToStr(FConnections[i].id) + ': ' + FConnections[i].position + ' --> ' + DateTimeToStr(FConnections[i].update_time));
     end;
     writeln('---- END CONN-----');
     writeln('');
end;

function TMemoryMessages.GetNextId(): integer;
var
   ret : integer;
begin
     if Length(FConnections)>0 then
     begin
        ret := FConnections[Length(FConnections)-1].id;
     end
     else
     begin
         ret := 0;
     end;
     Inc(ret);
     result := ret;
end;

function TMemoryMessages.GetIdxFromIdSession(id_session: integer
  ): integer;
var
   i : integer;
begin
     result := -1;
     i      := 0;
     while (i<Length(FConnections)) and (result = -1) do
     begin

          if FConnections[i].id = id_session then
          begin

               result := i;

          end else begin
               Inc(i);
          end;
     end;
end;

procedure TMemoryMessages.DeleteOldMessages();
var
   i   : integer;
   app : TArrayOfMessageItem;
begin
     SetLength(app, 0);
     for i := 0 to Length(FMessages) - 1 do
     begin
          if not FMessages[i].deleted then
          begin
               SetLength(app, Length(app) + 1);

               app[Length(app)-1].id_connection        := FMessages[i].id_connection;
               app[Length(app)-1].deleted              := FMessages[i].deleted;
               app[Length(app)-1].msg_destination      := FMessages[i].msg_destination;
               app[Length(app)-1].msg_source           := FMessages[i].msg_source;
               app[Length(app)-1].msg_type             := FMessages[i].msg_type;
               app[Length(app)-1].msg_text             := FMessages[i].msg_text;
               app[Length(app)-1].id_connection_source := FMessages[i].id_connection_source;
          end;
     end;
     SetLength(FMessages, 0);
     for i := 0 to Length(app) - 1 do
     begin
          SetLength(FMessages, Length(FMessages) + 1);

          FMessages[Length(FMessages)-1].id_connection   := app[i].id_connection;
          FMessages[Length(FMessages)-1].deleted         := app[i].deleted;
          FMessages[Length(FMessages)-1].msg_destination := app[i].msg_destination;
          FMessages[Length(FMessages)-1].msg_source      := app[i].msg_source;
          FMessages[Length(FMessages)-1].msg_type        := app[i].msg_type;
          FMessages[Length(FMessages)-1].msg_text        := app[i].msg_text;
          FMessages[Length(FMessages)-1].id_connection_source  := app[i].id_connection_source;
     end;
     SetLength(app, 0);
end;

procedure TMemoryMessages.DeleteOldConnections();
var
   i   : integer;
   j   : integer;
   app : TArrayOfConnectionItem;
begin
     SetLength(app, 0);
     for i := 0 to Length(FConnections) - 1 do
     begin
          if (Abs(MinutesBetween(FConnections[i].update_time, now))<=jwt_timeout) and (not FConnections[i].logout) then
          begin
               SetLength(app, Length(app) + 1);

               app[Length(app)-1].id               := FConnections[i].id;
               app[Length(app)-1].position         := FConnections[i].position;
               app[Length(app)-1].create_time      := FConnections[i].create_time;
               app[Length(app)-1].update_time      := FConnections[i].update_time;
               app[Length(app)-1].logout           := FConnections[i].logout;

          end else begin
              for j := 0 to Length(FMessages) - 1 do
              begin
                   if FMessages[j].id_connection = FConnections[i].id then
                      FMessages[j].deleted := true;
              end;
          end;
     end;
     SetLength(FConnections, 0);
     for i := 0 to Length(app) - 1 do
     begin
          SetLength(FConnections, Length(FConnections) + 1);

          FConnections[Length(FConnections)-1].id               := app[i].id;
          FConnections[Length(FConnections)-1].position         := app[i].position;
          FConnections[Length(FConnections)-1].create_time      := app[i].create_time;
          FConnections[Length(FConnections)-1].update_time      := app[i].update_time;
          FConnections[Length(FConnections)-1].logout           := app[i].logout;
     end;
     SetLength(app, 0);
     Self.DeleteOldMessages();
end;

procedure TMemoryMessages.SaveConnections();
var
   app : TStringList;
   i   : integer;
begin
     app := TStringList.Create;
     for i := 0 to Length(FConnections) - 1 do
     begin
          if (Abs(MinutesBetween(FConnections[i].update_time, now))<=jwt_timeout) and (not FConnections[i].logout) then
          begin

               app.Append(IntToStr(FConnections[i].id) + ';' + FConnections[i].position);

          end;
     end;
     if FileExists(CONNECTIONS_LOG) then
        DeleteFile(CONNECTIONS_LOG);

     if app.Count>1 then
        app.SaveToFile(CONNECTIONS_LOG);

     app.Free;
     app := nil;
end;

procedure TMemoryMessages.LoadConnections();
var
   app           : TStringList;
   i             : integer;
   read_id       : integer;
   read_position : string;
   pos_sep       : integer;
begin
     SetLength(FConnections,0);
     if FileExists(CONNECTIONS_LOG) then
     begin
          app := TStringList.Create;
          app.LoadFromFile(CONNECTIONS_LOG);

          for i := 0 to app.Count-1 do
          begin
               if trim(app[i])<>'' then
               begin
                    pos_sep := Pos(';', app[i]);
                    if pos_sep>1 then
                    begin
                         read_id      := StrToIntDef(Copy(app[i],1,pos_sep-1),-1);
                         if read_id <> -1 then
                         begin
                              if Length(app[i])>pos_sep then
                              begin
                                   read_position := Copy(app[i],pos_sep+1);

                                   SetLength(FConnections,Length(FConnections)+1);
                                   with FConnections[Length(FConnections)-1] do
                                   begin
                                     id              := read_id;
                                     position        := read_position;
                                     create_time     := now;
                                     update_time     := now;
                                     logout          := false;
                                   end;
                              end;
                         end;
                    end;
               end;
          end;

          app.Free;
          app := nil;

          Self.PrintConnections();

     end;
end;

end.

