unit uGenericFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, uExtended_fphttpapp, dateutils;

  procedure MySplit (const Delimiter: Char; Input: string; const Strings: TStrings);
  function  LastCharIsDirSeparator(stringa: string): boolean;
  function  CreateDirRecursive(dir_name: string): boolean;
  procedure LogMessage(UserName : string; UnitName : string; ProcedureName : string; RemoteHost : string; message : string);
  procedure SaveIntoFile(filename : string; value : string; append_if_exist : boolean = false);
  function  LoadFromFile(filename : string; out filevalue : string) : boolean;


implementation

procedure MySplit (const Delimiter: Char; Input: string; const Strings: TStrings);
begin
   Assert(Assigned(Strings)) ;
   Strings.Clear;
   Strings.StrictDelimiter := true;
   Strings.Delimiter := Delimiter;
   Strings.DelimitedText := Input;
end;

function LastCharIsDirSeparator(stringa: string): boolean;
var
   ret: boolean;
begin
     ret:=false;
     if RightStr(stringa,1)=System.DirectorySeparator then
        ret:=true;
     result:=ret;
end;

function CreateDirRecursive(dir_name: string): boolean;
var
   app_split : TStringList;
   i         : integer;
   name      : string;
   ret       : boolean;
begin

     if trim(dir_name) <> '' then
     begin
        if not LastCharIsDirSeparator(dir_name) then
        begin
             dir_name:=dir_name + System.DirectorySeparator;
        end;
        app_split:=TStringList.Create;
        MySplit(System.DirectorySeparator,dir_name,app_split);

        name:='';
        for i:=0 to app_Split.count-1 do
        begin

             name := name + app_Split[i];
             if not LastCharIsDirSeparator(name) then
                name := name + System.DirectorySeparator;

             if not DirectoryExists(name) then
             begin
                CreateDir(name);
             end;
        end;
        app_split.Free;

        if not DirectoryExists(dir_name) then
           ret:=false
        else
            ret:=true;
     end else begin
          ret:=false;
     end;

     result:=ret;
end;

procedure LogMessage(UserName : string; UnitName: string; ProcedureName: string;
  RemoteHost: string; message: string);
begin
     if Application_with_log then
     begin
          writeln(UserName + ' -- ' + AddChar(' ',DateTimeToStr(now),16) + ': ' + UnitName + ' ' + ProcedureName + ' ' + message);
     end;
end;

procedure SaveIntoFile(filename: string; value: string; append_if_exist: boolean
  );
var
   MyFile : TStringList;
begin
     MyFile := TStringList.Create;
     if append_if_exist then
     begin
          if FileExists(filename) then
             MyFile.LoadFromFile(filename);
     end;
     MyFile.Text := MyFile.Text + value;
     if FileExists(filename) then
        DeleteFile(filename);
     MyFile.SaveToFile(filename);
     MyFile.Free;
     MyFile := nil;
end;

function LoadFromFile(filename: string; out filevalue: string): boolean;
var
   MyFile : TStringList;
   i      : integer;
begin
     result   := false;
     filevalue := '';
     MyFile := TStringList.Create;
     if FileExists(filename) then
     begin
          MyFile.LoadFromFile(filename);
          for i := 0 to MyFile.Count-1 do
              filevalue := filevalue + MyFile.Strings[i];
          result := true;
     end;
     MyFile.Free;
     MyFile := nil;
end;

end.

