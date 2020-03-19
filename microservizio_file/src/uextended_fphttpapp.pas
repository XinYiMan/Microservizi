unit uExtended_fphttpapp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpapp, INIFiles, md5, uConsts, LazFileUtils;

type
    TUserPwdItem = object
      UserName   : string;
      Password   : string;
      Admin      : boolean;
      Permission : integer; //0 write and read, 1 = only read
    end;

var
   jwt_password              : string;
   jwt_timeout               : integer;
   Application_port          : integer;
   Application_with_ssl      : boolean;
   Application_with_log      : boolean;
   ApplicationCustomCryptKey : string;
   Start_Date                : TDateTime;
   File_Root                 : string;
   Tmp_Root                  : string;
   login_sleep_ms            : integer;
   ListOfUserPwd             : array of TUserPwdItem;

procedure LoadConfigIni;
function  IsValidUser(username : string; password_hash : string; out id_user : integer) : boolean;
function  IsAdminUser(id_user: integer) : boolean;
function  UserPermission(id_user: integer) : integer;
function  UserDescription(id_user: integer) : string;

implementation



procedure LoadConfigIni;
var
   INI            : TINIFile;
   qty            : integer;
   i              : integer;
   UserItem       : string;
   PwdItem        : string;
   PermissionItem : integer;
   Admin          : boolean;
begin
     Start_Date                := now;

     INI                       := TINIFile.Create(Application.Location + 'config.ini');
     Application_port          := StrToIntDef(INI.ReadString('Config','Port','9091'),9091);
     Application_with_ssl      := StrToBoolDef(INI.ReadString('Config','ActiveSSL','FALSE'),FALSE);
     Application_with_log      := StrToBoolDef(INI.ReadString('Config','ActiveLog','FALSE'),FALSE);
     ApplicationCustomCryptKey := Trim(INI.ReadString('Config','CustomCryptKey',''));
     if trim(jwt_password) = '' then
        jwt_password := '123Password789';
     jwt_password              := INI.ReadString('Config','JWTPassword','123Password789');
     jwt_timeout               := StrToIntDef(INI.ReadString('Config','JWTTimeOut',''),(((60*24)*30)));
     login_sleep_ms            := StrToIntDef(INI.ReadString('Config','login_sleep_ms','0'),0);

     File_Root                 := INI.ReadString('Config','FileRoot','');
     if trim(File_Root) = '' then
     begin
          File_Root := Application.Location + System.DirectorySeparator + 'files' + System.DirectorySeparator;
     end else begin
         if Copy(File_Root,Length(File_Root)-1,1) <> System.DirectorySeparator then
            File_Root := File_Root + System.DirectorySeparator;
     end;
     File_Root := CleanAndExpandDirectory(File_Root);

     Tmp_Root                 := INI.ReadString('Config','TmpRoot','');
     if trim(Tmp_Root) = '' then
     begin
          Tmp_Root := Application.Location + System.DirectorySeparator + 'tmp' + System.DirectorySeparator;
     end else begin
         if Copy(Tmp_Root,Length(Tmp_Root)-1,1) <> System.DirectorySeparator then
            Tmp_Root := Tmp_Root + System.DirectorySeparator;
     end;
     Tmp_Root := CleanAndExpandDirectory(Tmp_Root);

     qty                       := StrToIntDef(INI.ReadString('Users','Count','0'),0);
     SetLength(ListOfUserPwd,0);
     if qty>0 then
     begin

          for i := 0 to qty-1 do
          begin
               UserItem       := INI.ReadString('Users','User' + IntToStr(i),'');
               PwdItem        := INI.ReadString('Users','Pwd' + IntToStr(i),'');
               PermissionItem := StrToIntDef(INI.ReadString('Users','Permission' + IntToStr(i),'1'),1);
               if PermissionItem<>0 then
                  PermissionItem := 1;
               if i = 0 then
                  Admin := true
               else
                   Admin    := false;
               if (trim(UserItem)<>'') and (trim(PwdItem)<>'') then
               begin
                    SetLength(ListOfUserPwd,Length(ListOfUserPwd)+1);
                    ListOfUserPwd[Length(ListOfUserPwd)-1].UserName   := UserItem;
                    ListOfUserPwd[Length(ListOfUserPwd)-1].Password   := PwdItem;
                    ListOfUserPwd[Length(ListOfUserPwd)-1].Admin      := Admin;
                    ListOfUserPwd[Length(ListOfUserPwd)-1].Permission := PermissionItem;
               end;
          end;
     end;
     Ini.Free;
end;

function IsValidUser(username: string; password_hash: string; out
  id_user: integer): boolean;
var
   i : integer;
begin
     id_user := 0;
     result  := false;
     i       := 0;
     while (i<Length(ListOfUserPwd)) and (not result) do
     begin
          if (ListOfUserPwd[i].UserName=username) and (MD5Print(MD5String(ListOfUserPwd[i].Password))=password_hash) then
          begin
               id_user := i + 1; //perchè i parte da 1
               result := true
          end
          else
              Inc(i);
     end;
end;

function IsAdminUser(id_user: integer): boolean;
begin
     if id_user<=Length(ListOfUserPwd) then
        result := ListOfUserPwd[id_user-1].Admin
     else
         result := false;
end;

function UserPermission(id_user: integer): integer;
begin
     if id_user<=Length(ListOfUserPwd) then
        result := ListOfUserPwd[id_user-1].Permission
     else
         result := 1;
end;

function UserDescription(id_user: integer): string;
begin
     if id_user<=Length(ListOfUserPwd) then
        result := ListOfUserPwd[id_user-1].UserName
     else
         result := '';
end;

end.

