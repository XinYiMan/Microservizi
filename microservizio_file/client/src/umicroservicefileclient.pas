unit uMicroServiceFileClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,fpjson, jsonparser, uConsts, md5, ugethtml, uCryptFunction, strutils, uDateTimeToStr,
  //Per gestire progress bar non android
  ComCtrls, Forms;

type
    TMicroServiceFileInformation = object
          web_app_title        : String;
          web_app_versione     : String;
          web_app_author       : String;
    end;
type
    TMicroServiceFileIitem = object
          FileName     : String;
          FileSize     : Integer;
          TimeStamp    : TDateTime;
          IsDir        : Boolean;
    end;
type
    TArrayOfTMicroServiceFileIitem = array of TMicroServiceFileIitem;

type

    { TMicroServiceFileClient }

    TMicroServiceFileClient = class
     private
           Info                       : TMicroServiceFileInformation;
           Furl_server                : String;
           jwt                        : String;
           FServerDirSeparator        : string;
           FApplicationCustomCryptKey : string;
           current_dirname            : string;
           function ValidTitleAndVersion(server_result: String) : boolean;
           function RecuperaHTML(url : String; postItems: TArrayOfPostItem) : String;
           function CustomDecrypt(value: string): string;
           function CustomCrypt(value: string): string;
           function GetDirSeparatorFromServer(out IdError: integer; out Error: string
             ): boolean;
           function StringHash(value: string): string;
     public
           constructor Create(url : string; ApplicationCustomCryptKey : string);
           destructor Free;
           function GetInfo: boolean;
           function Login(user: String; password: String; out IdError : integer; out Error: String): boolean;
           function JWTIsNull : boolean;
           procedure GetServerInfoForPrint(value: TStrings);
           function Manual(value: TStrings; out IdError : integer; out Error: string): boolean;
           function Diagnostic(out operating_system: string; out
             web_app_date_start: string; out web_app_current_date : string; out IdError : integer; out Error: String): boolean;
           function RemoteFileExists(filename : string; out IdError : integer; out Error: string) : boolean;
           function RemoteDirExists(dirname: string; out IdError : integer; out Error: string): boolean;
           function CreateRemoteDir(dirname: string; out IdError : integer; out Error: string): boolean;
           function DeleteRemoteDir(dirname: string; out IdError : integer; out Error: string): boolean;
           function DeleteRemoteFile(filename: string; out IdError : integer; out Error: string): boolean;
           function RemoteFreeSpace(out IdError : integer; out Error: string): Int64;
           function RemoteDirContent(dirname : string;out content : TArrayOfTMicroServiceFileIitem; out IdError : integer; out Error : string) : boolean;
           function UploadFile(local_file : string; remote_dir : string; remote_filename : string; out IdError : integer; out Error : string; out ProgressBar : TProgressBar) : boolean;
           function DownloadFile(local_file: string; remote_filename: string; out IdError : integer; out
             Error: string; out ProgressBar: TProgressBar): boolean;
           function GetRemoteCurrentDirName : string;
     published
           property ServerDirSeparator : string read FServerDirSeparator;
           property UrlServer : string read Furl_server write Furl_server;
    end;

const
     VALID_WEB_APP_VERSION  = '0.0.1';
     VALID_WEB_APP_TITLE    = 'ngit_microservice_file';

implementation
uses
    base64, fpjsonrtti, variants, ufunctionbase64;

{ TMicroServiceFileClient }

function TMicroServiceFileClient.ValidTitleAndVersion(server_result: String
  ): boolean;
var
   resultjson : TJSONData;
   item       : TJSONData;
   ret        : boolean;
   tit, ver   : String;
begin
     try
        try

           ret := false;
           if trim(server_result)<>'' then
           begin
             resultjson := GetJSON(server_result);

             tit := '';
             item:=resultjson.FindPath('web_app_title');
             if (item <> nil) and (not item.IsNull) then
                tit := item.Value;

             ver := '';
             item:=resultjson.FindPath('web_app_version');
             if (item <> nil) and (not item.IsNull) then
                ver := item.Value;

             if (tit = VALID_WEB_APP_TITLE) and (ver = VALID_WEB_APP_VERSION) then
             begin
                  Info.web_app_title       := tit;
                  Info.web_app_versione    := ver;

                  item:=resultjson.FindPath('web_app_author');
                  if (item <> nil) and (not item.IsNull) then
                     Info.web_app_author   := item.Value;

                  ret := true;
             end else begin
               Info.web_app_title       := '';
               Info.web_app_versione    := '';
               Info.web_app_author      := '';
             end;


           end;

        finally
                if Assigned(resultjson) then
                begin
                   resultjson.Free;
                   resultjson := nil;
                end;
       end;
     except
           on E: Exception do
           begin

                Info.web_app_title       := '';
                Info.web_app_versione    := '';
                Info.web_app_author      := '';
                ret                      := false;
           end;
     end;

     result := ret;
end;

function TMicroServiceFileClient.RecuperaHTML(url: String;
  postItems: TArrayOfPostItem): String;
var
   app : string;
begin
     app    := GetHTML(url, postItems);
     result := CustomDeCrypt(app);
end;

constructor TMicroServiceFileClient.Create(url: string;
  ApplicationCustomCryptKey: string);
begin
     Self.FApplicationCustomCryptKey := ApplicationCustomCryptKey;
     Self.Furl_server                := url;
end;

destructor TMicroServiceFileClient.Free;
begin

end;

function TMicroServiceFileClient.GetInfo: boolean;
var
   resultjson            : TJSONData;
   result_server         : String;
begin
     try
        try

           result   := false;


           result_server  := RecuperaHtml(Self.Furl_server + 'info',[]);
           if ValidTitleAndVersion(result_server) then
           begin
                result := true;
           end;

        finally

        end;
     except
           on E: Exception do
           begin

                  result   := false;

           end;
     end;
end;

function TMicroServiceFileClient.Login(user: String; password: String; out
  IdError: integer; out Error: String): boolean;
var
   resultjson            : TJSONData;
   item                  : TJSONData;
   result_server         : String;
   ret                   : boolean;
   error_number          : integer;
   error_description     : String;
begin
     try
        try

           IdError := ERR_GENERIC_ERROR_CODE;
           ret     := false;
           Error   := '';



           result_server  := RecuperaHtml(Self.Furl_server + 'login?username=' + CustomCrypt(user) + '&pwd_hash=' + CustomCrypt(Self.StringHash(password)),[]);

           if trim(result_server) = '' then
           begin
                IdError := ERR_SERVER_UNREACHABLE_CODE;
                Error   := ERR_SERVER_UNREACHABLE_DESC;
           end else begin
                       if ValidTitleAndVersion(result_server) then
                       begin

                         resultjson := GetJSON(result_server);

                         item:=resultjson.FindPath('error_code');
                         if (item <> nil) and (not item.IsNull) then
                         begin
                              error_number := StrToIntDef(Item.value, 0);
                              IdError      := error_number;
                              item:=resultjson.FindPath('error_desc');
                              if (item <> nil) and (not item.IsNull) then
                              begin
                                   error_description := Item.value;

                                   if error_number<>ERR_NO_ERROR_CODE then
                                   begin
                                        Error := error_description;
                                   end else begin
                                          item:=resultjson.FindPath('jwt');
                                          if (item <> nil) and (not item.IsNull) then
                                          begin
                                               jwt := Item.value;

                                               if trim(jwt) <> '' then
                                               begin
                                                  ret := true
                                               end
                                               else
                                               begin
                                                   IdError := ERR_INVALID_JWT_CODE;
                                                   Error   := ERR_INVALID_JWT_DESC;
                                               end;
                                          end else begin
                                               IdError := ERR_INVALID_HTML_CODE;
                                               Error   := ERR_INVALID_HTML_DESC;
                                          end;

                                   end;

                              end else begin
                                   IdError := ERR_INVALID_HTML_CODE;
                                   Error   := ERR_INVALID_HTML_DESC;
                              end;

                              IdError := error_number;
                         end else begin
                              Error   := ERR_INVALID_HTML_DESC;
                              IdError := ERR_INVALID_HTML_CODE;
                         end;


                       end else begin
                           Error   := ERR_INVALID_HTML_DESC;
                           IdError := ERR_INVALID_HTML_CODE;
                       end;
           end;

        finally
                if ret then
                begin
                     ret := GetDirSeparatorFromServer(IdError, Error);
                end;
                if Assigned(resultjson) then
                begin
                   resultjson.Free;
                   resultjson := nil;
                end;
        end;
     except
           on E: Exception do
           begin

                  IdError := ERR_GENERIC_ERROR_CODE;
                  Error   := E.Message;
                  ret     := false;

           end;
     end;


     result := ret;

end;

function TMicroServiceFileClient.JWTIsNull: boolean;
begin
  if trim(Self.jwt) = '' then
     result := true
  else
      result := false;
end;

procedure TMicroServiceFileClient.GetServerInfoForPrint(value: TStrings);
begin
  try
     try

        if assigned(value) then
        begin
             value.Append('web_app_title: ' + Self.Info.web_app_title);
             value.Append('web_app_versione: ' + Self.Info.web_app_versione);
             value.Append('web_app_author: ' + Self.Info.web_app_author);
        end;

     finally
    end;
  except
        on E: Exception do
        begin


        end;
  end;
end;

function TMicroServiceFileClient.Manual(value: TStrings; out IdError: integer;
  out Error: string): boolean;
var
   resultjson            : TJSONData;
   item                  : TJSONData;
   result_server         : String;
   error_number          : integer;
   error_description     : String;
begin
     try
        try

           result   := false;
           IdError  := ERR_GENERIC_ERROR_CODE;

           result_server  := RecuperaHtml(Self.Furl_server + 'manual',[]);

           if trim(result_server) = '' then
           begin
                IdError := ERR_SERVER_UNREACHABLE_CODE;
                Error   := ERR_SERVER_UNREACHABLE_DESC;
           end else begin
                       if ValidTitleAndVersion(result_server) then
                       begin

                            resultjson := GetJSON(result_server);

                            item:=resultjson.FindPath('error_code');
                            if (item <> nil) and (not item.IsNull) then
                            begin
                                 error_number := StrToIntDef(Item.value, 0);
                                 IdError      := error_number;

                                 item:=resultjson.FindPath('error_desc');
                                 if (item <> nil) and (not item.IsNull) then
                                 begin
                                      error_description := Item.value;

                                      if error_number<>ERR_NO_ERROR_CODE then
                                      begin
                                           Error := error_description;
                                      end else begin

                                             value.Clear;

                                             item:=resultjson.FindPath('login_page');
                                             if (item <> nil) and (not item.IsNull) then
                                             begin
                                                  value.Add('login_page = ' + Item.value);
                                                  value.Add('');
                                             end;

                                             item:=resultjson.FindPath('info_page');
                                             if (item <> nil) and (not item.IsNull) then
                                             begin
                                                  value.Add('info_page = ' + Item.value);
                                                  value.Add('');
                                             end;

                                             item:=resultjson.FindPath('dirseparator_page');
                                             if (item <> nil) and (not item.IsNull) then
                                             begin
                                                value.Add('dirseparator_page = ' + Item.value);
                                                value.Add('');
                                             end;

                                             item:=resultjson.FindPath('direxists_page');
                                             if (item <> nil) and (not item.IsNull) then
                                             begin
                                                value.Add('direxists_page = ' + Item.value);
                                                value.Add('');
                                             end;

                                             item:=resultjson.FindPath('fileexists_page');
                                             if (item <> nil) and (not item.IsNull) then
                                             begin
                                                value.Add('fileexists_page = ' + Item.value);
                                                value.Add('');
                                             end;

                                             item:=resultjson.FindPath('diagnostic_page');
                                             if (item <> nil) and (not item.IsNull) then
                                             begin
                                                value.Add('diagnostic_page = ' + Item.value);
                                                value.Add('');
                                             end;

                                             item:=resultjson.FindPath('createdir_page');
                                             if (item <> nil) and (not item.IsNull) then
                                             begin
                                                value.Add('createdir_page = ' + Item.value);
                                                value.Add('');
                                             end;

                                             item:=resultjson.FindPath('deletedir_page');
                                             if (item <> nil) and (not item.IsNull) then
                                             begin
                                                value.Add('deletedir_page = ' + Item.value);
                                                value.Add('');
                                             end;

                                             item:=resultjson.FindPath('dircontent_page');
                                             if (item <> nil) and (not item.IsNull) then
                                             begin
                                                value.Add('dircontent_page = ' + Item.value);
                                                value.Add('');
                                             end;

                                             item:=resultjson.FindPath('deletefile_page');
                                             if (item <> nil) and (not item.IsNull) then
                                             begin
                                                value.Add('deletefile_page = ' + Item.value);
                                                value.Add('');
                                             end;

                                             item:=resultjson.FindPath('freediskspace_page');
                                             if (item <> nil) and (not item.IsNull) then
                                             begin
                                                value.Add('freediskspace_page = ' + Item.value);
                                                value.Add('');
                                             end;

                                             item:=resultjson.FindPath('downloadfile_page');
                                             if (item <> nil) and (not item.IsNull) then
                                             begin
                                                value.Add('downloadfile_page = ' + Item.value);
                                                value.Add('');
                                             end;

                                             item:=resultjson.FindPath('uploadfile_page');
                                             if (item <> nil) and (not item.IsNull) then
                                             begin
                                                value.Add('uploadfile_page = ' + Item.value);
                                                value.Add('');
                                             end;

                                             item:=resultjson.FindPath('manual_page');
                                             if (item <> nil) and (not item.IsNull) then
                                             begin
                                                value.Add('manual_page = ' + Item.value);
                                                value.Add('');
                                             end;

                                             result := true;
                                      end;

                                 end else begin
                                      IdError  := ERR_INVALID_HTML_CODE;
                                      Error    := ERR_INVALID_HTML_DESC;
                                 end;

                            end else begin
                                 IdError := ERR_INVALID_HTML_CODE;
                                 Error   := ERR_INVALID_HTML_DESC;
                            end;

                       end else begin
                                 IdError := ERR_INVALID_HTML_CODE;
                                 Error   := ERR_INVALID_HTML_DESC;
                       end;
           end;


        finally
                if Assigned(resultjson) then
                begin
                   resultjson.Free;
                   resultjson := nil;
                end;
        end;
     except
           on E: Exception do
           begin

                  IdError  := ERR_GENERIC_ERROR_CODE;
                  result   := false;

           end;
     end;
end;

function TMicroServiceFileClient.Diagnostic(out operating_system: string; out
  web_app_date_start: string; out web_app_current_date: string; out
  IdError: integer; out Error: String): boolean;
var
   resultjson            : TJSONData;
   item                  : TJSONData;
   result_server         : String;
   error_number          : integer;
   error_description     : String;
begin
     try
        try

           IdError               := ERR_GENERIC_ERROR_CODE;
           result                := false;
           Error                 := '';
           operating_system      := '';
           web_app_date_start    := '';
           web_app_current_date  := '';

           result_server  := RecuperaHtml(Self.Furl_server + 'diagnostic?mytoken=' + CustomCrypt(Self.jwt) ,[]);

           if trim(result_server) = '' then
           begin
                IdError := ERR_SERVER_UNREACHABLE_CODE;
                Error   := ERR_SERVER_UNREACHABLE_DESC;
           end else begin
                       if ValidTitleAndVersion(result_server) then
                       begin


                         resultjson := GetJSON(result_server);

                         item:=resultjson.FindPath('error_code');
                         if (item <> nil) and (not item.IsNull) then
                         begin
                              error_number := StrToIntDef(Item.value, 0);
                              IdError      := error_number;

                              item:=resultjson.FindPath('error_desc');
                              if (item <> nil) and (not item.IsNull) then
                              begin
                                   error_description := Item.value;

                                   if error_number<>ERR_NO_ERROR_CODE then
                                   begin
                                        Error := error_description;
                                   end else begin
                                          item:=resultjson.FindPath('jwt');
                                          if (item <> nil) and (not item.IsNull) then
                                          begin
                                               jwt := Item.value;

                                               if trim(jwt) <> '' then
                                               begin

                                                    result := true;

                                                    item:=resultjson.FindPath('operating_system');
                                                    if (item <> nil) and (not item.IsNull) then
                                                    begin
                                                         operating_system := Item.value;
                                                    end else begin
                                                         IdError := ERR_INVALID_HTML_CODE;
                                                         Error   := ERR_INVALID_HTML_DESC;
                                                    end;

                                                    item:=resultjson.FindPath('web_app_date_start');
                                                    if (item <> nil) and (not item.IsNull) then
                                                    begin
                                                         web_app_date_start := Item.value;
                                                    end else begin
                                                         IdError := ERR_INVALID_HTML_CODE;
                                                         Error   := ERR_INVALID_HTML_DESC;
                                                    end;

                                                    item:=resultjson.FindPath('web_app_current_date');
                                                    if (item <> nil) and (not item.IsNull) then
                                                    begin
                                                         web_app_current_date := Item.value;
                                                    end else begin
                                                         IdError := ERR_INVALID_HTML_CODE;
                                                         Error   := ERR_INVALID_HTML_DESC;
                                                    end;


                                               end
                                               else
                                               begin
                                                   IdError := ERR_INVALID_JWT_CODE;
                                                   Error   := ERR_INVALID_JWT_DESC;
                                               end;
                                          end else begin
                                               IdError := ERR_INVALID_HTML_CODE;
                                               Error   := ERR_INVALID_HTML_DESC;
                                          end;

                                   end;

                              end else begin
                                   IdError := ERR_INVALID_HTML_CODE;
                                   Error   := ERR_INVALID_HTML_DESC;
                              end;

                         end else begin
                              IdError := ERR_INVALID_HTML_CODE;
                              Error   := ERR_INVALID_HTML_DESC;
                         end;


                       end else begin
                           IdError := ERR_INVALID_HTML_CODE;
                           Error   := ERR_INVALID_HTML_DESC;
                       end;
           end;



        finally
                if Assigned(resultjson) then
                begin
                   resultjson.Free;
                   resultjson := nil;
                end;
        end;
     except
           on E: Exception do
           begin
                  IdError   := ERR_GENERIC_ERROR_CODE;
                  Error     := E.Message;
                  result    := false;

           end;
     end;

end;

function TMicroServiceFileClient.CreateRemoteDir(dirname: string; out
  IdError: integer; out Error: string): boolean;
var
   resultjson            : TJSONData;
   item                  : TJSONData;
   result_server         : String;
   error_number          : integer;
   error_description     : String;
   Parameters            : TArrayOfPostItem;
begin
     try
        try

           result              := false;
           IdError             := ERR_GENERIC_ERROR_CODE;
           Error               := '';

           SetLength(Parameters,1);
           Parameters[0].name  := 'dirname';
           Parameters[0].value := CustomCrypt(dirname);

           result_server  := RecuperaHtml(Self.Furl_server + 'createdir?mytoken=' + CustomCrypt(Self.jwt) ,Parameters);

           SetLength(Parameters,0);

           if trim(result_server) = '' then
           begin
                IdError := ERR_SERVER_UNREACHABLE_CODE;
                Error   := ERR_SERVER_UNREACHABLE_DESC;
           end else begin
                       if ValidTitleAndVersion(result_server) then
                       begin


                         resultjson := GetJSON(result_server);

                         item:=resultjson.FindPath('error_code');
                         if (item <> nil) and (not item.IsNull) then
                         begin
                              error_number := StrToIntDef(Item.value, 0);
                              IdError      := error_number;

                              item:=resultjson.FindPath('error_desc');
                              if (item <> nil) and (not item.IsNull) then
                              begin
                                   error_description := Item.value;

                                   if error_number<>ERR_NO_ERROR_CODE then
                                   begin
                                        Error := error_description;
                                   end else begin
                                          item:=resultjson.FindPath('jwt');
                                          if (item <> nil) and (not item.IsNull) then
                                          begin
                                               jwt := Item.value;

                                               if trim(jwt) <> '' then
                                               begin

                                                    item:=resultjson.FindPath('return_value');
                                                    if (item <> nil) and (not item.IsNull) then
                                                    begin
                                                         result := StrToBoolDef(Copy(item.AsJSON,2,Length(item.AsJSON)-2), false);
                                                    end;

                                               end
                                               else
                                               begin
                                                   IdError := ERR_INVALID_JWT_CODE;
                                                   Error   := ERR_INVALID_JWT_DESC;
                                               end;
                                          end else begin
                                               IdError := ERR_INVALID_HTML_CODE;
                                               Error   := ERR_INVALID_HTML_DESC;
                                          end;

                                   end;

                              end else begin
                                   IdError := ERR_INVALID_HTML_CODE;
                                   Error   := ERR_INVALID_HTML_DESC;
                              end;

                         end else begin
                              IdError  := ERR_INVALID_HTML_CODE;
                              Error    := ERR_INVALID_HTML_DESC;
                         end;


                       end else begin
                           IdError  := ERR_INVALID_HTML_CODE;
                           Error    := ERR_INVALID_HTML_DESC;
                       end;
           end;

        finally
                if Assigned(resultjson) then
                begin
                   resultjson.Free;
                   resultjson := nil;
                end;
        end;
     except
           on E: Exception do
           begin

                  IdError  := ERR_GENERIC_ERROR_CODE;
                  Error    := E.Message;
                  result   := false;

           end;
     end;
end;

function TMicroServiceFileClient.DeleteRemoteDir(dirname: string; out
  IdError: integer; out Error: string): boolean;
var
   resultjson            : TJSONData;
   item                  : TJSONData;
   result_server         : String;
   error_number          : integer;
   error_description     : String;
   Parameters            : TArrayOfPostItem;
begin
     try
        try

           result              := false;
           IdError             := ERR_GENERIC_ERROR_CODE;
           Error               := '';

           SetLength(Parameters,1);
           Parameters[0].name  := 'dirname';
           Parameters[0].value := CustomCrypt(dirname);

           result_server  := RecuperaHtml(Self.Furl_server + 'deletedir?mytoken=' + CustomCrypt(Self.jwt) ,Parameters);

           SetLength(Parameters,0);

           if trim(result_server) = '' then
           begin
                IdError := ERR_SERVER_UNREACHABLE_CODE;
                Error   := ERR_SERVER_UNREACHABLE_DESC;
           end else begin
                       if ValidTitleAndVersion(result_server) then
                       begin


                         resultjson := GetJSON(result_server);

                         item:=resultjson.FindPath('error_code');
                         if (item <> nil) and (not item.IsNull) then
                         begin
                              error_number := StrToIntDef(Item.value, 0);
                              IdError      := error_number;

                              item:=resultjson.FindPath('error_desc');
                              if (item <> nil) and (not item.IsNull) then
                              begin
                                   error_description := Item.value;

                                   if error_number<>ERR_NO_ERROR_CODE then
                                   begin
                                        Error := error_description;
                                   end else begin
                                          item:=resultjson.FindPath('jwt');
                                          if (item <> nil) and (not item.IsNull) then
                                          begin
                                               jwt := Item.value;

                                               if trim(jwt) <> '' then
                                               begin

                                                    item:=resultjson.FindPath('return_value');
                                                    if (item <> nil) and (not item.IsNull) then
                                                    begin
                                                         result := StrToBoolDef(Copy(item.AsJSON,2,Length(item.AsJSON)-2), false);
                                                    end;

                                               end
                                               else
                                               begin
                                                   IdError  := ERR_INVALID_JWT_CODE;
                                                   Error    := ERR_INVALID_JWT_DESC;
                                               end;
                                          end else begin
                                               IdError  := ERR_INVALID_HTML_CODE;
                                               Error    := ERR_INVALID_HTML_DESC;
                                          end;

                                   end;

                              end else begin
                                   IdError  := ERR_INVALID_HTML_CODE;
                                   Error    := ERR_INVALID_HTML_DESC;
                              end;

                         end else begin
                              IdError  := ERR_INVALID_HTML_CODE;
                              Error    := ERR_INVALID_HTML_DESC;
                         end;


                       end else begin
                           IdError  := ERR_INVALID_HTML_CODE;
                           Error := ERR_INVALID_HTML_DESC;
                       end;
           end;

        finally
                if Assigned(resultjson) then
                begin
                   resultjson.Free;
                   resultjson := nil;
                end;
        end;
     except
           on E: Exception do
           begin
                  IdError  := ERR_GENERIC_ERROR_CODE;
                  Error    := E.Message;
                  result   := false;

           end;
     end;
end;

function TMicroServiceFileClient.DeleteRemoteFile(filename: string; out
  IdError: integer; out Error: string): boolean;
var
   resultjson            : TJSONData;
   item                  : TJSONData;
   result_server         : String;
   error_number          : integer;
   error_description     : String;
   Parameters            : TArrayOfPostItem;
begin
     try
        try

           result              := false;
           IdError             := ERR_GENERIC_ERROR_CODE;
           Error               := '';

           SetLength(Parameters,1);
           Parameters[0].name  := 'filename';
           Parameters[0].value := CustomCrypt(filename);

           result_server  := RecuperaHtml(Self.Furl_server + 'deletefile?mytoken=' + CustomCrypt(Self.jwt) ,Parameters);

           SetLength(Parameters,0);

           if trim(result_server) = '' then
           begin
                IdError := ERR_SERVER_UNREACHABLE_CODE;
                Error   := ERR_SERVER_UNREACHABLE_DESC;
           end else begin
                       if ValidTitleAndVersion(result_server) then
                       begin


                         resultjson := GetJSON(result_server);

                         item:=resultjson.FindPath('error_code');
                         if (item <> nil) and (not item.IsNull) then
                         begin
                              error_number := StrToIntDef(Item.value, 0);
                              IdError      := error_number;

                              item:=resultjson.FindPath('error_desc');
                              if (item <> nil) and (not item.IsNull) then
                              begin
                                   error_description := Item.value;

                                   if error_number<>ERR_NO_ERROR_CODE then
                                   begin
                                        Error := error_description;
                                   end else begin
                                          item:=resultjson.FindPath('jwt');
                                          if (item <> nil) and (not item.IsNull) then
                                          begin
                                               jwt := Item.value;

                                               if trim(jwt) <> '' then
                                               begin

                                                    item:=resultjson.FindPath('return_value');
                                                    if (item <> nil) and (not item.IsNull) then
                                                    begin
                                                         result := StrToBoolDef(Copy(item.AsJSON,2,Length(item.AsJSON)-2), false);
                                                    end;

                                               end
                                               else
                                               begin
                                                   IdError := ERR_INVALID_JWT_CODE;
                                                   Error   := ERR_INVALID_JWT_DESC;
                                               end;
                                          end else begin
                                               IdError := ERR_INVALID_HTML_CODE;
                                               Error   := ERR_INVALID_HTML_DESC;
                                          end;

                                   end;

                              end else begin
                                   IdError := ERR_INVALID_HTML_CODE;
                                   Error   := ERR_INVALID_HTML_DESC;
                              end;

                         end else begin
                              IdError  := ERR_INVALID_HTML_CODE;
                              Error    := ERR_INVALID_HTML_DESC;
                         end;


                       end else begin
                           IdError  := ERR_INVALID_HTML_CODE;
                           Error    := ERR_INVALID_HTML_DESC;
                       end;
           end;

        finally
                if Assigned(resultjson) then
                begin
                   resultjson.Free;
                   resultjson := nil;
                end;
        end;
     except
           on E: Exception do
           begin
                  IdError  := ERR_GENERIC_ERROR_CODE;
                  Error    := E.Message;
                  result   := false;

           end;
     end;
end;

function TMicroServiceFileClient.RemoteFreeSpace(out IdError: integer; out
  Error: string): Int64;
var
   resultjson            : TJSONData;
   item                  : TJSONData;
   result_server         : String;
   error_number          : integer;
   error_description     : String;
begin
     try
        try

           result              := -1;
           IdError             := ERR_GENERIC_ERROR_CODE;
           Error               := '';

           result_server  := RecuperaHtml(Self.Furl_server + 'freediskspace?mytoken=' + CustomCrypt(Self.jwt) ,[]);

           if trim(result_server) = '' then
           begin
                IdError := ERR_SERVER_UNREACHABLE_CODE;
                Error   := ERR_SERVER_UNREACHABLE_DESC;
           end else begin
                       if ValidTitleAndVersion(result_server) then
                       begin


                         resultjson := GetJSON(result_server);

                         item:=resultjson.FindPath('error_code');
                         if (item <> nil) and (not item.IsNull) then
                         begin
                              error_number := StrToIntDef(Item.value, 0);
                              IdError      := error_number;

                              item:=resultjson.FindPath('error_desc');
                              if (item <> nil) and (not item.IsNull) then
                              begin
                                   error_description := Item.value;

                                   if error_number<>ERR_NO_ERROR_CODE then
                                   begin
                                        Error := error_description;
                                   end else begin
                                          item:=resultjson.FindPath('jwt');
                                          if (item <> nil) and (not item.IsNull) then
                                          begin
                                               jwt := Item.value;

                                               if trim(jwt) <> '' then
                                               begin

                                                    item:=resultjson.FindPath('return_value');
                                                    if (item <> nil) and (not item.IsNull) then
                                                    begin
                                                         result := StrToIntDef(Copy(item.AsJSON,2,Length(item.AsJSON)-2), -1);
                                                    end;

                                               end
                                               else
                                               begin
                                                   IdError := ERR_INVALID_JWT_CODE;
                                                   Error   := ERR_INVALID_JWT_DESC;
                                               end;
                                          end else begin
                                               IdError := ERR_INVALID_HTML_CODE;
                                               Error   := ERR_INVALID_HTML_DESC;
                                          end;

                                   end;

                              end else begin
                                   IdError := ERR_INVALID_HTML_CODE;
                                   Error   := ERR_INVALID_HTML_DESC;
                              end;

                         end else begin
                              IdError := ERR_INVALID_HTML_CODE;
                              Error   := ERR_INVALID_HTML_DESC;
                         end;


                       end else begin
                           IdError := ERR_INVALID_HTML_CODE;
                           Error   := ERR_INVALID_HTML_DESC;
                       end;
           end;


        finally
                if Assigned(resultjson) then
                begin
                   resultjson.Free;
                   resultjson := nil;
                end;
        end;
     except
           on E: Exception do
           begin

                  IdError  := ERR_GENERIC_ERROR_CODE;
                  Error    := E.Message;
                  result   := -1;

           end;
     end;
end;

function TMicroServiceFileClient.RemoteDirContent(dirname: string; out
  content: TArrayOfTMicroServiceFileIitem; out IdError: integer; out
  Error: string): boolean;
var
   resultjson            : TJSONData;
   item                  : TJSONData;
   result_server         : String;
   error_number          : integer;
   error_description     : String;
   Parameters            : TArrayOfPostItem;
   jData                 : TJSONData;
   jObject               : TJSONObject;
   jArray                : TJSonArray;
   i                     : Integer;
   ValueList             : TStringList;



   function IsValidJSON(json_str: string): boolean;
   var
      jData : TJSONData;
      ret   : boolean;
   begin
        try
           try

              ret := true;
              jData:=GetJSON(json_str);

           finally
                   if Assigned(resultjson) then
                   begin
                      jData.Free;
                      jData := nil;
                   end;
          end;
        except
              on E: Exception do
              begin

                 ret := false;

              end;
        end;


        result:=ret;

   end;

    procedure DumpJSONScalarArray(const Data: string;
      List: TStrings);
    var
       JS: TJSONDeStreamer;
       V : Variant;
       I : Integer;
       S : string;
    begin
      JS:= TJSONDeStreamer.Create(nil);
      List.BeginUpdate;
      List.Clear;
      try
        V:=JS.JSONToVariant(data);
        for I:=VarArrayLowBound(V,1) to VarArrayHighBound(V,1) do
        begin
          case VarType(V[I]) of
            varbyte, varword,
            varInteger, varlongword: S:=IntToStr(V[I]);
            varString,varOleStr : S:=V[I];
            else
              raise Exception.Create('Unexpected item type');
          end;
          List.add(S);
        end;
      finally
        List.EndUpdate;
        JS.Free;
      end;
    end;

begin
     try
        try

           result              := false;
           IdError             := ERR_GENERIC_ERROR_CODE;
           Error               := '';
           current_dirname     := '';
           SetLength(Content, 0);

           SetLength(Parameters,1);
           Parameters[0].name  := 'dirname';
           Parameters[0].value := CustomCrypt(dirname);

           result_server  := RecuperaHtml(Self.Furl_server + 'dircontent?mytoken=' + CustomCrypt(Self.jwt) ,Parameters);

           SetLength(Parameters,0);

           if trim(result_server) = '' then
           begin
                IdError := ERR_SERVER_UNREACHABLE_CODE;
                Error   := ERR_SERVER_UNREACHABLE_DESC;
           end else begin
                       if ValidTitleAndVersion(result_server) then
                       begin


                         resultjson := GetJSON(result_server);

                         item:=resultjson.FindPath('error_code');
                         if (item <> nil) and (not item.IsNull) then
                         begin
                              error_number := StrToIntDef(Item.value, 0);
                              IdError      := error_number;

                              item:=resultjson.FindPath('error_desc');
                              if (item <> nil) and (not item.IsNull) then
                              begin
                                   error_description := Item.value;

                                   if error_number<>ERR_NO_ERROR_CODE then
                                   begin
                                        Error := error_description;
                                   end else begin
                                          item:=resultjson.FindPath('jwt');
                                          if (item <> nil) and (not item.IsNull) then
                                          begin
                                               jwt := Item.value;

                                               if trim(jwt) <> '' then
                                               begin

                                                    item:=resultjson.FindPath('dirname');
                                                    if (item <> nil) and (not item.IsNull) then
                                                    begin
                                                         current_dirname := Item.value;
                                                         item:=resultjson.FindPath('return_value');
                                                         if (item <> nil) and (not item.IsNull) then
                                                         begin
                                                              result_server := '{ "files" : ' + item.AsJSON + '}';

                                                              if IsValidJSON(result_server) then
                                                              begin
                                                                   jData:=GetJSON(result_server);
                                                                   jObject := TJSONObject(jData);
                                                                   if jObject.Find('files', jData) then
                                                                   begin
                                                                        jArray := jObject.Arrays['files'];
                                                                        for i := 0 to jArray.Count-1 do
                                                                        begin
                                                                             if IsValidJSON(jArray[i].AsJSON) then
                                                                             begin
                                                                                  ValueList   := TStringList.Create;
                                                                                  DumpJSONScalarArray(jArray[i].AsJSON, ValueList);

                                                                                  if ValueList.Count=4 then
                                                                                  begin
                                                                                       SetLength(Content, Length(Content)+1);
                                                                                       result_server := ValueList[2];
                                                                                       Content[Length(Content)-1].FileName  := ValueList[0];
                                                                                       Content[Length(Content)-1].FileSize  := StrToIntDef(ValueList[1],-1);
                                                                                       Content[Length(Content)-1].TimeStamp := Str2DT(ValueList[2],now);

                                                                                       if ValueList[3]='1' then
                                                                                          Content[Length(Content)-1].IsDir    := true
                                                                                       else
                                                                                           Content[Length(Content)-1].IsDir    := false;
                                                                                  end;

                                                                                  ValueList.Free;

                                                                             end;

                                                                        end;
                                                                        jArray.Free;
                                                                   end;
                                                              end;

                                                              result := true;
                                                         end;
                                                    end else begin
                                                        IdError := ERR_INVALID_HTML_CODE;
                                                        Error   := ERR_INVALID_HTML_DESC;
                                                    end;

                                               end
                                               else
                                               begin
                                                   IdError := ERR_INVALID_JWT_CODE;
                                                   Error   := ERR_INVALID_JWT_DESC;
                                               end;
                                          end else begin
                                               IdError := ERR_INVALID_HTML_CODE;
                                               Error   := ERR_INVALID_HTML_DESC;
                                          end;

                                   end;

                              end else begin
                                   IdError := ERR_INVALID_HTML_CODE;
                                   Error   := ERR_INVALID_HTML_DESC;
                              end;

                         end else begin
                              IdError := ERR_INVALID_HTML_CODE;
                              Error   := ERR_INVALID_HTML_DESC;
                         end;


                       end else begin
                           IdError := ERR_INVALID_HTML_CODE;
                           Error   := ERR_INVALID_HTML_DESC;
                       end;
           end;


        finally
                if Assigned(resultjson) then
                begin
                   resultjson.Free;
                   resultjson := nil;
                end;
        end;
     except
           on E: Exception do
           begin

                  IdError  := ERR_GENERIC_ERROR_CODE;
                  Error    := E.Message;
                  result   := false;

           end;
     end;
end;

function TMicroServiceFileClient.DownloadFile(local_file: string;
  remote_filename: string; out IdError: integer; out Error: string; out
  ProgressBar: TProgressBar): boolean;
var
   resultjson            : TJSONData;
   item                  : TJSONData;
   item2                 : TJSONData;
   result_server         : String;
   error_number          : integer;
   error_description     : String;
   Parameters            : TArrayOfPostItem;
   content               : string;
   i                     : integer;
   exit_l                : boolean;
   index_pkg             : integer;
   total_pkg             : integer;
   hash                  : string;
   hash_recalculated     : string;
begin
     try
        try

           result              := false;
           IdError             := ERR_GENERIC_ERROR_CODE;
           Error               := '';


           if not FileExists(local_file) then
           begin

                SetLength(Parameters,2);
                Parameters[0].name  := 'filename';
                Parameters[0].value := CustomCrypt(remote_filename);
                ProgressBar.Min := 1;

                i               := 0;
                exit_l          := false;
                while (not exit_l) do
                begin

                     exit_l              := true;

                     Parameters[1].name  := 'index';
                     Parameters[1].value := CustomCrypt(IntToStr(i));

                     result_server  := RecuperaHtml(Self.Furl_server + 'downloadfile?mytoken=' + CustomCrypt(Self.jwt) ,Parameters);

                     if trim(result_server) = '' then
                     begin
                          IdError := ERR_SERVER_UNREACHABLE_CODE;
                          Error   := ERR_SERVER_UNREACHABLE_DESC;
                     end else begin
                          if ValidTitleAndVersion(result_server) then
                          begin


                            resultjson := GetJSON(result_server);

                            item:=resultjson.FindPath('error_code');
                            if (item <> nil) and (not item.IsNull) then
                            begin
                                 error_number := StrToIntDef(Item.value, 0);
                                 IdError      := error_number;
                                 item:=resultjson.FindPath('error_desc');
                                 if (item <> nil) and (not item.IsNull) then
                                 begin
                                      error_description := Item.value;

                                      if error_number<>ERR_NO_ERROR_CODE then
                                      begin
                                           Error := error_description;
                                      end else begin
                                             item:=resultjson.FindPath('jwt');
                                             if (item <> nil) and (not item.IsNull) then
                                             begin
                                                  jwt := Item.value;

                                                  if trim(jwt) <> '' then
                                                  begin

                                                       item:=resultjson.FindPath('return_value');
                                                       if (item <> nil) and (not item.IsNull) then
                                                       begin
                                                            result := StrToBoolDef(Copy(item.AsJSON,2,Length(item.AsJSON)-2), false);


                                                            item2:=resultjson.FindPath('index_pkg');
                                                            if (item2 <> nil) and (not item2.IsNull) then
                                                            begin
                                                                 index_pkg := StrToIntDef(Copy(item2.AsJSON,2,Length(item2.AsJSON)-2), 0);
                                                            end;

                                                            item2:=resultjson.FindPath('total_pkg');
                                                            if (item2 <> nil) and (not item2.IsNull) then
                                                            begin
                                                                 total_pkg := StrToIntDef(Copy(item2.AsJSON,2,Length(item2.AsJSON)-2), 0);
                                                                 ProgressBar.Max := total_pkg;
                                                            end;

                                                            item2:=resultjson.FindPath('file_b64');
                                                            if (item2 <> nil) and (not item2.IsNull) then
                                                            begin
                                                                 hash_recalculated := Self.StringHash(Copy(item2.AsJSON,2,Length(item2.AsJSON)-2));
                                                                 content           := content + Copy(item2.AsJSON,2,Length(item2.AsJSON)-2);
                                                            end;

                                                            item2:=resultjson.FindPath('hash');
                                                            if (item2 <> nil) and (not item2.IsNull) then
                                                            begin
                                                                 hash := Copy(item2.AsJSON,2,Length(item2.AsJSON)-2);
                                                            end;

                                                            if (hash=hash_recalculated) and (trim(hash)<>'') then
                                                            begin
                                                                 if index_pkg=(total_pkg-1) then
                                                                 begin

                                                                      if Base64ToFile(content, local_file) then
                                                                      begin

                                                                      end else begin
                                                                        IdError := ERR_B642FILE_CODE;
                                                                        Error   := ERR_B642FILE_DESC;
                                                                      end;

                                                                 end else begin
                                                                   Inc(i);
                                                                   exit_l := false;
                                                                 end;
                                                            end else begin
                                                                 IdError := ERR_HASH_CODE;
                                                                 Error   := ERR_HASH_DESC;
                                                            end;

                                                            ProgressBar.Position := index_pkg + 1;
                                                            ProgressBar.Invalidate;
                                                            Application.ProcessMessages;

                                                       end;

                                                  end
                                                  else
                                                  begin
                                                      IdError := ERR_INVALID_JWT_CODE;
                                                      Error   := ERR_INVALID_JWT_DESC;
                                                  end;
                                             end else begin
                                                  IdError := ERR_INVALID_HTML_CODE;
                                                  Error   := ERR_INVALID_HTML_DESC;
                                             end;

                                      end;

                                 end else begin
                                      IdError := ERR_INVALID_HTML_CODE;
                                      Error   := ERR_INVALID_HTML_DESC;
                                 end;

                            end else begin
                                 IdError := ERR_INVALID_HTML_CODE;
                                 Error   := ERR_INVALID_HTML_DESC;
                            end;


                          end else begin
                              IdError := ERR_INVALID_HTML_CODE;
                              Error   := ERR_INVALID_HTML_DESC;
                          end;
                     end;

                     ProgressBar.Position := i + 1;
                     ProgressBar.Invalidate;
                     Application.ProcessMessages;

                end;

           end else begin
               IdError := ERR_FILEEXISTS_CODE;
               Error   := ERR_FILEEXISTS_DESC;
           end;

        finally
                if Assigned(resultjson) then
                begin
                   resultjson.Free;
                   resultjson := nil;
                end;
        end;
     except
           on E: Exception do
           begin

                  IdError  := ERR_GENERIC_ERROR_CODE;
                  Error    := E.Message;
                  result   := false;

           end;
     end;

end;

function TMicroServiceFileClient.GetRemoteCurrentDirName: string;
begin
     result := current_dirname;
end;

function TMicroServiceFileClient.UploadFile(local_file: string;
  remote_dir: string; remote_filename: string; out IdError: integer; out
  Error: string; out ProgressBar: TProgressBar): boolean;
var
   resultjson            : TJSONData;
   item                  : TJSONData;
   result_server         : String;
   error_number          : integer;
   error_description     : String;
   Parameters            : TArrayOfPostItem;
   content               : string;
   tot                   : integer;
   i                     : integer;
   exit_l                : boolean;
   tot_hash              : string;
begin
     try
        try

           result              := false;
           IdError             := ERR_GENERIC_ERROR_CODE;
           Error               := '';


           if FileExists(local_file) then
           begin

                SetLength(Parameters,8);
                Parameters[0].name  := 'filename';
                Parameters[0].value := CustomCrypt(remote_filename);
                Parameters[1].name  := 'dirname';
                Parameters[1].value := CustomCrypt(remote_dir);
                Parameters[2].name  := 'filename';
                Parameters[2].value := CustomCrypt(remote_filename);
                content             := FileToBase64(local_file);
                tot_hash            := Self.StringHash(content);
                Parameters[3].name  := 'tot_hash';
                Parameters[3].value := CustomCrypt(tot_hash);

                exit_l              := false;
                tot := Trunc(Length(content)/MAX_FILE_NCHAR);
                if (Length(content) mod MAX_FILE_NCHAR)<>0 then
                   Inc(tot);

                ProgressBar.Min := 1;
                ProgressBar.Max := tot;

                i   := 0;
                while ((i<tot) and (not exit_l)) do
                begin

                     result              := false;
                     exit_l              := true;
                     Parameters[4].name  := 'index';
                     Parameters[4].value := CustomCrypt(IntToStr(i));
                     Parameters[5].name  := 'total';
                     Parameters[5].value := CustomCrypt(IntToStr(tot));
                     Parameters[6].name  := 'pkg_hash';
                     Parameters[6].value := CustomCrypt(Self.StringHash(copy(content, (i*MAX_FILE_NCHAR)+1, MAX_FILE_NCHAR)));
                     Parameters[7].name  := 'content_file';
                     Parameters[7].value := CustomCrypt(copy(content, (i*MAX_FILE_NCHAR)+1, MAX_FILE_NCHAR));

                     result_server  := RecuperaHtml(Self.Furl_server + 'uploadfile?mytoken=' + CustomCrypt(Self.jwt) ,Parameters);

                     if trim(result_server) = '' then
                     begin
                          IdError := ERR_SERVER_UNREACHABLE_CODE;
                          Error   := ERR_SERVER_UNREACHABLE_DESC;
                     end else begin
                          if ValidTitleAndVersion(result_server) then
                          begin

                            resultjson := GetJSON(result_server);

                            item:=resultjson.FindPath('error_code');
                            if (item <> nil) and (not item.IsNull) then
                            begin
                                 error_number := StrToIntDef(Item.value, 0);
                                 IdError      := error_number;
                                 item:=resultjson.FindPath('error_desc');
                                 if (item <> nil) and (not item.IsNull) then
                                 begin
                                      error_description := Item.value;

                                      if error_number<>ERR_NO_ERROR_CODE then
                                      begin
                                           Error := error_description;
                                      end else begin
                                             item:=resultjson.FindPath('jwt');
                                             if (item <> nil) and (not item.IsNull) then
                                             begin
                                                  jwt := Item.value;

                                                  if trim(jwt) <> '' then
                                                  begin

                                                       item:=resultjson.FindPath('return_value');
                                                       if (item <> nil) and (not item.IsNull) then
                                                       begin
                                                            result := StrToBoolDef(Copy(item.AsJSON,2,Length(item.AsJSON)-2), false);
                                                            exit_l := false;
                                                       end;

                                                  end
                                                  else
                                                  begin
                                                      IdError := ERR_INVALID_JWT_CODE;
                                                      Error   := ERR_INVALID_JWT_DESC;
                                                  end;
                                             end else begin
                                                  IdError := ERR_INVALID_HTML_CODE;
                                                  Error   := ERR_INVALID_HTML_DESC;
                                             end;

                                      end;

                                 end else begin
                                      IdError := ERR_INVALID_HTML_CODE;
                                      Error   := ERR_INVALID_HTML_DESC;
                                 end;

                            end else begin
                                 IdError := ERR_INVALID_HTML_CODE;
                                 Error   := ERR_INVALID_HTML_DESC;
                            end;


                          end else begin
                              IdError := ERR_INVALID_HTML_CODE;
                              Error   := ERR_INVALID_HTML_DESC;
                          end;
                     end;

                     ProgressBar.Position := i + 1;
                     ProgressBar.Invalidate;
                     Application.ProcessMessages;

                     Inc(i);
                end;

           end else begin
               IdError := ERR_FILENAME_NOTEXISTS_CODE;
               Error   := ERR_FILENAME_NOTEXISTS_DESC;
           end;

        finally
                if Assigned(resultjson) then
                begin
                   resultjson.Free;
                   resultjson := nil;
                end;
        end;
     except
           on E: Exception do
           begin

                  IdError  := ERR_GENERIC_ERROR_CODE;
                  Error    := E.Message;
                  result   := false;

           end;
     end;
end;

function TMicroServiceFileClient.RemoteFileExists(filename: string; out
  IdError: integer; out Error: string): boolean;
var
   resultjson            : TJSONData;
   item                  : TJSONData;
   result_server         : String;
   error_number          : integer;
   error_description     : String;
   Parameters            : TArrayOfPostItem;
begin
     try
        try

           result              := false;
           IdError             := ERR_GENERIC_ERROR_CODE;
           Error               := '';

           SetLength(Parameters,1);
           Parameters[0].name  := 'filename';
           Parameters[0].value := CustomCrypt(filename);

           result_server  := RecuperaHtml(Self.Furl_server + 'fileexists?mytoken=' + CustomCrypt(Self.jwt) ,Parameters);

           SetLength(Parameters,0);

           if trim(result_server) = '' then
           begin
                IdError := ERR_SERVER_UNREACHABLE_CODE;
                Error   := ERR_SERVER_UNREACHABLE_DESC;
           end else begin
                       if ValidTitleAndVersion(result_server) then
                       begin


                         resultjson := GetJSON(result_server);

                         item:=resultjson.FindPath('error_code');
                         if (item <> nil) and (not item.IsNull) then
                         begin
                              error_number := StrToIntDef(Item.value, 0);
                              IdError      := error_number;

                              item:=resultjson.FindPath('error_desc');
                              if (item <> nil) and (not item.IsNull) then
                              begin
                                   error_description := Item.value;

                                   if error_number<>ERR_NO_ERROR_CODE then
                                   begin
                                        Error := error_description;
                                   end else begin
                                          item:=resultjson.FindPath('jwt');
                                          if (item <> nil) and (not item.IsNull) then
                                          begin
                                               jwt := Item.value;

                                               if trim(jwt) <> '' then
                                               begin

                                                    item:=resultjson.FindPath('return_value');
                                                    if (item <> nil) and (not item.IsNull) then
                                                    begin
                                                         result := StrToBoolDef(Copy(item.AsJSON,2,Length(item.AsJSON)-2), false);
                                                    end;

                                               end
                                               else
                                               begin
                                                   IdError := ERR_INVALID_JWT_CODE;
                                                   Error   := ERR_INVALID_JWT_DESC;
                                               end;
                                          end else begin
                                               IdError := ERR_INVALID_HTML_CODE;
                                               Error   := ERR_INVALID_HTML_DESC;
                                          end;

                                   end;

                              end else begin
                                   IdError := ERR_INVALID_HTML_CODE;
                                   Error   := ERR_INVALID_HTML_DESC;
                              end;

                         end else begin
                              IdError := ERR_INVALID_HTML_CODE;
                              Error   := ERR_INVALID_HTML_DESC;
                         end;


                       end else begin
                           IdError := ERR_INVALID_HTML_CODE;
                           Error   := ERR_INVALID_HTML_DESC;
                       end;
           end;

        finally
                if Assigned(resultjson) then
                begin
                   resultjson.Free;
                   resultjson := nil;
                end;
        end;
     except
           on E: Exception do
           begin
                  IdError  := ERR_GENERIC_ERROR_CODE;
                  Error    := E.Message;
                  result   := false;

           end;
     end;
end;

function TMicroServiceFileClient.RemoteDirExists(dirname: string; out
  IdError: integer; out Error: string): boolean;
var
   resultjson            : TJSONData;
   item                  : TJSONData;
   result_server         : String;
   error_number          : integer;
   error_description     : String;
   Parameters            : TArrayOfPostItem;
begin
     try
        try

           result              := false;
           IdError             := ERR_GENERIC_ERROR_CODE;
           Error               := '';

           SetLength(Parameters,1);
           Parameters[0].name  := 'dirname';
           Parameters[0].value := CustomCrypt(dirname);

           result_server  := RecuperaHtml(Self.Furl_server + 'direxists?mytoken=' + CustomCrypt(Self.jwt) ,Parameters);

           SetLength(Parameters,0);

           if trim(result_server) = '' then
           begin
                IdError := ERR_SERVER_UNREACHABLE_CODE;
                Error   := ERR_SERVER_UNREACHABLE_DESC;
           end else begin
                       if ValidTitleAndVersion(result_server) then
                       begin


                         resultjson := GetJSON(result_server);

                         item:=resultjson.FindPath('error_code');
                         if (item <> nil) and (not item.IsNull) then
                         begin
                              error_number := StrToIntDef(Item.value, 0);
                              IdError      := error_number;

                              item:=resultjson.FindPath('error_desc');
                              if (item <> nil) and (not item.IsNull) then
                              begin
                                   error_description := Item.value;

                                   if error_number<>ERR_NO_ERROR_CODE then
                                   begin
                                        Error := error_description;
                                   end else begin
                                          item:=resultjson.FindPath('jwt');
                                          if (item <> nil) and (not item.IsNull) then
                                          begin
                                               jwt := Item.value;

                                               if trim(jwt) <> '' then
                                               begin

                                                    item:=resultjson.FindPath('return_value');
                                                    if (item <> nil) and (not item.IsNull) then
                                                    begin
                                                         result := StrToBoolDef(Copy(item.AsJSON,2,Length(item.AsJSON)-2), false);
                                                    end;

                                               end
                                               else
                                               begin
                                                   IdError := ERR_INVALID_JWT_CODE;
                                                   Error   := ERR_INVALID_JWT_DESC;
                                               end;
                                          end else begin
                                               IdError := ERR_INVALID_HTML_CODE;
                                               Error   := ERR_INVALID_HTML_DESC;
                                          end;

                                   end;

                              end else begin
                                   IdError := ERR_INVALID_HTML_CODE;
                                   Error   := ERR_INVALID_HTML_DESC;
                              end;

                         end else begin
                              IdError  := ERR_INVALID_HTML_CODE;
                              Error    := ERR_INVALID_HTML_DESC;
                         end;


                       end else begin
                           IdError  := ERR_INVALID_HTML_CODE;
                           Error := ERR_INVALID_HTML_DESC;
                       end;
           end;

        finally
                if Assigned(resultjson) then
                begin
                   resultjson.Free;
                   resultjson := nil;
                end;
        end;
     except
           on E: Exception do
           begin
                  IdError  := ERR_GENERIC_ERROR_CODE;
                  Error    := E.Message;
                  result   := false;

           end;
     end;
end;

function TMicroServiceFileClient.GetDirSeparatorFromServer(out IdError : integer; out Error: string): boolean;
var
   resultjson            : TJSONData;
   item                  : TJSONData;
   result_server         : String;
   error_number          : integer;
   error_description     : String;
begin
     try
        try


           FServerDirSeparator := '';
           result              := false;
           IdError             := ERR_GENERIC_ERROR_CODE;
           Error               := '';

           result_server  := RecuperaHtml(Self.Furl_server + 'dirseparator?mytoken=' + CustomCrypt(Self.jwt) ,[]);

           if trim(result_server) = '' then
           begin
                IdError := ERR_SERVER_UNREACHABLE_CODE;
                Error   := ERR_SERVER_UNREACHABLE_DESC;
           end else begin
                       if ValidTitleAndVersion(result_server) then
                       begin


                         resultjson := GetJSON(result_server);

                         item:=resultjson.FindPath('error_code');
                         if (item <> nil) and (not item.IsNull) then
                         begin
                              error_number := StrToIntDef(Item.value, 0);
                              IdError      := error_number;
                              item:=resultjson.FindPath('error_desc');
                              if (item <> nil) and (not item.IsNull) then
                              begin
                                   error_description := Item.value;

                                   if error_number<>ERR_NO_ERROR_CODE then
                                   begin
                                        Error := error_description;
                                   end else begin
                                          item:=resultjson.FindPath('jwt');
                                          if (item <> nil) and (not item.IsNull) then
                                          begin
                                               jwt := Item.value;

                                               if trim(jwt) <> '' then
                                               begin

                                                    item:=resultjson.FindPath('return_value');
                                                    if (item <> nil) and (not item.IsNull) then
                                                    begin
                                                         FServerDirSeparator := item.AsJSON;
                                                         FServerDirSeparator := Copy(FServerDirSeparator,2,Length(FServerDirSeparator)-2);
                                                    end;

                                                    result := true;
                                               end
                                               else
                                               begin
                                                   IdError := ERR_INVALID_JWT_CODE;
                                                   Error   := ERR_INVALID_JWT_DESC;
                                               end;
                                          end else begin
                                               IdError := ERR_INVALID_HTML_CODE;
                                               Error   := ERR_INVALID_HTML_DESC;
                                          end;

                                   end;

                              end else begin
                                   IdError := ERR_INVALID_HTML_CODE;
                                   Error   := ERR_INVALID_HTML_DESC;
                              end;

                         end else begin
                              IdError := ERR_INVALID_HTML_CODE;
                              Error   := ERR_INVALID_HTML_DESC;
                         end;


                       end else begin
                           IdError := ERR_INVALID_HTML_CODE;
                           Error   := ERR_INVALID_HTML_DESC;
                       end;
           end;

        finally
                if Assigned(resultjson) then
                begin
                   resultjson.Free;
                   resultjson := nil;
                end;
        end;
     except
           on E: Exception do
           begin

                  IdError  := ERR_GENERIC_ERROR_CODE;
                  Error    := E.Message;
                  result   := false;

           end;
     end;
end;

function TMicroServiceFileClient.CustomCrypt(value: string): string;
var
   CustomCyper1 : TCustomCyper;
begin
     if FApplicationCustomCryptKey<>'' then
     begin
          CustomCyper1 := TCustomCyper.Create;
          result := EncodeStringBase64(CustomCyper1.Encrypt(value,FApplicationCustomCryptKey, reversestring(FApplicationCustomCryptKey)));
          CustomCyper1.Free;
          CustomCyper1 := nil;
     end else begin
            result := EncodeStringBase64(value);
     end;
end;

function TMicroServiceFileClient.CustomDecrypt(value: string): string;
var
   CustomCyper1 : TCustomCyper;
begin
     value := DecodeStringBase64(value);
     if FApplicationCustomCryptKey<>'' then
     begin
          CustomCyper1 := TCustomCyper.Create;
          result := CustomCyper1.Decrypt(value,FApplicationCustomCryptKey, reversestring(FApplicationCustomCryptKey));
          CustomCyper1.Free;
          CustomCyper1 := nil;
     end else begin
            result := value;
     end;
end;

function TMicroServiceFileClient.StringHash(value: string): string;
begin
     result := MD5Print(MD5String(value));
end;

end.

