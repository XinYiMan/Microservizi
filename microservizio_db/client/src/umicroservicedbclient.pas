unit uMicroServiceDBClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,fpjson, jsonparser, uConsts, md5, ugethtml, uCryptFunction, strutils;

type
    TMicroServiceDBInformation = object
          web_app_title        : String;
          web_app_versione     : String;
          web_app_author       : String;
    end;

type

    { TMicroServiceDBClient }

    TMicroServiceDBClient = class
     private
           Info                       : TMicroServiceDBInformation;
           Furl_server                : String;
           jwt                        : String;
           FApplicationCustomCryptKey : string;
           function ValidTitleAndVersion(server_result: String) : boolean;
           function RecuperaHTML(url : String; postItems: TArrayOfPostItem) : String;
           function CustomDecrypt(value: string): string;
           function CustomCrypt(value: string): string;
     public
           constructor Create(url : string; ApplicationCustomCryptKey : string);
           destructor Free;
           function GetInfo: boolean;
           function Login(user: String; password: String; out IdError : integer; out Error: String): boolean;
           function JWTIsNull : boolean;
           procedure GetServerInfoForPrint(value: TStrings);
           function Manual(value: TStrings; out IdError : integer; out Error: string): boolean;
           function Select(sql: string; out fields_str: string; out IdError : integer; out Error: String): string;
           function Execute(sql : string; out IdError : integer; out Error: String) : boolean;
           function Script(sql : string; out IdError : integer; out Error: String) : boolean;
           function Diagnostic(out operating_system: string; out
             web_app_date_start: string; out web_app_current_date : string; out IdError : integer; out Error: String): boolean;
           function Current_TimeStamp(out IdError: integer; out Error: string;
             ValueIfError: TDateTime): TDateTime;
           function GetFieldFromTable(field: string; table: string; condition: string; out return_value: string; out IdError : integer; out Error: String): boolean;
    end;

const
     VALID_WEB_APP_VERSION  = '0.0.1';
     VALID_WEB_APP_TITLE    = 'ngit_microservice_db';

implementation
uses
    base64, uDateTimeToStr;

{ TMicroServiceDBClient }

function TMicroServiceDBClient.ValidTitleAndVersion(server_result: String
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

function TMicroServiceDBClient.RecuperaHTML(url: String;
  postItems: TArrayOfPostItem): String;
var
   app : string;
begin
     app    := GetHTML(url, postItems);
     result := CustomDeCrypt(app);
end;

constructor TMicroServiceDBClient.Create(url: string;
  ApplicationCustomCryptKey: string);
begin
     Self.FApplicationCustomCryptKey := ApplicationCustomCryptKey;
     Self.Furl_server                := url;
end;

destructor TMicroServiceDBClient.Free;
begin

end;

function TMicroServiceDBClient.GetInfo: boolean;
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

function TMicroServiceDBClient.Login(user: String; password: String; out
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


           ret     := false;
           IdError := ERR_GENERIC_ERROR_CODE;
           Error   := '';

           result_server  := RecuperaHtml(Self.Furl_server + 'login?username=' + CustomCrypt(user) + '&pwd_hash=' + CustomCrypt(MD5Print(MD5String(Password))),[]);


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
                                               Error := ERR_INVALID_HTML_DESC;
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

                  IdError := ERR_GENERIC_ERROR_CODE;
                  Error   := E.Message;
                  ret     := false;

           end;
     end;


     result := ret;

end;

function TMicroServiceDBClient.JWTIsNull: boolean;
begin
  if trim(Self.jwt) = '' then
     result := true
  else
      result := false;
end;

procedure TMicroServiceDBClient.GetServerInfoForPrint(value: TStrings);
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

function TMicroServiceDBClient.Manual(value: TStrings; out IdError: integer;
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
           Error    := '';

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

                                             item:=resultjson.FindPath('select_page');
                                             if (item <> nil) and (not item.IsNull) then
                                             begin
                                                value.Add('select_page = ' + Item.value);
                                                value.Add('');
                                             end;

                                             item:=resultjson.FindPath('execute_page');
                                             if (item <> nil) and (not item.IsNull) then
                                             begin
                                                value.Add('execute_page = ' + Item.value);
                                                value.Add('');
                                             end;

                                             item:=resultjson.FindPath('script_page');
                                             if (item <> nil) and (not item.IsNull) then
                                             begin
                                                value.Add('script_page = ' + Item.value);
                                                value.Add('');
                                             end;

                                             item:=resultjson.FindPath('diagnostic_page');
                                             if (item <> nil) and (not item.IsNull) then
                                             begin
                                                value.Add('diagnostic_page = ' + Item.value);
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
                                      IdError := ERR_INVALID_HTML_CODE;
                                      Error := ERR_INVALID_HTML_DESC;
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
                Error    := '';
                result   := false;

           end;
     end;
end;

function TMicroServiceDBClient.Select(sql: string; out fields_str: string; out
  IdError: integer; out Error: String): string;
var
   resultjson            : TJSONData;
   item                  : TJSONData;
   result_server         : String;
   error_number          : integer;
   error_description     : String;
   params                : TArrayOfPostItem;
begin
     try
        try


           result     := '';
           IdError    := ERR_GENERIC_ERROR_CODE;
           Error      := '';
           fields_str := '';

           SetLength(params,1);
           params[0].name:='sql';
           params[0].value:=CustomCrypt(sql);

           result_server  := RecuperaHtml(Self.Furl_server + 'select?mytoken=' + CustomCrypt(Self.jwt) ,params);
           SetLength(params,0);

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

                                                    item:=resultjson.FindPath('dataset');
                                                    if (item <> nil) and (not item.IsNull) then
                                                       result := item.AsJSON;

                                                    item:=resultjson.FindPath('fields');
                                                    if (item <> nil) and (not item.IsNull) then
                                                       fields_str := item.AsJSON;

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
                  result   := '';

           end;
     end;
end;

function TMicroServiceDBClient.Execute(sql: string; out IdError: integer; out
  Error: String): boolean;
var
   resultjson            : TJSONData;
   item                  : TJSONData;
   result_server         : String;
   error_number          : integer;
   error_description     : String;
   params                : TArrayOfPostItem;
begin
     try
        try


           result   := false;
           IdError  := ERR_GENERIC_ERROR_CODE;
           Error    := '';

           SetLength(params,1);
           params[0].name:='sql';
           params[0].value:=CustomCrypt(sql);

           result_server  := RecuperaHtml(Self.Furl_server + 'execute?mytoken=' + CustomCrypt(Self.jwt) ,params);
           SetLength(params,0);

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
                                   Error := ERR_INVALID_HTML_DESC;
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

function TMicroServiceDBClient.Script(sql: string; out IdError: integer; out
  Error: String): boolean;
var
   resultjson            : TJSONData;
   item                  : TJSONData;
   result_server         : String;
   error_number          : integer;
   error_description     : String;
   params                : TArrayOfPostItem;
begin
     try
        try


           result   := false;
           IdError  := ERR_GENERIC_ERROR_CODE;
           Error    := '';

           SetLength(params,1);
           params[0].name:='sql';
           params[0].value:=CustomCrypt(sql);

           result_server  := RecuperaHtml(Self.Furl_server + 'script?mytoken=' + CustomCrypt(Self.jwt) ,params);
           SetLength(params,0);

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

function TMicroServiceDBClient.Diagnostic(out operating_system: string; out
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


           result                := false;
           IdError               := ERR_GENERIC_ERROR_CODE;
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

                  IdError := ERR_GENERIC_ERROR_CODE;
                  Error   := E.Message;
                  result  := false;

           end;
     end;

end;

function TMicroServiceDBClient.Current_TimeStamp(out IdError: integer; out
  Error: string; ValueIfError : TDateTime): TDateTime;
var
   resultjson            : TJSONData;
   item                  : TJSONData;
   result_server         : String;
   error_number          : integer;
   error_description     : String;
begin
     try
        try


           result                := ValueIfError;
           IdError               := ERR_GENERIC_ERROR_CODE;
           Error                 := '';

           result_server  := RecuperaHtml(Self.Furl_server + 'current_timestamp?mytoken=' + CustomCrypt(Self.jwt) ,[]);

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

                                                    item:=resultjson.FindPath('current_timestamp');
                                                    if (item <> nil) and (not item.IsNull) then
                                                    begin
                                                         result := Str2DT(Item.value, ValueIfError);
                                                    end else begin
                                                         result := ValueIfError;
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

                  IdError := ERR_GENERIC_ERROR_CODE;
                  Error   := E.Message;
                  result  := ValueIfError;

           end;
     end;
end;

function TMicroServiceDBClient.GetFieldFromTable(field: string; table: string;
  condition: string; out return_value: string; out IdError: integer; out
  Error: String): boolean;
var
   resultjson            : TJSONData;
   item                  : TJSONData;
   result_server         : String;
   error_number          : integer;
   error_description     : String;
   params                : TArrayOfPostItem;
begin
     try
        try


           result     := false;
           IdError    := ERR_GENERIC_ERROR_CODE;
           Error      := '';

           SetLength(params,3);
           params[0].name:='field';
           params[0].value:=CustomCrypt(field);
           params[1].name:='table';
           params[1].value:=CustomCrypt(table);
           params[2].name:='condition';
           params[2].value:=CustomCrypt(condition);

           result_server  := RecuperaHtml(Self.Furl_server + 'getfieldfromtable?mytoken=' + CustomCrypt(Self.jwt) ,params);
           SetLength(params,0);

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
                                                         return_value := item.AsJSON;
                                                         return_value := Copy(return_value,2,Length(return_value)-2);
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

                  IdError := ERR_GENERIC_ERROR_CODE;
                  Error   := E.Message;
                  result  := false;

           end;
     end;
end;

function TMicroServiceDBClient.CustomCrypt(value: string): string;
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

function TMicroServiceDBClient.CustomDecrypt(value: string): string;
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

end.

