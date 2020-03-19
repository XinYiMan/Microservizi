unit uMicroServiceMessageClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,fpjson, jsonparser, uconsts, md5, ugethtml, uCryptFunction, strutils;

type
    TMicroServiceDBInformation = object
          web_app_title        : String;
          web_app_versione     : String;
          web_app_author       : String;
    end;

 type
     TItemViewMessages = object
         typemessage          : string;
         message              : string;
         source               : string;
         id_connection_source : integer;
         //data        : string;
         //inc_data    : integer;
     end;

 type TArrayOfItemViewMessages = array of TItemViewMessages;

type

    { TMicroServiceMessageClient }

    TMicroServiceMessageClient = class
     private
           Info                       : TMicroServiceDBInformation;
           Furl_server                : String;
           jwt                        : String;
           FApplicationCustomCryptKey : string;
           FPosition                  : string;
           Flast_date_read            : string;
           function ValidTitleAndVersion(server_result: String) : boolean;
           function RecuperaHTML(url : String; postItems: TArrayOfPostItem) : String;
           function CustomDecrypt(value: string): string;
           function CustomCrypt(value: string): string;
     public
           constructor Create(url : string; ApplicationCustomCryptKey : string; Position : string);
           destructor Free;
           function GetInfo: boolean;
           function Login(user: String; password: String; out IdError : integer; out Error: String): boolean;
           function JWTIsNull : boolean;
           procedure GetServerInfoForPrint(value: TStrings);
           function Manual(value: TStrings; out IdError : integer; out Error: string): boolean;
           function Diagnostic(out operating_system: string; out
             web_app_date_start: string; out web_app_current_date : string; out IdError : integer; out Error: String): boolean;
           function SendMessage(typemessage: string; message: string; destination: string;
             out IdError: integer; out Error: string; IdDestination : integer = -1): boolean;
           function ViewMessages(out MessagesList : TArrayOfItemViewMessages; out IdError: integer; out Error: string): boolean;
           function LogOut(out IdError : integer; out Error: string): boolean;
    end;

const
     VALID_WEB_APP_VERSION  = '0.0.1';
     VALID_WEB_APP_TITLE    = 'ngit_microservice_message';

implementation
uses
    base64, uJsonToBufferDataset, BufDataset;

{ TMicroServiceMessageClient }

function TMicroServiceMessageClient.ValidTitleAndVersion(server_result: String
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

function TMicroServiceMessageClient.RecuperaHTML(url: String;
  postItems: TArrayOfPostItem): String;
var
   app : string;
begin
     app    := GetHTML(url, postItems);
     result := CustomDeCrypt(app);
end;

constructor TMicroServiceMessageClient.Create(url: string;
  ApplicationCustomCryptKey: string; Position: string);
begin
     Self.FApplicationCustomCryptKey := ApplicationCustomCryptKey;
     Self.Furl_server                := url;
     Self.FPosition                  := position;
     Self.Flast_date_read            := '';
end;

destructor TMicroServiceMessageClient.Free;
begin

end;

function TMicroServiceMessageClient.GetInfo: boolean;
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

function TMicroServiceMessageClient.Login(user: String; password: String; out
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



           result_server  := RecuperaHtml(Self.Furl_server + 'login?username=' + CustomCrypt(user) + '&pwd_hash=' + CustomCrypt(MD5Print(MD5String(Password))) + '&position=' + CustomCrypt(Self.FPosition),[]);

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
                                                    ret := true;
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

function TMicroServiceMessageClient.JWTIsNull: boolean;
begin
  if trim(Self.jwt) = '' then
     result := true
  else
      result := false;
end;

procedure TMicroServiceMessageClient.GetServerInfoForPrint(value: TStrings);
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

function TMicroServiceMessageClient.Manual(value: TStrings; out IdError: integer;
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

                                             item:=resultjson.FindPath('sendmessage_page');
                                             if (item <> nil) and (not item.IsNull) then
                                             begin
                                                value.Add('sendmessage_page = ' + Item.value);
                                                value.Add('');
                                             end;

                                             item:=resultjson.FindPath('viewmessages_page');
                                             if (item <> nil) and (not item.IsNull) then
                                             begin
                                                value.Add('viewmessages_page = ' + Item.value);
                                                value.Add('');
                                             end;

                                             item:=resultjson.FindPath('logout_page');
                                             if (item <> nil) and (not item.IsNull) then
                                             begin
                                                value.Add('logout_page = ' + Item.value);
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

function TMicroServiceMessageClient.Diagnostic(out operating_system: string; out
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

function TMicroServiceMessageClient.SendMessage(typemessage: string;
  message: string; destination: string; out
  IdError: integer; out Error: string; IdDestination: integer): boolean;
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

           SetLength(params,5);
           params[0].name:='position';
           params[0].value:=CustomCrypt(Self.FPosition);
           params[1].name:='typemessage';
           params[1].value:=CustomCrypt(typemessage);
           params[2].name:='message';
           params[2].value:=CustomCrypt(message);
           params[3].name:='destination';
           params[3].value:=CustomCrypt(destination);
           params[4].name:='id_destination';
           params[4].value:=CustomCrypt(IntToStr(IdDestination));



           result_server  := RecuperaHtml(Self.Furl_server + 'sendmessage?mytoken=' + CustomCrypt(Self.jwt) ,params);
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
                                                       result := StrToBoolDef(item.Value,false);

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

function TMicroServiceMessageClient.ViewMessages(out
  MessagesList: TArrayOfItemViewMessages; out IdError: integer; out
  Error: string): boolean;
var
   resultjson            : TJSONData;
   item                  : TJSONData;
   result_server         : String;
   error_number          : integer;
   error_description     : String;
   params                : TArrayOfPostItem;
   dataset_json          : string;
   messages              : TJSONArray;
   jData                 : TJSONData;
   jObject               : TJSONObject;
   row                   : string;
   jData2                : TJSONData;
   jObject2              : TJSONObject;
   fields                : TJSONArray;
   i,j                   : integer;
begin
     try
        try

           result     := false;
           IdError    := ERR_GENERIC_ERROR_CODE;
           Error      := '';

           SetLength(MessagesList,0);

           SetLength(params,1);
           params[0].name:='position';
           params[0].value:=CustomCrypt(Self.FPosition);

           result_server  := RecuperaHtml(Self.Furl_server + 'viewmessages?mytoken=' + CustomCrypt(Self.jwt) ,params);
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
                                                            result := StrToBoolDef(item.Value,false);

                                                         if result then
                                                         begin

                                                              item:=resultjson.FindPath('dataset');
                                                              if (item <> nil) and (not item.IsNull) then
                                                                 dataset_json := item.AsJSON;

                                                              if (trim(dataset_json)<>'') and (trim(dataset_json)<>'[]') then
                                                              begin

                                                                   dataset_json := '{ "Msgs" : ' + dataset_json + '}';
                                                                   jData        := GetJSON(dataset_json);
                                                                   jObject      := TJSONObject(jData);
                                                                   messages     := jObject.Arrays['Msgs'];

                                                                   for i := 0 to messages.Count-1 do
                                                                   begin

                                                                        row       := '{ "Row" : ' + messages[i].AsString + '}';
                                                                        jData2    := GetJSON(row);
                                                                        jObject2  := TJSONObject(jData2);

                                                                        fields := jObject2.Arrays['Row'];
                                                                        for j := 0 to fields.Count-1 do
                                                                        begin
                                                                             if j = 0 then
                                                                             begin
                                                                                  SetLength(MessagesList,Length(MessagesList)+1);
                                                                                  MessagesList[Length(MessagesList)-1].message := fields[j].AsString;
                                                                             end;

                                                                             if j = 1 then
                                                                                MessagesList[Length(MessagesList)-1].typemessage := fields[j].AsString;

                                                                             if j = 2 then
                                                                                MessagesList[Length(MessagesList)-1].source := fields[j].AsString;

                                                                             if j = 3 then
                                                                                MessagesList[Length(MessagesList)-1].id_connection_source := StrToIntDef(fields[j].AsString,-1);

                                                                        end;

                                                                        fields.Free;
                                                                        fields := nil;

                                                                   end;

                                                                   messages.Free;
                                                                   messages := nil;


                                                              end;

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

function TMicroServiceMessageClient.LogOut(out IdError: integer; out
  Error: string): boolean;
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

           result_server  := RecuperaHtml(Self.Furl_server + 'logout?mytoken=' + CustomCrypt(Self.jwt),[]);

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

function TMicroServiceMessageClient.CustomCrypt(value: string): string;
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

function TMicroServiceMessageClient.CustomDecrypt(value: string): string;
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

