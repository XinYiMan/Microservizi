unit uMain;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, uExtended_fphttpapp,
  uCryptFunction, strutils;

type

  { TFPWebModule1 }

  TFPWebModule1 = class(TFPWebModule)
    procedure diagnosticRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure infoRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure loginRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure logoutRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure manualRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure sendmessageRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure viewmessagesRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
         function LoginWithGenerateJWT(mytoken: string; username: string;
           pwd_hash: string; position : string): string;
         function VerifiedJWT(mytoken : string) : string;
         function JWTFromAdmin(mytoken: string): boolean;
         function JWTUserName(mytoken: string): string;

         function CustomCrypt(value : string) : string;
         function CustomDecrypt(value : string) : string;
  public

  end;

var
  FPWebModule1: TFPWebModule1;

implementation
uses
    fpjson, jsonparser, base64, NGIT_Crypto_JWT, uConsts, dateutils, uGenericFunctions, uDateTimeToStr,
    uMemoryMessages;

{$R *.lfm}

{ TFPWebModule1 }

procedure TFPWebModule1.infoRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
   json_da_restituire : TJSONObject;
   RemoteHost         : string;
begin
  RemoteHost := ARequest.RemoteAddr;
  json_da_restituire:=TJSONObject.Create(['web_app_title',WEB_APP_TITLE,
                         'web_app_version',WEB_APP_VERSION,
                         'web_app_author',WEB_APP_AUTHOR,
                         'error_code',ERR_NO_ERROR_CODE,
                         'error_desc',ERR_NO_ERROR_DESC,
                         'jwt','']);

  LogMessage('','uMain','infoRequest',RemoteHost, json_da_restituire.AsJSON);

  AResponse.Contents.Text := CustomCrypt(json_da_restituire.AsJSON);
  json_da_restituire.Free;

  Handled := true;
end;

procedure TFPWebModule1.diagnosticRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
   json_da_restituire : TJSONObject;
   mytoken            : string;
   error              : integer;
   error_desc         : string;
   RemoteHost         : string;
begin
  RemoteHost         := ARequest.RemoteAddr;
  mytoken            := CustomDecrypt(ARequest.QueryFields.Values['mytoken']);

  if trim(mytoken)='' then
  begin
       //non faccio nulla perchè vuol dire che non è loggato
       error       := ERR_INVALID_JWT_CODE;
       error_desc  := ERR_INVALID_JWT_DESC;
  end else begin

       mytoken  := Self.VerifiedJWT(mytoken);
       if trim(mytoken)='' then
       begin
            error       := ERR_INVALID_JWT_CODE;
            error_desc  := ERR_INVALID_JWT_DESC;
       end else begin

              if JWTFromAdmin(mytoken) then
              begin
                   error       := ERR_NO_ERROR_CODE;
                   error_desc  := ERR_NO_ERROR_DESC;

                   //writeln('TotalAllocated: ' + IntToStr(GetHeapStatus.TotalAllocated)); //<--stabilire la memoria occupata ma non funziona come dico io. Però sarebbe utile per stabilire il carico di lavoro

                   json_da_restituire:=TJSONObject.Create(['web_app_title',WEB_APP_TITLE,
                                          'web_app_version',WEB_APP_VERSION,
                                          'web_app_author',WEB_APP_AUTHOR,
                                          'error_code',error,
                                          'error_desc',error_desc,
                                          {$ifdef WINDOWS}
                                          'operating_system','WINDOWS',
                                          {$else}
                                            {$ifdef DARWIN}
                                            'operating_system','Mac OS',
                                            {$else}
                                            'operating_system','Linux',
                                            {$endif}
                                          {$endif}
                                          'web_app_date_start',DateTimeToStr(Start_Date),
                                          'web_app_current_date',DateTimeToStr(now),
                                          'jwt',mytoken]);
              end else begin
                     error       := ERR_NOUSER_CODE;
                     error_desc  := ERR_NOUSER_DESC;
              end;

       end;

  end;

  AResponse.Contents.Clear;
  AResponse.ContentType := 'text/json;charset=utf-8';

  if error <> ERR_NO_ERROR_CODE then
  begin
       json_da_restituire:=TJSONObject.Create(['web_app_title',WEB_APP_TITLE,
                              'web_app_version',WEB_APP_VERSION,
                              'web_app_author',WEB_APP_AUTHOR,
                              'error_code',error,
                              'error_desc',error_desc,
                              'jwt','']);
  end;

  LogMessage(JWTUserName(mytoken),'uMain','diagnosticRequest',RemoteHost, json_da_restituire.AsJSON);

  AResponse.Contents.Text := CustomCrypt(json_da_restituire.AsJSON);
  json_da_restituire.Free;

  Handled := true;
end;

procedure TFPWebModule1.loginRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
   json_da_restituire : TJSONObject;
   username           : string;
   pwd_hash           : string;
   mytoken            : string;
   RemoteHost         : string;
   ErrorNum           : integer;
   ErrorDesc          : string;
   position           : string;
begin
  RemoteHost := ARequest.RemoteAddr;
  mytoken    := CustomDecrypt(ARequest.QueryFields.Values['mytoken']);
  if trim(mytoken)='' then
  begin

       //login
       username := CustomDecrypt(ARequest.QueryFields.Values['username']);
       pwd_hash := CustomDecrypt(ARequest.QueryFields.Values['pwd_hash']);
       position := CustomDecrypt(ARequest.QueryFields.Values['position']);

       if trim(position)='' then
       begin
            ErrorNum  := ERR_INVALID_POSITION_CODE;
            ErrorDesc := ERR_INVALID_POSITION_DESC;
       end else begin
           mytoken  := Self.LoginWithGenerateJWT(mytoken, username, pwd_hash, position);
       end;

  end else begin

              mytoken   := Self.VerifiedJWT(mytoken);

  end;

  if trim(mytoken) = '' then
  begin
       ErrorNum  := ERR_INVALID_JWT_CODE;
       ErrorDesc := ERR_INVALID_JWT_DESC;
  end else begin
       ErrorNum  := ERR_NO_ERROR_CODE;
       ErrorDesc := ERR_NO_ERROR_DESC;
  end;

  if login_sleep_ms>0 then
     Sleep(login_sleep_ms);

  AResponse.Contents.Clear;
  AResponse.ContentType := 'text/json;charset=utf-8';

  json_da_restituire:=TJSONObject.Create(['web_app_title',WEB_APP_TITLE,
                         'web_app_version',WEB_APP_VERSION,
                         'web_app_author',WEB_APP_AUTHOR,
                         'error_code',ErrorNum,
                         'error_desc',ErrorDesc,
                         'jwt',mytoken]);

  LogMessage(JWTUserName(mytoken),'uMain','loginRequest',RemoteHost, '[' + username + ' - ' + pwd_hash + '] --> ' + json_da_restituire.AsJSON);

  AResponse.Contents.Text := CustomCrypt(json_da_restituire.AsJSON);
  json_da_restituire.Free;

  Handled := true;
end;

procedure TFPWebModule1.logoutRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
   json_da_restituire : TJSONObject;
   mytoken            : string;
   error              : integer;
   error_desc         : string;
   RemoteHost         : string;
   id_session_str     : string;
   id_session         : integer;
begin
  RemoteHost         := ARequest.RemoteAddr;
  mytoken            := CustomDecrypt(ARequest.QueryFields.Values['mytoken']);

  if trim(mytoken)='' then
  begin
       //non faccio nulla perchè vuol dire che non è loggato
       error       := ERR_INVALID_JWT_CODE;
       error_desc  := ERR_INVALID_JWT_DESC;
  end else begin

       mytoken  := Self.VerifiedJWT(mytoken);
       if trim(mytoken)='' then
       begin
            error       := ERR_INVALID_JWT_CODE;
            error_desc  := ERR_INVALID_JWT_DESC;
       end else begin

              if NGITExtractValue(mytoken, 'id_session',id_session_str, error_desc) then
              begin
                  id_session := StrToIntDef(id_session_str , -1);
                  if id_session <> -1 then
                  begin
                       MemoryMessages.LogOut(id_session);
                       error  := ERR_NO_ERROR_CODE;
                       error_desc := ERR_NO_ERROR_DESC;
                       mytoken   := '';
                  end else begin
                  	error       := ERR_GENERIC_ERROR_CODE;
                  	error_desc  := '';
                  end;
              end else begin
                  error  := ERR_GENERIC_ERROR_CODE;
              end;

       end;

  end;

  AResponse.Contents.Clear;
  AResponse.ContentType := 'text/json;charset=utf-8';

  json_da_restituire:=TJSONObject.Create(['web_app_title',WEB_APP_TITLE,
                         'web_app_version',WEB_APP_VERSION,
                         'web_app_author',WEB_APP_AUTHOR,
                         'error_code',error,
                         'error_desc',error_desc,
                         'jwt',mytoken]);

  LogMessage(JWTUserName(mytoken),'uMain','logoutRequest',RemoteHost, json_da_restituire.AsJSON);

  AResponse.Contents.Text := CustomCrypt(json_da_restituire.AsJSON);
  json_da_restituire.Free;

  Handled := true;
end;

procedure TFPWebModule1.manualRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
   json_da_restituire : TJSONObject;
   RemoteHost         : string;
begin
  RemoteHost := ARequest.RemoteAddr;
  json_da_restituire:=TJSONObject.Create(['web_app_title',WEB_APP_TITLE,
                         'web_app_version',WEB_APP_VERSION,
                         'web_app_author',WEB_APP_AUTHOR,
                         'error_code',ERR_NO_ERROR_CODE,
                         'error_desc',ERR_NO_ERROR_DESC,
                         'login_page',LOGIN_PAGE_MAN,
                         'info_page',INFO_PAGE_MAN,
                         'sendmessage_page',SENDMESSAGE_PAGE_MAN,
                         'viewmessages_page',VIEWMESSAGES_PAGE_MAN,
                         'logout_page',LOGOUT_PAGE_MAN,
                         'diagnostic_page',DIAGNOSTIC_PAGE_MAN,
                         'manual_page',MANUAL_PAGE_MAN]);

  LogMessage('','uMain','manualRequest',RemoteHost, json_da_restituire.AsJSON);

  AResponse.Contents.Text := CustomCrypt(json_da_restituire.AsJSON);
  json_da_restituire.Free;

  Handled := true;
end;

procedure TFPWebModule1.sendmessageRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
   json_da_restituire    : TJSONObject;
   mytoken               : string;
   error                 : integer;
   error_desc            : string;
   RemoteHost            : string;
   ret                   : string;
   return_value          : boolean;
   position              : string;
   typemessage           : string;
   message               : string;
   destination           : string;
   id_session_str        : string;
   id_session            : integer;
   id_destination        : integer;
begin
  RemoteHost    := ARequest.RemoteAddr;
  return_value  := false;

  mytoken            := CustomDecrypt(ARequest.QueryFields.Values['mytoken']);
  position           := CustomDecrypt(ARequest.ContentFields.Values['position']);
  typemessage        := CustomDecrypt(ARequest.ContentFields.Values['typemessage']);
  message            := CustomDecrypt(ARequest.ContentFields.Values['message']);
  destination        := CustomDecrypt(ARequest.ContentFields.Values['destination']);
  id_destination     := StrToIntDef(CustomDecrypt(ARequest.ContentFields.Values['id_destination']),-1);

  if trim(mytoken)='' then
  begin
       error       := ERR_INVALID_JWT_CODE;
       error_desc  := ERR_INVALID_JWT_DESC;
  end else begin

       mytoken  := Self.VerifiedJWT(mytoken);
       if trim(mytoken)='' then
       begin
            error       := ERR_INVALID_JWT_CODE;
            error_desc  := ERR_INVALID_JWT_DESC;
       end else begin

              if trim(position) = '' then
              begin
                   error       := ERR_INVALID_POSITION_CODE;
                   error_desc  := ERR_INVALID_POSITION_DESC;
              end else begin
                            if trim(message)= '' then
                            begin
                                 error       := ERR_EMPTY_MESSAGE_CODE;
                                 error_desc  := ERR_EMPTY_MESSAGE_DESC;
                            end else begin
                                if NGITExtractValue(mytoken, 'id_session',id_session_str, error_desc) then
                                begin
                                     id_session := StrToIntDef(id_session_str , -1);
                                     if id_session <> -1 then
                                     begin
                                          return_value := MemoryMessages.AddMessage(id_session, message, typemessage, destination, Application_with_log, id_destination);
                                          if not return_value then
                                          begin
                                               error       := ERR_INVALID_ADDMSG_CODE;
                                               error_desc  := ERR_INVALID_ADDMSG_DESC;
                                          end else begin
                                                      error       := ERR_NO_ERROR_CODE;
                                                      error_desc  := ERR_NO_ERROR_DESC;
                                          end;
                                     end else begin
                                     	error       := ERR_GENERIC_ERROR_CODE;
                                     	error_desc  := '';
                                     end;
                                end else begin
                                    error  := ERR_GENERIC_ERROR_CODE;
                                end;
                            end;
              end;


       end;

  end;

  AResponse.Contents.Clear;
  AResponse.ContentType := 'text/json;charset=utf-8';

  json_da_restituire:=TJSONObject.Create(['web_app_title',WEB_APP_TITLE,
                         'web_app_version',WEB_APP_VERSION,
                         'web_app_author',WEB_APP_AUTHOR,
                         'error_code',error,
                         'error_desc',error_desc,
                         'jwt',mytoken,
                         'return_value',BoolToStr(return_value)]);

  ret := json_da_restituire.AsJSON;


  LogMessage(JWTUserName(mytoken),'uMain','sendmessage',RemoteHost, ret);


  AResponse.Contents.Text := CustomCrypt(ret);
  json_da_restituire.Free;

  Handled := true;
end;

procedure TFPWebModule1.viewmessagesRequest(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
var
   json_da_restituire    : TJSONObject;
   mytoken               : string;
   error                 : integer;
   error_desc            : string;
   RemoteHost            : string;
   ret                   : string;
   return_value          : boolean;
   dataset_json          : string;
   ResultMessages        : TArrayOfMessageItem;
   i                     : integer;
   jArray1               : TJSONArray;
   jArray2               : TJSONArray;
   id_session_str        : string;
   id_session            : integer;
begin
  RemoteHost         := ARequest.RemoteAddr;
  return_value       := false;

  mytoken            := CustomDecrypt(ARequest.QueryFields.Values['mytoken']);

  dataset_json       := '';

  if trim(mytoken)='' then
  begin
       error       := ERR_INVALID_JWT_CODE;
       error_desc  := ERR_INVALID_JWT_DESC;
  end else begin

       mytoken  := Self.VerifiedJWT(mytoken);

       if trim(mytoken)='' then
       begin
            error       := ERR_INVALID_JWT_CODE;
            error_desc  := ERR_INVALID_JWT_DESC;
       end else begin

              if NGITExtractValue(mytoken, 'id_session',id_session_str, error_desc) then
              begin
                  id_session := StrToIntDef(id_session_str , -1);
                  if id_session <> -1 then
                  begin
                       MemoryMessages.GetMessages(id_session, ResultMessages, Application_with_log);
                       jArray1 := TJSONArray.Create;

                       for i :=0 to Length(ResultMessages)-1 do
                       begin
                            jArray2 := TJSONArray.Create;
                            jArray2.Add(ResultMessages[i].msg_text);
                            jArray2.Add(ResultMessages[i].msg_type);
                            jArray2.Add(ResultMessages[i].msg_source);
                            jArray2.Add(IntToStr(ResultMessages[i].id_connection_source));
                            jArray1.Add(jArray2.AsJSON);
                            jArray2.Free;
                            jArray2 := nil;
                       end;
                       dataset_json := jArray1.AsJSON;
                       jArray1.Free;
                       jArray1 := nil;

                       error       := ERR_NO_ERROR_CODE;
                       error_desc  := ERR_NO_ERROR_DESC;

                       return_value := true;
                  end else begin
                  	error       := ERR_GENERIC_ERROR_CODE;
                  	error_desc  := '';
                  end;
              end else begin
                  error  := ERR_GENERIC_ERROR_CODE;
              end;

       end;

  end;

  if trim(dataset_json)='' then
     dataset_json := '[]';

  AResponse.Contents.Clear;
  AResponse.ContentType := 'text/json;charset=utf-8';

  json_da_restituire:=TJSONObject.Create(['web_app_title',WEB_APP_TITLE,
                         'web_app_version',WEB_APP_VERSION,
                         'web_app_author',WEB_APP_AUTHOR,
                         'error_code',error,
                         'error_desc',error_desc,
                         'jwt',mytoken,
                         'dataset','-++D++-',
                         'return_value',BoolToStr(return_value)]);

  ret := json_da_restituire.AsJSON;

  ret := stringReplace(ret, '"-++D++-"', dataset_json, [RfReplaceAll]);

  LogMessage(JWTUserName(mytoken),'uMain','viewmessages',RemoteHost, ret);


  AResponse.Contents.Text := CustomCrypt(ret);
  json_da_restituire.Free;

  Handled := true;
end;

function TFPWebModule1.LoginWithGenerateJWT(mytoken: string; username: string;
  pwd_hash: string; position: string): string;
var
   ret          : string;
   str_expired  : string;
   date_expired : TDateTime;
   id_user      : integer;
   id_session   : integer;
begin

  ret        := '';

  //login
  if IsValidUser(username,pwd_hash, id_user) then
  begin
       id_session := MemoryMessages.AddConnection(Application_with_log, position);
       date_expired := IncMinute(Now,jwt_timeout);
       str_expired  := IntToStr(MillisecondsBetween(date_expired,0));
       ret := NGITJWTSign(jwt_password,'{ "iduser" : "' + IntToStr(id_user) + '" , "username" : "' + username + '" , "exp" : "' + str_expired + '" , "id_session" : "' + IntToStr(id_session) + '" }');
  end;

  result := ret;
end;

function TFPWebModule1.VerifiedJWT(mytoken: string): string;
var
   output          : string;
   id_user_str     : string;
   error           : string;
   id_user         : integer;
   str_expired     : string;
   date_expired    : TDateTime;
   ret             : string;
   username        : string;
   id_session_str  : string;
   id_session      : integer;
begin
  //user logged

  ret := '';
  if NGITJWTParse(mytoken, jwt_password, Output) then
  begin
       //valid token
       //refresh token exp value
        if NGITExtractValue(mytoken, 'iduser',id_user_str, error) then
        begin

             if NGITExtractValue(mytoken, 'id_session',id_session_str, error) then
             begin
                  id_user := StrToIntDef(id_user_str , -1);
                  if id_user <> -1 then
                  begin

                       id_session := StrToIntDef(id_session_str , -1);
                       if id_session <> -1 then
                       begin
                            MemoryMessages.RefreshUpdateTimeConnection(id_session);
                            NGITExtractValue(mytoken, 'username',username, error);
                            date_expired := IncMinute(Now,jwt_timeout);
                            str_expired  := IntToStr(MillisecondsBetween(date_expired,0));
                            ret := NGITJWTSign(jwt_password,'{ "iduser" : "' + IntToStr(id_user) + '" , "username" : "' + username + '" , "exp" : "' + str_expired + '" , "id_session" : "' + IntToStr(id_session) + '" }');
                       end;

                  end;
             end;

        end;

  end;
  result := ret;
end;

function TFPWebModule1.JWTFromAdmin(mytoken: string): boolean;
var
   output       : string;
   error        : string;
   id_user_str  : string;
   id_user      : integer;
   ret          : boolean;
begin
  ret := false;
  if NGITJWTParse(mytoken, jwt_password, Output) then
  begin
       //valid token

        if NGITExtractValue(mytoken, 'iduser',id_user_str, error) then
        begin

             id_user := StrToIntDef(id_user_str , -1);
             ret     := IsAdminUser(id_user);

        end;

  end;
  result := ret;
end;

function TFPWebModule1.JWTUserName(mytoken: string): string;
var
   output       : string;
   error        : string;
   id_user_str  : string;
   id_user      : integer;
   ret          : string;
begin
  ret := '';
  if NGITJWTParse(mytoken, jwt_password, Output) then
  begin
       //valid token

        if NGITExtractValue(mytoken, 'iduser',id_user_str, error) then
        begin

             id_user := StrToIntDef(id_user_str , -1);
             ret     := UserDescription(id_user)

        end;

  end;
  result := ret;
end;

function TFPWebModule1.CustomCrypt(value: string): string;
var
   CustomCyper1 : TCustomCyper;
begin
     if ApplicationCustomCryptKey<>'' then
     begin
          CustomCyper1 := TCustomCyper.Create;
          result := EncodeStringBase64(CustomCyper1.Encrypt(value,ApplicationCustomCryptKey, reversestring(ApplicationCustomCryptKey)));
          CustomCyper1.Free;
          CustomCyper1 := nil;
     end else begin
            result := EncodeStringBase64(value);
     end;
end;

function TFPWebModule1.CustomDecrypt(value: string): string;
var
   CustomCyper1 : TCustomCyper;
begin
     value := DecodeStringBase64(value);
     if ApplicationCustomCryptKey<>'' then
     begin
          CustomCyper1 := TCustomCyper.Create;
          result := CustomCyper1.Decrypt(value,ApplicationCustomCryptKey, reversestring(ApplicationCustomCryptKey));
          CustomCyper1.Free;
          CustomCyper1 := nil;
     end else begin
            result := value;
     end;
end;

initialization
  RegisterHTTPModule('TFPWebModule1', TFPWebModule1);
end.

