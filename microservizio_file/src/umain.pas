unit uMain;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, uExtended_fphttpapp,
  uCryptFunction, strutils, FileUtil, LazFileUtils;

type
    TDirItem = object
      name  : string;
      IsDir : boolean;
      Size  : Int64;
    end;

type

  { TFPWebModule1 }

  TFPWebModule1 = class(TFPWebModule)
    procedure createdirRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure DataModuleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure deletedirRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure deletefileRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure diagnosticRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure dircontentRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure direxistsRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure dirseparatorRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure downloadfileRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure fileexistsRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure freediskspaceRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure infoRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure loginRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure manualRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure uploadfileRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
         function LoginWithGenerateJWT(mytoken: string; username: string;
           pwd_hash: string): string;
         function VerifiedJWT(mytoken : string) : string;
         function JWTFromAdmin(mytoken: string): boolean;
         function JWTUserPermission(mytoken: string): integer;
         function JWTUserName(mytoken: string): string;
         function ReadDirAndWriteIntoString(dirname : string) : string;

         function CustomCrypt(value : string) : string;
         function CustomDecrypt(value : string) : string;
         function StringHash(value : string) : string;
         function ReturnPurgedPath(value : string) : string;
  public

  end;

var
  FPWebModule1: TFPWebModule1;

implementation
uses
    fpjson, jsonparser, base64, NGIT_Crypto_JWT, uConsts, dateutils, uGenericFunctions, uNGITJsonDataSet, md5, ufunctionbase64, uDateTimeToStr;

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

procedure TFPWebModule1.fileexistsRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
   json_da_restituire : TJSONObject;
   RemoteHost         : string;
   mytoken            : string;
   filename           : string;
   return_value       : boolean;
   error              : integer;
   error_desc         : string;
   ret                : string;
begin
  RemoteHost           := ARequest.RemoteAddr;
  mytoken              := CustomDecrypt(ARequest.QueryFields.Values['mytoken']);
  filename             := CustomDecrypt(ARequest.ContentFields.Values['filename']);
  filename              := ReturnPurgedPath(File_Root + filename);

  return_value         := false;
  ret                  := '';

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

              if ExtractFileName(filename) = '' then
              begin
                   error       := ERR_FILENAME_EMPTY_CODE;
                   error_desc  := ERR_FILENAME_EMPTY_DESC;
              end else begin
                  if not FileExists(filename) then
                  begin
                       error       := ERR_FILENAME_NOTEXISTS_CODE;
                       error_desc  := ERR_FILENAME_NOTEXISTS_DESC;
                  end else begin

                              error         := ERR_NO_ERROR_CODE;
                              error_desc    := ERR_NO_ERROR_DESC;
                              return_value  := true;
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

  LogMessage(JWTUserName(mytoken),'uMain','fileexistsRequest',RemoteHost, '[' + filename + '] --> ' + ret);

  AResponse.Contents.Text := CustomCrypt(ret);
  json_da_restituire.Free;

  Handled := true;
end;

procedure TFPWebModule1.freediskspaceRequest(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
var
   json_da_restituire : TJSONObject;
   RemoteHost         : string;
   mytoken            : string;
   dirname            : string;
   return_value       : Int64;
   error              : integer;
   error_desc         : string;
   ret                : string;

   diskID             : byte;
begin
  RemoteHost           := ARequest.RemoteAddr;
  mytoken              := CustomDecrypt(ARequest.QueryFields.Values['mytoken']);

  return_value         := -1;
  ret                  := '';

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

              diskID := AddDisk(File_root);
              return_value := DiskFree(diskID);
              if return_value <>-1 then
              begin
                   error         := ERR_NO_ERROR_CODE;
                   error_desc    := ERR_NO_ERROR_DESC;
              end else begin
                     error         := ERR_DISKFREE_CODE;
                     error_desc    := ERR_DISKFREE_DESC;
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
                         'return_value',IntToStr(return_value)]);

  ret := json_da_restituire.AsJSON;

  LogMessage(JWTUserName(mytoken),'uMain','freediskspaceRequest',RemoteHost, ret);

  AResponse.Contents.Text := CustomCrypt(ret);
  json_da_restituire.Free;

  Handled := true;
end;

procedure TFPWebModule1.createdirRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
   json_da_restituire : TJSONObject;
   RemoteHost         : string;
   mytoken            : string;
   dirname            : string;
   return_value       : boolean;
   error              : integer;
   error_desc         : string;
   ret                : string;
begin
  RemoteHost           := ARequest.RemoteAddr;
  mytoken              := CustomDecrypt(ARequest.QueryFields.Values['mytoken']);
  dirname              := CustomDecrypt(ARequest.ContentFields.Values['dirname']);
  dirname              := ReturnPurgedPath(File_Root + dirname);

  return_value         := false;
  ret                  := '';

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

              if trim(dirname) = File_Root then
              begin
                   error       := ERR_DIR_EMPTY_CODE;
                   error_desc  := ERR_DIR_EMPTY_DESC;
              end else begin

                     if JWTUserPermission(mytoken) = 0 then
                     begin
                           if CreateDirRecursive(dirname) then
                           begin
                                error         := ERR_NO_ERROR_CODE;
                                error_desc    := ERR_NO_ERROR_DESC;
                                return_value  := true;
                           end else begin
                                  error         := ERR_NOCREATEDIR_CODE;
                                  error_desc    := ERR_NOCREATEDIR_DESC;
                           end;
                     end else begin
                            error         := ERR_NOUSER_CODE;
                            error_desc    := ERR_NOUSER_DESC;
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

  LogMessage(JWTUserName(mytoken),'uMain','createdirRequest',RemoteHost, '[' + dirname + '] --> ' + ret);

  AResponse.Contents.Text := CustomCrypt(ret);
  json_da_restituire.Free;

  Handled := true;
end;

procedure TFPWebModule1.DataModuleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  DeleteOldFileFromDir(Tmp_root,60);
  Handled := false;
end;

procedure TFPWebModule1.deletedirRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
   json_da_restituire : TJSONObject;
   RemoteHost         : string;
   mytoken            : string;
   dirname            : string;
   return_value       : boolean;
   error              : integer;
   error_desc         : string;
   ret                : string;
begin
  RemoteHost           := ARequest.RemoteAddr;
  mytoken              := CustomDecrypt(ARequest.QueryFields.Values['mytoken']);
  dirname              := CustomDecrypt(ARequest.ContentFields.Values['dirname']);
  dirname              := ReturnPurgedPath(File_root + dirname);

  return_value         := false;
  ret                  := '';

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

              if dirname = File_Root then
              begin
                   error       := ERR_DIR_EMPTY_CODE;
                   error_desc  := ERR_DIR_EMPTY_DESC;
              end else begin

                     if JWTUserPermission(mytoken) = 0 then
                     begin

                          if DirectoryExists(dirname) then
                          begin
                               if DeleteDirectory(dirname,false) then
                               begin
                                    error         := ERR_NO_ERROR_CODE;
                                    error_desc    := ERR_NO_ERROR_DESC;
                                    return_value  := true;
                               end else begin
                                    error         := ERR_DELETEFILE_CODE;
                                    error_desc    := ERR_DELETEFILE_DESC;
                               end;
                          end else begin
                               error         := ERR_DIR_NOTEXISTS_CODE;
                               error_desc    := ERR_DIR_NOTEXISTS_DESC;
                          end;

                     end else begin
                            error         := ERR_NOUSER_CODE;
                            error_desc    := ERR_NOUSER_DESC;
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

  LogMessage(JWTUserName(mytoken),'uMain','deletedirRequest',RemoteHost, '[' + dirname + '] --> ' + ret);

  AResponse.Contents.Text := CustomCrypt(ret);
  json_da_restituire.Free;

  Handled := true;
end;

procedure TFPWebModule1.deletefileRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
   json_da_restituire : TJSONObject;
   RemoteHost         : string;
   mytoken            : string;
   filename           : string;
   return_value       : boolean;
   error              : integer;
   error_desc         : string;
   ret                : string;
begin
  RemoteHost           := ARequest.RemoteAddr;
  mytoken              := CustomDecrypt(ARequest.QueryFields.Values['mytoken']);
  filename             := CustomDecrypt(ARequest.ContentFields.Values['filename']);
  filename             := ReturnPurgedPath(File_root + filename);
  if LastCharIsDirSeparator(filename) then
     filename := Copy(filename,1, Length(filename)-1);

  return_value         := false;
  ret                  := '';

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

              if filename = File_Root then
              begin
                   error       := ERR_FILENAME_EMPTY_CODE;
                   error_desc  := ERR_FILENAME_EMPTY_DESC;
              end else begin

                     if JWTUserPermission(mytoken) = 0 then
                     begin

                          if FileExists(filename) then
                          begin
                               if DeleteFile(filename) then
                               begin
                                    error         := ERR_NO_ERROR_CODE;
                                    error_desc    := ERR_NO_ERROR_DESC;
                                    return_value  := true;
                               end else begin
                                    error         := ERR_DELETEFILE_CODE;
                                    error_desc    := ERR_DELETEFILE_DESC;
                               end;
                          end else begin
                               error         := ERR_FILENAME_NOTEXISTS_CODE;
                               error_desc    := ERR_FILENAME_NOTEXISTS_DESC;
                          end;
                     end else begin
                            error         := ERR_NOUSER_CODE;
                            error_desc    := ERR_NOUSER_DESC;
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

  LogMessage(JWTUserName(mytoken),'uMain','deletefileRequest',RemoteHost, '[' + filename + '] --> ' + ret);

  AResponse.Contents.Text := CustomCrypt(ret);
  json_da_restituire.Free;

  Handled := true;

end;

procedure TFPWebModule1.diagnosticRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
   json_da_restituire : TJSONObject;
   mytoken            : string;
   sql                : string;
   error              : integer;
   error_desc         : string;
   RemoteHost         : string;
begin
  RemoteHost         := ARequest.RemoteAddr;
  mytoken            := CustomDecrypt(ARequest.QueryFields.Values['mytoken']);
  sql                := CustomDecrypt(ARequest.ContentFields.Values['sql']);
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

  LogMessage(JWTUserName(mytoken),'uMain','diagnosticRequest',RemoteHost, '[' + sql + '] --> ' + json_da_restituire.AsJSON);

  AResponse.Contents.Text := CustomCrypt(json_da_restituire.AsJSON);
  json_da_restituire.Free;

  Handled := true;
end;

procedure TFPWebModule1.dircontentRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
   json_da_restituire : TJSONObject;
   RemoteHost         : string;
   mytoken            : string;
   dirname            : string;
   return_value       : string;
   error              : integer;
   error_desc         : string;
   ret                : string;
begin
  RemoteHost           := ARequest.RemoteAddr;
  mytoken              := CustomDecrypt(ARequest.QueryFields.Values['mytoken']);
  dirname              := CustomDecrypt(ARequest.ContentFields.Values['dirname']);
  dirname              := ReturnPurgedPath(File_Root + dirname);

  return_value         := '[]';
  ret                  := '';

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

              if not DirectoryExists(dirname) then
              begin
                   error       := ERR_DIR_NOTEXISTS_CODE;
                   error_desc  := ERR_DIR_NOTEXISTS_DESC;
              end else begin

                          if DirIsSymbolinkLink(dirname) then
                          begin
                               error         := ERR_LINKFILE_CODE;
                               error_desc    := ERR_LINKFILE_DESC;
                          end else begin
                               error         := ERR_NO_ERROR_CODE;
                               error_desc    := ERR_NO_ERROR_DESC;
                               return_value  := ReadDirAndWriteIntoString(dirname);
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
                         'dirname', Copy(dirname, Length(File_Root)),
                         'return_value','$#---REPLACE---#$']);

  ret := json_da_restituire.AsJSON;

  ret := stringReplace(ret, '"$#---REPLACE---#$"', return_value, [RfReplaceAll]);

  LogMessage(JWTUserName(mytoken),'uMain','dircontentRequest',RemoteHost, '[' + dirname + '] --> ' + ret);

  AResponse.Contents.Text := CustomCrypt(ret);
  json_da_restituire.Free;

  Handled := true;
end;

procedure TFPWebModule1.direxistsRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
   json_da_restituire : TJSONObject;
   RemoteHost         : string;
   mytoken            : string;
   dirname            : string;
   return_value       : boolean;
   error              : integer;
   error_desc         : string;
   ret                : string;
begin
  RemoteHost           := ARequest.RemoteAddr;
  mytoken              := CustomDecrypt(ARequest.QueryFields.Values['mytoken']);
  dirname              := CustomDecrypt(ARequest.ContentFields.Values['dirname']);
  dirname              := ReturnPurgedPath(File_Root + dirname);

  return_value         := false;
  ret                  := '';

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

              if not DirectoryExists(dirname) then
              begin
                   error       := ERR_DIR_NOTEXISTS_CODE;
                   error_desc  := ERR_DIR_NOTEXISTS_DESC;
              end else begin

                          error         := ERR_NO_ERROR_CODE;
                          error_desc    := ERR_NO_ERROR_DESC;
                          return_value  := true;
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

  LogMessage(JWTUserName(mytoken),'uMain','direxistsRequest',RemoteHost, '[' + dirname + '] --> ' + ret);

  AResponse.Contents.Text := CustomCrypt(ret);
  json_da_restituire.Free;

  Handled := true;
end;

procedure TFPWebModule1.dirseparatorRequest(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
var
   json_da_restituire : TJSONObject;
   RemoteHost         : string;
   mytoken            : string;
   return_value       : string;
   error              : integer;
   error_desc         : string;
   ret                : string;
begin
  RemoteHost           := ARequest.RemoteAddr;
  mytoken              := CustomDecrypt(ARequest.QueryFields.Values['mytoken']);

  return_value         := '';
  ret                  := '';

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
              error       := ERR_NO_ERROR_CODE;
              error_desc  := ERR_NO_ERROR_DESC;

              return_value  := System.DirectorySeparator;

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
                         'return_value',return_value]);

  ret := json_da_restituire.AsJSON;

  LogMessage(JWTUserName(mytoken),'uMain','dirseparatorRequest',RemoteHost, ret);

  AResponse.Contents.Text := CustomCrypt(ret);
  json_da_restituire.Free;

  Handled := true;
end;

procedure TFPWebModule1.downloadfileRequest(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
var
   json_da_restituire : TJSONObject;
   RemoteHost         : string;
   mytoken            : string;
   filename           : string;
   return_value       : boolean;
   error              : integer;
   error_desc         : string;
   ret                : string;
   content            : string;
   tot                : integer;
   index              : integer;
   hash               : string;
begin
  RemoteHost           := ARequest.RemoteAddr;
  mytoken              := CustomDecrypt(ARequest.QueryFields.Values['mytoken']);
  filename             := CustomDecrypt(ARequest.ContentFields.Values['filename']);
  filename             := ReturnPurgedPath(File_Root + filename);
  if LastCharIsDirSeparator(filename) then
     filename := Copy(filename,1, Length(filename)-1);

  index                := StrToIntDef(CustomDecrypt(ARequest.ContentFields.Values['index']),0);
  tot                  := 1;
  hash                 := '';
  content              := '';

  return_value         := false;
  ret                  := '';

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

              if filename = File_Root then
              begin
                   error       := ERR_FILENAME_EMPTY_CODE;
                   error_desc  := ERR_FILENAME_EMPTY_DESC;
              end else begin
                  if not FileExists(filename) then
                  begin
                       error       := ERR_FILENAME_NOTEXISTS_CODE;
                       error_desc  := ERR_FILENAME_NOTEXISTS_DESC;
                  end else begin

                      if FileIsSymlink(filename) or FileIsHardLink(filename) then
                      begin
                           error         := ERR_LINKFILE_CODE;
                           error_desc    := ERR_LINKFILE_DESC;
                      end else begin
                           content := FileToBase64(filename);

                           tot := Trunc(Length(content)/MAX_FILE_NCHAR);
                           if (Length(content) mod MAX_FILE_NCHAR)<>0 then
                              Inc(tot);

                           if (index>=0) and (index<tot) then
                           begin

                                content := copy(content, (index*MAX_FILE_NCHAR)+1, MAX_FILE_NCHAR);

                                hash          := Self.StringHash(content);

                                error         := ERR_NO_ERROR_CODE;
                                error_desc    := ERR_NO_ERROR_DESC;
                                return_value  := true;
                           end else begin
                                error       := ERR_INVALIDPKG_CODE;
                                error_desc  := ERR_INVALIDPKG_DESC;
                           end;
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
                         'index_pkg',IntToStr(index),
                         'total_pkg', IntToStr(tot),
                         'file_b64', content,
                         'hash', hash,
                         'return_value',BoolToStr(return_value)]);

  ret := json_da_restituire.AsJSON;

  LogMessage(JWTUserName(mytoken),'uMain','downloadfileRequest',RemoteHost, '[' + filename + '] --> ' + ret);

  AResponse.Contents.Text := CustomCrypt(ret);
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
   ErrorDesc          : string;
   ErrorNum           : integer;
begin
  RemoteHost := ARequest.RemoteAddr;
  mytoken    := CustomDecrypt(ARequest.QueryFields.Values['mytoken']);

  if trim(mytoken)='' then
  begin
       //login

       username := CustomDecrypt(ARequest.QueryFields.Values['username']);
       pwd_hash := CustomDecrypt(ARequest.QueryFields.Values['pwd_hash']);

       mytoken  := Self.LoginWithGenerateJWT(mytoken, username, pwd_hash);

  end else begin

       mytoken  := Self.VerifiedJWT(mytoken);

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
                         'dirseparator_page',DIRSEPARATOR_PAGE_MAN,
                         'direxists_page',DIREXISTS_PAGE_MAN,
                         'fileexists_page',FILEEXISTS_PAGE_MAN,
                         'diagnostic_page',DIAGNOSTIC_PAGE_MAN,
                         'createdir_page',CREATEDIR_PAGE_MAN,
                         'deletedir_page',DELETEDIR_PAGE_MAN,
                         'deletefile_page',DELETEFILE_PAGE_MAN,
                         'dircontent_page',DIRCONTENT_PAGE_MAN,
                         'downloadfile_page',stringReplace(DOWNLOADFILE_PAGE_MAN, '[MAX_FILE_NCHAR]', IntToStr(MAX_FILE_NCHAR), [RfReplaceAll]),
                         'freediskspace_page',FREEDISK_PAGE_MAN,
                         'uploadfile_page',stringReplace(UPLOADFILE_PAGE_MAN, '[MAX_FILE_NCHAR]', IntToStr(MAX_FILE_NCHAR), [RfReplaceAll]),
                         'manual_page',MANUAL_PAGE_MAN]);



  LogMessage('','uMain','manualRequest',RemoteHost, json_da_restituire.AsJSON);

  AResponse.Contents.Text := CustomCrypt(json_da_restituire.AsJSON);
  json_da_restituire.Free;

  Handled := true;
end;

procedure TFPWebModule1.uploadfileRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
   json_da_restituire : TJSONObject;
   RemoteHost         : string;
   mytoken            : string;
   dirname            : string;
   filename           : string;
   return_value       : boolean;
   error              : integer;
   error_desc         : string;
   ret                : string;
   index              : integer;
   total              : integer;
   content_file       : string;
   hash               : string;
   tot_hash           : string;
   app_str            : string;
begin
  RemoteHost           := ARequest.RemoteAddr;
  mytoken              := CustomDecrypt(ARequest.QueryFields.Values['mytoken']);
  dirname              := CustomDecrypt(ARequest.ContentFields.Values['dirname']);
  dirname              := ReturnPurgedPath(File_Root + dirname);
  if not LastCharIsDirSeparator(dirname) then
     dirname              := dirname + System.DirectorySeparator;

  filename             := CustomDecrypt(ARequest.ContentFields.Values['filename']);
  filename             := ExtractFileName(filename);

  index                := StrToIntDef(CustomDecrypt(ARequest.ContentFields.Values['index']),-1);
  total                := StrToIntDef(CustomDecrypt(ARequest.ContentFields.Values['total']),0);
  content_file         := CustomDecrypt(ARequest.ContentFields.Values['content_file']);
  hash                 := CustomDecrypt(ARequest.ContentFields.Values['pkg_hash']);
  tot_hash             := CustomDecrypt(ARequest.ContentFields.Values['tot_hash']);

  return_value         := false;
  ret                  := '';

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


              if JWTUserPermission(mytoken) = 0 then
              begin
                    if (index=-1) or (total=0) or (total<=index) then
                    begin
                         error       := ERR_PKGUPLOAD_CODE;
                         error_desc  := ERR_PKGUPLOAD_DESC;
                    end else begin

                           if StringHash(content_file)<>hash then
                           begin
                                error       := ERR_HASH_CODE;
                                error_desc  := ERR_HASH_DESC;
                           end else begin

                               if not DirectoryExists(dirname) then
                               begin
                                    error       := ERR_DIR_NOTEXISTS_CODE;
                                    error_desc  := ERR_DIR_NOTEXISTS_DESC;
                               end else begin

                                             if (trim(dirname)<>'') and (not LastCharIsDirSeparator(trim(dirname))) then
                                                dirname := dirname + System.DirectorySeparator;

                                             if not FileExists(dirname + filename) then
                                             begin

                                                  if (index = 0) and (total = 1) then
                                                  begin
                                                       if Base64ToFile(content_file,dirname + filename) then
                                                       begin
                                                            error         := ERR_NO_ERROR_CODE;
                                                            error_desc    := ERR_NO_ERROR_DESC;
                                                            return_value  := true;
                                                       end else begin
                                                            error       := ERR_B642FILE_CODE;
                                                            error_desc  := ERR_B642FILE_DESC;
                                                       end;
                                                  end else begin

                                                            if index <> total-1 then
                                                            begin
                                                                 SaveIntoFile(Tmp_Root + RemoteHost + '_' + tot_hash, content_file, true);
                                                                 error         := ERR_NO_ERROR_CODE;
                                                                 error_desc    := ERR_NO_ERROR_DESC;
                                                                 return_value  := true;
                                                            end else begin
                                                                 LoadFromFile(Tmp_Root + RemoteHost + '_' + tot_hash, app_str);
                                                                 DeleteFile(Tmp_Root + RemoteHost + '_' + tot_hash);

                                                                 content_file := app_str + content_file;

                                                                 if StringHash(content_file)<>tot_hash then
                                                                 begin
                                                                      error       := ERR_HASH_CODE;
                                                                      error_desc  := ERR_HASH_DESC;
                                                                 end else begin
                                                                     if Base64ToFile(content_file,dirname + filename) then
                                                                     begin
                                                                          error         := ERR_NO_ERROR_CODE;
                                                                          error_desc    := ERR_NO_ERROR_DESC;
                                                                          return_value  := true;
                                                                     end else begin
                                                                          error       := ERR_B642FILE_CODE;
                                                                          error_desc  := ERR_B642FILE_DESC;
                                                                     end;
                                                                 end;

                                                            end;
                                                  end;

                                             end else begin
                                                  error       := ERR_FILEEXISTS_CODE;
                                                  error_desc  := ERR_FILEEXISTS_DESC;
                                             end;


                               end;

                           end;
                    end;
              end else begin
                     error         := ERR_NOUSER_CODE;
                     error_desc    := ERR_NOUSER_DESC;
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

  LogMessage(JWTUserName(mytoken),'uMain','uploadfileRequest',RemoteHost, '[' + dirname + filename + '] --> ' + ret);

  AResponse.Contents.Text := CustomCrypt(ret);
  json_da_restituire.Free;

  Handled := true;
end;

function TFPWebModule1.LoginWithGenerateJWT(mytoken: string; username: string;
  pwd_hash: string): string;
var
   ret          : string;
   str_expired  : string;
   date_expired : TDateTime;
   id_user      : integer;
begin

  ret        := '';

  //login
  if IsValidUser(username,pwd_hash, id_user) then
  begin
       date_expired := IncMinute(Now,jwt_timeout);
       str_expired  := IntToStr(MillisecondsBetween(date_expired,0));
       ret := NGITJWTSign(jwt_password,'{ "iduser" : "' + IntToStr(id_user) + '" , "username" : "' + username + '" , "exp" : "' + str_expired + '" }');
  end;

  result := ret;
end;

function TFPWebModule1.VerifiedJWT(mytoken: string): string;
var
   output       : string;
   id_user_str  : string;
   error        : string;
   id_user      : integer;
   str_expired  : string;
   date_expired : TDateTime;
   ret          : string;
   username     : string;
begin
  //user logged

  ret := '';
  if NGITJWTParse(mytoken, jwt_password, Output) then
  begin
       //valid token
       //refresh token exp value
        if NGITExtractValue(mytoken, 'iduser',id_user_str, error) then
        begin

             id_user := StrToIntDef(id_user_str , -1);
             if id_user <> -1 then
             begin

                  NGITExtractValue(mytoken, 'username',username, error);
                  date_expired := IncMinute(Now,jwt_timeout);
                  str_expired  := IntToStr(MillisecondsBetween(date_expired,0));
                  ret := NGITJWTSign(jwt_password,'{ "iduser" : "' + IntToStr(id_user) + '" , "username" : "' + username + '" , "exp" : "' + str_expired + '" }');

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

function TFPWebModule1.JWTUserPermission(mytoken: string): integer;
var
   output       : string;
   error        : string;
   id_user_str  : string;
   id_user      : integer;
   ret          : integer;
begin
  ret := 1;
  if NGITJWTParse(mytoken, jwt_password, Output) then
  begin
       //valid token

        if NGITExtractValue(mytoken, 'iduser',id_user_str, error) then
        begin

             id_user := StrToIntDef(id_user_str , -1);
             ret     := UserPermission(id_user);

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
             ret     := UserDescription(id_user);

        end;
  end;
  result := ret;

end;

function TFPWebModule1.ReadDirAndWriteIntoString(dirname: string): string;
var
   Info               : TSearchRec;
   jArray             : TJSONArray;
begin
     result := '';
     if (not LastCharIsDirSeparator(dirname)) and (trim(dirname)<>'') then
        dirname := dirname + System.DirectorySeparator;

     if (trim(dirname)<>'') and (DirectoryExists(dirname)) then
     begin
           If FindFirst (dirname + '*',faAnyFile and faDirectory,Info)=0 then
           begin
                Repeat
                      With Info do
                      begin
                           if ((Name='.') OR (Name='..')) then
                           begin

                           end
                           else
                           begin
                                jArray := TJSONArray.Create;
                                jArray.Add(Name);
                                jArray.Add(IntToStr(Size));
                                jArray.Add(DT2Str(TimeStamp));
                                If (Attr and faDirectory) = faDirectory then
                                begin
                                     jArray.Add('1');
                                end
                                else
                                begin
                                     jArray.Add('0');
                                end;
                                if result <> '' then
                                   result := result + ', ';
                                result := result + jArray.AsJSON;
                                jArray.Free;
                                jArray := nil;
                           end;
                      end;
                Until FindNext(info)<>0;
           end;
           FindClose(Info);
     end;
     result := '[' + result + ']';
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

function TFPWebModule1.StringHash(value: string): string;
begin
     result := MD5Print(MD5String(value));
end;

function TFPWebModule1.ReturnPurgedPath(value: string): string;
begin

     {
     result := CleanAndExpandDirectory(value);
     if Pos(File_Root, result)<>1 then
        result := File_Root;

     }

     result := CleanAndExpandDirectory(value);
     if not FileIsInPath(result,File_root) then
        result := File_root;

end;

initialization
  RegisterHTTPModule('TFPWebModule1', TFPWebModule1);
end.

