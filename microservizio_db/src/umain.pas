unit uMain;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, PQConnection, SQLDB, db,
  oracleconnection, IBConnection, SQLite3Conn, mysql57conn, uExtended_fphttpapp,
  uCryptFunction, strutils;

type

  { TFPWebModule1 }

  TFPWebModule1 = class(TFPWebModule)
    procedure current_timestampRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure diagnosticRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure executeRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure getfieldfromtableRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure infoRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure loginRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure manualRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure scriptRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure selectRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
         function LoginWithGenerateJWT(mytoken: string; username: string;
           pwd_hash: string): string;
         function VerifiedJWT(mytoken : string) : string;
         function JWTFromAdmin(mytoken: string): boolean;
         function JWTUserName(mytoken: string): string;
         function GetRecordCount(sql: string; DataBase: TDataBase): integer;

         function OpenDBConnection(out ConnPostgres: TPQConnection; out ConnFirebird: TIBConnection; out ConnMySQL: TMySQL57Connection; out ConnOracle: TOracleConnection; out ConnSQLite: TSQLite3Connection; out Transaction: TSQLTransaction; out Query: TSQLQuery; out SqlScript: TSQLScript; out ErrorDescription: string): integer;
         procedure CloseDbConnection(out ConnPostgres: TPQConnection; out ConnFirebird: TIBConnection; out ConnMySQL: TMySQL57Connection; out ConnOracle: TOracleConnection; out ConnSQLite: TSQLite3Connection; out Transaction: TSQLTransaction; out Query: TSQLQuery; out SqlScript: TSQLScript);
         function CustomCrypt(value : string) : string;
         function CustomDecrypt(value : string) : string;
  public

  end;

var
  FPWebModule1: TFPWebModule1;

implementation
uses
    fpjson, jsonparser, base64, NGIT_Crypto_JWT, uConsts, dateutils, uGenericFunctions, uNGITJsonDataSet,
    uDateTimeToStr;

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

procedure TFPWebModule1.executeRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
   json_da_restituire : TJSONObject;
   mytoken            : string;
   sql                : string;
   error              : integer;
   error_desc         : string;
   errordb_desc       : string;
   errordb_num        : integer;
   RemoteHost         : string;
   ConnPostgres       : TPQConnection;
   ConnFirebird       : TIBConnection;
   ConnMySQL          : TMySQL57Connection;
   ConnOracle         : TOracleConnection;
   ConnSQLite         : TSQLite3Connection;
   Transaction        : TSQLTransaction;
   Query              : TSQLQuery;
   SqlScript          : TSQLScript;
begin
  RemoteHost := ARequest.RemoteAddr;
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
              error       := ERR_NO_ERROR_CODE;
              error_desc  := ERR_NO_ERROR_DESC;
              errordb_num := Self.OpenDBConnection(ConnPostgres,ConnFirebird,ConnMySQL,ConnOracle,ConnSQLite,Transaction,Query,SqlScript,errordb_desc);
              if errordb_num<>ERR_NO_ERROR_CODE then
              begin
                   error       := errordb_num;
                   error_desc  := trim(error_desc);
              end else begin

                            Query.SQL.Text := sql;
                            try
                               try

                                  Query.ExecSQL;
                                  Transaction.Commit;

                               finally


                              end;
                            except
                                  on E: Exception do
                                  begin

                                         Transaction.Rollback;
                                         error       := ERR_EXEC_QUERY_CODE;
                                         error_desc  := ERR_EXEC_QUERY_DESC + ': ' + trim(E.Message);

                                  end;
                            end;


                   CloseDbConnection(ConnPostgres,ConnFirebird,ConnMySQL,ConnOracle,ConnSQLite,Transaction,Query,SqlScript);
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

  LogMessage(JWTUserName(mytoken),'uMain','executeRequest',RemoteHost, '[' + sql + '] --> ' + json_da_restituire.AsJSON);

  AResponse.Contents.Text := CustomCrypt(json_da_restituire.AsJSON);
  json_da_restituire.Free;

  Handled := true;
end;

procedure TFPWebModule1.getfieldfromtableRequest(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
var
   json_da_restituire : TJSONObject;
   mytoken            : string;
   sql                : string;
   error              : integer;
   error_desc         : string;
   errordb_desc       : string;
   errordb_num        : integer;
   ret                : string;
   field              : string;
   table              : string;
   condition          : string;
   value              : string;
   RemoteHost         : string;
   ConnPostgres       : TPQConnection;
   ConnFirebird       : TIBConnection;
   ConnMySQL          : TMySQL57Connection;
   ConnOracle         : TOracleConnection;
   ConnSQLite         : TSQLite3Connection;
   Transaction        : TSQLTransaction;
   Query              : TSQLQuery;
   SqlScript          : TSQLScript;
begin
  RemoteHost := ARequest.RemoteAddr;
  mytoken              := CustomDecrypt(ARequest.QueryFields.Values['mytoken']);
  field                := CustomDecrypt(ARequest.ContentFields.Values['field']);
  table                := CustomDecrypt(ARequest.ContentFields.Values['table']);
  condition            := CustomDecrypt(ARequest.ContentFields.Values['condition']);

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

              if trim(field) = '' then
              begin
                   error       := ERR_EMPTY_FIELD_CODE;
                   error_desc  := ERR_EMPTY_FIELD_DESC;
              end else begin
                  if trim(table) = '' then
                  begin
                       error       := ERR_EMPTY_TABLE_CODE;
                       error_desc  := ERR_EMPTY_TABLE_DESC;
                  end else begin

                      errordb_num := Self.OpenDBConnection(ConnPostgres,ConnFirebird,ConnMySQL,ConnOracle,ConnSQLite,Transaction,Query,SqlScript,errordb_desc);
                      if errordb_num<>ERR_NO_ERROR_CODE then
                      begin
                           error       := errordb_num;
                           error_desc  := trim(error_desc);
                      end else begin

                                    if trim(condition)<>'' then
                                       if Pos('WHERE ', uppercase(condition))<=0 then
                                          condition := 'where ' + condition;

                                    sql := trim('select ' + field + ' from ' + table + ' ' + condition);

                                    Query.SQL.Text := sql;
                                    try
                                       try

                                          Query.Open;
                                          LogMessage(JWTUserName(mytoken),'uMain','getfieldfromtableRequest',RemoteHost, 'Opened query: ' + sql);

                                          if not Query.EOF then
                                          begin
                                               Query.First;
                                               value := Query.Fields[0].AsString;
                                          end else begin
                                               value := '';
                                          end;

                                       finally

                                         if Query.Active then
                                            Query.Close;

                                      end;
                                    except
                                          on E: Exception do
                                          begin

                                                 error       := ERR_OPEN_QUERY_CODE;
                                                 error_desc  := ERR_OPEN_QUERY_DESC + ': ' + trim(E.Message);

                                          end;
                                    end;


                           CloseDbConnection(ConnPostgres,ConnFirebird,ConnMySQL,ConnOracle,ConnSQLite,Transaction,Query,SqlScript);
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
                         'return_value',value]);

  ret := json_da_restituire.AsJSON;

  LogMessage(JWTUserName(mytoken),'uMain','getfieldfromtableRequest',RemoteHost, '[' + field + ',' + table + ',' + condition + '] --> ' + ret);

  AResponse.Contents.Text := CustomCrypt(ret);
  json_da_restituire.Free;

  Handled := true;
end;

procedure TFPWebModule1.current_timestampRequest(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
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

              error       := ERR_NO_ERROR_CODE;
              error_desc  := ERR_NO_ERROR_DESC;

              json_da_restituire:=TJSONObject.Create(['web_app_title',WEB_APP_TITLE,
                                     'web_app_version',WEB_APP_VERSION,
                                     'web_app_author',WEB_APP_AUTHOR,
                                     'error_code',error,
                                     'error_desc',error_desc,
                                     'current_timestamp',DT2Str(now),
                                     'jwt',mytoken]);

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
                         'select_page',SELECT_PAGE_MAN,
                         'execute_page',EXECUTE_PAGE_MAN,
                         'script_page',SCRIPT_PAGE_MAN,
                         'diagnostic_page',DIAGNOSTIC_PAGE_MAN,
                         'manual_page',MANUAL_PAGE_MAN]);

  LogMessage('','uMain','manualRequest',RemoteHost, json_da_restituire.AsJSON);

  AResponse.Contents.Text := CustomCrypt(json_da_restituire.AsJSON);
  json_da_restituire.Free;

  Handled := true;
end;

procedure TFPWebModule1.scriptRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
   json_da_restituire : TJSONObject;
   mytoken            : string;
   sql                : string;
   error              : integer;
   error_desc         : string;
   errordb_desc       : string;
   errordb_num        : integer;
   RemoteHost         : string;
   ConnPostgres       : TPQConnection;
   ConnFirebird       : TIBConnection;
   ConnMySQL          : TMySQL57Connection;
   ConnOracle         : TOracleConnection;
   ConnSQLite         : TSQLite3Connection;
   Transaction        : TSQLTransaction;
   Query              : TSQLQuery;
   SqlScript          : TSQLScript;
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
              error       := ERR_NO_ERROR_CODE;
              error_desc  := ERR_NO_ERROR_DESC;
              errordb_num := Self.OpenDBConnection(ConnPostgres,ConnFirebird,ConnMySQL,ConnOracle,ConnSQLite,Transaction,Query,SqlScript,errordb_desc);
              if errordb_num<>ERR_NO_ERROR_CODE then
              begin
                   error       := errordb_num;
                   error_desc  := trim(error_desc);
              end else begin

                            SqlScript.Script.Text := sql;
                            try
                               try

                                  SqlScript.ExecuteScript;
                                  Transaction.Commit;

                               finally


                              end;
                            except
                                  on E: Exception do
                                  begin

                                         Transaction.Rollback;
                                         error       := ERR_EXEC_SCRIPT_CODE;
                                         error_desc  := ERR_EXEC_SCRIPT_DESC + ': ' + trim(E.Message);

                                  end;
                            end;


                   CloseDbConnection(ConnPostgres,ConnFirebird,ConnMySQL,ConnOracle,ConnSQLite,Transaction,Query,SqlScript);
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

  LogMessage(JWTUserName(mytoken),'uMain','scriptRequest',RemoteHost, '[' + sql + '] --> ' + json_da_restituire.AsJSON);

  AResponse.Contents.Text := CustomCrypt(json_da_restituire.AsJSON);
  json_da_restituire.Free;

  Handled := true;
end;

procedure TFPWebModule1.selectRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
   json_da_restituire : TJSONObject;
   mytoken            : string;
   sql                : string;
   error              : integer;
   error_desc         : string;
   errordb_desc       : string;
   errordb_num        : integer;
   NGITJsonDataSet1   : TNGITJsonDataSet;
   json_recordset     : string;
   json_datafields    : string;
   ret                : string;
   RemoteHost         : string;
   ConnPostgres       : TPQConnection;
   ConnFirebird       : TIBConnection;
   ConnMySQL          : TMySQL57Connection;
   ConnOracle         : TOracleConnection;
   ConnSQLite         : TSQLite3Connection;
   Transaction        : TSQLTransaction;
   Query              : TSQLQuery;
   SqlScript          : TSQLScript;
   qty_record         : integer;
   continue           : boolean;
begin
  RemoteHost := ARequest.RemoteAddr;

  mytoken            := CustomDecrypt(ARequest.QueryFields.Values['mytoken']);
  sql                := CustomDecrypt(ARequest.ContentFields.Values['sql']);

  json_recordset     := '';
  json_datafields    := '';
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
              errordb_num := 0;
              errordb_num := Self.OpenDBConnection(ConnPostgres,ConnFirebird,ConnMySQL,ConnOracle,ConnSQLite,Transaction,Query,SqlScript, errordb_desc);
              if errordb_num<>ERR_NO_ERROR_CODE then
              begin
                   error       := errordb_num;
                   error_desc  := trim(error_desc);
              end else begin

                            if (MaxRecordReturnWithOutError>0) then
                            begin

                                  qty_record := Self.GetRecordCount(sql , Query.DataBase);

                                  if qty_record <> -1 then
                                  begin
                                        if qty_record <= MaxRecordReturnWithOutError then
                                        begin
                                          continue := true;
                                        end
                                        else
                                        begin
                                          continue    := false;
                                          error       := ERR_DATASET_TOO_LONG_CODE;
                                          error_desc  := ERR_DATASET_TOO_LONG_DESC + IntToStr(MaxRecordReturnWithOutError);
                                        end;
                                  end else begin

                                      Transaction.EndTransaction;
                                      Transaction.StartTransaction;

                                      continue := true;
                                  end;

                            end else begin
                                 continue := true;
                            end;

                            if continue then
                            begin
                                 Query.SQL.Text := sql;
                                 try
                                    try
                                       NGITJsonDataSet1   := TNGITJsonDataSet.Create();

                                       Query.Open;

                                       LogMessage(JWTUserName(mytoken),'uMain','selectRequest',RemoteHost, 'Opened query: ' + sql);

                                       if NGITJsonDataSet1.DatasetToJSONString(Query, json_recordset, error_desc) then
                                       begin
                                            if NGITJsonDataSet1.DatasetFieldsTypeToJSONString(Query, json_datafields, error_desc) then
                                            begin
                                                 error       := ERR_NO_ERROR_CODE;
                                                 error_desc  := ERR_NO_ERROR_DESC;
                                            end else begin
                                                   error       := ERR_DATASETSTRUCTURE2JSON_CODE;
                                                   error_desc  := ERR_DATASETSTRUCTURE2JSON_DESC + ': ' + error_desc;
                                            end;


                                       end else begin
                                            error       := ERR_DATASET2JSON_CODE;
                                            error_desc  := ERR_DATASET2JSON_DESC + ': ' + error_desc;
                                       end;

                                    finally

                                      if Assigned(NGITJsonDataSet1) then
                                      begin
                                           NGITJsonDataSet1.Free;
                                           NGITJsonDataSet1 := nil;
                                      end;

                                      if Assigned(Query) then
                                      begin
                                           if Query.Active then
                                           begin
                                                Query.Close;
                                           end;
                                      end;

                                   end;
                                 except
                                       on E: Exception do
                                       begin

                                              error       := ERR_OPEN_QUERY_CODE;
                                              error_desc  := ERR_OPEN_QUERY_DESC + ': ' + trim(E.Message);

                                       end;
                                 end;
                            end;

              end;

              Self.CloseDbConnection(ConnPostgres,ConnFirebird,ConnMySQL,ConnOracle,ConnSQLite,Transaction,Query,SqlScript);
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
                         'fields','-++F++-',
                         'dataset','-++D++-']);

  ret := json_da_restituire.AsJSON;

  try
     try

        if trim(json_datafields)='' then
           json_datafields := '[]';
        if trim(json_recordset)='' then
           json_recordset := '[]';

        ret := stringReplace(ret, '"-++F++-"', json_datafields, [RfReplaceAll]);
        ret := stringReplace(ret, '"-++D++-"', json_recordset, [RfReplaceAll]);
        LogMessage(JWTUserName(mytoken),'uMain','selectRequest',RemoteHost, '[' + sql + '] --> ' + ret);

     finally

     end;
   except
         on E: Exception do
         begin

                LogMessage(JWTUserName(mytoken),'uMain','selectRequest',RemoteHost, 'stringreplace error: ' + E.Message);

         end;
   end;






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

function TFPWebModule1.GetRecordCount(sql: string; DataBase: TDataBase
  ): integer;
var
   Query : TSQLQuery;
begin
     result := 0;
     Query  := TSQLQuery.Create(nil);
     Query.DataBase     := DataBase;
     Query.SQL.Text := 'select count(*) as qta from (' + stringReplace(sql, ';', '', [RfReplaceAll]) + ') as a';

     try
        try

           Query.Open;
           if not Query.EOF then
           begin
             Query.First;
             result := Query.FieldByName('qta').AsInteger;
           end;
           Query.Close;

        finally

               if Assigned(Query) then
               begin
                     if Query.Active then
                     begin
                          Query.Close;
                     end;
                     Query.Free;
                     Query := nil;
               end;

        end;
     except
           on E: Exception do
           begin

                result := -1;

           end;
     end;
end;

function TFPWebModule1.OpenDBConnection(out ConnPostgres: TPQConnection; out
  ConnFirebird: TIBConnection; out ConnMySQL: TMySQL57Connection; out
  ConnOracle: TOracleConnection; out ConnSQLite: TSQLite3Connection; out
  Transaction: TSQLTransaction; out Query: TSQLQuery; out
  SqlScript: TSQLScript; out ErrorDescription: string): integer;
begin

     Transaction           := TSQLTransaction.Create(nil);
     Query                 := TSQLQuery.Create(nil);
     SqlScript             := TSQLScript.Create(nil);
     SqlScript.Transaction := Transaction;

     result           := ERR_NO_ERROR_CODE;
     ErrorDescription := ERR_NO_ERROR_DESC;
     Case TypeOfDbConnector Of

     'POSTGRES','POSTGRE','POSTGRESQL' :
                                       begin
                                              ConnPostgres              := TPQConnection.Create(nil);
                                              ConnPostgres.HostName     := DBHostName;
                                              //if DBPort>0 then
                                              //   Self.ConnPostgres.Port //non esiste come proprietà
                                              ConnPostgres.DatabaseName := DBName;
                                              ConnPostgres.UserName     := DBUser;
                                              ConnPostgres.Password     := DBPassword;
                                              ConnPostgres.Transaction  := Transaction;
                                              try
                                                 try

                                                    ConnPostgres.Open;
                                                    Transaction.Active := true;
                                                    Query.DataBase     := ConnPostgres;
                                                    SQLScript.DataBase := ConnPostgres;

                                                 finally

                                                end;
                                              except
                                                    on E: Exception do
                                                    begin

                                                           ErrorDescription := E.Message;
                                                           result           := ERR_DB_CONNECTION;

                                                    end;
                                              end;



                                       end;

     'FIREBIRD','FIREBIRDSQL' :
                                       begin
                                              ConnFirebird              := TIBConnection.Create(nil);
                                              ConnFirebird.HostName     := DBHostName;
                                              if DBPort>0 then
                                                 ConnFirebird.Port      := DBPort;
                                              ConnFirebird.DatabaseName := DBName;
                                              ConnFirebird.UserName     := DBUser;
                                              ConnFirebird.Password     := DBPassword;
                                              ConnFirebird.Transaction  := Transaction;
                                              try
                                                 try

                                                    ConnFirebird.Params.Clear;
                                                    with ConnFirebird.Params do
                                                    begin
                                                      Add('character_set_client=utf8');
                                                      Add('character_set_connection=utf8');
                                                      Add('character_set_database=utf8');
                                                      Add('character_set_results=utf8');
                                                      Add('character_set_server=utf8');
                                                      Add('character_set_system=utf8');
                                                      Add('collation_connection=utf8_general_ci');
                                                      Add('collation_database=utf8_general_ci');
                                                      Add('collation_server=utf8_general_ci');
                                                      Add('Codepage=utf8');
                                                    end;

                                                    Transaction.Params.Clear;
                                                    with Transaction.Params do
                                                    begin
                                                      Add('isc_tpb_read_committed');
                                                      Add('isc_tpb_wait');
                                                      //Add('isc_tpb_concurrency');
                                                      //Add('isc_tpb_nowait');
                                                    end;

                                                    ConnFirebird.Open;
                                                    Transaction.Active := true;
                                                    Query.DataBase     := ConnFirebird;
                                                    SQLScript.DataBase := ConnFirebird;

                                                 finally

                                                end;
                                              except
                                                    on E: Exception do
                                                    begin

                                                           ErrorDescription := E.Message;
                                                           result           := ERR_DB_CONNECTION;

                                                    end;
                                              end;



                                       end;

     'MYSQL' :
                                       begin
                                              ConnMySQL              := TMySQL57Connection.Create(nil);
                                              ConnMySQL.HostName     := DBHostName;
                                              if DBPort>0 then
                                                 ConnMySQL.Port         := DBPort;
                                              ConnMySQL.DatabaseName := DBName;
                                              ConnMySQL.UserName     := DBUser;
                                              ConnMySQL.Password     := DBPassword;
                                              ConnMySQL.Transaction  := Transaction;
                                              try
                                                 try

                                                    ConnMySQL.Open;
                                                    Transaction.Active := true;
                                                    Query.DataBase     := ConnMySQL;
                                                    SQLScript.DataBase := ConnMySQL;

                                                 finally

                                                end;
                                              except
                                                    on E: Exception do
                                                    begin

                                                           ErrorDescription := E.Message;
                                                           result           := ERR_DB_CONNECTION;

                                                    end;
                                              end;



                                       end;

     'ORACLE' :
                                       begin
                                              ConnOracle              := TOracleConnection.Create(nil);
                                              ConnOracle.HostName     := DBHostName;
                                              //if DBPort>0 then
                                              //   ConnOracle.Port         := DBPort;
                                              ConnOracle.DatabaseName := DBName;
                                              ConnOracle.UserName     := DBUser;
                                              ConnOracle.Password     := DBPassword;
                                              ConnOracle.Transaction  := Transaction;
                                              try
                                                 try

                                                    ConnOracle.Open;
                                                    Transaction.Active := true;
                                                    Query.DataBase     := ConnOracle;
                                                    SQLScript.DataBase := ConnOracle;

                                                 finally

                                                end;
                                              except
                                                    on E: Exception do
                                                    begin

                                                           ErrorDescription := E.Message;
                                                           result           := ERR_DB_CONNECTION;

                                                    end;
                                              end;



                                       end;
     else
       ConnSQLite              := TSQLite3Connection.Create(nil);
       ConnSQLite.HostName     := DBHostName;
       //if DBPort>0 then
       //   ConnSQLite.Port         := DBPort;
       ConnSQLite.DatabaseName := DBName;
       ConnSQLite.UserName     := DBUser;
       ConnSQLite.Password     := DBPassword;
       ConnSQLite.Transaction  := Transaction;
       try
          try

             ConnSQLite.Open;
             Transaction.Active := true;
             Query.DataBase     := ConnSQLite;
             SQLScript.DataBase := ConnSQLite;

          finally

         end;
       except
             on E: Exception do
             begin

                    ErrorDescription := E.Message;
                    result           := ERR_DB_CONNECTION;

             end;
       end;

     end;

end;

procedure TFPWebModule1.CloseDbConnection(out ConnPostgres: TPQConnection; out
  ConnFirebird: TIBConnection; out ConnMySQL: TMySQL57Connection; out
  ConnOracle: TOracleConnection; out ConnSQLite: TSQLite3Connection; out
  Transaction: TSQLTransaction; out Query: TSQLQuery; out SqlScript: TSQLScript
  );
begin

  if Query.Active then
     Query.Close;

  Transaction.Commit;
  Transaction.Active := false;


  Query.Free;
  Query := nil;

  SqlScript.Free;
  SqlScript := nil;

  Transaction.Free;
  Transaction := nil;


  Case TypeOfDbConnector Of

  'POSTGRES','POSTGRE','POSTGRESQL' :
                                    begin

                                           try

                                              ConnPostgres.Close(true);
                                              ConnPostgres.Free;
                                              ConnPostgres := nil;

                                           finally

                                          end;


                                    end;

  'FIREBIRD','FIREBIRDSQL' :
                                    begin

                                           try

                                              ConnFirebird.Close(true);
                                              ConnFirebird.Free;
                                              ConnFirebird := nil;

                                           finally

                                          end;

                                    end;

  'MYSQL' :
                                    begin

                                           try

                                              ConnMySQl.Close(true);
                                              ConnMySQl.Free;
                                              ConnMySQl := nil;

                                           finally

                                          end;

                                    end;

  'ORACLE' :
                                    begin
                                           try

                                              ConnOracle.Close(true);
                                              ConnOracle.Free;
                                              ConnOracle := nil;

                                           finally

                                          end;

                                    end;
  else

    try

       ConnSQLite.Close(true);
       ConnSQLite.Free;
       ConnSQLite := nil;

    finally

   end;

  end;
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

