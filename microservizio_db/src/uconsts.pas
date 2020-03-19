unit uConsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

CONST
     WEB_APP_TITLE   = 'ngit_microservice_db';
     WEB_APP_VERSION = '0.0.1';
     WEB_APP_AUTHOR  = 'Francesco Sammarco';

     ERR_SERVER_UNREACHABLE_CODE = -2;
     ERR_SERVER_UNREACHABLE_DESC = 'Server unreachable';

     ERR_GENERIC_ERROR_CODE = -1;

     ERR_NO_ERROR_CODE = 0;
     ERR_NO_ERROR_DESC = '';

     ERR_INVALID_JWT_CODE = 1;
     ERR_INVALID_JWT_DESC = 'Invalid token';

     ERR_DATASET2JSON_CODE = 2;
     ERR_DATASET2JSON_DESC = 'Conversion dataset into json';

     ERR_INVALID_HTML_CODE = 3;
     ERR_INVALID_HTML_DESC = 'Invalid HTML';

     ERR_OPEN_QUERY_CODE = 4;
     ERR_OPEN_QUERY_DESC = 'Open query';

     ERR_EXEC_QUERY_CODE = 5;
     ERR_EXEC_QUERY_DESC = 'Execute query';

     ERR_EXEC_SCRIPT_CODE = 6;
     ERR_EXEC_SCRIPT_DESC = 'Execute script';

     ERR_NOUSER_CODE = 7;
     ERR_NOUSER_DESC = 'The user is not enabled';

     ERR_DATASETSTRUCTURE2JSON_CODE = 8;
     ERR_DATASETSTRUCTURE2JSON_DESC = 'Conversion structure dataset into json';

     ERR_EMPTY_FIELD_CODE = 9;
     ERR_EMPTY_FIELD_DESC = 'Field value is empty.';

     ERR_EMPTY_TABLE_CODE = 10;
     ERR_EMPTY_TABLE_DESC = 'Table value is empty.';

     ERR_DATASET_TOO_LONG_CODE = 11;
     ERR_DATASET_TOO_LONG_DESC = 'Dataset too long. Max length : ';

     ERR_DB_CONNECTION = 12;

     LOGIN_PAGE_MAN        = 'I have to pass three values (get). mytoken = '''', username and hash (md5) of the password. I return the token that certifies the validity of the connection.';
     INFO_PAGE_MAN         = 'Returns some useful system information. No parameters required.';
     SELECT_PAGE_MAN       = 'Login request. Returns in json format the contents of the recordset opened on the basis of the "SQL" parameter. Requires a "mytoken" parameter (get) and "sql" parameter (post)';
     EXECUTE_PAGE_MAN      = 'Login request. Execute the query populate with "SQL" parameter. Requires a "mytoken" parameter (get) and "sql" parameter (post). If the query has been executed, returns error_code = 0';
     SCRIPT_PAGE_MAN       = 'Login request. Execute the script populate with "SQL" parameter. Requires a "mytoken" parameter (get) and "sql" parameter (post). If the script has been executed, returns error_code = 0';
     DIAGNOSTIC_PAGE_MAN   = 'Login request. Returns some information necessary for program diagnostics. You must be a user with administrative privileges.';
     MANUAL_PAGE_MAN       = 'This page.';

implementation

end.

