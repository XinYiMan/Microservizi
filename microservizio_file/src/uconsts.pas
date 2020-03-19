unit uConsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

CONST
     WEB_APP_TITLE   = 'ngit_microservice_file';
     WEB_APP_VERSION = '0.0.1';
     WEB_APP_AUTHOR  = 'Francesco Sammarco';

     MAX_FILE_NCHAR  = 1024*1024*1024; //characters

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

     ERR_FILENAME_EMPTY_CODE = 4;
     ERR_FILENAME_EMPTY_DESC = 'File name empty';

     ERR_FILENAME_NOTEXISTS_CODE = 5;
     ERR_FILENAME_NOTEXISTS_DESC = 'File not exists';

     ERR_DIR_EMPTY_CODE = 6;
     ERR_DIR_EMPTY_DESC = 'Dir name empty';

     ERR_DIR_NOTEXISTS_CODE = 7;
     ERR_DIR_NOTEXISTS_DESC = 'Dir not exists';

     ERR_NOUSER_CODE = 8;
     ERR_NOUSER_DESC = 'The user is not enabled';

     ERR_NOCREATEDIR_CODE = 9;
     ERR_NOCREATEDIR_DESC = 'Impossible create dir';

     ERR_DISKFREE_CODE = 10;
     ERR_DISKFREE_DESC = 'DiskFree function error';

     ERR_PKGUPLOAD_CODE = 11;
     ERR_PKGUPLOAD_DESC = 'Inconsistency between package indices';

     ERR_HASH_CODE = 12;
     ERR_HASH_DESC = 'Invalid hash';

     ERR_FILEEXISTS_CODE = 13;
     ERR_FILEEXISTS_DESC = 'File already present';

     ERR_B642FILE_CODE = 14;
     ERR_B642FILE_DESC = 'Error conversion from base64 to file';

     ERR_DELETEFILE_CODE = 15;
     ERR_DELETEFILE_DESC = 'Error when delete file';

     ERR_INVALIDPKG_CODE = 16;
     ERR_INVALIDPKG_DESC = 'Error when delete file';

     ERR_LINKFILE_CODE = 17;
     ERR_LINKFILE_DESC = 'Hard and soft links are not allowed';


     INFO_PAGE_MAN         = 'Returns some useful system information. No parameters required.';
     CREATEDIR_PAGE_MAN    = 'Login request. Returns 0 if the directory created. Requires a "mytoken" parameter (get) and dirname (post)';
     DELETEDIR_PAGE_MAN    = 'Login request. Returns 0 if the directory deleted. Requires a "mytoken" parameter (get) and dirname (post)';
     DELETEFILE_PAGE_MAN   = 'Login request. Returns 0 if the file deleted. Requires a "mytoken" parameter (get) and filename (post)';
     DIRCONTENT_PAGE_MAN   = 'Login request. Returns list of file with propertis if the directory exist. Requires a "mytoken" parameter (get) and dirname (post)';
     DOWNLOADFILE_PAGE_MAN = 'Login request. Download the file in base64 format by breaking it down into parts by [MAX_FILE_NCHAR] characters. And reassembling it and converting it on the client side. Requires a "mytoken" parameter (get) and filename (post) and index (post). Index is the number part of file.';
     FREEDISK_PAGE_MAN     = 'Login request. Returns free bytes. Requires a "mytoken" parameter (get)';
     LOGIN_PAGE_MAN        = 'I have to pass three values (get). mytoken = '''', username and hash (md5) of the password. I return the token that certifies the validity of the connection.';
     UPLOADFILE_PAGE_MAN   = 'Login request. Upload the file in base64 format by breaking it down into parts by [MAX_FILE_NCHAR] characters. And reassembling it and converting it on the server side. Requires a "mytoken" parameter (get) and filename (post) and index (post) and total (post) and content_file (post) and pkg_hash (post) and tot_hash (post). Index is the number part of file, content_file is single part of file in base64 format, pkg_hash is md5 hash of part of file, tot_hash is md5 hash of full file.';
     DIRSEPARATOR_PAGE_MAN = 'Login request. Returns the system directory separator. Requires a "mytoken" parameter (get)';
     DIREXISTS_PAGE_MAN    = 'Login request. Returns 0 if the directory does not exist. Requires a "mytoken" parameter (get)';
     FILEEXISTS_PAGE_MAN   = 'Login request. returns 0 if the file does not exist. Requires a "mytoken" parameter (get)';
     DIAGNOSTIC_PAGE_MAN   = 'Login request. Return some information necessary for program diagnostics. You must be a user with administrative privileges.';
     MANUAL_PAGE_MAN       = 'This page.';

implementation

end.

