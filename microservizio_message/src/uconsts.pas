unit uConsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

CONST
     WEB_APP_TITLE   = 'ngit_microservice_message';
     WEB_APP_VERSION = '0.0.1';
     WEB_APP_AUTHOR  = 'Francesco Sammarco';

     ERR_SERVER_UNREACHABLE_CODE = -2;
     ERR_SERVER_UNREACHABLE_DESC = 'Server unreachable';

     ERR_GENERIC_ERROR_CODE = -1;

     ERR_NO_ERROR_CODE = 0;
     ERR_NO_ERROR_DESC = '';

     ERR_INVALID_JWT_CODE = 1;
     ERR_INVALID_JWT_DESC = 'Invalid token';

     ERR_INVALID_HTML_CODE = 2;
     ERR_INVALID_HTML_DESC = 'Invalid HTML';

     ERR_NOUSER_CODE = 3;
     ERR_NOUSER_DESC = 'The user is not enabled';

     ERR_EMPTY_MESSAGE_CODE = 4;
     ERR_EMPTY_MESSAGE_DESC = 'Message value is empty.';

     ERR_INVALID_POSITION_CODE = 5;
     ERR_INVALID_POSITION_DESC = 'Invalid position';

     ERR_INVALID_ADDMSG_CODE = 6;
     ERR_INVALID_ADDMSG_DESC = 'Memory error';


     LOGIN_PAGE_MAN        = 'I have to pass three values (get). mytoken = '''', username and hash (md5) of the password. I return the token that certifies the validity of the connection.';
     INFO_PAGE_MAN         = 'Returns some useful system information. No parameters required.';
     DIAGNOSTIC_PAGE_MAN   = 'Login request. Returns some information necessary for program diagnostics. You must be a user with administrative privileges.';
     MANUAL_PAGE_MAN       = 'This page.';
     SENDMESSAGE_PAGE_MAN  = 'Allows you to send messages to other clients. It requires the following parameters: mytoken (which is used to legitimize the request), position (is used to indicate the origin), destination (is used to specify who to send the message to, if an empty string is passed it forwards the message to all connected clients) , typemessage (identifies the type of message), message (is the message to be forwarded). I return the token that certifies the validity of the connection.';
     VIEWMESSAGES_PAGE_MAN = 'It returns the messages addressed to my session that I have yet to read. It only requires the mytoken parameter which is used to legitimize the request. I return the token that certifies the validity of the connection.';
     LOGOUT_PAGE_MAN       = 'I log out. Removes my connection to the messaging system. It only requires the mytoken parameter which is used to legitimize the request. I return the token that certifies the validity of the connection.';

implementation

end.

