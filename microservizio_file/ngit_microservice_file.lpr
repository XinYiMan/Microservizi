program ngit_microservice_file;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  fphttpapp, SysUtils, uMain, uConsts, uExtended_fphttpapp, uGenericFunctions,
  opensslsockets, HTTPDefs, uErrorClass;

var
   ErrorClass1 : TErrorClass;

procedure CustomShowRequestException(AResponse: TResponse; AnException: Exception; var handled: boolean);
begin
    //writeln('CustomShowRequestException: ' + AnException.Message);
end;

begin
  LoadConfigIni;

  Application.Title := WEB_APP_TITLE;
  Application.Port  := Application_port;
  Application.OnShowRequestException := @CustomShowRequestException;

  ErrorClass1 := TErrorClass.Create;

  Application.OnException            := @ErrorClass1.CustomExceptionHandler;

  writeln('Title: ' + WEB_APP_TITLE);
  writeln('Version: ' + WEB_APP_VERSION);
  if Application_with_ssl then
     writeln('Url: ' + 'https://localhost:' + IntToStr(Application.Port) + '/TFPWebModule1/')
  else
      writeln('Url: ' + 'http://localhost:' + IntToStr(Application.Port) + '/TFPWebModule1/');

  if trim(ApplicationCustomCryptKey)<>'' then
     writeln('Custom crypt active');

  writeln('File root: ' + File_Root);
  writeln('Tmp root: ' + Tmp_Root);

  if not DirectoryExists(File_Root) then
  begin
       writeln(ERR_DIR_NOTEXISTS_DESC + ' ' + File_Root);
  end else begin

      if not DirectoryExists(Tmp_Root) then
      begin
           writeln(ERR_DIR_NOTEXISTS_DESC + ' ' + Tmp_Root);
      end else begin

            {$ifdef Windows}
            if Application_with_ssl then
            begin
              if not DirectoryExists('c:\temp\') then
              begin
                   writeln('There is no c:\temp\ folder necessary for OpenSSL to work on windows systems. Create it and then restart the program.');
                   Application.Terminate;
              end;
            end;
            {$endif}

            Application.UseSSL        := Application_with_ssl;
            Application.LegacyRouting := true;
            Application.Threaded      := True;
            Application.Initialize;
            Application.Run;
      end;

  end;
end.
