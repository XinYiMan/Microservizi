program ngit_microservice_message;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  fphttpapp, SysUtils, uMain, uConsts, uExtended_fphttpapp,
  uGenericFunctions,
  opensslsockets, HTTPDefs, uErrorClass, uMemoryMessages;



var
   ErrorClass1    : TErrorClass;

procedure CustomShowRequestException(AResponse: TResponse; AnException: Exception; var handled: boolean);
begin
    //writeln('CustomShowRequestException: ' + AnException.Message);
end;

begin
  Randomize;

  LoadConfigIni;

  MemoryMessages := TMemoryMessages.Create;

  Application.Title := WEB_APP_TITLE;
  Application.Port  := Application_port;
  Application.OnShowRequestException := @CustomShowRequestException;

  ErrorClass1 := TErrorClass.Create;

  Application.OnException            := @ErrorClass1.CustomExceptionHandler;

  writeln(WEB_APP_TITLE);
  writeln('Version: ' + WEB_APP_VERSION);
  if Application_with_ssl then
     writeln('https://localhost:' + IntToStr(Application.Port) + '/TFPWebModule1/')
  else
      writeln('http://localhost:' + IntToStr(Application.Port) + '/TFPWebModule1/');

  if trim(ApplicationCustomCryptKey)<>'' then
     writeln('Custom crypt active');

  Application.UseSSL        := Application_with_ssl;
  Application.LegacyRouting := true;
  Application.Threaded      := True;
  Application.Initialize;
  Application.Run;

  MemoryMessages.Free;
  MemoryMessages := nil;

end.
