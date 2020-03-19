unit uErrorClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

    { TErrorClass }

    TErrorClass = class
      constructor Create;
      destructor Free;
      Procedure CustomExceptionHandler(Sender : TObject; E : Exception);
    end;

implementation

{ TErrorClass }

constructor TErrorClass.Create;
begin

end;

destructor TErrorClass.Free;
begin

end;

procedure TErrorClass.CustomExceptionHandler(Sender: TObject; E: Exception);
begin
     //writeln('CustomExceptionHandler: ' + E.Message);
end;

end.

