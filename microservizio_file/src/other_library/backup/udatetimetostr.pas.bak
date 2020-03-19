unit uDateTimeToStr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, dateutils;

function DT2Str(const dt: TDateTime): string;
function Str2DT(datestr: string; default_value : TDateTime): TDateTime;

implementation

function DT2Str(const dt: TDateTime): string;
var
  YY, MM, DD, HH, NN, SS, MS: word;
begin
  DecodeDateTime(dt, YY, MM, DD, HH, NN, SS, MS);
  Result := AddCharR('0',IntToStr(YY),4) + AddChar('0',IntToStr(MM),2) + AddChar('0',IntToStr(DD),2) + AddChar('0',IntToStr(HH),2) + AddChar('0',IntToStr(NN),2) + AddChar('0',IntToStr(SS),2) + AddChar('0',IntToStr(MS),4);
end;

function Str2DT(datestr: string; default_value: TDateTime): TDateTime;
var
  YY, MM, DD, HH, NN, SS, MS: word;
begin
     if Length(datestr)=18 then
     begin
          YY := StrToIntDef(Copy(datestr,1,4),1900);
          MM := StrToIntDef(Copy(datestr,5,2),01);
          DD := StrToIntDef(Copy(datestr,7,2),01); 
          HH := StrToIntDef(Copy(datestr,9,2),00);
          NN := StrToIntDef(Copy(datestr,11,2),00);
          SS := StrToIntDef(Copy(datestr,13,2),00);
          MS := StrToIntDef(Copy(datestr,15,4),0001);
          result := EncodeDateTime(YY, MM, DD, HH, NN, SS, MS);
     end else begin
        result := default_value;
     end;
end;

end.

