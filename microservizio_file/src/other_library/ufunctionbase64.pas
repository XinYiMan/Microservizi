unit ufunctionbase64;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, base64;

function StreamToBase64(AInputStream: TStream): string;
function Base64ToStream(const ABase64:string; AOutStream: TStream):Boolean;
function Base64ToFile(const Base64, AFile: String): boolean;
function FileToBase64(const AFile: String): string;
function GetStringsBase64Difference(str1: string; str2: string; var modifiche_esistenti: boolean): string;
function SetStringsBase64Difference(str1: string; str2: string):string;

implementation

function StreamToBase64(AInputStream: TStream): string;
var
  OutputStream: TStringStream;
  Encoder: TBase64EncodingStream;
begin
  Result := '';

  OutputStream := TStringStream.Create('');
  Encoder := TBase64EncodingStream.Create(OutputStream);

  try
    AInputStream.Position:=0;
    Encoder.CopyFrom(AInputStream, AInputStream.Size);
    Encoder.Flush;

    Result := OutputStream.DataString;
  finally
    Encoder.Free;
    OutputStream.Free;
  end;
end;

function Base64ToStream(const ABase64:string; AOutStream: TStream):Boolean;
var
  InStream: TStringStream;
  Decoder: TBase64DecodingStream;
begin
  Result := False;
  InStream := TStringStream.Create(ABase64);
  try
    Decoder := TBase64DecodingStream.Create(InStream);
    try
       AOutStream.CopyFrom(Decoder, Decoder.Size);
       Result := True;
    finally
      Decoder.Free;
    end;
  finally
    InStream.Free;
  end;
end;

function Base64ToFile(const Base64, AFile: String): boolean;
var
  OutStream: TFileStream;
begin
      Result := False;

      OutStream := TFileStream.Create(AFile, fmCreate or fmShareExclusive);
      try
            Base64ToStream(Base64, OutStream);
            Result := True;
      finally
                Outstream.Free;
      end;
end;

function FileToBase64(const AFile: String): string;
var
  InputStream: TFileStream;
begin
  if not FileExists(AFile) then
    Exit('');

  InputStream := TFileStream.Create(AFile, fmOpenRead or fmShareDenyWrite);
  try
    Result := StreamToBase64(InputStream);
  finally
    InputStream.Free;
  end;
end;

function GetStringsBase64Difference(str1: string; str2: string;
  var modifiche_esistenti: boolean): string;
var
   ret      : string;
   i        : integer;
   len1     : integer;
   len2     : integer;
   lettera1 : string;
   lettera2 : string;
   modifica_corso : boolean;
begin
     ret:='';
     modifiche_esistenti:=false;
     len1:=Length(str1);
     len2:=Length(str2);
     modifica_corso:=false;

     ret := ret + IntToStr(len2); //la prima } stà per start, da qui so che poi inizia la stringa da processare, fino a qui si tratta solo della stringa che mi dice quanto è lunga in tutto la stringa 2

     for i:=0 to len2-1 do //sempre su len 2 perchèquello che interessa a noi è inviare le differenze per la stringa2
     begin

          if i<len1 then
             lettera1:=copy(str1,i+1,1)
          else
              lettera1:='';

          lettera2:=copy(str2,i+1,1); //c'è sempre

          if lettera1<>lettera2 then
          begin
               if modifica_corso=false then
               begin
                    modifica_corso:=true;
                    ret:=ret + '}';
                    ret:=ret+IntToStr(i);
                    ret:=ret+ '{';
                    modifiche_esistenti:=true;
               end;
               ret:=ret+lettera2;

          end else begin
               modifica_corso:=false;
          end;
     end;

     if trim(ret)<>'' then
        ret:=ret + '}';

     result:=ret;
end;

function SetStringsBase64Difference(str1: string; str2: string): string;
var
   ret         : string;
   app         : string;
   partenza    : integer;
   modifica    : string;
   lunghezza_f : integer;
begin
     ret:=str1;
     if str2<>'' then
     begin
          lunghezza_f := StrToIntDef(Copy(str2,1,Pos(str2,'}')),0);
          if lunghezza_f>0 then
          begin
               app:=Copy(str2,Pos(str2,'}'));
               if (Copy(app,1,1) = '}') or (Copy(app,1,1) = '{') then
                  app:=Copy(app,2);

               while (app<>'') do
               begin
                    partenza := StrToIntDef(Copy(app,1,Pos(app,'{')),0);
                    app:=Copy(app,Pos(app,'}'));
                    if (Copy(app,1,1) = '}') or (Copy(app,1,1) = '{') then
                       app:=Copy(app,2);

                    if partenza>0 then
                    begin
                         modifica:=Copy(app,1, Pos(app,'}'));
                         modifica:=stringReplace(modifica, '{', '', [RfReplaceAll]);
                         modifica:=stringReplace(modifica, '}', '', [RfReplaceAll]);

                         //applico la modifica
                         ret:=Copy(ret,1,partenza-1) + modifica + Copy(ret, partenza+Length(modifica));

                         app:=Copy(app,Pos(app,'{'));
                         if (Copy(app,1,1) = '}') or (Copy(app,1,1) = '{') then
                            app:=Copy(app,2);
                    end else begin
                         app:='';
                    end;

               end;

          end;
     end;
     result:=ret;
end;

end.

