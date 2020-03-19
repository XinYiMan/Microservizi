unit uCryptFunction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCPSha1, DCPcrypt2, DCPblockciphers, DCPblowfish, dcprijndael;

  type

      { TCustomCyper }

      TCustomCyper=class
            private
                  ErrorStr: string;
                  procedure SerErrorStr(val: string);
           public
                 constructor Create;
                 destructor Free;
                 function GetErrorStr(): string;
                 function IsError(): boolean;
                 function Encrypt(Original: String;Password:String; Password2: string): string;
                 function Decrypt(Original: String;Password:String; Password2: string): string;
      end;

implementation

{ TCustomCyper }

procedure TCustomCyper.SerErrorStr(val: string);
begin
     Self.ErrorStr:=val;
end;

constructor TCustomCyper.Create;
begin

end;

destructor TCustomCyper.Free;
begin

end;

function TCustomCyper.GetErrorStr: string;
begin
     result:=Self.ErrorStr;
end;

function TCustomCyper.IsError: boolean;
var
   ret: boolean;
begin
     ret:=false;
     if Length(trim(Self.ErrorStr))>0 then
        ret:=true;
     result:=ret;
end;

function TCustomCyper.Encrypt(Original: String; Password: String; Password2: string): string;
var
  BlowFish:TDCP_Blowfish;
  AES: TDCP_rijndael;
  ret: string;
begin
  ret:='';
  AES:=TDCP_rijndael.Create(nil);
  AES.InitStr(Password2,TDCP_Sha1);
  BlowFish := TDCP_Blowfish.Create(nil);
  BlowFish.InitStr(Password,TDCP_Sha1);
  ret:=Blowfish.EncryptString(Original);
  BlowFish.Burn;
  ret:=AES.EncryptString(ret);
  AES.Burn;
  Blowfish.Free;
  AES.Free;
  result:=ret;
end;

function TCustomCyper.Decrypt(Original: String; Password: String; Password2: string): string;
var
  BlowFish:TDCP_Blowfish;
  AES: TDCP_rijndael;
  ret: string;
begin
  ret:='';

  AES:=TDCP_rijndael.Create(nil);
  AES.InitStr(Password2,TDCP_Sha1);

  BlowFish := TDCP_Blowfish.Create(nil);
  BlowFish.InitStr(Password,TDCP_Sha1);

  ret:=AES.DecryptString(Original);
  AES.Burn;

  ret:=Blowfish.DecryptString(ret);
  BlowFish.Burn;

  Blowfish.Free;
  AES.Free;
  result:=ret;
end;

end.

