unit uFrmNumericSearch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFrmNumericSearch }

  TFrmNumericSearch = class(TForm)
    Btn_Abort: TButton;
    Btn_Ok: TButton;
    Cmb_Type: TComboBox;
    Txt_Da: TEdit;
    Txt_A: TEdit;
    procedure Btn_AbortClick(Sender: TObject);
    procedure Btn_OkClick(Sender: TObject);
    procedure Cmb_TypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Txt_AChange(Sender: TObject);
    procedure Txt_DaChange(Sender: TObject);
  private
         FSaved               : boolean;
         FIsFloatType         : boolean;
         function IsValidValue(value : string) : boolean;
  public
        function Saved : boolean;
  published
        property IsFloatType : boolean read FIsFloatType write FIsFloatType;
  end;

var
  FrmNumericSearch: TFrmNumericSearch;

CONST
  MSGABORT     = 'Annulla';
  MSGOK        = 'Ok';
  ITEM0        = 'Uguale a';
  ITEM1        = 'Minore di';
  ITEM2        = 'Maggiore di';
  ITEM3        = 'Compreso tra';
  ITEM4        = 'Escluso da';
  ERRTXT       = 'Valore "" non numerico.';

implementation

{$R *.lfm}

{ TFrmNumericSearch }

procedure TFrmNumericSearch.FormCreate(Sender: TObject);
begin
  Self.FSaved                    := false;
  Self.Btn_Abort.Caption         := MSGABORT;
  Self.Btn_Ok.Caption            := MSGOK;
  IsFloatType                    := false;
  Self.Cmb_Type.Clear;
  Self.Cmb_Type.Items.Add(ITEM0);
  Self.Cmb_Type.Items.Add(ITEM1);
  Self.Cmb_Type.Items.Add(ITEM2);
  Self.Cmb_Type.Items.Add(ITEM3);
  Self.Cmb_Type.Items.Add(ITEM4);
  Self.Cmb_Type.ItemIndex:=0;
end;

procedure TFrmNumericSearch.Txt_AChange(Sender: TObject);
begin
     if Pos(',',Self.Txt_A.Text)>0 then
        Self.Txt_A.Text := stringReplace(Self.Txt_A.Text, ',', '.', [RfReplaceAll]);
end;

procedure TFrmNumericSearch.Txt_DaChange(Sender: TObject);
begin
     if Pos(',',Self.Txt_Da.Text)>0 then
        Self.Txt_Da.Text := stringReplace(Self.Txt_Da.Text, ',', '.', [RfReplaceAll]);
end;

function TFrmNumericSearch.IsValidValue(value: string): boolean;
var
   retInt   : integer;
   retFloat : double;
begin

     try
        try
           result := true;
           if Self.IsFloatType then
              retFloat := StrToFloat(value)
           else
               retInt := StrToInt(value);

        finally

       end;
     except
           on E: Exception do
           begin

              result := false;

           end;
     end;

end;

procedure TFrmNumericSearch.Btn_OkClick(Sender: TObject);
begin
     if Self.IsValidValue(Self.Txt_Da.Text) then
     begin
       if (not Self.Txt_A.Enabled) or ((Self.Txt_A.Enabled) and (Self.IsValidValue(Self.Txt_A.Text))) then
       begin

            Self.FSaved := true;
            Close;

       end else begin
           ShowMessage(stringReplace(ERRTXT, '""', '"' + Self.Txt_A.Text + '"', [RfReplaceAll]));
       end;
     end else begin
         ShowMessage(stringReplace(ERRTXT, '""', '"' + Self.Txt_Da.Text + '"', [RfReplaceAll]));
     end;
end;

procedure TFrmNumericSearch.Cmb_TypeChange(Sender: TObject);
begin
     if (Self.Cmb_Type.ItemIndex = 3) or (Self.Cmb_Type.ItemIndex = 4) then
        Self.Txt_A.Enabled := true
     else
         Self.Txt_A.Enabled := false;
end;

procedure TFrmNumericSearch.Btn_AbortClick(Sender: TObject);
begin
     Close;
end;

function TFrmNumericSearch.Saved: boolean;
begin
     result := Self.FSaved;
end;

end.

