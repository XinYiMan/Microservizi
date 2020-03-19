unit uFrmTextSearch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFrmTextSearch }

  TFrmTextSearch = class(TForm)
    Btn_Abort: TButton;
    Btn_Ok: TButton;
    Chk_CaseSensitive: TCheckBox;
    Cmb_Type: TComboBox;
    Txt_Search: TEdit;
    procedure Btn_AbortClick(Sender: TObject);
    procedure Btn_OkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
         FSaved               : boolean;
  public
        function Saved : boolean;
  end;

var
  FrmTextSearch: TFrmTextSearch;

CONST
  MSGABORT     = 'Annulla';
  MSGOK        = 'Ok';
  MSG_CASESEN  = 'Case sensitive';
  HINT_CASESEN = 'Distingue tra maiuscole e minuscole';
  SEARCH_HINT  = 'Ricerca: ';
  ITEM0        = 'Uguale a';
  ITEM1        = 'Contiene';
  ITEM2        = 'Inizia con';
  ITEM3        = 'Finisce con';
  ERRTXT       = 'Devi inserire un valore';
  ERRCMB       = 'Devi selezionare un tipo di ricerca';

implementation

{$R *.lfm}

{ TFrmTextSearch }

procedure TFrmTextSearch.FormCreate(Sender: TObject);
begin
  Self.FSaved                    := false;

  Self.Btn_Abort.Caption         := MSGABORT;
  Self.Btn_Ok.Caption            := MSGOK;
  Self.Txt_Search.Hint           := SEARCH_HINT;
  Self.Txt_Search.TextHint       := SEARCH_HINT;
  Self.Chk_CaseSensitive.Caption := MSG_CASESEN;
  Self.Chk_CaseSensitive.Hint    := HINT_CASESEN;

  Self.Cmb_Type.Clear;
  Self.Cmb_Type.Items.Add(ITEM0);
  Self.Cmb_Type.Items.Add(ITEM1);
  Self.Cmb_Type.Items.Add(ITEM2);
  Self.Cmb_Type.Items.Add(ITEM3);
  Self.Cmb_Type.ItemIndex:=0;
end;

procedure TFrmTextSearch.FormShow(Sender: TObject);
begin
     Self.Txt_Search.SetFocus;
end;

procedure TFrmTextSearch.Btn_AbortClick(Sender: TObject);
begin
     Close;
end;

procedure TFrmTextSearch.Btn_OkClick(Sender: TObject);
begin
  if Self.Txt_Search.Text = '' then
  begin
       ShowMessage(ERRTXT);
  end else begin
       if Self.Cmb_Type.ItemIndex<>-1 then
       begin
            Self.FSaved := true;
            Close;
       end else begin
           ShowMessage(ERRCMB);
       end;
  end;
end;

function TFrmTextSearch.Saved: boolean;
begin
     result := Self.FSaved;
end;

end.

