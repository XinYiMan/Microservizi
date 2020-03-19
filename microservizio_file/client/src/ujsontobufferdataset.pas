unit uJsonToBufferDataset;

{$mode objfpc}{$H+}

{

Per le informazioni su JSON guarda qui: https://www.freepascal.org/docs-html/fcl/fpjson/index.html

}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, BufDataset, db, typinfo, fpjsonrtti, variants;

type

    { TJsonToBufferDataSet }

    TJsonToBufferDataSet = class
    private
           FDataSet: TBufDataset;
           function IsValidJSON(json_str: string): boolean;
           function FieldTypeToString(AFieldType: TFieldType): String;
           function StringToFieldType(Str: String): TFieldType;
           procedure DumpJSONScalarArray(const Data:string; List:TStrings);
    public
          constructor Create(DataSet: TBufDataset);
          procedure   DeleteAllRows;
          procedure   DestroyStructure;
          function    LoadFromJsonString(json_str : string; out ErrorStr : string) : boolean;
          function    CreateStructureFromJsonString(json_str : string; out ErrorStr : string) : boolean;
          destructor  Free;

    end;

implementation

{ TJsonToBufferDataSet }

function TJsonToBufferDataSet.IsValidJSON(json_str: string): boolean;
var
   jData : TJSONData;
   ret   : boolean;
begin
     try
        try

           ret := true;
           jData:=GetJSON(json_str);

        finally

       end;
     except
           on E: Exception do
           begin

              ret := false;

           end;
     end;


     result:=ret;

end;

function TJsonToBufferDataSet.FieldTypeToString(AFieldType: TFieldType): String;
begin
     //https://forum.lazarus.freepascal.org/index.php?topic=47236.0
     Result := GetEnumName(Typeinfo(TFieldType), integer(AFieldType));
end;

function TJsonToBufferDataSet.StringToFieldType(Str: String): TFieldType;
var
  ft: Integer;
begin
     //https://forum.lazarus.freepascal.org/index.php?topic=47236.0
  ft := GetEnumValue(TypeInfo(TFieldType), str);
  if (ft >= ord(Low(TFieldType))) and (ft <= ord(High(TFieldType))) then
    Result := TFieldType(ft)
  else
    Result := ftUnknown;
    // or: raise Exception.Create('Unknown field type');
end;

procedure TJsonToBufferDataSet.DumpJSONScalarArray(const Data: string;
  List: TStrings);
var
   JS: TJSONDeStreamer;
   V : Variant;
   I : Integer;
   S : string;
begin
  JS:= TJSONDeStreamer.Create(nil);
  List.BeginUpdate;
  List.Clear;
  try
    V:=JS.JSONToVariant(data);
    for I:=VarArrayLowBound(V,1) to VarArrayHighBound(V,1) do
    begin
      case VarType(V[I]) of
        varbyte, varword,
        varInteger, varlongword: S:=IntToStr(V[I]);
        varString,varOleStr : S:=V[I];
        else
          raise Exception.Create('Unexpected item type');
      end;
      List.add(S);
    end;
  finally
    List.EndUpdate;
    JS.Free;
  end;
end;

constructor TJsonToBufferDataSet.Create(DataSet: TBufDataset);
begin
     FDataSet := DataSet;
end;

procedure TJsonToBufferDataSet.DeleteAllRows;
begin
     if Self.FDataSet.Active then
     begin
          if Self.FDataSet.RecordCount>0 then
          begin
               Self.FDataSet.First;
               while not Self.FDataSet.EOF do
                     Self.FDataSet.Delete;
          end;
     end;
end;

procedure TJsonToBufferDataSet.DestroyStructure;
var
   FDisabled : boolean;
begin
     {Devo riattivare e poi disattivare i controlli quando cancello la struttura perchè altrimenti le dgrid non aggiornano i campi visibili}
     FDisabled := Self.FDataSet.ControlsDisabled;
     if FDisabled then
        Self.FDataSet.EnableControls;

     if Self.FDataSet.Active then
     begin
          Self.FDataSet.Close;
          Self.FDataSet.FieldDefs.Clear;
          Self.FDataSet.Fields.Clear;
     end;

     if FDisabled then
        Self.FDataSet.DisableControls;
end;

function TJsonToBufferDataSet.LoadFromJsonString(json_str: string; out
  ErrorStr: string): boolean;
var
   jData       : TJSONData;
   jData2      : TJSONData;
   jObject     : TJSONObject;
   i, j        : integer;
   rows        : TJSONArray;
   ValueList   : TStringList;
begin

     result   := false;
     ErrorStr := '';

     try
        try

           if IsValidJSON(json_str) then
           begin
                jData   := GetJSON(json_str);
                jObject := TJSONObject(jData);
                if jObject.Find('RowsCols', jData2) then
                begin

                     rows := jObject.Arrays['RowsCols'];

                     for i := 0 to rows.Count-1 do
                     begin
                          if IsValidJSON(rows[i].AsJSON) then
                          begin
                               ValueList   := TStringList.Create;
                               DumpJSONScalarArray(rows[i].AsJSON, ValueList);
                               Self.FDataSet.Append;
                               for j := 0 to ValueList.Count-1 do
                               begin
                                    Self.FDataSet.Fields[j].Value := ValueList[j];
                               end;
                               Self.FDataSet.Post;
                               ValueList.Free;

                          end;

                     end;
                     rows.Free;

                     result := true;
                end;
           end;

        finally

       end;
     except
           on E: Exception do
           begin

              ErrorStr := E.Message;
              result   := false;

           end;
     end;
end;

function TJsonToBufferDataSet.CreateStructureFromJsonString(json_str: string;
  out ErrorStr: string): boolean;
var
   jData       : TJSONData;
   jData2      : TJSONData;
   jDataProp   : TJSONData;
   jObject     : TJSONObject;
   jObject2    : TJSONObject;
   fieldprop   : string;
   i           : integer;
   fields      : TJSONArray;
   fieldname   : string;
   fieldtype   : string;
   fieldlength : integer;
   fields_str  : string;
begin
     result   := false;
     ErrorStr := '';
     try
        try

           if IsValidJSON(json_str) then
           begin
                jData   := GetJSON(json_str);
                jObject := TJSONObject(jData);
                if jObject.Find('Fields', jData2) then
                begin

                     fieldname   := '';
                     fieldtype   := '';
                     fieldlength := 0;

                     fields := jObject.Arrays['Fields'];
                     for i := 0 to fields.Count-1 do
                     begin
                          fieldprop := fields[i].AsJSON;
                          if IsValidJSON(fieldprop) then
                          begin
                               jDataProp  := GetJSON(fieldprop);
                               jObject2   := TJSONObject(jDataProp);

                               fieldname    := jObject2.Get('Name','');
                               fieldtype    := 'ftString'; //jObject2.Get('Type','ftString'); //per me qui sono tutte stringhe
                               fieldlength  := StrToIntDef(jObject2.Get('Length','0'),0);
                               if fieldlength<=0 then
                                  fieldtype := 'ftMemo'; //Nessun campo deve essere lungo 0 (classico caso dei campi memo)

                               if Self.FDataSet.Active then
                                  Self.FDataSet.Close;

                               Self.FDataSet.FieldDefs.Add(fieldname,Self.StringToFieldType(fieldtype), fieldlength);
                          end;
                     end;
                     fields.Free;
                     Self.FDataSet.CreateDataset;
                     Self.FDataSet.Open;
                     result := true;
                end;
           end;

        finally

       end;
     except
           on E: Exception do
           begin

                ErrorStr := E.Message;
                result   := false;

           end;
     end;

end;

destructor TJsonToBufferDataSet.Free;
begin

end;

end.

