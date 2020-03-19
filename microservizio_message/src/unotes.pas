unit uNotes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{

 Questa applicazione ha lo scopo di funzionare da microservizio web per la gestione dei messaggi tra client.

 1. Effettuo il login (ovvero mando l'username e l'hash [md5] della password
    Se il login è valido mi restituisce un token
 2. Quando interagisco con il microservizio gli devo passare sempre il token
    che funzionerà da garante per permettermi di utilizzare i vari servizi


 Serve una tabella per far funzionare il microservizio per la messaggistica.
 Esempio per

 CREATE TABLE t_messages
 (
    data text NOT NULL,
    inc_data integer NOT NULL,
    insert_datetime timestamp with time zone NOT NULL DEFAULT now(),
    source text NOT NULL,
    sessionid text NOT NULL,
    typemessage text NOT NULL,
    message text NOT NULL,
    destination text NOT NULL,
    PRIMARY KEY (data, inc_data)
 )
}

implementation

end.

