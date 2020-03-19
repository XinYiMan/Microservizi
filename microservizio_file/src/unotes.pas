unit uNotes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{

 Questa applicazione ha lo scopo di funzionare da microservizio web per il trasferimento di file (tipo ftp).

 1. Effettuo il login (ovvero mando l'username e l'hash [md5] della password
    Se il login è valido mi restituisce un token
 2. Quando interagisco con il microservizio gli devo passare sempre il token
    che funzionerà da garante per permetti di utilizzare i vari servizi

}

implementation

end.

