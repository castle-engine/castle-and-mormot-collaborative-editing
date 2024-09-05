{
  Copyright 2024-2024 Michalis Kamburelis.

  This is free software; see the file LICENSE,
  included in this distribution, for details about the copyright.

  This is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Connection to the server using mORMot. }
unit GameConnection;

interface

uses Mormot.Orm.Core, Mormot.Rest.Http.Client;

var
  HttpClient: TRestHttpClient;
  Model: TOrmModel;

implementation

uses SysUtils,
  SharedData;

initialization
  Model := CreateOrmModel;
finalization
  FreeAndNil(Model);
  FreeAndNil(HttpClient);
end.
