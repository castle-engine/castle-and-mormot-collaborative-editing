{
  Copyright 2024-2024 Michalis Kamburelis, mORMot framework contributors.
  The source code of this is mostly taken from the mORMot 2 example,
  https://github.com/synopse/mORMot2/blob/master/ex/ThirdPartyDemos/martin-doyle/02-HttpClientServerORM/src/Project02Server.dpr .

  This is free software; see the file LICENSE,
  included in this distribution, for details about the copyright.

  This is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

program castle_mormot_server_standalone;

{$apptype CONSOLE}

{$I mormot.defines.inc}

uses
  {$I mormot.uses.inc}
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.db.raw.sqlite3,
  mormot.db.raw.sqlite3.static,
  mormot.orm.core,
  mormot.rest.sqlite3,
  mormot.rest.http.server,
  SharedData;

const
  // TODO: Make configurable, like client also allows.
  HttpPort = '8077';

type
  TSampleServer = class(TRestServerDB)
  end;

var
  Model: TOrmModel;
  SampleServer: TSampleServer;
  HttpServer: TRestHttpServer;
  LogFamily: TSynLogFamily;

begin
  LogFamily := SQLite3Log.Family;
  LogFamily.Level := LOG_VERBOSE;
  LogFamily.PerThreadLog := ptIdentifiedInOnFile;
  LogFamily.EchoToConsole := LOG_VERBOSE;
  Model := CreateOrmModel;
  try
    SampleServer := TSampleServer.Create(Model, ChangeFileExt(Executable.ProgramFileName,'.db'));
    try
      SampleServer.DB.Synchronous := smOff;
      SampleServer.DB.LockingMode := lmExclusive;
      SampleServer.Server.CreateMissingTables;
      HttpServer := TRestHttpServer.Create(HttpPort,[SampleServer],'+',HTTP_DEFAULT_MODE, 4);
      HttpServer.AccessControlAllowOrigin := '*';
      try
        Writeln('Server started on port ' + HttpPort);
        Readln;
      finally
        HttpServer.Free;
      end;
    finally
      SampleServer.Free;
    end;
  finally
    Model.Free;
  end;
end.
