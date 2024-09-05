# Collaborative editing of 3D world using Castle Game Engine and mORMot2

## Introduction

A collaboration of two open-source projects, both using modern Pascal:

1. [Castle Game Engine](https://castle-engine.io/) - 3D and 2D game engine, with editor, lots of rendering features and powerful API. This provides local display and editing of the 3D world.

2. [mORMot 2](https://github.com/synopse/mORMot2/) - ORM / SOA / MVC framework. This provides the ORM -- a way to express the resulting world as objects that can be send over the the network and persisted on the server side.

In effect, you have a fun application where multiple people can connect and rearrange a 3D world. Example fun you can do:

- Design city from a buildings.
- Design a room from furniture.
- Arrange chess pieces on a board.

The _mORMot_ usage in this demo follows a simple example [ex/ThirdPartyDemos/martin-doyle/02-HttpClientServerORM/](https://github.com/synopse/mORMot2/tree/master/ex/ThirdPartyDemos/martin-doyle/02-HttpClientServerORM/src).

## Authors and license

Copyright 2024 by Michalis Kamburelis.

Licence: permissive 3-clause BSD license. Basically do what you want, just keep the copyright notice.

## Building

Get [mORMot 2](https://github.com/synopse/mORMot2/). Follow the quick-start guide there. In short:

```
git clone https://github.com/synopse/mORMot2/
cd static/
  wget https://synopse.info/files/mormot2static.7z
  7z x mormot2static.7z
cd ../
lazbuild packages/lazarus/mormot2.lpk
lazbuild packages/lazarus/mormot2ui.lpk
```

If you want to build using [CGE editor](https://castle-engine.io/editor) or [CGE command-line build tool](https://castle-engine.io/build_tool), then edit these files:

- `castle_mormot_client/CastleEngineManifest.xml`
- `castle_mormot_server/CastleEngineManifest.xml`

to 1. indicate the proper (absolute or relative) directory where mORMot2 is located, 2. indicate the proper path to static files, if you compile for something else than `win64`.

NOTE: There's no need to edit the `CastleEngineManifest.xml` files if you build using Lazarus or Delphi IDEs -- in this case, if you followed the mORMot2 quick-start guide, you're all set.

Both client and server can be built just like all _Castle Game Engine_ projects:

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `castle_mormot_client.lpi` (or `castle_mormot_server.lpi`) file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `castle_mormot_client.dproj` (or `castle_mormot_server.dproj`) file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
