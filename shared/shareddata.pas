{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Data types shared between client and server. }
unit SharedData;

interface

uses Mormot.Core.Base, Mormot.Orm.Base, Mormot.Orm.Core;

type
  { A 3D object that can be send over the network and persisted.
    This maps to a Castle Game Engine component
    with TCastleTransform and TCastleScene / TCastleSphere / TCastleBox.
    On mORMot side, this can be send over the network and persisted to database. }
  TOrmCastleTransform = class(TOrm)
  private
    FName: RawUTF8;

    { URL of the model file.
      May be
      @unorderedList(
        @item(@code(castle-data:/xxx) to refer to a file inside the data directory,
          these files are assumed to be available in all clients and equal.
          This results in TCastleScene with given URL.)
        @item(@code(castle-primitive:/sphere) -> TCastleSphere.)
        @item(@code(castle-primitive:/box) -> TCastleBox.)
      ) }
    FUrl: RawUTF8;

    { Translation, rotation and scale of the object.
      Used for TCastleTransform.Translation, TCastleTransform.Rotation, TCastleTransform.Scale. }
    FTranslationX: Double;
    FTranslationY: Double;
    FTranslationZ: Double;
    FRotationX: Double;
    FRotationY: Double;
    FRotationZ: Double;
    FRotationW: Double;
    FScaleX: Double;
    FScaleY: Double;
    FScaleZ: Double;
  published
    { Unique name that identifies the object.
      Will be used for TComponent.Name of CGE components. }
    property Name: RawUTF8 read FName write FName;
    property Url: RawUTF8 read FUrl write FUrl;
    property TranslationX: Double read FTranslationX write FTranslationX;
    property TranslationY: Double read FTranslationY write FTranslationY;
    property TranslationZ: Double read FTranslationZ write FTranslationZ;
    property RotationX: Double read FRotationX write FRotationX;
    property RotationY: Double read FRotationY write FRotationY;
    property RotationZ: Double read FRotationZ write FRotationZ;
    property RotationW: Double read FRotationW write FRotationW;
    property ScaleX: Double read FScaleX write FScaleX;
    property ScaleY: Double read FScaleY write FScaleY;
    property ScaleZ: Double read FScaleZ write FScaleZ;
  end;

{ Instance of TOrmModel that can deal with TOrmCastleTransform. }
function CreateOrmModel: TOrmModel;

implementation

function CreateOrmModel: TOrmModel;
begin
  Result := TOrmModel.Create([TOrmCastleTransform]);
end;

end.
