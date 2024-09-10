{
  Copyright 2024-2024 Michalis Kamburelis.

  This is free software; see the file LICENSE,
  included in this distribution, for details about the copyright.

  This is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Data types shared between client and server. }
unit SharedData;

interface

uses Classes,
  Mormot.Core.Base, Mormot.Orm.Base, Mormot.Orm.Core, Mormot.Core.Os,
  CastleTransform;

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
  public
    { Create TCastleTransform instance corresponding to this ORM state.

      Owner is the owner of the created TCastleTransform instance.
      Caller is responsible for freeing the result.

      Inside it, there may be a child (like TCastleScene) created,
      which is owned by the returned TCastleTransform instance. }
    function CreateTransform(const Owner: TComponent): TCastleTransform;

    { Update an existing TCastleTransform instance to reflect state of this ORM. }
    procedure UpdateToTransform(const Instance: TCastleTransform);

    { Set the state of this ORM to reflect the state of the given TCastleTransform.
      This is the reverse of UpdateToTransform.
      Note that it doesn't synchronize ID, as ID is not supposed to be changed this way. }
    procedure UpdateFromTransform(const Instance: TCastleTransform);
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

uses SysUtils,
  Mormot.Core.Unicode,
  CastleUriUtils, CastleScene, CastleVectors, CastleLog, CastleUtils;

function CreateOrmModel: TOrmModel;
begin
  Result := TOrmModel.Create([TOrmCastleTransform]);
end;

{ TOrmCastleTransform ------------------------------------------------------- }

function TOrmCastleTransform.CreateTransform(const Owner: TComponent): TCastleTransform;
begin
  Result := TCastleTransform.Create(Owner);
  UpdateToTransform(Result);
end;

procedure TOrmCastleTransform.UpdateToTransform(const Instance: TCastleTransform);

  procedure ClearChildren;
  begin
    while Instance.Count > 0 do
      Instance[0].Free; // this also removes from the list
  end;

  function MakeChildClass(const TransformClass: TCastleTransformClass): TCastleTransform;
  begin
    if (Instance.Count > 0) and (Instance[0] is TransformClass) then
      Result := Instance[0]
    else
    begin
      ClearChildren;
      Result := TransformClass.Create(Instance);
      Instance.Add(Result);
    end;
  end;

var
  Scene: TCastleScene;
  Sphere: TCastleSphere;
  Box: TCastleBox;
  UrlString: String;
begin
  {$ifdef CPU32}
    {$message warn 'TODO: This code is not safe on 32-bit platforms, as it assumes Tag can hold 64-bit ID.'}
  {$endif}
  Instance.Tag := ID;
  Instance.Name := Utf8ToString(FName);
  Instance.Translation := Vector3(FTranslationX, FTranslationY, FTranslationZ);
  Instance.Rotation := Vector4(FRotationX, FRotationY, FRotationZ, FRotationW);
  Instance.Scale := Vector3(FScaleX, FScaleY, FScaleZ);
  UrlString := Utf8ToString(FUrl);
  if UrlString = 'castle-primitive:/sphere' then
  begin
    Sphere := MakeChildClass(TCastleSphere) as TCastleSphere;
    Sphere.PreciseCollisions := true;
  end else
  if UrlString = 'castle-primitive:/box' then
  begin
    Box := MakeChildClass(TCastleBox) as TCastleBox;
    Box.PreciseCollisions := true;
  end else
  if UriProtocol(UrlString) = 'castle-data' then
  begin
    Scene := MakeChildClass(TCastleScene) as TCastleScene;
    Scene.PreciseCollisions := true;
    Scene.Url := UrlString;
    // just to look nice, play the first animation
    if Scene.AnimationsList.Count > 0 then
      Scene.PlayAnimation(Scene.AnimationsList[0], true);
  end else
  begin
    ClearChildren;
    WritelnWarning('Invalid URL for TOrmCastleTransform, ignoring: %s', [
      UriDisplay(UrlString)
    ]);
  end;
end;

procedure TOrmCastleTransform.UpdateFromTransform(const Instance: TCastleTransform);
var
  Child: TCastleTransform;
begin
  // ID := Instance.Tag; // cannot synchronize ID this way
  FName := StringToUTF8(Instance.Name);
  FTranslationX := Instance.Translation.X;
  FTranslationY := Instance.Translation.Y;
  FTranslationZ := Instance.Translation.Z;
  FRotationX := Instance.Rotation.X;
  FRotationY := Instance.Rotation.Y;
  FRotationZ := Instance.Rotation.Z;
  FRotationW := Instance.Rotation.W;
  FScaleX := Instance.Scale.X;
  FScaleY := Instance.Scale.Y;
  FScaleZ := Instance.Scale.Z;
  if Instance.Count > 0 then
  begin
    Child := Instance[0];
    if Child is TCastleScene then
      FUrl := StringToUTF8(TCastleScene(Child).Url)
    else
    if Child is TCastleSphere then
      FUrl := 'castle-primitive:/sphere'
    else
    if Child is TCastleBox then
      FUrl := 'castle-primitive:/box'
    else
      raise EInternalError.Create('Unexpected child class of TCastleTransform');
  end else
  begin
    WritelnWarning('No child of TCastleTransform, setting empty URL -- this is not a valid state');
    FUrl := '';
  end;
end;

end.
