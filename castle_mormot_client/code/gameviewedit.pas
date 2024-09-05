{
  Copyright 2024-2024 Michalis Kamburelis.

  This is free software; see the file LICENSE,
  included in this distribution, for details about the copyright.

  This is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Edit the world. }
unit GameViewedit;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleCameras, CastleFindFiles, CastleTransform;

type
  { View to edit the world. }
  TViewedit = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    FlyNavigation: TCastleWalkNavigation;
    ButtonAddRandom, ButtonAddSphere, ButtonAddBox, ButtonClearAll: TCastleButton;
    EditableAssetsParent: TCastleTransform;
  private
    { List of URLs of assets that can be placed in TOrmCastleTransform. }
    EditableAssets: TStringList;
    { All instances of TCastleTransform created from TOrmCastleTransform
      will be owned by this component. They cannot be just owned by FreeAtStop
      because they need a separate owner, as their names are in a separate namespace. }
    EditableAssetsOwner: TComponent;
    procedure FoundEditableAsset(const FileInfo: TFileInfo; var StopSearch: boolean);
    procedure ClickAddRandom(Sender: TObject);
    procedure ClickAddSphere(Sender: TObject);
    procedure ClickAddBox(Sender: TObject);
    procedure ClickClearAll(Sender: TObject);
    procedure NewEditableAsset(const NewUrl: String);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  end;

var
  Viewedit: TViewedit;

implementation

uses SysUtils, Contnrs,
  Mormot.Core.Unicode,
  CastleStringUtils, CastleClassUtils, CastleLog, CastleUriUtils,
  SharedData, GameConnection;

constructor TViewedit.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewedit.castle-user-interface';
end;

procedure TViewedit.Start;
var
  AllOrmTransforms: TObjectList;
  OrmTransformObj: Pointer;
  OrmTransform: TOrmCastleTransform;
  Transform: TCastleTransform;
begin
  inherited;

  // assign events
  ButtonAddRandom.OnClick := @ClickAddRandom;
  ButtonAddSphere.OnClick := @ClickAddSphere;
  ButtonAddBox.OnClick := @ClickAddBox;
  ButtonClearAll.OnClick := @ClickClearAll;

  // adjust FlyNavigation settings
  FlyNavigation.Input_Jump.Assign(keyE);
  FlyNavigation.Input_Crouch.Assign(keyQ);

  // calculate EditableAssets
  EditableAssets := TStringList.Create;
  FindFiles('castle-data:/editable_assets/', '*.gltf', false,
    {$ifdef FPC}@{$endif} FoundEditableAsset, [ffRecursive]);
  WritelnLog('Found %d editable assets', [EditableAssets.Count]);

  // synchronize from server the initial world state
  EditableAssetsOwner := TComponent.Create(FreeAtStop);
  AllOrmTransforms := HttpClient.RetrieveList(TOrmCastleTransform, '', []);
  if AllOrmTransforms = nil then
    raise Exception.Create('Failed to retrieve data, is the server running?');
  for OrmTransformObj in AllOrmTransforms do
  begin
    OrmTransform := TObject(OrmTransformObj) as TOrmCastleTransform;
    Transform := OrmTransform.CreateTransform(EditableAssetsOwner);
    EditableAssetsParent.Add(Transform);
    // FreeAndNil(OrmTransform); // TODO test
  end;
  FreeAndNil(AllOrmTransforms);
end;

procedure TViewedit.Stop;
begin
  FreeAndNil(EditableAssets);
  inherited;
end;

procedure TViewedit.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;

  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  FlyNavigation.MouseLook := buttonRight in Container.MousePressed;
end;

procedure TViewedit.FoundEditableAsset(const FileInfo: TFileInfo; var StopSearch: boolean);
var
  Url: String;
begin
  Url := FileInfo.Url;
  Url := MaybeUseDataProtocol(Url);
  if UriProtocol(Url) <> 'castle-data' then
    raise Exception.CreateFmt('File found in data, but failed to convert URL to be relative to data: %s', [
      UriDisplay(Url)
    ]);
  EditableAssets.Add(Url);
  //WritelnLog('Found editable asset: ' + Url);
end;

procedure TViewedit.NewEditableAsset(const NewUrl: String);

  function MakeValidPascalIdent(const S: String): String;
  begin
    Result := SDeleteChars(S, AllChars - ['a'..'z', 'A'..'Z', '0'..'9', '_']);

    // cannot be empty
    if Result = '' then
      Result := 'Component';

    // cannot start with a digit
    if SCharIs(Result, 1, ['0'..'9']) then
      Result := 'Component' + Result;
  end;

var
  OrmTransform: TOrmCastleTransform;
  BaseName: String;
  Transform: TCastleTransform;
begin
  OrmTransform := TOrmCastleTransform.Create;
  try
    OrmTransform.Url := StringToUTF8(NewUrl);
    OrmTransform.TranslationX := Random * 10;
    //OrmTransform.TranslationY := Random * 10; // don't randomize, keep on floor
    OrmTransform.TranslationZ := Random * 10;
    // normal uniform scale
    OrmTransform.ScaleX := 1;
    OrmTransform.ScaleY := 1;
    OrmTransform.ScaleZ := 1;

    BaseName := DeleteUriExt(ExtractUriName(NewUrl));
    BaseName := MakeValidPascalIdent(BaseName);
    BaseName := ProposeComponentName(TCastleTransform, EditableAssetsOwner, BaseName);
    OrmTransform.Name := StringToUTF8(BaseName);

    if HttpClient.Orm.Add(OrmTransform, true) = 0 then
      raise Exception.Create('Failed to add new asset to the server');

    WritelnLog('Added random asset (name: %s, url: %s)', [
      OrmTransform.Name,
      OrmTransform.Url
    ]);

    Transform := OrmTransform.CreateTransform(EditableAssetsOwner);
    EditableAssetsParent.Add(Transform);
  finally FreeAndNil(OrmTransform) end;
end;

procedure TViewedit.ClickAddRandom(Sender: TObject);
begin
  NewEditableAsset(EditableAssets[Random(EditableAssets.Count)]);
end;

procedure TViewedit.ClickAddSphere(Sender: TObject);
begin
  NewEditableAsset('castle-primitive:/sphere');
end;

procedure TViewedit.ClickAddBox(Sender: TObject);
begin
  NewEditableAsset('castle-primitive:/box');
end;

procedure TViewedit.ClickClearAll(Sender: TObject);
begin
  while EditableAssetsParent.Count > 0 do
    EditableAssetsParent[0].Free; // this also removes from the list
  { Remove all from the server.
    This way of removing means we also remove invalid ORM data,
    that was not reflected in any TCastleTransform instance, but was on server. }
  HttpClient.Orm.Delete(TOrmCastleTransform, '1=1', []);
end;

end.
