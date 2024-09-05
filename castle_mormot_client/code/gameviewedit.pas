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
  CastleCameras, CastleFindFiles, CastleTransform, CastleDebugTransform,
  CastleViewport;

type
  { View to edit the world. }
  TViewedit = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    FlyNavigation: TCastleWalkNavigation;
    ButtonAddRandom, ButtonAddSphere, ButtonAddBox, ButtonDelete, ButtonClearAll: TCastleButton;
    ButtonTranslate, ButtonRotate, ButtonScale: TCastleButton;
    EditableAssetsParent: TCastleTransform;
    MainViewport: TCastleViewport;
  private
    type
      TTransformMode = (tmTranslate, tmRotate, tmScale);
    var
      { List of URLs of assets that can be placed in TOrmCastleTransform. }
      EditableAssets: TStringList;
      { All instances of TCastleTransform created from TOrmCastleTransform
        will be owned by this component. They cannot be just owned by FreeAtStop
        because they need a separate owner, as their names are in a separate namespace. }
      EditableAssetsOwner: TComponent;
      VisualizeSelected, VisualizeHover: TDebugTransformBox;
      TransformMode: TTransformMode;
    procedure FoundEditableAsset(const FileInfo: TFileInfo; var StopSearch: boolean);
    procedure ClickAddRandom(Sender: TObject);
    procedure ClickAddSphere(Sender: TObject);
    procedure ClickAddBox(Sender: TObject);
    procedure ClickDelete(Sender: TObject);
    procedure ClickClearAll(Sender: TObject);
    procedure NewEditableAsset(const NewUrl: String);
    { Set Pressed state of 3 buttons based on TransformMode. }
    procedure UpdateTransformButtons;
    procedure ClickTranslate(Sender: TObject);
    procedure ClickRotate(Sender: TObject);
    procedure ClickScale(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  Viewedit: TViewedit;

implementation

uses SysUtils, Contnrs, Math,
  Mormot.Core.Unicode,
  CastleStringUtils, CastleClassUtils, CastleLog, CastleUriUtils, CastleColors,
  SharedData, GameConnection, CastleUtils;

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
  ButtonAddRandom.OnClick := {$ifdef FPC}@{$endif} ClickAddRandom;
  ButtonAddSphere.OnClick := {$ifdef FPC}@{$endif} ClickAddSphere;
  ButtonAddBox.OnClick := {$ifdef FPC}@{$endif} ClickAddBox;
  ButtonDelete.OnClick := {$ifdef FPC}@{$endif} ClickDelete;
  ButtonClearAll.OnClick := {$ifdef FPC}@{$endif} ClickClearAll;
  ButtonTranslate.OnClick := {$ifdef FPC}@{$endif} ClickTranslate;
  ButtonRotate.OnClick := {$ifdef FPC}@{$endif} ClickRotate;
  ButtonScale.OnClick := {$ifdef FPC}@{$endif} ClickScale;

  // adjust FlyNavigation settings
  FlyNavigation.Input_Jump.Assign(keyE);
  FlyNavigation.Input_Crouch.Assign(keyQ);
  // do not use arrow keys, we need to have them free to manipulate objects
  FlyNavigation.Input_Forward.MakeClear;
  FlyNavigation.Input_Backward.MakeClear;
  FlyNavigation.Input_LeftRotate.MakeClear;
  FlyNavigation.Input_RightRotate.MakeClear;

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

  { Tracking hover and selected objects.
    The TDebugTransformBox instances visualize the currently hovered over / selected
    TCastleTransform.
    They also automatically handle "what happens when Parent is freed".
    So we just use VisualizeHover.Parent and VisualizeSelected.Parent to track it. }
  VisualizeHover := TDebugTransformBox.Create(FreeAtStop);
  VisualizeHover.BoxColor := ColorOpacity(HexToColor('fffba0'), 0.25);
  VisualizeHover.Exists := true;

  VisualizeSelected := TDebugTransformBox.Create(FreeAtStop);
  VisualizeSelected.BoxColor := ColorOpacity(White, 0.25);
  VisualizeSelected.Exists := true;

  TransformMode := tmTranslate;
  UpdateTransformButtons;
end;

procedure TViewedit.Stop;
begin
  FreeAndNil(EditableAssets);
  inherited;
end;

procedure TViewedit.Update(const SecondsPassed: Single; var HandleInput: boolean);

  { Component-wise maximum of two vectors. }
  function MaxVector(const A, B: TVector3): TVector3;
  begin
    Result := Vector3(Max(A.X, B.X), Max(A.Y, B.Y), Max(A.Z, B.Z));
  end;

  { Perform transformation (given by TransformMode) on a selected object
    (given by VisualizeSelected.Parent) by Delta * SecondsPassed. }
  procedure Transform(const Delta: TVector2);
  var
    Sel: TCastleTransform; //< selected transform
  begin
    Sel := VisualizeSelected.Parent;
    case TransformMode of
      tmTranslate:
        Sel.Translation := Sel.Translation + SecondsPassed * 10 * Vector3(Delta.X, 0, Delta.Y);
      tmRotate:
        Sel.Rotation := Vector4(0, 1, 0, Sel.Rotation.W + Delta.X * SecondsPassed);
      tmScale:
        Sel.Scale := MaxVector(Vector3(0.1, 0.1, 0.1), Sel.Scale + SecondsPassed * Vector3(Delta.X, 0, Delta.Y));
      else raise EInternalError.Create('TransformMode?');
    end;
    // TODO: synch with server
  end;

begin
  inherited;

  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  FlyNavigation.MouseLook := buttonRight in Container.MousePressed;

  // update VisualizeHover
  if (MainViewport.TransformUnderMouse <> nil) and
     (MainViewport.TransformUnderMouse.Parent <> nil) and
     (MainViewport.TransformUnderMouse.Parent.Parent = EditableAssetsParent) then
    { We use TransformUnderMouse.Parent, because this TCastleTransform
      corresponds to TOrmCastleTransform, and has Tag equal ORM ID,
      which is critical to synchronize operations with the server. }
    VisualizeHover.Parent := MainViewport.TransformUnderMouse.Parent
  else
    VisualizeHover.Parent := nil;

  if VisualizeSelected.Parent <> nil then
  begin
    if Container.Pressed[keyArrowLeft] then
      Transform(Vector2(-1, 0));
    if Container.Pressed[keyArrowRight] then
      Transform(Vector2(1, 0));
    if Container.Pressed[keyArrowUp] then
      Transform(Vector2(0, -1));
    if Container.Pressed[keyArrowDown] then
      Transform(Vector2(0, 1));
  end;
end;

function TViewedit.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsMouseButton(buttonLeft) and (VisualizeHover.Parent <> nil) then
  begin
    VisualizeSelected.Parent := VisualizeHover.Parent;
    Exit(true);
  end;
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

    WritelnLog('Added random asset (name: %s, url: %s, ORM id: %d)', [
      OrmTransform.Name,
      OrmTransform.Url,
      OrmTransform.ID // this was updated by HttpClient.Orm.Add above
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

procedure TViewedit.ClickDelete(Sender: TObject);
begin
  if VisualizeSelected.Parent <> nil then
  begin
    { Remove from the server.
      We know that TCastleTransform.Tag holds the ID of the TOrmCastleTransform. }
    WriteLnLog('Deleting from server: %d', [VisualizeSelected.Parent.Tag]);
    if not HttpClient.Orm.Delete(TOrmCastleTransform, VisualizeSelected.Parent.Tag) then
      raise Exception.Create('Failed to delete from the server');
    VisualizeSelected.Parent.Free; // this also clears VisualizeSelected.Parent
  end;
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

procedure TViewedit.UpdateTransformButtons;
begin
  ButtonTranslate.Pressed := TransformMode = tmTranslate;
  ButtonRotate.Pressed := TransformMode = tmRotate;
  ButtonScale.Pressed := TransformMode = tmScale;
end;

procedure TViewedit.ClickTranslate(Sender: TObject);
begin
  TransformMode := tmTranslate;
  UpdateTransformButtons;
end;

procedure TViewedit.ClickRotate(Sender: TObject);
begin
  TransformMode := tmRotate;
  UpdateTransformButtons;
end;

procedure TViewedit.ClickScale(Sender: TObject);
begin
  TransformMode := tmScale;
  UpdateTransformButtons;
end;

end.
