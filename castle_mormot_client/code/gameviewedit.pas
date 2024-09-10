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
  CastleViewport, CastleTransformManipulate,
  SharedData;

type
  { View to edit the world. }
  TViewedit = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    FlyNavigation: TCastleWalkNavigation;
    ButtonAddRandom, ButtonAddSphere, ButtonAddBox, ButtonDuplicate,
      ButtonDelete, ButtonClearAll: TCastleButton;
    ButtonTranslate, ButtonRotate, ButtonScale: TCastleButton;
    EditableAssetsParent: TCastleTransform;
    MainViewport: TCastleViewport;
  private
    { List of URLs of assets that can be placed in TOrmCastleTransform. }
    EditableAssets: TStringList;
    { All instances of TCastleTransform created from TOrmCastleTransform
      will be owned by this component. They cannot be just owned by FreeAtStop
      because they need a separate owner, as their names are in a separate namespace. }
    EditableAssetsOwner: TComponent;
    TransformHover: TCastleTransformHover;
    TransformManipulate: TCastleTransformManipulate;
    procedure FoundEditableAsset(const FileInfo: TFileInfo; var StopSearch: boolean);
    procedure ClickAddRandom(Sender: TObject);
    procedure ClickAddSphere(Sender: TObject);
    procedure ClickAddBox(Sender: TObject);
    procedure ClickDuplicate(Sender: TObject);
    procedure ClickDelete(Sender: TObject);
    procedure ClickClearAll(Sender: TObject);

    { Create new TOrmCastleTransform instance initialized with mostly random values.
      The URL will be NewUrl.
      Name is unset. }
    function NewOrmCastleTransform(const NewUrl: String): TOrmCastleTransform;

    { Initialize given OrmTransform to appear in the world (client and server)
      correctly:
      @orderedList(
        @item(Initialize OrmTransform.Name to be something non-conflicting.)
        @item(Send to the server.)
        @item(Add to MainViewport (by adding to EditableAssetsParent).)
      ) }
    procedure NewEditableAsset(const OrmTransform: TOrmCastleTransform);

    { Set Pressed state of 3 buttons based on TransformManipulate.Mode. }
    procedure UpdateTransformButtons;

    procedure ClickTranslate(Sender: TObject);
    procedure ClickRotate(Sender: TObject);
    procedure ClickScale(Sender: TObject);
    procedure TransformManipulateModified(Sender: TObject);
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
  Mormot.Core.Base, Mormot.Core.Unicode, Mormot.Core.Os, Mormot.Orm.Core,
  CastleStringUtils, CastleClassUtils, CastleLog, CastleUriUtils, CastleColors,
  GameConnection, CastleUtils;

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
  ButtonDuplicate.OnClick := {$ifdef FPC}@{$endif} ClickDuplicate;
  ButtonDelete.OnClick := {$ifdef FPC}@{$endif} ClickDelete;
  ButtonClearAll.OnClick := {$ifdef FPC}@{$endif} ClickClearAll;
  ButtonTranslate.OnClick := {$ifdef FPC}@{$endif} ClickTranslate;
  ButtonRotate.OnClick := {$ifdef FPC}@{$endif} ClickRotate;
  ButtonScale.OnClick := {$ifdef FPC}@{$endif} ClickScale;

  // adjust FlyNavigation settings
  FlyNavigation.Input_Jump.Assign(keyE);
  FlyNavigation.Input_Crouch.Assign(keyQ);
  // do not use arrow keys, we need to have them free to manipulate objects
  FlyNavigation.Input_Forward.Assign(keyW);
  FlyNavigation.Input_Backward.Assign(keyS);
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
    // FreeAndNil(OrmTransform); // do not free OrmTransform, it will get freed by FreeAndNil(AllOrmTransforms)
  end;
  FreeAndNil(AllOrmTransforms);

  { Tracking hover and manipulated objects.
    The TCastleTransformHover and TCastleTransformManipulate instancess
    visualize the currently hovered over / manipulated TCastleTransform.
    They also automatically handle "what happens when the object is freed",
    setting their respective references to nil.
    So we just use
    - TransformHover.Current and
    - TransformManipulate.MainSelected
    to track what is now hovered over / manipulated. }
  TransformHover := TCastleTransformHover.Create(FreeAtStop);
  TransformManipulate := TCastleTransformManipulate.Create(FreeAtStop);
  TransformManipulate.Mode := mmTranslate;
  TransformManipulate.OnTransformModified := {$ifdef FPC}@{$endif} TransformManipulateModified;

  UpdateTransformButtons;
end;

procedure TViewedit.Stop;
begin
  FreeAndNil(EditableAssets);
  inherited;
end;

procedure TViewedit.TransformManipulateModified(Sender: TObject);
var
  Sel: TCastleTransform;
begin
  { TODO: This code to update feels a bit dirty -- calling UpdateField
    3 or 4 times is probably not optimal, and in general it feels not cool
    that we cannot use

      HttpClient.Orm.Update(TOrm)

    However, we don't have TOrmCastleTransform instance at this point.
    We could make it... but it would not have correct ID, as TOrm.ID is read-only,
    we cannot just set it from Sel.Tag.
    In general, all HttpClient.Orm.Update* feel a bit unsuitable for this case.

    There are no practical problems with this though, so maybe just accept
    it as the way to do it. }

  Sel := TransformManipulate.MainSelected;
  case TransformManipulate.Mode of
    mmTranslate:
      begin
        if not HttpClient.Orm.UpdateField(TOrmCastleTransform, Sel.Tag, 'TranslationX', Sel.Translation.X) or
           not HttpClient.Orm.UpdateField(TOrmCastleTransform, Sel.Tag, 'TranslationY', Sel.Translation.Y) or
           not HttpClient.Orm.UpdateField(TOrmCastleTransform, Sel.Tag, 'TranslationZ', Sel.Translation.Z) then
          raise Exception.Create('Failed to update the server');
      end;
    mmRotate:
      begin
        if not HttpClient.Orm.UpdateField(TOrmCastleTransform, Sel.Tag, 'RotationX', Sel.Rotation.X) or
           not HttpClient.Orm.UpdateField(TOrmCastleTransform, Sel.Tag, 'RotationY', Sel.Rotation.Y) or
           not HttpClient.Orm.UpdateField(TOrmCastleTransform, Sel.Tag, 'RotationZ', Sel.Rotation.Z) or
           not HttpClient.Orm.UpdateField(TOrmCastleTransform, Sel.Tag, 'RotationW', Sel.Rotation.W) then
          raise Exception.Create('Failed to update the server');
      end;
    mmScale:
      begin
        if not HttpClient.Orm.UpdateField(TOrmCastleTransform, Sel.Tag, 'ScaleX', Sel.Scale.X) or
           not HttpClient.Orm.UpdateField(TOrmCastleTransform, Sel.Tag, 'ScaleY', Sel.Scale.Y) or
           not HttpClient.Orm.UpdateField(TOrmCastleTransform, Sel.Tag, 'ScaleZ', Sel.Scale.Z) then
          raise Exception.Create('Failed to update the server');
      end;
    else raise EInternalError.Create('TransformMode?');
  end;
end;

procedure TViewedit.Update(const SecondsPassed: Single; var HandleInput: boolean);

  { Component-wise maximum of two vectors. }
  function MaxVector(const A, B: TVector3): TVector3;
  begin
    Result := Vector3(Max(A.X, B.X), Max(A.Y, B.Y), Max(A.Z, B.Z));
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
    TransformHover.Current := MainViewport.TransformUnderMouse.Parent
  else
    TransformHover.Current := nil;
end;

function TViewedit.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsMouseButton(buttonLeft) and (TransformHover.Current <> nil) then
  begin
    { We set selected in 2 ways on TransformManipulate.
      In the future, when we implement editing transformation of multiple objects
      at once, MainSelected will be removed (or will become an alias to SetSelected). }
    TransformManipulate.MainSelected := TransformHover.Current;
    TransformManipulate.SetSelected([TransformHover.Current]);
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

function TViewedit.NewOrmCastleTransform(const NewUrl: String): TOrmCastleTransform;
begin
  Result := TOrmCastleTransform.Create;

  Result.Url := StringToUTF8(NewUrl);
  Result.TranslationX := Random * 10;
  //Result.TranslationY := Random * 10; // don't randomize, keep on floor
  Result.TranslationZ := Random * 10;

  // normal uniform scale
  Result.ScaleX := 1;
  Result.ScaleY := 1;
  Result.ScaleZ := 1;
end;

procedure TViewedit.NewEditableAsset(const OrmTransform: TOrmCastleTransform);

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
  BaseName: String;
  Transform: TCastleTransform;
begin
  BaseName := DeleteUriExt(ExtractUriName(Utf8ToString(OrmTransform.Url)));
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

  // make newly added object selected; makes the UI nice, to further transform or duplicate
  TransformManipulate.MainSelected := Transform;
  TransformManipulate.SetSelected([Transform]);
end;

procedure TViewedit.ClickAddRandom(Sender: TObject);
var
  Orm: TOrmCastleTransform;
begin
  Orm := NewOrmCastleTransform(EditableAssets[Random(EditableAssets.Count)]);
  try
    NewEditableAsset(Orm);
  finally FreeAndNil(Orm) end;
end;

procedure TViewedit.ClickAddSphere(Sender: TObject);
var
  Orm: TOrmCastleTransform;
begin
  Orm := NewOrmCastleTransform('castle-primitive:/sphere');
  try
    NewEditableAsset(Orm);
  finally FreeAndNil(Orm) end;
end;

procedure TViewedit.ClickAddBox(Sender: TObject);
var
  Orm: TOrmCastleTransform;
begin
  Orm := NewOrmCastleTransform('castle-primitive:/box');
  try
    NewEditableAsset(Orm);
  finally FreeAndNil(Orm) end;
end;

procedure TViewedit.ClickDuplicate(Sender: TObject);
var
  Sel: TCastleTransform;
  Orm: TOrmCastleTransform;
begin
  Sel := TransformManipulate.MainSelected;
  if Sel <> nil then
  begin
    Orm := TOrmCastleTransform.Create;
    try
      Orm.UpdateFromTransform(Sel);
      { Apply tiny translation in XZ to let user see that it's duplicated.
        Note: This is probably a bad idea to do in non-demo application,
        because user may want to preserve original X / Z exactly.
        Displaying the selected name would probably be better. }
      Orm.TranslationX := Orm.TranslationX + 0.1;
      Orm.TranslationZ := Orm.TranslationZ + 0.1;
      NewEditableAsset(Orm);
    finally FreeAndNil(Orm) end;
  end;
end;

procedure TViewedit.ClickDelete(Sender: TObject);
var
  Sel: TCastleTransform;
begin
  Sel := TransformManipulate.MainSelected;
  if Sel <> nil then
  begin
    { Remove from the server.
      We know that TCastleTransform.Tag holds the ID of the TOrmCastleTransform. }
    WriteLnLog('Deleting from server: %d', [Sel.Tag]);
    if not HttpClient.Orm.Delete(TOrmCastleTransform, Sel.Tag) then
      raise Exception.Create('Failed to delete from the server');
    Sel.Free; // this also clears TransformManipulate.MainSelected
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
  ButtonTranslate.Pressed := TransformManipulate.Mode = mmTranslate;
  ButtonRotate.Pressed := TransformManipulate.Mode = mmRotate;
  ButtonScale.Pressed := TransformManipulate.Mode = mmScale;
end;

procedure TViewedit.ClickTranslate(Sender: TObject);
begin
  TransformManipulate.Mode := mmTranslate;
  UpdateTransformButtons;
end;

procedure TViewedit.ClickRotate(Sender: TObject);
begin
  TransformManipulate.Mode := mmRotate;
  UpdateTransformButtons;
end;

procedure TViewedit.ClickScale(Sender: TObject);
begin
  TransformManipulate.Mode := mmScale;
  UpdateTransformButtons;
end;

end.
