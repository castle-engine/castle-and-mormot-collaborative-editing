{
  Copyright 2024-2025 Michalis Kamburelis.

  This is free software; see the file LICENSE,
  included in this distribution, for details about the copyright.

  This is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Edit the world. }
unit GameViewEdit;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleCameras, CastleFindFiles, CastleTransform, CastleDebugTransform,
  CastleViewport, CastleTransformManipulate,
  SharedData;

type
  { View to edit the world. }
  TViewEdit = class(TCastleView)
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
    CheckboxSendChangesDuringTransform: TCastleCheckbox;
  private
    { List of URLs of assets that can be placed in TOrmCastleTransform. }
    EditableAssets: TStringList;
    { All instances of TCastleTransform created from TOrmCastleTransform
      will be owned by this component. They cannot be just owned by FreeAtStop
      because they need a separate owner, as their names are in a separate namespace. }
    EditableAssetsOwner: TComponent;
    TransformHover: TCastleTransformHover;
    TransformManipulate: TCastleTransformManipulate;
    PollChangesTimer: TCastleTimer;
    PollCount: Int64; //< number of times we called TimerPollChanges
    procedure FoundEditableAsset(const FileInfo: TFileInfo; var StopSearch: boolean);
    procedure ClickAddRandom(Sender: TObject);
    procedure ClickAddSphere(Sender: TObject);
    procedure ClickAddBox(Sender: TObject);
    procedure ClickDuplicate(Sender: TObject);
    procedure ClickDelete(Sender: TObject);
    procedure ClickClearAll(Sender: TObject);
    procedure TimerPollChanges(Sender: TObject);

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

    { Send the changes done to SelectedTransform by TransformManipulate
      to the server. }
    procedure TransformManipulateSendChanges;

    procedure ClickTranslate(Sender: TObject);
    procedure ClickRotate(Sender: TObject);
    procedure ClickScale(Sender: TObject);
    procedure TransformManipulateModified(Sender: TObject);
    procedure TransformManipulateModifyEnd(Sender: TObject);

    { One selected TEditableCastleTransform. }
    function SelectedTransform: TEditableCastleTransform;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewEdit: TViewEdit;

implementation

uses SysUtils, Contnrs, Math,
  Mormot.Core.Base, Mormot.Core.Unicode, Mormot.Core.Os, Mormot.Orm.Core,
  CastleStringUtils, CastleClassUtils, CastleLog, CastleUriUtils, CastleColors,
  GameConnection, CastleUtils;

constructor TViewEdit.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewedit.castle-user-interface';
end;

procedure TViewEdit.Start;
var
  AllOrmTransforms: TObjectList;
  OrmTransformObj: Pointer;
  OrmTransform: TOrmCastleTransform;
  Transform: TEditableCastleTransform;
begin
  inherited;

  PollChangesTimer := TCastleTimer.Create(FreeAtStop);
  { Poll changes 15 times per second. This is quite a lot.
    See README.md for notes why this whole "polling" approach is inefficient,
    and how to improve it. }
  PollChangesTimer.IntervalSeconds := 1 / 15;
  PollChangesTimer.OnTimer := {$ifdef FPC}@{$endif} TimerPollChanges;
  InsertBack(PollChangesTimer);

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
    - TransformManipulate.Selected
    to track what is now hovered over / manipulated. }
  TransformHover := TCastleTransformHover.Create(FreeAtStop);
  TransformManipulate := TCastleTransformManipulate.Create(FreeAtStop);
  TransformManipulate.Mode := mmTranslate;
  TransformManipulate.OnTransformModified := {$ifdef FPC}@{$endif} TransformManipulateModified;
  TransformManipulate.OnTransformModifyEnd := {$ifdef FPC}@{$endif} TransformManipulateModifyEnd;

  UpdateTransformButtons;
end;

procedure TViewEdit.Stop;
begin
  FreeAndNil(EditableAssets);
  inherited;
end;

procedure TViewEdit.TransformManipulateSendChanges;
var
  Sel: TEditableCastleTransform;
begin
  { TODO: This code to update feels a bit dirty -- calling UpdateField
    3 or 4 times is probably not optimal, and in general it feels not cool
    that we cannot use

      HttpClient.Orm.Update(TOrm)

    However, we don't have TOrmCastleTransform instance at this point.
    We could make it... but it would not have correct ID, as TOrm.ID is read-only,
    we cannot just set it from Sel.ID.
    In general, all HttpClient.Orm.Update* feel a bit unsuitable for this case.

    There are no practical problems with this though, so maybe just accept
    it as the way to do it. }

  Sel := SelectedTransform;
  case TransformManipulate.Mode of
    mmTranslate:
      begin
        if not HttpClient.Orm.UpdateField(TOrmCastleTransform, Sel.ID, 'TranslationX', Sel.Translation.X) or
           not HttpClient.Orm.UpdateField(TOrmCastleTransform, Sel.ID, 'TranslationY', Sel.Translation.Y) or
           not HttpClient.Orm.UpdateField(TOrmCastleTransform, Sel.ID, 'TranslationZ', Sel.Translation.Z) then
          raise Exception.CreateFmt('Failed to update the server state of TOrmCastleTransform with ID %d', [Sel.ID]);
      end;
    mmRotate:
      begin
        if not HttpClient.Orm.UpdateField(TOrmCastleTransform, Sel.ID, 'RotationX', Sel.Rotation.X) or
           not HttpClient.Orm.UpdateField(TOrmCastleTransform, Sel.ID, 'RotationY', Sel.Rotation.Y) or
           not HttpClient.Orm.UpdateField(TOrmCastleTransform, Sel.ID, 'RotationZ', Sel.Rotation.Z) or
           not HttpClient.Orm.UpdateField(TOrmCastleTransform, Sel.ID, 'RotationW', Sel.Rotation.W) then
          raise Exception.CreateFmt('Failed to update the server state of TOrmCastleTransform with ID %d', [Sel.ID]);
      end;
    mmScale:
      begin
        if not HttpClient.Orm.UpdateField(TOrmCastleTransform, Sel.ID, 'ScaleX', Sel.Scale.X) or
           not HttpClient.Orm.UpdateField(TOrmCastleTransform, Sel.ID, 'ScaleY', Sel.Scale.Y) or
           not HttpClient.Orm.UpdateField(TOrmCastleTransform, Sel.ID, 'ScaleZ', Sel.Scale.Z) then
          raise Exception.CreateFmt('Failed to update the server state of TOrmCastleTransform with ID %d', [Sel.ID]);
      end;
    else raise EInternalError.Create('TransformMode?');
  end;

  Inc(Sel.Revision);
  if not HttpClient.Orm.UpdateField(TOrmCastleTransform, Sel.ID, 'Revision', Sel.Revision) then
    raise Exception.CreateFmt('Failed to update the revision of TOrmCastleTransform with ID %d to %d', [
      Sel.ID,
      Sel.Revision
    ]);
end;

procedure TViewEdit.TransformManipulateModified(Sender: TObject);
begin
  if CheckboxSendChangesDuringTransform.Checked then
    TransformManipulateSendChanges;
end;

procedure TViewEdit.TransformManipulateModifyEnd(Sender: TObject);
begin
  if not CheckboxSendChangesDuringTransform.Checked then
    TransformManipulateSendChanges;
end;

procedure TViewEdit.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;

  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  FlyNavigation.MouseLook := buttonRight in Container.MousePressed;

  // update VisualizeHover
  if (MainViewport.TransformUnderMouse <> nil) and
     (MainViewport.TransformUnderMouse.Parent is TEditableCastleTransform) then
    { We use TransformUnderMouse.Parent, because we want to track
      the TEditableCastleTransform, that contains ID for ORM synchronization. }
    TransformHover.Current := MainViewport.TransformUnderMouse.Parent
  else
    TransformHover.Current := nil;
end;

function TViewEdit.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsMouseButton(buttonLeft) and (TransformHover.Current <> nil) then
  begin
    TransformManipulate.SetSelected([TransformHover.Current]);
    Exit(true);
  end;
end;

procedure TViewEdit.FoundEditableAsset(const FileInfo: TFileInfo; var StopSearch: boolean);
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

function TViewEdit.NewOrmCastleTransform(const NewUrl: String): TOrmCastleTransform;
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

procedure TViewEdit.NewEditableAsset(const OrmTransform: TOrmCastleTransform);

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
  TransformManipulate.SetSelected([Transform]);
end;

procedure TViewEdit.ClickAddRandom(Sender: TObject);
var
  Orm: TOrmCastleTransform;
begin
  Orm := NewOrmCastleTransform(EditableAssets[Random(EditableAssets.Count)]);
  try
    NewEditableAsset(Orm);
  finally FreeAndNil(Orm) end;
end;

procedure TViewEdit.ClickAddSphere(Sender: TObject);
var
  Orm: TOrmCastleTransform;
begin
  Orm := NewOrmCastleTransform('castle-primitive:/sphere');
  try
    NewEditableAsset(Orm);
  finally FreeAndNil(Orm) end;
end;

procedure TViewEdit.ClickAddBox(Sender: TObject);
var
  Orm: TOrmCastleTransform;
begin
  Orm := NewOrmCastleTransform('castle-primitive:/box');
  try
    NewEditableAsset(Orm);
  finally FreeAndNil(Orm) end;
end;

procedure TViewEdit.ClickDuplicate(Sender: TObject);
var
  Sel: TEditableCastleTransform;
  Orm: TOrmCastleTransform;
begin
  Sel := SelectedTransform;
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

procedure TViewEdit.ClickDelete(Sender: TObject);
var
  Sel: TEditableCastleTransform;
begin
  Sel := SelectedTransform;
  if Sel <> nil then
  begin
    { Remove from the server. }
    WriteLnLog('Deleting from server: %d', [Sel.ID]);
    if not HttpClient.Orm.Delete(TOrmCastleTransform, Sel.ID) then
      raise Exception.Create('Failed to delete from the server');
    Sel.Free; // this also clears SelectedTransform
  end;
end;

procedure TViewEdit.ClickClearAll(Sender: TObject);
begin
  while EditableAssetsParent.Count > 0 do
    EditableAssetsParent[0].Free; // this also removes from the list
  { Remove all from the server.
    This way of removing means we also remove invalid ORM data,
    that was not reflected in any TCastleTransform instance, but was on server. }
  HttpClient.Orm.Delete(TOrmCastleTransform, '1=1', []);
end;

procedure TViewEdit.UpdateTransformButtons;
begin
  ButtonTranslate.Pressed := TransformManipulate.Mode = mmTranslate;
  ButtonRotate.Pressed := TransformManipulate.Mode = mmRotate;
  ButtonScale.Pressed := TransformManipulate.Mode = mmScale;
end;

procedure TViewEdit.ClickTranslate(Sender: TObject);
begin
  TransformManipulate.Mode := mmTranslate;
  UpdateTransformButtons;
end;

procedure TViewEdit.ClickRotate(Sender: TObject);
begin
  TransformManipulate.Mode := mmRotate;
  UpdateTransformButtons;
end;

procedure TViewEdit.ClickScale(Sender: TObject);
begin
  TransformManipulate.Mode := mmScale;
  UpdateTransformButtons;
end;

function TViewEdit.SelectedTransform: TEditableCastleTransform;
begin
  { TransformManipulate supports multiple transforms being selected at once,
    but we only allow one selected at a time in this demo. }
  if (TransformManipulate.SelectedCount = 1) and
     (TransformManipulate.Selected[0] is TEditableCastleTransform) then
    Result := TEditableCastleTransform(TransformManipulate.Selected[0])
  else
    Result := nil;
end;

procedure TViewEdit.TimerPollChanges(Sender: TObject);

  function FindTransform(const ID: Int64): TEditableCastleTransform;
  var
    T: TCastleTransform;
  begin
    for T in EditableAssetsParent do
      if (T is TEditableCastleTransform) and
         (TEditableCastleTransform(T).ID = ID) then
        Exit(TEditableCastleTransform(T));
    Result := nil;
  end;

  procedure DetectRemovals;
  var
    T: TCastleTransform;
    ET: TEditableCastleTransform;
  begin
    for T in EditableAssetsParent do
      if T is TEditableCastleTransform then
      begin
        ET := TEditableCastleTransform(T);
        if ET.ExistsAtPollCount <> PollCount then
        begin
          WritelnLog('Polling', 'Other client DELETED transform %d', [ET.ID]);
          ET.Free; // this also removes ET from the EditableAssetsParent list
        end;
      end;
  end;

var
  AllOrmTransforms: TObjectList;
  OrmTransformObj: Pointer;
  OrmTransform: TOrmCastleTransform;
  Transform: TEditableCastleTransform;
begin
  AllOrmTransforms := HttpClient.RetrieveList(TOrmCastleTransform, '', []);
  if AllOrmTransforms = nil then
  begin
    WritelnWarning('Polling failed to retrieve the data, is the server running?');
    Exit;
  end;

  Inc(PollCount);

  for OrmTransformObj in AllOrmTransforms do
  begin
    OrmTransform := TObject(OrmTransformObj) as TOrmCastleTransform;

    Transform := FindTransform(OrmTransform.ID);
    if Transform <> nil then
    begin
      if Transform.Revision < OrmTransform.Revision then
      begin
        WritelnLog('Polling', 'Other client MODIFIED transform %d', [OrmTransform.ID]);
        OrmTransform.UpdateToTransform(Transform)
      end else
      if Transform.Revision > OrmTransform.Revision then
      begin
        WritelnWarning('Server has older revision than client of transform %d, this should not happen', [OrmTransform.ID]);
      end;
    end else
    begin
      WritelnLog('Polling', 'Other client ADDED transform %d', [OrmTransform.ID]);
      Transform := OrmTransform.CreateTransform(EditableAssetsOwner);
      EditableAssetsParent.Add(Transform);
    end;
    Transform.ExistsAtPollCount := PollCount;

    // FreeAndNil(OrmTransform); // do not free OrmTransform, it will get freed by FreeAndNil(AllOrmTransforms)
  end;
  FreeAndNil(AllOrmTransforms);

  DetectRemovals;
end;

end.
