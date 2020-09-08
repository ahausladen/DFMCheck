{******************************************************************************}
{*                                                                            *}
{* DfmCheck                                                                   *}
{*                                                                            *}
{* (C) 2006-2007 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit DfmCheck_Main;

{$I DfmCheck.inc}

interface

uses
  Windows, SysUtils, Classes, Contnrs, ToolsAPI, Forms, ActnList, Menus;

type
  TIdeNotifier = class(TNotifierObject, IOTAIDENotifier, IOTAIDENotifier50)
  private
    FOptionKey: string;
    FMenuItems: TObjectList;
    FActionItems: TObjectList;
    FInAction: Integer;
    FCheckConnected: Boolean;
    {FBuildingProject: Integer;
    FCompileUnitAdded: string;
    FCompileProject: IOTAProject;

    FMenuProjectBuildItem: TMenuItem;
    FMenuProjectBuildAllItem: TMenuItem;
    FNativeActionBuildItemExecute: TNotifyEvent;
    FNativeActionBuildAllItemExecute: TNotifyEvent;}

    //FActionConfig: TAction;
    FActionRunDfmCheck: TAction;
    FActionOpenCloseAllForms: TAction;
    FSuccessfullCompile: Boolean;
  protected
    function AddMenuAction(const BaseMainItemName, BaseItemName, ItemName, Caption: string;
      Execute: TNotifyEvent; InsertAfter: Boolean = True; InsertAsChild: Boolean = False): TAction;
    function HookActionExecute(MenuItem: TMenuItem;
      DoExecute: TNotifyEvent; AllowOnClickHook: Boolean = False): TNotifyEvent;
    procedure UnhookActionExecute(MenuItem: TMenuItem;
      DoExecute: TNotifyEvent; AllowOnClickHook: Boolean = False);
    function FindMenuItem(const Name: string): TMenuItem;

    procedure ActionUpdate(Sender: TObject);

    //procedure DoConfig(Sender: TObject);
    procedure DoRunDfmCheck(Sender: TObject);
    procedure DoOpenCloseAllForms(Sender: TObject);
    {procedure DoProjectBuildItem(Sender: TObject);
    procedure DoProjectBuildAllItem(Sender: TObject);}
    //procedure SaveOptions;

    function RunDfmCheck(Project: IOTAProject): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;
    procedure BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean; var Cancel: Boolean); overload;
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
  end;

implementation

uses
  DfmCheck_Options, DfmCheck_AppConsts, DfmCheck_DfmCheck,
  DfmCheck_OptionsFrm, DfmCheck_Utils;

{ TIdeNotifier }

constructor TIdeNotifier.Create;
var
  IsGerman: Boolean;
begin
  inherited Create;

  FActionItems := TObjectList.Create;
  FMenuItems := TObjectList.Create;

  Options := TOptions.Create;
  FOptionKey := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\DfmCheck';
  Options.LoadFromReg(FOptionKey);

  //FCheckConnected := True;

  IsGerman := (StripHotkey(FindMenuItem('FileMenu').Caption) = 'Datei') and
              (StripHotkey(FindMenuItem('EditMenu').Caption) = 'Bearbeiten');
  if IsGerman then
  begin
    //FActionConfig := AddMenuAction('ToolsMenu', 'ToolsGalleryItem', 'MenuToolsDfmCheckConfig', RsGerConfigurePlugin, DoConfig);
    FActionRunDfmCheck := AddMenuAction('ProjectMenu', 'ProjectBuildItem', 'MenuProjectRunDfmCheck', RsGerRunDfmCheck, DoRunDfmCheck);
    FActionOpenCloseAllForms := AddMenuAction('ProjectMenu', 'MenuProjectRunDfmCheck', 'MenuProjectOpenCloseAllForms', RsGerOpenCloseAllForms, DoOpenCloseAllForms);
    sConfirmSaveModifiedFile := RsGerConfirmSaveModifiedFile;
  end
  else
  begin
    //FActionConfig := AddMenuAction('ToolsMenu', 'ToolsGalleryItem', 'MenuToolsDfmCheckConfig', RsConfigurePlugin, DoConfig);
    FActionRunDfmCheck := AddMenuAction('ProjectMenu', 'ProjectBuildItem', 'MenuProjectRunDfmCheck', RsRunDfmCheck, DoRunDfmCheck);
    FActionOpenCloseAllForms := AddMenuAction('ProjectMenu', 'MenuProjectRunDfmCheck', 'MenuProjectOpenCloseAllForms', RsOpenCloseAllForms, DoOpenCloseAllForms);
    sConfirmSaveModifiedFile := RsConfirmSaveModifiedFile;
  end;

  {FMenuProjectBuildItem := FindMenuItem('ProjectBuildItem');
  FMenuProjectBuildAllItem := FindMenuItem('ProjectBuildAllItem');
  FNativeActionBuildItemExecute := HookActionExecute(FMenuProjectBuildItem, DoProjectBuildItem);
  FNativeActionBuildAllItemExecute := HookActionExecute(FMenuProjectBuildAllItem, DoProjectBuildAllItem);}
end;

destructor TIdeNotifier.Destroy;
begin
  //SaveOptions;
  FreeAndNil(Options);
  {UnhookActionExecute(FMenuProjectBuildItem, FNativeActionBuildItemExecute);
  UnhookActionExecute(FMenuProjectBuildAllItem, FNativeActionBuildAllItemExecute);}
  FMenuItems.Free; // first
  FActionItems.Free; // second
  inherited Destroy;
end;

function TIdeNotifier.FindMenuItem(const Name: string): TMenuItem;
begin
  Result := TMenuitem(Application.MainForm.FindComponent(Name));
  if not (TObject(Result) is TMenuItem) then
    Result := nil;
end;

function TIdeNotifier.AddMenuAction(const BaseMainItemName, BaseItemName, ItemName, Caption: string;
  Execute: TNotifyEvent; InsertAfter: Boolean; InsertAsChild: Boolean): TAction;
var
  MenuItem: TMenuItem;
  {$IFNDEF COMPILER9_UP}
  Item: TComponent;
  i: Integer;
  {$ENDIF !COMPILER9_UP}
begin
  Result := TAction.Create(nil);
  Result.Caption := Caption;
  Result.OnExecute := Execute;
  Result.OnUpdate := ActionUpdate;

  MenuItem := TMenuItem.Create(Result);
  MenuItem.Name := ItemName;
  MenuItem.Action := Result;
  Result.Tag := Integer(MenuItem);

  FActionItems.Add(Result);
  FMenuItems.Add(MenuItem);

  {$IFDEF COMPILER9_UP}
  NServices.AddActionMenu(BaseItemName, Result, MenuItem, InsertAfter, InsertAsChild);
  {$ELSE}
  Item := nil;
  for i := 0 to FMenuItems.Count - 1 do
  begin
    if CompareText(TMenuItem(FMenuItems[i]).Name, BaseItemName) = 0 then
    begin
      Item := TMenuItem(FMenuItems[i]);
      Break;
    end;
  end;

  if Item = nil then
  begin
    Item := Application.MainForm.FindComponent(BaseItemName);
    if not (Item is TMenuItem) then
    begin
      Item := Application.MainForm.FindComponent(BaseMainItemName);
      if not (Item is TMenuItem) then
        Item := nil;
      InsertAsChild := True;
    end;
  end;

  if Item <> nil then
  begin
    if InsertAsChild then
    begin
      if InsertAfter then
        TMenuItem(Item).Add(MenuItem)
      else
        TMenuItem(Item).Insert(0, MenuItem);
    end
    else
      TMenuItem(Item).Parent.Insert(TMenuItem(Item).Parent.IndexOf(TMenuItem(Item)) + Ord(InsertAfter), MenuItem);
  end
  else
    raise Exception.CreateFmt('Menu item "%s" not found.', [BaseItemName]);
  {$ENDIF COMPILER9_UP}
end;

function TIdeNotifier.HookActionExecute(MenuItem: TMenuItem; DoExecute: TNotifyEvent;
  AllowOnClickHook: Boolean): TNotifyEvent;
begin
  Result := nil;
  if Assigned(MenuItem) then
  begin
    if Assigned(MenuItem.Action) then
    begin
      Result := MenuItem.Action.OnExecute;
      MenuItem.Action.OnExecute := DoExecute;
    end
    else if AllowOnClickHook then
    begin
      Result := MenuItem.OnClick;
      MenuItem.OnClick := DoExecute;
    end;
  end;
end;

procedure TIdeNotifier.UnhookActionExecute(MenuItem: TMenuItem; DoExecute: TNotifyEvent;
  AllowOnClickHook: Boolean);
begin
  if Assigned(MenuItem) then
  begin
    if Assigned(MenuItem.Action) then
      MenuItem.Action.OnExecute := DoExecute
    else if AllowOnClickHook then
      MenuItem.OnClick := DoExecute;
  end;
end;

{procedure TIdeNotifier.SaveOptions;
begin
  if Options.Modified then
  begin
    try
      Options.SaveToReg(FOptionKey);
    except
      if not Application.Terminated then
        Application.HandleException(Self);
    end;
  end;
end;}

procedure TIdeNotifier.ActionUpdate(Sender: TObject);
begin
  {if Sender = FActionConfig then
    FActionConfig.Enabled := FInAction = 0
  else} if Sender = FActionRunDfmCheck then
    FActionRunDfmCheck.Enabled := (FInAction = 0) and (GetActiveProject <> nil)
  else if Sender = FActionOpenCloseAllForms then
    FActionOpenCloseAllForms.Enabled := (FInAction = 0) and (GetActiveProject <> nil);
end;

{procedure TIdeNotifier.DoConfig(Sender: TObject);
begin
  if TDfmCheck_FormOptions.Execute then
    SaveOptions;
end;}

procedure TIdeNotifier.DoRunDfmCheck(Sender: TObject);
begin
  RunDfmCheck(GetActiveProject);
end;

procedure TIdeNotifier.DoOpenCloseAllForms(Sender: TObject);
begin
  OpenCloseForms(GetActiveProject);
end;

function TIdeNotifier.RunDfmCheck(Project: IOTAProject): Boolean;
var
  DfmCheck: TDfmCheck;
  Added: Boolean;
  ProjectDir: string;
  CurDir: string;
begin
  Inc(FInAction);
  try
    DfmCheck := TDfmCheck.Create(Project);
    try
      Result := DfmCheck.CheckForms(True, FCheckConnected);
      if Result then
      begin
        Added := SilentAddUnitToProject(Project, DfmCheck.ProjectFileNameDfmCheckUnit);
        try
          { Compile project }
          FSuccessfullCompile := False;
          Result := Project.ProjectBuilder.BuildProject(cmOTAMake, True);
        finally
          if Added then
            SilentRemoveUnitFromProject(Project, DfmCheck.ProjectFileNameDfmCheckUnit);
          if FSuccessfullCompile then
          begin
            DeleteFile(DfmCheck.ProjectFileNameDfmCheckUnit);
            if ExtractFileExt(DfmCheck.ProjectFileNameDfmCheckUnit) = '.pas' then
            begin
              ProjectDir := ExtractFileDir(Project.FileName);
              CurDir := GetCurrentDir;
              try
                if not SameText(CurDir, ProjectDir) then
                  SetCurrentDir(ProjectDir);
                DeleteFile(AddPathDelim(ExpandFileName(Project.ProjectOptions.Values['UnitOutputDir'])) +
                  ChangeFileExt(ExtractFileName(DfmCheck.ProjectFileNameDfmCheckUnit), '.dcu'));
              finally
                if not SameText(CurDir, ProjectDir) then
                  SetCurrentDir(CurDir);
              end;
            end;
          end;
        end;
      end;
    finally
      DfmCheck.Free;
    end;
  finally
    Dec(FInAction);
  end;
end;

procedure TIdeNotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
end;

procedure TIdeNotifier.AfterCompile(Succeeded, IsCodeInsight: Boolean);
begin
{  if FCompileProject <> nil then
  begin
    try
      if FCompileUnitAdded <> '' then
        SilentRemoveUnitFromProject(FCompileProject, FCompileUnitAdded);

      Dec(FInAction);
    finally
      FCompileProject := nil;
    end;
  end;}
  if not IsCodeInsight then
    FSuccessfullCompile := Succeeded;
end;

procedure TIdeNotifier.AfterCompile(Succeeded: Boolean);
begin
end;

procedure TIdeNotifier.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin
end;

procedure TIdeNotifier.BeforeCompile(const Project: IOTAProject;
  IsCodeInsight: Boolean; var Cancel: Boolean);
{var
  DfmCheck: TDfmCheck;}
begin
{  FCompileProject := nil;
  Inc(FInAction);
  try
    if (FInAction = 1) and not IsCodeInsight and Options.AutoCheckBeforeCompile then
    begin
      if IsAlreadyInProject(Project, GetProjectDfmCheckUnitFileName(Project)) or
         (not Options.AutoCheckBeforeBuildOnly or (FBuildingProject > 0)) then
      begin
        FCompileUnitAdded := '';
        DfmCheck := TDfmCheck.Create(Project);
        try
          Cancel := not DfmCheck.CheckForms(Project.GetModuleCount > 30);
          if not Cancel then
            if SilentAddUnitToProject(Project, DfmCheck.ProjectFileNameDfmCheckUnit) then
              FCompileUnitAdded := DfmCheck.ProjectFileNameDfmCheckUnit;
        finally
          DfmCheck.Free;
        end;
      end;
    end;
  except
    Application.HandleException(Self);
  end;
  FCompileProject := Project;}
end;

{procedure TIdeNotifier.DoProjectBuildAllItem(Sender: TObject);
begin
  Inc(FBuildingProject);
  try
    FNativeActionBuildAllItemExecute(Sender);
  finally
    Dec(FBuildingProject);
  end;
end;

procedure TIdeNotifier.DoProjectBuildItem(Sender: TObject);
begin
  Inc(FBuildingProject);
  try
    FNativeActionBuildItemExecute(Sender);
  finally
    Dec(FBuildingProject);
  end;
end;}

{------------------------------------------------------------------------------}

{ Hybrid DLL-Expert-BPL-Package }

procedure DoneWizard;
begin
  { Finalize units (including this unit) }
  FinalizePackage(HInstance);
end;

function InitWizard(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc; var Terminate: TWizardTerminateProc): Boolean; stdcall;
begin
  { Initialize all units (including this one) }
  InitializePackage(HInstance);

  Terminate := DoneWizard;
  Result := True;
end;

exports
  InitWizard name WizardEntryPoint;
  
var
  Services: IOTAServices;
  IdeNotifierIndex: Integer;
  {$IFDEF COMPILER10_UP}
  AboutBoxServices: IOTAAboutBoxServices;
  AboutBoxIndex: Integer;
  {$ENDIF COMPILER10_UP}

initialization
  if Supports(BorlandIDEServices, IOTAServices, Services) then
    IdeNotifierIndex := Services.AddNotifier(TIdeNotifier.Create);

  {$IFDEF COMPILER10_UP}
  if Supports(BorlandIDEServices, IOTAAboutBoxServices, AboutBoxServices) then
  begin
    AboutBoxIndex := AboutBoxServices.AddPluginInfo(
      'DFM Check ' + sVersion,
      'DFM Check ' + sVersion + sLineBreak +
      sLineBreak +
      'Copyright (C) 2006-2011 Andreas Hausladen' + sLineBreak +
      'Use at your own risk.',
      0
    );
  end;
  {$ENDIF COMPILER10_UP}

finalization
  {$IFDEF COMPILER10_UP}
  if AboutBoxServices <> nil then
  begin
    AboutBoxServices.RemovePluginInfo(AboutBoxIndex);
    AboutBoxServices := nil;
  end;
  {$ENDIF COMPILER10_UP}

  if Services <> nil then
  begin
    Services.RemoveNotifier(IdeNotifierIndex);
  	Services := nil;
  end;

end.
