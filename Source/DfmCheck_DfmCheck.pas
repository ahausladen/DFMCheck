{******************************************************************************}
{*                                                                            *}
{* DfmCheck                                                                   *}
{*                                                                            *}
{* (C) 2006 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

unit DfmCheck_DfmCheck;

{$I DfmCheck.inc}

interface

uses
  Windows, SysUtils, Classes,
  {$IFDEF IDE}
  ToolsAPI, DfmCheck_ProgressFrm, ActiveX, Dialogs, Controls, Forms,
  {$ELSE}
  ToolsAPIRepl,
  {$ENDIF IDE}
  Contnrs;

type
  TDfmStruct = class(TObject)
  private
    FItems: TObjectList;
    FEvents: TStrings;
    FCompName: string;
    FCompClassName: string;
    function GetCount: Integer;
    function GetItem(Index: Integer): TDfmStruct;
  public
    constructor Create(const ACompName, ACompClassName: string);
    destructor Destroy; override;

    procedure AddComponent(Comp: TDfmStruct);

    property CompName: string read FCompName;
    property CompClassName: string read FCompClassName;

    property Items[Index: Integer]: TDfmStruct read GetItem;
    property Count: Integer read GetCount;

    property Events: TStrings read FEvents;
  end;

  TDfmCheck = class(TObject)
  private
    FProject: IOTAProject;
    FFormFiles: TStrings;
    FFormNames: TStrings;
    {$IFDEF IDE}
    FProgress: TDfmCheck_FormProgress;
    {$ENDIF IDE}
    FProjectFileNameDfmCheckUnit: string;
    FIsBCB: Boolean;
    FCheckConnected: Boolean;
    FConnectedList: TStrings;
  protected
    property Project: IOTAProject read FProject;
    property FormFiles: TStrings read FFormFiles;
    property FormNames: TStrings read FFormNames;

    function Progress(const FileName, FormName: string; Percentage: Integer): Boolean;

    procedure ProcessFormFiles;
    function CreateDfmStruct(Lines: TStrings): TDfmStruct;
    procedure AppendDelphiHeaderCode(Code: TStrings);
    procedure AppendBCBHeaderCode(Code: TStrings);
    procedure AppendDelphiFooterCode(Code: TStrings);
    procedure AppendBCBFooterCode(Code: TStrings);
    procedure AppendDelphiAccessCode(const FormFileName: string; DfmStruct: TDfmStruct; Code: TStrings);
    procedure AppendBCBAccessCode(const FormFileName: string; DfmStruct: TDfmStruct; Code: TStrings);

    function InternCheckForms: Boolean;

    property IsBCB: Boolean read FIsBCB write FIsBCB;
  public
    constructor Create(AProject: IOTAProject);
    destructor Destroy; override;

    function CheckForms(ShowProgress, CheckConnected: Boolean): Boolean;

    property ConnectedList: TStrings read FConnectedList;
    property ProjectFileNameDfmCheckUnit: string read FProjectFileNameDfmCheckUnit;
  end;

{$IFDEF IDE}
procedure OpenCloseForms(Files: TStrings); overload;
procedure OpenCloseForms(Project: IOTAProject); overload;
{$ENDIF IDE}

implementation

uses
  {$IFDEF IDE}
  DfmCheck_TaskbarIntf,
  {$ENDIF IDE}
  DfmCheck_Utils, DfmCheck_AppConsts, DfmCheck_Options;

{$IFDEF IDE}
function DfmCheck_CheckWinVersion(Major, Minor: Integer): Boolean;
begin
  {$IFDEF COMPILER7_UP}
  Result := CheckWin32Version(Major, Minor);
  {$ELSE}
  // Delphi 6 has a wrong implementation and Delphi 5 doesn't have it at all
  Result := (Win32MajorVersion > Major) or
            ((Win32MajorVersion = Major) and (Win32MinorVersion >= Minor));
  {$ENDIF COMPILER7_UP}
end;

function GetTaskbarFormHandle: HWND;
begin
  {$IFDEF COMPILER11_UP}
  if Application.MainFormOnTaskBar then
    Result := Application.MainFormHandle
  else
  {$ENDIF COMPILER11_UP}
    Result := Application.Handle;
end;

procedure BeginTaskbarProgress(var TaskbarList3: ITaskbarList3; MaxValue: Integer);
var
  TaskbarList: ITaskbarList;
  TaskbarFormHandle: HWND;
begin
  if (TaskbarList3 = nil) and DfmCheck_CheckWinVersion(6, 1) then
  begin
    try
      TaskbarList := CreateTaskbarList;
      if not Supports(TaskbarList, IID_ITaskbarList3, TaskbarList3) then
        TaskbarList3 := nil;
    except
      TaskbarList := nil;
      TaskbarList3 := nil;
    end;
  end;

  if TaskbarList3 <> nil then
  begin
    TaskbarFormHandle := GetTaskbarFormHandle;
    if TaskbarFormHandle <> 0 then
    begin
      TaskbarList3.SetProgressValue(TaskbarFormHandle, 0, MaxValue);
      TaskbarList3.SetProgressState(TaskbarFormHandle, TBPF_NORMAL);
    end;
  end;
end;

procedure TaskbarProgress(const TaskbarList3: ITaskbarList3; Progress, MaxValue: Integer);
var
  TaskbarFormHandle: HWND;
begin
  if TaskbarList3 <> nil then
  begin
    TaskbarFormHandle := GetTaskbarFormHandle;
    if TaskbarFormHandle <> 0 then
      TaskbarList3.SetProgressValue(TaskbarFormHandle, Progress, MaxValue);
  end;
end;

procedure EndTaskbarProgress(var TaskbarList3: ITaskbarList3);
var
  TaskbarFormHandle: HWND;
begin
  if TaskbarList3 <> nil then
  begin
    TaskbarFormHandle := GetTaskbarFormHandle;
    if TaskbarFormHandle <> 0 then
      TaskbarList3.SetProgressState(TaskbarFormHandle, TBPF_NOPROGRESS);
    TaskbarList3 := nil;
  end;
end;
{$ENDIF IDE}

{$IFDEF IDE}
function CompareFormResource(NewStream: TMemoryStream; const Filename: string): Boolean;
var
  FileStream: TFileStream;
  OpenStream: TMemoryStream;
  DiskStream: TMemoryStream;
begin
  OpenStream := nil;
  DiskStream := TMemoryStream.Create;
  try
    if TestStreamFormat(NewStream) <> sofBinary then
    begin
      OpenStream := TMemoryStream.Create;
      ObjectTextToBinary(NewStream, OpenStream);
      OpenStream.Position := 0;
      NewStream := OpenStream;
    end;

    FileStream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
    try
      if TestStreamFormat(FileStream) = sofBinary then
        DiskStream.LoadFromStream(FileStream)
      else
        ObjectTextToBinary(FileStream, DiskStream);
    finally
      FileStream.Free;
    end;
    Result := (DiskStream.Size = NewStream.Size) and CompareMem(DiskStream.Memory, OpenStream.Memory, OpenStream.Size);
  finally
    DiskStream.Free;
    OpenStream.Free;
  end;
end;

procedure OpenCloseForms(Files: TStrings);
var
  I: Integer;
  ActionServices: IOTAActionServices;
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  Editor: IOTAEditor;
  FormEditor: IOTAFormEditor;
  Modified: Boolean;
  EditorIndex: Integer;
  FormStream: TMemoryStream;
  YesToAll: Boolean;
  TaskbarList3: ITaskbarList3;
begin
  if Files.Count = 0 then
    Exit;

  if Supports(BorlandIDEServices, IOTAActionServices, ActionServices) then
  begin
    BeginTaskbarProgress(TaskbarList3, Files.Count);
    try
      YesToAll := False;
      ModuleServices := BorlandIDEServices as IOTAModuleServices;
      for I := 0 to Files.Count - 1 do
      begin
        try
          if ActionServices.OpenFile(Files[I]) then
          begin
            TaskbarProgress(TaskbarList3, I + 1, Files.Count);
            { Find out if the file was changed. Work around the DFM "ignore" bug where Modified isn't set }
            Module := ModuleServices.FindModule(Files[I]);
            if Module <> nil then
            begin
              Modified := False;
              for EditorIndex := 0 to Module.GetModuleFileCount - 1 do
              begin
                Editor := Module.GetModuleFileEditor(EditorIndex);
                if Editor.Modified then
                begin
                  Modified := True;
                  Break;
                end
                else if Supports(Editor, IOTAFormEditor, FormEditor) then
                begin
                  { Form could have been altered but the IDE forgot to update the "Modified" flag. }
                  FormStream := TMemoryStream.Create;
                  try
                    FormEditor.GetFormResource(TStreamAdapter.Create(FormStream));
                    FormStream.Position := 0;
                    Modified := not CompareFormResource(FormStream, ChangeFileExt(Files[I], '.dfm'));
                    if Modified then
                      FormEditor.MarkModified;
                  finally
                    FormStream.Free;
                  end;
                  Break;
                end;
              end;

              if Modified then
              begin
                if YesToAll then
                  Module.Save(False, True)
                else
                begin
                  case MessageDlg(Format(sConfirmSaveModifiedFile, [ExtractFileName(Files[I])]),
                                  mtConfirmation, [mbYes, mbNo, mbCancel, mbYesToAll], 0) of
                    mrYes:
                      Module.Save(False, True);
                    mrYesToAll:
                      begin
                        Module.Save(False, True);
                        YesToAll := True;
                      end;
                    mrCancel:
                      Exit;
                  end;
                end;
              end;
            end;
            ActionServices.CloseFile(Files[I]);
          end;
        except
          // ignore every exception
        end;
        Application.ProcessMessages;
      end;
    finally
      EndTaskbarProgress(TaskbarList3);
    end;
  end;
end;

procedure OpenCloseForms(Project: IOTAProject);
var
  I: Integer;
  FileName, FormFileName: string;
  Files: TStrings;
begin
  if Assigned(Project) then
  begin
    Files := TStringList.Create;
    try
      { get all form files with FormName }
      for I := 0 to Project.GetModuleCount - 1 do
      begin
        FileName := Project.GetModule(I).FileName;
        if FileName <> '' then // ignore units that are not "Project Files" like "Forms.pas"
        begin
          FormFileName := ChangeFileExt(FileName, '.dfm');
          if FastFileExists(FormFileName) then
            Files.Add(FileName);
        end;
      end;
      OpenCloseForms(Files);
    finally
      Files.Free;
    end;
  end;
end;
{$ENDIF IDE}

{ TDfmStruct }

constructor TDfmStruct.Create(const ACompName, ACompClassName: string);
begin
  inherited Create;
  FCompName := ACompName;
  FCompClassName := ACompClassName;
  FItems := TObjectList.Create;
  FEvents := TStringList.Create;
end;

destructor TDfmStruct.Destroy;
begin
  FItems.Free;
  FEvents.Free;
  inherited Destroy;
end;

function TDfmStruct.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TDfmStruct.GetItem(Index: Integer): TDfmStruct;
begin
  Result :=  TDfmStruct(FItems[Index]);
end;

procedure TDfmStruct.AddComponent(Comp: TDfmStruct);
begin
  FItems.Add(Comp);
end;

{ TDfmCheck }

constructor TDfmCheck.Create(AProject: IOTAProject);
begin
  inherited Create;
  FIsBCB := IsBCBPersonality(AProject);
  FProject := AProject;
  FFormFiles := TStringList.Create;
  FFormNames := TStringList.Create;
  FConnectedList := TStringList.Create;

  FProjectFileNameDfmCheckUnit := GetProjectDfmCheckUnitFileName(AProject);
end;

destructor TDfmCheck.Destroy;
begin
  FConnectedList.Free;
  FFormFiles.Free;
  FFormNames.Free;
  FProject := nil;
  inherited Destroy;
end;

function TDfmCheck.Progress(const FileName, FormName: string;
  Percentage: Integer): Boolean;
begin
  Result := True;
  {$IFDEF IDE}
  if Assigned(FProgress) then
  begin
    if (FileName = '') and (Percentage = 100) then
      FProgress.Status := RsFinished
    else
      FProgress.Status := FormName + ' (' + ExtractFileName(FileName) + ')';
    FProgress.Percentage := Percentage;
    Result := not FProgress.IsAborted;
  end;
  {$ENDIF IDE}
end;

function TDfmCheck.CheckForms(ShowProgress, CheckConnected: Boolean): Boolean;
begin
  FCheckConnected := CheckConnected;
  {$IFDEF IDE}
  if ShowProgress then
  begin
    FProgress := TDfmCheck_FormProgress.Create(nil);
    try
      Result := FProgress.Execute(InternCheckForms);
      if FProgress.IsAborted then
        Result := False;
    finally
      FreeAndNil(FProgress);
    end;
  end
  else
  {$ENDIF IDE}
    Result := InternCheckForms;
end;

function TDfmCheck.InternCheckForms: Boolean;
var
  I: Integer;
  FileName, FormFileName: string;
begin
  FormFiles.Clear;
  FormNames.Clear;

  { get all form files with FormName }
  for I := 0 to Project.GetModuleCount - 1 do
  begin
    FileName := Project.GetModule(I).FileName;
    if FileName <> '' then // ignore units that are not "Project Files" like "Forms.pas"
    begin
      FormFileName := ChangeFileExt(FileName, '.dfm');
      if FastFileExists(FormFileName) then
      begin
        FormFiles.Add(FormFileName);
        if Project.GetModule(I).FormName = '' then // IDE sometimes does not recognize all forms
          FormNames.Add(ReadFormClassNameFromFile(FormFileName))
        else
          FormNames.Add('T' + Project.GetModule(I).FormName);
      end;
    end;
  end;
  ProcessFormFiles;
  Result := True;
end;

procedure TDfmCheck.ProcessFormFiles;
var
  I: Integer;
  Stream: TStream;
  MemStream: TMemoryStream;
  Code: TStrings;
  Lines: TStrings;
  DfmStruct: TDfmStruct;
  {$IFDEF IDE}
  k, l: Integer;
  Modules: IOTAModuleServices;
  Module: IOTAModule;
  Editor: IOTAFormEditor;
  {$ENDIF IDE}
  Modified: Boolean;
begin
  {$IFDEF IDE}
  Modules := BorlandIDEServices as IOTAModuleServices;
  {$ENDIF IDE}
  Code := TStringList.Create;
  try
    if IsBCB then
      AppendBCBHeaderCode(Code)
    else
      AppendDelphiHeaderCode(Code);

    { Convert binary to text and generate pas files }
    for I := 0 to FormFiles.Count - 1 do
    begin
      if not Progress(FormFiles[I], FormNames[I], I * 100 div FormFiles.Count) then
        Exit;

      {$IFDEF IDE}
      Stream := nil;
      try
        for k := 0 to Modules.ModuleCount - 1 do
        begin
          Module := Modules.Modules[k];
          if SameText(ChangeFileExt(Module.FileName, '.dfm'), FormFiles[I]) then
          begin
            for l := 0 to Module.GetModuleFileCount - 1 do
            begin
              if Supports(Module.GetModuleFileEditor(l), IOTAFormEditor, Editor) then
              begin
                Stream := TMemoryStream.Create;
                Editor.GetFormResource(TStreamAdapter.Create(Stream));
                Stream.Position := 0;
                Break;
              end;
            end;
          end;
          if Stream <> nil then
            Break;
        end;
      except
        ;
      end;

      if Stream = nil then
      {$ENDIF IDE}
        Stream := TFileStream.Create(FormFiles[I], fmOpenRead or fmShareDenyWrite);
      try
        case TestStreamFormat(Stream) of
          sofUnknown:
            raise EDfmCheckError.CreateResFmt(@RsInvalidDfmFile, [ExtractFileName(FormFiles[I])]);
          sofBinary:
            begin
              MemStream := TMemoryStream.Create;
              try
                ObjectResourceToText(Stream, MemStream);
              finally
                Stream.Free;
                Stream := MemStream;
              end;
            end;
        end;
        Stream.Position := 0;
        Lines := TStringList.Create;
        try
          Lines.LoadFromStream(Stream);
          DfmStruct := CreateDfmStruct(Lines);
          if Assigned(DfmStruct) then
          begin
            if IsBCB then
              AppendBCBAccessCode(FormFiles[I], DfmStruct, Code)
            else
              AppendDelphiAccessCode(FormFiles[I], DfmStruct, Code);
            DfmStruct.Free;
          end;
        finally
          Lines.Free;
        end;
      finally
        Stream.Free;
      end;
    end;
    if FormFiles.Count > 0 then
      if not Progress('', '', 100) then
        Exit;

    if IsBCB then
      AppendBCBFooterCode(Code)
    else
      AppendDelphiFooterCode(Code);

    Modified := True;
    if FastFileExists(ProjectFileNameDfmCheckUnit) then
    begin
      Lines := TStringList.Create;
      try
        Lines.LoadFromFile(ProjectFileNameDfmCheckUnit);
        if CompareStrings(Lines, Code) then
          Modified := False;
      finally
        Lines.Free;
      end;
    end;
    {$IFDEF IDE}
    for k := 0 to Modules.ModuleCount - 1 do
    begin
      Module := Modules.Modules[k];
      if SameText(Module.FileName, ProjectFileNameDfmCheckUnit) then
      begin
        Module.CloseModule(True);
        Application.ProcessMessages;
        Break;
      end;
    end;
    {$ENDIF IDE}
    if Modified then
      Code.SaveToFile(ProjectFileNameDfmCheckUnit);
  finally
    Code.Free;
  end;
end;

function TDfmCheck.CreateDfmStruct(Lines: TStrings): TDfmStruct;
var
  I: Integer;
  LineParser: TLineParser;
  CompName, CompClassName: string;
  AccessStack: TObjectStack;
  OwnerStack: TObjectStack;
  CollectionStack: TStack;
  Comp: TDfmStruct;
  Kind: string;
  {$IFDEF EVENTS}
  PropName: string;
  PropValue: string;
  Cls: TClass;
  ps: Integer;
  Info: PPropInfo;
  {$ENDIF EVENTS}
begin
  Result := nil;
  AccessStack := TObjectStack.Create;
  OwnerStack := TObjectStack.Create;
  CollectionStack := TStack.Create;
  LineParser := TLineParser.Create;
  try
    for I := 0 to Lines.Count - 1 do
    begin
      LineParser.Text := Lines[I];
      if LineParser.Next then
      begin
        Kind := LowerCase(LineParser.Current);
        if (Kind = 'object') or
           (Kind = 'inline') or
           (Kind = 'inherited') then
        begin // "o/i Name: TName"
          if LineParser.Next then
          begin
            CompName := LineParser.Current;
            if LineParser.Next and (LineParser.Current = ':') and
               LineParser.Next then
            begin
              CompClassName := LineParser.Current;
              if CompClassName <> '' then
              begin
                Comp := TDfmStruct.Create(CompName, CompClassName);
                OwnerStack.Push(Comp);
                if Result = nil then
                begin
                  Result := Comp;
                  AccessStack.Push(Comp);
                end
                else
                begin
                  if Kind = 'object' then
                    Result.AddComponent(Comp)
                  else
                    TDfmStruct(AccessStack.Peek).AddComponent(Comp);
                  if (FormNames.IndexOf(CompClassName) >= 0) or (Kind = 'inline') then
                    AccessStack.Push(Comp);
                end;
              end;
            end
            else
            begin
              //if SameText(CompName[1], 'T') then
              begin
                CompClassName := CompName;
                CompName := '';
                // unnamed component
                Comp := TDfmStruct.Create(CompName, CompClassName);
                OwnerStack.Push(Comp);
                if Kind = 'object' then
                  Result.AddComponent(Comp)
                else
                  TDfmStruct(AccessStack.Peek).AddComponent(Comp);
                if (FormNames.IndexOf(CompClassName) >= 0) or (Kind = 'inline') then
                  AccessStack.Push(Comp);
              end;
            end;
          end;
        end
        else
        if SameText(LineParser.Current, 'item') then
        begin
          if not LineParser.Next then
            CollectionStack.Push(nil);
        end
        else
        if SameText(LineParser.Current, 'end') then
        begin
          if CollectionStack.Count > 0 then
            CollectionStack.Pop
          else
          begin
            { if the top of the stack is a frame then remove }
            if OwnerStack.Pop = AccessStack.Peek then
              AccessStack.Pop;
          end;
        end
        else
        if FCheckConnected and (LineParser.Current = 'Connected') then
        begin
          if LineParser.Next and (LineParser.Current = '=') and LineParser.Next then
          begin
            if SameText(LineParser.Current, 'True') then
              FConnectedList.Add(CompName + ': ' + CompClassName);
          end;
{$IFDEF EVENTS}
        end
        else
        begin
          { find event handler }
          PropName := LineParser.Current;
          if PropName[1] in ['A'..'Z', 'a'..'z', '_'] then
          begin
            { get full PropName }
            while LineParser.Next and (LineParser.Current <> '=') do
            begin
              if (LineParser.Current = '.') and LineParser.Next and
                 (LineParser.Current[1] in ['A'..'Z', 'a'..'z', '_']) then
                PropName := PropName + '\' + LineParser.Current;
            end;
            if (LineParser.Current = '=') and LineParser.Next then
            begin
              PropValue := LineParser.Current;
              if (PropValue[1] in ['A'..'Z', 'a'..'z', '_']) and not LineParser.Next then
              begin
                 // maybe an event handler
                Cls := GetClass(TDfmStruct(OwnerStack.Peek).CompClassName);
                if Cls <> nil then
                begin
                  { Get prop info }
                  Info := nil;
                  ps := Pos('.', PropName);
                  while ps > 0 do
                  begin
                    Info := GetPropInfo(Cls, Copy(PropName, 1, ps - 1));
                    if Info = nil then
                      Break;
                    if Info.PropType^.Kind <> tkClass then
                    begin
                      Cls := GetObjectPropClass(Info);
                      Break;
                    end;
                    PropName := Copy(PropName, ps + 1, MaxInt);
                    ps := Pos('.', PropName);
                  end;
                  if Cls <> nil then
                    Info := GetPropInfo(Cls, Copy(PropName, 1, ps - 1));

                  if (Info <> nil) and (Info.PropType^.Kind = tkMethod) then
                    TDfmStruct(AccessStack.Peek).Events.Add(PropValue);

//                  AllocConsole;
//                  WriteLn(PropName + ' = ' + PropValue);*)
                end;
              end;
            end;
          end;
{$ENDIF EVENTS}
        end;
      end;
    end;
  finally
    LineParser.Free;
    CollectionStack.Free;
    AccessStack.Free;
    OwnerStack.Free;
  end;
end;

procedure TDfmCheck.AppendDelphiAccessCode(const FormFileName: string;
  DfmStruct: TDfmStruct; Code: TStrings);

  procedure AppendAccessCodeComp(Comp: TDfmStruct; const Prefix: string);
  var
    I: Integer;
  begin
    for I := 0 to Comp.Count - 1 do
    begin
      if Comp.Items[I].CompName = '' then
      begin
        if Options.ShowHintForUnnamedComponents then
          Code.Add('    {$MESSAGE HINT ''Components[' + IntToStr(I) + ']: ' + Comp.Items[I].CompClassName + ' has no name''}');
        if FormNames.IndexOf(Comp.Items[I].CompClassName) >= 0 then
          AppendAccessCodeComp(Comp.Items[I], Comp.Items[I].CompClassName + '(' + Prefix + 'Components[' + IntToStr(I) + ']).');
      end
      else
      begin
        Code.Add('    ' + Prefix + Comp.Items[I].CompName + '.ClassName; { ' + Comp.Items[I].CompName + ': ' + Comp.Items[I].CompClassName + '; }');
        AppendAccessCodeComp(Comp.Items[I], Prefix + Comp.Items[I].CompName + '.');
      end;
    end;
    for I := 0 to Comp.Events.Count - 1 do
      Code.Add('    @' + Comp.Events[I]);
  end;

var
  I: Integer;
begin
  Code.Add('{ ' + DfmStruct.CompName + ': ' + DfmStruct.CompClassName + ' }');
  Code.Add('  with ' + DfmStruct.CompClassName + '(nil) do { ' + ExtractFileName(ChangeFileExt(FormFileName, '.pas')) + ' }');
  Code.Add('  begin');
  for I := 0 to FConnectedList.Count - 1 do
    Code.Add('    {$MESSAGE ERROR ''' + FConnectedList[I] + ' (Connected = True)''}');
  AppendAccessCodeComp(DfmStruct, '');
  Code.Add('  end;');
  Code.Add('');
end;

procedure TDfmCheck.AppendBCBAccessCode(const FormFileName: string;
  DfmStruct: TDfmStruct; Code: TStrings);

  procedure AppendAccessCodeComp(Comp: TDfmStruct; const Prefix: string);
  var
    I: Integer;
  begin
    for I := 0 to Comp.Count - 1 do
    begin
      if Comp.Items[I].CompName = '' then
      begin
        if Options.ShowHintForUnnamedComponents then
          Code.Add('#pragma message Components[' + IntToStr(I) + ']: ' + Comp.Items[I].CompClassName + ' has no name''}');
        if FormNames.IndexOf(Comp.Items[I].CompClassName) >= 0 then
          AppendAccessCodeComp(Comp.Items[I], '(' + Comp.Items[I].CompClassName + ')(' + Prefix + 'Components[' + IntToStr(I) + '])->');
      end
      else
      begin
        Code.Add('  ' + Prefix + Comp.Items[I].CompName + '->ClassName(); /* ' + Comp.Items[I].CompClassName + ' ' + Comp.Items[I].CompName + '; */');
        AppendAccessCodeComp(Comp.Items[I], Prefix + Comp.Items[I].CompName + '->');
      end;
    end;
    for I := 0 to Comp.Events.Count - 1 do
      Code.Add('  if (' + Comp.Events[I] + ') ;');
  end;

var
  I: Integer;
begin
  Code.Add('/* ' + DfmStruct.CompClassName + ' ' + DfmStruct.CompName + ' */');
  Code.Add('  ' + DfmStruct.CompClassName + ' *' + DfmStruct.CompName + ' = 0; /* ' + ExtractFileName(ChangeFileExt(FormFileName, '')) + ' */');
  for I := 0 to FConnectedList.Count - 1 do
    Code.Add('#pragma message ' + FConnectedList[I] + ' (Connected = True)');
  AppendAccessCodeComp(DfmStruct, DfmStruct.CompName + '->');
  Code.Add('');
end;

procedure TDfmCheck.AppendDelphiHeaderCode(Code: TStrings);
var
  I: Integer;
begin
  Code.Add('unit ' + ChangeFileExt(ExtractFileName(ProjectFileNameDfmCheckUnit), '') + ';');
  Code.Add('');
  Code.Add('interface');
  Code.Add('');
  Code.Add('implementation');
  Code.Add('');
  Code.Add('uses');
  for I := 0 to FormFiles.Count - 1 do
    Code.Add('  ' + ChangeFileExt(ExtractFileName(FormFiles[I]), '') + ',');
  Code.Add('  SysUtils;');
  Code.Add('');
  Code.Add('procedure TestDfmFormConsistency;');
  Code.Add('begin');
end;

procedure TDfmCheck.AppendDelphiFooterCode(Code: TStrings);
begin
  Code.Add('end;');
  Code.Add('');
  Code.Add('end.');
end;

procedure TDfmCheck.AppendBCBHeaderCode(Code: TStrings);
var
  I: Integer;
begin
  for I := 0 to FormFiles.Count - 1 do
    Code.Add('#include "' + ChangeFileExt(ExtractFileName(FormFiles[I]), '.h') + '"');
  Code.Add('');
  Code.Add('void TestDfmFormConsistency()');
  Code.Add('{');
end;

procedure TDfmCheck.AppendBCBFooterCode(Code: TStrings);
begin
  Code.Add('}');
  Code.Add('');
end;

end.
