program DfmCheck;

{$APPTYPE CONSOLE}

{$SetPEFlags 1}
{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

uses
  Windows,
  SysUtils,
  Classes,
  StrUtils,
  RTLConsts,
  DfmCheck_DfmCheck in '..\DfmCheck_DfmCheck.pas',
  DfmCheck_Options in '..\DfmCheck_Options.pas',
  DfmCheck_PascalParser in '..\DfmCheck_PascalParser.pas',
  DfmCheck_Utils in '..\DfmCheck_Utils.pas',
  DfmCheck_AppConsts in '..\DfmCheck_AppConsts.pas',
  ToolsAPIRepl in 'ToolsAPIRepl.pas',
  ProjectFileReader in 'ProjectFileReader.pas';

{.$R *.res}
{$R '..\..\Version.res'}

var
  SuccessDeleteFiles: TStrings;

procedure CopyProjFile(const ProjectFileName, NewProjectFileName, SourceProj, DestProj: string);
var
  Lines: TStrings;
  S: string;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(SourceProj);
    S := Lines.Text;
    S := StringReplace(S, ProjectFileName, NewProjectFileName, [rfReplaceAll, rfIgnoreCase]);
    Lines.Text := S;
    Lines.SaveToFile(DestProj);
  finally
    Lines.Free;
  end;
end;

function CreateDFMCheckUnit(const ProjectFileName: string): string;
var
  DfmCheck: TDfmCheck;
  Project: IOTAProject;
  Lines: TStrings;
  Content: string;
  RelDfmCheckUnit: string;
begin
  Result := '';
  Options := TOptions.Create; // Globale Options-Variable für DFMCheck initialisieren
  try
    if not FileExists(ProjectFileName) then
      raise EInOutError.Create(SFileNotFound + sLineBreak + ProjectFileName);
    Project := LoadProject(ProjectFileName);
    if Project.ModuleCount > 0 then
    begin
      DfmCheck := TDfmCheck.Create(Project);
      try
        Result := ChangeFileExt(Project.FileName, '_dfmcheck' + ExtractFileExt(ProjectFileName));
        DfmCheck.CheckForms(True, False);
        SuccessDeleteFiles.Add(DfmCheck.ProjectFileNameDfmCheckUnit);

        Lines := TStringList.Create;
        try
          Lines.LoadFromFile(Project.FileName);
          Content := Lines.Text;
          RelDfmCheckUnit := ExtractRelativePath(ExtractFilePath(Project.FileName), DfmCheck.ProjectFileNameDfmCheckUnit);
          if AppendProjectUnit(Content, RelDfmCheckUnit) then
          begin
            Lines.Text := Content;
            Lines.SaveToFile(Result);
            CopyFile(PChar(ChangeFileExt(ProjectFileName, '.cfg')), PChar(ChangeFileExt(Result, '.cfg')), False);
            if FileExists(ChangeFileExt(ProjectFileName, '.dproj')) then
              CopyProjFile(ExtractFileName(ProjectFileName), ExtractFileName(Result), ChangeFileExt(ProjectFileName, '.dproj'), ChangeFileExt(Result, '.dproj'));
            //if FileExists(ChangeFileExt(ProjectFileName, '.cbproj')) then
            //  CopyProjFile(ExtractFileName(ProjectFileName), ExtractFileName(Result), ChangeFileExt(ProjectFileName, '.cbproj'), ChangeFileExt(Result, '.cbproj'));
          end;
        finally
          Lines.Free;
        end;
      finally
        DfmCheck.Free;
      end;
    end;
  finally
    Options.Free;
  end;
end;

procedure Main;
var
  I: Integer;
  //PrjFile: string;
begin
  if ParamCount = 0 then
  begin
    WriteLn('Usage:');
    WriteLn('  DfmCheck.exe Project.dpr [Project2.dpk] [...]');
    WriteLn;
    ExitCode := 1;
    Exit;
  end;

  SuccessDeleteFiles := TStringList.Create;
  try
    for I := 1 to ParamCount do
      CreateDFMCheckUnit(ParamStr(I));

    {for I := 0 to SuccessDeleteFiles.Count - 1 do
      DeleteFile(SuccessDeleteFiles[I]);
    DeleteFile(PrjFile);
    DeleteFile(ChangeFileExt(PrjFile, '.cfg'));}
  finally
    SuccessDeleteFiles.Free;
  end;
  ExitCode := 0;
end;

begin
  try
    Main;
  except
    on E: Exception do
    begin
      WriteLn(ErrOutput, E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.

