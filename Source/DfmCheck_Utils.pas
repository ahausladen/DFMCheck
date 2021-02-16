unit DfmCheck_Utils;

{$I DfmCheck.inc}
{$IFDEF UNICODE}
  {$WARN WIDECHAR_REDUCED OFF}
{$ENDIF UNICODE}

interface

uses
  Windows, SysUtils,
  {$IFDEF IDE}
  ToolsAPI, Forms,
  {$ELSE}
  ToolsAPIRepl,
  {$ENDIF IDE}
  Classes;

type
  EDfmCheckError = class(Exception);

function FastFileExists(const FileName: string): Boolean;
function AddPathDelim(const Dir: string): string;

function ReadFormClassNameFromFile(const FileName: string): string;
function CompareStrings(Sl1, Sl2: TStrings): Boolean;

type
  TLineParser = class(TObject)
  private
    FIndex: Integer;
    FCurrent: string;
    FText: string;
    FTextLen: Integer;
    procedure SetText(const Value: string);
  public
    constructor Create;
    function Next: Boolean;
    property Current: string read FCurrent;

    property Text: string read FText write SetText;
  end;

{$IFDEF IDE}
function NServices: INTAServices; {$IFDEF COMPILER9_UP} inline; {$ENDIF}
function GetProjectGroup: IOTAProjectGroup;
function GetActiveProject: IOTAProject;
function GetEditorSource(Editor: IOTAEditBuffer): string;
procedure SetEditorSource(Editor: IOTAEditBuffer; const Content: string);

function IsAlreadyInProject(Project: IOTAProject; const FileName: string): Boolean;
function SilentAddUnitToProject(Project: IOTAProject; const FileName: string): Boolean;
function SilentRemoveUnitFromProject(Project: IOTAProject; const FileName: string): Boolean;
{$ENDIF IDE}

function IsBCBPersonality(Project: IOTAProject): Boolean;
function GetProjectDfmCheckUnitFileName(Project: IOTAProject): string;

function AppendProjectUnit(var ProjectContent: string; const UnitFileName: string): Boolean;
function RemoveProjectUnit(var ProjectContent: string; const UnitFileName: string): Boolean;

implementation

uses
  DfmCheck_AppConsts, DfmCheck_PascalParser;

{$IFDEF COMPILER5}
const
  sLineBreak = #13#10;
{$ENDIF COMPILER5}

{$IFNDEF UNICODE}
type
  RawByteString = AnsiString;
{$ENDIF ~UNICODE}

function TrimRightA(const S: RawByteString): RawByteString;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (S[I] <= ' ') do
    Dec(I);
  Result := Copy(S, 1, I);
end;

function FastFileExists(const FileName: string): Boolean;
begin
  Result := GetFileAttributes(PChar(FileName)) and FILE_ATTRIBUTE_DIRECTORY = 0;
end;

function AddPathDelim(const Dir: string): string;
begin
  if (Dir <> '') and (Dir[Length(Dir)] <> '\') then
    Result := Dir + '\'
  else
    Result := Dir;
end;

function ReadFormClassNameFromFile(const FileName: string): string;
var
  f: TextFile;
  Line: string;
  ps: Integer;
  Stream: TFileStream;
  MemStream: TStringStream;
begin
  AssignFile(f, FileName);
  Reset(f);
  while not Eof(f) do
  begin
    ReadLn(f, Line);
    if Trim(Line) <> ''  then
      Break;
  end;
  CloseFile(f);

  if (Line <> '') and (Ord(Line[1]) = $FF) then
  begin
    // binary DFM file
    MemStream := TStringStream.Create(''{$IFDEF COMPILER12_UP}, TEncoding.UTF8{$ENDIF});
    try
      Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
      try
        ObjectResourceToText(Stream, MemStream);
      finally
        Stream.Free;
      end;
      Line := Trim(Copy(MemStream.DataString, 1, Pos(#10, MemStream.DataString) - 1));
    finally
      MemStream.Free;
    end;
  end;
  ps := Pos(':', Line);
  if ps > 0 then
    Result := Trim(Copy(Line, ps + 1, MaxInt))
  else
    raise EDfmCheckError.CreateResFmt(@RsInvalidDfmFile, [ExtractFileName(FileName)]);
end;

function CompareStrings(Sl1, Sl2: TStrings): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Sl1.Count <> Sl2.Count then
    Exit;
  for i := 0 to Sl1.Count - 1 do
    if Sl1[i] <> Sl2[i] then
      Exit;
  Result := True;
end;

{ TLineParser }

constructor TLineParser.Create;
begin
  inherited Create;
  FIndex := 1;
end;

function TLineParser.Next: Boolean;
const
  Symbols: set of AnsiChar = [#33..#$FF] - ['A'..'Z', 'a'..'z', '0'..'9', '_'];
var
  Start: Integer;
begin
  while (FIndex <= FTextLen) and (FText[FIndex] <= ' ') do
    Inc(FIndex);

  if FIndex > FTextLen then
  begin
    Result := False;
    Exit;
  end;

  Start := FIndex;
  if FText[FIndex] in (['A'..'Z', 'a'..'z', '_']) then
  begin
    while (FIndex <= FTextLen) and (FText[FIndex] in ['A'..'Z', 'a'..'z', '_', '0'..'9']) do
      Inc(FIndex);
  end
  else
  if FText[FIndex] in (['0'..'9', '.']) then
  begin
    while (FIndex <= FTextLen) and (FText[FIndex] in ['0'..'9', '.']) do
      Inc(FIndex);
  end
  else
  if FText[FIndex] in Symbols then
  begin
    while (FIndex <= FTextLen) and (FText[FIndex] in Symbols) do
      Inc(FIndex);
  end;

  FCurrent := Copy(FText, Start, FIndex - Start);
  if FCurrent[1] = '''' then // string
  begin
    FCurrent := Copy(FText, Start, MaxInt);
    FIndex := FTextLen;
  end;
  Result := True;
end;

procedure TLineParser.SetText(const Value: string);
begin
  FText := Value;
  FTextLen := Length(FText);
  FIndex := 1;
end;

{$IFDEF IDE}
function NServices: INTAServices; {$IFDEF COMPILER9_UP} inline; {$ENDIF}
begin
  Result := BorlandIDEServices as INTAServices;
end;

function GetProjectGroup: IOTAProjectGroup;
var
  i: Integer;
  Modules: IOTAModuleServices;
begin
  if Supports(BorlandIDEServices, IOTAModuleServices, Modules) then
    for i := 0 to Modules.ModuleCount - 1 do
      if Supports(Modules.Modules[i], IOTAProjectGroup, Result) then
        Exit;
  Result := nil;
end;

function GetActiveProject: IOTAProject;
{$IFDEF COMPILER9_UP}
begin
  Result := ToolsAPI.GetActiveProject;
end;
{$ELSE}
var
  Group: IOTAProjectGroup;
begin
  Group := GetProjectGroup;
  if Group <> nil then
    Result := Group.ActiveProject
  else
    Result := nil;
end;
{$ENDIF COMPILER9_UP}

function GetEditorSource(Editor: IOTAEditBuffer): string;
const
  MaxBufSize = 15872; // The compiler uses this number of bytes for read operations.
var
  Readn: Integer;
  Buf: array[0..MaxBufSize] of AnsiChar;
  Data: RawByteString;
  Reader: IOTAEditReader;
begin
  Data := '';
  Reader := Editor.CreateReader;
  repeat
    Readn := Reader.GetText(Length(Data), Buf, MaxBufSize);
    Buf[Readn] := #0;
    Data := Data + Buf;
  until Readn < MaxBufSize;
  Result := {$IFDEF UNICODE}UTF8ToString{$ENDIF}(Data);
end;

procedure SetEditorSource(Editor: IOTAEditBuffer; const Content: string);
var
  Writer: IOTAEditWriter;
  ReadOnly: Boolean;
  Data: RawByteString;
begin
  Data := TrimRightA({$IFDEF UNICODE}UTF8Encode{$ENDIF}(Content)) + sLineBreak;
  ReadOnly := Editor.IsReadOnly;
  try
    Editor.IsReadOnly := False;
    Writer := Editor.CreateUndoableWriter;
    Writer.DeleteTo(MaxInt); // clear buffer
    Writer.Insert(PAnsiChar(Data));
  finally
    if Editor.IsReadOnly <> ReadOnly then
      Editor.IsReadOnly := ReadOnly;
  end;
end;

function AddUnitTo(var Data: string; const FileName, Keyword: string): Boolean;
var
  Parser: TPascalParser;
  token: PTokenInfo;
begin
  Result := False;

  Parser := TPascalParser.Create('', Data);
  try
    while Parser.GetToken(token) do
    begin
      if (token.Kind = tkIdent) and SameText(token.Value, Keyword) then
      begin
        while (token <> nil) and (token.Value <> ';') do
          token := Parser.GetToken;
        if token <> nil then
        begin
          Parser.Replace(token, token, ',' + sLineBreak + '  ' +
            ChangeFileExt(ExtractFileName(FileName), '') {+ ' in ' +
            QuotedStr(ExtractFileName(FileName))} + ';');
          Result := True;
          Data := Parser.Text;
        end;
        Break;
      end;
    end;
  finally
    Parser.Free;
  end;
end;

function RemoveUnitFrom(var Data: string; const FileName, Keyword: string): Boolean;
var
  Parser: TPascalParser;
  token: PTokenInfo;
  ItemName: string;
  StartIndex: Integer;
begin
  Result := False;
  ItemName := ChangeFileExt(ExtractFileName(FileName), '');

  Parser := TPascalParser.Create('', Data);
  try
    while Parser.GetToken(token) do
    begin
      if (token.Kind = tkIdent) and SameText(token.Value, Keyword) then
      begin
        while (token <> nil) and (CompareText(token.Value, ItemName) <> 0) do
          token := Parser.GetToken;
        if token <> nil then
        begin
          StartIndex := Parser.PreToken.StartIndex;
          while (token <> nil) and (token.Value <> ';') and (token.Value <> ',') do
            token := Parser.GetToken;
          if token <> nil then
          begin
            Parser.Delete(StartIndex, token.StartIndex - StartIndex);
            Data := Parser.Text;
            Result := True;
          end;
          Break;
        end;
      end;
    end;
  finally
    Parser.Free;
  end;
end;

function IsAlreadyInProject(Project: IOTAProject; const FileName: string): Boolean;
var
  S: string;
  i: Integer;
begin
  Result := False;
  S := ChangeFileExt(ExtractFileName(FileName), '');
  for i := 0 to Project.GetModuleCount - 1 do
  begin
    if SameText(ExtractFileName(Project.GetModule(i).Name), S) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function SilentAddUnitToProject(Project: IOTAProject; const FileName: string): Boolean;
var
  i: Integer;
  FileAlreadyInList: Boolean;
  Ext, PrjFileNameCpp: string;
  Data: string;
  EditBuffer: IOTAEditBuffer;
begin
  Result := False;
  //FileAlreadyInList := IsAlreadyInProject(Project, FileName);
  SilentRemoveUnitFromProject(Project, FileName);
  FileAlreadyInList := False;

  if IsBCBPersonality(Project) then
  begin
    Result := not FileAlreadyInList;
    if Result then
    begin
      PrjFileNameCpp := ChangeFileExt(Project.FileName, '.cpp');
      for i := 0 to Project.GetModuleFileCount - 1 do
      begin
        if SameText(Project.GetModuleFileEditor(i).FileName, PrjFileNameCpp) then
        begin
          if Supports(Project.GetModuleFileEditor(i), IOTAEditBuffer, EditBuffer) then
          begin
            Data := GetEditorSource(EditBuffer);
            Data := Data + sLineBreak + '#include "' + ExtractFileName(FileName) + '"';
            if Result then
            begin
              SetEditorSource(EditBuffer, Data);
              Project.GetModuleFileEditor(i).Show;
              Project.GetModuleFileEditor(i).MarkModified;
            end;
          end;
          Break;
        end;
      end;
//      Project.AddFile(FileName, True);
    end;
  end
  else
  begin
    { Add ProjectFileNameDfmCheckUnit to the project }
    if not FileAlreadyInList then
    begin
      for i := 0 to Project.GetModuleFileCount - 1 do
      begin
        Ext := AnsiLowerCase(ExtractFileExt(Project.GetModuleFileEditor(i).FileName));
        if (Ext = '.dpr') or (Ext = '.dpk') then
        begin
          if Supports(Project.GetModuleFileEditor(i), IOTAEditBuffer, EditBuffer) then
          begin
            Data := GetEditorSource(EditBuffer);
            if Ext = '.dpr' then
              Result :=  AddUnitTo(Data, FileName, 'uses')
            else
            if Ext = '.dpk' then
              Result :=  AddUnitTo(Data, FileName, 'contains');

            if Result then
            begin
              SetEditorSource(EditBuffer, Data);
              Project.GetModuleFileEditor(i).Show;
              Project.GetModuleFileEditor(i).MarkModified;
            end;
          end;
          Break;
        end;
      end;
    end;
  end;
end;

function SilentRemoveUnitFromProject(Project: IOTAProject; const FileName: string): Boolean;
var
  i: Integer;
  Ext, PrjFileNameCpp: string;
  S, Data: string;
  ps: Integer;
  EditBuffer: IOTAEditBuffer;
begin
  Result := False;
  if IsBCBPersonality(Project) then
  begin
    PrjFileNameCpp := ChangeFileExt(Project.FileName, '.cpp');
    for i := 0 to Project.GetModuleFileCount - 1 do
    begin
      if SameText(Project.GetModuleFileEditor(i).FileName, PrjFileNameCpp) then
      begin
        if Supports(Project.GetModuleFileEditor(i), IOTAEditBuffer, EditBuffer) then
        begin
          Data := GetEditorSource(EditBuffer);
          S := sLineBreak + '#include "' + ExtractFileName(FileName) + '"';
          ps := Pos(S, Data);
          if ps > 0 then
          begin
            Data := Copy(Data, 1, ps - 1) + Copy(Data, ps + Length(S), MaxInt);
            SetEditorSource(EditBuffer, Data);
            Project.GetModuleFileEditor(i).MarkModified;
          end;
        end;
        Break;
      end;
    end;
//    Project.RemoveFile(FileName);
  end
  else
  begin
    { Remove ProjectFileNameDfmCheckUnit from the project }
    try
      for i := 0 to Project.GetModuleFileCount - 1 do
      begin
        Ext := AnsiLowerCase(ExtractFileExt(Project.GetModuleFileEditor(i).FileName));
        if (Ext = '.dpr') or (Ext = '.dpk') then
        begin
          if Supports(Project.GetModuleFileEditor(i), IOTAEditBuffer, EditBuffer) then
          begin
            // make sure that the file is not listed anymore (especially for D7)
            Data := GetEditorSource(EditBuffer);
            if Data <> '' then
            begin
              if Ext = '.dpr' then
                Result :=  RemoveUnitFrom(Data, FileName, 'uses')
              else
              if Ext = '.dpk' then
                Result :=  RemoveUnitFrom(Data, FileName, 'contains');
            end;
            if Result then
            begin
              SetEditorSource(EditBuffer, Data);

(*              {$IFNDEF COMPILER6_UP}
              SetEditorSource(EditBuffer, Data)
              {$ELSE}
              if not EditBuffer.Undo then
                SetEditorSource(EditBuffer, Data)
              else
              begin // undo didn't undo the changes (IDE bug)
                Data := GetEditorSource(EditBuffer);
                if Data <> '' then
                begin
                  if Ext = '.dpr' then
                    Result :=  RemoveUnitFrom(Data, FileName, 'uses')
                  else
                  if Ext = '.dpk' then
                    Result :=  RemoveUnitFrom(Data, FileName, 'contains');
                  if Result then
                    SetEditorSource(EditBuffer, Data)
                end;
              end;
              {$ENDIF ~COMPILER6_UP}
*)
            end;
          end;
          Break;
        end;
      end;
      { RemoveFile destroys the dpr/dpk file if the file isn't in the uses clause anymore }
      //Project.RemoveFile(FileName); // Delphi 2007 adds the file to the dproj file and then nothing works
    except
      // ignore all OpenToolsAPI bugs
    end;
  end;
end;

function IsBCBPersonality(Project: IOTAProject): Boolean;
begin
  {$IFDEF COMPILER10_UP}
  Result := SameText(Project.Personality, sCBuilderPersonality);
  {$ELSE}
  Result := SameText(ExtractFileName(ParamStr(0)), 'bcb.exe');
  {$ENDIF COMPILER10_UP}
end;

{$ELSE}

function IsBCBPersonality(Project: IOTAProject): Boolean;
begin
  Result := SameText(ExtractFileExt(Project.FileName), '.bpr') or
            SameText(ExtractFileExt(Project.FileName), '.bpk');
end;

{$ENDIF IDE}

function GetProjectDfmCheckUnitFileName(Project: IOTAProject): string;
begin
  if IsBCBPersonality(Project) then
    Result := ChangeFileExt(Project.FileName, '') + '_DfmCheck_Unit.cpp'
  else
    Result := ChangeFileExt(Project.FileName, '') + '_DfmCheck_Unit.pas';
end;

function AppendRemoveUnit(var Content: string; const UnitFileName: string; RemoveUnit: Boolean): Boolean;
var
  UnitName: string;
  N, FN: string;
  Parser: TPascalParser;
  Token: PTokenInfo;
  NxtToken: PTokenInfo;
  RemoveStartIndex: Integer;

  function NextToken(out Token: PTokenInfo): Boolean;
  begin
    repeat
      Result := Parser.GetToken(Token);
    until not Result or (Token.Kind <> tkComment);
  end;

begin
  RemoveStartIndex := -1;
  Result := False;
  UnitName := ChangeFileExt(ExtractFileName(UnitFileName), '');
  Parser := TPascalParser.Create('', Content);
  try
    while NextToken(Token) do
    begin
      if Token.Kind = tkIdent then
      begin
        if SameText(Token.Value, 'uses') or SameText(Token.Value, 'contains') then
        begin
          // USES parsen
          while NextToken(Token) do
          begin
            FN := '';

            N := Token.Value;

            // Unit mit Namespace
            while (Parser.GetToken(NxtToken, True) and (NxtToken.Kind = tkSymbol) and (NxtToken.Value = '.')) do
            begin
              N := N + '.';

              NextToken(Token); // .

              NextToken(Token); // Namespace/Name
              N := N + Token.Value;
            end;

            if SameText(N, UnitName) then
            begin
              if RemoveUnit then
              begin
                if (Parser.PreToken <> nil) and (Parser.PreToken.Value = ',') then
                  RemoveStartIndex := Parser.PreToken.StartIndex
                else
                  RemoveStartIndex := Token.StartIndex;
              end
              else
                Exit; // Unit already in uses list
            end;

            if NextToken(Token) then
            begin
              if Token.Kind = tkIdent then
              begin
                if SameText(Token.Value, 'in') then
                begin
                  if NextToken(Token) then
                  begin
                    if Token.Kind = tkString then
                      FN := Token.Value
                    else
                      raise Exception.CreateFmt('Invalid project file. filename-string expected but "%s" found', [Token.Value]);
                    NextToken(Token);
                  end;
                end
                else
                  raise Exception.CreateFmt('Invalid project file. "in" expected but "%s" found', [Token.Value]);
              end;
            end;

            if Token = nil then
              raise Exception.Create('Unexpected end of file');

            if Token.Kind = tkSymbol then
            begin
              if RemoveUnit and (RemoveStartIndex > 0) then
              begin
                Parser.Delete(RemoveStartIndex, Token.StartIndex - RemoveStartIndex);
                Result := True;
                Break;
              end;

              if not RemoveUnit and (Token.Value = ';') then
              begin
                Parser.Replace(Token, Token, ',' + sLineBreak + '  ' + UnitName + ' in ''' + UnitFileName + ''';');
                Result := True;
                Break;
              end
              else
              if Token.Value <> ',' then
                raise Exception.CreateFmt('Invalid project file. ";" or "," expected but "%s" found', [Token.Value]);
            end
            else
              raise Exception.CreateFmt('Invalid project file. ";" or "," expected but "%s" found', [Token.Value]);
          end;
          Break;
        end;
      end;
    end;
    if Result then
      Content := Parser.Text;
  finally
    Parser.Free;
  end;
end;

function AppendProjectUnit(var ProjectContent: string; const UnitFileName: string): Boolean;
begin
  Result := AppendRemoveUnit(ProjectContent, UnitFileName, False);
end;

function RemoveProjectUnit(var ProjectContent: string; const UnitFileName: string): Boolean;
begin
  Result := AppendRemoveUnit(ProjectContent, UnitFileName, True);
end;

end.
