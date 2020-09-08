unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

uses
  DfmCheck_DfmCheck, ToolsAPIRepl, DfmCheck_Options,
  DfmCheck_PascalParser;

{$R *.dfm}

function AppendRemoveUnit(var Content: string; const UnitFileName: string; RemoveUnit: Boolean): Boolean;
var
  UnitName: string;
  N, FN: string;
  Parser: TPascalParser;
  Token: PTokenInfo;
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

function AppendUnit(var Content: string; const UnitFileName: string): Boolean;
begin
  Result := AppendRemoveUnit(Content, UnitFileName, False);
end;

function RemoveUnit(var Content: string; const UnitFileName: string): Boolean;
begin
  Result := AppendRemoveUnit(Content, UnitFileName, True);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  DfmCheck: TDfmCheck;
  Project: IOTAProject;
  Lines: TStrings;
  Content: string;
  RelDfmCheckUnit: string;
begin
  Options := TOptions.Create;
  try
    Project := LoadProject('C:\PreviewPictureDialog\test\test.dpr');
    if Project.ModuleCount > 1 then
    begin
      DfmCheck := TDfmCheck.Create(Project);
      try
        DfmCheck.CheckForms(True);
        Lines := TStringList.Create;
        try
          Lines.LoadFromFile(Project.FileName);
          Content := Lines.Text;
          RelDfmCheckUnit := ExtractRelativePath(ExtractFilePath(Project.FileName), DfmCheck.ProjectFileNameDfmCheckUnit);
          if AppendUnit(Content, RelDfmCheckUnit) then
          begin
            Lines.Text := Content;
            Lines.SaveToFile(Project.FileName);
          end;
          // Compile
          if RemoveUnit(Content, RelDfmCheckUnit) then
          begin
            Lines.Text := Content;
            Lines.SaveToFile(Project.FileName);
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

end.

